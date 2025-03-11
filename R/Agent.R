library("R6")
library("httr")
library("jsonlite")
library("future")
library("logger")

# Set up parallel processing
plan("multisession")

aigen_Agent <- R6Class(
  "aigen_Agent",
  
  public = list(
    #' @description Initialize a new advanced agent
    initialize = function(model,
                          name = "Assistant",
                          short_term_memory = 20, 
                          tools = list(),
                          log_file = NULL,
                          debug_mode = FALSE,
                          max_react_iterations = 5,
                          enable_react = TRUE) {  
      
      if (!inherits(model, "aigen_LanguageModel")) {
        stop("model must be an instance of aigen_LanguageModel")
      }
      
      private$name <- name
      private$model <- model
      private$debug_mode <- debug_mode
      private$log_file <- log_file
      private$tools <- list()
      private$tool_knowledge <- new.env(parent = emptyenv())
      private$available_data_sources <- character(0)
      private$max_react_iterations <- max_react_iterations
      private$enable_react <- enable_react
      
      # Initialize short-term memory with size limits
      private$short_term_memory <- LimitedConversationMemory$new(max_messages = short_term_memory)
      
      # Initialize logging only if log_file is provided
      if (!is.null(log_file)) {
        file.create(log_file, showWarnings = FALSE)
        private$log_event("Agent initialized")
      }
      
      # Add initial tools if provided
      if (length(tools) > 0) {
        for (tool_name in names(tools)) {
          self$add_tool(tool_name, tools[[tool_name]])
        }
      }
      
      # Initial registration of data sources
      self$register_data_sources()
    },
    
    #' @description Get the agent's name
    get_name = function() {
      return(private$name)
    },
    
    #' @description Main chat interface with ReAct loop support
    chat = function(user_input, system_prompt = NULL, context_window = 5, use_react = NULL) {
      tryCatch({
        # Basic validation
        if (!is.character(user_input) || length(user_input) != 1) {
          stop("Invalid user input - must be a single character string")
        }
        
        # Determine whether to use ReAct based on settings or parameter
        use_react_for_this_query <- if (!is.null(use_react)) {
          use_react  # Use explicit parameter value if provided
        } else {
          private$enable_react  # Otherwise use class default
        }
        
        # Update available data sources
        self$register_data_sources()
        
        # Log request
        private$log_event(sprintf("Processing request: %s", substr(user_input, 1, 100)))
        
        # Add user input to short-term memory
        private$short_term_memory$add_message("user", user_input)
        
        # If ReAct is enabled, use the ReAct loop for this request
        if (use_react_for_this_query) {
          response <- private$react_loop(user_input, system_prompt, context_window)
        } else {
          # Otherwise use the standard approach
          # 1) Attempt to detect direct tool usage in user_input
          direct_tool_results <- private$detect_direct_tool_usage(user_input)
          
          # 2) If direct usage isn't found, try generating a plan with the model
          if (is.null(direct_tool_results)) {
            tool_results <- private$process_tool_request(user_input)
          } else {
            tool_results <- direct_tool_results
          }
          
          # 3) Build context
          context <- private$build_context(
            user_input = user_input,
            system_prompt = system_prompt,
            context_window = context_window,
            tool_results = tool_results
          )
          
          # 4) Generate final answer
          response <- private$generate_response(context)
        }
        
        # Save agent response to memory
        private$short_term_memory$add_message("assistant", response)
        
        # Log success
        private$log_event("Response generated successfully")
        
        return(response)
        
      }, error = function(e) {
        private$log_event(sprintf("Error in chat: %s", e$message), "ERROR")
        stop(sprintf("Chat error: %s", e$message))
      })
    },
    
    #' @description Add a tool to the agent's toolkit with enhanced validation
    add_tool = function(name, fn) {
      tryCatch({
        if (!is.character(name) || length(name) != 1) {
          stop("Tool name must be a single string")
        }
        if (!is.function(fn)) {
          stop("Tool must be a function")
        }
        
        # Validate function has proper documentation
        fn_formals <- formals(fn)
        if (length(fn_formals) > 0) {
          # Check that parameters have meaningful names (not just "..." or single letters)
          param_names <- names(fn_formals)
          if (any(nchar(param_names) <= 1) && !all(param_names == "...")) {
            warning(sprintf("Tool '%s' has parameters with very short names, which may affect usability", name))
          }
        }
        
        # Store the tool
        private$tools[[name]] <- fn
        
        # Analyze and store tool metadata with enhanced documentation
        private$analyze_tool(name)
        
        private$log_event(sprintf("Tool '%s' added successfully", name))
        
        invisible(self)
        
      }, error = function(e) {
        private$log_event(sprintf("Error adding tool: %s", e$message), "ERROR")
        stop(sprintf("Tool addition error: %s", e$message))
      })
    },
    
    #' @description Register (or refresh) available data sources in .GlobalEnv
    register_data_sources = function() {
      all_objects <- ls(envir = .GlobalEnv)
      
      data_sources <- character(0)
      for (obj_name in all_objects) {
        tryCatch({
          obj <- get(obj_name, envir = .GlobalEnv)
          if (is.data.frame(obj) || is.matrix(obj)) {
            # Store additional metadata about the data source
            dimensions <- dim(obj)
            col_info <- if (is.data.frame(obj)) {
              paste(colnames(obj), collapse = ", ")
            } else {
              "matrix"
            }
            
            # Add to known data sources with metadata
            data_sources <- c(data_sources, obj_name)
            
            # Optionally store metadata in a separate structure
            # This could be extended to include more details about each data source
            if (is.null(private$data_source_metadata)) {
              private$data_source_metadata <- new.env(parent = emptyenv())
            }
            
            assign(obj_name, list(
              type = if(is.data.frame(obj)) "data.frame" else "matrix",
              dim = dimensions,
              columns = if(is.data.frame(obj)) colnames(obj) else NULL,
              last_updated = Sys.time()
            ), envir = private$data_source_metadata)
          }
        }, error = function(e) {
          private$log_event(sprintf("Error checking object %s: %s", obj_name, e$message), "WARNING")
        })
      }
      
      private$available_data_sources <- data_sources
      private$log_event(sprintf("Registered %d data sources: %s",
                                length(data_sources), 
                                paste(data_sources, collapse = ", ")))
      
      invisible(self)
    },
    
    #' @description Execute a tool by name with enhanced verification and parameter resolution
    use_tool = function(tool_name, ...) {
      # Validate tool existence
      if (!tool_name %in% names(private$tools)) {
        stop(sprintf("Tool '%s' not found in agent's toolkit", tool_name))
      }
      
      # Get the tool function
      fn <- private$tools[[tool_name]]
      private$log_event(sprintf("Preparing to execute tool: %s", tool_name))
      
      tryCatch({
        # Collect all arguments into a list
        args <- list(...)
        
        # Enhance arguments with proper type conversion and validation
        enhanced_args <- private$process_parameters(args)
        
        # Verify that required parameters are present and have valid types
        valid_args <- private$verify_tool_requirements(tool_name, fn, enhanced_args)
        
        # Execute the tool with provided arguments
        private$log_event(sprintf("Executing tool '%s' with validated parameters", tool_name))
        result <- do.call(fn, valid_args)
        
        # Verify the tool output (basic sanity check)
        verified_result <- private$verify_tool_output(tool_name, result)
        
        # Update tool usage statistics
        if (exists(tool_name, envir = private$tool_knowledge)) {
          tool_info <- get(tool_name, envir = private$tool_knowledge)
          tool_info$usage_count <- tool_info$usage_count + 1
          tool_info$last_used <- Sys.time()
          assign(tool_name, tool_info, envir = private$tool_knowledge)
          
          private$log_event(sprintf("Updated usage stats for tool '%s' (count: %d)", 
                                    tool_name, 
                                    tool_info$usage_count))
        }
        
        private$log_event(sprintf("Successfully executed tool '%s'", tool_name))
        return(verified_result)
        
      }, error = function(e) {
        private$log_event(sprintf("Error executing tool '%s': %s", tool_name, e$message), "ERROR")
        stop(sprintf("Error executing %s: %s", tool_name, e$message))
      })
    },
    
    #' @description Enable or disable the ReAct loop for future queries
    set_react_mode = function(enable = TRUE) {
      private$enable_react <- enable
      private$log_event(sprintf("ReAct mode %s", if(enable) "enabled" else "disabled"))
      invisible(self)
    },
    
    #' @description Set the maximum number of iterations for the ReAct loop
    set_max_react_iterations = function(max_iterations) {
      if (!is.numeric(max_iterations) || max_iterations < 1) {
        stop("max_iterations must be a positive number")
      }
      private$max_react_iterations <- as.integer(max_iterations)
      private$log_event(sprintf("Max ReAct iterations set to %d", private$max_react_iterations))
      invisible(self)
    },
    
    #' @description Get information about available tools
    get_tool_info = function() {
      if (length(private$tools) == 0) {
        return(list())
      }
      
      tool_info <- lapply(names(private$tools), function(name) {
        if (exists(name, envir = private$tool_knowledge)) {
          knowledge <- get(name, envir = private$tool_knowledge)
          
          list(
            name = name,
            description = knowledge$description,
            parameters = knowledge$parameters,
            usage_count = knowledge$usage_count,
            last_used = knowledge$last_used
          )
        } else {
          list(
            name = name,
            parameters = names(formals(private$tools[[name]])),
            usage_count = 0,
            last_used = NULL
          )
        }
      })
      
      names(tool_info) <- names(private$tools)
      return(tool_info)
    },
    
    #' @description Get information about available data sources
    get_data_sources = function() {
      if (length(private$available_data_sources) == 0) {
        return(list())
      }
      
      if (is.null(private$data_source_metadata)) {
        # Basic info if metadata not available
        return(as.list(private$available_data_sources))
      }
      
      # Return detailed metadata
      metadata_list <- lapply(private$available_data_sources, function(name) {
        if (exists(name, envir = private$data_source_metadata)) {
          get(name, envir = private$data_source_metadata)
        } else {
          list(name = name)
        }
      })
      
      names(metadata_list) <- private$available_data_sources
      return(metadata_list)
    }
  ),
  
  private = list(
    name = NULL,
    model = NULL,
    debug_mode = NULL,
    log_file = NULL,
    tools = NULL,
    tool_knowledge = NULL,
    short_term_memory = NULL, 
    available_data_sources = NULL,
    data_source_metadata = NULL,
    max_react_iterations = NULL,
    enable_react = NULL,
    
    #' ---------------------- LOGGING ----------------------
    log_event = function(message, level = "INFO") {
      if (!is.null(private$log_file)) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        log_entry <- sprintf("[%s] [%s] %s\n", timestamp, level, message)
        cat(log_entry, file = private$log_file, append = TRUE)
        if (private$debug_mode) cat(log_entry)
      }
    },
    
    #' ---------------------- REACT LOOP IMPLEMENTATION ----------------------
    
    #' @description Main ReAct loop - Thinking, Acting, and Observing
    react_loop = function(user_input, system_prompt = NULL, context_window = 5) {
      # Initialize the ReAct state
      react_state <- list(
        query = user_input,
        thoughts = list(),
        actions = list(),
        observations = list(),
        iteration = 0,
        final_answer = NULL
      )
      
      private$log_event("Starting ReAct loop for query")
      
      # Main ReAct loop
      while (react_state$iteration < private$max_react_iterations && is.null(react_state$final_answer)) {
        # Update iteration counter
        react_state$iteration <- react_state$iteration + 1
        private$log_event(sprintf("ReAct iteration %d/%d", 
                                  react_state$iteration, 
                                  private$max_react_iterations))
        
        # THINK: Reason about what to do next
        react_state <- private$react_think(react_state, system_prompt)
        
        # Check if we have a final answer already
        if (!is.null(react_state$final_answer)) {
          private$log_event("ReAct loop completed with final answer")
          break
        }
        
        # ACT: Execute the chosen action (usually a tool)
        if (!is.null(react_state$next_action)) {
          react_state <- private$react_act(react_state)
          
          # OBSERVE: Record the results
          react_state <- private$react_observe(react_state)
        } else {
          # No action was specified, prompt for a final answer
          react_state <- private$react_finalize(react_state, system_prompt)
        }
      }
      
      # If we've reached max iterations without a final answer, generate one
      if (is.null(react_state$final_answer)) {
        private$log_event("ReAct reached maximum iterations, generating final answer")
        react_state <- private$react_finalize(react_state, system_prompt)
      }
      
      # Return the final answer
      return(react_state$final_answer)
    },
    
    #' @description Think step of the ReAct loop - reason about next steps
    react_think = function(state, system_prompt = NULL) {
      # Construct the thinking prompt
      base_prompt <- "You are a reasoning agent that helps solve problems by thinking step-by-step."
      if (!is.null(system_prompt)) {
        system_msg <- paste(base_prompt, system_prompt, sep = "\n\n")
      } else {
        system_msg <- base_prompt
      }
      
      # Build context from the current state
      context <- sprintf("User Query: %s\n\n", state$query)
      
      # Add previous thoughts, actions, and observations
      if (length(state$thoughts) > 0) {
        for (i in seq_along(state$thoughts)) {
          # Add the thought
          context <- paste0(context, sprintf("Thought %d: %s\n\n", i, state$thoughts[[i]]))
          
          # Add the action if available
          if (i <= length(state$actions) && !is.null(state$actions[[i]])) {
            action_str <- if (is.list(state$actions[[i]])) {
              sprintf("Action %d: Use tool '%s' with parameters: %s", 
                      i, 
                      state$actions[[i]]$tool,
                      paste(names(state$actions[[i]]$parameters), 
                            state$actions[[i]]$parameters, 
                            sep = "=", 
                            collapse = ", "))
            } else {
              sprintf("Action %d: %s", i, state$actions[[i]])
            }
            context <- paste0(context, sprintf("%s\n\n", action_str))
          }
          
          # Add the observation if available
          if (i <= length(state$observations) && !is.null(state$observations[[i]])) {
            context <- paste0(context, sprintf("Observation %d: %s\n\n", 
                                               i, 
                                               state$observations[[i]]))
          }
        }
      }
      
      # Add available tools information
      tools_info <- private$format_tool_descriptions()
      context <- paste0(context, "Available Tools:\n", tools_info, "\n\n")
      
      # Add available data sources
      data_sources <- paste(private$available_data_sources, collapse = ", ")
      context <- paste0(context, "Available Data Sources: ", 
                        ifelse(length(private$available_data_sources) > 0, 
                               data_sources, 
                               "None"), 
                        "\n\n")
      
      # Add instructions for the next step
      next_step_prompt <- paste0(
        context,
        "Based on the above, provide your next thought and action. ",
        "You can:\n",
        "1. Use a tool by specifying 'ACTION: Use tool <tool_name> with parameters <param1=value1, param2=value2, ...>'\n",
        "2. Provide a final answer with 'FINAL ANSWER: <your detailed answer>'\n\n",
        "Next Thought:"
      )
      
      # Generate the next thought
      thought_response <- private$model$generate(
        prompt = next_step_prompt,
        system_message = system_msg
      )
      
      private$log_event("Generated next thought in ReAct loop")
      
      # Parse the thought to extract action or final answer
      parsed_response <- private$parse_react_response(thought_response)
      
      # Update the state with the new thought and next action
      state$thoughts[[length(state$thoughts) + 1]] <- parsed_response$thought
      state$next_action <- parsed_response$action
      state$final_answer <- parsed_response$final_answer
      
      return(state)
    },
    
    #' @description Act step of the ReAct loop - execute the chosen action
    react_act = function(state) {
      if (is.null(state$next_action)) {
        private$log_event("No action specified in ReAct act step", "WARNING")
        return(state)
      }
      
      action <- state$next_action
      private$log_event(sprintf("ReAct executing action: %s", 
                                if(is.list(action)) action$tool else "custom"))
      
      # Execute the action (typically a tool)
      result <- tryCatch({
        if (is.list(action) && !is.null(action$tool)) {
          # It's a tool execution
          if (action$tool %in% names(private$tools)) {
            tool_fn <- private$tools[[action$tool]]
            params <- private$process_parameters(action$parameters)
            
            # Verify parameters
            validated_params <- private$verify_tool_requirements(
              action$tool, tool_fn, params
            )
            
            # Execute the tool
            res <- do.call(tool_fn, validated_params)
            
            # Verify output
            private$verify_tool_output(action$tool, res)
          } else {
            sprintf("Error: Tool '%s' not found", action$tool)
          }
        } else {
          # It's a custom action (text)
          action
        }
      }, error = function(e) {
        # Record the error as the result
        sprintf("Error executing action: %s", e$message)
      })
      
      # Store the action and its result
      state$actions[[length(state$actions) + 1]] <- state$next_action
      state$action_result <- result
      
      return(state)
    },
    
    #' @description Observe step of the ReAct loop - record the results of actions
    react_observe = function(state) {
      if (is.null(state$action_result)) {
        private$log_event("No action result to observe in ReAct observe step", "WARNING")
        return(state)
      }
      
      # Format the observation
      observation <- tryCatch({
        if (is.list(state$action_result) || is.data.frame(state$action_result)) {
          # For complex results, format as string representation
          paste(capture.output(print(state$action_result)), collapse = "\n")
        } else {
          # For simple results, use as is
          as.character(state$action_result)
        }
      }, error = function(e) {
        sprintf("Error formatting observation: %s", e$message)
      })
      
      private$log_event("Recorded observation in ReAct loop")
      
      # Store the observation
      state$observations[[length(state$observations) + 1]] <- observation
      
      # Clear the action result and next action for the next iteration
      state$action_result <- NULL
      state$next_action <- NULL
      
      return(state)
    },
    
    #' @description Generate the final answer when the ReAct loop concludes
    react_finalize = function(state, system_prompt = NULL) {
      # Create a system message
      base_prompt <- sprintf("You are %s. Provide a comprehensive final answer.", private$name)
      if (!is.null(system_prompt)) {
        system_msg <- paste(base_prompt, system_prompt, sep = "\n\n")
      } else {
        system_msg <- base_prompt
      }
      
      # Compile all thoughts, actions, and observations
      context <- sprintf("User Query: %s\n\n", state$query)
      
      if (length(state$thoughts) > 0) {
        context <- paste0(context, "Reasoning Process:\n\n")
        
        for (i in seq_along(state$thoughts)) {
          # Add the thought
          context <- paste0(context, sprintf("Thought %d: %s\n", i, state$thoughts[[i]]))
          
          # Add the action if available
          if (i <= length(state$actions) && !is.null(state$actions[[i]])) {
            action_str <- if (is.list(state$actions[[i]])) {
              sprintf("Action %d: Used tool '%s' with parameters: %s", 
                      i, 
                      state$actions[[i]]$tool,
                      paste(names(state$actions[[i]]$parameters), 
                            state$actions[[i]]$parameters, 
                            sep = "=", 
                            collapse = ", "))
            } else {
              sprintf("Action %d: %s", i, state$actions[[i]])
            }
            context <- paste0(context, sprintf("%s\n", action_str))
          }
          
          # Add the observation if available
          if (i <= length(state$observations) && !is.null(state$observations[[i]])) {
            context <- paste0(context, sprintf("Observation %d: %s\n", 
                                               i, 
                                               state$observations[[i]]))
          }
          
          context <- paste0(context, "\n")
        }
      }
      
      # Request a final answer
      final_prompt <- paste0(
        context,
        "Based on the above reasoning process and observations, please provide a comprehensive final answer to the user's query."
      )
      
      # Generate the final answer
      answer <- private$model$generate(
        prompt = final_prompt,
        system_message = system_msg
      )
      
      private$log_event("Generated final answer from ReAct reasoning")
      
      # Update the state
      state$final_answer <- answer
      
      return(state)
    },
    
    #' @description Parse the response from the thinking step to extract actions or final answers
    parse_react_response = function(response) {
      # Initialize result structure
      result <- list(
        thought = response,
        action = NULL,
        final_answer = NULL
      )
      
      # Split response by lines
      lines <- strsplit(response, "\n")[[1]]
      
      # Extract the thought part (everything before ACTION or FINAL ANSWER)
      thought_lines <- c()
      action_line <- NULL
      final_answer_lines <- c()
      
      in_thought <- TRUE
      in_final_answer <- FALSE
      
      for (line in lines) {
        if (grepl("^ACTION\\s*:", line, ignore.case = TRUE)) {
          in_thought <- FALSE
          action_line <- line
        } else if (grepl("^FINAL ANSWER\\s*:", line, ignore.case = TRUE)) {
          in_thought <- FALSE
          in_final_answer <- TRUE
          final_answer_lines <- c(final_answer_lines, gsub("^FINAL ANSWER\\s*:\\s*", "", line))
        } else if (in_thought) {
          thought_lines <- c(thought_lines, line)
        } else if (in_final_answer) {
          final_answer_lines <- c(final_answer_lines, line)
        }
      }
      
      # Set the thought
      result$thought <- paste(thought_lines, collapse = "\n")
      
      # Parse action if present
      if (!is.null(action_line)) {
        # Extract "Use tool X with parameters Y, Z"
        action_text <- gsub("^ACTION\\s*:\\s*", "", action_line)
        
        # Check if it's a tool usage
        if (grepl("^[Uu]se\\s+tool\\s+([A-Za-z0-9_]+)", action_text)) {
          # Extract the tool name
          tool_pattern <- "^[Uu]se\\s+tool\\s+([A-Za-z0-9_]+)"
          tool_name <- regmatches(action_text, regexec(tool_pattern, action_text))[[1]][2]
          
          # Extract parameters if present
          params <- list()
          param_pattern <- "(?:with|using)\\s+parameters?\\s+(.*?)$"
          param_matches <- regmatches(action_text, regexec(param_pattern, action_text, perl = TRUE))
          
          if (length(param_matches[[1]]) > 1) {
            param_str <- param_matches[[1]][2]
            params <- private$parse_parameter_string(param_str)
          }
          
          result$action <- list(
            tool = tool_name,
            parameters = params
          )
        } else {
          # Custom action
          result$action <- action_text
        }
      }
      
      # Set final answer if present
      if (length(final_answer_lines) > 0) {
        result$final_answer <- paste(final_answer_lines, collapse = "\n")
      }
      
      return(result)
    },
    
    #' ---------------------- TOOL USAGE DETECTION ----------------------
    
    #' @description Attempts to detect explicit instructions in user input
    detect_direct_tool_usage = function(user_input) {
      tool_names <- names(private$tools)
      
      # Build a pattern capturing something like: "use <toolname> with paramA=valA paramB=valB..."
      usage_regex <- "(?i)(?:use|run)\\s+([A-Za-z0-9_]+)\\s+(?:with\\s+)?(.*)"
      
      match <- regexpr(usage_regex, user_input, perl = TRUE)
      if (match[1] == -1) {
        return(NULL)  # not found
      }
      
      # Extract the matches
      matches <- regmatches(user_input, match)
      if (length(matches) == 0) return(NULL)
      
      # Extract tool name and parameters
      tool_line <- regmatches(user_input, match, invert = FALSE)[[1]]
      capture_groups <- regexpr(usage_regex, tool_line, perl = TRUE)
      group_values <- regmatches(tool_line, capture_groups)
      
      if (length(group_values) < 3) {
        # Could be "use <tool>" but no param mention
        possible_tool <- group_values[2]
        if (possible_tool %in% tool_names) {
          # Return with empty param block
          return(private$execute_found_tool(possible_tool, list()))
        } else {
          return(NULL)
        }
      }
      
      # Now we have a recognized tool name & parameter chunk
      found_tool_name <- group_values[2]
      param_string <- group_values[3]
      
      # Check that the tool is recognized
      if (!found_tool_name %in% tool_names) {
        private$log_event(sprintf("User mentioned unrecognized tool '%s'", found_tool_name), "INFO")
        return(NULL)
      }
      
      # Parse the param string
      param_list <- private$parse_parameter_string(param_string)
      
      # Attempt execution
      return(private$execute_found_tool(found_tool_name, param_list))
    },
    
    #' @description Process tool request based on LLM planning
    process_tool_request = function(user_input) {
      if (length(private$tools) == 0) return(NULL)
      
      plan <- private$generate_tool_plan(user_input)
      if (is.null(plan)) {
        return(NULL)
      }
      
      # Attempt to execute the plan
      result <- tryCatch({
        if (!plan$tool %in% names(private$tools)) {
          stop(sprintf("Planned tool '%s' not found", plan$tool))
        }
        
        tool_fn <- private$tools[[plan$tool]]
        # Process and validate parameters
        params <- private$process_parameters(plan$parameters)
        validated_params <- private$verify_tool_requirements(plan$tool, tool_fn, params)
        
        private$log_event(sprintf("Executing tool '%s' via model plan", plan$tool))
        output <- do.call(tool_fn, validated_params)
        
        # Verify output
        verified_output <- private$verify_tool_output(plan$tool, output)
        
        list(
          tool = plan$tool,
          results = verified_output,
          summary = paste(capture.output(print(verified_output)), collapse = "\n")
        )
        
      }, error = function(e) {
        private$log_event(sprintf("Tool execution error: %s", e$message), "ERROR")
        return(NULL)
      })
      
      return(result)
    },
    
    #' @description Creates or returns the final tool usage result
    execute_found_tool = function(tool_name, param_list) {
      # Validate the function
      tool_fn <- private$tools[[tool_name]]
      if (is.null(tool_fn)) {
        return(NULL)
      }
      
      # Process and validate parameters
      params <- private$process_parameters(param_list)
      validated_params <- private$verify_tool_requirements(tool_name, tool_fn, params)
      
      # Execute
      out <- tryCatch({
        private$log_event(sprintf("Executing tool '%s' from direct user instruction", tool_name))
        res <- do.call(tool_fn, validated_params)
        
        # Verify output
        verified_res <- private$verify_tool_output(tool_name, res)
        
        list(
          tool = tool_name,
          results = verified_res,
          summary = paste(capture.output(print(verified_res)), collapse = "\n")
        )
      }, error = function(e) {
        private$log_event(sprintf("Error in direct usage of tool '%s': %s", tool_name, e$message),
                          "ERROR")
        NULL
      })
      
      return(out)
    },
    
    #' ---------------------- TOOL PLANNING ----------------------
    
    #' @description Use LLM to generate a tool execution plan
    generate_tool_plan = function(user_input) {
      if (length(private$tools) == 0) return(NULL)
      
      # Create a detailed planning prompt
      planning_prompt <- sprintf(
        "User Input: %s\n\n",
        user_input
      )
      
      # Add tool descriptions with examples
      planning_prompt <- paste0(
        planning_prompt,
        "Available Tools:\n",
        private$format_tool_descriptions_with_examples(),
        "\n\n"
      )
      
      # Add data source information with more details
      data_sources_info <- if (length(private$available_data_sources) > 0) {
        ds_descriptions <- sapply(private$available_data_sources, function(ds_name) {
          if (!is.null(private$data_source_metadata) && 
              exists(ds_name, envir = private$data_source_metadata)) {
            
            metadata <- get(ds_name, envir = private$data_source_metadata)
            sprintf("- %s: %s with dimensions %s", 
                    ds_name, 
                    metadata$type,
                    paste(metadata$dim, collapse = "x"))
          } else {
            ds_name
          }
        })
        paste(ds_descriptions, collapse = "\n")
      } else {
        "None"
      }
      
      planning_prompt <- paste0(
        planning_prompt,
        "Available Data Sources:\n",
        data_sources_info,
        "\n\n"
      )
      
      # Request format guidance
      planning_prompt <- paste0(
        planning_prompt,
        "Analyze the user input and determine if a tool should be used.\n",
        "If tool usage is needed, respond in this format:\n",
        "Tool: <tool_name>\n",
        "Parameters:\n",
        "- <param_name>: <value>\n",
        "- <param_name>: <value>\n\n",
        "If no tool usage is needed, respond with just 'None'."
      )
      
      # Generate the plan
      plan_response <- private$model$generate(
        prompt = planning_prompt,
        system_message = sprintf(
          "You are %s, a technical assistant that helps decide when and how to use tools. Analyze requests carefully and determine if tool usage would be helpful. If yes, specify exactly which tool and parameters to use.", 
          private$name
        )
      )
      
      # Parse the plan
      return(private$parse_tool_plan(plan_response))
    },
    
    #' @description Parse the LLM's tool plan output
    parse_tool_plan = function(plan_response) {
      if (is.null(plan_response) || nchar(trimws(plan_response)) == 0) {
        private$log_event("Received empty plan response", "WARNING")
        return(NULL)
      }
      
      # If LLM says "None", assume no tool usage
      if (tolower(trimws(plan_response)) == "none") {
        return(NULL)
      }
      
      lines <- strsplit(plan_response, "\n")[[1]]
      lines <- trimws(lines[nchar(trimws(lines)) > 0])
      
      # Parse for "Tool: <toolName>"
      tool_pattern <- "^Tool:\\s*(\\w+)$"
      tool_line_idx <- grep(tool_pattern, lines, perl = TRUE)
      if (length(tool_line_idx) == 0) {
        private$log_event("No tool found in plan", "WARNING")
        return(NULL)
      }
      
      tool_line <- lines[tool_line_idx[1]]
      tool_name <- gsub(tool_pattern, "\\1", tool_line, perl = TRUE)
      
      if (!tool_name %in% names(private$tools)) {
        private$log_event(sprintf("Specified tool '%s' not in registry", tool_name), "WARNING")
        return(NULL)
      }
      
      # Parse for "Parameters:" section
      param_section_idx <- grep("^Parameters:", lines, ignore.case = TRUE)
      if (length(param_section_idx) == 0) {
        private$log_event("No parameter section found in plan", "INFO")
        return(list(tool = tool_name, parameters = list()))
      }
      
      # Everything below "Parameters:" are param lines
      param_lines <- lines[(param_section_idx[1] + 1):length(lines)]
      
      parameters <- list()
      for (pl in param_lines) {
        # Example format: "- paramA: valueA"
        param_pattern <- "^[-*]\\s*([A-Za-z0-9_]+)\\s*:\\s*(.+)$"
        if (grepl(param_pattern, pl, perl = TRUE)) {
          matches <- regmatches(pl, regexec(param_pattern, pl, perl = TRUE))[[1]]
          if (length(matches) == 3) {
            p_name <- trimws(matches[2])
            p_val <- trimws(matches[3])
            parameters[[p_name]] <- p_val
          }
        }
      }
      
      private$log_event(sprintf("Parsed plan => Tool: '%s' with %d parameters", 
                                tool_name, 
                                length(parameters)))
      return(list(tool = tool_name, parameters = parameters))
    },
    
    #' @description Parse parameter string for direct tool usage
    parse_parameter_string = function(param_string) {
      if (!nzchar(trimws(param_string))) return(list())
      
      # Split by commas or spaces
      raw_parts <- unlist(strsplit(param_string, "[,;]|(?<=\\w)\\s+(?=\\w)", perl = TRUE))
      raw_parts <- trimws(raw_parts[nchar(trimws(raw_parts)) > 0])
      
      params <- list()
      for (part in raw_parts) {
        # param=val or param:val
        kv_pattern <- "^([A-Za-z0-9_]+)\\s*[:=]\\s*(.+)$"
        if (grepl(kv_pattern, part, perl = TRUE)) {
          matches <- regmatches(part, regexec(kv_pattern, part, perl = TRUE))[[1]]
          if (length(matches) == 3) {
            p_name <- trimws(matches[2])
            p_val <- trimws(matches[3])
            params[[p_name]] <- p_val
          }
        }
        # Handle bare words as boolean TRUE flags
        else if (grepl("^[A-Za-z0-9_]+$", part)) {
          params[[part]] <- TRUE
        }
      }
      
      params
    },
    
    #' ---------------------- PARAMETER RESOLUTION ----------------------
    
    #' @description Enhanced parameter processing with better type inference and data source resolution
    process_parameters = function(params) {
      processed <- list()
      
      for (name in names(params)) {
        value <- params[[name]]
        
        if (is.character(value)) {
          # Check if direct value needs conversion
          if (value %in% private$available_data_sources) {
            # Attempt to get the referenced data
            ds <- tryCatch({
              get(value, envir = .GlobalEnv)
            }, error = function(e) NULL)
            
            # If it's a valid data.frame or matrix, use it
            if (is.data.frame(ds) || is.matrix(ds)) {
              value <- ds
              private$log_event(sprintf("Resolved data source '%s' in param '%s'", value, name))
            }
          } 
          # Check for vectors using c(...) syntax
          else if (grepl("^c\\(.*\\)$", trimws(value))) {
            # Try to parse as an R vector expression
            tryCatch({
              vector_expr <- paste0("list(", sub("^c\\((.*)\\)$", "\\1", value), ")")
              vector_result <- eval(parse(text = vector_expr), envir = new.env())
              value <- unlist(vector_result)
              private$log_event(sprintf("Converted string '%s' to vector of length %d", 
                                        substr(value, 1, 15), 
                                        length(value)))
            }, error = function(e) {
              # Keep as string if parsing fails
              private$log_event(sprintf("Failed to parse '%s' as vector: %s", 
                                        value, 
                                        e$message), 
                                "WARNING")
            })
          }
          # Check for list using list(...) syntax
          else if (grepl("^list\\(.*\\)$", trimws(value))) {
            # Try to parse as an R list expression
            tryCatch({
              list_result <- eval(parse(text = value), envir = new.env())
              if (is.list(list_result)) {
                value <- list_result
                private$log_event(sprintf("Converted string '%s' to list with %d elements", 
                                          substr(value, 1, 15), 
                                          length(value)))
              }
            }, error = function(e) {
              # Keep as string if parsing fails
              private$log_event(sprintf("Failed to parse '%s' as list: %s", 
                                        value, 
                                        e$message), 
                                "WARNING")
            })
          }
          # Handle data.frame(...) syntax
          else if (grepl("^data\\.frame\\(.*\\)$", trimws(value))) {
            # Try to parse as a data.frame expression
            tryCatch({
              df_result <- eval(parse(text = value), envir = new.env())
              if (is.data.frame(df_result)) {
                value <- df_result
                private$log_event(sprintf("Converted string to data.frame with %d rows, %d cols", 
                                          nrow(df_result), 
                                          ncol(df_result)))
              }
            }, error = function(e) {
              # Keep as string if parsing fails
              private$log_event(sprintf("Failed to parse as data.frame: %s", e$message), "WARNING")
            })
          }
          # More advanced type inference
          else if (identical(tolower(value), "true") || identical(tolower(value), "false")) {
            # Boolean values
            value <- tolower(value) == "true"
          }
          else if (grepl("^\\d+$", value)) {
            # Integer values
            value <- as.integer(value)
          }
          else if (grepl("^-?\\d+\\.\\d+$", value)) {
            # Floating point values
            value <- as.numeric(value)
          }
          else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", value)) {
            # Date values
            tryCatch({
              value <- as.Date(value)
            }, error = function(e) {
              # Keep as string if parsing fails
            })
          }
          else if (grepl("^NULL$", trimws(value))) {
            # NULL value
            value <- NULL
          }
          else if (grepl("^NA$", trimws(value))) {
            # NA value
            value <- NA
          }
        }
        
        processed[[name]] <- value
      }
      
      processed
    },
    
    #' ---------------------- TOOL VERIFICATION ----------------------
    
    #' @description Verify that all required parameters are present and valid
    verify_tool_requirements = function(tool_name, fn, provided_params) {
      fn_formals <- formals(fn)
      param_names <- names(fn_formals)
      
      # Identify required parameters (those without defaults)
      required_params <- param_names[sapply(fn_formals, is.symbol)]
      
      # Check if any required parameters are missing
      missing_required <- setdiff(required_params, names(provided_params))
      if (length(missing_required) > 0 && !"..." %in% param_names) {
        stop(sprintf("Required parameters missing for tool '%s': %s", 
                     tool_name, 
                     paste(missing_required, collapse = ", ")))
      }
      
      # Prepare the final parameter list
      final_params <- list()
      
      # Include provided parameters that match the function signature
      for (param in names(provided_params)) {
        if (param %in% param_names || "..." %in% param_names) {
          final_params[[param]] <- provided_params[[param]]
        } else {
          private$log_event(sprintf("Ignoring unknown parameter '%s' for tool '%s'", 
                                    param, 
                                    tool_name), 
                            "WARNING")
        }
      }
      
      # Add default values for parameters not provided by the user
      for (param in param_names) {
        if (!param %in% names(final_params) && !is.symbol(fn_formals[[param]])) {
          final_params[[param]] <- eval(fn_formals[[param]], envir = .GlobalEnv)
        }
      }
      
      # Log the parameter verification
      private$log_event(sprintf("Verified parameters for tool '%s': %s", 
                                tool_name, 
                                paste(names(final_params), collapse = ", ")))
      
      return(final_params)
    },
    
    #' @description Verify the output of a tool execution
    verify_tool_output = function(tool_name, output) {
      # This could be extended with more sophisticated validation
      # For now, just check that output is not NULL or error
      if (is.null(output)) {
        warning(sprintf("Tool '%s' returned NULL output", tool_name))
      }
      
      if (inherits(output, "try-error")) {
        stop(sprintf("Tool '%s' returned an error: %s", tool_name, attr(output, "condition")$message))
      }
      
      # If output is a list, check if it has an 'error' element indicating a problem
      if (is.list(output) && !is.null(output$error)) {
        warning(sprintf("Tool '%s' returned an error message: %s", tool_name, output$error))
      }
      
      # Log successful verification
      private$log_event(sprintf("Verified output from tool '%s'", tool_name))
      
      return(output)
    },
    
    #' ---------------------- TOOL DISCOVERY ----------------------
    
    #' @description Analyze a tool and generate documentation
    analyze_tool = function(tool_name) {
      fn <- private$tools[[tool_name]]
      params <- names(formals(fn))
      
      # Get parameter details including defaults - with robust error handling
      param_details <- tryCatch({
        lapply(params, function(p) {
          tryCatch({
            # Safely extract the formal parameter
            formal <- formals(fn)[[p]]
            
            # Safely determine if it has a default value
            has_default <- !is.symbol(formal)
            
            # Safely get default value if it exists
            default <- if (has_default) {
              tryCatch({
                eval(formal, envir = .GlobalEnv)
              }, error = function(e) {
                # If we can't evaluate the default, just deparse it
                paste0("<default: ", deparse(formal), ">")
              })
            } else {
              "<required>"
            }
            
            # Return the parameter details
            list(
              name = p,
              default = default,
              required = !has_default && p != "..."
            )
          }, error = function(e) {
            # Fallback if anything goes wrong for this parameter
            private$log_event(
              sprintf("Error analyzing parameter '%s' for tool '%s': %s", p, tool_name, e$message),
              "WARNING"
            )
            list(
              name = p,
              default = "<unknown>",
              required = FALSE
            )
          })
        })
      }, error = function(e) {
        # If the entire parameter analysis fails, create a basic list
        private$log_event(
          sprintf("Error analyzing parameters for tool '%s': %s", tool_name, e$message),
          "WARNING"
        )
        
        # Create a simplified parameter list as fallback
        lapply(params, function(p) {
          list(
            name = p,
            default = "<unknown>",
            required = p != "..."
          )
        })
      })
      
      # Format param info for documentation - with safe handling
      param_doc <- sapply(param_details, function(p) {
        if (isTRUE(p$required)) {
          sprintf("%s (required)", p$name)
        } else if (identical(p$name, "...")) {
          "... (additional arguments)"
        } else {
          default_text <- if (is.character(p$default)) {
            # Ensure proper quoting for character defaults
            paste0('"', gsub('"', '\\"', p$default), '"')
          } else {
            # Safe conversion to string for any other type
            tryCatch(as.character(p$default), error = function(e) "<complex>")
          }
          sprintf("%s (default: %s)", p$name, default_text)
        }
      })
      
      # Generate documentation prompt
      doc_prompt <- sprintf(
        "Function name: %s\nParameters: %s\n\nYour task is to write a concise yet comprehensive description of this function's purpose and usage. Include what it does, when to use it, and brief explanations of each parameter. Be specific and technical but user-friendly.",
        tool_name,
        paste(param_doc, collapse = ", ")
      )
      
      # Generate description using LLM
      description <- private$model$generate(
        prompt = doc_prompt,
        system_message = "You are a technical documentation expert specializing in R function documentation."
      )
      
      # Clean up whitespace in description
      description <- trimws(gsub("\\s+", " ", description))
      
      # Generate example usage
      example_prompt <- sprintf(
        "Create a concise, realistic example of using the function '%s' with parameters: %s.\nProvide the example in the format of a function call with appropriate parameter values. Be specific and practical.",
        tool_name,
        paste(param_doc, collapse = ", ")
      )
      
      example <- private$model$generate(
        prompt = example_prompt,
        system_message = "You are an R programming expert. Create a helpful, realistic example."
      )
      
      # Store tool knowledge
      assign(tool_name, list(
        description = description,
        parameters = params,
        param_details = param_details,
        example = example,
        usage_count = 0,
        last_used = NULL
      ), envir = private$tool_knowledge)
      
      private$log_event(sprintf("Generated documentation for tool '%s'", tool_name))
    },
    
    #' @description Format tool descriptions for planning
    format_tool_descriptions = function() {
      descriptions <- sapply(names(private$tools), function(name) {
        if (exists(name, envir = private$tool_knowledge)) {
          knowledge <- get(name, envir = private$tool_knowledge)
          
          # Format parameter list
          param_desc <- sapply(knowledge$param_details, function(p) {
            if (p$required) {
              sprintf("%s (required)", p$name)
            } else if (p$name == "...") {
              "... (optional additional args)"
            } else {
              sprintf("%s (optional)", p$name)
            }
          })
          
          sprintf("- %s: %s\n  Parameters: %s",
                  name,
                  knowledge$description,
                  paste(param_desc, collapse = ", "))
        } else {
          params <- names(formals(private$tools[[name]]))
          sprintf("- %s\n  Parameters: %s",
                  name,
                  paste(params, collapse = ", "))
        }
      })
      
      paste(descriptions, collapse = "\n\n")
    },
    
    #' @description Format tool descriptions with examples for enhanced planning
    format_tool_descriptions_with_examples = function() {
      descriptions <- sapply(names(private$tools), function(name) {
        if (exists(name, envir = private$tool_knowledge)) {
          knowledge <- get(name, envir = private$tool_knowledge)
          
          # Format parameter list
          param_desc <- sapply(knowledge$param_details, function(p) {
            if (p$required) {
              sprintf("%s (required)", p$name)
            } else if (p$name == "...") {
              "... (optional additional args)"
            } else {
              sprintf("%s (optional)", p$name)
            }
          })
          
          # Include example if available
          example_text <- if (!is.null(knowledge$example)) {
            sprintf("\n  Example: %s", knowledge$example)
          } else {
            ""
          }
          
          sprintf("- %s: %s\n  Parameters: %s%s",
                  name,
                  knowledge$description,
                  paste(param_desc, collapse = ", "),
                  example_text)
        } else {
          params <- names(formals(private$tools[[name]]))
          sprintf("- %s\n  Parameters: %s",
                  name,
                  paste(params, collapse = ", "))
        }
      })
      
      paste(descriptions, collapse = "\n\n")
    },
    
    #' ---------------------- CONTEXT BUILDING ----------------------
    
    #' @description Build context for response generation
    build_context = function(user_input, system_prompt = NULL, context_window = 5, tool_results = NULL) {
      history <- private$short_term_memory$get_recent_messages(context_window)
      
      context_parts <- c()
      
      # Add system prompt if provided
      if (!is.null(system_prompt)) {
        context_parts <- c(
          context_parts,
          paste(sprintf("You are %s.", private$name), system_prompt, sep = "\n\n")
        )
      } else {
        context_parts <- c(context_parts, sprintf("You are %s.", private$name))
      }
      
      # Add conversation history
      if (length(history) > 0) {
        convo <- sapply(history, function(msg) {
          sprintf("%s: %s", toupper(msg$role), msg$content)
        })
        context_parts <- c(context_parts, convo)
      }
      
      # Add tool results if available
      if (!is.null(tool_results)) {
        context_parts <- c(
          context_parts,
          sprintf("\nTool Results:\n%s", tool_results$summary)
        )
      }
      
      # Return the context
      list(
        system_message = if (!is.null(system_prompt)) {
          paste(sprintf("You are %s.", private$name), system_prompt, sep = "\n\n")
        } else {
          sprintf("You are %s.", private$name)
        },
        prompt = paste(context_parts, collapse = "\n\n")
      )
    },
    
    #' @description Generate response using the language model
    generate_response = function(context) {
      response <- private$model$generate(
        prompt = context$prompt,
        system_message = context$system_message
      )
      response
    }
  )
)