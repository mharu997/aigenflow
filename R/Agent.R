#' Advanced Agent Implementation (Simplified with Only Short-Term Memory)
#' A sophisticated R6 class for intelligent tool utilization with enhanced data source handling
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
                          short_term_memory = 20,  # Adjust as needed
                          tools = list(),
                          log_file = NULL,
                          debug_mode = FALSE) {  
      
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
      
      # Initialize only short-term memory with size limits
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
    
    #' @description Process user input and generate response
    chat = function(user_input, system_prompt = NULL, context_window = 5) {
      tryCatch({
        if (!is.character(user_input) || length(user_input) != 1) {
          stop("Invalid user input")
        }
        
        # Update available data sources
        self$register_data_sources()
        
        # Log the incoming request if logging is enabled
        private$log_event(sprintf("Processing request: %s", substr(user_input, 1, 100)))
        
        # Add to memory
        private$short_term_memory$add_message("user", user_input)
        
        # Check for tool requirements
        tool_results <- private$process_tool_request(user_input)
        
        # Build context
        context <- private$build_context(
          user_input = user_input,
          system_prompt = system_prompt,
          context_window = context_window,
          tool_results = tool_results
        )
        
        # Generate response
        response <- private$generate_response(context)
        
        # Add response to memory (Removed transfer to long-term memory)
        private$short_term_memory$add_message("assistant", response)
        
        private$log_event("Response generated successfully")
        
        return(response)
        
      }, error = function(e) {
        private$log_event(sprintf("Error in chat: %s", e$message), "ERROR")
        stop(sprintf("Chat error: %s", e$message))
      })
    },
    
    #' @description Add a tool to the agent's toolkit
    add_tool = function(name, fn) {
      tryCatch({
        if (!is.character(name) || length(name) != 1) {
          stop("Tool name must be a single string")
        }
        if (!is.function(fn)) {
          stop("Tool must be a function")
        }
        
        # Store the tool
        private$tools[[name]] <- fn
        
        # Analyze and store tool metadata
        private$analyze_tool(name)
        
        private$log_event(sprintf("Tool '%s' added successfully", name))
        
        invisible(self)
        
      }, error = function(e) {
        private$log_event(sprintf("Error adding tool: %s", e$message), "ERROR")
        stop(sprintf("Tool addition error: %s", e$message))
      })
    },
    
    #' @description Register available data sources
    register_data_sources = function() {
      # Get all objects in the global environment
      all_objects <- ls(envir = .GlobalEnv)
      
      # Filter for data frames and matrices
      data_sources <- character(0)
      for (obj_name in all_objects) {
        tryCatch({
          obj <- get(obj_name, envir = .GlobalEnv)
          if (is.data.frame(obj) || is.matrix(obj)) {
            data_sources <- c(data_sources, obj_name)
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
    
    #' @description Execute a tool by name
    use_tool = function(tool_name, ...) {
      # Validate tool existence
      if (!tool_name %in% names(private$tools)) {
        stop(sprintf("Tool '%s' not found", tool_name))
      }
      
      # Get the tool function
      fn <- private$tools[[tool_name]]
      
      # Log the attempt
      private$log_event(sprintf("Attempting to execute tool: %s", tool_name))
      
      tryCatch({
        # Collect all arguments into a list
        args <- list(...)
        
        # Execute the tool with provided arguments
        result <- do.call(fn, args)
        
        # Update tool usage statistics if metadata exists
        if (exists(tool_name, envir = private$tool_knowledge)) {
          tool_info <- get(tool_name, envir = private$tool_knowledge)
          tool_info$usage_count <- tool_info$usage_count + 1
          tool_info$last_used <- Sys.time()
          assign(tool_name, tool_info, envir = private$tool_knowledge)
          
          # Log the successful update
          private$log_event(sprintf("Updated usage stats for tool '%s' (count: %d)", 
                                    tool_name, 
                                    tool_info$usage_count))
        }
        
        # Log success
        private$log_event(sprintf("Successfully executed tool '%s'", tool_name))
        
        return(result)
        
      }, error = function(e) {
        # Log the error
        private$log_event(sprintf("Error executing tool '%s': %s", tool_name, e$message), "ERROR")
        stop(sprintf("Error executing %s: %s", tool_name, e$message))
      })
    }
  ),
  
  private = list(
    name = NULL,
    model = NULL,
    debug_mode = NULL,
    log_file = NULL,
    tools = NULL,
    tool_knowledge = NULL,
    short_term_memory = NULL,  # Retained
    # long_term_memory = NULL,  # Removed
    available_data_sources = NULL,
    
    #' @description Log events with timestamp if logging is enabled
    log_event = function(message, level = "INFO") {
      # Only log if log_file is set
      if (!is.null(private$log_file)) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        log_entry <- sprintf("[%s] [%s] %s\n", timestamp, level, message)
        cat(log_entry, file = private$log_file, append = TRUE)
        if (private$debug_mode) cat(log_entry)
      }
    },
    
    #' @description Process tool requests
    process_tool_request = function(user_input) {
      # Generate tool execution plan
      plan <- private$generate_tool_plan(user_input)
      
      if (is.null(plan)) {
        return(NULL)
      }
      
      # Execute tool
      result <- tryCatch({
        if (!plan$tool %in% names(private$tools)) {
          stop(sprintf("Tool '%s' not found", plan$tool))
        }
        
        # Get the tool function
        tool_fn <- private$tools[[plan$tool]]
        
        # Process parameters
        params <- private$process_parameters(plan$parameters)
        
        # Execute the tool
        private$log_event(sprintf("Executing tool '%s'", plan$tool))
        output <- do.call(tool_fn, params)
        
        list(
          tool = plan$tool,
          results = output,
          summary = paste(capture.output(print(output)), collapse = "\n")
        )
        
      }, error = function(e) {
        private$log_event(sprintf("Tool execution error: %s", e$message), "ERROR")
        NULL
      })
      
      return(result)
    },
    
    #' @description Generate tool execution plan with data source awareness
    generate_tool_plan = function(user_input) {
      if (length(private$tools) == 0) return(NULL)
      
      # Build planning prompt with available data sources
      planning_prompt <- sprintf(
        "User Input: %s\n\nAvailable Tools:\n%s\n\nAvailable Data Sources:\n%s\n\nProvide a plan in this format:\nTool: [tool_name]\nParameters:\n- [param]: [value]",
        user_input,
        private$format_tool_descriptions(),
        paste(private$available_data_sources, collapse = ", ")
      )
      
      plan_response <- private$model$generate(
        prompt = planning_prompt,
        system_message = sprintf("You are %s. Plan tool usage.", private$name)
      )
      
      return(private$parse_tool_plan(plan_response))
    },
    
    #' @description Parse tool execution plan with enhanced robustness
    parse_tool_plan = function(plan_response) {
      if (is.null(plan_response) || nchar(trimws(plan_response)) == 0) {
        private$log_event("Received empty tool plan response", "WARNING")
        return(NULL)
      }
      
      # Initialize result
      result <- list(tool = NULL, parameters = list())
      
      # Split response into lines and clean
      lines <- strsplit(plan_response, "\n")[[1]]
      lines <- trimws(lines[nchar(trimws(lines)) > 0])  # Remove empty lines
      
      if (length(lines) == 0) {
        private$log_event("No valid content in tool plan", "WARNING")
        return(NULL)
      }
      
      # Extract tool name - more flexible pattern matching
      tool_pattern <- "^(?:Tool|tool|USE|use|Using|using)?:?\\s*(\\w+)\\s*$"
      tool_line <- grep(tool_pattern, lines, value = TRUE, perl = TRUE)[1]
      
      if (!is.na(tool_line)) {
        tool_name <- gsub(tool_pattern, "\\1", tool_line, perl = TRUE)
        tool_name <- trimws(tool_name)
        
        # Validate tool exists
        if (tool_name %in% names(private$tools)) {
          result$tool <- tool_name
        } else {
          private$log_event(sprintf("Specified tool '%s' not found", tool_name), "WARNING")
          return(NULL)
        }
      } else {
        private$log_event("No tool specified in plan", "WARNING")
        return(NULL)
      }
      
      # Extract parameters with improved handling
      param_section_start <- grep("^(?:Parameters|params|arguments|args)\\s*:?\\s*$", 
                                  lines, 
                                  ignore.case = TRUE, 
                                  perl = TRUE)[1]
      
      if (!is.na(param_section_start) && param_section_start < length(lines)) {
        # Get all parameter lines
        param_lines <- lines[(param_section_start + 1):length(lines)]
        
        # Parse parameters
        for (line in param_lines) {
          # More flexible parameter pattern matching
          param_pattern <- "^[-*]?\\s*([\\w_]+)\\s*[:=]\\s*(.+)$"
          if (grepl(param_pattern, line, perl = TRUE)) {
            matches <- regexec(param_pattern, line, perl = TRUE)
            parts <- regmatches(line, matches)[[1]]
            
            if (length(parts) >= 3) {
              param_name <- trimws(parts[2])
              param_value <- trimws(parts[3])
              
              # Validate parameter against tool definition
              tool_params <- names(formals(private$tools[[result$tool]]))
              if (param_name %in% tool_params) {
                result$parameters[[param_name]] <- param_value
              } else {
                private$log_event(
                  sprintf("Unknown parameter '%s' for tool '%s'", param_name, result$tool),
                  "WARNING"
                )
              }
            }
          }
        }
        
        # Validate required parameters are present
        tool_params <- formals(private$tools[[result$tool]])
        required_params <- names(tool_params)[sapply(tool_params, is.symbol)]
        missing_params <- setdiff(required_params, names(result$parameters))
        
        if (length(missing_params) > 0) {
          private$log_event(
            sprintf("Missing required parameters for '%s': %s", 
                    result$tool, 
                    paste(missing_params, collapse = ", ")),
            "WARNING"
          )
          return(NULL)
        }
      } else {
        private$log_event("No parameters section found in plan", "WARNING")
        return(NULL)
      }
      
      if (length(result$parameters) == 0) {
        private$log_event("No valid parameters found in plan", "WARNING")
        return(NULL)
      }
      
      private$log_event(sprintf("Successfully parsed tool plan for '%s' with %d parameters", 
                                result$tool, 
                                length(result$parameters)))
      
      return(result)
    },
    
    #' @description Process and validate parameters with enhanced data source handling
    process_parameters = function(params) {
      processed <- list()
      
      for (name in names(params)) {
        value <- params[[name]]
        
        if (is.character(value)) {
          # Check if value references a known data source
          if (value %in% private$available_data_sources) {
            tryCatch({
              # Attempt to get the referenced data
              referenced_data <- get(value, envir = .GlobalEnv)
              
              # Verify it's a data structure we can work with
              if (is.data.frame(referenced_data) || is.matrix(referenced_data)) {
                value <- referenced_data
                private$log_event(sprintf("Successfully resolved data source: %s", value))
              } else {
                private$log_event(
                  sprintf("Referenced object %s is not a data frame or matrix", value),
                  "WARNING"
                )
              }
            }, error = function(e) {
              private$log_event(
                sprintf("Error accessing data source %s: %s", value, e$message),
                "WARNING"
              )
            })
          }
          # Handle other special cases
          else if (value %in% c("TRUE", "FALSE")) {
            value <- as.logical(value)
          }
          else if (grepl("^\\d+(\\.\\d+)?$", value)) {
            value <- as.numeric(value)
          }
        }
        
        processed[[name]] <- value
      }
      
      return(processed)
    },
    
    #' @description Analyze tool and store metadata
    analyze_tool = function(tool_name) {
      fn <- private$tools[[tool_name]]
      params <- names(formals(fn))
      
      # Generate tool documentation
      doc_prompt <- sprintf(
        "Function name: %s\nParameters: %s\nAnalyze this function and provide a brief description.",
        tool_name,
        paste(params, collapse = ", ")
      )
      
      description <- private$model$generate(
        prompt = doc_prompt,
        system_message = "You are a technical documentation expert."
      )
      
      # Store tool knowledge
      assign(tool_name, list(
        description = description,
        parameters = params,
        usage_count = 0,
        last_used = NULL
      ), envir = private$tool_knowledge)
    },
    
    #' @description Format tool descriptions for prompts
    format_tool_descriptions = function() {
      descriptions <- sapply(names(private$tools), function(name) {
        knowledge <- get(name, envir = private$tool_knowledge)
        sprintf("- %s: %s\n  Parameters: %s",
                name,
                knowledge$description,
                paste(knowledge$parameters, collapse = ", "))
      })
      
      paste(descriptions, collapse = "\n\n")
    },
    
    #' @description Build context for response generation
    build_context = function(user_input, system_prompt = NULL, 
                             context_window = 5, tool_results = NULL) {
      # Get recent conversation history using memory class
      history <- private$short_term_memory$get_recent_messages(context_window)
      
      # Build context string
      context_parts <- c()
      
      # Add system prompt if provided
      if (!is.null(system_prompt)) {
        context_parts <- c(context_parts, 
                           paste(sprintf("You are %s.", private$name), system_prompt, sep = "\n\n"))
      } else {
        context_parts <- c(context_parts, sprintf("You are %s.", private$name))
      }
      
      # Add conversation history
      if (length(history) > 0) {
        context_parts <- c(context_parts,
                           sapply(history, function(msg) {
                             sprintf("%s: %s", toupper(msg$role), msg$content)
                           }))
      }
      
      # Add tool results if available
      if (!is.null(tool_results)) {
        context_parts <- c(context_parts,
                           sprintf("\nTool Results:\n%s", tool_results$summary))
      }
      
      # Combine all parts
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
      
      return(response)
    }
  )
)