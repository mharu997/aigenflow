library("R6")
library("httr")
library("jsonlite")

#' @title Agent Class
#' @description Agent class for an agent that interacts with language models and tools.
#' @export
aigen_Agent <- R6Class(
  "aigen_Agent",
  public = list(
    #' @description Initialize a new agent with a language model and optional memory and tools
    #' @param model An instance of aigen_LanguageModel
    #' @param name Character string for agent name
    #' @param memory Optional ConversationMemory instance
    #' @param tools Named list of functions to use as tools
    initialize = function(model, 
                          name = "Assistant",
                          memory = NULL, 
                          tools = list()) {
      if (!inherits(model, "aigen_LanguageModel")) {
        stop("model must be an instance of aigen_LanguageModel")
      }
      
      if (!is.character(name) || length(name) != 1) {
        stop("name must be a single character string")
      }
      
      if (length(tools) > 0) {
        if (is.null(names(tools))) {
          stop("tools must be a named list of functions")
        }
        for (tool_name in names(tools)) {
          if (!is.function(tools[[tool_name]])) {
            stop(sprintf("Tool '%s' must be a function", tool_name))
          }
        }
      }
      
      private$name <- name
      private$model <- model
      private$memory <- if (is.null(memory)) ConversationMemory$new() else memory
      private$tools <- tools
      private$tool_knowledge <- new.env(parent = emptyenv())
      
      if (length(tools) > 0) {
        for (name in names(tools)) {
          private$analyze_tool(name)
        }
      }
    },
    
    #' @description Get the agent's name
    get_name = function() {
      return(private$name)
    },
    
    #' @description Set the agent's name
    set_name = function(new_name) {
      if (!is.character(new_name) || length(new_name) != 1) {
        stop("new_name must be a single character string")
      }
      private$name <- new_name
      invisible(self)
    },
    
    #' @description Process user input and generate a response
    chat = function(user_input, system_prompt = NULL, context_window = 5) {
      if (!is.character(user_input) || length(user_input) != 1) {
        stop("user_input must be a single character string")
      }
      if (!is.null(system_prompt) && (!is.character(system_prompt) || length(system_prompt) != 1)) {
        stop("system_prompt must be NULL or a single character string")
      }
      if (!is.numeric(context_window) || context_window < 1) {
        stop("context_window must be a positive number")
      }
      
      private$memory$add_message("user", user_input)
      
      # If we have tools, try to use them first
      tool_response <- NULL
      if (length(private$tools) > 0) {
        tool_result <- private$try_tool_execution(user_input)
        if (!is.null(tool_result)) {
          tool_response <- sprintf("%s used %s: %s", 
                                   private$name,
                                   tool_result$tool_name, 
                                   as.character(tool_result$result))
          
          # For concise requests, just return the tool response
          if (!is.null(system_prompt) && 
              grepl("concise", tolower(system_prompt), fixed = TRUE)) {
            private$memory$add_message("assistant", tool_response)
            return(tool_response)
          }
        }
      }
      
      # Build system prompt with tool context and agent identity
      enhanced_system_prompt <- if (!is.null(system_prompt)) {
        paste(sprintf("You are %s. ", private$name), 
              system_prompt, "\n\n", 
              private$build_tool_context())
      } else {
        paste(sprintf("You are %s. ", private$name),
              private$build_tool_context())
      }
      
      messages <- self$build_prompt(enhanced_system_prompt, context_window)
      
      # Generate response based on whether a tool was used
      final_prompt <- if (!is.null(tool_response)) {
        sprintf(
          "%s\n\nI've already performed this calculation: %s\nPlease provide a complete response addressing all aspects of the user's request, incorporating this result.",
          messages$prompt,
          tool_response
        )
      } else {
        messages$prompt
      }
      
      response <- private$model$generate(
        prompt = final_prompt,
        system_message = messages$system
      )
      
      private$memory$add_message("assistant", response)
      return(response)
    },
    
    #' @description Build prompt from conversation history
    build_prompt = function(system_prompt = NULL, context_window = 5) {
      recent_messages <- private$memory$get_recent_messages(context_window * 2)
      prompt <- paste(sapply(recent_messages, function(msg) {
        sprintf("%s: %s", toupper(msg$role), msg$content)
      }), collapse = "\n")
      
      return(list(
        prompt = prompt,
        system = system_prompt
      ))
    },
    
    #' @description Add a tool to the agent's toolkit
    add_tool = function(name, fn) {
      if (!is.character(name) || length(name) != 1) {
        stop("name must be a single character string")
      }
      if (!is.function(fn)) {
        stop("fn must be a function")
      }
      private$tools[[name]] <- fn
      private$analyze_tool(name)
    },
    
    #' @description Execute a tool by name
    use_tool = function(tool_name, ...) {
      fn <- private$tools[[tool_name]]
      if (is.null(fn)) {
        stop(sprintf("Tool '%s' not found", tool_name))
      }
      
      tryCatch({
        result <- do.call(fn, list(...))
        
        if (exists(tool_name, envir = private$tool_knowledge)) {
          tool_info <- get(tool_name, envir = private$tool_knowledge)
          tool_info$usage_count <- tool_info$usage_count + 1
          tool_info$last_used <- Sys.time()
          assign(tool_name, tool_info, envir = private$tool_knowledge)
        }
        
        return(result)
      }, error = function(e) {
        stop(sprintf("Error executing %s: %s", tool_name, e$message))
      })
    },
    
    #' @description List all available tools
    list_tools = function() {
      if (length(private$tools) == 0) {
        return(list())
      }
      
      tool_info <- lapply(names(private$tools), function(name) {
        knowledge <- get(name, envir = private$tool_knowledge)
        list(
          name = name,
          description = knowledge$description,
          usage_count = knowledge$usage_count,
          last_used = knowledge$last_used
        )
      })
      
      names(tool_info) <- names(private$tools)
      return(tool_info)
    },
    
    #' @description Clear the conversation memory
    clear_memory = function() {
      private$memory$clear_memory()
    },
    
    #' @description Get the usage statistics for tools
    get_tool_stats = function() {
      if (length(private$tools) == 0) {
        return(data.frame(
          tool_name = character(),
          usage_count = integer(),
          last_used = character(),
          stringsAsFactors = FALSE
        ))
      }
      
      stats_list <- lapply(names(private$tools), function(name) {
        knowledge <- get(name, envir = private$tool_knowledge)
        data.frame(
          tool_name = name,
          usage_count = as.integer(knowledge$usage_count %||% 0),
          last_used = if (!is.null(knowledge$last_used)) 
            as.character(knowledge$last_used) 
          else 
            NA_character_,
          stringsAsFactors = FALSE
        )
      })
      
      do.call(rbind, stats_list)
    },
    
    #' @description Get agent information including name and tools
    get_info = function() {
      list(
        name = private$name,
        tools_available = names(private$tools),
        tool_stats = self$get_tool_stats(),
        memory_size = length(private$memory$get_recent_messages(Inf))
      )
    }
  ),
  
  private = list(
    name = NULL,
    model = NULL,           
    memory = NULL,          
    tools = NULL,           
    tool_knowledge = NULL,  
    
    analyze_tool = function(tool_name) {
      fn <- private$tools[[tool_name]]
      params <- names(formals(fn))
      body_text <- deparse(body(fn))
      
      analysis_prompt <- sprintf(
        "Analyze this R function:\nName: %s\nParameters: %s\nBody: %s\n\nProvide a clear, concise description of what this function does and how to use it.",
        tool_name,
        paste(params, collapse = ", "),
        paste(body_text, collapse = " ")
      )
      
      understanding <- private$model$generate(
        prompt = analysis_prompt,
        system_message = "You are an expert R programmer. Analyze this function concisely."
      )
      
      assign(tool_name, list(
        description = understanding,
        params = params,
        usage_count = 0,
        last_used = NULL
      ), envir = private$tool_knowledge)
    },
    
    try_tool_execution = function(user_input) {
      tool_context <- private$build_tool_context()
      decision_prompt <- sprintf(
        "Given the user input: '%s'\n\nAvailable tools:\n%s\n\nRespond in this exact format if a tool should be used:\nUSE_TOOL|tool_name|param1=value1,param2=value2\nOr respond with NO_TOOL if no tool is appropriate.\nOnly respond with the format above, no other text.",
        user_input,
        tool_context
      )
      
      decision <- private$model$generate(
        prompt = decision_prompt,
        system_message = sprintf("You are %s, a tool execution planner. Only decide if and how to use available tools.", 
                                 private$name)
      )
      
      if (startsWith(decision, "USE_TOOL|")) {
        parts <- strsplit(decision, "\\|")[[1]]
        tool_name <- parts[2]
        params_str <- parts[3]
        
        if (tool_name %in% names(private$tools)) {
          params <- lapply(strsplit(params_str, ",")[[1]], function(p) {
            param_parts <- strsplit(p, "=")[[1]]
            if (length(param_parts) == 2) {
              val <- tryCatch({
                eval(parse(text = param_parts[2]))
              }, error = function(e) param_parts[2])
              return(setNames(list(val), param_parts[1]))
            }
            return(NULL)
          })
          params <- do.call(c, params)
          
          result <- tryCatch({
            tool_result <- do.call(private$tools[[tool_name]], params)
            
            tool_info <- get(tool_name, envir = private$tool_knowledge)
            tool_info$usage_count <- tool_info$usage_count + 1
            tool_info$last_used <- Sys.time()
            assign(tool_name, tool_info, envir = private$tool_knowledge)
            
            list(tool_name = tool_name, result = tool_result)
          }, error = function(e) NULL)
          
          return(result)
        }
      }
      
      return(NULL)
    },
    
    build_tool_context = function() {
      if (length(private$tools) == 0) return("")
      
      tool_descriptions <- sapply(names(private$tools), function(name) {
        knowledge <- get(name, envir = private$tool_knowledge)
        usage_info <- if (knowledge$usage_count > 0) {
          sprintf(" (used %d times)", knowledge$usage_count)
        } else {
          ""
        }
        sprintf("- %s%s: %s. Parameters: (%s)", 
                name, 
                usage_info, 
                knowledge$description,
                paste(knowledge$params, collapse = ", "))
      })
      
      paste(
        sprintf("Available tools for %s:", private$name),
        paste(tool_descriptions, collapse = "\n"),
        "\nTo use a tool, specify it in the format: USE_TOOL|tool_name|param1=value1,param2=value2",
        sep = "\n"
      )
    }
  )
)
