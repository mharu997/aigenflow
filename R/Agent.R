library(R6)          
library(httr)        
library(jsonlite) 

#' @title Agent Class
#' @description Agent class for an agent that interacts with language models and tools.
#' @export
aigen_Agent <- R6Class(
  "aigen_Agent",
  public = list(
    #' @description Initialize a new agent with a language model and optional memory and tools
    initialize = function(model, memory = NULL, tools = list()) {
      if (!inherits(model, "aigen_LanguageModel")) {
        stop("model must be an instance of aigen_LanguageModel")
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
          tool_response <- sprintf("Used %s: %s", 
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
      
      # Build system prompt with tool context
      enhanced_system_prompt <- if (!is.null(system_prompt)) {
        paste(system_prompt, "\n\n", private$build_tool_context())
      } else {
        private$build_tool_context()
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
    
    clear_memory = function() {
      private$memory$clear_memory()
    },
    
    get_tool_stats = function() {
      if (length(private$tools) == 0) {
        return(data.frame())
      }
      
      stats <- do.call(rbind, lapply(names(private$tools), function(name) {
        knowledge <- get(name, envir = private$tool_knowledge)
        data.frame(
          tool_name = name,
          usage_count = knowledge$usage_count,
          last_used = as.character(knowledge$last_used),
          stringsAsFactors = FALSE
        )
      }))
      
      return(stats)
    }
  ),
  
  private = list(
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
        system_message = "You are a tool execution planner. Only decide if and how to use available tools."
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
        "Available tools:",
        paste(tool_descriptions, collapse = "\n"),
        "\nTo use a tool, specify it in the format: USE_TOOL|tool_name|param1=value1,param2=value2",
        sep = "\n"
      )
    }
  )
)
