#' @import httr
#' @import jsonlite
#' @import R6
#' @import magrittr
#' @import logger
#' @import future
library("jsonlite")
library("future")
library("logger")
library("R6")
library("httr")
library("magrittr")


############################################
# Model Wrappers (OpenAI, Anthropic, Azure)
############################################

#' Create a new OpenAI model instance
#' 
#' @param model_name Character string for model name (e.g., "gpt-4")
#' @param api_key Character string for API key, defaults to OPENAI_API_KEY environment variable
#' @param max_tokens Integer for maximum tokens in response
#' @param temperature Float between 0 and 1 controlling randomness
#' @return An OpenAIModel instance
#' @export
OpenAIModel <- function(model_name, api_key = Sys.getenv("OPENAI_API_KEY"), 
                        max_tokens = 1000, temperature = 0.7) {
  # Basic input validation
  if (!is.character(model_name)) stop("model_name must be a character string")
  if (!is.numeric(temperature) || temperature < 0 || temperature > 1) {
    stop("temperature must be a number between 0 and 1")
  }
  if (!is.numeric(max_tokens) || max_tokens < 1) {
    stop("max_tokens must be a positive integer")
  }
  
  model_instance = aigen_OpenAIModel$new(
    model_name = model_name, 
    api_key = api_key, 
    max_tokens = as.integer(max_tokens), 
    temperature = temperature
  )
}


#' Create a new Anthropic model instance
#' 
#' @param model_name Character string for model name (e.g., "claude-3-opus-20240229")
#' @param api_key Character string for API key, defaults to ANTHROPIC_API_KEY environment variable
#' @param max_tokens Integer for maximum tokens in response
#' @param temperature Float between 0 and 1 controlling randomness
#' @return An AnthropicModel instance
#' @export
AnthropicModel <- function(model_name = "claude-3-opus-20240229", 
                           api_key = Sys.getenv("ANTHROPIC_API_KEY"), 
                           max_tokens = 1000, 
                           temperature = 0.7) {
  if (!is.character(model_name)) stop("model_name must be a character string")
  if (!is.numeric(temperature) || temperature < 0 || temperature > 1) {
    stop("temperature must be a number between 0 and 1")
  }
  if (!is.numeric(max_tokens) || max_tokens < 1) {
    stop("max_tokens must be a positive integer")
  }
  
  aigen_AnthropicModel$new(
    model_name = model_name,
    api_key = api_key,
    max_tokens = as.integer(max_tokens),
    temperature = temperature
  )
}



#' Create a new Azure OpenAI model instance
#' 
#' @param deployment_name Character string for Azure deployment name
#' @param resource_name Character string for Azure resource name
#' @param api_version Character string for Azure API version
#' @param api_key Character string for API key, defaults to AZURE_OPENAI_KEY environment variable
#' @param max_tokens Integer for maximum tokens in response
#' @param temperature Float between 0 and 1 controlling randomness
#' @return An AzureOpenAIModel instance
#' @export
AzureOpenAIModel <- function(deployment_name,
                             resource_name,
                             api_version = "2024-02-15-preview",
                             api_key = Sys.getenv("AZURE_OPENAI_KEY"),
                             max_tokens = 1000,
                             temperature = 0.7) {
  # Input validation
  if (!is.character(deployment_name)) stop("deployment_name must be a character string")
  if (!is.character(resource_name)) stop("resource_name must be a character string")
  if (!is.character(api_version)) stop("api_version must be a character string")
  if (!is.numeric(temperature) || temperature < 0 || temperature > 1) {
    stop("temperature must be a number between 0 and 1")
  }
  if (!is.numeric(max_tokens) || max_tokens < 1) {
    stop("max_tokens must be a positive integer")
  }
  
  aigen_AzureOpenAIModel$new(
    deployment_name = deployment_name,
    resource_name = resource_name,
    api_version = api_version,
    api_key = api_key,
    max_tokens = as.integer(max_tokens),
    temperature = temperature
  )
}



#' Create an Azure ML endpoint model instance
#'
#' This function creates a new Azure ML endpoint model instance with the specified
#' configuration. It supports different types of models including chat models,
#' completion models, and embedding models.
#' @param endpoint_url The Azure ML endpoint URL
#' @param model_type Type of model ("chat", "completion", or "embedding")
#' @param api_key Azure ML API key
#' @param max_tokens Maximum tokens in response
#' @param temperature Sampling temperature
#' @return An AzureMLEndpointModel instance
#' @export
AzureMLEndpoint <- function(endpoint_url,
                            model_type = c("chat", "completion", "embedding"),
                            api_key = Sys.getenv("AZURE_ML_KEY"),
                            max_tokens = 1000,
                            temperature = 0.7) {
  # Validate model_type first
  tryCatch({
    model_type <- match.arg(model_type)
  }, error = function(e) {
    stop("arg should be one of 'chat', 'completion', 'embedding'")
  })
  
  # Validate endpoint URL
  if (is.null(endpoint_url) || endpoint_url == "") {
    stop("Azure ML endpoint URL is required.")
  }
  
  # Validate numeric parameters
  if (!is.numeric(max_tokens) || max_tokens < 1) {
    stop("max_tokens must be a positive integer")
  }
  if (!is.numeric(temperature) || temperature < 0 || temperature > 1) {
    stop("temperature must be a number between 0 and 1")
  }
  
  # Create and return the model instance
  aigen_AzureMLEndpointModel$new(
    endpoint_url = endpoint_url,
    model_type = model_type,
    api_key = api_key,
    max_tokens = as.integer(max_tokens),
    temperature = temperature
  )
}


#' Create a Llama 2 endpoint instance
#' @param endpoint_url URL of the Llama 2 endpoint
#' @param api_key API key for authentication
#' @param max_tokens Maximum tokens for response
#' @param temperature Temperature for response randomness (0-1)
#' @return An AzureMLEndpointModel instance
#' @examples
#' \dontrun{
#' # Create a Llama 2 model endpoint
#' model <- Llama2Endpoint(
#'  endpoint_url = "https://your-endpoint.azureml.ms/endpoints/your-model/invoke"
#'  )
#'  # Generate a response
#'  response <- model$generate(
#'  prompt = "What is machine learning?",
#'  system_message = "You are a helpful AI assistant."
#'  )
#'  }
#'  @export
#'  Create a Llama 2 endpoint instance
Llama2Endpoint <- function(endpoint_url, 
                           api_key = Sys.getenv("AZURE_ML_KEY"),
                           max_tokens = 1000,
                           temperature = 0.7) {
  aigen_AzureMLEndpointModel$new(
    endpoint_url = endpoint_url,
    model_type = "chat",
    api_key = api_key,
    max_tokens = max_tokens,
    temperature = temperature
  )
}

#' Create a Mistral endpoint instance
#' @param endpoint_url The endpoint URL
#' @param api_key API key
#' @param max_tokens Maximum tokens in response
#' @param temperature Sampling temperature
#' @return An AzureMLEndpointModel instance configured for Mistral
#' @export
MistralEndpoint <- function(endpoint_url, 
                            api_key = Sys.getenv("AZURE_ML_KEY"),
                            max_tokens = 1000,
                            temperature = 0.7) {
  aigen_AzureMLEndpointModel$new(
    endpoint_url = endpoint_url,
    model_type = "chat",
    api_key = api_key,
    max_tokens = max_tokens,
    temperature = temperature
  )
}


#' Null coalesce operator
#' @title Null Coalescing Operator
#' @description Returns `y` if `x` is `NULL`; otherwise returns `x`.
#' @param x The value to check for `NULL`.
#' @param y The fallback value if `x` is `NULL`.
#' @return Returns `x` if it is not `NULL`, otherwise `y`.
#' @private
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Initialize an Advanced Conversational Agent
#'
#' @param model An instance of `aigen_LanguageModel`. This is the language model the agent will use.
#' @param name A character string representing the agent's name. Default is "Assistant".
#' @param short_term_memory An integer specifying the maximum number of messages to retain in short-term memory. Default is 20.
#' @param tools A named list of functions representing tools the agent can utilize. Default is an empty list.
#' @param log_file A character string specifying the path to the log file. If NULL, logging is disabled. Default is NULL.
#' @param debug_mode A logical value indicating whether to enable debug mode. If TRUE, logs are printed to the console. Default is FALSE.
#'
#' @return An instance of `AdvancedAgent`.
#' @examples
#' \dontrun{
#' # Assuming LimitedConversationMemory and aigen_LanguageModel are defined elsewhere
#' 
#' # Initialize a language model (placeholder)
#' language_model <- OpenAIModel("gpt-4o")
#' 
#' # Define some tools
#' summarize_tool <- function(text) {
#'   summary <- summary(text)
#'   return(summary)
#' }
#' 
#' stats_tool <- function(data) {
#'   stats <- summary(data)
#'   return(stats)
#' }
#' 
#' # Initialize the agent using the wrapper
#' agent <- Agent(
#'   model = language_model,
#'   name = "DataAssistant",
#'   short_term_memory = 30,
#'   tools = list(
#'     summarize = summarize_tool,
#'     stats = stats_tool
#'   ),
#'   log_file = "agent.txt",
#'   debug_mode = TRUE
#' )
#' 
#' # Interact with the agent
#' response <- agent$chat("Analyze my dataset for trends.")
#' print(response)
#' }
Agent <- function(model,
                             name = "Assistant",
                             short_term_memory = 20,
                             tools = list(),
                             log_file = NULL,
                             debug_mode = FALSE) {
  
  # Input Validation
  if (missing(model)) {
    stop("Parameter 'model' is required and must be an instance of aigen_LanguageModel.")
  }
  
  if (!inherits(model, "aigen_LanguageModel")) {
    stop("Parameter 'model' must be an instance of aigen_LanguageModel.")
  }
  
  if (!is.character(name) || length(name) != 1) {
    stop("Parameter 'name' must be a single character string.")
  }
  
  if (!is.numeric(short_term_memory) || length(short_term_memory) != 1 || short_term_memory <= 0) {
    stop("Parameter 'short_term_memory' must be a positive integer.")
  }
  
  if (!is.list(tools) || (length(tools) > 0 && is.null(names(tools)))) {
    stop("Parameter 'tools' must be a named list of functions.")
  }
  
  if (!is.null(log_file) && (!is.character(log_file) || length(log_file) != 1)) {
    stop("Parameter 'log_file' must be a single character string or NULL.")
  }
  
  if (!is.logical(debug_mode) || length(debug_mode) != 1) {
    stop("Parameter 'debug_mode' must be a single logical value (TRUE or FALSE).")
  }
  
  # Initialize the AdvancedAgent
  agent <- aigen_Agent$new(
    model = model,
    name = name,
    short_term_memory = short_term_memory,
    tools = tools,
    log_file = log_file,
    debug_mode = debug_mode
  )
  
  return(agent)
}

#' Add tools to an existing Agent instance
#' 
#' @param agent An Agent instance
#' @param tools Named list of functions or character vector of function names
#' @param tools_env Environment to look for tool functions
#' @return The modified Agent instance (invisibly)
#' @export
add_tools <- function(agent, tools, tools_env = parent.frame()) {
  if (!inherits(agent, "aigen_Agent")) {
    stop("agent must be an Agent instance")
  }
  
  # Process each tool
  if (is.character(tools)) {
    # Convert names to functions
    for (i in seq_along(tools)) {
      fname <- tools[i]
      name <- names(tools)[i] %||% fname
      
      if (exists(fname, envir = tools_env, mode = "function")) {
        agent$add_tool(name, get(fname, envir = tools_env, mode = "function"))
      } else {
        stop(sprintf("Function '%s' not found", fname))
      }
    }
  } else if (is.list(tools) && !is.null(names(tools))) {
    # Add each function directly
    for (name in names(tools)) {
      agent$add_tool(name, tools[[name]])
    }
  } else {
    stop("tools must be either a named list of functions or a character vector of function names")
  }
  
  invisible(agent)
}






####################################################
# Function to enable %>% operators with Agents
####################################################

#' Enable piping for AI models, agents, and data
#' @importFrom magrittr %>%
#' Make models pipe-friendly
#' @private
`%>%.aigen_LanguageModel` <- function(model, f, ...) {
  if (inherits(f, "function") && identical(f, Agent)) {
    f(model, ...)
  } else {
    magrittr::freduce(model, list(f))
  }
}

#' Make agents pipe-friendly
#' @private
`%>%.aigen_Agent` <- function(agent, f, ...) {
  if (is.function(f) && identical(f, ask)) {
    f(agent, ...)
  } else {
    magrittr::freduce(agent, list(f))
  }
}



#' Response object for chaining
#' @export
ai_response <- function(agent, response, history = list()) {
  structure(
    list(
      agent = agent,
      response = response,
      history = history  # Track chain of responses
    ),
    class = "ai_response"
  )
}


#' Ask function for direct questions with string interpolation
#' @param agent An Agent instance
#' @param template Question template with variables in curly braces
#' @param ... Variables to interpolate into template
#' @param system_prompt Optional system instructions
#' @param context_window Number of messages to include for context
#' @param verbose Whether to print detailed output
#' @export
ask <- function(agent, user_input, system_prompt = NULL, context_window = 5, verbose = TRUE) {
  if (!inherits(agent, "aigen_Agent")) {
    stop("First argument must be an Agent")
  }
  
  # Check if we're being called from mutate
  in_mutate <- inherits(user_input, "numeric") || inherits(user_input, "character") && length(user_input) > 1
  
  if (in_mutate) {
    # Vectorized operation for mutate
    return(vapply(user_input, function(p) {
      response <- agent$chat(p, system_prompt, context_window)
      if (verbose) {
        cat("\n=== Ask:", agent$get_name(), "===\n")
        cat("Input:", p, "\n")
        if (!is.null(system_prompt)) cat("System:", system_prompt, "\n")
        cat("Response:\n", response, "\n")
        cat("===============================\n")
      }
      response
    }, character(1)))
  } else {
    # Regular single prompt operation
    response <- agent$chat(user_input, system_prompt, context_window)
    
    if (verbose) {
      cat("\n=== Ask:", agent$get_name(), "===\n")
      cat("Input:", user_input, "\n")
      if (!is.null(system_prompt)) cat("System:", system_prompt, "\n")
      cat("Response:\n", response, "\n")
      cat("===============================\n")
    }
    
    # Return just the response when in a mutate context
    if (identical(topenv(), .GlobalEnv)) {
      ai_response(agent, response)
    } else {
      response
    }
  }
}


#' Get response history
#' @export
get_history <- function(x) {
  if (inherits(x, "ai_response")) {
    x$history
  } else {
    NULL
  }
}

#' Get just the response from an ai_response object
#' @export
get_response <- function(x) {
  if (inherits(x, "ai_response")) {
    x$response
  } else {
    x  # Return unchanged if not ai_response
  }
}









######################################################
# Model Orchestrators (Orchestrate, Design, Execute)
######################################################
############################################
# High-Level Workflow API
############################################

#' Create a workflow step configuration
#' @param name Character string identifying the step
#' @param agent Character string identifying the agent to use
#' @param prompt The prompt template for this step
#' @param system Optional system prompt for the step
#' @param pass_to_next Logical, whether to pass output to next step
#' @return A list containing step configuration
#' @export 
Step <- function(name, agent, prompt, 
                 system = NULL, 
                 pass_to_next = FALSE) {
  if (!is.character(name) || !is.character(agent) || !is.character(prompt)) {
    stop("name, agent, and prompt must be character strings")
  }
  
  list(
    name = name,
    agent = agent,
    prompt = prompt,
    system_prompt = system,
    output_to_input = pass_to_next
  )
}

#' Create a workflow definition
#' @param ... Step configurations created by Step()
#' @return A list containing the workflow definition
#' @export
Workflow <- function(...) {
  steps <- list(...)
  if (length(steps) == 0) {
    stop("Workflow must contain at least one step")
  }
  
  # Validate steps
  if (!all(sapply(steps, function(s) {
    all(c("name", "agent", "prompt") %in% names(s))
  }))) {
    stop("Each step must be created using the Step() function")
  }
  
  list(steps = steps)
}

#' Create an orchestrated flow with agents and workflows
#' @param agents Named list of Agent instances or configurations
#' @param workflows Named list of workflow definitions
#' @return An Orchestrator instance
#' @export
CreateFlow <- function(agents = list(), workflows = list()) {
  # Handle single agent case
  if (inherits(agents, "aigen_Agent")) {
    agents <- list(default = agents)
  }
  
  # Process agents
  agent_instances <- lapply(names(agents), function(name) {
    config <- agents[[name]]
    
    if (inherits(config, "aigen_Agent")) {
      return(config)
    }
    
    if (!is.list(config) || is.null(config$model)) {
      stop(sprintf("Invalid configuration for agent '%s'", name))
    }
    
    Agent(
      model = config$model,
      name = name,
      memory = config$memory,
      tools = config$tools
    )
  })
  names(agent_instances) <- names(agents)
  
  # Create orchestrator
  orchestrator <- Orchestrator$new(agents = agent_instances)
  
  # Define workflows
  if (length(workflows) > 0) {
    for (name in names(workflows)) {
      workflow <- workflows[[name]]
      orchestrator$define_workflow(
        name = name,
        steps = workflow$steps
      )
    }
  }
  
  orchestrator
}

#' Execute a workflow
#' @param orchestrator An Orchestrator instance
#' @param workflow Name of the workflow to execute
#' @param input Input data for the workflow
#' @param context Optional context data
#' @return Results of workflow execution
#' @export
RunFlow <- function(orchestrator, workflow, input, context = list()) {
  if (!inherits(orchestrator, "Orchestrator")) {
    stop("orchestrator must be an Orchestrator instance")
  }
  
  orchestrator$execute_workflow(
    workflow_name = workflow,
    initial_input = input,
    context = context
  )
}



#############################################
# Function to enable Agent Memory
#############################################

#' Create a new memory instance for conversation history
#' 
#' @param type Type of memory to create ("conversation", "vectorstore")
#' @return A Memory instance ready to use with Agent
#' @export
AgentMemory <- function(type = c("conversation", "vectorstore")) {
  type <- match.arg(type)
  
  switch(type,
         "conversation" = ConversationMemory$new(),
         "vectorstore" = stop("Vectorstore memory not yet implemented"))
}

#' Create conversation memory with optional size limit
#' 
#' @param max_messages Maximum number of messages to retain (NULL for unlimited)
#' @return A ConversationMemory instance
#' @examples
#' memory <- ConversationStore(max_messages = 10)
#' agent <- Agent(model, memory = memory)
#' @export
AgentConversationStore <- function(max_messages = NULL) {
  if (is.null(max_messages)) {
    ConversationMemory$new()
  } else {
    LimitedConversationMemory$new(max_messages = max_messages)
  }
}


#############################################
# Function to analyze data using both agents
#############################################


#' @title Perform comprehensive data analysis
#' @description Analyzes a dataset providing summary statistics, structure, correlations, and missing values
#' @param data The dataset to analyze
#' @return A formatted string containing the analysis results
#' @export
analyze_data <- function(data) {
  # Input validation
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input must be a data frame or matrix")
  }
  
  # Perform analysis
  analysis <- list(
    summary = summary(data),
    dimensions = dim(data),
    missing = colSums(is.na(data))
  )
  
  # Add correlations if possible
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (any(numeric_cols)) {
      analysis$correlations <- cor(data[, numeric_cols, drop = FALSE])
    }
  }
  
  # Format results
  result <- capture.output({
    cat("Dataset Dimensions:", paste(analysis$dimensions, collapse = " x "), "\n\n")
    
    cat("Summary Statistics:\n")
    print(analysis$summary)
    
    if (!is.null(analysis$correlations)) {
      cat("\nCorrelations between numeric variables:\n")
      print(analysis$correlations)
    }
    
    cat("\nMissing Values per Column:\n")
    print(analysis$missing)
  })
  
  return(paste(result, collapse = "\n"))
}

#' Format text into different styles and structures
#' @param text The text to format
#' @param style The output style ("markdown", "bullet", "paragraph", "summary")
#' @param max_length Optional maximum length for summaries
#' @return Formatted text string
#' @export
format_text <- function(text, 
                        style = c("markdown", "bullet", "paragraph", "summary"),
                        max_length = NULL) {
  
  # Validate inputs
  if (!is.character(text)) {
    stop("text must be a character string")
  }
  style <- match.arg(style)
  
  # Clean the text
  text <- trimws(text)
  
  # Format based on style
  result <- switch(style,
                   "markdown" = {
                     # Convert to markdown with headers and emphasis
                     lines <- unlist(strsplit(text, "\n"))
                     
                     # Add markdown formatting
                     formatted <- sapply(lines, function(line) {
                       if (grepl("^[A-Z][A-Za-z\\s]+:.*$", line)) {
                         # Convert "Title: content" to markdown header
                         parts <- strsplit(line, ":", fixed = TRUE)[[1]]
                         sprintf("## %s\n%s", trimws(parts[1]), trimws(parts[2]))
                       } else if (grepl("^[0-9]+\\.", line)) {
                         # Already numbered, just add spacing
                         sprintf("%s\n", line)
                       } else if (nchar(trimws(line)) > 0) {
                         # Regular line
                         sprintf("%s\n", line)
                       } else {
                         # Empty line
                         ""
                       }
                     })
                     
                     paste(formatted, collapse = "\n")
                   },
                   
                   "bullet" = {
                     # Convert to bullet points
                     lines <- unlist(strsplit(text, "\n"))
                     lines <- lines[nchar(trimws(lines)) > 0]  # Remove empty lines
                     
                     # Add bullets
                     formatted <- sprintf("- %s", lines)
                     paste(formatted, collapse = "\n")
                   },
                   
                   "paragraph" = {
                     # Format as clean paragraphs
                     # Split on double newlines to separate paragraphs
                     paras <- strsplit(text, "\n\n+")[[1]]
                     
                     # Clean each paragraph
                     formatted <- sapply(paras, function(p) {
                       # Replace single newlines with spaces
                       p <- gsub("\n", " ", p)
                       # Clean up multiple spaces
                       p <- gsub("\\s+", " ", p)
                       # Trim and add paragraph break
                       sprintf("%s\n\n", trimws(p))
                     })
                     
                     paste(formatted, collapse = "")
                   },
                   
                   "summary" = {
                     # Create a brief summary
                     # Remove extra whitespace
                     text <- gsub("\\s+", " ", text)
                     text <- trimws(text)
                     
                     if (!is.null(max_length) && nchar(text) > max_length) {
                       # Truncate to max_length at word boundary
                       truncated <- substr(text, 1, max_length)
                       # Find last complete word
                       last_space <- regexpr("\\s[^\\s]*$", truncated)[1]
                       if (last_space > 0) {
                         truncated <- substr(truncated, 1, last_space)
                       }
                       sprintf("%s...", trimws(truncated))
                     } else {
                       text
                     }
                   }
  )
  
  return(result)
}

#' @title Create analysis tools for agent
#' @description Creates a list of analysis tools that can be passed to an agent
#' @return Named list of analysis functions
#' @export
get_analysis_tools <- function() {
  list(
    analyze = analyze_data,
    summarize = function(data) summary(data),
    correlate = function(data) {
      if (is.data.frame(data)) {
        numeric_cols <- sapply(data, is.numeric)
        if (any(numeric_cols)) {
          return(cor(data[, numeric_cols, drop = FALSE]))
        }
      }
      stop("Data must be a data frame with numeric columns")
    }
  )
}


# This function demonstrates how agents with specialized tools can collaborate.
# It performs data analysis using the data agent and generates a summary report using the writer agent.
#' Generate a comprehensive data analysis report
#' 
#' @description 
#' Creates a detailed, multi-layered report from data analysis, including technical 
#' statistics, expert interpretation, and a user-friendly summary. The function uses
#' multiple specialized AI agents to analyze, interpret, and communicate findings.
#' 
#' @param model An instance of LanguageModel
#' @param data A data frame or matrix to analyze
#' @param data_question Specific question or focus for the analysis
#' @param system_prompt Instructions for the analytical approach
#' @param output_format Output format ("text" or "markdown", default: "markdown")
#' 
#' @return A list containing:
#'   \item{raw_analysis}{Technical statistical analysis}
#'   \item{interpretation}{Expert interpretation of findings}
#'   \item{final_report}{User-friendly report}
#'   \item{metadata}{Analysis metadata including timestamps and parameters}
#' 
#' @examples
#' \dontrun{
#' model <- OpenAIModel("gpt-4")
#' results <- summarize_and_report(
#'   model = model,
#'   data = iris,
#'   data_question = "What distinguishes different iris species?",
#'   system_prompt = "You are a botanist and data scientist."
#' )
#' cat(results$final_report)
#' }
#' 
#' @export
aigen_report <- function(model, 
                         data, 
                         data_question = "What are the key insights from this data?",
                         system_prompt = "You are a data analyst. Interpret these statistics and provide key insights.") {
  
  # Validate inputs
  if (!inherits(model, "aigen_LanguageModel")) {
    stop("'model' must be an instance of aigen_LanguageModel")
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }
  
  # Initialize metadata
  metadata <- list(
    timestamp = Sys.time(),
    data_dimensions = dim(data),
    data_columns = colnames(data),
    question = data_question,
    data = data,  # Store the actual data
    analysis_parameters = list(
      system_prompt = system_prompt
    )
  )
  
  # Initialize analysis agent with tools
  data_agent <- aigen_Agent$new(
    model = model,
    tools = list(analyze = analyze_data)
  )
  
  # Get raw analysis
  raw_analysis <- tryCatch({
    data_agent$use_tool("analyze", data)
  }, error = function(e) {
    stop("Analysis failed: ", e$message)
  })
  
  # Get interpretation
  interpretation <- data_agent$chat(
    user_input = paste(
      "Here is the statistical analysis of the data:\n\n",
      raw_analysis,
      "\n\nBased on these statistics, ",
      data_question
    ),
    system_prompt = system_prompt
  )
  
  # Get final report
  writer_agent <- aigen_Agent$new(model = model)
  final_report <- writer_agent$chat(
    user_input = paste(
      "Here is a technical analysis of some data:\n\n",
      interpretation,
      "\n\nPlease convert this into a clear, engaging summary for a general audience."
    ),
    system_prompt = "You are a technical writer. Create clear, engaging summaries of technical analyses."
  )
  
  # Add execution time to metadata
  metadata$execution_time <- difftime(Sys.time(), metadata$timestamp, units = "secs")
  
  # Create and return report object
  structure(
    list(
      raw_analysis = raw_analysis,
      interpretation = interpretation,
      final_report = final_report,
      metadata = metadata
    ),
    class = "aigen_report"
  )
}


#' Print method for aigen_report objects
#' @export
print.aigen_report <- function(x, ...) {
  cat("\n=== Data Analysis Report ===\n\n")
  cat("Generated:", format(x$metadata$timestamp), "\n")
  cat("Analysis Duration:", round(x$metadata$execution_time, 2), "seconds\n")
  cat("Data Dimensions:", paste(x$metadata$data_dimensions, collapse = " x "), "\n")
  cat("\n--- Final Report ---\n\n")
  cat(x$final_report)
  cat("\n\nUse $raw_analysis, $interpretation, or $final_report to access specific components.\n")
}

#' Summary method for aigen_report objects
#' @export
summary.aigen_report <- function(object, ...) {
  cat("\n=== Analysis Summary ===\n\n")
  cat("Analysis Parameters:\n")
  cat("- Data Dimensions:", paste(object$metadata$data_dimensions, collapse = " x "), "\n")
  cat("- Analysis Question:", object$metadata$question, "\n")
  cat("- Analysis Time:", format(object$metadata$timestamp), "\n")
  cat("\nKey Components:\n")
  cat("1. Raw Analysis (", nchar(object$raw_analysis), " characters)\n")
  cat("2. Expert Interpretation (", nchar(object$interpretation), " characters)\n")
  cat("3. Final Report (", nchar(object$final_report), " characters)\n")
}

