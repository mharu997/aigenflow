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
                  debug_mode = FALSE,
                  max_react_iterations = 5,
                  enable_react = TRUE) {
  
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
    debug_mode = debug_mode,
    max_react_iterations = max_react_iterations,
    enable_react = enable_react
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



# Also update the ai_response pipe operator method
`%>%.ai_response` <- function(x, f, ...) {
  if (inherits(f, "function") && identical(f, ask)) {
    # Pass the entire ai_response to ask
    result <- f(x, ...)
    return(result)
  } else {
    magrittr::freduce(x, list(f))
  }
}

# Fix 2: Add new methods to the Agent class
# These would be added to the aigen_Agent R6 Class
#' @description Get messages from short-term memory
get_memory_messages = function() {
  return(private$short_term_memory$get_memory())
}

#' @description Add a message directly to memory
add_to_memory = function(role, content) {
  if (is.null(private$short_term_memory)) {
    warning("Agent has no short-term memory initialized")
    return(invisible(self))
  }
  
  private$short_term_memory$add_message(role, content)
  private$log_event(sprintf("Added message to memory: %s (%d chars)", 
                            role, nchar(content)))
  return(invisible(self))
}

ask <- function(agent_or_response, user_input, system_prompt = NULL, context_window = 5, verbose = TRUE) {
  # Extract agent and history
  if (inherits(agent_or_response, "ai_response")) {
    agent <- agent_or_response$agent
    previous_history <- agent_or_response$history
  } else if (inherits(agent_or_response, "aigen_Agent")) {
    agent <- agent_or_response
    previous_history <- list()
  } else {
    stop("First argument must be either an Agent or ai_response object")
  }
  
  # Format conversation history for inclusion in the prompt
  history_text <- ""
  if (length(previous_history) > 0) {
    # Limit to last 'context_window' exchanges to manage token usage
    start_idx <- max(1, length(previous_history) - (2 * context_window))
    recent_history <- previous_history[start_idx:length(previous_history)]
    
    history_text <- paste(
      "Previous conversation (please read carefully and maintain context):\n",
      paste(sapply(recent_history, function(msg) {
        sprintf("%s: %s", toupper(msg$role), msg$content)
      }), collapse = "\n\n"),
      "\n\n"
    )
  }
  
  # Create the enhanced prompt with history
  enhanced_prompt <- paste0(
    history_text,
    "Current question: ", user_input
  )
  
  # Generate response using the enhanced prompt
  response <- agent$chat(enhanced_prompt, system_prompt, context_window)
  
  if (verbose) {
    cat("\n=== Ask:", agent$get_name(), "===\n")
    cat("Input:", user_input, "\n")
    if (!is.null(system_prompt)) cat("System:", system_prompt, "\n")
    cat("Response:\n", response, "\n")
    cat("===============================\n")
  }
  
  # Update history with this interaction
  current_history <- c(
    previous_history,
    list(list(role = "user", content = user_input, timestamp = Sys.time())),
    list(list(role = "assistant", content = response, timestamp = Sys.time()))
  )
  
  # Return ai_response object
  return(structure(
    list(
      agent = agent,
      response = response,
      history = current_history
    ),
    class = "ai_response"
  ))
}



#' Get response history
#' @export
get_history <- function(x) {
  if (inherits(x, "ai_response")) {
    # Return formatted history with timestamps
    lapply(x$history, function(entry) {
      list(
        timestamp = format(entry$timestamp),
        role = entry$role,
        content = entry$content
      )
    })
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

#' Generate a comprehensive, research-grade data analysis report
#' 
#' @description 
#' Creates a detailed, publication-quality report from data analysis, incorporating
#' rigorous statistical analysis, expert interpretation, and clear communication.
#' The function employs specialized AI agents to conduct thorough analysis,
#' interpret findings, and present results with academic rigor while maintaining
#' accessibility.
#' 
#' @param model An instance of LanguageModel
#' @param data A data frame or matrix to analyze
#' @param data_question Specific question or focus for the analysis
#' @param system_prompt Instructions for the analytical approach
#' @param output_format Output format ("text" or "markdown", default: "markdown")
#' 
#' @return A list containing:
#'   \item{raw_analysis}{Detailed statistical analysis with comprehensive metrics}
#'   \item{interpretation}{Expert interpretation with statistical significance}
#'   \item{final_report}{Clear, structured report with key findings}
#'   \item{metadata}{Analysis metadata including timestamps and parameters}
#' 
aigen_report <- function(model, 
                         data, 
                         data_question = "What are the key insights from this data?",
                         system_prompt = NULL,
                         output_format = "markdown") {
  
  # Input validation
  if (!inherits(model, "aigen_LanguageModel")) {
    stop("'model' must be an instance of aigen_LanguageModel")
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }
  
  # Enhanced metadata capturing
  metadata <- list(
    timestamp = Sys.time(),
    data_dimensions = dim(data),
    data_columns = colnames(data),
    column_types = sapply(data, class),
    missing_data = colSums(is.na(data)),
    question = data_question,
    data_summary = summary(data),
    analysis_parameters = list(
      system_prompt = system_prompt,
      output_format = output_format
    )
  )
  
  # Initialize analysis agent with enhanced tools
  data_agent <- aigen_Agent$new(
    model = model,
    tools = list(
      analyze = analyze_data,
      summarize = summary,
      correlate = cor
    )
  )
  
  # Detailed statistical analysis with enhanced context
  raw_analysis <- tryCatch({
    data_agent$use_tool("analyze", data)
  }, error = function(e) {
    stop("Statistical analysis failed: ", e$message)
  })
  
  # Expert interpretation with research focus
  interpretation_prompt <- paste(
    "As a research statistician, analyze this dataset with academic rigor:\n\n",
    raw_analysis,
    "\n\nFocus your analysis on the following aspects:",
    "\n1. Statistical Significance: Identify and explain significant patterns",
    "\n2. Data Distribution: Analyze the distribution of key variables",
    "\n3. Relationships: Examine correlations and potential causal relationships",
    "\n4. Anomalies: Identify and explain any outliers or unusual patterns",
    "\n5. Limitations: Discuss any data limitations or potential biases",
    "\n\nBased on these statistics and considering the question: ",
    data_question
  )
  
  interpretation <- data_agent$chat(
    user_input = interpretation_prompt,
    system_prompt = "You are a senior research statistician with expertise in data analysis. 
    Provide a thorough, academically rigorous interpretation of the data. 
    Focus on statistical significance, methodological soundness, and meaningful patterns. 
    Be precise in your language and support conclusions with specific evidence from the data."
  )
  
  # Enhanced report generation with structured output
  report_prompt <- paste(
    "Transform this technical analysis into a clear, comprehensive report:\n\n",
    interpretation,
    "\n\nStructure the report as follows:",
    "\n1. Executive Summary (4-5 sentence paragraph of key findings)",
    "\n2. Methodology & Data Overview",
    "\n3. Key Findings",
    "  - Primary Insights (statistically significant findings)",
    "  - Secondary Observations (interesting patterns)",
    "  - Data Relationships (correlations and potential causations)",
    "\n4. Limitations & Considerations",
    "\n5. Actionable Insights & Recommendations",
    "\n\nEnsure the report is:",
    "- Precise and evidence-based",
    "- Free of unnecessary jargon",
    "- Focused on meaningful insights",
    "- Actionable for decision-making"
  )
  
  writer_agent <- aigen_Agent$new(model = model)
  final_report <- writer_agent$chat(
    user_input = report_prompt,
    system_prompt = "You are an expert research communicator with a deep understanding of 
    statistical analysis and scientific writing. Your goal is to translate complex 
    technical findings into clear, actionable insights while maintaining scientific 
    rigor. Focus on precision, clarity, and meaningful interpretation of the data. 
    Avoid speculation and unsupported conclusions. Structure your writing to be 
    both academically sound and practically useful."
  )
  
  # Add execution time and analysis quality metrics to metadata
  metadata$execution_time <- difftime(Sys.time(), metadata$timestamp, units = "secs")
  metadata$analysis_coverage <- list(
    variables_analyzed = length(metadata$column_types),
    missing_data_handled = any(metadata$missing_data > 0),
    correlation_analysis = "correlate" %in% names(data_agent$tools)
  )
  
  # Create and return enhanced report object
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

#' Print method for aigen_report objects with enhanced formatting
#' @export
print.aigen_report <- function(x, ...) {
  cat("\n=== Research Analysis Report ===\n")
  cat("\nAnalysis Generated:", format(x$metadata$timestamp))
  cat("\nExecution Time:", round(x$metadata$execution_time, 2), "seconds")
  cat("\nData Dimensions:", paste(x$metadata$data_dimensions, collapse = " x "))
  cat("\n\n=== Executive Summary ===\n\n")
  
  # Extract and print executive summary (first paragraph of final report)
  summary_lines <- strsplit(x$final_report, "\n")[[1]]
  exec_summary <- summary_lines[which(nchar(summary_lines) > 0)[1]]
  cat(exec_summary)
  
  cat("\n\n=== Full Report ===\n\n")
  cat(x$final_report)
  
  cat("\n\nUse $raw_analysis, $interpretation, or $final_report to access specific components.\n")
}

#' Enhanced summary method for aigen_report objects
#' @export
summary.aigen_report <- function(object, ...) {
  cat("\n=== Analysis Overview ===\n")
  cat("\nAnalysis Parameters:")
  cat("\n- Data Dimensions:", paste(object$metadata$data_dimensions, collapse = " x"))
  cat("\n- Variables Analyzed:", length(object$metadata$column_types))
  cat("\n- Analysis Question:", object$metadata$question)
  cat("\n- Execution Time:", format(object$metadata$execution_time))
  
  cat("\n\nData Quality Metrics:")
  cat("\n- Missing Data Present:", any(object$metadata$missing_data > 0))
  cat("\n- Column Types:", paste(unique(object$metadata$column_types), collapse = ", "))
  
  cat("\n\nReport Components:")
  cat("\n1. Raw Analysis:", nchar(object$raw_analysis), "characters")
  cat("\n2. Expert Interpretation:", nchar(object$interpretation), "characters")
  cat("\n3. Final Report:", nchar(object$final_report), "characters")
  
  cat("\n\nUse print() for full report or access components directly.\n")
}

########################################
# Ollama Model Implementation
########################################


#' Ollama model management utilities
#' @description Functions for managing local Ollama models independently of model instances
#' @keywords internal

#' List all available Ollama models with enhanced error handling
#' @param base_url Base URL for Ollama API
#' @param include_details Include additional model details in output
#' @return Data frame of installed models
#' @export
ollama_models <- function(base_url = "http://localhost:11434/api", include_details = FALSE) {
  # Load crayon for colors
  if (!requireNamespace("crayon", quietly = TRUE)) {
    install.packages("crayon")
    library(crayon)
  }
  
  # Define color styles
  header_style <- function(x) crayon::bold(crayon::blue(x))
  error_style <- function(x) crayon::red(paste("✗", x))
  detail_style <- function(x) crayon::silver(x)
  
  tryCatch({
    result <- httr::GET(
      url = file.path(base_url, "tags"),
      httr::config(timeout = 30)
    )
    
    if (result$status_code != 200) {
      stop("API request failed with status: ", result$status_code)
    }
    
    # Parse JSON response
    response_text <- rawToChar(result$content)
    parsed <- jsonlite::fromJSON(response_text)
    
    # Filter out NA values and create data frame
    valid_models <- !is.na(parsed$models$name)
    models_df <- data.frame(
      name = parsed$models$name[valid_models],
      model = parsed$models$model[valid_models],
      modified = parsed$models$modified_at[valid_models],
      size_gb = round(parsed$models$size[valid_models] / (1024^3), 2),
      stringsAsFactors = FALSE
    )
    
    # Add details if requested
    if (include_details && nrow(models_df) > 0) {
      models_df$parameter_size <- parsed$models$details$parameter_size[valid_models]
      models_df$format <- parsed$models$details$format[valid_models]
      models_df$family <- parsed$models$details$family[valid_models]
      models_df$quantization <- parsed$models$details$quantization_level[valid_models]
    }
    
    return(models_df)
    
  }, error = function(e) {
    # Get the base error message
    error_msg <- conditionMessage(e)
    
    # Print formatted error message
    cat("\n", header_style("Ollama Models Error"), "\n")
    cat(header_style("══════════════════"), "\n\n")
    cat(error_style("Connection Failed"), "\n")
    
    # Determine specific error type and provide helpful message
    if (grepl("Couldn't connect to server", error_msg)) {
      cat(detail_style("\nPossible causes:"), "\n")
      cat(detail_style("1. Ollama service is not running"), "\n")
      cat(detail_style("2. Wrong port number (default: 11434)"), "\n")
      cat(detail_style("3. Network connectivity issues"), "\n")
      cat(detail_style("\nTroubleshooting steps:"), "\n")
      cat(detail_style("1. Check if Ollama is running:"), "\n")
      cat(detail_style("   - Run 'ollama serve' in terminal or the Ollama app"), "\n")
      cat(detail_style("2. Verify the API endpoint:"), "\n")
      cat(detail_style(sprintf("   - Current endpoint: %s", base_url)), "\n")
      cat(detail_style("3. Run diagnostics:"), "\n")
      cat(detail_style("   - Use ollama_diagnostics() for detailed status"), "\n")
    } else if (grepl("Timeout", error_msg)) {
      cat(detail_style("\nThe request timed out. Try:"), "\n")
      cat(detail_style("1. Check your internet connection"), "\n")
      cat(detail_style("2. Verify Ollama is responding"), "\n")
      cat(detail_style("3. Increase timeout duration if needed"), "\n")
    } else {
      cat(detail_style("\nError details:"), "\n")
      cat(detail_style(error_msg), "\n")
    }
    
    cat("\n") # Add final newline for cleaner output
    
    # Return NULL invisibly rather than stopping with error
    return(invisible(NULL))
  })
}

#' Get number of installed Ollama models
#' @param parsed_content Parsed API response
#' @return Number of valid models
#' @keywords internal
count_valid_models <- function(parsed_content) {
  if (is.null(parsed_content$models)) return(0)
  sum(!is.na(parsed_content$models$name))
}


#' Install an Ollama model
#' @param model_name Name of the model to install
#' @param base_url Base URL for Ollama API
#' @param quiet Suppress progress messages
#' @return TRUE if successful
#' @export
ollama_install <- function(model_name, base_url = "http://localhost:11434/api", quiet = FALSE) {
  tryCatch({
    if (!quiet) message("Installing model: ", model_name)
    
    result <- POST(
      url = file.path(base_url, "pull"),
      body = list(name = model_name),
      encode = "json",
      config = list(timeout = 3600)
    )
    
    if (result$status_code != 200) {
      stop("Model installation failed with status: ", result$status_code)
    }
    
    # Monitor download progress
    response_text <- rawToChar(result$content)
    responses <- strsplit(response_text, "\n")[[1]]
    
    for (resp in responses) {
      if (nchar(resp) > 0) {
        parsed <- fromJSON(resp)
        if (!quiet && !is.null(parsed$status)) {
          message(parsed$status)
        }
      }
    }
    
    if (!quiet) message("Installation complete: ", model_name)
    return(TRUE)
    
  }, error = function(e) {
    stop("Model installation failed: ", e$message)
  })
}

#' Remove an Ollama model
#' @param model_name Name of the model to remove
#' @param base_url Base URL for Ollama API
#' @return TRUE if successful
#' @export
ollama_remove <- function(model_name, base_url = "http://localhost:11434/api") {
  tryCatch({
    result <- DELETE(
      url = file.path(base_url, "delete"),
      body = list(name = model_name),
      encode = "json",
      config = list(timeout = 30)
    )
    
    if (result$status_code != 200) {
      stop("Model removal failed with status: ", result$status_code)
    }
    
    message("Successfully removed model: ", model_name)
    return(TRUE)
    
  }, error = function(e) {
    stop("Failed to remove model: ", e$message)
  })
}

#' Check if an Ollama model is installed
#' @param model_name Name of the model to check
#' @param base_url Base URL for Ollama API
#' @return Logical indicating if model is installed
#' @export
ollama_exists <- function(model_name, base_url = "http://localhost:11434/api") {
  tryCatch({
    models <- ollama_models(base_url)
    return(model_name %in% models$name)
  }, error = function(e) {
    warning("Could not verify model installation status: ", e$message)
    return(FALSE)
  })
}



#' Updated diagnostic function for model counting
#' @param base_url Base URL for Ollama API
#' @return Diagnostic information
#' @export
ollama_diagnostics <- function(base_url = "http://localhost:11434/api", verbose = TRUE) {
  results <- list()
  
  # Load crayon for colors
  if (!requireNamespace("crayon", quietly = TRUE)) {
    install.packages("crayon")
    library(crayon)
  }
  
  # Define color styles
  header_style <- function(x) crayon::bold(crayon::blue(x))
  success_style <- function(x) crayon::green(paste("✓", x))
  error_style <- function(x) crayon::red(paste("✗", x))
  section_style <- function(x) crayon::cyan(x)
  detail_style <- function(x) crayon::silver(x)
  
  # 1. Check using httr GET request
  results$httr_check <- tryCatch({
    response <- httr::GET(file.path(base_url, "tags"))
    content <- jsonlite::fromJSON(rawToChar(response$content))
    list(
      success = response$status_code == 200,
      status_code = response$status_code,
      content = content,
      model_count = count_valid_models(content)  # Use new counting function
    )
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  if (verbose) {
    cat("\n", header_style("Ollama Diagnostic Results"), "\n")
    cat(header_style("═══════════════════════"), "\n")
    
    cat("\n", section_style("API Check:"), "\n")
    cat("Status: ", if(results$httr_check$success) 
      success_style("Connected") else error_style("Failed"), "\n")
    
    if (results$httr_check$success) {
      valid_models <- results$httr_check$model_count
      cat(detail_style(sprintf("Available Models: %d model(s)", valid_models)), "\n")
      
      if (valid_models > 0) {
        models_df <- ollama_models(base_url)
        cat(detail_style("\nInstalled Models:"), "\n")
        for (i in 1:nrow(models_df)) {
          cat(detail_style(sprintf("- %s (%.2f GB)\n", 
                                   models_df$name[i], 
                                   models_df$size_gb[i])))
        }
      }
    }
  }
  
  invisible(results)
}

#' Check if Ollama service is available
#' @param base_url Base URL for Ollama API
#' @param quiet Suppress status messages
#' @return Logical indicating if service is available
#' @keywords internal
ollama_status <- function(base_url = "http://localhost:11434/api", quiet = FALSE) {
  tryCatch({
    result <- httr::GET(
      url = file.path(base_url, "tags"),
      httr::config(timeout = 5)
    )
    
    is_available <- result$status_code == 200
    
    if (!quiet) {
      if (is_available) {
        cat(crayon::green("✓ Ollama service is available\n"))
      } else {
        cat(crayon::red("✗ Ollama service is unavailable\n"))
      }
    }
    
    return(is_available)
  }, error = function(e) {
    if (!quiet) {
      cat(crayon::red("✗ Ollama service is unavailable\n"))
      cat(crayon::silver("Error: ", conditionMessage(e), "\n"))
    }
    return(FALSE)
  })
}

#' List installed Ollama models
#' @param base_url Base URL for Ollama API
#' @return Data frame of installed models
#' @export
ollama_models <- function(base_url = "http://localhost:11434/api") {
  tryCatch({
    result <- httr::GET(
      url = file.path(base_url, "tags"),
      httr::config(timeout = 30)
    )
    
    if (result$status_code != 200) {
      stop("Failed to get model list with status: ", result$status_code)
    }
    
    parsed <- jsonlite::fromJSON(rawToChar(result$content))
    
    if (is.null(parsed$models) || length(parsed$models) == 0) {
      return(data.frame(
        name = character(),
        model = character(),
        modified = character(),
        size_gb = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Filter out NA values
    valid_models <- !is.na(parsed$models$name)
    data.frame(
      name = parsed$models$name[valid_models],
      model = parsed$models$model[valid_models],
      modified = parsed$models$modified_at[valid_models],
      size_gb = round(parsed$models$size[valid_models] / (1024^3), 2),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    stop("Failed to list models: ", conditionMessage(e))
  })
}





#' Check Ollama service availability
#' @param base_url Base URL for Ollama API
#' @param quiet Suppress status messages
#' @return Logical indicating if service is available
#' @keywords internal
ollama_status <- function(base_url = "http://localhost:11434/api", quiet = FALSE) {
  tryCatch({
    result <- httr::GET(
      url = file.path(base_url, "tags"),
      httr::config(timeout = 5)
    )
    return(result$status_code == 200)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check if Ollama model exists
#' @param model_name Name of the model
#' @param base_url Base URL for Ollama API
#' @return Logical indicating if model exists
#' @keywords internal
ollama_exists <- function(model_name, base_url = "http://localhost:11434/api") {
  tryCatch({
    result <- httr::GET(
      url = file.path(base_url, "tags"),
      httr::config(timeout = 5)
    )
    
    if (result$status_code != 200) return(FALSE)
    
    models <- jsonlite::fromJSON(rawToChar(result$content))$models
    return(!is.null(models) && model_name %in% models$name)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Monitor active compute device for Ollama inference (Heuristic)
#' Detect system hardware configuration
#' @return List containing platform details and available hardware
#' @keywords internal
detect_system_hardware <- function() {
  result <- list(
    platform = "unknown",
    hardware = list(
      accelerator = NULL,
      gpu = NULL,
      cpu = TRUE
    )
  )
  
  os <- Sys.info()['sysname']
  
  if (os == "Darwin") {
    cpu_info <- tryCatch({
      system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
    }, error = function(e) "")
    
    if (grepl("Apple", cpu_info, ignore.case = TRUE)) {
      result$platform <- "apple_silicon"
      result$hardware$accelerator <- "ANE"
      result$hardware$gpu <- "Apple GPU"
    }
  } else if (os == "Linux") {
    if (system("which nvidia-smi", ignore.stderr = TRUE) == 0) {
      gpu_info <- try(system("nvidia-smi --query-gpu=gpu_name --format=csv,noheader", intern = TRUE), silent = TRUE)
      if (!inherits(gpu_info, "try-error")) {
        result$platform <- "nvidia"
        result$hardware$gpu <- gpu_info[1]
      }
    } else if (file.exists("/dev/dri/renderD128")) {
      result$platform <- "amd"
      result$hardware$gpu <- "AMD GPU"
    }
  } else if (os == "Windows") {
    if (system("where nvidia-smi", ignore.stderr = TRUE) == 0) {
      gpu_info <- try(system("nvidia-smi --query-gpu=gpu_name --format=csv,noheader", intern = TRUE), silent = TRUE)
      if (!inherits(gpu_info, "try-error")) {
        result$platform <- "nvidia"
        result$hardware$gpu <- gpu_info[1]
      }
    }
  }
  
  if (result$platform == "unknown") {
    result$platform <- "cpu_only"
  }
  
  return(result)
}

#' Monitor active hardware usage during model inference
#' @param platform Platform type from detect_system_hardware
#' @return List of active hardware components
#' @keywords internal
monitor_inference_hardware <- function(platform) {
  result <- list(
    accelerator = FALSE,
    gpu = FALSE,
    cpu = FALSE,
    details = list()
  )
  
  if (platform == "apple_silicon") {
    ane_processes <- system("ps aux | grep ollama | grep -i Neural | grep -v grep", 
                            intern = TRUE, ignore.stderr = TRUE)
    metal_processes <- system("ps aux | grep ollama | grep -i Metal | grep -v grep", 
                              intern = TRUE, ignore.stderr = TRUE)
    
    result$accelerator <- length(ane_processes) > 0
    result$gpu <- length(metal_processes) > 0
    result$cpu <- !result$accelerator && !result$gpu
    
    if (result$accelerator) result$details$ane <- ane_processes
    if (result$gpu) result$details$gpu <- metal_processes
  } 
  else if (platform == "nvidia") {
    gpu_processes <- try(system("nvidia-smi --query-compute-apps=pid,process_name --format=csv,noheader | grep ollama",
                                intern = TRUE), silent = TRUE)
    result$gpu <- !inherits(gpu_processes, "try-error") && length(gpu_processes) > 0
    result$cpu <- !result$gpu
    if (result$gpu) result$details$gpu <- gpu_processes
  }
  else if (platform == "amd") {
    gpu_usage <- try(system("rocm-smi --showuse | grep ollama", intern = TRUE), silent = TRUE)
    result$gpu <- !inherits(gpu_usage, "try-error") && length(gpu_usage) > 0
    result$cpu <- !result$gpu
    if (result$gpu) result$details$gpu <- gpu_usage
  }
  else {
    result$cpu <- TRUE
  }
  
  return(result)
}

#' Format hardware status for display
#' @param hw_info Hardware info from monitor_inference_hardware
#' @param system_info System info from detect_system_hardware
#' @return List containing formatted status strings
#' @keywords internal
format_hardware_status <- function(hw_info, system_info) {
  active <- crayon::green("●")
  inactive <- crayon::red("○")
  
  status <- character(0)
  
  if (!is.null(system_info$hardware$accelerator)) {
    status <- c(status, paste0(
      if(hw_info$accelerator) active else inactive,
      " ", system_info$hardware$accelerator
    ))
  }
  
  if (!is.null(system_info$hardware$gpu)) {
    status <- c(status, paste0(
      if(hw_info$gpu) active else inactive,
      " ", system_info$hardware$gpu
    ))
  }
  
  status <- c(status, paste0(
    if(hw_info$cpu) active else inactive,
    " CPU"
  ))
  
  return(list(
    status = paste(status, collapse = "  "),
    platform = crayon::blue(paste0("[", system_info$platform, "]"))
  ))
}