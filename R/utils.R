library(R6)          
library(httr)        
library(jsonlite) 


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
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create a new Agent instance with optional tools
#' 
#' @param model An instance of a language model (OpenAI, Anthropic, etc.)
#' @param memory Optional ConversationMemory instance for managing chat history
#' @param tools Named list of functions to use as tools
#' @param tools_env Environment to look for tool functions, defaults to parent frame
#' @param validate_tools Logical indicating whether to validate tool functions
#' @return An Agent instance
#' @examples
#' # Simple initialization with a model
#' model <- OpenAIModel("gpt-4")
#' agent <- Agent_new(model)
#' 
#' # Initialize with tools
#' sum_two <- function(a, b) a + b
#' analyze_data <- function(data, col) summary(data[[col]])
#' agent <- Agent_new(
#'   model = model,
#'   tools = c("sum" = "sum_two", 
#'             "analyze" = "analyze_data")
#' )
#' 
#' @export
Agent <- function(model, 
                      memory = NULL,
                      tools = NULL,
                      tools_env = parent.frame(),
                      validate_tools = TRUE) {
  # Validate model
  if (!inherits(model, "aigen_LanguageModel")) {
    stop("model must be an instance of aigen_LanguageModel")
  }
  
  # Process tools if provided
  tool_list <- list()
  if (!is.null(tools)) {
    # Handle different tool input formats
    if (is.list(tools) && all(sapply(tools, is.function))) {
      # Direct list of functions
      tool_list <- tools
    } else if (is.character(tools)) {
      # Character vector of function names
      tool_list <- lapply(tools, function(fname) {
        if (exists(fname, envir = tools_env, mode = "function")) {
          get(fname, envir = tools_env, mode = "function")
        } else {
          stop(sprintf("Function '%s' not found", fname))
        }
      })
      names(tool_list) <- names(tools) %||% tools
    } else {
      stop("tools must be either a named list of functions or a character vector of function names")
    }
    
    # Validate tools if requested
    if (validate_tools) {
      invalid_tools <- sapply(names(tool_list), function(name) {
        tool <- tool_list[[name]]
        !is.function(tool) || is.null(formals(tool))
      })
      
      if (any(invalid_tools)) {
        stop(sprintf(
          "Invalid tools found: %s", 
          paste(names(tool_list)[invalid_tools], collapse = ", ")
        ))
      }
    }
  }
  
  # Create and return agent instance
  aigen_Agent$new(
    model = model,
    memory = memory,
    tools = tool_list
  )
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


########################################
# Function to analyze data using both agents
########################################


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
#' @param model An instance of aigen_LanguageModel
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
    tools = get_analysis_tools()
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

