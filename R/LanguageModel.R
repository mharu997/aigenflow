library(R6)          
library(httr)        
library(jsonlite) 
library(ggplot2) 

#' @title LanguageModel Class
#' @description Base class for defining language models.
#' @export
########################################
# Base Language Model Class
########################################

aigen_LanguageModel <- R6Class(
  "aigen_LanguageModel",
  public = list(
    # Constructor
    initialize = function(model_name, api_key, max_tokens = 1000, temperature = 0.7) {
      # Purpose: Initialize the class with model settings
      # Parameters:
      # - model_name: String specifying the language model's name (e.g., "GPT-4")
      # - api_key: String containing the API key for authentication
      # - max_tokens: Integer specifying the maximum tokens for a single response
      # - temperature: Float controlling the randomness of the output (0.0 to 1.0)
      
      private$model_name <- model_name
      private$api_key <- api_key
      private$max_tokens <- max_tokens
      private$temperature <- temperature
    },
    
    # Abstract generate method
    generate = function(prompt, system_message = NULL) {
      # Purpose: Placeholder for generating a response
      # Parameters:
      # - prompt: String containing the user's message
      # - system_message: Optional string for system instructions
      stop("The generate method must be implemented by subclasses.")
    }
  ),
  private = list(
    model_name = NULL,
    api_key = NULL,
    max_tokens = NULL,
    temperature = NULL
  )
)

#' @title OpenAIModel Class
#' @description Implements functionality specific to OpenAI language models.
#' @export
########################################
# OpenAI Model Implementation
########################################

aigen_OpenAIModel <- R6Class(
  "aigen_OpenAIModel",
  inherit = aigen_LanguageModel,
  public = list(
    # Constructor with API key handling
    initialize = function(model_name, 
                          api_key = Sys.getenv("OPENAI_API_KEY"), 
                          max_tokens = 1000,
                          temperature = 0.7) {
      if (is.null(api_key) || api_key == "") {
        stop("OpenAI API key not provided. Please set it as an environment variable 'OPENAI_API_KEY'.")
      }
      super$initialize(model_name, api_key, max_tokens, temperature)
    },
    
    # Generate method specific to OpenAI API
    generate = function(prompt, system_message = "You are a helpful assistant.") {
      # Purpose: Sends a request to the OpenAI API and generates a response
      # Parameters:
      # - prompt: String containing the user's message
      # - system_message: String containing system instructions
      
      # Prepare messages array
      messages <- list(
        list(role = "system", content = system_message),
        list(role = "user", content = prompt)
      )
      
      response <- tryCatch({
        result <- POST(
          url = "https://api.openai.com/v1/chat/completions",
          add_headers(
            Authorization = paste("Bearer", private$api_key),
            "Content-Type" = "application/json"
          ),
          body = list(
            model = private$model_name,
            messages = messages,
            temperature = private$temperature,
            max_tokens = private$max_tokens
          ),
          encode = "json"
        )
        
        response_text <- rawToChar(result$content)
        parsed_response <- fromJSON(response_text, simplifyVector = FALSE)
        
        if (!is.null(parsed_response$error)) {
          stop(paste("API Error:", parsed_response$error$message))
        }
        
        if (length(parsed_response$choices) > 0 && 
            !is.null(parsed_response$choices[[1]]$message) &&
            !is.null(parsed_response$choices[[1]]$message$content)) {
          return(parsed_response$choices[[1]]$message$content)
        } else {
          stop("Unexpected API response format")
        }
      }, error = function(e) {
        stop(paste("API request failed:", e$message))
      })
      
      return(response)
    }
  )
)


#' @title AnthropicModel Class
#' @description Implements functionality specific to Anthropic Claude models.
#' @export
########################################
# Anthropic Model Implementation
########################################
aigen_AnthropicModel <- R6Class(
  "aigen_AnthropicModel",
  inherit = aigen_LanguageModel,
  public = list(
    # Constructor with API key handling
    initialize = function(model_name = "claude-3-opus-20240229", 
                          api_key = Sys.getenv("ANTHROPIC_API_KEY"), 
                          max_tokens = 1000,
                          temperature = 0.7) {
      if (is.null(api_key) || api_key == "") {
        stop("Anthropic API key not provided. Please set it as an environment variable 'ANTHROPIC_API_KEY'.")
      }
      super$initialize(model_name, api_key, max_tokens, temperature)
    },
    
    # Generate method specific to Anthropic API
    generate = function(prompt, system_message = "You are Claude, a helpful AI assistant.") {
      # Purpose: Sends a request to the Anthropic API and generates a response
      # Parameters:
      # - prompt: String containing the user's message
      # - system_message: String containing system instructions
      
      response <- tryCatch({
        result <- POST(
          url = "https://api.anthropic.com/v1/messages",
          add_headers(
            "x-api-key" = private$api_key,
            "anthropic-version" = "2023-06-01",
            "Content-Type" = "application/json"
          ),
          body = list(
            model = private$model_name,
            messages = list(
              list(role = "user", content = prompt)
            ),
            system = system_message,
            temperature = private$temperature,
            max_tokens = private$max_tokens
          ),
          encode = "json"
        )
        
        response_text <- rawToChar(result$content)
        parsed_response <- fromJSON(response_text, simplifyVector = FALSE)
        
        if (!is.null(parsed_response$error)) {
          stop(paste("API Error:", parsed_response$error$message))
        }
        
        if (!is.null(parsed_response$content) && 
            length(parsed_response$content) > 0 &&
            !is.null(parsed_response$content[[1]]$text)) {
          return(parsed_response$content[[1]]$text)
        } else {
          stop("Unexpected API response format")
        }
      }, error = function(e) {
        stop(paste("API request failed:", e$message))
      })
      
      return(response)
    }
  )
)

#' @title AzureOpenAIModel Class
#' @description Implements functionality specific to Azure OpenAI models.
#' @export
########################################
# Azure OpenAI Model Implementation
########################################
aigen_AzureOpenAIModel <- R6Class(
  "aigen_AzureOpenAIModel",
  inherit = aigen_LanguageModel,
  public = list(
    # Constructor with Azure-specific parameters
    initialize = function(deployment_name,
                          resource_name,
                          api_version = "2024-02-15-preview",
                          api_key = Sys.getenv("AZURE_OPENAI_KEY"),
                          max_tokens = 1000,
                          temperature = 0.7) {
      if (is.null(api_key) || api_key == "") {
        stop("Azure OpenAI API key not provided. Please set it as an environment variable 'AZURE_OPENAI_KEY'.")
      }
      if (is.null(deployment_name) || deployment_name == "") {
        stop("Azure deployment name is required.")
      }
      if (is.null(resource_name) || resource_name == "") {
        stop("Azure resource name is required.")
      }

      # Store Azure-specific parameters
      private$deployment_name <- deployment_name
      private$resource_name <- resource_name
      private$api_version <- api_version

      # Initialize base class with deployment name as model name
      super$initialize(deployment_name, api_key, max_tokens, temperature)
    },

    # Generate method specific to Azure OpenAI API
    generate = function(prompt, system_message = "You are a helpful assistant.") {
      # Purpose: Sends a request to the Azure OpenAI API and generates a response
      # Parameters:
      # - prompt: String containing the user's message
      # - system_message: String containing system instructions

      # Construct Azure endpoint URL
      endpoint <- sprintf(
        "https://%s.openai.azure.com/openai/deployments/%s/chat/completions?api-version=%s",
        private$resource_name,
        private$deployment_name,
        private$api_version
      )

      # Prepare messages array
      messages <- list(
        list(role = "system", content = system_message),
        list(role = "user", content = prompt)
      )

      response <- tryCatch({
        result <- POST(
          url = endpoint,
          add_headers(
            "api-key" = private$api_key,
            "Content-Type" = "application/json"
          ),
          body = list(
            messages = messages,
            temperature = private$temperature,
            max_tokens = private$max_tokens
          ),
          encode = "json"
        )

        response_text <- rawToChar(result$content)
        parsed_response <- fromJSON(response_text, simplifyVector = FALSE)

        if (!is.null(parsed_response$error)) {
          stop(paste("API Error:", parsed_response$error$message))
        }

        if (length(parsed_response$choices) > 0 &&
            !is.null(parsed_response$choices[[1]]$message) &&
            !is.null(parsed_response$choices[[1]]$message$content)) {
          return(parsed_response$choices[[1]]$message$content)
        } else {
          stop("Unexpected API response format")
        }
      }, error = function(e) {
        stop(paste("API request failed:", e$message))
      })

      return(response)
    }
  ),
  private = list(
    deployment_name = NULL,
    resource_name = NULL,
    api_version = NULL
  )
)


########################################
# Azure ML Endpoint Implementation
########################################
#' @title AzureMLEndpointModel Class
#' @description Base class for Azure ML Endpoint models supporting various model types
#' @keywords internal
#' @export
########################################
# Azure ML Endpoint Core Implementation
########################################

aigen_AzureMLEndpointModel <- R6Class(
  "aigen_AzureMLEndpointModel",
  inherit = aigen_LanguageModel,

  public = list(
    #' @description Initialize a new Azure ML Endpoint model
    #' @param endpoint_url The full URL of the Azure ML endpoint
    #' @param model_type Type of model: "chat", "completion", or "embedding"
    #' @param api_key Azure ML API key for authentication
    #' @param max_tokens Maximum number of tokens in response
    #' @param temperature Sampling temperature (0-1)
    initialize = function(endpoint_url,
                          model_type = c("chat", "completion", "embedding"),
                          api_key = Sys.getenv("AZURE_ML_KEY"),
                          max_tokens = 1000,
                          temperature = 0.7) {

      # Validate endpoint URL
      if (is.null(endpoint_url) || endpoint_url == "") {
        stop("Azure ML endpoint URL is required.")
      }

      # Validate API key
      if (is.null(api_key) || api_key == "") {
        stop("Azure ML API key not provided.")
      }

      # Store endpoint-specific parameters
      private$endpoint_url <- endpoint_url
      private$model_type <- model_type

      # Initialize base class
      super$initialize("azure-ml", api_key, max_tokens, temperature)
    },

    #' @description Generate a response from the model
    #' @param prompt The input text prompt
    #' @param system_message Optional system message for chat models
    #' @param custom_payload Optional custom request payload
    #' @param response_handler Optional custom response handler function
    #' @return Generated text response
    generate = function(prompt,
                        system_message = NULL,
                        custom_payload = NULL,
                        response_handler = NULL) {

      # Construct request body based on model type or custom payload
      body <- if (!is.null(custom_payload)) {
        custom_payload
      } else {
        switch(private$model_type,
               # Chat model format
               "chat" = {
                 messages <- if (!is.null(system_message)) {
                   list(
                     list(role = "system", content = system_message),
                     list(role = "user", content = prompt)
                   )
                 } else {
                   list(list(role = "user", content = prompt))
                 }
                 list(
                   messages = messages,
                   temperature = private$temperature,
                   max_tokens = private$max_tokens
                 )
               },
               # Completion model format
               "completion" = {
                 list(
                   prompt = prompt,
                   temperature = private$temperature,
                   max_tokens = private$max_tokens
                 )
               },
               # Embedding model format
               "embedding" = {
                 list(input = prompt)
               }
        )
      }

      # Make API request with error handling
      response <- tryCatch({
        # Send request to Azure ML endpoint
        result <- POST(
          url = private$endpoint_url,
          add_headers(
            "Authorization" = paste("Bearer", private$api_key),
            "Content-Type" = "application/json"
          ),
          body = body,
          encode = "json"
        )

        # Parse response
        response_text <- rawToChar(result$content)
        parsed_response <- fromJSON(response_text, simplifyVector = FALSE)

        # Handle response using custom handler if provided
        if (!is.null(response_handler)) {
          return(response_handler(parsed_response))
        }

        # Default response handling based on model type
        switch(private$model_type,
               # Extract response for chat models
               "chat" = {
                 if (!is.null(parsed_response$choices) &&
                     length(parsed_response$choices) > 0 &&
                     !is.null(parsed_response$choices[[1]]$message$content)) {
                   return(parsed_response$choices[[1]]$message$content)
                 }
               },
               # Extract response for completion models
               "completion" = {
                 if (!is.null(parsed_response$choices) &&
                     length(parsed_response$choices) > 0 &&
                     !is.null(parsed_response$choices[[1]]$text)) {
                   return(parsed_response$choices[[1]]$text)
                 }
               },
               # Extract response for embedding models
               "embedding" = {
                 if (!is.null(parsed_response$data) &&
                     length(parsed_response$data) > 0 &&
                     !is.null(parsed_response$data[[1]]$embedding)) {
                   return(parsed_response$data[[1]]$embedding)
                 }
               }
        )

        stop("Unexpected API response format")

      }, error = function(e) {
        stop(paste("API request failed:", e$message))
      })

      return(response)
    }
  ),

  private = list(
    endpoint_url = NULL,  # Store the Azure ML endpoint URL
    model_type = NULL     # Store the type of model being used
  )
)

