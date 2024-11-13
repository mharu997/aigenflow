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

# This is the base class for defining language models. It provides a structure
# for initializing and generating responses from various language models.
LanguageModel <- R6Class(
  "LanguageModel",    # The class name, identifying the type of model.
  public = list(
    # Constructor
    initialize = function(model_name, api_key, max_tokens = 1000) {
      # Purpose: Initialize the class with the model's name, API key, and token limit.
      # Parameters:
      # - model_name: String specifying the language model's name (e.g., "GPT-4").
      # - api_key: String containing the API key for authentication.
      # - max_tokens: Integer specifying the maximum tokens for a single response.
      
      private$model_name <- model_name  # Assign the model name to a private variable.
      private$api_key <- api_key        # Assign the API key to a private variable.
      private$max_tokens <- max_tokens # Assign the maximum token limit to a private variable.
    },
    
    # Abstract generate method
    generate = function(messages, temperature = 0.7) {
      # Purpose: Placeholder for generating a response. It must be implemented by any subclass inheriting this class.
      # Parameters:
      # - messages: A list containing the input conversation or text.
      # - temperature: A float controlling the randomness of the output; higher values produce more creative outputs.
      stop("The generate method must be implemented by subclasses.") # Stops execution if not implemented in a subclass.
    }
  ),
  private = list(
    # Private variables that are not accessible directly outside the class.
    model_name = NULL,  # Stores the name of the language model.
    api_key = NULL,     # Stores the API key for secure API interaction.
    max_tokens = NULL   # Stores the maximum allowable tokens for a response.
  )
)


#' @title OpenAIModel Class
#' @description Implements functionality specific to OpenAI language models.
#' @export
#######################################
# OpenAI Model Implementation
########################################

# This class implements the functionality specific to OpenAI language models (e.g., GPT-4).
OpenAIModel <- R6Class(
  "OpenAIModel",      # The class name, representing the OpenAI-specific model implementation.
  inherit = LanguageModel,  # Inherits methods and properties from the base LanguageModel class.
  public = list(
    # Constructor with API key handling
    initialize = function(model_name, api_key = Sys.getenv("OPENAI_API_KEY"), max_tokens = 1000) {
      # Purpose: Initialize an OpenAI-specific language model. It validates and sets up the API key.
      # Parameters:
      # - model_name: Name of the OpenAI model to use (e.g., "GPT-4").
      # - api_key: API key for OpenAI, defaults to the environment variable "OPENAI_API_KEY".
      # - max_tokens: Maximum number of tokens allowed for responses.
      
      if (is.null(api_key) || api_key == "") {
        # Stops execution if no valid API key is found.
        stop("OpenAI API key not provided. Please set it as an environment variable 'OPENAI_API_KEY'.")
      }
      # Calls the parent class's initialize method to set model_name, api_key, and max_tokens.
      super$initialize(model_name, api_key, max_tokens)
    },
    
    # Generate method specific to OpenAI API
    generate = function(messages, temperature = 0.7) {
      # Purpose: Sends a request to the OpenAI API and generates a response based on input messages.
      # Parameters:
      # - messages: A list of input messages representing the conversation or query.
      # - temperature: A float controlling the randomness/creativity of the output.
      
      response <- tryCatch({
        # Makes an HTTP POST request to the OpenAI API for generating chat completions.
        result <- POST(
          url = "https://api.openai.com/v1/chat/completions",  # API endpoint URL.
          add_headers(
            Authorization = paste("Bearer", private$api_key),   # Authentication header using the API key.
            "Content-Type" = "application/json"                # Specifies that the request body is JSON.
          ),
          body = list(
            model = private$model_name,        # Specifies the model to use (e.g., "gpt-4").
            messages = messages,               # Passes the conversation history or input.
            temperature = temperature,         # Sets the randomness level for the response.
            max_tokens = private$max_tokens    # Sets the maximum length of the response.
          ),
          encode = "json"                      # Converts the body to JSON format.
        )
        
        # Converts the raw response content to a character string.
        response_text <- rawToChar(result$content)
        # Parses the JSON response into an R object.
        parsed_response <- fromJSON(response_text, simplifyVector = FALSE)
        
        # Checks if there was an error in the API response.
        if (!is.null(parsed_response$error)) {
          # Raises an error with the message from the API.
          stop(paste("API Error:", parsed_response$error$message))
        }
        
        # Extracts and returns the first response's content if available.
        if (length(parsed_response$choices) > 0 && 
            !is.null(parsed_response$choices[[1]]$message) &&
            !is.null(parsed_response$choices[[1]]$message$content)) {
          return(parsed_response$choices[[1]]$message$content)
        } else {
          # Raises an error for unexpected response formats.
          stop("Unexpected API response format")
        }
      }, error = function(e) {
        # Handles errors during the API request and raises an error with a descriptive message.
        stop(paste("API request failed:", e$message))
      })
      
      return(response)  # Returns the final response.
    }
  )
)
