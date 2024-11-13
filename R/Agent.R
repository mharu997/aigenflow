library(R6)          
library(httr)        
library(jsonlite) 
library(ggplot2) 

#' @title Agent Class
#' @description Agent class for an agent that interacts with language models and tools.
#' @export
########################################
# Agent Class
########################################

# This class represents an agent that interacts with language models and tools.
# The agent maintains conversation memory and integrates tools for additional functionality.
Agent <- R6Class(
  "Agent",  # Class name representing the agent.
  public = list(
    # Constructor
    initialize = function(model, memory = NULL, tools = list()) {
      # Purpose: Initializes the agent with a language model, optional memory, and a set of tools.
      # Parameters:
      # - model: An instance of a language model (e.g., OpenAIModel).
      # - memory: Optional; an instance of memory (e.g., ConversationMemory).
      # - tools: A list of tools that the agent can use.
      
      private$model <- model  # Assigns the language model to a private variable.
      private$memory <- if (is.null(memory)) ConversationMemory$new() else memory  # Initializes memory.
      private$tools <- tools  # Stores the list of tools.
    },
    
    # Chat method to interact with the agent
    chat = function(user_input, system_prompt = NULL, context_window = 5, temperature = 0.7) {
      # Purpose: Processes user input, generates a response using the model, and updates the memory.
      # Parameters:
      # - user_input: The user's input message or query.
      # - system_prompt: Optional; a system-level instruction for the agent (e.g., "You are a data analyst.").
      # - context_window: The number of recent messages to include in the context.
      # - temperature: Controls the randomness/creativity of the model's response.
      
      private$memory$add_message("user", user_input)  # Adds the user's input to conversation memory.
      messages <- self$build_prompt(system_prompt, context_window)  # Builds a prompt including context.
      response <- private$model$generate(messages, temperature = temperature)  # Generates a response using the model.
      private$memory$add_message("assistant", response)  # Adds the model's response to conversation memory.
      return(response)  # Returns the generated response.
    },
    
    # Build the prompt with system prompt and conversation history
    build_prompt = function(system_prompt = NULL, context_window = 5) {
      # Purpose: Constructs a prompt by combining system instructions and recent conversation history.
      # Parameters:
      # - system_prompt: Optional; a system-level instruction for the agent.
      # - context_window: The number of recent messages to include in the context.
      
      messages <- list()  # Initializes an empty list for the prompt.
      
      # Adds the system prompt to the messages list if provided.
      if (!is.null(system_prompt)) {
        messages <- append(messages, list(list(role = "system", content = system_prompt)))
      }
      
      # Retrieves recent conversation history and adds it to the prompt.
      recent_messages <- private$memory$get_recent_messages(context_window * 2)
      for (msg in recent_messages) {
        messages <- append(messages, list(list(role = msg$role, content = msg$content)))
      }
      return(messages)  # Returns the complete prompt.
    },
    
    # Add a tool to the agent
    add_tool = function(tool) {
      # Purpose: Adds a tool to the agent's set of available tools.
      # Parameters:
      # - tool: An instance of a tool to be added (e.g., CalculatorTool).
      
      private$tools[[tool$name]] <- tool  # Adds the tool to the private tools list, keyed by its name.
    },
    
    # Use a tool by name
    use_tool = function(tool_name, ...) {
      # Purpose: Executes a specific tool by its name.
      # Parameters:
      # - tool_name: The name of the tool to execute.
      # - ...: Additional arguments required by the tool.
      
      tool <- private$tools[[tool_name]]  # Retrieves the tool from the list by its name.
      if (is.null(tool)) {
        stop(paste("Tool", tool_name, "not found"))  # Raises an error if the tool is not found.
      }
      return(tool$run(...))  # Executes the tool's `run` method and returns the result.
    },
    
    # Clear the conversation memory
    clear_memory = function() {
      # Purpose: Clears all stored conversation history from memory.
      private$memory$clear_memory()  # Invokes the memory's `clear_memory` method.
    }
  ),
  private = list(
    model = NULL,  # Private variable to store the language model.
    memory = NULL,  # Private variable to store the conversation memory.
    tools = NULL  # Private variable to store the list of tools.
  )
)