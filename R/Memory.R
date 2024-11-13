library(R6)          
library(httr)        
library(jsonlite) 
library(ggplot2) 

#' @title Memory Class
#' @description Memory class for storing and managing memory data.
#' @export
########################################
# Base Memory Class
########################################

# This class provides a generic structure for storing and managing memory data.
# It serves as a base for implementing more specialized memory functionalities.
Memory <- R6Class(
  "Memory",  # Class name representing generic memory handling.
  public = list(
    # Constructor
    initialize = function() {
      # Purpose: Initializes an empty list to store memory entries.
      private$memory_data <- list()  # Creates an empty list to hold memory entries.
    },
    
    # Add entry to memory
    add_entry = function(entry) {
      # Purpose: Adds a new entry to the memory.
      # Parameters:
      # - entry: The data to be added to memory (e.g., conversation details).
      
      private$memory_data <- append(private$memory_data, list(entry))  # Adds the entry to the memory list.
    },
    
    # Retrieve memory data
    get_memory = function() {
      # Purpose: Retrieves all stored memory entries.
      return(private$memory_data)  # Returns the list containing all memory entries.
    },
    
    # Clear memory data
    clear_memory = function() {
      # Purpose: Clears all stored memory data by resetting the list.
      private$memory_data <- list()  # Resets the memory data to an empty list.
    }
  ),
  private = list(
    memory_data = NULL  # Private variable to store memory data securely.
  )
)

#' @title ConversationMemory Class
#' @description Memory class for store and manage conversation history.
#' @export
########################################
# Conversation Memory Implementation
########################################

# This class extends the base Memory class to store and manage conversation history.
# It adds features specific to conversational context, such as roles and timestamps.
ConversationMemory <- R6Class(
  "ConversationMemory",
  inherit = Memory,  # Inherits from the generic Memory class.
  public = list(
    # Add a message to the conversation history
    add_message = function(role, content) {
      # Purpose: Adds a conversational message with metadata to the memory.
      # Parameters:
      # - role: The role of the message sender (e.g., "user", "assistant").
      # - content: The actual message text.
      
      entry <- list(
        role = role,               # Stores the sender's role.
        content = content,         # Stores the message content.
        timestamp = Sys.time()     # Records the current timestamp.
      )
      self$add_entry(entry)  # Adds the message entry to memory.
    },
    
    # Retrieve recent messages for context
    get_recent_messages = function(n = 5) {
      # Purpose: Retrieves the last `n` messages from the conversation history.
      # Parameters:
      # - n: The number of recent messages to retrieve.
      
      len <- length(private$memory_data)       # Gets the total number of messages stored.
      start_idx <- max(1, len - n + 1)         # Calculates the start index for retrieval.
      return(private$memory_data[start_idx:len])  # Returns the most recent `n` messages.
    }
  )
)
