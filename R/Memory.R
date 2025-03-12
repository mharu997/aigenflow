library("R6")
library("httr")
library("jsonlite")
library("ggplot2") 

library("R6")

#' @title Memory Class
#' @description Base class for storing and managing memory data.
#' @private
########################################
# Base Memory Class
########################################

Memory <- R6Class(
  "Memory",
  public = list(
    # Constructor
    initialize = function() {
      private$memory_data <- list()
    },
    
    # Add entry to memory
    add_entry = function(entry) {
      if (!is.list(entry) || is.null(entry$role) || is.null(entry$content)) {
        stop("Entry must be a list with 'role' and 'content' elements")
      }
      
      # Add timestamp if not provided
      if (is.null(entry$timestamp)) {
        entry$timestamp <- Sys.time()
      }
      
      private$memory_data <- append(private$memory_data, list(entry))
      invisible(self)
    },
    
    # Retrieve memory data
    get_memory = function() {
      return(private$memory_data)
    },
    
    # Clear memory data
    clear_memory = function() {
      private$memory_data <- list()
      invisible(self)
    },
    
    # Get count of messages
    count = function() {
      return(length(private$memory_data))
    }
  ),
  private = list(
    memory_data = NULL
  )
)

#' @title ConversationMemory Class
#' @description Memory class for store and manage conversation history.
#' @private
########################################
# Conversation Memory Implementation
########################################

ConversationMemory <- R6Class(
  "ConversationMemory",
  inherit = Memory,
  public = list(
    # Add a message to the conversation history
    add_message = function(role, content) {
      if (!is.character(role) || length(role) != 1) {
        stop("Role must be a single character string")
      }
      
      if (!is.character(content) || length(content) != 1) {
        stop("Content must be a single character string")
      }
      
      entry <- list(
        role = role,
        content = content,
        timestamp = Sys.time()
      )
      
      self$add_entry(entry)
      invisible(self)
    },
    
    # Retrieve recent messages for context
    get_recent_messages = function(n = 5) {
      memory_data <- private$memory_data
      len <- length(memory_data)
      
      if (len == 0 || n <= 0) {
        return(list())
      }
      
      start_idx <- max(1, len - n + 1)
      return(memory_data[start_idx:len])
    },
    
    # Get messages by role
    get_messages_by_role = function(role, n = Inf) {
      if (!is.character(role) || length(role) != 1) {
        stop("Role must be a single character string")
      }
      
      filtered <- Filter(function(msg) msg$role == role, private$memory_data)
      
      if (is.finite(n) && n > 0) {
        len <- length(filtered)
        if (len > n) {
          filtered <- filtered[(len - n + 1):len]
        }
      }
      
      return(filtered)
    }
  )
)


#' @title LimitedConversationMemory Class
#' @description Memory class for storing conversation history with a size limit
#' @private
LimitedConversationMemory <- R6Class(
  "LimitedConversationMemory",
  inherit = ConversationMemory,
  
  public = list(
    #' @description Initialize memory with size limit
    #' @param max_messages Maximum number of messages to retain
    initialize = function(max_messages = NULL) {
      super$initialize()
      
      if (!is.null(max_messages)) {
        if (!is.numeric(max_messages) || max_messages < 1) {
          stop("max_messages must be a positive number or NULL")
        }
        max_messages <- as.integer(max_messages)
      }
      
      private$max_messages <- max_messages
      private$preservation_pairs <- TRUE
    },
    
    #' @description Add a message while respecting size limit with intelligent preservation
    #' @param role The role of the message sender
    #' @param content The message content
    add_message = function(role, content) {
      # First add the new message
      super$add_message(role, content)
      
      # Then enforce the size limit if needed
      if (!is.null(private$max_messages) && self$count() > private$max_messages) {
        private$trim_to_capacity()
      }
      
      invisible(self)
    },
    
    #' @description Set pair preservation mode
    #' @param preserve Whether to preserve user-assistant pairs
    set_preserve_pairs = function(preserve = TRUE) {
      private$preservation_pairs <- isTRUE(preserve)
      invisible(self)
    },
    
    #' @description Get maximum messages setting
    get_max_messages = function() {
      return(private$max_messages)
    },
    
    #' @description Change the maximum messages limit
    #' @param new_max New maximum message count
    set_max_messages = function(new_max) {
      if (!is.null(new_max)) {
        if (!is.numeric(new_max) || new_max < 1) {
          stop("new_max must be a positive number or NULL")
        }
        new_max <- as.integer(new_max)
      }
      
      private$max_messages <- new_max
      
      # If the new limit is smaller than current count, trim immediately
      if (!is.null(new_max) && self$count() > new_max) {
        private$trim_to_capacity()
      }
      
      invisible(self)
    },
    
    #' @description Export memory as data frame
    #' @param include_timestamps Whether to include timestamps
    as_data_frame = function(include_timestamps = FALSE) {
      memory_data <- private$memory_data
      
      if (length(memory_data) == 0) {
        cols <- if (include_timestamps) {
          c("role", "content", "timestamp")
        } else {
          c("role", "content")
        }
        return(data.frame(matrix(ncol = length(cols), nrow = 0, 
                                 dimnames = list(NULL, cols))))
      }
      
      df_data <- do.call(rbind, lapply(memory_data, function(msg) {
        if (include_timestamps) {
          data.frame(
            role = msg$role,
            content = msg$content,
            timestamp = msg$timestamp,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            role = msg$role,
            content = msg$content,
            stringsAsFactors = FALSE
          )
        }
      }))
      
      return(df_data)
    }
  ),
  
  private = list(
    max_messages = NULL,
    preservation_pairs = TRUE,
    
    #' @description Trim memory to capacity with context preservation
    trim_to_capacity = function() {
      if (is.null(private$max_messages) || private$max_messages <= 0) {
        return()
      }
      
      current_count <- self$count()
      
      if (current_count <= private$max_messages) {
        return()  # Already within limits
      }
      
      # Calculate how many to remove
      to_remove <- current_count - private$max_messages
      
      if (to_remove <= 0) {
        return()  # Nothing to remove
      }
      
      messages <- private$memory_data
      
      if (private$preservation_pairs && to_remove == 1) {
        # If removing just one and pair preservation is on, check if we need to remove a pair
        if (length(messages) >= 2) {
          first_two_roles <- c(messages[[1]]$role, messages[[2]]$role)
          if (setequal(first_two_roles, c("user", "assistant"))) {
            # Remove two messages to preserve the pair structure
            private$memory_data <- messages[3:length(messages)]
            return()
          }
        }
      }
      
      if (private$preservation_pairs) {
        # Handle complex pair preservation
        # This ensures we don't break user-assistant pairs when possible
        preserved <- list()
        pairs <- list()
        
        # Group into pairs while preserving order
        i <- 1
        while (i < length(messages)) {
          if (i + 1 <= length(messages) && 
              ((messages[[i]]$role == "user" && messages[[i+1]]$role == "assistant") ||
               (messages[[i]]$role == "assistant" && messages[[i+1]]$role == "user"))) {
            # It's a pair
            pairs <- c(pairs, list(list(messages[[i]], messages[[i+1]])))
            i <- i + 2
          } else {
            # Standalone message
            preserved <- c(preserved, list(messages[[i]]))
            i <- i + 1
          }
        }
        
        # Handle any trailing message
        if (i <= length(messages)) {
          preserved <- c(preserved, list(messages[[i]]))
        }
        
        # Calculate how many units (pairs or individual messages) to keep
        total_units <- length(pairs) + length(preserved)
        units_to_keep <- max(0, total_units - to_remove)
        
        # Prioritize keeping pairs
        pairs_to_keep <- min(length(pairs), units_to_keep)
        preserved_to_keep <- max(0, units_to_keep - pairs_to_keep)
        
        # Rebuild the memory
        selected_pairs <- pairs[(length(pairs) - pairs_to_keep + 1):length(pairs)]
        selected_preserved <- preserved
        if (length(preserved) > preserved_to_keep) {
          selected_preserved <- preserved[(length(preserved) - preserved_to_keep + 1):length(preserved)]
        }
        
        # Flatten the list structure
        new_memory <- list()
        for (pair in selected_pairs) {
          new_memory <- c(new_memory, pair)
        }
        new_memory <- c(new_memory, selected_preserved)
        
        # Ensure we don't exceed max_messages (could happen with pair preservation)
        if (length(new_memory) > private$max_messages) {
          new_memory <- new_memory[(length(new_memory) - private$max_messages + 1):length(new_memory)]
        }
        
        private$memory_data <- new_memory
      } else {
        # Simple removal of oldest messages without pair preservation
        private$memory_data <- messages[(to_remove + 1):length(messages)]
      }
    }
  )
)