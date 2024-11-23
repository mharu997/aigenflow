library(R6)          
library(httr)        
library(jsonlite) 
library(ggplot2) 

#' @title Chain Class
#' @description Chain class for a sequenced of steps that can be executed in a defined order.
#' @export
########################################
# Chain Class for High-Level Abstraction
########################################

# This class represents a sequence of steps that can be executed in a defined order.
# It enables the chaining of actions to process data or perform tasks incrementally.
Chain <- R6Class(
  "Chain",  # Class name representing a chain of actions.
  public = list(
    # Constructor
    initialize = function(steps = list()) {
      # Purpose: Initializes the chain with a sequence of steps.
      # Parameters:
      # - steps: A list of step objects, each representing an action in the chain.
      
      private$steps <- steps  # Stores the list of steps.
    },
    
    # Add a step to the chain
    add_step = function(step) {
      # Purpose: Adds a new step to the chain.
      # Parameters:
      # - step: An instance of a step object.
      
      private$steps <- c(private$steps, list(step))  # Appends the step to the existing list.
    },
    
    # Run the chain of steps
    run = function(input) {
      # Purpose: Executes the chain of steps sequentially on the provided input.
      # Parameters:
      # - input: The initial input to be processed by the chain.
      
      output <- input  # Starts with the provided input.
      for (step in private$steps) {
        output <- step$execute(output)  # Executes each step in sequence, passing the output to the next step.
      }
      return(output)  # Returns the final output after processing all steps.
    }
  ),
  private = list(
    steps = NULL  # Private variable to store the list of steps.
  )
)
