library(R6)          
library(httr)        
library(jsonlite) 
library(ggplot2) 


#' @title Step Class
#' @description Step class for a single step in a chain, defined by its name and action.
#' @export
########################################
# Step Class for Chain Elements
########################################
# This class represents a single step in a chain, defined by its name and action.
# Each step processes data and passes the result to the next step.
Step <- R6Class(
  "Step",  # Class name representing a single step in a chain.
  public = list(
    # Constructor
    initialize = function(name, action) {
      # Purpose: Initializes the step with a name and an action.
      # Parameters:
      # - name: The name of the step (e.g., "Normalize Data").
      # - action: A function that defines the step's behavior.
      
      private$name <- name  # Stores the name of the step.
      private$action <- action  # Stores the action function.
    },
    
    # Execute the action of the step
    execute = function(input) {
      # Purpose: Executes the step's action on the provided input.
      # Parameters:
      # - input: The data to be processed by the step.
      
      return(private$action(input))  # Executes the action and returns the result.
    }
  ),
  private = list(
    name = NULL,  # Private variable to store the step's name.
    action = NULL  # Private variable to store the step's action.
  )
)