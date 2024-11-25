library("R6")
library("httr")
library("jsonlite")

#' @title Orchestrator Class
#' @description Manages interactions and workflows between multiple agents
#' @export
Orchestrator <- R6Class(
  "Orchestrator",
  public = list(
    #' @description Initialize a new orchestrator
    #' @param agents Named list of Agent instances
    #' @param workflows Named list of workflow definitions
    initialize = function(agents = list(), workflows = list()) {
      private$validate_agents(agents)
      private$agents <- agents
      private$workflows <- workflows
      private$history <- list()
      private$active_workflow <- NULL
    },
    
    #' @description Add an agent to the orchestrator
    #' @param name Name for the agent
    #' @param agent Instance of Agent class
    add_agent = function(name, agent) {
      if (!inherits(agent, "aigen_Agent")) {
        stop("agent must be an instance of aigen_Agent")
      }
      private$agents[[name]] <- agent
      invisible(self)
    },
    
    #' @description Define a new workflow
    #' @param name Workflow name
    #' @param steps List of workflow steps
    #' @param conditions Optional conditions for workflow transitions
    define_workflow = function(name, steps, conditions = NULL) {
      if (!is.character(name) || length(name) != 1) {
        stop("name must be a single character string")
      }
      
      # Validate steps format
      if (!is.list(steps) || length(steps) == 0) {
        stop("steps must be a non-empty list")
      }
      
      # Validate that referenced agents exist
      agent_names <- names(private$agents)
      for (step in steps) {
        if (!step$agent %in% agent_names) {
          stop(sprintf("Agent '%s' not found", step$agent))
        }
      }
      
      workflow <- list(
        steps = steps,
        conditions = conditions,
        current_step = 1
      )
      
      private$workflows[[name]] <- workflow
      invisible(self)
    },
    
    #' @description Execute a specific workflow
    #' @param workflow_name Name of the workflow to execute
    #' @param initial_input Initial input for the workflow
    #' @param context Optional context data for the workflow
    execute_workflow = function(workflow_name, initial_input, context = list()) {
      if (!workflow_name %in% names(private$workflows)) {
        stop(sprintf("Workflow '%s' not found", workflow_name))
      }
      
      workflow <- private$workflows[[workflow_name]]
      private$active_workflow <- workflow_name
      
      result <- private$execute_workflow_steps(
        workflow = workflow,
        input = initial_input,
        context = context
      )
      
      private$active_workflow <- NULL
      return(result)
    },
    
    #' @description Get the execution history
    #' @param workflow_name Optional name to filter history by workflow
    get_history = function(workflow_name = NULL) {
      if (!is.null(workflow_name)) {
        return(private$history[[workflow_name]])
      }
      return(private$history)
    },
    
    #' @description Clear execution history
    #' @param workflow_name Optional name to clear specific workflow history
    clear_history = function(workflow_name = NULL) {
      if (!is.null(workflow_name)) {
        private$history[[workflow_name]] <- NULL
      } else {
        private$history <- list()
      }
      invisible(self)
    }
  ),
  
  private = list(
    agents = NULL,
    workflows = NULL,
    history = NULL,
    active_workflow = NULL,
    
    validate_agents = function(agents) {
      if (length(agents) > 0) {
        if (is.null(names(agents))) {
          stop("agents must be a named list")
        }
        for (agent in agents) {
          if (!inherits(agent, "aigen_Agent")) {
            stop("all agents must be instances of aigen_Agent")
          }
        }
      }
    },
    
    execute_workflow_steps = function(workflow, input, context) {
      results <- list()
      current_input <- input
      
      for (i in seq_along(workflow$steps)) {
        step <- workflow$steps[[i]]
        
        # Check if conditions are met for this step
        if (!is.null(workflow$conditions)) {
          condition <- workflow$conditions[[i]]
          if (!is.null(condition) && !private$evaluate_condition(condition, context, results)) {
            next
          }
        }
        
        # Execute the step
        agent <- private$agents[[step$agent]]
        
        # Prepare the system prompt if provided
        system_prompt <- if (!is.null(step$system_prompt)) {
          private$prepare_prompt(step$system_prompt, context, results)
        } else {
          NULL
        }
        
        # Execute the step and store result
        result <- agent$chat(
          user_input = private$prepare_prompt(step$prompt, context, list(input = current_input, results = results)),
          system_prompt = system_prompt
        )
        
        # Store result
        results[[step$name]] <- result
        
        # Update input for next step if specified
        if (!is.null(step$output_to_input) && step$output_to_input) {
          current_input <- result
        }
        
        # Record in history
        private$record_step(
          workflow_name = private$active_workflow,
          step_name = step$name,
          agent_name = step$agent,
          input = current_input,
          output = result
        )
      }
      
      return(results)
    },
    
    prepare_prompt = function(template, context, data) {
      # Helper function to summarize dataset
      summarize_dataset <- function(df) {
        # Get basic structure
        dims <- dim(df)
        col_types <- sapply(df, class)
        n_missing <- colSums(is.na(df))
        
        # Identify numeric columns for basic stats
        numeric_cols <- sapply(df, is.numeric)
        
        # Build summary
        summary_parts <- list(
          # Basic information
          sprintf("Dataset Overview:\n- Dimensions: %d rows × %d columns", dims[1], dims[2]),
          
          # Column information
          sprintf("Column Types:\n%s", 
                  paste(sprintf("- %s: %s", names(col_types), col_types), 
                        collapse = "\n")),
          
          # Missing values (only if there are any)
          if(any(n_missing > 0)) {
            sprintf("Missing Values:\n%s",
                    paste(sprintf("- %s: %d", names(n_missing), n_missing),
                          collapse = "\n"))
          },
          
          # Basic statistics for numeric columns
          if(any(numeric_cols)) {
            numeric_summary <- summary(df[, numeric_cols, drop = FALSE])
            sprintf("Numeric Column Statistics:\n%s",
                    paste(capture.output(numeric_summary), collapse = "\n"))
          },
          
          # Categorical column summaries (if any)
          if(any(!numeric_cols)) {
            cat_summaries <- lapply(names(df)[!numeric_cols], function(col) {
              vals <- table(df[[col]])
              if(length(vals) <= 10) { # Only show if reasonably small number of categories
                sprintf("- %s: %s", col, 
                        paste(sprintf("%s (%d)", names(vals), vals), collapse = ", "))
              } else {
                sprintf("- %s: %d unique values", col, length(unique(df[[col]])))
              }
            })
            sprintf("Categorical Columns:\n%s", paste(cat_summaries, collapse = "\n"))
          },
          
          # Sample data (first few rows)
          sprintf("Sample Data (first 5 rows):\n%s",
                  paste(capture.output(head(df, 5)), collapse = "\n"))
        )
        
        # Combine all parts
        paste(unlist(summary_parts), collapse = "\n\n")
      }
      
      # Helper function to format vectors and lists
      format_value <- function(value) {
        if (is.data.frame(value)) {
          summarize_dataset(value)
        } else if (is.list(value)) {
          paste(sapply(names(value), function(n) {
            sprintf("%s:\n%s", n, format_value(value[[n]]))
          }), collapse = "\n")
        } else if (is.vector(value) && length(value) > 1) {
          if (is.numeric(value)) {
            # Provide summary statistics for numeric vectors
            stats <- summary(value)
            paste("Vector summary:", 
                  paste(names(stats), stats, sep = ": ", collapse = ", "),
                  sprintf("\nLength: %d", length(value)))
          } else {
            # For other vectors, show length and sample if long
            if (length(value) > 10) {
              paste0(paste(value[1:10], collapse = ", "), 
                     sprintf("... (%d more items)", length(value) - 10))
            } else {
              paste(value, collapse = ", ")
            }
          }
        } else {
          as.character(value)
        }
      }
      
      result <- template
      
      # Replace context variables
      if (length(context) > 0) {
        for (name in names(context)) {
          pattern <- sprintf("\\{\\{context.%s\\}\\}", name)
          result <- gsub(pattern, format_value(context[[name]]), result)
        }
      }
      
      # Replace data variables
      if (length(data) > 0) {
        for (name in names(data)) {
          if (is.list(data[[name]])) {
            for (subname in names(data[[name]])) {
              pattern <- sprintf("\\{\\{%s.%s\\}\\}", name, subname)
              result <- gsub(pattern, format_value(data[[name]][[subname]]), result)
            }
          }
          pattern <- sprintf("\\{\\{%s\\}\\}", name)
          result <- gsub(pattern, format_value(data[[name]]), result)
        }
      }
      
      return(result)
    },
    
    evaluate_condition = function(condition, context, results) {
      # Evaluate condition expression with current context and results
      tryCatch({
        eval(
          parse(text = private$prepare_prompt(condition, context, list(results = results))),
          envir = new.env()
        )
      }, error = function(e) FALSE)
    },
    
    record_step = function(workflow_name, step_name, agent_name, input, output) {
      if (is.null(private$history[[workflow_name]])) {
        private$history[[workflow_name]] <- list()
      }
      
      step_record <- list(
        timestamp = Sys.time(),
        step_name = step_name,
        agent_name = agent_name,
        input = input,
        output = output
      )
      
      private$history[[workflow_name]] <- c(
        private$history[[workflow_name]],
        list(step_record)
      )
    }
  )
)
