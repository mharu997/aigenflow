library(R6)          
library(httr)        
library(jsonlite) 
library(ggplot2) 

#' @title Tool Class
#' @description Tool class for a generic structure for implementing tools.
#' @export
########################################
# Tool Base Class
########################################

# This class provides a generic structure for implementing tools.
# Tools are functionalities that an agent can use to perform tasks (e.g., calculations).
Tool <- R6Class(
  "Tool",  # Class name representing a generic tool.
  public = list(
    name = NULL,  # Public variable to store the tool's name.
    
    # Constructor
    initialize = function(name) {
      # Purpose: Sets the name of the tool during initialization.
      # Parameters:
      # - name: The name of the tool (e.g., "Calculator").
      
      self$name <- name  # Assigns the name to the public variable.
    },
    
    # Abstract run method
    run = function(...) {
      # Purpose: Placeholder for the tool's functionality. Must be implemented by subclasses.
      stop("The run method must be implemented by subclasses.")  # Stops execution if not implemented.
    }
  )
)

#' @title CalculatorTool Class
#' @description CalculatorTool class for a simple calculator tool for evaluating mathematical expressions.
#' @export
########################################
# Example Tool Implementation: Calculator
########################################

# This class implements a simple calculator tool for evaluating mathematical expressions.
CalculatorTool <- R6Class(
  "CalculatorTool",
  inherit = Tool,  # Inherits from the generic Tool class.
  public = list(
    # Constructor
    initialize = function() {
      # Purpose: Initializes the Calculator tool with the name "Calculator".
      super$initialize("Calculator")  # Calls the parent class's initialize method.
    },
    
    # Run method to evaluate mathematical expressions
    run = function(expression) {
      # Purpose: Evaluates a mathematical expression and returns the result.
      # Parameters:
      # - expression: A string containing a mathematical expression (e.g., "2 + 2 * 5").
      
      result <- tryCatch({
        eval(parse(text = expression))  # Safely evaluates the expression.
      }, error = function(e) {
        NA  # Returns NA if an error occurs during evaluation.
      })
      return(result)  # Returns the result of the evaluated expression.
    }
  )
)

#' @title FileOperationsTool Class
#' @description FileOperationsTool class for a operations such as reading, writing, and listing files.
#' @export
########################################
# File Operations Tool
########################################

# This tool provides functionalities to perform file operations such as reading, writing, and listing files.
FileOperationsTool <- R6Class(
  "FileOperationsTool",
  inherit = Tool,  # Inherits from the generic Tool class.
  public = list(
    # Constructor
    initialize = function() {
      # Purpose: Initializes the File Operations tool with the name "FileOperations".
      super$initialize("FileOperations")  # Calls the parent class's initialize method.
    },
    
    # Run method for file operations
    run = function(operation, path, ...) {
      # Purpose: Executes a specified file operation (read, write, list files).
      # Parameters:
      # - operation: The type of file operation to perform ("read_csv", "write_csv", "list_files").
      # - path: The file path for the operation.
      # - ...: Additional arguments for specific operations.
      
      switch(operation,
             "read_csv" = read.csv(path, ...),        # Reads a CSV file and returns its content.
             "write_csv" = write.csv(..., file = path), # Writes data to a CSV file at the specified path.
             "list_files" = list.files(path, ...),    # Lists all files in the specified directory.
             stop("Unsupported operation")           # Raises an error for unsupported operations.
      )
    }
  )
)

#' @title VisualizationTool Class
#' @description VisualizationTool class to create visualizations using the ggplot2 library.
#' @export
########################################
# Visualization Tool
########################################

# This tool provides functionalities to create visualizations using the ggplot2 library.
VisualizationTool <- R6Class(
  "VisualizationTool",
  inherit = Tool,  # Inherits from the generic Tool class.
  public = list(
    # Constructor
    initialize = function() {
      # Purpose: Initializes the Visualization tool with the name "Visualization".
      super$initialize("Visualization")  # Calls the parent class's initialize method.
      if (!require(ggplot2)) install.packages("ggplot2")  # Installs ggplot2 if not already installed.
      library(ggplot2)  # Loads the ggplot2 library for creating visualizations.
    },
    
    # Run method for creating visualizations
    run = function(data, plot_type, x, y = NULL, color = NULL) {
      # Purpose: Generates visualizations based on the specified plot type.
      # Parameters:
      # - data: The dataset to visualize.
      # - plot_type: The type of plot to create ("scatter", "line", "histogram", "boxplot").
      # - x, y: The x and y variables for the plot.
      # - color: Optional; a variable for coloring the plot.
      
      p <- ggplot(data)  # Initializes a ggplot object with the dataset.
      
      switch(plot_type,
             "scatter" = p + geom_point(aes_string(x = x, y = y, color = color)),  # Creates a scatter plot.
             "line" = p + geom_line(aes_string(x = x, y = y, color = color)),      # Creates a line plot.
             "histogram" = p + geom_histogram(aes_string(x = x)),                  # Creates a histogram.
             "boxplot" = p + geom_boxplot(aes_string(x = x, y = y)),               # Creates a box plot.
             stop("Unsupported plot type")                                        # Raises an error for unsupported plot types.
      )
    }
  )
)

#' @title TextProcessingTool Class
#' @description TextProcessingTool class or text processing such as tokenization, word count, and case conversion.
#' @export
########################################
# Text Processing Tool
########################################

# This tool provides functionalities for text processing such as tokenization, word count, and case conversion.
TextProcessingTool <- R6Class(
  "TextProcessingTool",
  inherit = Tool,  # Inherits from the generic Tool class.
  public = list(
    # Constructor
    initialize = function() {
      # Purpose: Initializes the Text Processing tool with the name "TextProcessing".
      super$initialize("TextProcessing")  # Calls the parent class's initialize method.
    },
    
    # Run method for text processing operations
    run = function(text, operation = "tokenize") {
      # Purpose: Executes text processing tasks based on the specified operation.
      # Parameters:
      # - text: The input text to process.
      # - operation: The type of text processing task ("tokenize", "word_count", "to_lower", "to_upper").
      
      switch(operation,
             "tokenize" = strsplit(text, "\\s+")[[1]],  # Splits the text into individual words (tokens).
             "word_count" = length(strsplit(text, "\\s+")[[1]]),  # Counts the number of words in the text.
             "to_lower" = tolower(text),  # Converts the text to lowercase.
             "to_upper" = toupper(text),  # Converts the text to uppercase.
             stop("Unsupported text operation")  # Raises an error for unsupported operations.
      )
    }
  )
)

#' @title DataAnalysisTool Class
#' @description DataAnalysisTool class to performs detailed analysis on the dataset, including summary, structure, correlations, and missing values.
#' @export
########################################
# Data Analysis Tool with Detailed Analysis
########################################

# This version of the Data Analysis Tool includes more detailed analytical methods.
DataAnalysisTool <- R6Class(
  "DataAnalysisTool",
  inherit = Tool,  # Inherits from the generic Tool class.
  public = list(
    # Constructor
    initialize = function() {
      # Purpose: Initializes the Data Analysis tool with the name "DataAnalysis".
      super$initialize("DataAnalysis")  # Calls the parent class's initialize method.
    },
    
    # Run method with detailed analysis
    run = function(data) {
      # Purpose: Performs detailed analysis on the dataset, including summary, structure, correlations, and missing values.
      # Parameters:
      # - data: The dataset to analyze.
      
      # Perform multiple types of analysis on the dataset.
      analysis <- list(
        summary = summary(data),                                   # Summary statistics for the dataset.
        structure = str(data),                                    # Structure of the dataset.
        correlations = if (is.data.frame(data)) cor(data[sapply(data, is.numeric)]),  # Correlations for numeric variables.
        missing = colSums(is.na(data)),                           # Count of missing values per column.
        dimensions = dim(data)                                    # Dimensions (rows and columns) of the dataset.
      )
      
      # Convert the analysis results into a text format for easy display.
      result <- capture.output({
        cat("Dataset Dimensions:", paste(analysis$dimensions, collapse = " x "), "\n\n")  # Print dimensions.
        cat("Summary Statistics:\n")
        print(analysis$summary)  # Print summary statistics.
        cat("\nCorrelations between numeric variables:\n")
        print(analysis$correlations)  # Print correlation matrix.
        cat("\nMissing Values per Column:\n")
        print(analysis$missing)  # Print missing values.
      })
      
      return(paste(result, collapse = "\n"))  # Returns the formatted analysis results as a single string.
    }
  )
)
