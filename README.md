Sys.setenv(OPENAI_API_KEY = "your-openai-api-key")

library(aigenflow)

# Create an OpenAI model instance
openai_model <- OpenAIModel$new(
  model_name = "gpt-4"
)

# Create an agent with the OpenAI model
agent <- Agent$new(model = openai_model)

# Add a calculator tool to the agent
calculator_tool <- CalculatorTool$new()
agent$add_tool(calculator_tool)

# Start a conversation with the agent
response <- agent$chat(
  user_input = "What is random forest and when should I use it?",
  system_prompt = "You are a data science expert specializing in machine learning.",
  context_window = 5,
  temperature = 0.7
)
cat("Assistant:", response, "\n")

# Use the Calculator tool through the agent
calculation_result <- agent$use_tool("Calculator", "2 + 2 * 5")
cat("Calculator Result:", calculation_result, "\n")


########################################
# Example Usage: Advanced Agent Capabilities  with iris dataset
########################################

# This section demonstrates more advanced usage of the agent and tools for tasks such as
# data analysis and report generation.

# Create agents
data_agent <- Agent$new(model = openai_model)  # Initializes an agent for data analysis.
data_agent$add_tool(DataAnalysisTool$new())   # Adds the DataAnalysis tool to the agent.

writer_agent <- Agent$new(model = openai_model)  # Initializes a separate agent for writing tasks.


# Demonstrates how the summarize_and_report function can be applied to a dataset like iris.
results <- summarize_and_report(
  iris,  # The classic iris dataset used for this example.
  "What are the main patterns and relationships between iris flower measurements?"  # Custom question for analysis.
)

# Print the results
cat("Raw Analysis:\n", results$raw_analysis, "\n\n")         # Prints the raw data analysis.
cat("Data Interpretation:\n", results$interpretation, "\n\n") # Prints the interpreted insights.
cat("Final Report:\n", results$final_report, "\n")           # Prints the final user-friendly report.



