# *AIGenFlow* by *KatmAI Labs*: An R Framework To Build LLM Applications, Agents, Tools, and Automated Workstreams

## Description
*AIGenFlow* is a user-friendly, comprehensive R-based framework that simplifies integrating OpenAI’s GPT-4 and other Large Language Models (LLMs) into your R environment. Designed with ease of use in mind, it enables you to create intelligent agents and orchestrate workflows with just a few lines of code, making advanced AI capabilities accessible to developers, data scientists, and researchers across diverse fields.

### Key Features:
- **Simplicity and Versatility:** Build agents and workflows effortlessly while leveraging a highly intuitive and structured, object-oriented architecture. *AIGenFlow* ensures a seamless user experience, even for complex tasks.
- **Powerful AI Integration:** Seamlessly connect with LLMs like GPT-4 and other large language models to enable natural language processing, conversational agents, and execution agents, all within R.
- **Customizable Tools:** Extend agent functionalities and workflows with highly customizable tools for invoking agentic actions, ensuring the framework adapts to your unique needs.
- **Constantly Evolving:** Though experimental, *AIGenFlow* is regularly updated to enhance its functionality, versatility, and usability, ensuring it remains a cutting-edge solution.

Whether you’re automating workflows, building conversational agents, or unlocking insights through AI-driven solutions, *AIGenFlow* empowers you to innovate and streamline your projects—all within the familiar R environment.

## Table of Contents
- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Core Components](#core-components)
- [Examples](#examples)
- [Applications](#applications)
- [Contributing](#contributing)
- [License](#license)

## Features
- **Simplified API Integration**: Abstract LLM and Agent classes with seamless language model integrations
- **State Management**: Built-in conversation memory system for Agents
- **Modular Tool Framework**: Extensible architecture for custom tools
- **Workflow Automation**: Chain and Step classes for complex operations
- **Advanced Analytics**: AI-powered data interpretation and report generation
- **Conversation Context**: Maintains context across multiple interactions

## Installation
```r
# Install from CRAN
install.packages("aigenflow")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("username/aigenflow")
```

## Quick Start
```r
# Load the package
library(aigenflow)

# Set your OpenAI API key
Sys.setenv(OPENAI_API_KEY = "your-openai-api-key")

# Initialize the OpenAI model
openai_model <- OpenAIModel$new(model_name = "gpt-4")

# Create an agent with memory
agent <- Agent$new(model = openai_model, memory = ConversationMemory$new())

# Start chatting!
response <- agent$chat(
  user_input = "Can you explain the bias-variance tradeoff?",
  system_prompt = "You are a statistics expert.",
  context_window = 5
)
```

## Core Components

### Language Models
```r
# Initialize different language models
openai_model <- OpenAIModel$new(model_name = "gpt-4")
```

### Agents with Memory
```r
# Create an agent with conversation memory
agent <- Agent$new(
  model = openai_model,
  memory = ConversationMemory$new()
)
```

### Tools (Currently supports 6 out-of-box data analysis and visualization tools)
```r
# Add tools to extend functionality
data_analysis_tool <- DataAnalysisTool$new()
agent$add_tool(data_analysis_tool)
```

### Workflow Chains
```r
# Create a data processing workflow
workflow <- Chain$new(steps = list(
  load_data_step,
  clean_data_step,
  analyze_data_step
))
```

## Examples

### Creating Q&A and Calculator Agents 
```r
Sys.setenv(OPENAI_API_KEY = "YOUR_KEY")
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
```
### Creating Data Analysis and Reporting Assistant
```r
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
```



## Applications
The package enables building various applications including:
1. **Intelligent Virtual Assistants**: For data analysis and interpretation
2. **Automated Report Generation**: Create comprehensive reports from data
3. **Educational Chatbots**: Interactive learning and tutoring
4. **Smart Customer Support**: Handle inquiries and troubleshooting
5. **Data Pipeline Management**: Automate complex data processing workflows

## Contributing
We welcome contributions! Please follow these steps:
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License
Still researching liceence modes.
