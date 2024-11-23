# *AIGenFlow*: A Cutting-Edge R Framework for Creating, Managing, and Scaling LLM Agents, Teams, and Applications


<p align="center">
  <img src="https://github.com/user-attachments/assets/4acfac91-9b2d-4222-92c5-f5e4a7a9087b" alt="image">
</p>


## Description
*AIGenFlow* is a user-friendly, comprehensive R-based framework that simplifies integrating OpenAI’s GPT-4 and other Large Language Models (LLMs) into your R environment. Designed with ease of use in mind, it enables you to create intelligent agents and orchestrate workflows with just a few lines of code, making advanced AI capabilities accessible to developers, data scientists, and researchers across diverse fields.

### Key Features:
- **Simplicity and Versatility:** Build agents and workflows effortlessly while leveraging a highly intuitive and structured architecture. Based on best practices of high level object-oriented programming abstractions, *AIGenFlow* ensures a seamless user experience, even for complex tasks.
- **Powerful AI Integration:** Seamlessly connect with LLMs like GPT-4, Claude, and Mistral to enable natural language processing, conversational agents, and execution agents, all within R.
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

# Or install development version
devtools::install_github("username/aigenflow")
```

## Environment Setup
```r
Sys.setenv(OPENAI_API_KEY = "your-openai-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-key")
Sys.setenv(AZURE_OPENAI_KEY = "your-azure-key")
```

## Basic Usage
```r
library(aigenflow)

# Initialize models
openai_model <- OpenAIModel(
  model_name = "gpt-4",
  temperature = 0.7
)

anthropic_model <- AnthropicModel(
  model_name = "claude-3-opus-20240229",
  temperature = 0.7
)

azure_model <- AzureOpenAIModel(
  deployment_name = "your-deployment",
  resource_name = "your-resource"
)

# Create basic agent
agent <- Agent(
  model = openai_model,
  name = "Assistant",
  memory = ConversationMemory$new()
)

# Simple chat
response <- agent$chat(
  user_input = "What is machine learning?",
  system_prompt = "You are a data science expert."
)
```

## Data Analysis Example
```r
# Create data analysis agent
data_agent <- Agent(
  model = openai_model,
  name = "DataAnalyst",
  memory = ConversationMemory$new()
)

# Add analysis tools
analysis_tools <- list(
  analyze = analyze_data,
  summarize = function(data) summary(data),
  correlate = function(data) cor(data[sapply(data, is.numeric)])
)
add_tools(data_agent, analysis_tools)

# Analyze dataset
analysis_result <- data_agent$chat(
  user_input = "Analyze this dataset and explain the relationships.",
  system_prompt = "You are a data scientist."
)

# Use tools directly
correlations <- data_agent$use_tool("correlate", iris[, 1:4])
summary_stats <- data_agent$use_tool("summarize", iris)
```

## Multi-Agent Workflow Example
```r
# Initialize orchestrator
orchestrator <- Orchestrator$new()

# Add agents
orchestrator$add_agent("analyst", data_agent)
orchestrator$add_agent("writer", Agent(
  model = anthropic_model,
  name = "TechnicalWriter"
))

# Define workflow
orchestrator$define_workflow(
  name = "data_analysis",
  steps = list(
    list(
      name = "analyze",
      agent = "analyst",
      prompt = "Analyze this dataset: {{input}}",
      system_prompt = "You are a data scientist. Provide detailed analysis."
    ),
    list(
      name = "report",
      agent = "writer",
      prompt = "Create a clear report from this analysis: {{results.analyze}}",
      system_prompt = "You are a technical writer. Create clear, engaging reports."
    )
  )
)

# Execute workflow
results <- orchestrator$execute_workflow(
  workflow_name = "data_analysis",
  initial_input = iris,
  context = list(
    focus = "species classification",
    detail_level = "technical"
  )
)
```

## Automated Report Generation
```r
# Generate comprehensive report
report <- aigen_report(
  model = anthropic_model,
  data = iris,
  data_question = "What distinguishes different iris species?",
  system_prompt = "You are a botanist and data scientist."
)

# Access report components
cat(report$raw_analysis)
cat(report$interpretation)
cat(report$final_report)
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