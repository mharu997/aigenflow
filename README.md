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

# AIgen: R Package for AI Agent Orchestration

## Installation
```r
# Install from CRAN
install.packages("aigenflow")

# Or install development version
devtools::install_github("mharu997/aigenflow")
```
## Environment Setup
```r
Sys.setenv(OPENAI_API_KEY = "your-openai-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-key")
Sys.setenv(AZURE_OPENAI_KEY = "your-azure-key")
```
## Basic Usage
```r
# Create an OpenAI model instance
model <- OpenAIModel(
  model_name = "gpt-4o",
  temperature = 0.7
)

# Create a basic agent with conversation memory
agent <- Agent(
  model = model,
  name = "Assistant",
  memory = AgentConversationStore(max_messages = 10)
)

# Chat with the agent
response <- agent$chat(
  user_input = "What are the key principles of data analysis?",
  system_prompt = "You are a data science educator. Explain concepts clearly and concisely."
)

print(cat(response))
```

## Data Analysis Example
```r
# Create model and agent with analysis tools
model <- OpenAIModel("gpt-4o")

# Define analysis tools
analysis_tools <- list(
  analyze_data,
  summarize = function(data) summary(data),
  correlate = function(data) cor(data)
)

# Create analysis agent with tools
analyst <- Agent(
  model = model,
  name = "DataAnalyst",
  tools = analysis_tools
)
data("mtcars")
# Analyze mtcars dataset
result <- analyst$chat(
  user_input = "Analyze the mtcars dataset. Focus on the relationship between mpg and other variables.",
  system_prompt = "You are a data analyst. Use the available tools to analyze the data and provide insights."
)


```

## Multi-Agent Workflow Example
```r
# Define agents with different roles
agents <- list(
  analyst = list(
    model = model,
    tools = list(
      analyze = analyze_data
    )
  ),
  interpreter = list(
    model = model
  ),
  reporter = list(
    model = model
  )
)

# Create orchestrator
orchestrator <- OrchestrateFlow(agents = agents)

# Define workflow
workflow <- DesignFlow(
  orchestrator = orchestrator,
  name = "data_analysis_workflow",
  steps = list(
    list(
      name = "analyze",
      agent = "analyst",
      prompt = "Analyze this dataset: {{input}}",
      output_to_input = TRUE
    ),
    list(
      name = "interpret",
      agent = "interpreter",
      prompt = "Interpret these statistical results: {{input}}",
      system_prompt = "You are a data scientist. Explain technical findings in clear terms.",
      output_to_input = TRUE
    ),
    list(
      name = "report",
      agent = "reporter",
      prompt = "Create a business-friendly report from this analysis: {{input}}",
      system_prompt = "You are a business analyst. Create clear, actionable reports."
    )
  )
)

# Execute workflow with mtcars dataset
results <- ExecuteFlow(
  orchestrator = orchestrator,
  workflow = "data_analysis_workflow",
  input = mtcars,
  context = list(
    dataset_name = "mtcars",
    analysis_time = Sys.time()
  )
)
cat(results$analyze, results$interpret, results$report)
```

## Automated Report Generation
```r
# Create model
model <- OpenAIModel("gpt-4o")

# Generate comprehensive report for mtcars dataset
report <- aigen_report(
  model = model,
  data = mtcars,
  data_question = "What factors most strongly influence a car's fuel efficiency?",
  system_prompt = "You are a data scientist and technical writer. 
                  Analyze the data and create clear, insightful reports."
)

# Access report components
print(report)  # Prints full report with metadata
cat(report$raw_analysis)  # Technical analysis
cat(report$interpretation)  # Expert interpretation
cat(report$final_report)  # Executive summary
```

## Additional Features

### Using Different Model Providers
```r
# Using Anthropic Claude
claude_model <- AnthropicModel(
  model_name = "claude-3-opus-20240229",
  temperature = 0.7
)

# Using Azure OpenAI
azure_model <- AzureOpenAIModel(
  deployment_name = "your-deployment",
  resource_name = "your-resource"
)

# Using Azure ML Endpoint
azure_endpoint <- AzureMLEndpoint(
  endpoint_url = "your-endpoint-url",
  model_type = "chat"
)
```

### Adding Tools to Existing Agents
```r
# Add new tools to an agent
add_tools(analyst, 
          tools = list(
            visualize = function(data) {
              ggplot(data, aes(x = wt, y = mpg)) +
                geom_point() +
                theme_minimal()
            },
            cluster = function(data, k = 3) {
              kmeans(data, centers = k)
            }
          )
)
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