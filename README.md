# *AIGenFlow*: A Cutting-Edge R Framework for Creating, Managing, and Scaling LLM Agents, Teams, and Applications





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
  analysistool = analyze_data,
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

## Pipeline Operations

The package supports intuitive pipeline operations using the `%>%` operator, allowing for seamless chaining of models, agents, and data analysis.

### Key Functions

- `ask()`: Direct questions to an agent
- `analyze()`: Analyze data with an agent
- `process_with()`: Process previous results with another agent
- `get_response()`: Extract final response
- `get_history()`: Get full conversation history

### Basic Model and Agent Pipeline

Create models and agents using a simple pipeline:

```r
# Create an OpenAI GPT-4 agent
gpt4_agent <- OpenAIModel("gpt-4") %>%
  Agent(memory = AgentConversationStore(100))

# Create a Claude agent
claude_agent <- AnthropicModel("claude-3-opus-20240229") %>%
  Agent()
```

### Direct Questions

Ask questions and get responses:

```r
# Single question
response <- gpt4_agent %>%
  ask("What is machine learning?") %>%
  get_response()

# Chain of questions
responses <- gpt4_agent %>%
  ask("What is deep learning?") %>%
  process_with(claude_agent, "Expand on that explanation:") %>%
  process_with(gpt4_agent, "Summarize the key points:")

# Access full response chain
print(responses)  # Shows all intermediate steps
final_answer <- get_response(responses)  # Gets just the final response
history <- get_history(responses)  # Gets the full conversation history
```

### Create a new column using an agent by asking a question

Function is ideal for text summarization and parsing data. 
Here is an example of how to use the ask() function to create a new column in a dataset by asking a question to an agent.

```r
new_col <- mtcars %>%
  # rowwise() %>%  # Add this line
  mutate(analysis = ask(analyst, paste0("Report the largest value between ", cyl, 
                          ", ", mpg, "and, ", hp, 
                          ". Only return a single value without accompaniing words just the number.")))
```


### Data Analysis Pipeline

Analyze data using multiple agents:

```r
# Basic data analysis
analysis <- mtcars %>%
  analyze(gpt4_agent, "Analyze the relationships between MPG and other variables:")

# Multi-step analysis with different agents
result <- mtcars %>%
  analyze(gpt4_agent, "Initial statistical analysis:") %>%
  process_with(claude_agent, "Expand on these findings:") %>%
  process_with(gpt4_agent, "Summarize the key insights:")

# Access results
print(result)  # Shows full analysis chain
final_insights <- get_response(result)  # Gets final summary
```


### Working with Groups

Analyze grouped data:

```r
iris %>%
  group_by(Species) %>%
  summarise(across(everything(), mean)) %>%
  analyze(gpt4_agent, "Compare the characteristics across species:") %>%
  process_with(claude_agent, "What evolutionary insights can we draw?") %>%
  get_response()
```


### Verbose Output

All operations support verbose output to track the conversation. 
This will show each step of the process, including:
- The agent being used
- The prompt/input
- The response from each step

```r
mtcars %>%
  analyze(gpt4_agent, "Analyze this dataset:", verbose = TRUE) %>%
  process_with(claude_agent, "What additional patterns do you see?", verbose = TRUE) %>%
  get_response()
```


### Response Chain Access

The response chain maintains a complete history of the conversation. Each step in the history includes:
- The agent used
- The prompt given
- The response received

```r
result <- mtcars %>%
  analyze(gpt4_agent, "Initial analysis:") %>%
  process_with(claude_agent, "Review findings:") %>%
  process_with(gpt4_agent, "Final summary:")

# Access different components
final_response <- get_response(result)  # Final summary
history <- get_history(result)  # Full analysis chain
print(result)  # Complete output with all steps
```

This pipeline system allows for intuitive data analysis workflows while maintaining a complete record of the analysis process.



## Multi-Agent Workflow Example

This example demonstrates how to create a data analysis workflow using multiple specialized agents.

```r
# Create agents with specialized tools
analyst <- Agent(
  model = OpenAIModel("gpt-4"),
  tools = list(
    analyze = function(data) {
      list(
        stats = summary(data),
        correlations = cor(data[sapply(data, is.numeric)])
      )
    }
  )
)

viz_expert <- Agent(
  model = OpenAIModel("gpt-4"),
  tools = list(
    plot = function(data) {
      ggplot(data) + 
        geom_point(aes(x = mpg, y = wt)) +
        theme_minimal()
    }
  )
)

writer <- Agent(
  model = AnthropicModel("claude-3-opus-20240229")
)

# Create workflow
flow <- CreateFlow(
  agents = list( analyst = analyst, viz = viz_expert, writer = writer),
  workflows = list(data_report = Workflow(
      Step(name = "analyze",agent = "analyst", prompt = "Analyze this dataset focusing on key patterns: {{input}}", 
      pass_to_next = TRUE),
      Step(name = "visualize", agent = "viz", prompt = "Create visualizations for the key findings: {{input}}",
        pass_to_next = TRUE),
      Step(name = "report",agent = "writer", 
      prompt = paste("Create a report combining:", "Analysis: {{results.analyze}}", "Visuals: {{results.visualize}}"
)))))

# Run workflow
results <- RunFlow(flow, workflow = "data_report", input = mtcars,
  context = list(objective = "Understand factors affecting fuel efficiency")
)

# Access results
report <- results$report
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
