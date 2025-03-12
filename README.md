# *AIGenFlow*: A Cutting-Edge R Framework for Creating, Managing, and Scaling LLM Agents, Teams, and Applications



## Table of Contents
- [Description](#Description)
- [Features](#features)
- [Installation & Quick Start](#installation)
- [Applications](#applications)
- [Contributing](#contributing)
- [License](#license)


## Description
*AIGenFlow* is a user-friendly, comprehensive R-based framework that simplifies integrating OpenAI’s GPT-4 and other Large Language Models (LLMs) into your R environment. Designed with ease of use in mind, it enables you to create intelligent agents and orchestrate workflows with just a few lines of code, making advanced AI capabilities accessible to developers, data scientists, and researchers across diverse fields.

### Key Features:
- **Simplicity and Versatility:** Build agents and workflows effortlessly while leveraging a highly intuitive and structured architecture. Based on best practices of high level object-oriented programming abstractions, *AIGenFlow* ensures a seamless user experience, even for complex tasks.
- **Powerful AI Integration:** Seamlessly connect with LLMs like GPT-4, Claude, and Mistral to enable natural language processing, conversational agents, and execution agents, all within R.
- **Customizable Tools:** Extend agent functionalities and workflows with highly customizable tools for invoking agentic actions, ensuring the framework adapts to your unique needs.
- **Constantly Evolving:** Though experimental, *AIGenFlow* is regularly updated to enhance its functionality, versatility, and usability, ensuring it remains a cutting-edge solution.

Whether you’re automating workflows, building conversational agents, or unlocking insights through AI-driven solutions, *AIGenFlow* empowers you to innovate and streamline your projects—all within the familiar R environment.

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
devtools::install_github("https://github.com/mharu997/aigenflow")
```
## Environment Setup
```r
Sys.setenv(OPENAI_API_KEY = "your-openai-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-key")
Sys.setenv(AZURE_OPENAI_KEY = "your-azure-key")
```
## Quick Start
```r
# Create an OpenAI model instance
model <- OpenAIModel(
  model_name = "gpt-4o",
  max_tokens = 1000,
  temperature = 0
)

# Create a basic agent with memory, logging, and debugging
agent <- Agent(
  model = model,
  name = "DataAssistant",
  short_term_memory = 20,
  tools = list(
    analyze = analyze_data,  # From utils.R
    summarize = summary
  ),
  log_file = "agent_logs.txt",
  debug_mode = TRUE,
  enable_react = TRUE,  # Enable ReAct reasoning
  max_react_iterations = 3
)

# Add additional tools after creation
add_tools(agent, 
  tools = list(
    correlate = cor
  )
)

# Chat with the agent
response <- agent$chat(
  user_input = "What are the key principles of data analysis?",
  system_prompt = "You are a data science educator. Explain concepts clearly and concisely."
)

# Print response
cat(response)



get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}

# Create an agent with tools
timer_agent <- Agent(model, 
                 short_term_memory = 5,
                 tools = list(time_tool = get_current_time,
                              analysis_tool = summary), 
                 log_file = "agent_logs.txt",
                 debug_mode = TRUE,
                 enable_react = TRUE,  # Enable ReAct reasoning
                 max_react_iterations = 3)

calculate_age <- timer_agent$chat(user_input = "How long has it been since my birthday 1997 April 24?", 
                              system_prompt = "You are a data analyst.")

cat(calculate_age)
```

## Data Analysis Example
```r
# Create model and agent with analysis tools
model <- OpenAIModel("gpt-4o")


# Create analysis agent with tools
analyst <- Agent(
  model = model,
  name = "DataAssistant",
  short_term_memory = 20,
  tools = list(summary_tool = summary),
  log_file = "agent_logs.txt",
  debug_mode = TRUE,
  enable_react = TRUE,  # Enable ReAct reasoning
  max_react_iterations = 3
)
data("mtcars")
# Analyze mtcars dataset
result <- analyst$chat(
  user_input = "Analyze the mtcars dataset. Focus on the relationship between mpg and other variables.",
  system_prompt = "You are a data analyst."
)
```

## Pipeline Operations

The package supports intuitive pipeline operations using the `%>%` operator, allowing for seamless chaining of models, agents, and data analysis.

### Key Functions:

- `ask()`: Direct questions to an agent
- `get_response()`: Extract final response
- `get_history()`: Get full conversation history as a list containing user and agent interactions as separate elements

### Create an agent and ask a chain questions with pipeline operations
```r
# Create a conversational agent using pipe syntax
results <- OpenAIModel("gpt-4") %>%
  Agent(name = "ResearchAssistant", enable_react = TRUE, short_term_memory = 10) %>% 
  ask(user_input = "What was Marie Curie's most significant contribution to science?") %>%
  ask(user_input = "Why was this work so revolutionary for that time period?") %>% 
  ask(user_input = "How did this influence modern scientific research methods?")

# Get the final response
final_answer <- results$response
get_response(results)
# Get the entire conversation history
conversation <- get_history(results)


```

### Create a new column using an agent by asking a question

Function is ideal for text summarization and parsing data.  
Here is an example of how to use the ask() function to create a new column in a dataset by asking a question to an agent.

```r
new_col <- mtcars %>%
  mutate(analysis = ask(analyst, paste0("Report the largest value between ", cyl, ", ", mpg, "and, ", hp, 
  ". Only return a single value without accompaniing words just the number.")))
```

## Multi-Agent Workflow Example
```r
data_analyst <- OpenAIModel("gpt-4") %>%
  Agent(name = "Analyst", short_term_memory = 15) %>%
  add_tools(get_analysis_tools())  # From utils.R

content_writer <- AnthropicModel("claude-3-opus-20240229") %>%
  Agent(name = "Writer", short_term_memory = 20)
```

# Define the workflow with proper template variables
```r
# Create an orchestrated workflow
flow <- CreateFlow(
  agents = list(
    "analyst" = data_analyst,
    "writer" = content_writer
  ),
  workflows = list(
    "data_report" = Workflow(
      Step(
        name = "analysis",
        agent = "analyst",
        prompt = "Analyze this data thoroughly: {{context.dataset}}",
        pass_to_next = TRUE
      ),
      Step(
        name = "report",
        agent = "writer",
        prompt = "Create a professional report based on this analysis: {{analysis}}",
        system = "You are a professional business writer. Create clear, concise reports."
      )
    )
  )
)

# Run the workflow
report <- RunFlow(
  orchestrator = flow,
  workflow = "data_report",
  input = "Please analyze our quarterly sales data",
  context = list(
    dataset = sales_data  # Your data frame
  )
)

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

### Using Local LLMs with Ollama
```r
# Check available Ollama models (IMPORTANT: Requires Ollama installed that you could download from https://ollama.com)
available_models <- ollama_models(include_details = TRUE)
print(available_models)

# Install a new Ollama model
ollama_install("llama2", quiet = FALSE)

# Run diagnostics to verify Ollama is working correctly
diagnostics <- ollama_diagnostics(verbose = TRUE)

# Create an Ollama model instance
local_model <- OllamaModel(
  model_name = "llama2",
  max_tokens = 1000,
  temperature = 0.7
)

# Create an agent using the local Ollama model
local_agent <- Agent(
  model = local_model,
  name = "LocalAssistant",
  short_term_memory = 10,
  tools = list(
    summarize = summary,
    analyze = analyze_data
  ),
  enable_react = TRUE
)

# Chat with the local agent
response <- local_agent$chat(
  user_input = "Explain the advantages of using local LLMs versus cloud-based ones.",
  system_prompt = "You are a helpful AI assistant running locally on the user's machine."
)

# Print the response
cat(response)

# Create a chain of questions with the local model
conversation <- local_model %>%
  Agent(name = "LocalChat") %>%
  ask("What are the main features of the Llama model architecture?") %>%
  ask("How does it compare to other open-source models?") %>%
  ask("What are some good use cases for this type of model?")

# Remove a model when you're done with it
ollama_remove("llama2")

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
Copyright 2025

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
