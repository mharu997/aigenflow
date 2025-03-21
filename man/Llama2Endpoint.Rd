% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{Llama2Endpoint}
\alias{Llama2Endpoint}
\title{Create a Llama 2 endpoint instance}
\usage{
Llama2Endpoint(
  endpoint_url,
  api_key = Sys.getenv("AZURE_ML_KEY"),
  max_tokens = 1000,
  temperature = 0.7
)
}
\arguments{
\item{endpoint_url}{URL of the Llama 2 endpoint}

\item{api_key}{API key for authentication}

\item{max_tokens}{Maximum tokens for response}

\item{temperature}{Temperature for response randomness (0-1)}
}
\value{
An AzureMLEndpointModel instance
}
\description{
Create a Llama 2 endpoint instance
}
\examples{
\dontrun{
# Create a Llama 2 model endpoint
model <- Llama2Endpoint(
 endpoint_url = "https://your-endpoint.azureml.ms/endpoints/your-model/invoke"
 )
 # Generate a response
 response <- model$generate(
 prompt = "What is machine learning?",
 system_message = "You are a helpful AI assistant."
 )
 }
 @export
 Create a Llama 2 endpoint instance
}
