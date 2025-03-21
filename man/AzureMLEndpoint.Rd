% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{AzureMLEndpoint}
\alias{AzureMLEndpoint}
\title{Create an Azure ML endpoint model instance}
\usage{
AzureMLEndpoint(
  endpoint_url,
  model_type = c("chat", "completion", "embedding"),
  api_key = Sys.getenv("AZURE_ML_KEY"),
  max_tokens = 1000,
  temperature = 0.7
)
}
\arguments{
\item{endpoint_url}{The Azure ML endpoint URL}

\item{model_type}{Type of model ("chat", "completion", or "embedding")}

\item{api_key}{Azure ML API key}

\item{max_tokens}{Maximum tokens in response}

\item{temperature}{Sampling temperature}
}
\value{
An AzureMLEndpointModel instance
}
\description{
This function creates a new Azure ML endpoint model instance with the specified
configuration. It supports different types of models including chat models,
completion models, and embedding models.
}
