% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_tools}
\alias{add_tools}
\title{Add tools to an existing Agent instance}
\usage{
add_tools(agent, tools, tools_env = parent.frame())
}
\arguments{
\item{agent}{An Agent instance}

\item{tools}{Named list of functions or character vector of function names}

\item{tools_env}{Environment to look for tool functions}
}
\value{
The modified Agent instance (invisibly)
}
\description{
Add tools to an existing Agent instance
}
