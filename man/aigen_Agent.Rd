% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Agent.R
\name{aigen_Agent}
\alias{aigen_Agent}
\title{Agent Class}
\description{
Base class for defining LLM Agents.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-aigen_Agent-new}{\code{aigen_Agent$new()}}
\item \href{#method-aigen_Agent-get_name}{\code{aigen_Agent$get_name()}}
\item \href{#method-aigen_Agent-chat}{\code{aigen_Agent$chat()}}
\item \href{#method-aigen_Agent-add_tool}{\code{aigen_Agent$add_tool()}}
\item \href{#method-aigen_Agent-register_data_sources}{\code{aigen_Agent$register_data_sources()}}
\item \href{#method-aigen_Agent-use_tool}{\code{aigen_Agent$use_tool()}}
\item \href{#method-aigen_Agent-set_react_mode}{\code{aigen_Agent$set_react_mode()}}
\item \href{#method-aigen_Agent-set_max_react_iterations}{\code{aigen_Agent$set_max_react_iterations()}}
\item \href{#method-aigen_Agent-get_tool_info}{\code{aigen_Agent$get_tool_info()}}
\item \href{#method-aigen_Agent-get_data_sources}{\code{aigen_Agent$get_data_sources()}}
\item \href{#method-aigen_Agent-clone}{\code{aigen_Agent$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-new"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new advanced agent
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$new(
  model,
  name = "Assistant",
  short_term_memory = 20,
  tools = list(),
  log_file = NULL,
  debug_mode = FALSE,
  max_react_iterations = 5,
  enable_react = TRUE
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-get_name"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-get_name}{}}}
\subsection{Method \code{get_name()}}{
Get the agent's name
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$get_name()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-chat"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-chat}{}}}
\subsection{Method \code{chat()}}{
Main chat interface with ReAct loop support
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$chat(
  user_input,
  system_prompt = NULL,
  context_window = 5,
  use_react = NULL
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-add_tool"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-add_tool}{}}}
\subsection{Method \code{add_tool()}}{
Add a tool to the agent's toolkit with enhanced validation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$add_tool(name, fn)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-register_data_sources"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-register_data_sources}{}}}
\subsection{Method \code{register_data_sources()}}{
Register (or refresh) available data sources in .GlobalEnv
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$register_data_sources()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-use_tool"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-use_tool}{}}}
\subsection{Method \code{use_tool()}}{
Execute a tool by name with enhanced verification and parameter resolution
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$use_tool(tool_name, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-set_react_mode"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-set_react_mode}{}}}
\subsection{Method \code{set_react_mode()}}{
Enable or disable the ReAct loop for future queries
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$set_react_mode(enable = TRUE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-set_max_react_iterations"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-set_max_react_iterations}{}}}
\subsection{Method \code{set_max_react_iterations()}}{
Set the maximum number of iterations for the ReAct loop
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$set_max_react_iterations(max_iterations)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-get_tool_info"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-get_tool_info}{}}}
\subsection{Method \code{get_tool_info()}}{
Get information about available tools
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$get_tool_info()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-get_data_sources"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-get_data_sources}{}}}
\subsection{Method \code{get_data_sources()}}{
Get information about available data sources
---------------------- LOGGING ----------------------
---------------------- REACT LOOP IMPLEMENTATION ----------------------
---------------------- RESPONSE PARSING ----------------------
---------------------- TOOL USAGE DETECTION ----------------------
---------------------- TOOL PLANNING ----------------------
---------------------- PARAMETER PROCESSING ----------------------
---------------------- TOOL DISCOVERY ----------------------
---------------------- CONTEXT BUILDING ----------------------
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$get_data_sources()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-aigen_Agent-clone"></a>}}
\if{latex}{\out{\hypertarget{method-aigen_Agent-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{aigen_Agent$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
