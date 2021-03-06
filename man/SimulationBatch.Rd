% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-batch.R
\docType{class}
\name{SimulationBatch}
\alias{SimulationBatch}
\title{SimulationBatch}
\description{
An optimized simulation with faster loading. The corresponding .NET class is
"OSPSuite.R.Services.ConcurrentRunSimulationBatch"
}
\examples{

## ------------------------------------------------
## Method `SimulationBatch$addRunValues`
## ------------------------------------------------

\dontrun{
sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
parameters <- c("Organism|Liver|Volume", "R1|k1")
molecules <- "Organism|Liver|A"
# Create two simulation batches.
simulationBatch1 <- createSimulationBatch(simulation = sim1,
parametersOrPaths = parameters,
moleculesOrPaths = molecules)
simulationBatch2 <- createSimulationBatch(simulation = sim2,
parametersOrPaths = parameters,
moleculesOrPaths = molecules)
#Ids of run values
ids <- c()
ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
}
}
\section{Super classes}{
\code{\link[ospsuite:Printable]{ospsuite::Printable}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{SimulationBatch}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{simulation}}{Underlying simulation used for the batch run. Read only.}

\item{\code{runValuesIds}}{Ids of the run values that will be executed on next run}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{SimulationBatch$new()}}
\item \href{#method-addRunValues}{\code{SimulationBatch$addRunValues()}}
\item \href{#method-finalize}{\code{SimulationBatch$finalize()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationBatch$new(ref, simulation)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ref}}{.NET reference object.}

\item{\code{simulation}}{Simulation used in the batch run}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{SimulationBatch} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addRunValues"></a>}}
\if{latex}{\out{\hypertarget{method-addRunValues}{}}}
\subsection{Method \code{addRunValues()}}{
Add a set of parameter and start values for next execution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationBatch$addRunValues(parameterValues = NULL, initialValues = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parameterValues}}{Vector of parameter values to set in the simulation (default is \code{NULL})}

\item{\code{initialValues}}{Vector of initial values to set in the simulation  (default is \code{NULL})}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Intended for the use with \code{runSimulationBatches}. The simulation batch is executed
with the sets of parameter and initial values that have been scheduled. The set of run values is cleared after successful run.
}

\subsection{Returns}{
Id of the values set that can be used to get the correct result from \code{runSimulationBatches}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
parameters <- c("Organism|Liver|Volume", "R1|k1")
molecules <- "Organism|Liver|A"
# Create two simulation batches.
simulationBatch1 <- createSimulationBatch(simulation = sim1,
parametersOrPaths = parameters,
moleculesOrPaths = molecules)
simulationBatch2 <- createSimulationBatch(simulation = sim2,
parametersOrPaths = parameters,
moleculesOrPaths = molecules)
#Ids of run values
ids <- c()
ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-finalize}{}}}
\subsection{Method \code{finalize()}}{
Clears the reference to the wrapped .NET object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationBatch$finalize()}\if{html}{\out{</div>}}
}

}
}
