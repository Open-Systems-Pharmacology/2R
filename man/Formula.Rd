% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula.R, R/table-formula.R
\docType{class}
\name{Formula}
\alias{Formula}
\alias{TableFormula}
\title{Formula}
\description{
A formula of the model (Typically related to a \code{Quantity} such as a parameter)

A table formula of the model (Typically related to a \code{Quantity} such as a parameter)
}
\section{Super classes}{
\code{\link[ospsuite:Printable]{ospsuite::Printable}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{\link[ospsuite:ObjectBase]{ospsuite::ObjectBase}} -> \code{Formula}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{isTable}}{Is this a table formula (Read-Only)}

\item{\code{isTableWithOffSet}}{Is this a table formula with Offset (Read-Only)}

\item{\code{isTableWithXArgument}}{Is this a table formula with xArgs (typically time, or pH) (Read-Only)}

\item{\code{isConstant}}{Is this a constant formula (Read-Only)}

\item{\code{isExplicit}}{Is this an explicit formula (Read-Only)}

\item{\code{isDistributed}}{Is this a distributed formula (Read-Only)}

\item{\code{dimension}}{The dimension in which the quantity is defined  (Read-Only)}

\item{\code{formulaString}}{Returns the formula as a string for an \code{ExplicitFormula} or \code{NULL} otherwise (Read-Only).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-print}{\code{Formula$print()}}
\item \href{#method-printFormula}{\code{Formula$printFormula()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="ospsuite" data-topic="DotNetWrapper" data-id="finalize">}\href{../../ospsuite/html/DotNetWrapper.html#method-finalize}{\code{ospsuite::DotNetWrapper$finalize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ospsuite" data-topic="DotNetWrapper" data-id="initialize">}\href{../../ospsuite/html/DotNetWrapper.html#method-initialize}{\code{ospsuite::DotNetWrapper$initialize()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Formula$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Rest arguments.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-printFormula"></a>}}
\if{latex}{\out{\hypertarget{method-printFormula}{}}}
\subsection{Method \code{printFormula()}}{
Print the formula to the console without the name of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Formula$printFormula()}\if{html}{\out{</div>}}
}

}
}
\section{Super classes}{
\code{\link[ospsuite:Printable]{ospsuite::Printable}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{\link[ospsuite:ObjectBase]{ospsuite::ObjectBase}} -> \code{\link[ospsuite:Formula]{ospsuite::Formula}} -> \code{TableFormula}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{allPoints}}{Returns all points defined in the table formulafor a \code{TableFormula} or \code{NULL}  otherwise (Read-Only).}

\item{\code{useDerivedValues}}{Indicates whether table values should be derived during solving. the ODE system. Default value is \code{TRUE}}

\item{\code{xDimension}}{The dimension in which the x values are defined (Read-Only).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-addPoints}{\code{TableFormula$addPoints()}}
\item \href{#method-removePoint}{\code{TableFormula$removePoint()}}
\item \href{#method-clearPoints}{\code{TableFormula$clearPoints()}}
\item \href{#method-setPoints}{\code{TableFormula$setPoints()}}
\item \href{#method-print}{\code{TableFormula$print()}}
\item \href{#method-valueAt}{\code{TableFormula$valueAt()}}
\item \href{#method-printFormula}{\code{TableFormula$printFormula()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="ospsuite" data-topic="DotNetWrapper" data-id="finalize">}\href{../../ospsuite/html/DotNetWrapper.html#method-finalize}{\code{ospsuite::DotNetWrapper$finalize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ospsuite" data-topic="DotNetWrapper" data-id="initialize">}\href{../../ospsuite/html/DotNetWrapper.html#method-initialize}{\code{ospsuite::DotNetWrapper$initialize()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addPoints"></a>}}
\if{latex}{\out{\hypertarget{method-addPoints}{}}}
\subsection{Method \code{addPoints()}}{
Adds one or more points to a table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$addPoints(xValues, yValues)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xValues}}{x values (single value or array) in base unit for XDimension}

\item{\code{yValues}}{y values (single value or array) in base unit for Dimension}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-removePoint"></a>}}
\if{latex}{\out{\hypertarget{method-removePoint}{}}}
\subsection{Method \code{removePoint()}}{
Remove the point having the same x and y from the table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$removePoint(xValue, yValue)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xValue}}{xValue value in base unit for XDimension}

\item{\code{yValue}}{yValue value in base unit for Dimension}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clearPoints"></a>}}
\if{latex}{\out{\hypertarget{method-clearPoints}{}}}
\subsection{Method \code{clearPoints()}}{
Remove all points from the table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$clearPoints()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-setPoints"></a>}}
\if{latex}{\out{\hypertarget{method-setPoints}{}}}
\subsection{Method \code{setPoints()}}{
Replace all points defined in the table with the new values given.
This is a convenience method for callign \code{clearPoints} and \code{addPoints}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$setPoints(xValues, yValues)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xValues}}{x values (single value or array) in base unit for XDimension}

\item{\code{yValues}}{y values (single value or array) in base unit for Dimension}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Rest arguments.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-valueAt"></a>}}
\if{latex}{\out{\hypertarget{method-valueAt}{}}}
\subsection{Method \code{valueAt()}}{
Returns the y defined for the x value in base unit. If not exact match is found, value will be interpolated between two existing points
If the table contains no point, 0 is returned
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$valueAt(xValue)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xValue}}{x value for in base unit for which the yValue should be returned}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-printFormula"></a>}}
\if{latex}{\out{\hypertarget{method-printFormula}{}}}
\subsection{Method \code{printFormula()}}{
Print the formula to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableFormula$printFormula()}\if{html}{\out{</div>}}
}

}
}
