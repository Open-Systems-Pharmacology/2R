% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-molecule.R
\name{setMoleculeInitialValues}
\alias{setMoleculeInitialValues}
\title{Set molecule start values}
\usage{
setMoleculeInitialValues(molecules, values)
}
\arguments{
\item{molecules}{A single or a list of \code{Molecule}}

\item{values}{A numeric value that should be assigned to the molecule start value or a vector
of numeric values, if the start value of more than one molecule should be changed. Must have the same
length as 'molecules'}
}
\description{
Set molecule start values
}
\examples{

simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
molecule <- getMolecule("Organism|Liver|A", sim)
setMoleculeInitialValues(molecule, 1)
molecules <- getAllMoleculesMatching("Organism|**|A", sim)
setMoleculeInitialValues(molecules, c(2, 3))
}
\seealso{
\code{\link{getMolecule}} and \code{\link{getAllMoleculesMatching}} to retrieve objects of type Molecule
}
