% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JunctionDetection.R
\name{checkBreakPoints}
\alias{checkBreakPoints}
\title{checkBreakPoints}
\usage{
checkBreakPoints(candidateNodes, allPaths, nodeGraph, terminalNodes, dims)
}
\arguments{
\item{candidateNodes}{possible breakpoints}

\item{allPaths}{list of paths}

\item{nodeGraph}{graph of nodes; call the getNodeGraph function}

\item{terminalNodes}{nodes at the endpoints of the graph}

\item{dims}{graph dimensions}
}
\value{
a graph without breakpoints and separated letters
}
\description{
Internal function called by processHandwriting that eliminates breakpoints based on rules to try to coherently separate letters.
}
