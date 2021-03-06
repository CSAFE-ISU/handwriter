% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JunctionDetection.R
\name{checkSimplicityBreaks}
\alias{checkSimplicityBreaks}
\title{checkSimplicityBreaks}
\usage{
checkSimplicityBreaks(
  candidateBreaks,
  pathList,
  loopList,
  letters,
  nodeGraph0,
  nodeList,
  terminalNodes,
  hasTrough,
  dims
)
}
\arguments{
\item{candidateBreaks}{possible breakpoints}

\item{pathList}{list of paths}

\item{loopList}{list of loops}

\item{letters}{list of individual letter characters}

\item{nodeGraph0}{skeletonized graph}

\item{nodeList}{list of nodes}

\item{terminalNodes}{nodes at the ends of letters}

\item{hasTrough}{wether or not break has a trough}

\item{dims}{graph dimensions}
}
\value{
removes breakpoints on simple graphs
}
\description{
Internal function for removing breakpoints that separate graphs that are too simple to be split. Remove break if graph on left and right of the break have 4 or fewer nodes and no loops or double paths. Never remove break on a trough.
}
