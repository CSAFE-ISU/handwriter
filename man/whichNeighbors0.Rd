% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JunctionDetection.R
\name{whichNeighbors0}
\alias{whichNeighbors0}
\title{whichNeighbors0}
\usage{
whichNeighbors0(coords, img)
}
\arguments{
\item{coords}{coordinates to consider}

\item{img}{The image as a bitmap

Internal function for identifying which neighbors are black excluding diagonals
to the middle point when a non-diagonal between those two vertices exists.}
}
\description{
whichNeighbors0
}
