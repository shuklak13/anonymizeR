% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/randomizeCategories.R
\name{randomizeCategories}
\alias{randomizeCategories}
\title{Randomize Assignment of Categories to Values in a Column}
\usage{
randomizeCategories(x, cols = 1:ncol(x), magnitude = "MEDIUM",
  probability = NULL)
}
\arguments{
\item{x}{a data.frame}

\item{cols}{the columns that are to be affected.
If no value is given, all columns are affected.}

\item{magnitude}{a shortcut for the probability parameter - see details}

\item{probability}{- the probability that an element will be randomized}
}
\value{
data.frame with some of the elements in specified columns randomized
}
\description{
Randomize Assignment of Categories to Values in a Column
}
\details{
By default, each observation has a 50% chance of having its value replaced by
the value of another observation in the data. The replaced value is determined randomly
following the distribution of the data, so the distribution of categories after randomization
should be similar to the distribution of categories before randomization.
The chance of replacement can be altered with the probability parameter.
magnitude is a shortcut for the probability parameter:
\itemize{
     \item LOW:      proability  = .25
     \item MEDIUM:   probability = .5
     \item HIGH:     probability = .75
}
}
\examples{
randomizeCategories(data)
randomizeCategories(data, magnitude="LOW")
randomizeCategories(data, 1:5, probability = .7)
}

