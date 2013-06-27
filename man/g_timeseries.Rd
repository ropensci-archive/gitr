\name{g_timeseries}
\alias{g_timeseries}
\title{Create time series bar plot.}
\usage{
  g_timeseries(data = NULL)
}
\arguments{
  \item{data}{Data set to plot with.}
}
\description{
  Create time series bar plot.
}
\examples{
\dontrun{
# Run with example data set (commits from the ropensci organization account)
g_timeseries()

# Get your own data
g_auth()
mydat <- llply(c('ggplot2','plyr','httr'), function(x) g_commits(userorg='hadley',repo=x,limit=500))
mydat <- ldply(mydat)
g_timeseries(mydat)
}
}

