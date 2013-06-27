\name{g_repo}
\alias{g_repo}
\title{Get GitHub metrics on a user or organization's repositories.}
\usage{
  g_repo(userorg = NA, repo = NA, return_ = "show")
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{repo}{Repository name.}

  \item{return_}{what to return, one of: show (raw data),
  allstats, watchers, open_issues, or forks}
}
\value{
  json
}
\description{
  Get GitHub metrics on a user or organization's
  repositories.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_repo(userorg = 'ropensci', repo = 'rmendeley')
g_repo(userorg = 'hadley', repo = 'ggplot2')
g_repo(userorg = 'hadley', repo = 'ggplot2', 'allstats')
g_repo(userorg = 'hadley', repo = 'ggplot2', return_ = 'forks')
}
}

