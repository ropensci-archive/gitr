\name{g_downloads}
\alias{g_downloads}
\title{Get GitHub downloads for a repo}
\usage{
  g_downloads(userorg = NULL, repo = NULL)
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{repo}{Repository name.}
}
\value{
  List of results.
}
\description{
  Get GitHub downloads for a repo
}
\examples{
\dontrun{
github_auth()
options(useragent='ropensci')
g_downloads(userorg = 'ropensci', repo='rgbif')
}
}

