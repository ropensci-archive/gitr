\name{g_commit_get}
\alias{g_commit_get}
\title{Get commit details with specific sha.}
\usage{
  g_commit_get(userorg = NULL, repo = NULL, sha = NULL)
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{repo}{Repository name.}

  \item{sha}{Githu sha alphanumeric string.}
}
\value{
  stuff.
}
\description{
  Get commit details with specific sha.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_commit_get(userorg = 'ropensci', repo='rgbif', sha='7549351599f3d7de97a1800a439071f7c5c57c98')
}
}

