\name{g_allrepos}
\alias{g_allrepos}
\title{List GitHub repositories for a user or organization.}
\usage{
  g_allrepos(userorg = NA, type = "org", return_ = "names",
    per_page = 100)
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{type}{One of user or org (defaults to org)}

  \item{repo}{Repository name.}

  \item{per_page}{(optional) Number of results to return}
}
\value{
  Vector of names or repos for organization or user.
}
\description{
  List GitHub repositories for a user or organization.
}
\examples{
\dontrun{
github_auth()
options(useragent='ropensci')
g_allrepos(userorg = 'ropensci')
}
}

