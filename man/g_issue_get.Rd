\name{g_issue_get}
\alias{g_issue_get}
\title{Get a GitHub issue.}
\usage{
  g_issue_get(owner, repo, issue, parse = TRUE)
}
\arguments{
  \item{repo}{Repository name, quoted.}

  \item{issue}{Issue number, is specific to a repo.}
}
\value{
  Data for a single issue, or many if you pass in a vector
  of issue numbers for a single repo.
}
\description{
  Get a GitHub issue.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_issue_get(owner='ropensci', repo='reml', issue=5)
}
}

