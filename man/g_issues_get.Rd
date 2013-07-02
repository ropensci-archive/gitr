\name{g_issues_get}
\alias{g_issues_get}
\title{Get a GitHub issue.}
\usage{
  g_issues_get(owner, repo, issues, ..., comments = FALSE)
}
\arguments{
  \item{repo}{Repository name, quoted.}

  \item{issue}{Issue number, is specific to a repo.}

  \item{...}{Futher curl arguments passed on.}
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

# A single issue
g_issues_get(owner='ropensci', repo='reml', issues=5)

# Many issues
g_issues_get(owner='ropensci', repo='reml', issues=5:7)
}
}

