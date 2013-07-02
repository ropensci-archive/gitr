\name{g_issue_repo}
\alias{g_issue_repo}
\title{Get issues for a GitHub repository.}
\usage{
  g_issue_repo(owner, repo, milestone = NULL,
    assignee = NULL, creator = NULL, mentioned = NULL,
    labels = NULL, sort = NULL, direction = NULL,
    since = NULL, ...)
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
  Get issues for a GitHub repository.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_issue_repo(owner='ropensci', repo='reml')
}
}

