\name{g_comments_create}
\alias{g_comments_create}
\title{Create a comment on an issue.}
\usage{
  g_comments_create(owner, repo, issue = NULL,
    comment = NULL, ...)
}
\arguments{
  \item{owner}{Name of owner of repository.}

  \item{repo}{Repository name, quoted.}

  \item{issue}{Issue number, is specific to a repo.}

  \item{comment}{Comment text.}

  \item{...}{Futher arguments passed on to
  \code{link{GET}}.}
}
\value{
  Data for the comment created.
}
\description{
  Create a comment on an issue.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_comments_create(owner='schamberlain', repo='foobar', issue=5, comment='Nice code, its way to slow, start over')
}
}
\seealso{
  \code{link{g_comments}}, \code{link{g_comments_edit}},
  \code{link{g_comments_delete}}
}

