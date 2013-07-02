\name{g_comments_edit}
\alias{g_comments_edit}
\title{Edit a comment on an issue.}
\usage{
  g_comments_edit(owner, repo, commentid = NULL,
    comment = NULL, ...)
}
\arguments{
  \item{owner}{Name of owner of repository.}

  \item{repo}{Repository name, quoted.}

  \item{commentid}{Comment ID number.}

  \item{comment}{Comment text.}

  \item{...}{Futher arguments passed on to
  \code{link{GET}}.}
}
\value{
  Data for the comment created.
}
\description{
  Edit a comment on an issue.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_comments_edit(owner='schamberlain', repo='foobar', commentid='20353078', comment='Nice code, its way to slow, start over')
}
}
\seealso{
  \code{link{g_comments}}, \code{link{g_comments_create}},
  \code{link{g_comments_delete}}
}

