\name{g_comments_delete}
\alias{g_comments_delete}
\title{Delete a comment on an issue.}
\usage{
  g_comments_delete(owner, repo, commentid = NULL, ...)
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
  Delete a comment on an issue.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_comments_delete(owner='schamberlain', repo='foobar', commentid='20351662')
}
}
\seealso{
  \code{link{g_comments}}, \code{link{g_comments_create}},
  \code{link{g_comments_edit}}
}

