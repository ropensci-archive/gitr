\name{g_comments}
\alias{g_comments}
\title{Get issue comments.}
\usage{
  g_comments(owner, repo, issue = NULL, commentid = NULL,
    sort = NULL, direction = NULL, since = NULL, ...)
}
\arguments{
  \item{owner}{Name of owner of repository.}

  \item{repo}{Repository name, quoted.}

  \item{issue}{Issue number, is specific to a repo.}

  \item{commentid}{Comment ID number.}

  \item{sort}{created, updated, comments, default:
  created.}

  \item{direction}{asc or desc, default: desc.}

  \item{since}{Optional string of a timestamp in ISO 8601
  format: YYYY-MM-DDTHH:MM:SSZ}

  \item{...}{Futher arguments passed on to
  \code{link{GET}}.}
}
\value{
  Issue comments data for a single issue, all comments in a
  repo, or a single comment.
}
\description{
  This function retrieves comments on issues for one of
  three scenarios. 1. All comments for a repository -
  specify repo owner, and repo 2. All comments for an issue
  within a repository - specify repo owner, repo, and issue
  number 3. A single comment - specify repo owner, repo,
  and comment ID

  See examples below for these three scenarios.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')

# List all comments in a repo
g_comments(owner='ropensci', repo='reml')

# List comments on a specific issue
g_comments(owner='ropensci', repo='reml', issue=5)

# Get a single comment, default action is 'get', so no need to specify it
g_comments(owner='ropensci', repo='reml', commentid=12345)
}
}
\seealso{
  There are three other functions for creating
  \code{link{g_comments_create}}, editing
  \code{link{g_comments_edit}}, and deleting
  \code{link{g_comments_delete}} comments.
}

