\name{g_commit_create}
\alias{g_commit_create}
\title{Get GitHub downloads for a repo}
\usage{
  g_commit_create(userorg = NULL, repo = NULL,
    message = NULL, tree = NULL, parents = NULL)
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{repo}{Repository name.}

  \item{message}{String of the commit message.}

  \item{tree}{String of the SHA of the tree object this
  commit points to.}

  \item{parents}{Array of the SHAs of the commits that were
  the parents of this commit.  If omitted or empty, the
  commit will be written as a root commit. For a single
  parent, an array of one SHA should be provided, for a
  merge commit, an array of more than one should be
  provided.}
}
\value{
  stuff.
}
\description{
  Get GitHub downloads for a repo
}
\examples{
\dontrun{
github_auth()
options(useragent='ropensci')
g_commit(userorg = 'ropensci', repo='rgbif')
}
}

