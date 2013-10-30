\name{g_gists_create}
\alias{g_gists_create}
\title{Create a GitHub gist}
\usage{
  g_gists_create(description = NULL, public = "true",
    filename = NULL, content = NULL)
}
\arguments{
  \item{description}{Optional string}

  \item{public}{Required boolean}

  \item{filename}{Path to a file with the code you want to
  upload.}

  \item{content}{The content of the file (only use if
  filename=NULL).}
}
\value{
  The gist metadata.
}
\description{
  Create a GitHub gist
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_gists_create(description='my cool description', filename="fromr.md", content="testing creating a gist from R")
g_gists_create(description='my cool description', filename="~/github/ropensci/stuff.md")
}
}

