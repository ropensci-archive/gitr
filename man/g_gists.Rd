\name{g_gists}
\alias{g_gists}
\title{List GitHub gists}
\usage{
  g_gists(type = "mine", user = NULL, gistid = NULL,
    since = NULL, parse = TRUE)
}
\arguments{
  \item{since}{Optional string of a timestamp in ISO 8601
  format: YYYY-MM-DDTHH:MM:SSZ}

  \item{parse}{Whether to parse results or not. Setting
  parse=TRUE composes a list of nested items of similar
  attributes, each with 0 to many items: urls, info, user,
  pull_request, repo, and body}
}
\value{
  The gist.
}
\description{
  List GitHub gists
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_gists('mine', 'schamberlain')
g_gists('public')
g_gists('starred')
g_gists('gistid', gistid=5937553)
}
}

