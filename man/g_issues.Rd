\name{g_issues}
\alias{g_issues}
\title{List GitHub issues globally, for XXXX.}
\usage{
  g_issues(type = "global", org = NULL, filter = "all",
    state = NULL, labels = NULL, sort = NULL,
    direction = NULL, since = NULL, parse = TRUE)
}
\arguments{
  \item{type}{One of global, user, or org. If org, specify
  a GittHub organization name in the org parameter. See
  description below for more.}

  \item{org}{Github organization name.}

  \item{filter}{See options below.}

  \item{state}{open, closed, default: open}

  \item{labels}{String list of comma separated Label names.
  Example: bug,ui,}

  \item{sort}{created, updated, comments, default:
  created.}

  \item{direction}{asc or desc, default: desc.}

  \item{since}{Optional string of a timestamp in ISO 8601
  format: YYYY-MM-DDTHH:MM:SSZ}

  \item{parse}{Whether to parse results or not. Setting
  parse=TRUE composes a list of nested items of similar
  attributes, each with 0 to many items: urls, info, user,
  pull_request, repo, and body}
}
\value{
  Vector of names or repos for organization or user.
}
\description{
  Options for type parameter: \itemize{ \item{global} List
  all issues across all the authenticated user’s visible
  repositories including owned repositories, member
  repositories, and organization repositories \item{user}
  List all issues across owned and member repositories for
  the authenticated user \item{org} List all issues for a
  given organization for the authenticated user }

  Options for filter parameter: \itemize{ \item{assigned}
  Issues assigned to you (default) \item{created} Issues
  created by you \item{mentioned} Issues mentioning you
  \item{subscribed} Issues you’re subscribed to updates for
  \item{all} All issues the authenticated user can see,
  regardless of participation or creation state }
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_issues()
g_issues('user')
g_issues('user', 'mentioned')
g_issues('org', 'ropensci')
}
}

