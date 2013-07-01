\name{g_issues_create}
\alias{g_issues_create}
\title{Create a GitHub issue.}
\usage{
  g_issues_create(owner, repo, title = NULL, body = NULL,
    assignee = NULL, milestone = NULL, labels = NULL,
    parse = TRUE)
}
\arguments{
  \item{title}{Required string}

  \item{body}{Optional string}

  \item{assignee}{Optional string - Login for the user that
  this issue should be assigned to. NOTE: Only users with
  push access can set the assignee for new issues. The
  assignee is silently dropped otherwise.}

  \item{milestone}{Optional number - Milestone to associate
  this issue with. NOTE: Only users with push access can
  set the milestone for new issues. The milestone is
  silently dropped otherwise.}

  \item{labels}{Optional array of strings - Labels to
  associate with this issue. NOTE: Only users with push
  access can set labels for new issues. Labels are silently
  dropped otherwise.}

  \item{parse}{Whether to parse results or not. Setting
  parse=TRUE composes a list of nested items of similar
  attributes, each with 0 to many items: urls, info, user,
  pull_request, repo, and body}
}
\value{
  Vector of names or repos for organization or user.
}
\description{
  Create a GitHub issue.
}
\examples{
\dontrun{
g_auth()
options(useragent='ropensci')
g_issues_create('schamberlain','sofa',title='testing from gitr',body='some stuff here')
}
}

