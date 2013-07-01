#' Parse a github issue
#' @export
#' @keywords internal
parse_issue <- function(x){
  urls <- x[c('url','labels_url','comments_url','events_url','html_url')]
  info <- x[c('id','number','title','state','assignee','milestone','comments','created_at','updated_at','closed_at')]
#   user <- data.frame(x$user)
  user <- x$user
  pull_request <- data.frame(x$pull_request)
  repo_owner <- x$repository[['owner']]
  repo <- x$repository[!names(x$repository) %in% 'owner']
  body <- x$body
  list(urls = urls, info = info, user = user, pull_request = pull_request, repo = repo, body = body)
}

#' Parse a github issue comments
#' @export
#' @keywords internal
parse_issue_comments <- function(x){
  urls <- x[c('url','html_url','issue_url')]
  info <- x[c('id','created_at','updated_at')]
#   user <- data.frame(x$user)
  user <- x$user
  body <- x$body
  list(urls = urls, info = info, user = user, body = body)
}