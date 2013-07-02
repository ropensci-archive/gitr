#' Create a comment on an issue.
#' 
#' @import httr
#' @param owner Name of owner of repository.
#' @param repo Repository name, quoted. 
#' @param issue Issue number, is specific to a repo.
#' @param comment Comment text.
#' @param ... Futher arguments passed on to \code{link{GET}}.
#' @return Data for the comment created.
#' @seealso \code{link{g_comments}}, \code{link{g_comments_edit}}, \code{link{g_comments_delete}}
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_comments_create(owner='schamberlain', repo='foobar', issue=5, comment='Nice code, its way to slow, start over')
#' }
#' @export
g_comments_create <- function(owner, repo, issue = NULL, comment = NULL, ...)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')

  if(!is.null(issue))
    issue <- as.numeric(issue)
  
  url <- sprintf("https://api.github.com/repos/%s/%s/issues/%s/comments?access_token=%s", owner, repo, issue, access_token)
  args <- compact(list(body=comment))
  res <- content(POST(url, add_headers('User-Agent' = useragent), body=toJSON(args), ...))
  
  out <- parse_issue_comments(res)
  class(out) <- 'comments'
  out
}