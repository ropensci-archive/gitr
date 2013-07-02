#' Delete a comment on an issue.
#' 
#' @import httr
#' @param owner Name of owner of repository.
#' @param repo Repository name, quoted. 
#' @param commentid Comment ID number.
#' @param comment Comment text.
#' @param ... Futher arguments passed on to \code{link{GET}}.
#' @return Data for the comment created.
#' @seealso \code{link{g_comments}}, \code{link{g_comments_create}}, \code{link{g_comments_edit}}
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_comments_delete(owner='schamberlain', repo='foobar', commentid='20351662')
#' }
#' @export
g_comments_delete <- function(owner, repo, commentid = NULL, ...)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')

  url <- sprintf("https://api.github.com/repos/%s/%s/issues/comments/%s?access_token=%s", owner, repo, commentid, access_token)
  res <- DELETE(url, add_headers('User-Agent' = useragent), ...)
  
  if(grepl("204", http_status(res)$message))
    "success! comment deleted"
  else
    http_status(res)$message
}