#' Get issue comments.
#' 
#' @import httr
#' @param owner Name of owner of repository.
#' @param repo Repository name, quoted. 
#' @param issue Issue number, is specific to a repo.
#' @param commentid Comment ID number.
#' @param sort created, updated, comments, default: created. 
#' @param direction asc or desc, default: desc.
#' @param since Optional string of a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
#' @param ... Futher arguments passed on to \code{link{GET}}.
#' @return Issue comments data for a single issue, all comments in a repo, or a single comment.
#' @description This function retrieves comments on issues for one of three scenarios. 
#' 1. All comments for a repository - specify repo owner, and repo
#' 2. All comments for an issue within a repository - specify repo owner, repo, and issue number
#' 3. A single comment - specify repo owner, repo, and comment ID
#' 
#' See examples below for these three scenarios.
#' @seealso There are three other functions for creating \code{link{g_comments_create}}, 
#'    editing \code{link{g_comments_edit}}, and deleting \code{link{g_comments_delete}} comments.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' 
#' # List all comments in a repo
#' g_comments(owner='ropensci', repo='reml')
#' 
#' # List comments on a specific issue
#' g_comments(owner='ropensci', repo='reml', issue=5)
#' 
#' # Get a single comment, default action is 'get', so no need to specify it
#' g_comments(owner='ropensci', repo='reml', commentid=12345)
#' }
#' @export
g_comments <- function(owner, repo, issue = NULL, commentid = NULL, 
                      sort = NULL, direction = NULL, since = NULL, ...)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')

  if(!is.null(issue))
    issue <- as.numeric(issue)
  
  if(is.null(issue) & is.null(commentid)){
    url <- sprintf("https://api.github.com/repos/%s/%s/issues/comments", owner, repo)
    args <- compact(list(access_token=access_token,sort=sort,direction=direction,since=since))
  } else
  if(is.numeric(issue) & is.null(commentid)){
    url <- sprintf("https://api.github.com/repos/%s/%s/issues/%s/comments", owner, repo, issue)
    args <- compact(list(access_token=access_token))
  } else
  {
    url <- sprintf("https://api.github.com/repos/%s/%s/issues/comments/%s", owner, repo, commentid)
    args <- compact(list(access_token=access_token))
  }
  res <- content(GET(url, add_headers('User-Agent' = useragent), query=args, ...))
  
  if(length(res)==1)
    out <- parse_issue_comments(res)
  else
    out <- llply(res, parse_issue_comments)
  class(out) <- 'comments'
  out
}