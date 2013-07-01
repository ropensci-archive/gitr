#' Get a GitHub issue.
#' 
#' @import httr
#' @param repo Repository name, quoted. 
#' @param issue Issue number, is specific to a repo.
#' @param ... Futher curl arguments passed on.
#' @return Data for a single issue, or many if you pass in a vector of issue numbers
#'    for a single repo.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' 
#' # A single issue
#' g_issues_get(owner='ropensci', repo='reml', issues=5)
#' 
#' # Many issues
#' g_issues_get(owner='ropensci', repo='reml', issues=5:7)
#' 
#' # Get comments on a single issue
#' g_issues_get(owner='ropensci', repo='reml', issues=5, comments=TRUE)
#' 
#' # Get comments on multiple issues
#' g_issues_get(owner='ropensci', repo='reml', issues=5:7, comments=TRUE)
#' }
#' @export
g_issues_get <- function(owner, repo, issues, ..., comments = FALSE)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
    
  docalls <- function(x){
    if(comments)
      url <- sprintf("https://api.github.com/repos/%s/%s/issues/%s/comments", owner, repo, x)
    else
      url <- sprintf("https://api.github.com/repos/%s/%s/issues/%s", owner, repo, x)
    args <- compact(list(access_token=access_token))
    content(GET(url, add_headers('User-Agent' = useragent), query=args, ...))
  }
  
  if(comments){
    res <- llply(issues, docalls)
    if(length(res)==1)
      out <- parse_issue_comments(res)
    else
      out <- llply(res, function(x) parse_issue_comments(x[[1]]))
    names(out) <- issues
  } else
  {
    res <- llply(issues, docalls)
    out <- llply(res, parse_issue)
    names(out) <- issues
  }
  class(out) <- 'issues'
  out
}