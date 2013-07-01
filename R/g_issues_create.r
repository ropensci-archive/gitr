#' Create a GitHub issue.
#' 
#' @import httr
#' @param title Required string
#' @param body Optional string
#' @param assignee Optional string - Login for the user that this issue should be assigned to. NOTE: Only users with push access can set the assignee for new issues. The assignee is silently dropped otherwise.
#' @param milestone Optional number - Milestone to associate this issue with. NOTE: Only users with push access can set the milestone for new issues. The milestone is silently dropped otherwise.
#' @param labels Optional array of strings - Labels to associate with this issue. NOTE: Only users with push access can set labels for new issues. Labels are silently dropped otherwise.
#' @param parse Whether to parse results or not. Setting parse=TRUE composes a 
#'    list of nested items of similar attributes, each with 0 to many items: 
#'    urls, info, user, pull_request, repo, and body
#' @return Vector of names or repos for organization or user.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_issues_create('schamberlain','sofa',title='testing from gitr',body='some stuff here')
#' }
#' @export
g_issues_create <- function(owner, repo, title = NULL, body = NULL, assignee = NULL, milestone = NULL,
                            labels = NULL, parse = TRUE)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url <- sprintf("https://api.github.com/repos/%s/%s/issues?access_token=%s", owner, repo, access_token)
  args <- compact(list(title=title,body=body,assignee=assignee,milestone=milestone,labels=labels))
  out <- content(POST(url, add_headers('User-Agent' = useragent), body=toJSON(args)))
  
  if(parse){
    res <- parse_issue(out)
    class(res) <- 'issues'
    res
  }
  else
  {
    class(out) <- 'issues'
    out
  }
}