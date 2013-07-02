#' Get issues for a GitHub repository.
#' 
#' @import httr
#' @param repo Repository name, quoted. 
#' @param issue Issue number, is specific to a repo.
#' @return Data for a single issue, or many if you pass in a vector of issue numbers
#'    for a single repo.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_issue_repo(owner='ropensci', repo='reml')
#' }
#' @export
g_issue_repo <- function(owner, repo, milestone = NULL, assignee = NULL, creator = NULL,
                         mentioned = NULL, labels = NULL, sort = NULL, direction = NULL, 
                         since = NULL, ...)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url <- sprintf("https://api.github.com/repos/%s/%s/issues", owner, repo)
  args <- compact(list(access_token=access_token,milestone=milestone,assignee=assignee,
                       creator=creator,mentioned=mentioned,labels=labels,sort=sort,
                       direction=direction,since=since))
  res <- content(GET(url, add_headers('User-Agent' = useragent), query=args, ...))
  
  out <- llply(res, parse_issue)
  class(out) <- 'issues'
  out
}