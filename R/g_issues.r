#' List GitHub issues globally, for 
#' 
#' @import httr
#' @param type One of global, user, or org. If org, specify a GittHub organization 
#'    name in the org parameter. See description below for more. 
#' @param org Github organization name.
#' @param filter See options below.
#' @param state open, closed, default: open
#' @param labels String list of comma separated Label names. Example: bug,ui,@high
#' @param sort created, updated, comments, default: created. 
#' @param direction asc or desc, default: desc.
#' @param since Optional string of a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
#' @param parse Whether to parse results or not. Setting parse=TRUE composes a 
#'    list of nested items of similar attributes, each with 0 to many items: 
#'    urls, info, user, pull_request, repo, and body
#' @description 
#' Options for type parameter:
#' \itemize{
#'  \item{global} List all issues across all the authenticated user’s visible 
#'  repositories including owned repositories, member repositories, and 
#'  organization repositories
#'  \item{user} List all issues across owned and member repositories for the 
#'  authenticated user
#'  \item{org} List all issues for a given organization for the authenticated user
#' }
#' 
#' Options for filter parameter:
#' \itemize{
#'  \item{assigned} Issues assigned to you (default)
#'  \item{created} Issues created by you
#'  \item{mentioned} Issues mentioning you
#'  \item{subscribed} Issues you’re subscribed to updates for
#'  \item{all} All issues the authenticated user can see, regardless of 
#'  participation or creation state
#' }
#' @return Vector of names or repos for organization or user.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_issues()
#' g_issues('user')
#' g_issues('user', 'mentioned')
#' g_issues('org', 'ropensci')
#' }
#' @export
g_issues <- function(type='global', org = NULL, filter = 'all', state = NULL, 
                     labels = NULL, sort = NULL, direction = NULL, since = NULL,
                     parse = TRUE)
{
  type <- match.arg(type, choices=c('global','user','org'))
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  if(type == 'global'){ url <- "https://api.github.com/issues" } else
    if(type == 'user'){ url <- "https://api.github.com/user/issues" } else
      { url <- sprintf("https://api.github.com/orgs/%s/issues", org) }
  args <- compact(list(access_token=access_token, filter=filter, state=state, 
               labels=labels, direction=direction, since=since))
  out <- content(GET(url, add_headers('User-Agent' = useragent), query=args))
  
  if(parse){
    parseout <- function(x){
      urls <- x[c('url','labels_url','comments_url','events_url','html_url')]
      info <- x[c('id','number','title','state','assignee','milestone','comments','created_at','updated_at','closed_at')]
      user <- data.frame(x$user)
      pull_request <- data.frame(x$pull_request)
      repo_owner <- x$repository[['owner']]
      repo <- x$repository[!names(x$repository) %in% 'owner']
      body <- x$body
      list(urls = urls, info = info, user = user, pull_request = pull_request, repo = repo, body = body)
    }
    res <- llply(out, parseout)
    class(res) <- 'issues'
    res
  }
  else
  {
    class(out) <- 'issues'
    out
  }
}

#' Print summary, just issue titles and IDs
#' @method print issues
#' @S3method print issues
#' @export
print.issues <- function(x)
{
  if(!is.issues(x))
    stop("Input is not of class issues")  
  cat("\nIssue Titles\n")
  print(ldply(x, function(y) data.frame(c(y$info$number,y$info$title))))
}

#' Check if object is of class issues
#' @param x input
#' @export
is.issues <- function(x) inherits(x, "issues")