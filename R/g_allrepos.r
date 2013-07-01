#' List GitHub repositories for a user or organization.
#' 
#' @import httr
#' @param userorg User or organization GitHub name.
#' @param type One of user or org (defaults to org)
#' @param repo Repository name.
#' @param per_page (optional) Number of results to return
#' @return Vector of names or repos for organization or user.
#' @examples \dontrun{
#' github_auth()
#' options(useragent='ropensci')
#' g_allrepos(userorg = 'ropensci')
#' }
#' @export
g_allrepos <- function(userorg = NA, type = 'org', return_ = 'names', per_page = 100)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url = "https://api.github.com/"
  if(type == 'org'){
    url2 <- paste0(url, 'orgs/', userorg, '/repos')
  } else
  { url2 <- paste0(url, 'users/', userorg, '/repos') }
  args <- list(access_token=access_token, per_page=per_page)
  tt = content(GET(url2, add_headers('User-Agent' = useragent), query=args))
  
  if(return_=='show'){tt} else
    if(return_=='names'){
        sapply(tt, function(x) x$name)
    } else
      { NULL }
}