#' List GitHub gists
#' 
#' @import httr
#' @param since Optional string of a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
#' @param parse Whether to parse results or not. Setting parse=TRUE composes a 
#'    list of nested items of similar attributes, each with 0 to many items: 
#'    urls, info, user, pull_request, repo, and body
#' @return The gist.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_gists('mine', 'schamberlain')
#' g_gists('public')
#' g_gists('starred')
#' g_gists('gistid', gistid=5937553)
#' }
#' @export
g_gists <- function(type='mine', user=NULL, gistid=NULL, since=NULL, parse = TRUE)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  

  if(type == 'mine'){ url <- sprintf("https://api.github.com/users/%s/gists", user) } else
    if(type == 'public'){ url <- "https://api.github.com/gists/public" } else
      if(type == 'starred'){ url <- "https://api.github.com/gists/starred" } else
        if(type == 'gistid'){ url <- sprintf("https://api.github.com/gists/%s", gistid) } else
          { stop("type must be on of mine, public, starred, or gistid") }
  args <- compact(list(access_token=access_token, since=since))
  out <- content(GET(url, add_headers('User-Agent' = useragent), query=args))
  out
}
  # if(parse){
  #   res <- llply(out, parse_issue)
  #   class(res) <- 'issues'
  #   res
  # }
  # else
  # {
  #   class(out) <- 'issues'
  #   out
  # }
# }