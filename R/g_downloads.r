#' Get GitHub downloads for a repo
#' 
#' @import httr
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @return List of results.
#' @examples \dontrun{
#' github_auth()
#' options(useragent='ropensci')
#' g_downloads(userorg = 'ropensci', repo='rgbif')
#' }
#' @export
g_downloads <- function(userorg = NULL, repo = NULL)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url = "https://api.github.com/repos/"
  url2 <- paste0(url, userorg, '/', repo, '/downloads')
  args <- list(access_token=access_token)
  content(GET(url2, add_headers('User-Agent' = useragent), query=args))
}