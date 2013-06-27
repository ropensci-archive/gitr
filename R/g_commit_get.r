#' Get commit details with specific sha.
#' 
#' @import httr
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @param sha Githu sha alphanumeric string.
#' @return stuff.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_commit_get(userorg = 'ropensci', repo='rgbif', sha='7549351599f3d7de97a1800a439071f7c5c57c98')
#' }
#' @export
g_commit_get <- function(userorg = NULL, repo = NULL, sha = NULL)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url = "https://api.github.com/repos/"
  url2 <- paste0(url, userorg, '/', repo, '/git/commits/', sha)
  args <- list(access_token=access_token)
  content(GET(url2, add_headers('User-Agent' = useragent), query=args))
}