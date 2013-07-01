#' Get GitHub metrics on a user or organization's repositories.
#' 
#' @import httr
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @param return_ what to return, one of: show (raw data), allstats, watchers, open_issues, or forks
#' @return json
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_repo(userorg = 'ropensci', repo = 'rmendeley')
#' g_repo(userorg = 'hadley', repo = 'ggplot2')
#' g_repo(userorg = 'hadley', repo = 'ggplot2', 'allstats')
#' g_repo(userorg = 'hadley', repo = 'ggplot2', return_ = 'forks')
#' }
#' @export
g_repo <- function(userorg = NA, repo = NA, return_ = 'show')
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
	url = "https://api.github.com/repos/"
	url2 <- paste(url, userorg, '/', repo, sep='')
  args <- list(access_token=access_token)
  tt = content(GET(url2, add_headers('User-Agent' = useragent), query=args))

	if(return_=='show'){tt} else
	if(return_=='allstats'){
    	list('watchers'=tt$watchers, 'open_issues'=tt$open_issues, 
        	 'forks'=tt$forks)} else
  	if(return_=='watchers'){tt$watchers} else
    	if(return_=='open_issues'){tt$open_issues} else
      		if(return_=='forks'){tt$forks}
}
