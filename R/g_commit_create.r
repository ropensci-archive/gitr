#' Get GitHub downloads for a repo
#' 
#' @import httr
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @param message String of the commit message.
#' @param tree String of the SHA of the tree object this commit points to.
#' @param parents Array of the SHAs of the commits that were the parents of this commit. 
#'    If omitted or empty, the commit will be written as a root commit. For a 
#'    single parent, an array of one SHA should be provided, for a merge commit, 
#'    an array of more than one should be provided.
#' @return stuff.
#' @examples \dontrun{
#' github_auth()
#' options(useragent='ropensci')
#' g_commit(userorg = 'ropensci', repo='rgbif')
#' }
#' @export
g_commit_create <- function(userorg = NULL, repo = NULL, message = NULL, tree = NULL, parents = NULL)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url = "https://api.github.com/repos/"
  url2 <- paste0(url, userorg, '/', repo, '/git/commits')
  args <- list(access_token = access_token, message = message, tree = tree, parents = parents)
  content(POST(url2, add_headers('User-Agent' = useragent), query=args))
}