#' Create a new github repo.
#' 
#' @import plyr httr
#' @param userorg User or organization GitHub name.
#' @param name Your new repo name. Required
#' @param description Description of repo. Optional
#' @param homepage Homepage for repo. Optional
#' @param private Make the repo private? Creating private repositories requires a paid GitHub account.
#' @param has_issues true to enable issues for this repository, false to disable them. 
#' @param has_wiki true to enable the wiki for this repository, false to disable it
#' @param has_downloads true to enable downloads for this repository, false to disable them. 
#' @param team_id The id of the team that will be granted access to this repository. This is only valid when creating a repo in an organization.
#' @param auto_init true to create an initial commit with empty README. Default is False.
#' @param gitignore_template See \link{https://github.com/github/gitignore}
#' @param session (optional) the authentication credentials from \code{\link{github_auth}}. 
#'		If not provided, will attempt to load from cache as long as github_auth has been run. 
#' @examples \dontrun{
#' github_auth(scope='repo')
#' options(useragent='ropensci')
#' g_create_repo(user='schamberlain', name='test')
#' 
#' g_create_repo(user='schamberlain', name='test', description='testing', homepage='http://schamberlain.github.com/scott/')
#' }
#' @export
g_create_repo <- function(user=NULL, org=NULL, name=NULL, description=NULL, 
	homepage=NULL, private='False', has_issues='True', has_wiki='True', 
	has_downloads='True', team_id=NULL, auto_init='False', 
	gitignore_template=NULL)
{
	message("this function doesn't currently work")
# 	session = sandbox:::github_get_auth(scope='repo')
#   useragent <- getOption('useragent')
#   if(is.null(useragent))
#     stop('You must provide a User-Agent string')
#   
#   access_token <- getOption('github_token')
#   if(is.null(access_token))
#     stop('You must authenticate with Github first, see github_auth()')
#   
# 	url = "https://api.github.com/"
# 	if(!is.null(user))
# 		# url2 <- paste(url, user, '/repos', sep='')
# 		url2 <- paste0(url, 'user/repos?access_token=', access_token)
# 	if(!is.null(org))
# 		  url2 <- paste0(url, 'orgs/', org, '/repos?access_token=', access_token)
# 	args <- compact(list(name=name, description=description, homepage=homepage, 
# 		private=private, has_issues=has_issues, has_wiki=has_wiki, 
# 		has_downloads=has_downloads, team_id=team_id, auto_init=auto_init, 
# 		gitignore_template=gitignore_template, access_token=access_token))	
#   tt <- content(POST(url2, config=add_headers('User-Agent' = useragent), body=args))
# 	return( tt )
}
