#' Authenticate with github
#' @import httr
#' @param client_id Consumer key. can be supplied here or read from Options()
#' @param client_secret Consumer secret. can be supplied here or read from Options()
#' @param scope Comma separated list of scopes. One or more of: user, user:email, 
#' 		user:follow, public_repo, repo, repo:status, delete_repo, notifications, gist
#' @examples \dontrun{
#' g_auth()
#' }
#' @export
g_auth <- function(client_id = NULL, client_secret = NULL, scope = NULL)
{
	if(!is.null(client_id)) {client_id=client_id} 
		else {client_id = getOption("github_client_id", stop("Missing Github client id"))}
	if(!is.null(client_secret)) {client_secret=client_secret} 
		else {client_secret = getOption("github_client_secret", stop("Missing Github client secret"))}
	if(!exists("github_sign")){
		github_app <- oauth_app("github", key=client_id, secret=client_secret)
		github_urls <- oauth_endpoint(NULL, "authorize", "access_token", base_url = "https://github.com/login/oauth")
		github_token <- oauth2.0_token(github_urls, github_app, scope = scope)
    options(github_token = github_token$access_token)
# 		github_sign <- httr::sign_oauth2.0(github_token$access_token)
# 		assign('github_sign', github_sign, envir=sandbox:::GitHubAuthCache)
# 		message("\n GitHub authentication was successful \n")
# 		invisible(github_sign)
	} else { NULL }
}