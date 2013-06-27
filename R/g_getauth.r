#' Helper function to get authentication
#'
#' The authentication environment is created by new.env function in the zzz.R file.  
#' The authentication token 'oauth' is created by the github_auth() function.  
#' This helper function lets all other functions load the authentication.  
#' @keywords internal
g_get_auth <- function(...)
{
#   if(!exists("github_sign", envir=sandbox:::GitHubAuthCache))
#     tryCatch(github_auth(...), error= function(e) 
#       stop("Requires authentication. 
#       Are your credentials stored in options? 
#       See github_auth function for details."))
#   get("github_sign", envir=sandbox:::GitHubAuthCache)
  message("deprecated")
}