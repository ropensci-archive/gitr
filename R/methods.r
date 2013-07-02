#' Print summary, just issue titles and IDs
#' @method summary issues
#' @S3method summary issues
#' @export
summary.issues <- function(x)
{
  if(!is.issues(x))
    stop("Input is not of class issues")
  
  if(all(names(x) %in% c('urls', 'info', 'user', 'pull_request', 'repo', 'body'))){
    temp <- ldply(x, function(y) c(issue_no=y$info$number,issue_title=y$info$title))    
  } else
  {
    temp <- c(issue_no=x$number,issue_title=x$title)
  }
  cat("\nIssue Titles\n")
  print(temp)
}

#' Print summary, just comment ID, user, and body of comment
#' @method summary comments
#' @S3method summary comments
#' @export
summary.comments <- function(x)
{
  if(!is.comments(x))
    stop("Input is not of class comments")
  
  if(all(names(x) %in% c('urls', 'info', 'user', 'body'))){
    temp <- llply(x, function(y) c(comment_id=y$info$id,user=y$user$login,comment=y$body))
  } else
  {
    temp <- c(comment_id=x$info$id,user=x$user$login,comment=x$body)
  }
  print(temp)
}

# pretty_comments <- function(x)
# {
#   if(!is.comments(x))
#     stop("Input is not of class comments")
# 
#   if(all(names(x) %in% c('urls', 'info', 'user', 'body'))){
#     temp <- llply(x, function(y) y$body)
#   } else
#   {
#     temp <- x$body
#   }
#   cat("\nComments\n")
#   print(temp)
# }

#' Print details of an issue
#' @param input An issues object
#' @examples \dontrun{
#' issue_deets()
#' }
#' @export
issue_deets <- function(x)
{
  if(!is.issues(x))
    stop("Input is not of class issues")
  x
}

#' Check if object is of class issues
#' @param x input
#' @export
is.issues <- function(x) inherits(x, "issues")

#' Check if object is of class comments
#' @param x input
#' @export
is.comments <- function(x) inherits(x, "comments")