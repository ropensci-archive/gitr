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