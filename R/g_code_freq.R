#' Get weekly additions and deletions for a reop
#' @description Get the entire history of weekly totals of additions and deletions for a given repo.
#' @import httr plyr
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @return A data frame with .
#' @examples \dontrun{
#' require(ggplot2)
#' g_auth()
#' options(useragent='ropensci')
#' jekyll_cf <- g_code_freq("mojombo","jekyll")
#' ggplot(jekyll_cf,aes(x=week,y=log(additions+1)))+geom_point()+geom_path()+ylab("Weekly number of additions")
#' }
#' @export

g_code_freq <- function(userorg,repo){
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
  url <- "https://api.github.com/repos"
  url2 <- paste(url,userorg,repo,"stats/code_frequency",sep="/") 
  args <- list(access_token=access_token)
  cf <- content(GET(url2, add_headers('User-Agent' = useragent), query=args))
  df_out <- ldply(cf)
  colnames(df_out) <- c("week","additions","deletions")
  df_out$week <- as.Date(as.POSIXct(df_out$week,origin = "1970-01-01",tz="GMT"))
  
  return(df_out)
  
}

