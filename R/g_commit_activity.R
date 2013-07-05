#' Get a years worth of commit activity grouped by week
#' @description Get a years worth commit data summarized by both daily count and weekly total count.
#' @import httr plyr
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @return A data frame with .
#' @examples \dontrun{
#' github_auth()
#' options(useragent='ropensci')
#' jekyll_commit <- g_commit_activity("mojombo","jekyll")
#' week_sums <- ddply(jekyll_commit,.(Week),summarise,week_avg = mean(weekly_count))
#' ggplot(week_sums,aes(x=Week,y=week_avg))+geom_point()+geom_path()+ylab("Weekly commit totals")
#' }
#' @export

g_commit_activity(userorg,repo){
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')

  url <- "https://api.github.com/repos"
  url2 <- paste(url,userorg,repo,"stats/commit_activity",sep="/") 
  args <- list(access_token=access_token)
  ca <- content(GET(url2, add_headers('User-Agent' = useragent), query=args))
  
  df_out <- ldply(ca,data.frame)
  df_out$week <- as.Date(as.POSIXct(df_out$week,origin = "1970-01-01",tz="GMT"))
  days <- as.factor(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  days <- ordered(days,c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  df_out$day <- rep(days,52)
  colnames(df_out) <- c("daily_count","weekly_count","Week","Day")
  return(df_out)
  

}
