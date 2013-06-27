#' Get GitHub metrics on a user or organization's repositories.
#' 
#' @import plyr ggplot2 httr lubridate reshape2
#' @param userorg User or organization GitHub name.
#' @param repo Repository name.
#' @param since Date to start at.
#' @param until Date to stop at.
#' @param author Specify a committer, if none, will return all.
#' @param limit Number of commits to return per call
#' @param sha The commit sha to start returning commits from.
#' @param timeplot Make a ggplot2 plot visualizing additions and deletions by user. Defaults to FALSE.
#' @return data.frame or ggplot2 figure.
#' @examples \dontrun{
#' g_commits(userorg = 'ropensci', repo = 'rmendeley')
#' g_commits(userorg = 'ropensci', repo = 'rfigshare', since='2009-01-01T')
#' g_commits(userorg = 'ropensci', repo = 'taxize_', since='2009-01-01T', limit=500, timeplot=TRUE)
#' }
#' @export
g_commits <- function(userorg = NA, repo = NA, since = NULL, until = NULL,
	author = NULL, limit = 100, sha = NULL, timeplot = FALSE)
{	
# 	session = sandbox:::g_get_auth()
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
  
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')
  
	url = "https://api.github.com/repos/"
	url2 <- paste0(url, userorg, '/', repo, '/commits')
	if(limit > 100) {per_page = 100} else {per_page = limit}

	if(limit > 100) {
		tt <- list()
		shavec <- list("youdummy")
		iter = 0
		iter_ = 1
		status = "notdone"
		while(status == "notdone"){
			iter <- iter + 1
			iter_ <- iter_ + 1
			args <- compact(list(since = since, until = until, author = author, per_page = per_page, sha = sha, access_token=access_token))
# 			out <- content(GET(url2, session, query=args))
# 			tt = content(GET(url2, user_agent('rOpenSci'), query=args))
			tt <- content(GET(url2, add_headers('User-Agent' = useragent), query=args))
			sha <- out[[length(out)]]$sha; since = NULL; until = NULL
			# sha <- out[[length(out)]]$sha
			shavec[[iter_]] <- sha
			if(shavec[[(length(shavec)-1)]] == shavec[[length(shavec)]]) { status = "done" } else
			{
				tt[[iter]] <- out
			}
		}
		
		tt <- do.call(c, tt)
	} else
	{
		args <- compact(list(since = since, until = until, author = author, per_page = per_page, sha = sha, access_token=access_token))
		tt <- content(GET(url2, add_headers('User-Agent' = useragent), query=args))
# 		tt = content(GET(url2, session, query=args))
# 		tt = content(GET(url2, user_agent('rOpenSci'), query=args))
	}
	
	shas <- unique(laply(tt, function(x) x$sha))
	getstats <- function(x){
# 		tempist <- content(GET(paste0(url2,"/",x), session))
		tempist <- content(GET(paste0(url2,"/",x), user_agent('rOpenSci')))
		c(tempist$sha, tempist$stats[[2]], tempist$stats[[3]])
	}
	stats <- llply(shas, getstats)
	statsdf <- ldply(stats)
	statsdf <- colClasses(statsdf, c("character","numeric","numeric"))
	
	forceit <- function(x){
		dd <- c(x$sha, x$commit$committer[["date"]], x$committer$login)
		if(!length(dd)==3){ c(dd, "whoknowshit") } else
			{ dd }
	}
	temp <- ldply(tt, forceit)
	names(temp) <- c("sha","date","name")
	temp$name <- as.factor(temp$name)
	temp$date <- as.Date(temp$date)
	temp <- temp[!duplicated(temp),]

	alldat <- merge(statsdf, temp, by.x="V1", by.y="sha")
	alldat <- alldat[,-1] # drop sha column
	names(alldat)[1:2] <- c("additions","deletions")

	alldat_m <- melt(alldat, id=3:4)
	
	if(!timeplot){ alldat_m } else
	{
		ggplot(alldat_m, aes(date, value, colour=name)) +
			geom_line() +
			scale_x_date() +
			facet_grid(variable ~ .) + 
			labs(x="", y="")
		
	}
}