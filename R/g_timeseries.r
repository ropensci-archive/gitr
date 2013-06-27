#' Create time series bar plot.
#' 
#' @import plyr ggplot2 reshape2 ggthemes
#' @param data Data set to plot with.
#' @examples \dontrun{
#' # Run with example data set (commits from the ropensci organization account)
#' g_timeseries()
#' 
#' # Get your own data
#' g_auth()
#' mydat <- llply(c('ggplot2','plyr','httr'), function(x) g_commits(userorg='hadley',repo=x,limit=500))
#' mydat <- ldply(mydat)
#' g_timeseries(mydat)
#' }
#' @export
g_timeseries <- function(data = NULL)
{	
	data(ropensci_commits) # load data set
	ropensci_commits$date <- as.Date(as.character(ropensci_commits$date))
	dframe <- ropensci_commits[!ropensci_commits$.id %in% c("citeulike", "challenge", "docs", "ropensci-book", 
		"usecases", "textmine", "usgs", "ropenscitoolkit", "neotoma", "rEWDB", "rgauges", 
		"rodash", "ropensci.github.com", "ROAuth"), ]
	dframe$.id <- tolower(dframe$.id)
	dframe <- ddply(dframe, .(.id, date), summarise, value = sum(value))

	mindates <- llply(unique(dframe$.id), function(x) min(dframe[dframe$.id == x, "date"]))
	names(mindates) <- unique(dframe$.id)
	mindates <- sort(do.call(c, mindates))
	dframe$.id <- factor(dframe$.id, levels = names(mindates))

	ggplot(dframe, aes(date, value, fill = .id)) + 
		geom_bar(stat = "identity", width = 0.5) + 
		geom_rangeframe(sides = "b", colour = "grey") + 
		theme_bw(base_size = 9) + 
		scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
		scale_y_log10() + 
		facet_grid(.id ~ .) + 
		labs(x = "", y = "") + 
		theme(axis.text.y = element_blank(), 
			axis.text.x = element_text(colour = "black"), 
			axis.ticks.y = element_blank(), 
			strip.text.y = element_text(angle = 0, size = 8, ), 
			strip.background = element_rect(size = 0), 
			panel.grid.major = element_blank(), 
			panel.grid.minor = element_blank(), 
			legend.text = element_text(size = 8), 
			legend.position = "none", 
			panel.border = element_blank())
}

