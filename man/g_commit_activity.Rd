\name{g_commit_activity}
\alias{g_commit_activity}
\title{Get a years worth of commit activity grouped by week}
\usage{
  g_commit_activity(userorg, repo)
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{repo}{Repository name.}
}
\value{
  A data frame with .
}
\description{
  Get a years worth commit data summarized by both daily
  count and weekly total count.
}
\examples{
\dontrun{
require(ggplot2)
g_auth()
options(useragent='ropensci')
jekyll_commit <- g_commit_activity("mojombo","jekyll")
week_sums <- ddply(jekyll_commit,.(week),summarise,week_avg = mean(weekly_count))
ggplot(week_sums,aes(x=week,y=week_avg))+geom_point()+geom_path()+ylab("Weekly commit totals")
}
}

