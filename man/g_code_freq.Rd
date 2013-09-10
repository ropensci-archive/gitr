\name{g_code_freq}
\alias{g_code_freq}
\title{Get weekly additions and deletions for a reop}
\usage{
  g_code_freq(userorg, repo)
}
\arguments{
  \item{userorg}{User or organization GitHub name.}

  \item{repo}{Repository name.}
}
\value{
  A data frame with .
}
\description{
  Get the entire history of weekly totals of additions and
  deletions for a given repo.
}
\examples{
\dontrun{
require(ggplot2)
g_auth()
options(useragent='ropensci')
jekyll_cf <- g_code_freq("mojombo","jekyll")
ggplot(jekyll_cf,aes(x=week,y=log(additions+1)))+geom_point()+geom_path()+ylab("Weekly number of additions")
}
}

