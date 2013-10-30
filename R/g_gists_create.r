#' Create a GitHub gist
#' 
#' @import httr
#' @param description Optional string
#' @param public Required boolean
#' @param filename Path to a file with the code you want to upload.
#' @param content The content of the file (only use if filename=NULL).
#' @return The gist metadata.
#' @examples \dontrun{
#' g_auth()
#' options(useragent='ropensci')
#' g_gists_create(description='my cool description', filename="fromr.md", content="testing creating a gist from R")
#' g_gists_create(description='my cool description', filename="~/github/ropensci/stuff.md")
#' }
#' @export
g_gists_create <- function(description=NULL, public='true', filename=NULL, content=NULL)
{
  useragent <- getOption('useragent')
  if(is.null(useragent))
    stop('You must provide a User-Agent string')
   
  access_token <- getOption('github_token')
  if(is.null(access_token))
    stop('You must authenticate with Github first, see g_auth()')

  if(is.null(content))
    content <- readLines(filename)

# files <- '{"description":"my cool description", "public":true, "files": {"fromr.md" : {"content": "```coffee\\npoints <- rnorm(10)\\nplot(points)\\n```"}  }}'
  files <- paste0('{"description":"',description,'","public":',public,',"files":{"',filename,'":{"content":"',content,'"}}}')
  
  url <- sprintf("https://api.github.com/gists?access_token=%s", access_token)
  out <- content(POST(url, add_headers('User-Agent' = useragent), body=I(files)))
  out
}

# thing <- '
#### Do a thing
# points <- rnorm(10)
# plot(points)

#### Do something else
# ![](http://upload.wikimedia.org/wikipedia/commons/3/39/Bachelor%27s_button%2C_Basket_flower%2C_Boutonniere_flower%2C_Cornflower_-_3.jpg)
# '