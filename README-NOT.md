# gitr #

[![Project Status: Abandoned – Initial development has started, but there has not yet been a stable, usable release; the project has been abandoned and the author(s) do not intend on continuing development.](http://www.repostatus.org/badges/latest/abandoned.svg)](http://www.repostatus.org/#abandoned)

You can use the following packages:

* [`git2r`](https://github.com/ropensci/git2r), R bindings to the libgit2 library, to interact with git repos. 

* the more specific [`gitsum`](https://github.com/lorenzwalthert/gitsum) package parses and summarises git repository history.

* [`gh`](https://github.com/r-lib/gh) is a minimalistic GitHub API V3 client

* [`ghql`](https://github.com/ropensci/ghql), general purpose GraphQL R client, allows to write a client to Github API V4 (with examples in the docs).

Install using install_github within Hadley's devtools package.

```R
install.packages("devtools")
require(devtools)
install_github("gitr", "ropensci")
require(gitr)
```

git & github from R

* [GitHub](http://github.com/) - [API documentation](http://developer.github.com/) - Get metrics on code repositories, including forks, watchers, and open issues. 

A similar project from AT&T folks called rgithub [here](https://github.com/cscheid/rgithub)
