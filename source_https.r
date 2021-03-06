source_https <- function(url, ...) {
	     # load package
	     require(RCurl)

	     # parse and evaluate each .R script
	     sapply(c(url, ...), function(u) {
	     eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
	     })
	}


# prerequirements
# at the shell
# > sudo apt-get install libcurl4-openssl-dev
# in R
# > install.packages("RCurl")


# Example
# source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R", "https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/htmlToText/htmlToText.R"))

# NOTE: This function is from http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/