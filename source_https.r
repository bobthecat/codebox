#
# source_https.r
#
# 2012-02-14.  
# copied from http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
#
##################### USAGE #########################
# 
# source_https(...)
# 
#####################################################

source_https <- function(url, ...) {
  # load package
  require(RCurl)
  # using the raw option
  if(length(grep('https://github.com', url))==1) stop("github RAW source required")
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
