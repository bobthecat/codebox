#
# sex_diff_pain.r
#
# Created by David Ruau on 2011-02-17.  
# Department of Pediatrics/Div. System Medicine Stanford University.
# 
##################### USAGE #########################
# 
# source('pubmed_trend.r')
# sex.pub <- pubmed_trend(search.str = 'Sex+Characteristics[mh] AND Pain[mh]', year.span=1970:2011)
# 
#####################################################

pubmed_trend <- function(search.str = 'Sex+Characteristics[mh] AND Pain[mh]', year.span=1970:2011) {
  require(XML)
  require(RCurl)
  
  results <- NULL
  tmpf <- "./tempfile.xml"
  ## clean before
  system(paste("rm", tmpf))

  for(i in year.span){
    queryString <- paste(search.str, ' AND ', i, '[dp]', sep="")
    print(paste('queryString:', queryString))
    sysString <- paste('./pubmed_trend.pl "', queryString,'"', sep="")
    system(sysString)

    xml <- xmlTreeParse(tmpf, useInternalNodes=TRUE)
    pubTerm <- as.numeric(xmlValue(getNodeSet(xml, "//Count")[[1]]))
    print(paste("#______num pub for",i,":",pubTerm))
    rm(xml)
    results <- append(results, pubTerm)
    ## avoid being kicked out!
    Sys.sleep(1)
  }
  names(results) <- year.span
  ## clean after
  system(paste("rm", tmpf))
  
  return(results)
}