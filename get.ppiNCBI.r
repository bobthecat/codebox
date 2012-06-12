#
# get.ppiNCBI.r
#
# Created by David Ruau on 2012-06-05.
# Dept. of Pediatrics/Div. Systems Medicine, Stanford University.
# 
#
##################### USAGE #########################
# see http://brainchronicle.blogspot.com/2012/05/obtain-protein-protein-interaction-from.html
# ppi <- get.ppiNCBI(c("777", "57619"))
# 
#####################################################

get.ppiNCBI <- function(g.n) {
  require(XML)
  ppi <- data.frame()
  for(i in 1:length(g.n)){
    o <- htmlParse(paste("http://www.ncbi.nlm.nih.gov/gene/", g.n[i], sep=''))
    # check if interaction table exists
    exist <- length(getNodeSet(o, "//table//th[@id='inter-prod']"))>0
    if(exist){
      p <- getNodeSet(o, "//table")
      ## need to know which table is the good one
      for(j in 1:length(p)){
        int <- readHTMLTable(p[[j]])
        if(colnames(int)[2]=="Interactant"){break}
      }
      ppi <- rbind(ppi, data.frame(egID=g.n[i], intSymbol=int$`Other Gene`))
    }
    # play nice! and avoid being kicked out from NCBI servers
    Sys.sleep(1)
  }
  if(dim(ppi)[1]>0){
    ppi <- unique(ppi)
    print(paste(dim(ppi)[1], "interactions found"))
    return(ppi)
  } else{
    print("No interaction found")
  }
}
