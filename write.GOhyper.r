#
# write.GOhyper.r
#
# Created by David Ruau on 2012-01-23.  
# Dept. of Pediatrics/Div. Systems Medicine, Stanford University.
# 
#
##################### USAGE #########################
# 
# write.GOhyper(mfhyper, filename="results.xlsx")
# 
#####################################################

write.GOhyper <- function(mfhyper, filename='GO_results.xlsx') {
  require(GOstats)
  require(multtest)
  require(xlsx)
  
  gogo <- summary(mfhyper)
  gogo$adjPvalue <- mt.rawp2adjp(gogo$Pvalue)$adjp[,"BH"]
  gogo <- gogo[,c(1:2,8,3:7)]
  write.xlsx(gogo, file=filename)
  print(paste('Results written in', filename))
  return(gogo)
}
