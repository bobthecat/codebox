#
# slopegraph.r
#
# Created by David Ruau on 2011-07-18.  
# 2011 Dept. of Pediatrics/Div. Systems Medicine
# Stanford University.
# 
#
##################### USAGE #########################
# label.cex: magnification for numeric line labels from 0 to 1
# axis.cex: magnificatoin for axis titles from 0 to 1
# digits: number of significant digits to report
# rounding.method: can be NULL, round or signif
# ...: supplementary arguments supplied to par, usually margins
#
# EXAMPLE:
# source('slopegraph.r')
# pdf('slopegraph.pdf', height=7, width=8)
# slopegraph(data = WorldPhones[,1:3], mymain = "YEARS", mar=c(5, 5, 5, 5))
# dev.off()
# 
#####################################################

slopegraph <- function(data, label.cex=0.9, axis.cex=0.9, digits = 2, rounding.method = NULL, mymain = "slopegraph", ...) {
  require(plotrix)
  if(!is.null(rounding.method)){
    fmt <- .rd.method(rounding.method, width, digits)
    data.annot <- sprintf(fmt, data)
  }
  else{
    data.annot <- as.character(as.vector(data))
  }
  data.annot <- matrix(data.annot, nrow=nrow(data), ncol=ncol(data))
  old.par <- par(no.readonly = TRUE)
  par(...)
  matplot(data, type='b', pch=NA, axes=FALSE, xlab='', ylab='', lty='solid', col="grey", ...)
  mtext(text = rownames(data), side = 3, at=1:nrow(data), line = 1, cex=axis.cex)
  mtext(text = colnames(data), side = 2, at=data[1,], line = 1, las=1, cex=axis.cex)
  title(main = mymain, line=3)
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      boxed.labels(i, data[i,j], labels=data.annot[i,j], bg='white', border = FALSE, cex=label.cex)
    }
  }
  mtext(text = colnames(data), side = 4, at=data[nrow(data),], line = 1, las=1, cex=axis.cex)
  par(old.par)
}


.rd.method <- function(rounding.method, width, digits){
  rounding.character <- switch(match(rounding.method, c("round", "signif")), "f", "g")
  fmt = paste("%.", digits, rounding.character, sep = "")
  return(fmt)
}


