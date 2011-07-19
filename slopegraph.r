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
# slopegraph(data = t(WorldPhones[,1:3]), mymain = "YEARS", mar=c(2, 5, 5, 5), label.cex=0.8, axis.cex=0.9)
# dev.off()
#
# Tips: when values overlap try first to extend the height of you plot and if this does not work
# round your value using the option rounding.method = 'round' and digits=0
# 
#####################################################

slopegraph <- function(data, label.cex=0.8, axis.cex=0.9, digits = 2, rounding.method = NULL, mymain = "slopegraph", ...) {
  require(plotrix)
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  
  if(!is.null(rounding.method)){
    data.temp <- .rd.method(rounding.method, width, digits)
    data.temp <- as.numeric(sprintf(fmt, as.matrix(data)))
    data <- as.data.frame(matrix(data.temp, nrow=nrow(data), ncol=ncol(data), dimnames=list(rownames(data), colnames(data))))
  }
  
  old.par <- par(no.readonly = TRUE)
  par(...)
  matplot(t(data), type='b', pch=NA, axes=FALSE, xlab='', ylab='', lty='solid', col="grey", ...)
  for(i in 1:ncol(data)){
    for(j in 1:nrow(data)){
      boxed.labels(i, data[j,i], labels=data[j,i], bg='white', border = FALSE, cex=label.cex)
    }
  }
  mtext(text = rownames(data), side = 2, at=data[,1], line = 0.5, las=1, cex=axis.cex)
  mtext(text = colnames(data), side = 3, at=1:ncol(data), line = 1, cex=axis.cex)
  mtext(text = rownames(data), side = 4, at=data[,ncol(data)], line = 0.5, las=1, cex=axis.cex)
  title(main = mymain, line=3)
  par(old.par)
}


.rd.method <- function(rounding.method, width, digits){
  rounding.character <- switch(match(rounding.method, c("round", "signif")), "f", "g")
  fmt = paste("%.", digits, rounding.character, sep = "")
  return(fmt)
}


