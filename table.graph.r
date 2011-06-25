#
# table.graph.r
#
# Created by David Ruau on 2011-06-23.  
# Copyright (c) 2011 Department of Pediatrics/Cancer Biology Stanford University. All rights reserved.
# 
#
##################### USAGE #########################
# Following the example table-graphic shown in the Tufte book
# "the visal display of quantitative information" p. 158
#
# Column names and row names will be used to label the plot
# Depending on the length of your rownames the margin might have to be adjusted
#
# df: data frame with 2 column
# table.graph(df)
# 
#####################################################

table.graph <- function(df, line.col=c("grey", "black")) {
  xmin <- min(df)
  xmax <- max(df)
  X <- as.numeric(as.vector(x$[,1]))
  X <- as.numeric(as.vector(x$[,2]))
  # margin
  par(mar=c(5, 8, 1, 8) + 0.1)
  # left
  plot(rep(0, nrow(df)), X1, xlim=c(0,1), ylim=c(xmin, xmax), 
    axes=FALSE, xlab='', ylab='', type='n')
  mtext(text=paste(rownames(df), X1, sep='  '), side=2, at=X1, las=1, cex=0.7)
  par(new=TRUE)
  # right
  plot(rep(1, nrow(df)), X2, xlim=c(0,1), ylim=c(xmin, xmax), 
    axes=FALSE, xlab='', ylab='', type='n')
  mtext(text=paste(X2, rownames(df), sep='  '), side=4, at=X2, las=1, cex=0.7)
  # margin text
  mtext(colnames(df)[1], side=3, at=0, cex=0.9)
  mtext(colnames(df)[2], side=3, at=1, cex=0.9)
  # lines
  segments(x0 = rep(0, nrow(df)), y0 = X1, x1 = rep(1, nrow(df)), y1 = X2,
   col=ifelse({X1 - X2} < 0, line.col[1], line.col[2]))
}