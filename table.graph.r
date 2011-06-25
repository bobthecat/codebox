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
# line.col: vector length 2 with colors for the lines. Default to grey for
#   value going up and black for value going down
# label.cex: magnification for x and y labels
# title.cex: magnificatoin for titles
# ...: supplementary arguments supplied to par usually margin
# table.graph(df, mar=c(5, 5, 1, 5))
# 
#####################################################

table.graph <- function(df, line.col=c("grey", "black"), label.cex=1, title.cex=1, ...) {
  xmin <- min(df)
  xmax <- max(df)
  X1 <- as.numeric(as.vector(df[,1]))
  X2 <- as.numeric(as.vector(df[,2]))
  # original settings
  old.par <- par(no.readonly = TRUE)
  # par settings usually margins
  par(...)
  # left
  plot(rep(0, nrow(df)), X1, xlim=c(0,1), ylim=c(xmin, xmax), 
    axes=FALSE, xlab='', ylab='', type='n')
  mtext(text=paste(rownames(df), X1, sep='  '), side=2, at=X1, las=1, cex=label.cex)
  par(new=TRUE)
  # right
  plot(rep(1, nrow(df)), X2, xlim=c(0,1), ylim=c(xmin, xmax), 
    axes=FALSE, xlab='', ylab='', type='n')
  mtext(text=paste(X2, rownames(df), sep='  '), side=4, at=X2, las=1, cex=label.cex)
  # class label
  mtext(colnames(df)[1], side=3, at=0, cex=title.cex)
  mtext(colnames(df)[2], side=3, at=1, cex=title.cex)
  # lines
  segments(x0 = rep(0, nrow(df)), y0 = X1, x1 = rep(1, nrow(df)), y1 = X2,
   col=ifelse({X1 - X2} < 0, line.col[1], line.col[2]))
  # restore original settings
  par(old.par)
}