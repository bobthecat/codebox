#
# population.plot.r
#
# Created by David Ruau on 2011-07-09.  
# 2011 Dept. of Pediatrics/Div. Systems Medicine
# Stanford University.
# 
#
##################### USAGE #########################
# 
# m <- c(2300, 2500, 2400, 2000, 1900, 1800, 1000)
# f <- c(2400, 2200, 2500, 3000, 2200, 2000, 1500)
# population.plot(m, f, labels=c('0-18', '19-25', '26-40', '40-50', '51-65', '66-99'))
# 
#####################################################

population.plot <- function(male.count, female.count, sub.title="count", labels=NULL) {
  m.xmax <- max(male.count)
  f.xmax <- max(female.count)
  op <- par()
  par(mfrow=c(1,2), mar=c(4,0,4,1))
  
  barplot(-male.count, axes=F, axisnames=F, space=0, xlim=c(-m.xmax, 0), horiz=T)
  mtext('Male', side=3, at=0, line=1, cex=1.5, adj=1)
  mtext(sub.title, side=1, line=2, at=-mean(male.count), adj=1)
  grid(nx=NULL, ny=NA, col='white', lty="solid")
  # axis(side=1, at=c(0,-1000, -2000, -3000), labels=c(0,1000, 2000, 3000))
  axis(side=1)
  axis(side=4, at=seq(0.5, 10.5, 1), labels=seq(0, 10, 1) ,las=1, tick=F, adj=0.5)
  par(mar=c(4,2,4,2))
  barplot(female.count, axes=F, axisnames=F, space=0, xlim=c(0,f.xmax), horiz=T)
  mtext('Female', side=3, at=0, line=1, cex=1.5, adj = 0)
  mtext("sub.title", side=1, line=2, at=mean(female.count), adj=0)
  grid(nx=NULL, ny=NA, col='white', lty="solid")
  axis(side=1)
  # par reset
  par(op)
}