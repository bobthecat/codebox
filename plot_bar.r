#
# plot_bar.r
#
# Created by David Ruau on 2011-03-29.  
# Department of Pediatrics/Div. System Medicine Stanford University.
# 
##################### USAGE #########################
# source('sex_diff_pain.r')
# sex.pub <- sex_diff_pain(search.str = 'Sex+Characteristics[mh] AND Pain[mh]', year.span=1970:2011)
# par(las=1)
# plot_bar(x=sex.pub[1:41], linecol='royalblue', cols="Reds", myTitle='Number of publication per year for\n"sex differences" and "pain"')
# 
#####################################################

plot_bar <- function(x=sex.pub, linecol="royalblue", cols, addArg=TRUE) {
  require("RColorBrewer")
  # colorfunction = colorRampPalette(brewer.pal(9, cols))
  # mycolors = colorfunction(length(x))
  ## This draw the bar plot and save it in an object...
  # bp <- barplot(x, col=mycolors)
  bp <- barplot(x, col=cols, add=addArg)
  fit <- stats::lowess(x, f=1/3)
  lines(x=bp, fit$y, col=linecol, lwd=3)  
}
