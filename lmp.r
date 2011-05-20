#
# lmp.r
#
# http://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
#
# Function to extract the overall ANOVA p-value out of a linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
