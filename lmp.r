#
# lmp.r
#
# http://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
#
# Function to extract the overall ANOVA p-value out of a linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm'")
  f <- summary(modelobject)$fstatistic
  print(f)
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# lmp.gls(ModFull, data.average$zygosity)
lmp.gls <- function (modelobject, varianceFactor=NULL) {
  if (class(modelobject) != "gls") stop("Not an object of class 'gls'")
  z <- summary(modelobject)
  # fitted values
  f <- modelobject$fitted
  # rank (number of column) of the model matrix
  p <- modelobject$dims$p
  # total number of degree of freedom
  df.int <- modelobject$dims$N
  # residual degree of freedom
  rdf <- df.int - p
  # residuals
  r <- modelobject$residuals
  # weights
  w <- as.vector(coef(modelobject$modelStruct, unconstrained=F))
  w <- ifelse(varianceFactor == 1, 1, w)
  # print(w)
  if (is.null(w)) {
    mss <- sum((f - mean(f))^2)
    rss <- sum(r^2)
    print(paste(mss, rss))
  } 
  else {
    m <- sum(w * f/sum(w))
    mss <- sum(w * (f - m)^2)
    rss <- sum(w * r^2)
  }
  # 
  resvar <- rss/rdf
  # F-statistics
  f <- c(value = (mss/(p))/resvar, numdf = p, dendf = rdf)
  print(f)
  p <- pf(f[1],f[2],f[3], lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

