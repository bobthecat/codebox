#
# Affy_QA.r
#
# Created by David Ruau on 2011-07-26.  
# 2011 Dept. of Pediatrics/Div. Systems Medicine, Stanford University.
# 
#
##################### USAGE #########################
# R function to perform automatically all the quality control test describe
# in the book "Bioinformatics and Computational Biology Solutions Using R and Bioconductor"
# from Gentleman, Carey, Huber, Irizarry and Dudoit; Springer.
# argument: an affybatch
# 
# EXAMPLE:
# library(affydata)
# data(Dilution)
# Affy_QA(Dilution)
# 
#####################################################

Affy_QA <- function(abatch) {
  require(affy)
  require(simpleaffy)
  require(RColorBrewer)
  require(affyPLM)

  if (class(abatch)!= 'AffyBatch') {
    stop("argument must be AffyBatch!")
  }

  # colors
  cols <- brewer.pal(12, "Set3")

  # Boxplot
  pdf(file='qa_boxplot.pdf', height=8, width=10)
  boxplot(abatch, col=cols, main="Unprocessed log scale probe-level data", xlab="If discrepancy, they are not conclusive\n Difference can be reduce by normalization")
  dev.off()

  # Histogram
  pdf(file='qa_histogram.pdf', height=8, width=8)
  hist(abatch, col=cols, lwd=2, xlab="Log(base2) intensities; Bimodal distribution indicate spatial artifact\n Second mode is the result of array(s) having abnormally high value")
  legend("topright", sampleNames(abatch), lty=1, lwd=2,col=cols)
  dev.off()

  #RNA degradation
  pdf(file="qa_RNAdeg.pdf", height=8, width=8)
  RNAdeg <- AffyRNAdeg(abatch)
  plotAffyRNAdeg(RNAdeg, cols=cols)
  legend("topleft", sampleNames(abatch), lty=1, lwd=2, col=cols)
  box()
  dev.off()

  # simpleaffy graph
  abatch.qc <- qc(abatch)
  pdf(file="qa_QC-simpleaffy.pdf", height=8, width=10)
  plot(abatch.qc)
  dev.off()

  pset <- fitPLM(abatch)

  # false color image control
  for (n in 1:length(abatch)) {
    filename <- paste("qa_QC",as.vector(sampleNames(abatch))[n],".png")
    png(file=filename, height=900, width=800)
    .img.Test(abatch,pset,n)
    dev.off()
  }

  # RLE plot
  pdf(file="qa_RLE.pdf", height=8, width= 8)
  Mbox(pset, col = cols, main ="RLE (Relative Log Expression)", xlab="Assuming that the majority of the gene are not changing\n Ideally these boxes would have small spread and be centered at M=0")
  dev.off()

  # NUSE plot
  pdf(file="qa_NUSE.pdf", height=8, width= 8)
  boxplot(pset, col=cols, main= "NUSE (Normalized Unscaled Standard Error)", xlab="High values of median NUSE are indicative of a problematic array")
  dev.off()
}

.img.Test <- function(batch,pset,x) {
par(mfrow = c(2,2))
affy::image(batch[,x])
affy::image(pset, type = "weights", which = x)
affy::image(pset, type = "resids", which = x)
affy::image(pset, type = "sign.resids", which = x)
}