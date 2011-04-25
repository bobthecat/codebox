#
# hapmap.LD.r
#
# Created by David Ruau on 2011-04-23.  
# Copyright (c) 2011 Department of Pediatrics/Cancer Biology Stanford University. All rights reserved.
# 
#
##################### USAGE #########################
# You give either the Entrez gene name (the SYMBOL) or optionally
# the chr position + the region on the chr using the start and stop
# > hapmap.LD("ABLIM3")
# 
# output the heatmap into a EPS file
# 
#####################################################

hapmap.LD <- function(gene.name=NULL, chr=NULL, 
  start=NULL, stop=NULL, method=c("r2", "Dprime"),
  mysnp=NULL) {
  require(snpMatrix)
  require(NCBI2R)
  require(annotate)
  require(org.Hs.eg.db)
  

  # grab chromosome number + gene position
  if(!is.null(gene.name)){
    # this is NCBI gene info not HapMap info.
    # coordinates can be different. It is better to use your own coordinates
    geneID <- as.vector(unlist(mget(gene.name, env=revmap(org.Hs.egSYMBOL))))
    x <- GetGeneInfo(geneID)
  }
  else{
    if(is.null(chr) | is.null(start) | is.null(stop)){
      stop("If no gene name is define then chr, start and stop are needed!")
    } else{
      x <- list(chr=chr, GeneLowPoint=start, GeneHighPoint=stop)
    }
  }
  # grab LD info from HapMap just to know which SNPs are in the gene
  f <- GetLDInfo(x$chr, x$GeneLowPoint, x$GeneHighPoint)
  subSNP <- unique(f$SNPA)
  # No LDdata available
  if(is.null(dim(f))){
    cat("No LD data available\n")
    return(f)
  }

  # Choose the file describing the chromosome where your SNPs are
  # To know it use:
  # GetSNPInfo("rs12345")$chr
  chrURL <- paste("ftp://ftp.ncbi.nlm.nih.gov/hapmap/genotypes/2010-08_phaseII+III/forward/genotypes_chr",x$chr,"_CEU_r28_nr.b36_fwd.txt.gz")
  hapmap <- read.HapMap.data(chrURL)

  # Usually you are interested in only a subset of the SNPs here: subSNP (vector)
  hapmap$snp.data@.Data <- hapmap$snp.data@.Data[,subSNP]
  ldinfo <- ld.snp(hapmap$snp.data, depth=dim(hapmap$snp.data)[2])
  plot(ldinfo, filename='ld_plot.eps')
}