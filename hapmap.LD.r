#
# hapmap.LD.r
#
# Created by David Ruau on 2011-04-23.  
# Department of Pediatrics/Div. Systems Medicine
# Stanford University.
# 
#
##################### USAGE #########################
# Input:
# geneName: Entrez gene ID [if provided subSNP is ignored]
# subSNP: [required if geneID is NULL] character vector; the rs SNP ID you want to 
#   plot otherwise try to plot the entire chromosome 
#   LD map (will fail)
# chr: integer; chromosome number
# hapmap.file: file location of HapMap chromosome file 
#   downloaded using readHapMap.data from snpMatrix 
#   package
# ...: further argument for ld.snp function from snpMatrix
#
# USAGE
# > library(org.Hs.eg.db)
# > (id <- as.vector(unlist(mget("POU5F1", org.Hs.egSYMBOL2EG))))
# > hapmap.LD(geneID=id)
# 
# output the heatmap into a EPS file
# 
#####################################################

hapmap.LD <- function(geneID=NULL, subSNP=NULL, chr=NULL, hapmap.file=NULL, ...) {
  require(snpMatrix)
  require(annotate)
  require(org.Hs.eg.db)
  require(foreach)
  require(doMC)
  require(multicore)
  ncore = multicore:::detectCores()
  registerDoMC(cores = ncore)
  
  if(is.null(subSNP) && is.null(geneID)){
    stop("subSNP and geneID are NULL. Selection of SNP to plot is required.\nAn entire chromosome cannot be plotted")
  }
  
  if(!is.null(geneID)){
    start <- mget(geneID, env=org.Hs.egCHRLOC)
    end <- mget(geneID, env=org.Hs.egCHRLOCEND)
    chr <- mget(geneID, env=org.Hs.egCHR)
    subSNP <- NULL
  }
  
  print('Getting LD info from HapMap')
  if(is.null(hapmap.file)){
    if(is.null(chr)){
      stop("Chromosome number has to be provided if no HapMap file is given")
    }
    hapmap.file <- paste("ftp://ftp.ncbi.nlm.nih.gov/hapmap/genotypes/2010-08_phaseII+III/forward/genotypes_chr",chr,"_CEU_r28_nr.b36_fwd.txt.gz", sep='')
  }
  else{
    hapmap.file <- paste("file://", hapmap.file, sep="")
    hapmap <- read.HapMap.data(hapmap.file)
  }
  
  
  if(is.null(subSNP)){
    xx <- hapmap$snp.support[as.numeric(as.vector(hapmap$snp.support$Position))>= start,]
    xx <- xx[as.numeric(as.vector(xx$Position)) <= end,]
    subSNP <- rownames(xx)
    idx.cols <- which(colnames(hapmap$snp.data@.Data) %in% subSNP)
    hapmap$snp.data@.Data <- hapmap$snp.data@.Data[,idx.cols]
  }
  else{
    idx.cols <- which(colnames(hapmap$snp.data@.Data) %in% subSNP)
    hapmap$snp.data@.Data <- hapmap$snp.data@.Data[,idx.cols]
  }
  
  # remove grey lines SNP with no info for the selected region
  idx <- foreach(i = 1:ncol(hapmap$snp.data@.Data), .combine=c) %dopar% {
    if(length(unique(as.numeric(hapmap$snp.data@.Data[,i]))) <= 2){
      i
    }
  }
  
  if(length(idx)>0){
    hapmap$snp.data@.Data <- hapmap$snp.data@.Data[,-idx]
  }
  print('a')
  ldinfo <- ld.snp(hapmap$snp.data, depth=dim(hapmap$snp.data)[2], ...)
  print('b')
  fName <- paste('ld_plot',".eps", sep='')
  plot.snp.dprime(ldinfo, start=a, end=b, filename=fName)
}