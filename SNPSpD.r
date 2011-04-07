## Method for computing the effective number of variable (SNPs) based on the 
# spectral decomposition (SpD) of matrices of pairwise LD between SNPs
# See Nyholt et al. 2004 (PMID:14997420)
library(snpMatrix)
library(NCBI2R)
library(annotate)

# Choose the file describing the chromosome where your SNP are
# To know it use:
GetSNPInfo("rs12345")$chr
 
chrURL <- "ftp://ftp.ncbi.nlm.nih.gov/hapmap/genotypes/2010-08_phaseII+III/forward/genotypes_chr8_CEU_r28_nr.b36_fwd.txt.gz"
chr <- read.HapMap.data(chrURL)

# Usually you are interested in only a subset of the SNPs here: subSNP (vector)
chr$snp.data@.Data <- chr$snp.data@.Data[,subSNP]
ldinfo <- ld.snp(chr$snp.data, depth=dim(chr$snp.data)[2])
plot(ldinfo, filename='ld_plot.eps')

# matrix massage
ldinfo$rsq2 <- ldinfo$rsq2[,dim(ldinfo$rsq2)[2]:1]
ldinfo$rsq2 <- cbind(0, ldinfo$rsq2)
ldinfo$rsq2 <- rbind(ldinfo$rsq2, 0)
ldinfo$rsq2 <- t(ldinfo$rsq2)
# fill diagonal with 1
for(i in 1:dim(ldinfo$rsq2)[1]){
  ldinfo$rsq2[i,i]=1
}

# EIGEN values extraction
e <- eigen(ldinfo$rsq2, symmetric=TRUE, only.values=TRUE)

# The effective number of variable (Meff)
Meff <- function(lambda){
  return(1 + (length(lambda) - 1)*(1 - (var(lambda)/length(lambda))))
}

Meff(e$values)

## final notes
# reduction are not huge so don't be surprise
# a few example 131 SNP were reduced to 122.4
# 16 -> 11.3; 51-> 38.7...
