library(snpMatrix)
library(NCBI2R)
library(annotate)

# Choose the file describing the chromosome where your SNP are
# To know it use:
GetSNPInfo("rs12345")$chr
 
chrURL <- "ftp://ftp.ncbi.nlm.nih.gov/hapmap/genotypes/2010-08_phaseII+III/forward/genotypes_chr8_CEU_r28_nr.b36_fwd.txt.gz"
chr8 <- read.HapMap.data(chrURL)

# Nyholt et al. Meff
Meff <- function(lambda){
  return(1 + (length(lambda) - 1)*(1 - (var(lambda)/length(lambda))))
}

chr8$snp.data@.Data <- chr8$snp.data@.Data[,subSNP]
ldinfo <- ld.snp(chr8$snp.data, depth=dim(chr8$snp.data)[2])
plot(ldinfo, filename='NCALD.eps')

ldinfo$rsq2 <- ldinfo$rsq2[,dim(ldinfo$rsq2)[2]:1]
ldinfo$rsq2 <- cbind(0, ldinfo$rsq2)
ldinfo$rsq2 <- rbind(ldinfo$rsq2, 0)
ldinfo$rsq2 <- t(ldinfo$rsq2)
# fill diagonal with 1
for(i in 1:dim(ldinfo$rsq2)[1]){
  ldinfo$rsq2[i,i]=1
}

# sanity check
# head(ldinfo$rsq2[,1:5])

e <- eigen(ldinfo$rsq2, symmetric=TRUE, only.values=TRUE)

Meff(e$values)
## final notes
# reduction are not huge so don't be surprise
# I had 131 SNP and they were reduced to 122.4
