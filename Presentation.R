### R code from vignette source 'Presentation.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: init
###################################################
options(width=60)


###################################################
### code chunk number 2: Introduction
###################################################
3 + 3
# This is a comment
x <- 3
x^2
x = c(1, 1, 2, 3, 5, 8)
x
x+10 # R is vectorized
x[4] # Accessing the fourth element


###################################################
### code chunk number 3: Introduction 2
###################################################
x[x>2]
1:7
x[1:3]
is.na(x)
sum(x)


###################################################
### code chunk number 4: help (eval = FALSE)
###################################################
help.start()


###################################################
### code chunk number 5: help (eval = FALSE)
###################################################
?plot


###################################################
### code chunk number 6: packages (eval = FALSE)
###################################################
# installing a package from CRAN
install.packages("foreign")
# installing the recommended packages from BioConductor
source("http://bioconductor.org/biocLite.R")
# installing a specific package from BioC
biocLite("ArrayExpress")


###################################################
### code chunk number 7: math (eval = FALSE)
###################################################
mean(x)
median(x)
sd(x)
sum(x)
sqrt(x)


###################################################
### code chunk number 8: math2
###################################################
summary(x)


###################################################
### code chunk number 9: FOR example
###################################################
for(i in 1:5){
	# do something
	print(i)
}


###################################################
### code chunk number 10: IF example
###################################################
i <- 1
if(i == 1){
  	print("i is equal 1")
} else{
	print("i is NOT equal to 1")
}


###################################################
### code chunk number 11: ifelse
###################################################
ifelse(i == 1, "i is equal 1", "i is NOT equal to 1")


###################################################
### code chunk number 12: foreach example
###################################################
library(foreach)
library(doMC)
library(multicore)
ncore = multicore:::detectCores()
registerDoMC(cores = ncore)

results <- foreach(i = 1:5, .combine=c) %dopar% {
	i+i
}
results


###################################################
### code chunk number 13: apply
###################################################
mat <- matrix(1:10, nrow=2, byrow=T)
mat
# summing the columns
apply(mat, 2, sum)
# summing the rows
apply(mat, 1, sum)


###################################################
### code chunk number 14: apply2 (eval = FALSE)
###################################################
## ?lapply
## ?tapply


###################################################
### code chunk number 15: simple function
###################################################
Mr.euclide <- function(x, y){
	dist <- sqrt(sum((x - y)^2))
	return(dist)
}


###################################################
### code chunk number 16: sourcing (eval = FALSE)
###################################################
## source('myscript.r')


###################################################
### code chunk number 17: Euclide distance
###################################################
x <- c(1, 1)
y <- c(2, 2)
## We execute our function like that:
Mr.euclide(x, y)


###################################################
### code chunk number 18: OOeuclide
###################################################
setClass("david", representation(x = "numeric"),	prototype = prototype(x = 0))
myA <- new("david", x=c(1, 1))
myA


###################################################
### code chunk number 19: OOeuclide (eval = FALSE)
###################################################
Mr.euclide <- function(x, y){
	if(class(x)!="david" & class(y)!="david") stop("error") 
	dist <- sqrt(sum((x - y)^2))
	return(dist)
}


###################################################
### code chunk number 20: vector
###################################################
## Vector of strings
x <- c("Lincoln", "Roosevelt", "Jackson")
## Replace the second element of the vector
x[2] <- NA
x[!is.na(x)]


###################################################
### code chunk number 21: matrix
###################################################
matrix(1:10, ncol=2)


###################################################
### code chunk number 22: matrix2
###################################################
matrix(1:10, nrow=2, byrow=TRUE)


###################################################
### code chunk number 23: list
###################################################
my.list <- list(
  fruits = c("oranges", "bananas", "apples"), 
  mat = matrix(1:10, nrow=2)
)
my.list


###################################################
### code chunk number 24: data.frame
###################################################
df <- data.frame(
  fruits = c("oranges", "bananas", "apples"), 
  color = c("orange", "yellow", "red"),
  quantity = c(2, 3, 1)
)
df


###################################################
### code chunk number 25: data.frame2
###################################################
attributes(df)
names(df)
## Accessing data
df$fruits


###################################################
### code chunk number 26: data.frame2
###################################################
a <- array(1:3, c(2,3,2)) # row, column, depth
a


###################################################
### code chunk number 27: import1
###################################################
URL <- "http://www.stanford.edu/~druau/pivot_table.csv"
pivot <- read.table(URL, sep=',', header=TRUE)
pivot$value <- round(pivot$value, digits=3)
head(pivot, 5)


###################################################
### code chunk number 28: format
###################################################
## Load the reshape library
library(reshape)
## reformatting the data with the cast function
## Here we specify that we want the table organize 
## with gene by condition
head(cast(pivot, gene ~ condition), 5)


###################################################
### code chunk number 29: format2
###################################################
pivot <- cast(pivot, gene ~ condition)


###################################################
### code chunk number 30: format3
###################################################
head(subset(pivot, cheetos>=0.2), 2)


###################################################
### code chunk number 31: format4 (eval = FALSE)
###################################################
library(sqldf)
head(sqldf('SELECT * FROM pivot WHERE cheetos >= 0.2'), 2)


###################################################
### code chunk number 32: database (eval = FALSE)
###################################################
require("RMySQL")
con <- dbConnect(MySQL(), user="druau", pasword="will_not_tell_you", dbname="db", host="mysql_server")
results <- dbGetQuery(con, "SELECT * from patients")


###################################################
### code chunk number 33: foreign (eval = FALSE)
###################################################
help(package="foreign")


###################################################
### code chunk number 34: microarray (eval = FALSE)
###################################################
library(affy)
library(GEOquery)
library(mouse4302cdf)
getGEOSuppFiles("GSE12499")
# Let's clean that up
system('tar -xf GSE12499/GSE12499_RAW.tar -C GSE12499/')
system('rm GSE12499/*.CHP*; rm GSE12499/*.tar')
# and import the data
da <- ReadAffy(celfile.path="./GSE12499/", compress=TRUE)


###################################################
### code chunk number 35: ArrayExpress (eval = FALSE)
###################################################
# Query ArrayExpress
pneumoHS = queryAE(keywords = "pneumonia", species = "homo+sapiens")
# download the data
EGEOD1724 <- getAE("E-GEOD-1724", type='processed')
cnames = getcolproc(EGEOD1724)
# generate an expression set
EGEOD1724.da <- procset(EGEOD1724, cnames[2])


###################################################
### code chunk number 36: export (eval = FALSE)
###################################################
save(list=c(pivot, mat), file="data_exoprt.Rda")
# and then
load("data_export.Rda")


###################################################
### code chunk number 37: export1 (eval = FALSE)
###################################################
write.table(mat, file='matrix.csv', sep=',')


###################################################
### code chunk number 38: baseScatterPlot (eval = FALSE)
###################################################
library(datasets)
data(cars)
pdf(file='scatterplot.pdf', height=7, width=7)
plot(cars$speed, cars$dist)
dev.off()


###################################################
### code chunk number 39: baseScatterPlot (eval = FALSE)
###################################################
pdf(file='scatterplot2.pdf', height=7, width=7)
plot(cars$speed, cars$dist)
fit <- lm(cars$dist ~ cars$speed)
abline(fit, col='red')
x <- summary(fit)
title(paste("Speed vs Dist r2=", round(x$r.squared, 2)))
dev.off()


###################################################
### code chunk number 40: baseBoxPlot (eval = FALSE)
###################################################
pdf(file='boxplot.pdf', height=7, width=7)
boxplot(len ~ dose * supp,  data=ToothGrowth)
dev.off()


###################################################
### code chunk number 41: baseHistogram (eval = FALSE)
###################################################
pdf(file='histo.pdf', height=7, width=7)
normal <- rnorm(1000, 1)
par(mfrow=c(2,1))
hist(normal, main="HISTOGRAM")
barplot(normal, main="BARPLOT")
dev.off()


###################################################
### code chunk number 42: welch (eval = FALSE)
###################################################
t.test(MYC ~ condition, data = experiment, alternative = "two.sided")


###################################################
### code chunk number 43: vartest (eval = FALSE)
###################################################
var.test(MYC ~ condition, data = experiment)


###################################################
### code chunk number 44: paired1
###################################################
library(ISwR)
attach(intake)


###################################################
### code chunk number 45: paired2
###################################################
t.test(pre, post, paired = T)


###################################################
### code chunk number 46: lm1 (eval = FALSE)
###################################################
fit <- lm(post ~ pre)
summary(fit)
plot(pre, post, xlim=c(3900, 9000), ylim=c(3900, 9000), cex=2)
abline(fit, col='red', lwd=2)


###################################################
### code chunk number 47: detach
###################################################
detach(intake)


###################################################
### code chunk number 48: false color (eval = FALSE)
###################################################
library(affyPLM) # if not already loaded
pset <- fitPLM(da)
# little function combining the 4 types fo image
img.Test <- function(batch,pset,x) {
	par(mfrow = c(2,2))
	image(batch[,x])
	image(pset, type = "weights", which = x)
	image(pset, type = "resids", which = x)
	image(pset, type = "sign.resids", which = x)
}
# execute the function for each microarray
for(n in 1:length(da)) {
		filename <- paste("QC",as.vector(sampleNames(da))[n],".png", sep="")
		png(file=filename, height=900, width=800)
		img.Test(da,pset,n)
		dev.off()
}


###################################################
### code chunk number 49: RLE (eval = FALSE)
###################################################
library(RColorBrewer)
cols <- brewer.pal(12, "Set3")
Mbox(pset, col = cols, main ="RLE (Relative Log Expression)",  xlab="Assuming that the majority of the gene are not changing\n Ideally these boxes would have small spread and be centered at M=0")


###################################################
### code chunk number 50: NUSE (eval = FALSE)
###################################################
boxplot(pset, col=cols, main= "NUSE (Normalized Unscaled Standard Error)", xlab="High values of median NUSE are indicative of a problematic array")


###################################################
### code chunk number 51: RNA_degradation (eval = FALSE)
###################################################
RNAdeg <- AffyRNAdeg(da)
plotAffyRNAdeg(RNAdeg, cols=cols)
legend("topleft", sampleNames(da), lty=1, col=cols)
box()


###################################################
### code chunk number 52: microarray_analysis1 (eval = FALSE)
###################################################
URL <- "http://www.stanford.edu/~druau/treatment.txt"
pd <- read.table(URL, sep='\t', header=TRUE, row.names=sampleNames(da))
pData(da) <- pd
sampleNames(da) <- pd[,1]
da.rma <- rma(da) # pre-processing


###################################################
### code chunk number 53: micro_analysis2
###################################################
library(affy)
class(da.rma)
da.e <- exprs(da.rma)
class(da.e)
dim(da.e)


###################################################
### code chunk number 54: RP (eval = FALSE)
###################################################
library(RankProd)
cl <- c(rep(0,3), rep(1,4))
da.rp <- RP(da.e[,c(1:34:10)], cl=cl, logged=TRUE, num.perm=100, plot=FALSE, rand=5432)


###################################################
### code chunk number 55: annot_package
###################################################
## ANNOTATION PACKAGE
# necessary step to obtain annotated gene lists.
library(mouse4302.db)
gnames <- as.vector(unlist(as.list(mouse4302SYMBOL)))


###################################################
### code chunk number 56: genes regulated
###################################################
r.nsc.1f_nsc <- topGene(da.rp, cutoff = 0.001, method = "pfp", logged = TRUE, logbase = 2, gene.names=gnames)


###################################################
### code chunk number 57: GeneLists (eval = FALSE)
###################################################
# The genes significantly up-regulated
head(r.nsc.1f_nsc$Table1, 5)
# how many genes are in table 2
dim(r.nsc.1f_nsc$Table1)

# The genes significantly down-regulated
head(r.nsc.1f_nsc$Table2, 5)
# how many genes are in table 2
dim(r.nsc.1f_nsc$Table2)


###################################################
### code chunk number 58: hclust (eval = FALSE)
###################################################
library(bioDist)
d <- cor.dist(t(da.e)) # transpose your matrix
## dendrogram
hc = hclust(d, method = "average")
plot(hc, labels = colnames(da.e), main = "Hier. clust. Pearson", hang=-1)


###################################################
### code chunk number 59: hierarchicalClustering1
###################################################
library(RColorBrewer)
hmcol = colorRampPalette(brewer.pal(10, "RdBu"))(256)
library(bioDist)
d <- cor.dist(t(da.e))
library(gplots)
heatmap.2(as.matrix(d), 
  distfun=function(x){as.dist(x)}, 
  hclustfun=function(m){hclust(m, method="average")},
  symm=F, col=hmcol, trace='none', notecol='black', 
  denscol='black', notecex=0.8, dendrogram="column")


###################################################
### code chunk number 60: hclustplot
###################################################
library(RColorBrewer)
hmcol = colorRampPalette(brewer.pal(10, "RdBu"))(256)
library(bioDist)
d <- cor.dist(t(da.e))
library(gplots)
heatmap.2(as.matrix(d), 
  distfun=function(x){as.dist(x)}, 
  hclustfun=function(m){hclust(m, method="average")},
  symm=F, col=hmcol, trace='none', notecol='black', 
  denscol='black', notecex=0.8, dendrogram="column")


###################################################
### code chunk number 61: VolcanoPlot (eval = FALSE)
###################################################
# We need to compute the t-values for plotting the volcano plot
# Rank Product does not produce the t-value
# we use the simpleaffy package
library(simpleaffy)
results <- pairwise.comparison(da.rma , "cell_type", c("NSC", "NSC_1F"), spots = da)
plot(-{da.rp$AveFC}, -log(slot(results, "tt")))
## combine the list of up- and down-regulated genes
g.list <- c(rownames(r.nsc.1f_nsc$Table1), rownames(r.nsc.1f_nsc$Table2))
x <- -{da.rp$AveFC[g.list, ]}
y <- -{log(slot(results, "tt")[g.list])}
z <- getSYMBOL(g.list, "mouse4302")
## display the gene significant;y regulated
points(x, y, col = "green", pch = 19)


###################################################
### code chunk number 62: ScatterPlot (eval = FALSE)
###################################################
plot(slot(results, "means"),
  xlim = c(0, 15), ylim=c(0, 15), 
  xlab = "NSC", ylab = "NSC_1F", 
  main = "Gene differentially expressed between NSC and NSC_1F")
x <- slot(results, "means")[g.list, "NSC"]
y <- slot(results, "means")[g.list, "NSC_1F"]
z <- getSYMBOL(g.list, "mouse4302")
points(x, y, col = "green", pch = 19)
abline(2, 1, col='red')
abline(-2, 1, col='red')


###################################################
### code chunk number 63: Gene_List_comparison (eval = FALSE)
###################################################
# iPS cells compared to NSC
cl <- c(rep(0,3), rep(1,4))
da.rp <- RP(da.e[,4:10], cl=cl, logged=TRUE, num.perm=100, plot=FALSE, rand=5432)
r.nsc.1f_nsc <- topGene(da.rp, cutoff = 0.001, method = "pfp", logged = TRUE, logbase = 2, gene.names=gnames)
geneList_A <- rownames(r.nsc.1f_nsc$Table2)
geneList_A <- geneList_A[!is.na(geneList_A)]
# iPS cells compared to NSC_1F_iPS cells
cl <- c(rep(0,3), rep(1,3))
da.rp <- RP(da.e[,c(4:6, 1:3)], cl=cl, logged=TRUE, num.perm=100, plot=FALSE, rand=5432)
r.1f_nsc.nsc_1f <- topGene(da.rp, cutoff = 0.001, method = "pfp", logged = TRUE, logbase = 2, gene.names=gnames)
geneList_B <- rownames(r.1f_nsc.nsc_1f$Table1)
geneList_B <- geneList_B[!is.na(geneList_B)]


###################################################
### code chunk number 64: VennDiagram (eval = FALSE)
###################################################
library(Vennerable)
vv <- list(nsc_to_1f_nsc_=geneList_A, X1f_nsc_to_nsc_1f=geneList_B)
vv <- Venn(vv)
plot(vv)
intersect(geneList_B, geneList_A)


###################################################
### code chunk number 65: DEgenes (eval = FALSE)
###################################################
r.nsc.nsc_1f <- topGene(da.rp, cutoff = 0.001, method = "pfp", logged = TRUE, logbase = 2, gene.names= rownames(da.rp$AveFC))
## differentially regulated genes
g.list <- c(rownames(r.nsc.nsc_1f$Table1), rownames(r.nsc.nsc_1f$Table2))


###################################################
### code chunk number 66: PAM (eval = FALSE)
###################################################
library(bioDist)
sub.da <- da.e[g.list,]
d <- cor.dist(sub.da)
### K-means ###
# Cluster the genes for k = 3
g.kmeans <- kmeans(d, centers = 3, iter.max=1000)


###################################################
### code chunk number 67: GO analysis (eval = FALSE)
###################################################
library(GOstats)
library(mouse4302.db)
uniqueId <- mouse4302ENTREZID
entrezUniverse <- unique(as.character(uniqueId))
ourlist <- mouse4302ENTREZID[g.list]
ourlist <- unique(as.character(ourlist))
# creating the GOHyperGParams object
params = new("GOHyperGParams", geneIds=ourlist, 
universeGeneIds=entrezUniverse, annotation='mouse4302.db', 
ontology="BP", pvalueCutoff=0.001, conditional=FALSE, testDirection="over")
# running the test
mfhyper = hyperGTest(params)


###################################################
### code chunk number 68: GO analysis 2
###################################################
mfhyper
head(summary(mfhyper), 2)


###################################################
### code chunk number 69: GO analysis 3 (eval = FALSE)
###################################################
# grabbing detail of a GO category
GOTERM[["GO:0032502"]]
# Information on the Directed Acyclic Graph (DAG)
goDag(mfhyper)
# how many gene were mapped in the end?
geneMappedCount(mfhyper)
# how many genes are in the universe
universeMappedCount(mfhyper)
# html output
htmlReport(mfhyper, file="BP_list_significant.html")


###################################################
### code chunk number 70: GO graph with label (eval = FALSE)
###################################################
library(Rgraphviz)
g1 <- GOGraph(head(summary(mfhyper))$GOBPID, GOBPPARENTS)
# grabbing the label of the nodes
my.labels <- vector()
for(i in 1:length(slot(g1, "nodes"))){
  my.labels[i] <- Term(get(slot(g1, "nodes")[[i]], GOTERM))
}
my.labels
# determining the nodes attributes and associating the node labels
nodattr <- makeNodeAttrs(g1, label=my.labels, 
  shape = "ellipse", fillcolor = "#f2f2f2", fixedsize = FALSE)
# plotting the graph
plot(g1, nodeAttrs = nodattr)


###################################################
### code chunk number 71: KEGG GO (eval = FALSE)
###################################################
library(GO.db)
xx <- as.list(GOTERM)
GObp <- unlist(lapply(xx, function(x){ifelse(Ontology(x)=="BP", TRUE, FALSE)}))
GObp <- GObp[GObp]

library(org.Hs.eg.db)
GO <- as.list(org.Hs.egGO2EG)
# filter for GO biological Processes only
GO <- GO[names(GObp)]
GO <- GO[!is.na(names(GO))]
go2eg <- lapply(GO, function(x){as.vector(unlist(x))})
length(go2eg)
#go2eg <- go2eg[1:500]
KEGG <- as.list(org.Hs.egPATH2EG)
kegg2eg <- lapply(KEGG, function(x){as.vector(unlist(x))})
length(kegg2eg)

go2kegg <- matrix(nrow=length(go2eg), ncol=length(kegg2eg))
for(i in 1:length(go2eg)) {
	for(j in 1:length(kegg2eg)) {
		go2kegg[i,j] <- length(intersect(go2eg[[i]], kegg2eg[[j]]))
	}
}
rownames(go2kegg) <- names(go2eg)
colnames(go2kegg) <- names(kegg2eg)
summary(rowSums(go2kegg))
summary(colSums(go2kegg))
X <- go2kegg
X <- X[-which(rowSums(X) <= 80),]
X <- X[,-which(colSums(X) <= 100)]
dim(X)

colnames(X) <- as.vector(unlist(mget(colnames(X), KEGGPATHID2NAME)))
rownames(X) <- apply(as.matrix(rownames(X)), 1, FUN=function(x){Term(get(x, GOTERM))})
cols <- colorRampPalette(c("white", "darkblue"))(256)
heatmap(X, col=cols, margins = c(20, 20), dist=function(x){cor.dist(x)})
mtext("KEGG", 1, line=6, cex=3)
mtext("GO", 4, line=6, cex=3, at=50)


###################################################
### code chunk number 72: linear regression (eval = FALSE)
###################################################
fit <- lm(rs121212 ~ disease_yn + sex + age, data = snpMATRIX)
summary(fit)
library(multtest)
mt.rawp2adjp(raw.p-value)


###################################################
### code chunk number 73: snpBoxPlot (eval = FALSE)
###################################################
boxplot(TRAIT ~ sex * rs229922, data=SNPdata, xlab="Genotype by sex", ylab="TRAIT", main='SNP vs. trait', col=c('white', 'grey'))


###################################################
### code chunk number 74: googleVis_earthquakes (eval = FALSE)
###################################################
## Load the library
library(googleVis)
library(XML)
## Set an option to have the data NOT transform to factor 
## when they are imported into a data.frame
options(stringsAsFactors = FALSE)
## Get earthquake data of the last 30 days from the IRIS website
eq=readHTMLTable("http://www.iris.edu/seismon/last30.html")
## eq is a list() object with 2 elements
## Extract the earthquake data.frame
eq <- eq[[2]] 
## We look for Japan earthquakes using the grep function.
## Look at ?grep for more info
jp <- grep("*japan*", eq$REGION, ignore.case=T)
## Filter the original data.frame for the Japanese earthquakes
eq <- eq[jp,]
## Build the location information in the right format
## googleVis expect LAT:LONG as a string
## look at ?paste for the detail of this step
eq$loc <- paste(eq$LAT, eq$LON, sep=":")
## Plot the googleVis graph
M <- gvisGeoMap(eq, locationvar="loc", numvar="MAG", hovervar="DATE",  options=list(region="JP"))
plot(M)


