#
# GO_over.r
#
# Created by David Ruau on 2012-01-23.
# Dept. of Pediatrics/Div. Systems Medicine, Stanford University.
# 
#
##################### USAGE #########################
# 
# GO_over(...)
# 
#####################################################



GO_over <- function(universe, glist, annot='HsAgilentDesign026652.db', ontology='BP', cutoff=0.001) {
  require(GOstats)
  glist <- unique(as.character(glist))
  
  # conditional hypergeometric test
  # "uses the structure of the GO graph to estimate for each term whether or
  # not there is evidence beyond that which is provided by the term’s children
  # to call the term in question statistically overrepresented." - GOstatsHyperG vignette
  params <-  new("GOHyperGParams", 
  geneIds=glist,
  universeGeneIds=universe, # this is a gene list of all the genes tested on the microarray
  annotation=annot, # annotation pacakge for the microarray in question
  ontology=ontology, # either BP, CC, or MF
  pvalueCutoff=cutoff, # for uncorrected pvalues
  conditional=TRUE, # Use conditional algorithms or standard Hypergeometric test
  testDirection="over")
  
  mfhyper = hyperGTest(params)
  return(mfhyper)
}
