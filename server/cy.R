library(plyr)
library(RCy3)
library(gplots)
library(igraph)

localpath = paste(getwd(), '/', sep='')
objectkey = read.table(paste(localpath, "DataObjectKey.txt", sep=""), sep="\t", stringsAsFactors=FALSE)
objectkey = objectkey[,c(1,3)]
names(objectkey) = c("data.object", "description")

load(file=paste(localpath, "LC_TMT_Nets.RData", sep=""))
load(file=paste(localpath, "LC_TMT_Functions.RData", sep=""))
load(file="protein_families.RData")

source("MGRCyFunctions.R")
source("MGFunctions.R")
source("pepnetfunctions.R")

buildBromoNetwork <- function() {
  if(is.na(getWindowID(cy, "BromoDomainTemplate"))){
    bromodomainPs <- c("ASH1L", "ATAD2", "ATAD2B", "BAZ1A", "BAZ1B", "BAZ2A", "BAZ2B", "BPTF", "BRD1", "BRD2", "BRD3", "BRD4", "BRD7", "BRD8", "BRD9", "BRDT", "BRPF1", "BRPF3", "BRWD1", "BRWD3", "CECR2", "CREBBP", "EP300", "KAT2A", "KAT2B", "KIAA2026", "KMT2A", "PBRM1", "PHIP", "SMARCA2", "SMARCA4", "SP110", "SP140", "SP140L", "TAF1", "TAF1L", "TRIM24", "TRIM28", "TRIM33", "TRIM66", "ZMYND11", "ZMYND8")
    bromos <- func.key[grep("BROMO", func.key$Domains), "Gene.Name"]
    bromoPs <- sort(unique(c(bromos, bromodomainPs)))
    
    # Identify interactions between dually modified proteins and acetyl-binding proteins
    dualbromo.net <- filter.edges.between(bromoPs, dualpack.neg.genes, essl.cfn)
    # Identify interactions between dually modified proteins and kinases, phosphatases
    dualkin.net <- filter.edges.between(c(kins, phosphatases), dualpack.neg.genes, essl.cfn)
    # Identify interactions between dually modified proteins and acetyltransferases, deacetylases
    dualacetyl.net <- filter.edges.between(c(acetyltransferases, deacetylases), dualpack.neg.genes, essl.cfn)
    # Identify interactions between dually modified  and proteins that bind phosphorylated tyrosine, serine, threonine
    dualp_binding.net <- filter.edges.between(c(ptbPs, sh2list, pSbindingPs), dualpack.neg.genes, essl.cfn)
    # This latter one is 312 edges. We will keep it simpler by not including these edges initially.
    # Put the first three together to test the hypothesis that ki all these to look at nodes that interact
    dual.net <- unique(rbind(dualbromo.net, dualacetyl.net, dualkin.net)) # 271 edges
    # Simplify edges using mergeEdges
    dual.net2 <- mergeEdges.dir(dual.net)  # now 198 edges
    # Option: simplify further by removing interactions that are only genetic interactions, which may be indirect
    dual.net3 <- dual.net2[-which(dual.net2$edgeType=="Genetic interactions"),]   # now 146 edges
    # Note: we could have started with the CFN without genetic interactions (essl.cfn.nogen) but this way the genetic interactions are shown only along with other evidence for interactions.
    # How many nodes are in this network?
    # extract.gene.names(dual.net2) #$ 161
    # extract.gene.names(dual.net3) #$ 122
    #
    
    dualbromkin.cf <- make.anynetcf(edge.df=dual.net2, data.file=ldgene.fc,  geneatts=essl.netatts, ptmcccnatts=essl.cccn.edges, func.key=func.key)
    dualbromkin.g <- cyPlot(dualbromkin.cf, dual.net2)
    
    BROMO_WINDOW <<- CytoscapeWindow("BromoDomainTemplate", dualbromkin.g)
    displayGraph(BROMO_WINDOW)
    layoutNetwork(BROMO_WINDOW, "genemania-force-directed")
  } else {
    BROMO_WINDOW <<- existing.CytoscapeWindow("BromoDomainTemplate")
  }
  
  return(getWindowID(cy, "BromoDomainTemplate"))
}
