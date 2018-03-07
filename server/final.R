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

cy <- CytoscapeConnection()
ALL_NODES <- c()

for(gene in c(essl.cfn["Gene.1"], essl.cfn["Gene.2"])){
    ALL_NODES <- c(ALL_NODES, gene)
}

ALL_NODES <- unique(ALL_NODES)
BROMO_WINDOW <- NULL

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
    edgeDprops(BROMO_WINDOW)
    ratioprops(BROMO_WINDOW, dualbromkin.cf, "Total")
    #netattProps3(BROMO_WINDOW, dualbromkin.cf, use="ppi")
    showGraphicsDetails(BROMO_WINDOW, TRUE)
    nodeDprops.new(BROMO_WINDOW, dualbromkin.cf)
    setEdgeWidths.log(BROMO_WINDOW, factor=1.2)

    # First store the existing parameters so they can be easily restored.
    gm.layout.values <- getLayoutPropertyValue(cy, "genemania-force-directed", getLayoutPropertyNames(cy, "genemania-force-directed"))
    # The following parameters were determined by experimentation.
    setLayoutProperties(cy, "genemania-force-directed", list(numIterations=100, defaultSpringCoefficient=0.1, defaultSpringLength=50, minNodeMass=0.001, maxNodeMass=2000, midpointEdges=250, curveSteepness=7.0e-03, isDeterministic=1, singlePartition=0, ignoreHiddenElements=1))
    layoutNetwork(BROMO_WINDOW, layout.name= "genemania-force-directed")
    # This looks better.
  } else {
    BROMO_WINDOW <<- existing.CytoscapeWindow("BromoDomainTemplate")
  }
}

new.window.from.edges <- function(edges, windowName) {
  all.fc <- rbind(ldgene.fc, ld.fc)
  cf <- make.anynetcf(edge.df=edges, data.file=all.fc,  geneatts=essl.netatts, ptmcccnatts=essl.cccn.edges, func.key=func.key)
  plot <- cyPlot(cf, edges)

  window <- CytoscapeWindow(windowName, plot)
  displayGraph(window)
  #layoutNetwork(window, "genemania-force-directed")
  #edgeDprops(window)
  #ratioprops(window, cf, "Total")
  ##netattProps3(window, cf, use="ppi")
  #showGraphicsDetails(window, TRUE)
  #nodeDprops.new(window, cf)
  #setEdgeWidths.log(window, factor=1.2)

  # First store the existing parameters so they can be easily restored.
  gm.layout.values <- getLayoutPropertyValue(cy, "genemania-force-directed", getLayoutPropertyNames(cy, "genemania-force-directed"))
  # The following parameters were determined by experimentation.
  setLayoutProperties(cy, "genemania-force-directed", list(numIterations=100, defaultSpringCoefficient=0.1, defaultSpringLength=50, minNodeMass=0.001, maxNodeMass=2000, midpointEdges=250, curveSteepness=7.0e-03, isDeterministic=1, singlePartition=0, ignoreHiddenElements=1))
  layoutNetwork(window, layout.name="genemania-force-directed")
  # This looks better.
  newWindowID <- getWindowID(cy, windowName)
  return(as.integer(newWindowID))
}


#------------Python Facing Functions-------------#

generate.custom.graph <- function(nodeVector) {
  cyWindowHandle <- toString(Sys.time())
  netEdges       <- filter.edges.between(nodeVector, dualpack.neg.genes, essl.cfn)
  netEdges.cf    <- make.anynetcf(edge.df=netEdges, data.file=ldgene.fc,  geneatts=essl.netatts, ptmcccnatts=essl.cccn.edges, func.key=func.key)
  netEdges.g     <- cyPlot(netEdges.cf, netEdges)

  newWindow <- CytoscapeWindow(cyWindowHandle, netEdges.g)
  displayGraph(newWindow)
  # The following parameters were determined by experimentation.
  setLayoutProperties(cy, "genemania-force-directed", list(numIterations=100, defaultSpringCoefficient=0.1, defaultSpringLength=50, minNodeMass=0.001, maxNodeMass=2000, midpointEdges=250, curveSteepness=7.0e-03, isDeterministic=1, singlePartition=0, ignoreHiddenElements=1))
  layoutNetwork(newWindow, layout.name= "genemania-force-directed")
  # This looks better.

  newWindowID <- getWindowID(cy, cyWindowHandle)
  return(as.integer(newWindowID))
}

generate.with.filter.between <- function(nodeVec1, nodeVec2, windowName) {
  networkEdges <- filter.edges.between(nodeVec1, nodeVec2, essl.cfn)
  return(new.window.from.edges(networkEdges, windowName))
}

graphNetworkPath3.wrapper <- function(nodeVector, columns=crizexpts.nc, windowName) {
  path.nodeslist <- c()
  
  # Iterate through all 
  for(i in 1:(length(nodeVector)-1)){
    for(j in (i+1):length(nodeVector)){
      try({
      sp <- all_shortest_paths(graph= essl.cfn.ig, from=nodeVector[i], to=nodeVector[j], mode="all")
      path.nodeslist <- c(path.nodeslist, unique(lapply(sp[[1]], names)))
      }, TRUE)
    }
  }
  
  unique(path.nodeslist)
  edges.list <- lapply(path.nodeslist, filter.edges.0, edge.file=essl.cfn)
  connected.path <- unique(remove.autophos(ldply(edges.list)))
  
  #connected.path <- connectNodes.all(nodeVector, essl.cfn.ig, essl.cfn)
  networkObjects <- graphNetworkPath3(nodenames=nodeVector, path.edges=connected.path, ptmedgefile=essl.cccn.edges, datacolumns=columns, geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts)

  window <- CytoscapeWindow(windowName, networkObjects$gPlot)
  displayGraph(window)
  layoutNetwork(window, "genemania-force-directed")
  #setEdgeLineWidthRule(net.w, edge.attribute.name="Weight", attribute.values=net.edges$Weight, line.widths=net.edges$Weight)
  #edgeDprops(window)
  #ratioprops(window, networkObjects$net.cf, "Total")
  #showGraphicsDetails(window, TRUE)
  #nodeDprops.new(window, networkObjects$net.cf)
  #setEdgeWidths.log(window, factor=1.2)

  newWindowID <- getWindowID(cy, windowName)
  return(as.integer(newWindowID))
}

connectNodes.all.wrapper <- function(nodeVector, windowName) {
  allEdges <- rbind(essl.cfn, subset(essl.cccn.edges, select=-Alt.Weight ))
  networkEdges <- connectNodes.all(nodeVector, essl.cfn.ig, edgefile=allEdges)
  return(new.window.from.edges(networkEdges, windowName))
}

# Filter edges 0 wrapper
filterEdges0.wrapper <- function(nodeVector, windowName) {
  networkEdges <- filter.edges.0(nodeVector, essl.cfn)
  return(new.window.from.edges(networkEdges, windowName))
}

# Filter edges wrapper
filterEdges1.wrapper <- function(nodeVector, windowName) {
  networkEdges <- filter.edges.1(nodeVector, essl.cfn)
  return(new.window.from.edges(networkEdges, windowName))
}

# Filter edges CCCN wrapper
filterEdges1cccn.wrapper <- function(nodeVector, windowName, datacolumns=crizexpts.nc) {

  # Our selected gene nodes are stored here
  # Lets find any edges between the nodes
  path.nodes <- nodeVector

  # EGFR has an edge that points to itself, which is why this condition is nessesary
  if (length(path.nodes) > 1) {
    path.edges <-	filter.edges.0(path.nodes, essl.cfn)
  }
  else {
    # No edges for you
    path.edges <- NA
  }

  # Now lets do the same for the PTM CFFN
  netpeps <- unlist(sapply(path.nodes, extract.peptides, essl.cccn.edges))
  pepnodes.df <- data.frame(netpeps, pep.nodes=(sapply(netpeps, function(x) unlist(strsplit(x, " "))[1])))
  netpeps <- pepnodes.df[pepnodes.df$pep.nodes %in% path.nodes, 1]
  ptm.cccn <-	filter.edges.0(netpeps, essl.cccn.edges)


  # If there were any edges, lets plot them!
  # If not, we'll just plot the nodes
  if (!is.na(path.edges)) {
    net.full <- mergeEdges(path.edges)
    net.full$Alt.Weight <- net.full$Weight
    net.gene.cf <- make.anynetcf(edge.df=net.full, data.file=ldgene.fc[, datacolumns], geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max"))
  }
  else {
  net.gene.cf <- make.anynetcf.nodes(cf.nodes=path.nodes, data.file=ldgene.fc[, datacolumns], geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max"))
  }

  # Exactly the same for the PTM CCCN
  if (!is.na(ptm.cccn)) {
    net.pep.cf <- make.anynetcf(edge.df=ptm.cccn, data.file=ld.fc[, datacolumns], geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts, func.key=func.key, use=c("total", "mean", "median"))

  }
  else {
    print(nodeVector)
    print(netpeps)
    nP <- c(netpeps)
    net.pep.cf <-  make.anynetcf.nodes(cf.nodes=nP, data.file=ld.fc[, datacolumns], geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max"))

  }
  # combine node attribute cytoscape file

  net.cf <- harmonize_cfs3(pepcf=net.pep.cf, genecf=net.gene.cf)


  # make gene-peptide edges
  net.gpe <- data.frame(Gene.1=net.pep.cf$Gene.Name, Gene.2=net.pep.cf$Peptide.Name, edgeType="peptide", Weight=100, Alt.Weight=1)

  # bind only if they exist!
  if (!is.na(path.edges) && !is.na(ptm.cccn)) {
    net.full <- net.full[, c("Gene.1", "Gene.2", "Weight", "edgeType", "Alt.Weight")]
    net.edges <- rbind(net.gpe, ptm.cccn, net.full)
  }
  else if (!is.na(ptm.cccn)) {
    net.edges <- rbind(net.gpe, ptm.cccn)
  }
  else if (!is.na(path.edges)) {
    net.full <- net.full[, c("Gene.1", "Gene.2", "Weight", "edgeType", "Alt.Weight")]
    net.edges <- rbind(net.gpe, net.full)
  }
  else {
    net.edges <- net.gpe
  }

  # combine edge files
  print("We made it here5x")
  # Graph in RCy3
  net.g <- cyPlot(net.cf, net.edges)
  print("We made it here6x")
  networkObjects <- list(gPlot=net.g, net.cf=net.cf)

  window <- CytoscapeWindow(windowName, net.g)
  displayGraph(window)
  print("We made it here7x")
  layoutNetwork(window, "genemania-force-directed")

  newWindowID <- getWindowID(cy, windowName)
  return(as.integer(newWindowID))
}

update_data_column <- function(edge.dataframe, dataColumn) {
  gene_weights <- list()
  jsonString <- "["
  use_gene_name <- FALSE
  
  net.gene.cf = 0;
  net.pep.cf = 0;
  
  try({
    net.gene.cf <- make.anynetcf(edge.df=edge.dataframe, data.file=ldgene.fc[,dataColumn], geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts, func.key=func.key)
  }, silent=TRUE)
  
  try({
    net.pep.cf <- make.anynetcf(edge.df=edge.dataframe, data.file=ld.fc[,dataColumn], geneatts=essl.netatts, ptmcccnatts=essl.ptmcccnatts, func.key=func.key, use=c("total", "mean", "median"))
  }, silent=TRUE)
  
  if(net.gene.cf == 0) {
    cf2 <- net.pep.cf
  }else if(net.pep.cf == 0){
    cf2 <- net.gene.cf
    use_gene_name <- TRUE
  }else{
    cf2 <- harmonize_cfs3(pepcf=net.pep.cf, genecf=net.gene.cf)
  }
  
  nameParameter <- if(use_gene_name) "Gene.Name" else "Node"
  
  for(i in 1:lengths(cf2)[1]){
    name <- cf2[i, nameParameter]
    weight <- cf2[i,"Total"]
    commaChar <- ","
    #gene_weights[[name]] <- weight
    if (i == lengths(cf2)[1]) {
      commaChar <- ""
    }
    hashString <- sprintf("{\"%s\":\"%s\"}%s", name, weight, commaChar)
    jsonString <- paste(jsonString, hashString)
  }

  jsonString <- paste(jsonString, "]")

  return(jsonString)
}

#------------------------------------------------#


#-------------Grimes' Functions-------------#

filter.edges.between <- function(nodes1, nodes2, edge.file) {
  sel.edges1 <- edge.file[edge.file$Gene.1 %in% nodes1 & edge.file$Gene.2%in% nodes2,]
  sel.edges2 <- edge.file[edge.file$Gene.1 %in% nodes2 & edge.file$Gene.2%in% nodes1,]
  sel.edges <- rbind(sel.edges1, sel.edges2)
  if(dim(sel.edges)[1] == 0){
    return(NA)
  } else {
    return(sel.edges)
  }
}

# connectNodes.all  uses all_shortest_paths and returns just the edge file
connectNodes.all <- function(nodeVector, ig.graph=NULL, edgefile, newgraph=FALSE)	{
  if (newgraph==TRUE) {
    ig.graph <- graph.data.frame(edgefile, directed=FALSE) 
  }
  
  path.nodeslist <- c()
  print(nodeVector)
  for(i in 1:(length(nodeVector)-1)){
    for(j in (i+1):length(nodeVector)){
      try({
        sp <- all_shortest_paths(graph= ig.graph, from=nodeVector[i], to=nodeVector[j], mode="all")
        path.nodeslist <- c(path.nodeslist, (unique(lapply(sp[[1]], names))))
      }, TRUE)
    }
  }
  
  #sp <- all_shortest_paths(graph= ig.graph, from=nodeVector[1], to=nodeVector[2], mode="all")
  #path.nodeslist <-  unique(lapply(sp[[1]], names))
  unique(path.nodeslist)
  edges.list <- lapply(path.nodeslist, filter.edges.0, edge.file=edgefile)
  path.edges <- unique(remove.autophos(ldply(edges.list)))
  return(path.edges)
}

# funcion to filter networks to include only selected nodes and those with edges to them
filter.edges.0 <- function(nodenames, edge.file) {
  nodenames <-as.character(nodenames)
  a = as.character(edge.file$Gene.1)
  b = as.character(edge.file$Gene.2)
  edgefile.nodes <- unique(c(a,b))
  flub <- setdiff(edgefile.nodes, nodenames) 
  # show pruned nodes (turned off)
  # if (length(flub) >= 1) { 
  # cat("\n","\t", "The following GM names do not match ","\n","\t", flub) }	
  sel.edges <- edge.file[edge.file$Gene.1 %in% nodenames & edge.file$Gene.2%in% nodenames,]
  if(dim(sel.edges)[1] == 0) {return(NA)} else return(sel.edges) 
}

# graphNetworkPath3 function, modified to return list of relevant data
# original function instantiated cytoscape window, and returned unhelpful data for this context.
graphNetworkPath3 <- function(nodenames, path.edges, ptmedgefile, datacolumns=names(ld.fc), geneatts, ptmcccnatts)	{
  path.nodes <- unique(c(as.character(path.edges$Gene.1), as.character(path.edges$Gene.2)))
  # Get peptides from this network
  netpeps <- unlist(sapply(path.nodes, extract.peptides, ptmedgefile))
  pepnodes.df <- data.frame(netpeps, pep.nodes=(sapply(netpeps, function(x) unlist(strsplit(x, " "))[1])))
  netpeps <- pepnodes.df[pepnodes.df$pep.nodes %in% path.nodes, 1]
  ptm.cccn <-	filter.edges.0(netpeps, ptmedgefile)
  #nf <- list()
  #for (i in 1:(length(path.nodes)-1)) {
  #     nf[[i]] <- filter.edges.0(c(path.nodes[i], path.nodes[i+1]), ppinetwork)
  #}
  net.full <- mergeEdges(path.edges)
  net.full$Alt.Weight <- net.full$Weight
  net.gene.cf <- make.anynetcf(edge.df=net.full, data.file=ldgene.fc[, datacolumns], geneatts=geneatts, ptmcccnatts=ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max"))
  net.pep.cf <- make.anynetcf(edge.df=ptm.cccn, data.file=ld.fc[, datacolumns], geneatts=geneatts, ptmcccnatts=ptmcccnatts, func.key=func.key, use=c("total", "mean", "median"))
  # combine node attribute cytoscape file
  net.cf <- harmonize_cfs3(pepcf=net.pep.cf, genecf=net.gene.cf)
  # make gene-peptide edges
  net.gpe <- data.frame(Gene.1=net.pep.cf$Gene.Name, Gene.2=net.pep.cf$Peptide.Name, edgeType="peptide", Weight=100, Alt.Weight=1)
  # net.gpe <- genepep.edges(ptm.cccn)
  # combine edge files
  net.full <- net.full[, c("Gene.1", "Gene.2", "Weight", "edgeType", "Alt.Weight")]
  net.edges <- rbind(net.gpe, ptm.cccn, net.full)
  # Graph in RCy3
  net.g <- cyPlot(net.cf, net.edges)
  return( list(gPlot=net.g, net.cf=net.cf) )
}

# Make.anynet, except now it takes cf.nodes as a variable instead of an edge file
make.anynetcf.nodes <- function(cf.nodes, data.file, geneatts, ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max")) {
     cf.data <- data.frame(data.file[rownames(data.file) %in% cf.nodes,])
     if (any(grepl("Total", names(cf.data)))) {
          cf.data <- cf.data[, -grep("Total",names(cf.data))] }
     cf <- data.frame(Peptide.Name=rownames(cf.data))
     cf$Peptide.Name <- as.character(cf$Peptide.Name)
     cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
     if(identical(cf$Peptide.Name, cf$Gene.Name)) {genenodes=TRUE} else {genenodes=FALSE}
     # node classification:
     cf.functions <- func.key[func.key$Gene.Name %in% cf$Gene.Name,]
     cf <- plyr::join(cf, cf.functions, by="Gene.Name", type="left")
     # Fix any genes not in func.key
     if(any(is.na(cf))) {cf[is.na(cf)] <- "undefined"}
     # cfheadcols <- dim(cf)[2]
     # quantitiative data:
     data.class <- sapply (cf.data, class)
     #
     if (any(use=="total")) {
          cf.Total <- rowSums(cf.data[,data.class=="numeric"], na.rm=TRUE)
          cf.Total[is.na(cf.Total)] <- 0
          cf$Total <- as.numeric(cf.Total)	}
     if (any(use=="mean")) {
          cf.Mean <- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
          cf.Mean[is.na(cf.Mean)] <- 0
          cf$Mean <- as.numeric(cf.Mean)	}
     if (any(use=="median"))	{
          cf.Median	<- apply(cf.data[,data.class=="numeric"], 1, median, na.rm=TRUE)
          cf.Median[is.na(cf.Median)] <- 0
          cf$Median <- as.numeric(cf.Median)	}
     if (any(use=="max"))	{
          cf.Max	<- apply(cf.data[,data.class=="numeric"], 1, function(x) {
               if (all(is.na(x))) return (NA) else {return (unique(as.numeric((x[which(abs(x)==max.na(abs(x)))]))))}				})
          if(class(cf.Max)=="list") {
               cf.Mean <- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
               dupmeans <- cf.Mean[which(sapply(cf.Max, length)==2)]
               dupmax <- cf.Max[which(sapply(cf.Max, length)==2)]
               for (i in 1:length(dupmax))		{
                    if(dupmeans[[i]]>=0) dupmax[[i]] <- max.na(dupmax[[i]])
                    if(dupmeans[[i]]<0) dupmax[[i]] <- min.na(dupmax[[i]])
               }
               cf.Max[which(sapply(cf.Max, length)==2)] <- dupmax
               cf.Max <- unlist(cf.Max)
          }
          cf.Max[is.na(cf.Max)] <- 0
          cf$Max <- as.numeric(cf.Max)	}
     if (genenodes==TRUE)  {
          cf=cf[,names(cf) %w/o% "Peptide.Name"]
          cf$No.Samples <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "No.Samples"]
          cf$No.Modifications <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "No.Modifications"]
          cf$ppidegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppidegree"]
          cf$ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppibetween"]
          cf$norm.ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "norm.ppibetween"]
     }
     if (genenodes==FALSE) {
          cf$Node.ID <- "gene"
          # split between gene and peptide nodes
          cf[which(cf$Peptide.Name!=cf$Gene.Name), "Node.ID"] <- "peptide"
          cf.list <- dlply(cf, .(Node.ID))
          cf.genes <- cf.list$gene
          cf.genes$parent <- ""
          if(length(cf.genes)>0){
               cf.genes$No.Samples <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "No.Samples"]
               cf.genes$No.Modifications <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "No.Modifications"]
               cf.genes$ppidegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppidegree"]
               cf.genes$ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppibetween"]
               cf.genes$norm.ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "norm.ppibetween"]}
          cf.peptide <- cf.list$peptide
          cf.peptide$parent <- cf.peptide$Gene.Name
          cf.peptide$No.Samples <- ptmcccnatts[ptmcccnatts $Peptide.Name %in% cf.peptide$Peptide.Name, "No.Samples"]
          cf.peptide$No.Modifications <- 1
          cf.peptide$ppidegree <- 0
          cf.peptide$ppibetween <- 0
          cf.peptide$pepdegree <- ptmcccnatts[ptmcccnatts $Peptide.Name %in% cf.peptide$Peptide.Name, "cccndegree"]
          cf.peptide$pepbetween <- ptmcccnatts[ptmcccnatts $Peptide.Name %in% cf.peptide$Peptide.Name, "cccnbetween"]
          # Node names need to be in first column
          # This is Peptide.Name for peptide-containing cfs
          if(length(dim(cf.genes[[1]])[1])==0) {cf.genes <- NULL}
          cf <-  rbind(cf.genes, cf.peptide) }
     if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
     return(cf)
}

#-------------------------------------------#

buildBromoNetwork()
