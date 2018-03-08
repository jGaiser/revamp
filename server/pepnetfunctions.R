	# stepwise: hypothetical pathway analysis
	# Retrieve in sets, e.g. RTKs -> adaptors -> kinases -> nuclear proteins
#genes=pd1genes
#genes[genes %in% rtklist]
#edgefile <- pd4.ppi
#edgefile <-  pd3.criz.sgp
#genes <- pd3.criz.sgp

#
rtk.pep.net <- function (genes, degree=2, edgefile, lincs=lincs, selectededges.df=selectededges.df) {
	genes <- unique(genes)
	rtks <- genes[genes %in% rtklist]
	if(length(rtks)>0) {
		rtkpeps <- lincs[lincs$Gene.Name %in% rtks, "Peptide.Name"]
	deg1.net <- filter.edges.1(rtks, edgefile)
	neighbors <- unique(c(as.character(deg1.net$Gene.1), as.character(deg1.net$Gene.2)))	 %w/o% rtks
	neighborpeps <- lincs[lincs$Gene.Name %in% neighbors, "Peptide.Name"]	
	rtkpepedges <- filter.edges.1(rtkpeps, selectededges.df)
	nbrpepedges <- filter.edges.1(neighborpeps, selectededges.df)
	deg1.pepedges <- filter.edges.0(c(rtkpeps, neighborpeps), rbind(rtkpepedges, nbrpepedges))
	}
	if(degree > 1) {
		deg2.net <- filter.edges.1(neighbors, edgefile)
		deg2.net <- deg2.net[deg2.net$Gene.1 %in% (deg2.net$Gene.1 %w/o% rtks),]
		deg2.net <- deg2.net[deg2.net$Gene.2 %in% (deg2.net$Gene.2 %w/o% rtks),]
		neighbors2 <- unique(c(deg2.net$Gene.1, deg2.net$Gene.2)) %w/o% neighbors
	if (length(neighbors2)>0) {
	neighbor2peps <- lincs[lincs$Gene.Name %in% neighbors2, "Peptide.Name"]	
	nbr2pepedges <- filter.edges.1(neighbor2peps, selectededges.df)
	deg2.pepedges <- filter.edges.0(c(neighborpeps, neighbor2peps), rbind(nbr2pepedges, nbrpepedges))
	deg12pepedges <- unique(rbind(deg1.pepedges, deg2.pepedges)) 
	return(deg12pepedges) }} else return(deg1.pepedges)
}
# END rtk.pep.net
#
# Function to use startgenes instead of, or in addition to, RTKs
#
#
start.pep.net <- function (genes, startgenes, degree=2, edgefile, lincs=lincs, selectededges.df=selectededges.df) {
	genes <- unique(genes)
	if(length(startgenes)>0) {
		startpeps <- lincs[lincs$Gene.Name %in% startgenes, "Peptide.Name"]
	deg1.net <- filter.edges.1(startgenes, edgefile)
	neighbors <- unique(c(as.character(deg1.net$Gene.1), as.character(deg1.net$Gene.2)))	 %w/o% startgenes
	neighborpeps <- lincs[lincs$Gene.Name %in% neighbors, "Peptide.Name"]	
	startpepedges <- filter.edges.1(startpeps, selectededges.df)
	nbrpepedges <- filter.edges.1(neighborpeps, selectededges.df)
	deg1.pepedges <- filter.edges.0(c(startpeps, neighborpeps), rbind(startpepedges, nbrpepedges))
	}
	if(degree > 1) {
		deg2.net <- filter.edges.1(neighbors, edgefile)
		deg2.net <- deg2.net[deg2.net$Gene.1 %in% (deg2.net$Gene.1 %w/o% startgenes),]
		deg2.net <- deg2.net[deg2.net$Gene.2 %in% (deg2.net$Gene.2 %w/o% startgenes),]
		neighbors2 <- unique(c(as.character(deg2.net$Gene.1), as.character(deg2.net$Gene.2))) %w/o% neighbors
	if (length(neighbors2)>0) {
	neighbor2peps <- lincs[lincs$Gene.Name %in% neighbors2, "Peptide.Name"]	
	nbr2pepedges <- filter.edges.1(neighbor2peps, selectededges.df)
	deg2.pepedges <- filter.edges.0(c(neighborpeps, neighbor2peps), rbind(nbr2pepedges, nbrpepedges))
	deg12pepedges <- unique(rbind(deg1.pepedges, deg2.pepedges)) 
	return(deg12pepedges) }} else return(deg1.pepedges)
}
# END start.pep.net
# To find more than just a single shortest path:
all.paths.net <- function (node1, node2, edgefile, extended=TRUE) {
     node1.net=NULL; node2.net=NULL; node1.deg2.net=NULL; node2.deg2.net=NULL; node1.deg3.net=NULL; node2.deg3.net=NULL; node1.deg4.net=NULL; node2.deg4.net=NULL
     node1.neighbors=NULL; node2.neighbors=NULL; node1.deg2.neighbors=NULL; node2.deg2.neighbors=NULL; 
     node1.deg3.neighbors=NULL; node2.deg3.neighbors=NULL; node1.deg4.neighbors=NULL; node2.deg4.neighbors=NULL;
     common=NULL
     if(length(extract.peptides(as.character(node1), edgefile))==0) {return(paste(node1, " is not in edge file!"))}
     if(length(extract.peptides(as.character(node2), edgefile))==0) {return(paste(node2, " is not in edge file!"))}
     node1.net <- filter.edges.1(as.character(node1), edgefile)
     node1.neighbors <- unique(c(as.character(node1.net$Gene.1), as.character(node1.net$Gene.2))) %w/o% node1
     node2.net <- filter.edges.1(as.character(node2), edgefile)
     node2.neighbors <- unique(c(as.character(node2.net$Gene.1), as.character(node2.net$Gene.2))) %w/o% node2
     common <- intersect(node1.neighbors, node2.neighbors)
     if (length(intersect(node1.neighbors, node2.neighbors))==0){
          node1.deg2.net <- filter.edges.1(node1.neighbors, edgefile)
          node1.deg2.neighbors <- unique(c(as.character(node1.deg2.net$Gene.1), as.character(node1.deg2.net$Gene.2))) %w/o% node1.neighbors
          node2.deg2.net <- filter.edges.1(node2.neighbors, edgefile)
          node2.deg2.neighbors <- unique(c(as.character(node2.deg2.net$Gene.1), as.character(node2.deg2.net$Gene.2))) %w/o% node2.neighbors
          common <- intersect(node1.deg2.neighbors, node2.deg2.neighbors)
     } else {
          if (length(intersect(node1.deg2.neighbors, node2.deg2.neighbors))==0){
               # do it again
               node1.deg3.net <- filter.edges.1(node1.deg2.neighbors, edgefile)
               node1.deg3.neighbors <- unique(c(as.character(node1.deg3.net$Gene.1), as.character(node1.deg3.net$Gene.2))) %w/o% node1.deg2.neighbors
               node2.deg3.net <- filter.edges.1(node2.deg2.neighbors, edgefile)
               node2.deg3.neighbors <- unique(c(as.character(node2.deg3.net$Gene.1), as.character(node2.deg3.net$Gene.2))) %w/o% node2.deg2.neighbors
               common <- intersect(node1.deg3.neighbors, node2.deg3.neighbors)
          }  else {
               if (length(intersect(node1.deg3.neighbors, node2.deg3.neighbors))==0){
                    # do it again
                    node1.deg4.net <- filter.edges.1(node1.deg3.neighbors, edgefile)
                    node1.deg4.neighbors <- unique(c(as.character(node1.deg4.net$Gene.1), as.character(node1.deg4.net$Gene.2))) %w/o% node1.deg3.neighbors
                    node2.deg4.net <- filter.edges.1(node2.deg3.neighbors, edgefile)
                    node2.deg4.neighbors <- unique(c(as.character(node2.deg4.net$Gene.1), as.character(node2.deg4.net$Gene.2))) %w/o% node2.deg3.neighbors
                    common <- intersect(node1.deg4.neighbors, node2.deg4.neighbors)
               } else {
                    if (length(intersect(node1.deg4.neighbors, node2.deg4.neighbors))==0){
                         # do it again
                         node1.deg5.net <- filter.edges.1(node1.deg4.neighbors, edgefile)
                         node1.deg5.neighbors <- unique(c(as.character(node1.deg5.net$Gene.1), as.character(node1.deg5.net$Gene.2))) %w/o% node1.deg4.neighbors
                         node2.deg5.net <- filter.edges.1(node2.deg4.neighbors, edgefile)
                         node2.deg5.neighbors <- unique(c(as.character(node2.deg5.net$Gene.1), as.character(node2.deg5.net$Gene.2))) %w/o% node2.deg4.neighbors
                         common <- intersect(node1.deg4.neighbors, node2.deg4.neighbors)
                    }
                    
               }}}
     if(length(common)==0) {return("Nodes did not connect at degree 5.")}  
     if (length(common)>0){
          goodnodes <- c(node1, node2, intersect(node1.neighbors, node2.neighbors), intersect(node1.deg2.neighbors, node2.deg2.neighbors), intersect(node1.deg3.neighbors, node2.deg3.neighbors), intersect(node1.deg4.neighbors, node2.deg4.neighbors))
          comb.net <- rbind(node1.net, node2.net, node1.deg2.net, node2.deg2.net, node1.deg3.net, node2.deg3.net, node1.deg4.net, node2.deg4.net)
          comb.net <- comb.net[comb.net$Gene.1 %in% goodnodes | comb.net$Gene.2 %in% goodnodes,]
          nodepath <- connectNodes(nodepair=(c(node1, node2)), edgefile=comb.net, newgraph=TRUE)
          comb.path <- filter.edges.0 (nodepath[[2]], comb.net)
          comb.path2 <- filter.edges.1 (goodnodes, comb.net)
     }
     if (extended==TRUE) {return(comb.path2)} else return(comb.path)
}
# end all.paths.net
#
# Problem (?) corr edges between ts factors and rtks are returned though no interactions are in ppi file
#
filter.path <- function(ppi.net, corr.net) {
		edgefile=corr.net
		edgefile$Gene.Name.1 <- sapply((as.character(edgefile$Gene.1)),  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
		edgefile$Gene.Name.2 <- sapply((as.character(edgefile$Gene.2)),  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	# reverse order to included reversed ppi edges
		edgefile$Gene.Name.1.R <- edgefile$Gene.Name.2
		edgefile$Gene.Name.2.R <- edgefile$Gene.Name.1
		edgefile $nodes.combined <- noquote(paste(edgefile $Gene.Name.1, edgefile $Gene.Name.2))
		edgefile $nodes.reversed <- noquote(paste(edgefile $Gene.Name.2, edgefile $Gene.Name.1))
	# make combined ppi column
		ppi.net $nodes.combined <- noquote(paste(ppi.net $Gene.1, ppi.net $Gene.2))
	# filter		
		edgefile.f1 <- edgefile[edgefile$nodes.combined %in% ppi.net$nodes.combined, ] 
		edgefile.f2 <- edgefile[edgefile$nodes.reversed %in% ppi.net$nodes.combined, ]
		edgefile.f  <- unique(rbind(edgefile.f1, edgefile.f2)) 
		return(edgefile.f[, 1:dim(corr.net)[2]])
}
# END filter.path
filter.ppi <- function(ppi.edges, corr.edges) {
		corr.net=corr.edges
		ppi.net=ppi.edges
	# reverse order to included reversed ppi edges
		corr.net $nodes.combined <- noquote(paste(corr.net $Gene.1, corr.net $Gene.2))
		corr.net $nodes.reversed <- noquote(paste(corr.net $Gene.2, corr.net $Gene.1))
	# make combined ppi column
		ppi.net $nodes.combined <- noquote(paste(ppi.net $Gene.1, ppi.net $Gene.2))
	# filter	ppis by corr edges	
		ppi.f1 <- ppi.net[ppi.net $nodes.combined %in% corr.net$nodes.combined, ] 
		ppi.f2 <- ppi.net[ppi.net $nodes.combined %in% corr.net$nodes.reversed, ] 
		ppi.f  <- unique(rbind(ppi.f1, ppi.f2)) 
		return(ppi.f[, 1:dim(ppi.edges)[2]])
}
#
filter.edges.between <- function(nodes1, nodes2, edge.file) {
	sel.edges1 <- edge.file[edge.file$Gene.1 %in% nodes1 & edge.file$Gene.2%in% nodes2,]
	sel.edges2 <- edge.file[edge.file$Gene.1 %in% nodes2 & edge.file$Gene.2%in% nodes1,]
	sel.edges <- rbind(sel.edges1, sel.edges2)			
	if(dim(sel.edges)[1] == 0) {return(NA)} else return(sel.edges) 
	}
# Function to get peptide names from a list of nodes containing gene names 
grep.edges <- function(nodes, edgefile) {
	edgefile$cyedgenames <- mapply(paste, edgefile $Gene.1, " (", edgefile $edgeType, ") ", edgefile $Gene.2, sep="")
	edgelist <- list()
	for (i in 1:length(nodes)) {
		edgelist[[i]] <- edgefile[grep(nodes[i], edgefile$cyedgenames),]
	}
	selectedEdges <- ldply(edgelist)
	return(unique(selectedEdges))
	}
#
# for this function edgefile = peptide correlation edge file
 genepep.edges <- function(edgefile) {
	edgefile$Gene.Name.1 <- sapply((as.character(edgefile$Gene.1)),  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	edgefile$Gene.Name.2 <- sapply((as.character(edgefile$Gene.2)),  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	genepepedges.1 <- data.frame(gene=edgefile$Gene.Name.1, peptide=edgefile$Gene.1) 
	genepepedges.2 <- data.frame(gene=edgefile$Gene.Name.2, peptide=edgefile$Gene.2)
	gpedges <- rbind(unique(genepepedges.1), unique(genepepedges.2))
	gpedges <- gpedges[order(gpedges$peptide),]
	gpedges$Weight <- 1
	gpedges$edgeType <- "peptide"
	gpedges$gene <- as.character(gpedges$gene)
	gpedges$peptide <- as.character(gpedges$peptide)
	names(gpedges)[1:2] <- c("Gene.1", "Gene.2")
	gpedges <- unique(gpedges)
	# harmonize with edgefile
	# gpedges$Group.Intersect <- 0
	gpedges$Alt.Weight <- 100
	gpedges$Directed <- FALSE
	gpedges <- gpedges[, c(1,2,4,3,5,6)]
	return(gpedges)
}
# see pepnetfunction_tests.R for rest
genepep.edges.2 <- function(edgefile, pepkey=ld.key) {
	nodes <- unique(c(edgefile$Gene.1, edgefile$Gene.2))
	gpedges <- pepkey[pepkey$Gene.Name %in% nodes, 1:2]	
	names(gpedges)[1:2] <- c("Gene.1", "Gene.2")
	gpedges$edgeType <- "peptide"
	gpedges$Weight <- 1
	gpedges$Alt.Weight <- 100
	gpedges$Directed <- FALSE
	gpedges <- unique(gpedges)
	return(gpedges)
}
# version for starting with node names		
genepep.edges.3 <- function(nodelist, pepkey=ld.key) {
	nodelist <- unique(nodelist)
	gpedges <- pepkey[pepkey$Gene.Name %in% nodelist, 1:2]	
	names(gpedges)[1:2] <- c("Gene.1", "Gene.2")
	gpedges$edgeType <- "peptide"
	gpedges$Weight <- 1
	gpedges$Alt.Weight <- 100
	gpedges$Directed <- FALSE
	return(unique(gpedges))
}
#
	# make cytoscape file from peptides
	# give nodes names according to kinase / ts factor
#	
make.cf.1 <- function(edge.df, data.file, use=c("total", "mean", "median")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
	cf.data <- data.file[rownames(data.file) %in% cf.nodes,]
	if (any(grepl("Total", names(cf.data)))) {
		cf.data <- cf.data[, -grep("Total",names(cf.data))] }	
	cf <- data.frame(Peptide.Name=cf.nodes)
	cf$Peptide.Name <- as.character(cf$Peptide.Name)
	cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	# node classification: 
	cf$nodeType <- "undefined"
	cf[cf$Gene.Name %in% membraneproteins, "nodeType"] 	<- "membrane protein"
	cf[cf$Gene.Name %in% gpcrproteins, "nodeType"] 	<- "G protein-coupled receptor"
	cf[cf$Gene.Name %in% rnabindingproteins, "nodeType"] 	<- "RNA binding protein"
	cf[cf$Gene.Name %in% rnaprocproteins, "nodeType"] 	<- "RNA processing protein"
	cf[cf$Gene.Name %in% rnaspliceproteins, "nodeType"] 	<- "RNA splicing protein"
	cf[cf$Gene.Name %in% intersect(rnabindingproteins, rnaspliceproteins), "nodeType"] 	<- "RNA binding and splicing protein"
	cf[cf$Gene.Name %in% intersect(rnabindingproteins, rnaprocproteins), "nodeType"] 	<- "RNA binding and processing protein"
	cf[cf$Gene.Name %in% intersect(rnaprocproteins, rnaspliceproteins), "nodeType"] 	<- "RNA processing and splicing protein"
	cf[cf$Gene.Name %in% sh3list, "nodeType"] 		<- "SH3 protein"
	cf[cf$Gene.Name %in% sh2list, "nodeType"] 		<- "SH2 protein"
	cf[cf$Gene.Name %in% sh2sh3s, "nodeType"] 		<- "SH2-SH3 protein"
	cf[cf$Gene.Name %in% htsfactors, "nodeType"] 	<- "transcription factor"
	cf[cf$Gene.Name %in% tsregulators, "nodeType"] 	<- "transcription regulator"
	cf[cf$Gene.Name %in% kins, "nodeType"] 			<- "kinase"
	cf[cf$Gene.Name %in% acetyltransferases, "nodeType"] 	<- "acetyltransferase"
	cf[cf$Gene.Name %in% deacetylases, "nodeType"] 			<- "deacetylase"
	cf[cf$Gene.Name %in% demethylases, "nodeType"] 			<- "demethylase"
	cf[cf$Gene.Name %in% methyltransferases, "nodeType"] 	<- "methyltransferase"
	cf[cf$Gene.Name %in% phosphatases, "nodeType"] 		<- "phosphatase"
	cf[cf$Gene.Name %in% Yphosphatases, "nodeType"] 	<- "tyrosine phosphatase"
	cf[cf$Gene.Name %in% yks, "nodeType"] 			<- "tyrosine kinase"
	cf[cf$Gene.Name %in% rtklist, "nodeType"] 		<- "receptor tyrosine kinase"
	cf[cf$Gene.Name %in% sfklist, "nodeType"] 		<- "SRC-family kinase"
	# quantitiative data:
	data.class <- sapply (cf.data, class)	
	if (any(use=="total")) 	cf$Total 	<- rowSums(cf.data[,data.class=="numeric"], na.rm=TRUE)
	if (any(use=="mean")) 	cf$Mean 		<- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
	if (any(use=="median"))	cf$Median	<- apply(cf.data[,data.class=="numeric"], 1, median, na.rm=TRUE)	
	return(cf)
}	
# End make.cf.1
make.cf.2 <- function(edge.df, data.file, use=c("total", "mean", "median")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
	cf.data <- data.file[rownames(data.file) %in% cf.nodes,]
	if (any(grepl("Total", names(cf.data)))) {
		cf.data <- cf.data[, -grep("Total",names(cf.data))] }	
	cf <- data.frame(Peptide.Name=cf.nodes)
	cf$Peptide.Name <- as.character(cf$Peptide.Name)
	cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	# node classification: 
	cf.functions <- func.key[func.key$Gene.Name %in% cf.nodes,]
	cf <- plyr::join(cf, cf.functions, by="Gene.Name", type="left")
	# quantitiative data:
	data.class <- sapply (cf.data, class)	
	if (any(use=="total")) 	cf$Total 	<- rowSums(cf.data[,data.class=="numeric"], na.rm=TRUE)
	if (any(use=="mean")) 	cf$Mean 		<- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
	if (any(use=="median"))	cf$Median	<- apply(cf.data[,data.class=="numeric"], 1, median, na.rm=TRUE)	
	return(cf)
}
# End make.cf.2
#
# Revised to use func.key and distinguish gene vs. peptide nodes:
make.cf <- function(edge.df, data.file, gene.mods=genes.cf, func.key=func.key, use=c("total", "mean", "median")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
	cf.data <- data.file[rownames(data.file) %in% cf.nodes,]
	if (any(grepl("Total", names(cf.data)))) {
		cf.data <- cf.data[, -grep("Total",names(cf.data))] }	
	cf <- data.frame(Peptide.Name=cf.nodes)
	cf$Peptide.Name <- as.character(cf$Peptide.Name)
	cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	if(identical(cf$Peptide.Name, cf$Gene.Name)) {genenodes=TRUE} else {genenodes=FALSE}
	# node classification: 
	cf.functions <- func.key[func.key$Gene.Name %in% cf$Gene.Name,]
	cf <- plyr::join(cf, cf.functions, by="Gene.Name", type="left")
	# quantitiative data:
	data.class <- sapply (cf.data, class)	
	if (any(use=="total")) 	cf$Total 	<- rowSums(cf.data[,data.class=="numeric"], na.rm=TRUE)
	if (any(use=="mean")) 	cf$Mean 		<- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
	if (any(use=="median"))	cf$Median	<- apply(cf.data[,data.class=="numeric"], 1, median, na.rm=TRUE)
	if (genenodes==TRUE)  {
		cf=cf[,names(cf) %w/o% "Peptide.Name"]
		cf$No.Modifications <- gene.mods[gene.mods$Gene.Name %in% cf$Gene.Name, "No.Modifications"]	}
	if (genenodes==FALSE) {cf <- cf[,c(2,1,3:dim(cf)[2])]}	
	# Fix any genes not in func.key
	cf[is.na(cf)] <- "undefined"
	return(cf)
}

# End make.cf (5 in pepnetfunctions_tests.R)

#
# add No.Samples to this version

make.ldcf <- function(edge.df, data.file, gene.mods=genes.cf, func.key=func.key, use=c("total", "mean", "median")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
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
	# quantitiative data:
	data.class <- sapply (cf.data, class)
	cf.sum <- data.frame(Gene.Name <- cf.functions$Gene.Name)	
	if (any(use=="total")) {
		cf.Total <- rowSums(cf.data[,data.class=="numeric"], na.rm=TRUE)
		cf.Total[is.na(cf.Total)] <- 0 
		cf$Total <- cf.Total	}
	if (any(use=="mean")) {
		cf.Mean <- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
		cf.Mean[is.na(cf.Mean)] <- 0 
		cf$Mean <- cf.Mean	}
	if (any(use=="median"))	{
		cf.Median	<- apply(cf.data[,data.class=="numeric"], 1, median, na.rm=TRUE)
		cf.Median[is.na(cf.Median)] <- 0 
		cf$Median <- cf.Median	}
	if (genenodes==TRUE)  {
		cf=cf[,names(cf) %w/o% "Peptide.Name"]
		cf$No.Samples <- gene.mods[gene.mods$Gene.Name %in% cf$Gene.Name, "No.Samples"]
		cf$No.Modifications <- gene.mods[gene.mods$Gene.Name %in% cf$Gene.Name, "No.Modifications"]	
			}
	if (genenodes==FALSE) {
		cf <- cf[,c(2,1,3:dim(cf)[2])]
		cf$No.Samples <- gene.mods[gene.mods$Peptide.Name %in% cf$Peptide.Name, "No.Samples"]	
		cf$No.Modifications <- 1		}
		# Fix any genes not in func.key
	if(any(is.na(cf))) {cf[is.na(cf)] <- "undefined"}
	return(cf)
}

# add network attributes to this version:

make.netcf <- function(edge.df, data.file, atts=netatts, func.key=func.key, use=c("total", "mean", "median", "max")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
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
		cf$Total <- cf.Total	}
	if (any(use=="mean")) {
		cf.Mean <- rowMeans(cf.data[,data.class=="numeric"], na.rm=TRUE)
		cf.Mean[is.na(cf.Mean)] <- 0 
		cf$Mean <- cf.Mean	}
	if (any(use=="median"))	{
		cf.Median	<- apply(cf.data[,data.class=="numeric"], 1, median, na.rm=TRUE)
		cf.Median[is.na(cf.Median)] <- 0 
		cf$Median <- cf.Median	}
	if (any(use=="max"))	{
		cf.Max	<- apply(cf.data[,data.class=="numeric"], 1, function(x) (
			if (all(is.na(x))) return (NA) else return (max.na(abs(x)))		))		
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
		cf$Max <- cf.Max	}
	if (genenodes==TRUE)  {
		cf=cf[,names(cf) %w/o% "Peptide.Name"]
		cf$No.Samples <- atts[atts$Gene.Name %in% cf$Gene.Name, "No.Samples"]
		cf$No.Modifications <- atts[atts$Gene.Name %in% cf$Gene.Name, "No.Modifications"]
		cf$ppidegree.s <- atts[atts$Gene.Name %in% cf$Gene.Name, "ppidegree.s"]
		cf$ppidegree.m <- atts[atts$Gene.Name %in% cf$Gene.Name, "ppidegree.m"]
		cf$cccndegree <- atts[atts$Gene.Name %in% cf$Gene.Name, "cccndegree"]	
		cf$ppibetween.s <- atts[atts$Gene.Name %in% cf$Gene.Name, "ppibetween.s"]
		cf$ppibetween.m <- atts[atts$Gene.Name %in% cf$Gene.Name, "ppibetween.m"]
		cf$cccnbetween <- atts[atts$Gene.Name %in% cf$Gene.Name, "cccnbetween"]
			}
	if (genenodes==FALSE) {
		cf <- cf[,c(2,1,3:dim(cf)[2])]
		cf$No.Samples <- atts[atts$Peptide.Name %in% cf$Peptide.Name, "No.Samples"]	
		cf$No.Modifications <- 1		
		cf$pepcccndegree <- atts[atts$Peptide.Name %in% cf$Peptide.Name, "pepcccndegree"]	
		cf$pephalfcccndegree <- atts[atts$Peptide.Name %in% cf$Peptide.Name, "pephalfcccndegree"]	
		cf$pepcccnbetween <- atts[atts$Peptide.Name %in% cf$Peptide.Name, "pepcccnbetween"]	
		cf$pepcccnhalfbetween <- atts[atts$Peptide.Name %in% cf$Peptide.Name, "pepcccnhalfbetween"]
		# make gene nodes here
		 	tmp <- cf[,names(cf) %w/o% "Peptide.Name"]
		 	tmp.1 <- ddply(tmp, names(tmp)[1:8], numcolwise(sum.na))
		 	tmp.2 <- data.frame(Peptide.Name=tmp.1$Gene.Name, tmp.1)
		 	cf <- rbind(cf, tmp.2)
						}
	if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
	return(cf)
}
# This version handles mixed gene and peptide nodes:
make.netcf2 <- function(edge.df, data.file, geneatts=netatts, pepatts=pepcccnatts, func.key=func.key, use=c("total", "mean", "median", "max")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
	cf.data <- data.frame(data.file[rownames(data.file) %in% cf.nodes,])
	if (any(grepl("Total", names(cf.data)))) {
		cf.data <- cf.data[, -grep("Total",names(cf.data))] }	
	cf <- data.frame(Peptide.Name=rownames(cf.data))
	cf$Peptide.Name <- as.character(cf$Peptide.Name)
	cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	if(identical(cf$Peptide.Name, cf$Gene.Name)) {genenodes=TRUE} else {genenodes=FALSE}
	if(genenodes==FALSE)		{
		cf$node <- "gene"
		cf[which(cf$Peptide.Name!=cf$Gene.Name), "node"] <- "peptide"	}
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
		cf$ppidegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppidegree.s"]
		# cf$ppidegree.m <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppidegree.m"]
		cf$cccndegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "cccndegree"]	
		cf$ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppibetween.s"]
		# cf$ppibetween.m <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppibetween.m"]
		cf$cccnbetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "cccnbetween"]
			}
	if (genenodes==FALSE) {
		# split between gene and peptide nodes
		cf.list <- dlply(cf, .(node))
		cf.genes <- cf.list$gene
		  cf.genes$No.Samples <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "No.Samples"]
		  cf.genes$No.Modifications <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "No.Modifications"]
		  cf.genes$ppidegree <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppidegree.s"]
		  # cf.genes$ppidegree.m <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppidegree.m"]
		  cf.genes$cccndegree <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "cccndegree"]	
		  cf.genes$ppibetween <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppibetween.s"]
		  # cf.genes$ppibetween.m <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppibetween.m"]
		  cf.genes$cccnbetween <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "cccnbetween"]
		cf.peptide <- cf.list$peptide
		# cf <- cf[,c(2,1,3:dim(cf)[2])]
	 	  cf.peptide$No.Samples <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "No.Samples"]	
		  cf.peptide$No.Modifications <- 1		
		  cf.peptide$ppidegree <- 1
		  # cf.peptide$pepcccndegree <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pepcccndegree"]	
		  cf.peptide$cccndegree <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pephalfcccndegree"]	
		  # cf.peptide$pepcccnbetween <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pepcccnbetween"]
		  cf.peptide$ppibetween <- 0
		  cf.peptide$cccnbetween <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pepcccnhalfbetween"]
		  # Node names need to be in first column
		  cf.peptide$Gene.Name <- cf.peptide$Peptide.Name
		cf <-  rbind(cf.genes, cf.peptide) }
	if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
	return(cf)
}	
# This version is tweaked to use optimized network attributes
 make.optnetcf <- function(edge.df, data.file, geneatts=optgenetatts, pepatts=optpepatts, func.key=func.key, use=c("total", "mean", "median", "max")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
	cf.data <- data.frame(data.file[rownames(data.file) %in% cf.nodes,])
	if (any(grepl("Total", names(cf.data)))) {
		cf.data <- cf.data[, -grep("Total",names(cf.data))] }	
	cf <- data.frame(Peptide.Name=rownames(cf.data))
	cf$Peptide.Name <- as.character(cf$Peptide.Name)
	cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	if(identical(cf$Peptide.Name, cf$Gene.Name)) {genenodes=TRUE} else {genenodes=FALSE}
	if(genenodes==FALSE)		{
		cf$node <- "gene"
		cf[which(cf$Peptide.Name!=cf$Gene.Name), "node"] <- "peptide"	}
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
		cf$ppidegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppi.degree"]
		cf$cccndegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "cccn.degree"]	
		cf$ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppi.between"]
		cf$cccnbetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "cccn.between"]
		cf$norm.ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "norm.ppibetween"]
			}
	if (genenodes==FALSE) {
		# split between gene and peptide nodes
		cf.list <- dlply(cf, .(node))
		cf.genes <- cf.list$gene
		  cf.genes$No.Samples <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "No.Samples"]
		  cf.genes$No.Modifications <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "No.Modifications"]
		  cf.genes$ppidegree <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppi.degree"]
		  cf.genes$cccndegree <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "cccn.degree"]	
		  cf.genes$ppibetween <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppi.between"]
		  cf.genes$cccnbetween <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "cccn.between"]
		cf$norm.ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "norm.ppibetween"]
		cf.peptide <- cf.list$peptide
		# cf <- cf[,c(2,1,3:dim(cf)[2])]
	 	  cf.peptide$No.Samples <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "No.Samples"]	
		  cf.peptide$No.Modifications <- 1		
		  cf.peptide$ppidegree <- 1
		  cf.peptide$pepdegree <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "optpepdegree"]	
		  cf.peptide$pepbetween <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "optpepbetween"]
		  cf.peptide$ppibetween <- 0
		  # Node names need to be in first column
		  cf.peptide$Gene.Name <- cf.peptide$Peptide.Name
		cf <-  rbind(cf.genes, cf.peptide) }
	if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
	return(cf)
 }
#
 # For use with different netatts files. Returns cccnatts for ptms, ppinetatts for genes
 make.anynetcf <- function(edge.df, data.file, geneatts, ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max")) {
      cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
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
# 
corr.edges.style.2 <- function(windowname, edgefile) {
			# Set up Cytoscape to make positive correlation yellow, negative blue
		edgeTypes <- c("negative correlation", "positive correlation", "peptide")
		corredgecolors = c('#0000FF', "#FCD116", '#000000') 
    		#   blue; sign yellow; black; 
   		myarrows <- c ('No Arrow', 'No Arrow', "No Arrow")
		setDefaultBackgroundColor(cy,'#888888', 'default' )
		# set up (have to make a graph first)
 	   	setEdgeTargetArrowRule(windowname, 'edgeType', edgeTypes, myarrows, default='No Arrow')  
	    setEdgeTargetArrowColorRule(windowname, "edgeType",  edgeTypes, corredgecolors, default.color='#FF0000')      
	 	setEdgeColorRule(windowname, 'edgeType', edgeTypes, corredgecolors, mode='lookup', default.color='#FDF8FF') 	# defalult zinc white
	 	line.widths <- 4.5*abs(as.numeric(edgefile$Weight))
		setEdgeLineWidthRule(windowname, "Weight", attribute.values=as.character(edgefile$Weight), line.widths, default.width=1.2)
		alledges <- getAllEdges(windowname)
		proteinedges <- alledges[grep("peptide", alledges, fixed=TRUE)]
		setEdgeLineWidthDirect(windowname, edge.names=proteinedges, new.value=1.2)
		redraw(windowname)
		add.style.name <- paste("Corr Edges Style",length(getWindowList(cy)))
		if (!(add.style.name %in% getVisualStyleNames(cy)))	{ copyVisualStyle (windowname, "default", add.style.name) }
		hidePanel(cy, "Results")
}
nodeDprops.RCy3 <- function (windowobj) {
	setDefaultBackgroundColor (windowobj, 'E0E0E0') # grey 88
	setDefaultNodeShape(windowobj, "ELLIPSE")
	setDefaultNodeColor (windowobj, '#C9C9C9') # gray 79
	setDefaultNodeSize  (windowobj, 30) # for grey non-data nodes
	setDefaultNodeFontSize(windowobj, 20)
	setDefaultNodeLabelColor(windowobj, '#000000')
	setDefaultNodeBorderWidth (windowobj, 1.8)
	setDefaultNodeBorderColor (windowobj, '#888888')  # gray 
    setDefaultNodeSelectionColor(windowobj, "#FF3388")
	molclasses <- c("unknown", "receptor tyrosine kinase",  "SH2 protein", "SH3 protein", "tyrosine kinase",  "SRC-family kinase",   "kinase", "transcription factor", "RNA binding protein")
	#  NOTE getNodeShapes(cy) returns node shapes in random order!  Define manually
	 #	*12 for RCy2; 9 for RCy3	
	 nodeshapes <- c("ELLIPSE","ROUND_RECTANGLE", "RECTANGLE", "TRIANGLE", "HEXAGON", "DIAMOND", "OCTAGON", "PARALLELOGRAM", "VEE")
	if(length(sessionInfo()$otherPkgs$RCy3)==0) {
		nodeshapes <- c("ellipse", "round_rect", "rect", "triangle", "hexagon", "diamond", "octagon", "parallelogram", "vee")}
setNodeShapeRule (windowobj, node.attribute.name="nodeType", attribute.values=molclasses, node.shapes=nodeshapes, default.shape="ELLIPSE")
	setDefaultNodeSelectionColor (windowobj,  "#CC00FF") 
    # setNodeLabelRule(windowobj, node.attribute.name)
    redraw(windowobj)
   }
#		setLayoutProperties (cy, "kamada-kawai", list (edge_attribute='Weight', weight_type=3, min_weight=min(-1), max_weight=max(100), layout_passes=50, iterations_pernode=50, distance_strength=8008, rest_length=60, disconnected_strength=0.01, disconnected_rest_length=500, anticollisionStrength=10)) 


nodeDprops.new <- function (windowobj, cf, merged=FALSE) {
	setDefaultBackgroundColor (windowobj, "#949494") # grey 58
	if (merged) setDefaultBackgroundColor (windowobj, "#FFFFFF") # white
	setDefaultNodeShape(windowobj, "ELLIPSE")
	setDefaultNodeColor (windowobj, '#F0FFFF') # azure1
	setDefaultNodeSize  (windowobj, 100) # for grey non-data nodes
	setDefaultNodeFontSize(windowobj, 22)
	setDefaultNodeLabelColor(windowobj, '#000000')  # black
	setDefaultNodeBorderWidth (windowobj, 1.8)
	setDefaultNodeBorderColor (windowobj, '#888888')  # gray 
    setDefaultNodeSelectionColor(windowobj, "#FF3388")
	molclasses <- c("unknown", "receptor tyrosine kinase",  "SH2 protein", "SH2-SH3 protein", "SH3 protein", "tyrosine kinase",  "SRC-family kinase",   "kinase", "phosphatase", "transcription factor", "RNA binding protein")
	#  NOTE getNodeShapes(cy) returns node shapes in random order!  Define manually 
	 #	*12 for RCy2; 9 for RCy3
	 # there are now 24 nodeType classes
	nodeshapes <- c("ELLIPSE","ROUND_RECTANGLE", "VEE", "VEE", "TRIANGLE", "HEXAGON", "DIAMOND", "OCTAGON", "OCTAGON", "PARALLELOGRAM", "RECTANGLE")
	if(length(sessionInfo()$otherPkgs$RCy3)==0) {
		nodeshapes <- c("ellipse", "round_rect", "vee", "triangle", "hexagon", "diamond", "octagon", "octagon", "parallelogram", "rect")}
	setDefaultNodeSelectionColor (windowobj,  "#CC00FF") 
	setNodeShapeRule (windowobj, node.attribute.name="nodeType", attribute.values=molclasses, node.shapes=nodeshapes, default.shape="ELLIPSE")
	setNodeBorderWidthRule(windowobj, node.attribute.name="nodeType", attribute.values=c("deacetylase","acetyltransferase","demethylase","methyltransferase","membrane protein", "receptor tyrosine kinase", "G protein-coupled receptor", "SRC-family kinase", "tyrosine kinase", "kinase", "phosphatase"), line.widths=c(4,12,4,12,8,16,16,12,12,12,14), default.width=4)
	if (length(cf[grep("SH2", cf$Domains), 1])>0 & !all(grep("SH2", cf$Domains) %in% which(cf$nodeType %in% molclasses))) {
	setNodeShapeDirect(windowobj, cf[grep("SH2", cf$Domains) %w/o% which(cf$nodeType %in% molclasses), 1], nodeshapes[3])} 
	if (length(cf[grep("RNA", cf$nodeType), 1])>0) {
	setNodeShapeDirect(windowobj, cf[grep("RNA", cf$nodeType), 1], nodeshapes[11])}
	if (length(cf[grep("transcription", cf$nodeType), 1])>0) {
	setNodeShapeDirect(windowobj, cf[grep("transcription", cf$nodeType), 1], nodeshapes[10])}
	if (length(cf[grep("acetyl", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("acetyl", cf$nodeType), 1], "#FF8C00")} # darkorange
	if (length(cf[grep("methyl", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("methyl", cf$nodeType), 1], "#005CE6")} # blue
	if (length(cf[grep("membrane", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("membrane", cf$nodeType), 1], "#6600CC") # purple
	setNodeShapeDirect(windowobj, cf[grep("membrane", cf$nodeType), 1], nodeshapes[2])} 
	if (length(cf[grep("kinase", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("kinase", cf$nodeType), 1], "#EE0000")} # red2
	if (length(cf[grep("phosphatase", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("phosphatase", cf$nodeType), 1], "#FFEC8B")} # lightgoldenrod1
	if (length(cf[grep("receptor", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("receptor", cf$nodeType), 1], "#BF3EFF") # darkorchid1
	setNodeShapeDirect(windowobj, cf[grep("receptor", cf$nodeType), 1], nodeshapes[2])} 
	if (length(cf[grep("TM", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("TM", cf$Domains), 1], "#6600CC") # purple
	setNodeShapeDirect(windowobj, cf[grep("TM", cf$Domains), 1], nodeshapes[2])} 
    # setNodeLabelRule(windowobj, node.attribute.name)
    # redraw(windowobj)
   }
   # end
# Network Attribute properties: degree and betweeness
#		based on degree=size; betweenness=color
netattProps <- function (windowobj, cytoscape.file, use="ppi", colscale=2, sizescale=1.2, merged=FALSE, pepnet=FALSE, half=FALSE) {
	node.sizes     = c (25, seq(30, 140, length.out=9), 150)
	setVisualStyle (windowobj, "default")	
	if(use=="ppi")		{
		nodesizeatt <- "ppidegree.s" 
		nodecoloratt <- "ppibetween.s"
		nodedegree <- cytoscape.file$ppidegree.s
		nodebetween <- cytoscape.file$ppibetween.s 	
		if (merged) {
			nodesizeatt <- "ppidegree.m" 
			nodecoloratt <- "ppibetween.m"
			nodedegree <- cytoscape.file$ppidegree.m
			nodebetween <- cytoscape.file$ppibetween.m 		
			}
		}
	if(use=="cccn")		{
		nodesizeatt <- "cccndegree" 
		nodecoloratt <- "cccnbetween"
		nodedegree <- cytoscape.file$cccndegree
		nodebetween <- cytoscape.file$cccnbetween 	}
	if(pepnet==TRUE) {
		nodesizeatt <- "pepcccndegree" 
		nodecoloratt <- "pepcccnbetween"
		nodedegree <- cytoscape.file$pepcccndegree
		nodebetween <- cytoscape.file$pepcccnbetween 	}
	if(half==TRUE) {
		nodesizeatt <- "pephalfcccndegree" 
		nodecoloratt <- "pepcccnhalfbetween"
		nodedegree <- cytoscape.file$pephalfcccndegree
		nodebetween <- cytoscape.file$pepcccnhalfbetween 	}
	maxdegree <- 	max(nodedegree)
	size.control.points = maxdegree/sizescale^(8:0)
	setNodeSizeRule (windowobj, nodesizeatt, size.control.points, node.sizes, mode='interpolate')
	maxbetween <- max(nodebetween)
	color.control.points = maxbetween/colscale^(8:0)
	between.colors = c ('#0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
	setNodeColorRule (windowobj, nodecoloratt, color.control.points, between.colors, mode='interpolate')
		# new.style.name = paste(plotcol, noquote("_ratio"),  sep="", collapse=NULL) 
		# if (!(new.style.name %in% getVisualStyleNames(cy)))	{copyVisualStyle(windowobj, 'default', new.style.name)} # bsroken
		# setVisualStyle(windowobj, new.style.name)
		# return(windowobj)
    }
#
 #   For use with make.netcf2: size is between, color degree
netattProps2 <- function (windowobj, cytoscape.file, use="ppi", colscale=2, sizescale=1.2) {
	node.sizes     = c (25, seq(30, 140, length.out=9), 150)
	setVisualStyle (windowobj, "default")	
	if(use=="ppi")		{
		nodesizeatt <- "ppibetween" 
		nodecoloratt <- "ppidegree"
		nodedegree <- cytoscape.file$ppidegree
		nodebetween <- cytoscape.file$ppibetween }	
	if(use=="cccn")		{
		nodesizeatt <- "cccnbetween" 
		nodecoloratt <- "cccndegree"
		nodedegree <- cytoscape.file$cccndegree
		nodebetween <- cytoscape.file$cccnbetween 	}
	maxbetween <- max(nodebetween)
	maxdegree <- 	max(nodedegree)
	size.control.points = maxbetween/sizescale^(8:0)
	setNodeSizeRule (windowobj, nodesizeatt, size.control.points, node.sizes, mode='interpolate')
	color.control.points = maxdegree/colscale^(8:0)
	degree.colors = c ('#0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
	setNodeColorRule (windowobj, nodecoloratt, color.control.points, degree.colors, mode='interpolate')
		# new.style.name = paste(plotcol, noquote("_ratio"),  sep="", collapse=NULL) 
		# if (!(new.style.name %in% getVisualStyleNames(cy)))	{copyVisualStyle(windowobj, 'default', new.style.name)} # bsroken
		# setVisualStyle(windowobj, new.style.name)
		# return(windowobj)
    }
#
fix.baddot <- function(cell) {  
	baddot <-  c("HLA.A","NKX2.1","NME1.NME2")
	goodhyphen <-  c("HLA-A","NKX2-1","NME1-NME2")
		cellv <- unlist(strsplit(as.character(cell), "; "))
		if (any(baddot %in% cellv)) {
		cellv.new <- gsub(baddot[baddot %in% cellv], goodhyphen[baddot %in% cellv], cellv)	
		return (paste(cellv.new, collapse="; "))
		} else return(cell)		}	 
#
pepgene <- function(peps) unique(sapply(peps,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1]))

#
extract.gene.names <- function (peptide.edgefile)	{
	peps <- c(peptide.edgefile$Gene.1, peptide.edgefile$Gene.2)
	genes <- unique(sapply(peps,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1]))
	return(genes)
}
	
extract.peptides <- function(nodename, edgefile=pepnet.edges) {
	peps.1 <- edgefile$Gene.1[grep(nodename, edgefile$Gene.1, fixed=TRUE)]
	peps.2 <- edgefile$Gene.2[grep(nodename, edgefile$Gene.2, fixed=TRUE)]
	return(unique(c(peps.1, peps.2)))
}

# Function to harmonize cytoscape node attributes
harmonize_cf <- function(pepcf, genecf) {
	 # test for gene nodes missing
	 diffgenes <- setdiff(unique(pepcf$Gene.Name), genecf$Gene.Name)
	 diffrows <- ddply(pepcf[pepcf$Gene.Name %in% diffgenes,], c("Gene.Name", "Approved.Name", "Hugo.Gene.Family", "HPRD.Function", "nodeType", "Domains", "Compartment", "Compartment.Overview"), numcolwise(sum), .drop=FALSE)
	 genecf <- rbind(genecf, diffrows)
	 genecf.new <- data.frame(Peptide.Name= genecf$Gene.Name, genecf)
	 genecf.new $Node.ID <- "protein"
	 pepcf$Node.ID <- "modification site"
	 # Create "parent" attribute for peptide nodes
	 genecf.new$parent <- ""
	 pepcf.new <- pepcf
	 pepcf.new$parent <- pepcf.new$Gene.Name
	 pepgenenodes <- pepcf$Gene.Name
	 # stopifnot(identical(names(genecf.new), names(pepcf.new)), "Attribute names match")  
	# Combine cytoscape node attribute files
	# 
	 combined.cf <- rbind(pepcf.new, genecf.new) 
	 return(combined.cf)
	 } 
harmonize_cfs2 <- function(pepcf, genecf) {
	 genecf.new <- data.frame(Peptide.Name= genecf$Gene.Name, genecf)
	 genecf.new $Node.ID <- "protein"
	 genecf.new$parent <- ""
	 pepcf.new <- pepcf
	 pepcf.new$parent <- pepcf.new$Gene.Name
	 pepcf$Node.ID <- "modification site"
	 cf <- merge(genecf.new, pepcf.new, all=TRUE)
	if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
	 return(cf)
	 }

# Super functions to graph CFN and CCCN
		
connectNodes.1 <- function(nodepair, edgefile= ldgenepepnet.nogen, ig.graph= ldcombined.nogen.ig, newgraph=FALSE)	{
	if (newgraph==TRUE) {
		ig.graph <- graph.data.frame(edgefile, directed=FALSE) }
		sp <- shortest_paths(graph= ig.graph, from=nodepair[1], to=nodepair[2], weights= edgefile$Alt.Weight, mode="all", output="both")
		path.edges <- edgefile[sp$epath[[1]], ]
		ig.df <- get.data.frame(ig.graph, what="both")
		path.nodes <- ig.df[[1]][sp$vpath[[1]],]
		return(list(path.edges, path.nodes))
	}	

# This version uses optimized networks 
connectNodes.2 <- function(nodepair, edgefile= optimized.ppi.nogenetic, ig.graph= optimized.ppi.nogenetic.ig, newgraph=FALSE)	{
	if (newgraph==TRUE) {
		ig.graph <- graph.data.frame(edgefile, directed=FALSE) }
		sp <- shortest_paths(graph= ig.graph, from=nodepair[1], to=nodepair[2], weights= edgefile$Alt.Weight, mode="all", output="both")
		path.edges <- edgefile[sp$epath[[1]], ]
		ig.df <- get.data.frame(ig.graph, what="both")
		path.nodes <- ig.df[[1]][sp$vpath[[1]],]
		return(list(path.edges, path.nodes))
	}
# This version is for loading any network
connectNodes <- function(nodepair, ig.graph=NULL, edgefile, newgraph=FALSE)	{
     if (newgraph==TRUE) {
          ig.graph <- graph.data.frame(edgefile, directed=FALSE) 
     }
     sp <- shortest_paths(graph= ig.graph, from=nodepair[1], to=nodepair[2], weights= edgefile$Alt.Weight, mode="all", output="both")
     path.edges <- edgefile[sp$epath[[1]], ]
     ig.df <- get.data.frame(ig.graph, what="both")
     path.nodes <- ig.df[[1]][sp$vpath[[1]],]
     return(list(path.edges, path.nodes))
}
# connectNodes.all  uses all_shortest_paths and returns just the edge file
connectNodes.all <- function(nodepair, ig.graph=NULL, edgefile, newgraph=FALSE)	{
     if (newgraph==TRUE) {
          ig.graph <- graph.data.frame(edgefile, directed=FALSE) }
     sp <- all_shortest_paths(graph= ig.graph, from=nodepair[1], to=nodepair[2], mode="all")
     path.nodeslist <-  unique(lapply(sp[[1]], names))
     edges.list <- lapply(path.nodeslist, filter.edges.0, edge.file=edgefile)
     path.edges <- unique(remove.autophos(ldply(edges.list)))
     return(path.edges)
}
# Functions to make edge files and igraph graphs:
make.ptm.edgefile <- function(CorThresh, cccn)	{
     cccn.lim <- replace (cccn, abs(cccn)<CorThresh, NA)
     cccn.lim.NA <- which(is.na(cccn.lim), arr.ind = TRUE)
     cccn.lim0 <- replace (cccn.lim, cccn.lim.NA, 0)	
     # peptides igraph network
     cccnnet.g <- graph.adjacency(as.matrix(cccn.lim0), mode="lower", diag=FALSE, weighted="Weight") 
     cccnnet.edges <- data.frame(as_edgelist(cccnnet.g))	
     cccnnet.edges$Weight <- edge_attr(cccnnet.g)[[1]]
     cccnnet.edges$edgeType <- "correlation" 
     cccnnet.edges $edgeType[cccnnet.edges$Weight<=-0.5] <- "negative correlation"
     cccnnet.edges $edgeType[cccnnet.edges$Weight>=0.5] <- "positive correlation"
     negs <- cccnnet.edges[cccnnet.edges$Weight<0,'Weight']
     frnegs <- exp (negs*20)
     # plot (negs,frnegs)
     cccnnet.edges$Alt.Weight <- cccnnet.edges $Weight
     cccnnet.edges[cccnnet.edges$Weight<0,'Alt.Weight'] <- frnegs
     names(cccnnet.edges)[1:2] <- c("Gene.1", "Gene.2")
     return(list(cccnnet.g, cccnnet.edges))
}
make.gene.cccn <- function(CorThresh, ptmcccn) {
     cccn.lim  <- replace (ptmcccn, abs(ptmcccn)< CorThresh, NA)
     cccn.lim0 <- cccn.lim
     cccn.lim.NA <- which(is.na(cccn.lim), arr.ind = TRUE)
     cccn.lim0 <- replace (cccn.lim0, cccn.lim.NA, 0)	
     hist(unlist(cccn.lim0),breaks=1000, xlim=c(-1, 1), ylim=c(0, 100)) 
     gene.cccn <- data.frame(cccn.lim)
     gene.cccn $Gene.Name <- sapply(rownames(gene.cccn), function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
     gene.cccn[lower.tri(gene.cccn)] <- NA	
     gene.cccn2 <- ddply(gene.cccn, .(Gene.Name), numcolwise(function(x) sum(x, na.rm=T)), .progress = "tk")
     rownames(gene.cccn2) <- gene.cccn2$Gene.Name
     gene.cccn2 <- gene.cccn2[, 2:ncol(gene.cccn2)]
     gene.cccn2 <- data.frame(t(gene.cccn2))
     gene.cccn2$Gene <- sapply(rownames(gene.cccn2), function (x) unlist(strsplit(x, ".",  fixed=TRUE))[1])
     gene.cccn3 <- ddply(gene.cccn2, .(Gene), numcolwise(function(x) sum(x, na.rm=T)), .progress = "tk")
     gene.cccn0 <- gene.cccn3[,2:ncol(gene.cccn3)]
     rownames(gene.cccn0) <- colnames(gene.cccn0)
     genenet.g <- graph.adjacency(as.matrix(gene.cccn0), mode="lower", diag=FALSE, weighted="Weight")
     genecccn.edges <- data.frame(as_edgelist(genenet.g))
     names(genecccn.edges) <- c("Gene.1", "Gene.2")
     genecccn.edges$Weight <- edge_attr(genenet.g)[[1]]
     genecccn.edges$edgeType <- "correlation" 
     genecccn.edges $edgeType[genecccn.edges$Weight<=-0.5] <- "negative correlation"
     genecccn.edges $edgeType[genecccn.edges$Weight>=0.5] <- "positive correlation"
     negs <- genecccn.edges[genecccn.edges$Weight<0,'Weight']
     frnegs <- exp (negs*20)
     # plot (negs,frnegs)
     genecccn.edges$Alt.Weight <- genecccn.edges $Weight
     genecccn.edges[genecccn.edges$Weight<0,'Alt.Weight'] <- frnegs	
     # genecccn.edges[grep("NKX", genecccn.edges$Gene.1),]
     # bad dot here
     # fix edge files and node names
     genecccn.edges$Gene.1 <- sapply(genecccn.edges$Gene.1, fix.baddot)	
     genecccn.edges$Gene.2 <- sapply(genecccn.edges$Gene.2, fix.baddot)
     return(list(genenet.g, genecccn.edges))
}  
	
# This version uses the output from connectNodes = connectnet
graphNetworkPath <- function(nodenames, connectnet, datacolumns=names(ld.fc), network= ldgenepepnet.nogen, merged=FALSE)	{
	net <- connectnet
		# Get peptides from this network
		netpeps <- unlist(sapply(net[[2]], extract.peptides, edgefile=pepnet.half.edges))  
		# filter.edges.0(netpeps, pepnet.edges) # use 0.5 now
	net.cccn <-	filter.edges.0(netpeps, pepnet.half.edges) 
	nf <- list()
	netpath <- net[[2]]
	for (i in 1:(length(netpath)-1)) {
		nf[[i]] <- filter.edges.0(c(netpath[i], netpath[i+1]), network)
		}
	net.full <- ldply(nf)
	net.gene.cf <- make.netcf(edge.df=net.full, data.file=ldgene.fc[, datacolumns], atts=netatts, func.key=func.key, use=c("total", "mean", "median"))
	net.pep.cf <- make.netcf(edge.df=net.cccn, data.file=ld.fc[, datacolumns], atts= pepcccnatts, func.key=func.key, use=c("total", "mean", "median"))
	# combine node attribute cytoscape file
	net.cf <- harmonize_cfs2(pepcf=net.pep.cf, genecf=net.gene.cf)
	# make gene-peptide edges
	net.gpe <- genepep.edges (net.cccn)
	# combine edge files
	net.edges <- rbind(net.gpe, net.cccn, net.full)
	# Graph in RCy3
	net.g <- cyPlot(net.cf, net.edges)
	net.w <- CytoscapeWindow(paste(nodenames,  collapse=" to "), net.g)
	displayGraph(net.w)
	layoutNetwork(net.w, "genemania-force-directed")
    edgeDprops(net.w)
	# setEdgeLineWidthRule(net.w, edge.attribute.name="Weight", attribute.values=net.edges$Weight, line.widths=net.edges$Weight)
     ratioprops(net.w, net.cf, "Total")  
       showGraphicsDetails(net.w, TRUE) 
    nodeDprops.new(net.w, net.cf, merged=merged)
    return(list(net, net.cccn, net.gene.cf, net.pep.cf, net.cf, net.edges, net.w))   
	}
#
graphNetworks <- function(cfn, cccn, windowname, datacolumns=names(ld.fc), merged=FALSE)	{
	net.gene.cf <- make.netcf(edge.df=cfn, data.file=ldgene.fc[, datacolumns], atts=netatts, func.key=func.key, use=c("total", "mean", "median"))
	net.pep.cf <- make.netcf(edge.df=cccn, data.file=ld.fc[, datacolumns], atts= pepcccnatts, func.key=func.key, use=c("total", "mean", "median"))
	# combine node attribute cytoscape file
	net.cf <- harmonize_cfs2(pepcf=net.pep.cf, genecf=net.gene.cf)
	# make gene-peptide edges
	net.gpe <- genepep.edges (cccn)
	# combine edge files
	net.edges <- rbind(net.gpe, cccn, cfn)
	# Graph in RCy3
	net.g <- cyPlot(net.cf, net.edges)
	net.w <- CytoscapeWindow(windowname, net.g)
	displayGraph(net.w)
	layoutNetwork(net.w, "genemania-force-directed")
    edgeDprops(net.w)
	setEdgeLineWidthRule(net.w, edge.attribute.name="Weight", attribute.values=net.edges$Weight, line.widths=10*net.edges$Weight)
     ratioprops(net.w, net.cf, "Total")  
       showGraphicsDetails(net.w, TRUE) 
    nodeDprops.new(net.w, net.cf, merged=merged)
    return(list(net.cf, net.edges, net.g, net.w))   
	}
#			  
# To find and graph heat map data of intersecting node pair
coCluster <- function(nodes, clust.list=esp.pmd.ro.f) 	{
	clusts1 <- which(unlist(lapply(clust.list, function (x) any(grepl(x, pattern=nodes[1], fixed=TRUE)))))
	clusts2 <- which(unlist(lapply(clust.list, function (x) any(grepl(x, pattern=nodes[2], fixed=TRUE)))))	
	overlap <- intersect(names(unlist(clusts1)), names(unlist(clusts2)))
	overlap.mods <- clust.list[overlap]
	overlap.mods <- overlap.mods[which(lapply(overlap.mods, length)>0)]
	return(overlap.mods)
}
graph.coCluster <- function(nodes, clust.list=esp.pmd.ro.f) 	{
	clusts1 <- which(unlist(lapply(clust.list, function (x) any(grepl(x, pattern=nodes[1], fixed=TRUE)))))
	clusts2 <- which(unlist(lapply(clust.list, function (x) any(grepl(x, pattern=nodes[2], fixed=TRUE)))))	
	overlap <- intersect(names(unlist(clusts1)), names(unlist(clusts2)))
	overlap.mods <- clust.list[overlap]
	overlap.mods <- overlap.mods[which(lapply(overlap.mods, length)>0)]
	if(length(overlap.mods) > 1) {
	for (i in 1:length(overlap.mods)) {
		overlap.data[[i]] <- graph.clust6d.l(clust.data.from.vector(overlap.mods[[i]], ld.fc))}
		} else {
			overlap.mods <- clust.list[[overlap]]
			overlap.data <- graph.clust6d.l(clust.data.from.vector(overlap.mods, ld.fc))}
	return(overlap.data)
}
	
#	These two functions are generally useful for setting node size by any attribute
# 		Setting sizescale modifies how midrange values are visualized
graphNodeSize <- function(windowobj, cf, plotcol="No.Modifications", sizescale=1.5) {
	if(!(plotcol %in% getNodeAttributeNames(windowobj)))	{
		print (getNodeAttributeNames (windowobj))
		cat("\n","\n","\t", "Which attribute will set node size and color?")
		plotcol <- as.character(readLines(con = stdin(), n = 1))		}
	node.sizes     = c (25, seq(30, 140, length.out=9), 150)
	nodeweights <- cf[, plotcol]	
	maxweight <- 	max(nodeweights)
	size.control.points = maxweight/sizescale^(8:0)
	setNodeSizeRule (windowobj, plotcol, size.control.points, node.sizes, mode='interpolate')
		}

graphNodeColor <- function(windowobj, cf, plotcol="Total", colscale=2){
	if(!(plotcol %in% getNodeAttributeNames(windowobj)))	{
		print (getNodeAttributeNames (windowobj))
		cat("\n","\n","\t", "Which attribute will set node size and color?")
		plotcol <- as.character(readLines(con = stdin(), n = 1))		}
	setVisualStyle (windowobj, "default")		
	nodecoloratt <- plotcol
	nodecoldata <- cf[, plotcol]
	maxdata <- max(nodecoldata)
	color.control.points = maxdata/colscale^(8:0)
	between.colors = c ('#0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
	setNodeColorRule (windowobj, nodecoloratt, color.control.points, between.colors, mode='interpolate')
}
#
# Pattern detection  functions
decreasing <- function(x)	{
		dpattern <- vector()
		for(i in 1:length(x)-1) dpattern[i]=(x[i+1]<x[i])
		return(dpattern)
		}
#				
splitPattern <- function(tbl){
	pat.t <- apply(tbl, 1, decreasing)
	pat <- t(pat.t)
	patz <- data.frame(pat)
	names(patz) <- paste("col", 1:ncol(patz), sep="")
	patz$vector <- apply(patz, 1, function (x) paste(x, collapse=", "))
	patz$Peptide.Name <- rownames(patz)
	peptidelist <- dlply(patz, .(vector), "[[", "Peptide.Name")
	pnames <- names(peptidelist)
	pnames <- gsub("TRUE", "down", pnames)
	pnames <- gsub("FALSE", "up", pnames)
	names(peptidelist) <- pnames
	return(peptidelist)
}
netattProps3 <- function (windowobj, cytoscape.file, use="norm", colscale=2, sizescale=1.2) {
	node.sizes     = c (25, seq(30, 140, length.out=9), 150)
	setVisualStyle (windowobj, "default")	
	if(use=="norm")		{
		nodesizeatt <- "norm.ppibetween" 
		nodecoloratt <- "ppidegree"
		nodedegree <- cytoscape.file$ppidegree
		nodebetween <- cytoscape.file$norm.ppibetween }	
	if(use=="ppi")		{
		nodesizeatt <- "ppibetween" 
		nodecoloratt <- "ppidegree"
		nodedegree <- cytoscape.file$ppidegree
		nodebetween <- cytoscape.file$ppibetween }	
	if(use=="cccn")		{
		nodesizeatt <- "cccnbetween" 
		nodecoloratt <- "cccndegree"
		nodedegree <- cytoscape.file$cccndegree
		nodebetween <- cytoscape.file$cccnbetween 	}
	maxbetween <- max(nodebetween)
	maxdegree <- 	max(nodedegree)
	size.control.points = maxbetween/sizescale^(8:0)
	setNodeSizeRule (windowobj, nodesizeatt, size.control.points, node.sizes, mode='interpolate')
	color.control.points = maxdegree/colscale^(8:0)
	degree.colors = c ('#0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
	setNodeColorRule (windowobj, nodecoloratt, color.control.points, degree.colors, mode='interpolate')
		# new.style.name = paste(plotcol, noquote("_ratio"),  sep="", collapse=NULL) 
		# if (!(new.style.name %in% getVisualStyleNames(cy)))	{copyVisualStyle(windowobj, 'default', new.style.name)} # bsroken
		# setVisualStyle(windowobj, new.style.name)
		# return(windowobj)
    }


 make.netcf3 <- function(edge.df, data.file, geneatts=netatts2, pepatts=pepcccnatts, func.key=func.key, use=c("total", "mean", "median", "max")) {
	cf.nodes <- unique(c(as.character(edge.df[,1]), as.character(edge.df[,2])))
	cf.data <- data.frame(data.file[rownames(data.file) %in% cf.nodes,])
	if (any(grepl("Total", names(cf.data)))) {
		cf.data <- cf.data[, -grep("Total",names(cf.data))] }	
	cf <- data.frame(Peptide.Name=rownames(cf.data))
	cf$Peptide.Name <- as.character(cf$Peptide.Name)
	cf$Gene.Name <- sapply(cf$Peptide.Name,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	if(identical(cf$Peptide.Name, cf$Gene.Name)) {genenodes=TRUE} else {genenodes=FALSE}
	if(genenodes==FALSE)		{
		cf$node <- "gene"
		cf[which(cf$Peptide.Name!=cf$Gene.Name), "node"] <- "peptide"	}
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
		cf$ppidegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppidegree.s"]
		# cf$ppidegree.m <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppidegree.m"]
		cf$cccndegree <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "cccndegree"]	
		cf$ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppibetween.s"]
		# cf$ppibetween.m <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "ppibetween.m"]
		cf$cccnbetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "cccnbetween"]
		cf$norm.ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "norm.ppibetween"]
			}
	if (genenodes==FALSE) {
		# split between gene and peptide nodes
		cf.list <- dlply(cf, .(node))
		cf.genes <- cf.list$gene
		  cf.genes$No.Samples <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "No.Samples"]
		  cf.genes$No.Modifications <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "No.Modifications"]
		  cf.genes$ppidegree <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppidegree.s"]
		  # cf.genes$ppidegree.m <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppidegree.m"]
		  cf.genes$cccndegree <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "cccndegree"]	
		  cf.genes$ppibetween <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppibetween.s"]
		  # cf.genes$ppibetween.m <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "ppibetween.m"]
		  cf.genes$cccnbetween <- geneatts[geneatts $Gene.Name %in% cf.genes$Gene.Name, "cccnbetween"]
		cf$norm.ppibetween <- geneatts[geneatts $Gene.Name %in% cf$Gene.Name, "norm.ppibetween"]
		cf.peptide <- cf.list$peptide
		# cf <- cf[,c(2,1,3:dim(cf)[2])]
	 	  cf.peptide$No.Samples <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "No.Samples"]	
		  cf.peptide$No.Modifications <- 1		
		  cf.peptide$ppidegree <- 1
		  # cf.peptide$pepcccndegree <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pepcccndegree"]	
		  cf.peptide$cccndegree <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pephalfcccndegree"]	
		  # cf.peptide$pepcccnbetween <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pepcccnbetween"]
		  cf.peptide$ppibetween <- 0
		  cf.peptide$cccnbetween <- pepatts[pepatts $Peptide.Name %in% cf.peptide$Peptide.Name, "pepcccnhalfbetween"]
		  # Node names need to be in first column
		  cf.peptide$Gene.Name <- cf.peptide$Peptide.Name
		cf <-  rbind(cf.genes, cf.peptide) }
	if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
	return(cf)
 }
make.CFN <- function (pepnet.cccn, CorThresh)	{		
  # Filter pepnet.cccn by CorThresh
  if(any(is.na(pepnet.cccn))) 	{pepnet.cccn[is.na(pepnet.cccn)] <- 0	}
  pepnet.cccn.lim <- replace (pepnet.cccn, abs(pepnet.cccn)< CorThresh, 0)
	# use igraph function: graph.adjacency(); must convert data.frame into matrix
	# mode = "lower" returns correct values, including negative, with appropriate min, max
	pepnet.graph <- graph.adjacency(as.matrix(pepnet.cccn.lim), mode="lower", diag=FALSE, weighted="Weight") 
	# hist(edge_attr(pepnet.graph)[[1]], breaks=1000, col="magenta", xlim=c(-1,1))
	pepnet.cccn.edges <- data.frame(as_edgelist(pepnet.graph))	
	# pepnet.cccn.edges$Weight <- edge_attr(pepnet.graph)[[1]]
	# pepnet.cccn$edgeType <- "correlation" 
	# hist(pepnet.cccn$Weight, breaks=1000, col="blue")
	# now convert into gene name
	pepnet.cccn.edges$Gene.1 <- sapply(pepnet.cccn.edges[,1], function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	pepnet.cccn.edges$Gene.2 <- sapply(pepnet.cccn.edges[,2], function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
	genenet.cccn <- unique(pepnet.cccn.edges[, c("Gene.1", "Gene.2")])  		
	# PPI edges may be in reverse orientation
	genenet.cccn2 <- genenet.cccn[, c("Gene.2", "Gene.1")]
	names(genenet.cccn2) <- c("Gene.1", "Gene.2")
	genenet.cccn <- rbind(genenet.cccn, genenet.cccn2)
	#
	# Filter String, GeneMANIA edges
	ld.str.cfn <- merge(genenet.cccn, ld.str.edges, by=c("Gene.1", "Gene.2"))
	ld.GM.cfn <- merge(genenet.cccn, ld.GM.edges, by=c("Gene.1", "Gene.2"))
	# Pathway commons, kinsub, bioplex
	ld.pcnet.cfn <- merge(genenet.cccn, pcnet[,1:4], by=c("Gene.1", "Gene.2"))
	ld.kinsub.cfn	 <- merge(genenet.cccn, kinsub, by=c("Gene.1", "Gene.2"))
	ld.bioplex.cfn <- merge(genenet.cccn, bioplex, by=c("Gene.1", "Gene.2"))
	gp <- rbind(ld.str.cfn, ld.GM.cfn, ld.pcnet.cfn, ld.kinsub.cfn, ld.bioplex.cfn)
	# NOTE: there are duplicated edges with different weights, particularly in the GM Pysical interactions
	genenet.cfn <- ddply(gp, .(Gene.1, Gene.2, edgeType), numcolwise(sum), .drop=TRUE)
	return(genenet.cfn)
}
additive.CFN1 <- function(pepnet.cccn, CorThresh){
  if(any(is.na(pepnet.cccn))) 	{pepnet.cccn[is.na(pepnet.cccn)] <- 0	}
  # make gene cccn first
  genenet.cccn <- data.frame(pepnet.cccn)
  genenet.cccn$Gene.Name <- sapply(rownames(genenet.cccn), function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
  # length(unique(genenet.cccn$Gene.Name))   # 2639 for pepnet.cccn.lim
  # Take lower triangle to not duplicate values
  genenet.cccn[lower.tri(genenet.cccn)] <- 0  
  genenet.cccn2 <- ddply(genenet.cccn, .(Gene.Name), numcolwise(function(x) sum(abs(x))))
  # Did this on server, much faster at ddply step
  rownames(genenet.cccn2) <- genenet.cccn2$Gene.Name
  genenet.cccn2 <- genenet.cccn2[, 2:ncol(genenet.cccn2)]
  genenet.cccn2 <- data.frame(t(genenet.cccn2))
  genenet.cccn2$Gene <- sapply(rownames(genenet.cccn2), function (x) unlist(strsplit(x, ".",  fixed=TRUE))[1])
  genenet.cccn3 <- ddply(genenet.cccn2, .(Gene), numcolwise(function(x) sum(abs(x))))
  # dim(genenet.cccn3)  #  2639 2640
  # Check:
  # genenet.cccn3[1:20, 1:8]
  genenet.cccn0 <- genenet.cccn3[,2:ncol(genenet.cccn3)]
  rownames(genenet.cccn0) <- genenet.cccn3$Gene
  if(any(is.na(genenet.cccn0))) 	{genenet.cccn0[is.na(genenet.cccn0)] <- 0}
  # genenet.cccn.lim <- replace (genenet.cccn0, abs(genenet.cccn0)< CorThresh, 0)
  return(genenet.cccn0)
  }
  # check
  # hist(unlist(genenet.cccn0), breaks=1000, xlim=c(-1, 12), ylim=c(0, 100))
# Now do the steps to filter ppi edges - on Mac because server has old version of R incompatible with igraph

additive.CFN2 <- function(genenet.cccn, CorThresh){
  genenet.cccn.lim <- replace (genenet.cccn, abs(genenet.cccn0)< CorThresh, 0)
	genenet.graph <- graph.adjacency(as.matrix(genenet.cccn.lim), mode="lower", diag=FALSE, weighted="Weight") 
	# hist(edge_attr(genenet.graph)[[1]], breaks=1000, col="magenta", xlim=c(-1,1))
	genenet.cccn.edges <- data.frame(as_edgelist(genenet.graph))	
	# genenet.cccn.edges$Weight <- edge_attr(genenet.graph)[[1]]
	# genenet.cccn$edgeType <- "correlation" 
	# hist(genenet.cccn$Weight, breaks=1000, col="blue")
	# already converted into gene name
	names(genenet.cccn.edges) <- c("Gene.1", "Gene.2")
	# PPI edges may be in reverse orientation
	genenet.cccn.edges2 <- genenet.cccn.edges[, c("Gene.2", "Gene.1")]
	names(genenet.cccn.edges2) <- c("Gene.1", "Gene.2")
	genenet.cccn.edges <- rbind(genenet.cccn.edges, genenet.cccn.edges2)
	#
	# Filter String, GeneMANIA edges
	ld.str.cfn <- merge(genenet.cccn.edges, ld.str.edges, by=c("Gene.1", "Gene.2"))
	ld.GM.cfn <- merge(genenet.cccn.edges, ld.GM.edges, by=c("Gene.1", "Gene.2"))
	# Pathway commons, kinsub, bioplex
	ld.pcnet.cfn <- merge(genenet.cccn.edges, pcnet[,1:4], by=c("Gene.1", "Gene.2"))
	ld.kinsub.cfn	 <- merge(genenet.cccn.edges, kinsub, by=c("Gene.1", "Gene.2"))
	ld.bioplex.cfn <- merge(genenet.cccn.edges, bioplex, by=c("Gene.1", "Gene.2"))
	gp <- rbind(ld.str.cfn, ld.GM.cfn, ld.pcnet.cfn, ld.kinsub.cfn, ld.bioplex.cfn)
	# NOTE: there are duplicated edges with different weights, particularly in the GM Pysical interactions
	genenet.cfn <- ddply(gp, .(Gene.1, Gene.2, edgeType), numcolwise(sum), .drop=TRUE)
	return(genenet.cfn)
}

make.avi.CFN <- function (pepnet.cccn, CorThresh)	{		
		  # Filter pepnet.cccn by CorThresh
		  if(any(is.na(pepnet.cccn))) 	{pepnet.cccn[is.na(pepnet.cccn)] <- 0	}
		  pepnet.cccn.lim <- replace (pepnet.cccn, abs(pepnet.cccn)< CorThresh, 0)
		  # use igraph function: graph.adjacency(); must convert data.frame into matrix
		  # mode = "lower" returns correct values, including negative, with appropriate min, max
		  pepnet.graph <- graph.adjacency(as.matrix(pepnet.cccn.lim), mode="lower", diag=FALSE, weighted="Weight") 
		  # hist(edge_attr(pepnet.graph)[[1]], breaks=1000, col="magenta", xlim=c(-1,1))
		  pepnet.cccn.edges <- data.frame(as_edgelist(pepnet.graph))	
		  # pepnet.cccn.edges$Weight <- edge_attr(pepnet.graph)[[1]]
		  # pepnet.cccn$edgeType <- "correlation" 
		  # hist(pepnet.cccn$Weight, breaks=1000, col="blue")
		  # now convert into gene name
		  pepnet.cccn.edges$Gene.1 <- sapply(pepnet.cccn.edges[,1], function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
		  pepnet.cccn.edges$Gene.2 <- sapply(pepnet.cccn.edges[,2], function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1])
		  genenet.cccn <- unique(pepnet.cccn.edges[, c("Gene.1", "Gene.2")])  		
		  # PPI edges may be in reverse orientation
		  genenet.cccn2 <- genenet.cccn[, c("Gene.2", "Gene.1")]
		  names(genenet.cccn2) <- c("Gene.1", "Gene.2")
		  genenet.cccn <- rbind(genenet.cccn, genenet.cccn2)
		  #
		  # Filter Avi's edges
		  ld.avi.cfn <- merge(genenet.cccn, ld.avi.edges, by=c("Gene.1", "Gene.2"))
		  # NOTE: there may be duplicated edges with different weights
		  genenet.cfn <- ddply(ld.avi.cfn, .(Gene.1, Gene.2, edgeType), numcolwise(sum), .drop=TRUE)
		  return(genenet.cfn)
}

# Take an ig graph file and key file and make a netatts file for cytoscape
make.netatts <- function(ig.network, keyfile, ppinet=FALSE){
     cccndegree  <- igraph::degree(ig.network, mode="all", loops=F, normalized=F)
     cccnbetween <- igraph::betweenness(ig.network)
     net.df <- data.frame(cccndegree, cccnbetween)            
     net.df$Peptide.Name <- rownames(net.df) 
     net.df <- net.df[,c(3,1,2)]
     if(ppinet==TRUE){
          names(net.df) <- c("Gene.Name", "ppidegree", "ppibetween")
          allppibetween <- betweenness(ld.all.ppi.ig)
          allppibetween <- allppibetween[sort(names(allppibetween))]
          allbetween.df <- data.frame(allppibetween)
          allbetween.df$Gene.Name <- rownames(allbetween.df)
          net.df <- merge(net.df, allbetween.df, all=TRUE)
          net.df$norm.ppibetween <- net.df$ppibetween/net.df$allppibetween}
     netatts.df <- merge(keyfile, net.df, all=TRUE)
     netatts.df[is.na(netatts.df)] <- 0
     return(netatts.df)
}
#Renames first column, which could be gene or peptide, creates parent relationship
harmonize_cfs3 <- function(pepcf, genecf) {
     genecf.new <- data.frame(Peptide.Name= genecf$Gene.Name, genecf)
     genecf.new$parent <- ""
     genecf.new$Node.ID <- "gene"
     pepcf.new <- pepcf
     pepcf.new$parent <- pepcf.new$Gene.Name
     cf <- merge(genecf.new, pepcf.new, all=TRUE)
     names(cf)[1] <- "Node"
     if(any(is.na(cf))) {cf[is.na(cf)] <- 0}
     return(cf)
}
graphNetworkPath2 <- function(nodenames, connectnet, ptmedgefile, datacolumns=names(ld.fc), ppinetwork, geneatts, ptmcccnatts)	{
     path.edges <- connectnet[[1]]
     path.nodes <- connectnet[[2]]
     # Get peptides from this network
     netpeps <- unlist(sapply(path.nodes, extract.peptides, ptmedgefile))  
     ptm.cccn <-	filter.edges.0(netpeps, ptmedgefile) 
     nf <- list()
     for (i in 1:(length(path.nodes)-1)) {
          nf[[i]] <- filter.edges.0(c(path.nodes[i], path.nodes[i+1]), ppinetwork)
     }
     net.full <- ldply(nf)
     net.full$Alt.Weight <- net.full$Weight
     net.gene.cf <- make.anynetcf(edge.df=net.full, data.file=ldgene.fc[, datacolumns], geneatts=geneatts, ptmcccnatts=ptmcccnatts, func.key=func.key, use=c("total", "mean", "median", "max"))
     net.pep.cf <- make.anynetcf(edge.df=ptm.cccn, data.file=ld.fc[, datacolumns], geneatts=geneatts, ptmcccnatts=ptmcccnatts, func.key=func.key, use=c("total", "mean", "median"))
     # combine node attribute cytoscape file
     net.cf <- harmonize_cfs3(pepcf=net.pep.cf, genecf=net.gene.cf)
     # make gene-peptide edges
     net.gpe <- data.frame(Gene.1=net.pep.cf$Gene.Name, Gene.2=net.pep.cf$Peptide.Name, edgeType="peptide", Weight=1, Alt.Weight=100)
     # net.gpe <- genepep.edges(ptm.cccn)
     # combine edge files
     net.edges <- rbind(net.gpe, ptm.cccn, net.full)
     # Graph in RCy3
     net.g <- cyPlot(net.cf, net.edges)
     net.w <- CytoscapeWindow(paste(nodenames,  collapse=" to "), net.g)
     displayGraph(net.w)
     layoutNetwork(net.w, "genemania-force-directed")
     edgeDprops(net.w)
     # setEdgeLineWidthRule(net.w, edge.attribute.name="Weight", attribute.values=net.edges$Weight, line.widths=net.edges$Weight)
     ratioprops(net.w, net.cf, "Total")  
     showGraphicsDetails(net.w, TRUE) 
     nodeDprops.new(net.w, net.cf)
     return(list(net.full, net.cccn,  net.cf, net.edges, net.w))   
}

# This version starts with the more complete network (edgefile) from all.paths.net
# Note: due to a limitation in graphNEL objects, duplicated edges are not allowed, so they are merged here
# nodenames is just for naming the Cy window
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
     net.w <- CytoscapeWindow(paste(paste(nodenames,  collapse=" to "), 1+length(getWindowList(cy))), net.g)
     displayGraph(net.w)
     layoutNetwork(net.w, "genemania-force-directed")
     #setEdgeLineWidthRule(net.w, edge.attribute.name="Weight", attribute.values=net.edges$Weight, line.widths=net.edges$Weight)
     edgeDprops(net.w)
     ratioprops(net.w, net.cf, "Total")  
     showGraphicsDetails(net.w, TRUE) 
     nodeDprops.new(net.w, net.cf)
     setEdgeWidths.log(net.w, factor=1.2)
     return(list(net.cf, net.edges, net.w))   
}
cloneNodePosition <- function(win1, win2)  {
     win1.pos <- RCy3::getNodePosition(win1, getAllNodes(win1))
     x = as.integer (sapply (win1.pos, function (node.loc) return (node.loc$x)))
     y = as.integer (sapply (win1.pos, function (node.loc) return (node.loc$y)))
     setNodePosition (win2, getAllNodes(win2), x, y)
     fitContent(win2)
}