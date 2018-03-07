##### To UpDATE
# source("http://www.bioconductor.org/biocLite.R")
# biocLite("RCytoscape") 
library(RCy3)
options(stringsAsFactors=FALSE)
# 
#
# Functions for RCytoscape plots
#
# requires: library(RCytoscape) or RCy3
 	cy <-  CytoscapeConnection ()
#
#  Function to load the primary cytoscape file
#
#		cytofilename <- readLines(con = stdin(), n = 1)  # Test: cytofilename="VignetteNodesCy.txt"
#		cytoscape.file <- read.table(cytofilename, header=TRUE, sep = "\t", na.strings='', fill=TRUE)
#
#  Function to plot the primary cytoscape file
# 	syntax: > mydata<-plot.cy(cytoscape.file)
#
plot.cy <- function (cytoscape.file) {
		mydata <- new("graphNEL", edgemode='directed', nodes=as.character(cytoscape.file[, 1]))
#	Set up and load all the node attributes
	for (i in 2:ncol(cytoscape.file)) {
		if (class(cytoscape.file[1,i]) != "numeric") {
			mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file[i]), attribute.type='char', default.value=0.0)
			nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file[i])) <- as.character(cytoscape.file[,i])		}
		if (class(cytoscape.file[1,i])=="numeric") {	
		mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file[i]), attribute.type='numeric', default.value=0.0) 
		nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file[i])) <- as.numeric(cytoscape.file[,i]) } 
		}
		# load standard edge attributes
		mydata <- initEdgeAttribute (graph= mydata, attribute.name='edgeType', attribute.type='char', default.value='undefined')
 		mydata <- initEdgeAttribute(mydata, attribute.name = "Weight", attribute.type = "numeric", default.value = 0.0)
		return(mydata)	}
		#
# a second version that loads edges too:
plot.cy.2 <- function (cytoscape.file, edge.file) {
		mydata <- new("graphNEL", edgemode='directed', nodes=as.character(cytoscape.file[, 1]))
#	Set up and load all the node attributes
	for (i in 2:ncol(cytoscape.file)) {
		if (class(cytoscape.file[1,i]) != "numeric") {
			mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file[i]), attribute.type='char', default.value=0.0)
			nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file[i])) <- as.character(cytoscape.file[,i])		}
		if (class(cytoscape.file[1,i])=="numeric") {	
		mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file[i]), attribute.type='numeric', default.value=0.0) 
		nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file[i])) <- as.numeric(cytoscape.file[,i]) } 
		}
		# load standard edge attributes
		mydata <- initEdgeAttribute (graph= mydata, attribute.name='edgeType', attribute.type='char', default.value='undefined')
 		mydata <- initEdgeAttribute(mydata, attribute.name = "Weight", attribute.type = "numeric", default.value = 0.0)
 		mydata = addEdge (as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), mydata) 
 		edgeData (mydata, as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), attr='edgeType') <- as.character(edge.file$edgeType)
		edgeData (mydata, as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), attr='Weight') <- edge.file$Weight
		return(mydata)	}
		#
#  This version adds edge nodes that may not be in cytoscape.file:		
plot.cy.3 <- function (cytoscape.file, edge.file) {
	edge.nodes <- unique(c(as.character(edge.file$Gene.1), as.character(edge.file$Gene.2)))		
	mydata <- new("graphNEL", edgemode='directed', nodes = unique(c(as.character(cytoscape.file[, 1]), edge.nodes)))
#	Set up and load all the node attributes
	for (i in 2:ncol(cytoscape.file)) {
		if (class(cytoscape.file[1,i]) != "numeric") {
			mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file[i]), attribute.type='char', default.value=0.0)
			nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file[i])) <- as.character(cytoscape.file[,i])		}
		if (class(cytoscape.file[1,i])=="numeric") {	
		mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file[i]), attribute.type='numeric', default.value=0.0) 
		nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file[i])) <- as.numeric(cytoscape.file[,i]) } 
		}
		# load standard edge attributes
		mydata <- initEdgeAttribute (graph= mydata, attribute.name='edgeType', attribute.type='char', default.value='undefined')
 		mydata <- initEdgeAttribute(mydata, attribute.name = "Weight", attribute.type = "numeric", default.value = 0.0)
 		mydata = addEdge (as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), mydata) 
 		edgeData (mydata, as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), attr='edgeType') <- as.character(edge.file$edgeType)
		edgeData (mydata, as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), attr='Weight') <- edge.file$Weight
		return(mydata)	}
# This version also adds other edge attributes according to class()
plot.cy.4 <- function (cytoscape.file, edge.file) {
	edge.nodes <- unique(c(as.character(edge.file$Gene.1), as.character(edge.file$Gene.2)))		
	mydata <- new("graphNEL", edgemode='directed', nodes = unique(c(as.character(cytoscape.file[, 1]), edge.nodes)))
#	Set up and load all the node attributes
	for (i in 2:ncol(cytoscape.file)) {
		if (class(cytoscape.file[1,i]) != "numeric") {
			mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file)[i], attribute.type='char', default.value='undefined')
			nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file)[i]) <- as.character(cytoscape.file[,i])		}
		if (class(cytoscape.file[1,i])=="numeric") {	
		mydata <- initNodeAttribute (graph=mydata,  attribute.name=names(cytoscape.file)[i], attribute.type='numeric', default.value=0.0) 
		nodeData (mydata, n=as.character(cytoscape.file[, 1]), attr=names(cytoscape.file)[i]) <- as.numeric(cytoscape.file[,i]) } 
		}
 		mydata = addEdge (as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), mydata)
		# load all edge attributes
	for (i in 3:ncol(edge.file)) {
		if (class(edge.file[1,i]) != "numeric") {
		mydata <- initEdgeAttribute (graph= mydata, attribute.name=names(edge.file)[i], attribute.type='char', default.value='undefined')
 		edgeData (mydata, as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), attr=names(edge.file)[i]) <- as.character(edge.file[,i])		}
		if (class(edge.file[1,i])=="numeric"|class(edge.file[1,i])=="integer") {
		 	mydata <- initEdgeAttribute(mydata, attribute.name =names(edge.file)[i], attribute.type = "numeric", default.value = 0)
		edgeData (mydata, as.vector(edge.file$Gene.1, mode="character"), as.vector(edge.file$Gene.2, mode="character"), attr=names(edge.file[i])) <- as.numeric(edge.file[,i])	}	}
		return(mydata)	}
#
# New version to read node and edge attributes according to class()
plot.cy.5 <-  function (node.df, edge.df) {
    edge.nodes <- unique(c(as.character(edge.df[, 1]), as.character(edge.df[, 
        2])))
    mydata <- new("graphNEL", edgemode = "directed", nodes = unique(c(as.character(node.df[, 
        1]), edge.nodes)))
    node.df[, 1] <- as.character(node.df[, 1])
    edge.df[, 1:2] <- sapply(edge.df[, 1:2], as.character)
    node.class <- sapply(node.df, class)
    if (any(grep("factor", node.class))) {
        node.df[, grep("factor", node.class)] <- sapply(node.df[, 
            grep("factor", node.class)], as.character)
    }
    if (any(grep("integer", node.class))) {
        node.df[, grep("integer", node.class)] <- sapply(node.df[, 
            grep("integer", node.class)], as.numeric)
    }
    node.class <- sapply(node.df, class)
    edge.class <- sapply(edge.df, class)
    if (any(grep("factor", edge.class))) {
        edge.df[, grep("factor", edge.class)] <- sapply(edge.df[, 
            grep("factor", edge.class)], as.character)
    }
    edge.class <- sapply(edge.df, class)
    if (dim(node.df)[2] > 2) {
    for (i in 2:length(grep("character", node.class))) {
        mydata <- initNodeAttribute(graph = mydata, attribute.name = names(node.class[grep("character", 
            node.class)])[i], attribute.type = "char", default.value = "undefined")
        nodeData(mydata, n = as.character(node.df[, 1]), attr = names(node.class[grep("character", 
            node.class)])[i]) <- as.character(node.df[, grep("character", 
            node.class)[i]])
    }
    for (i in 1:length(grep("numeric", node.class))) {
        mydata <- initNodeAttribute(graph = mydata, attribute.name = names(node.class[grep("numeric", 
            node.class)])[i], attribute.type = "numeric", default.value = 0)
        nodeData(mydata, n = as.character(node.df[, 1]), attr = names(node.class[grep("numeric", 
            node.class)])[i]) <- as.numeric(node.df[, grep("numeric", 
            node.class)[i]])
    }}
    mydata = addEdge(as.vector(edge.df[, 1], mode = "character"), 
        as.vector(edge.df[, 2], mode = "character"), mydata)
    if (dim(edge.df)[2] > 2) {
    for (i in 3:length(grep("character", edge.class))) {
        mydata <- initEdgeAttribute(graph = mydata, attribute.name = names(edge.df[, 
            grep("character", edge.class)])[i], attribute.type = "char", 
            default.value = "undefined")
        edgeData(mydata, as.vector(edge.df[, 1], mode = "character"), 
            as.vector(edge.df[, 2], mode = "character"), attr = names(edge.df[, 
                grep("character", edge.class)])[i]) <- as.character(edge.df[, 
            grep("character", edge.class)[i]])
    }
    for (i in 1:length(grep("numeric", edge.class))) {
        mydata <- initEdgeAttribute(mydata, attribute.name = names(edge.class[grep("numeric", 
            edge.class)])[i], attribute.type = "numeric", default.value = 0)
        edgeData(mydata, as.vector(edge.df[, 1], mode = "character"), 
            as.vector(edge.df[, 2], mode = "character"), attr = names(edge.class[grep("numeric", 
                edge.class)])[i]) <- as.numeric(edge.df[, grep("numeric", 
            edge.class)[i]])
            }
    }
    return(mydata)
}# END plot.cy.5	
#
loadedges.1 <- function (Cy_Wind, Sep_Edges) {
	filename=NULL
	for(i in 1:length(Sep_Edges))	{
		filename[i] <- paste(edgeTypes[i], noquote("edges"), sep=" ", collapse=NULL)
		edgefile <- data.frame(Sep_Edges[i])
		names(edgefile) <- names(sgp.combined)
		nmg2 <- new("graphNEL", nodes= nmgnodes, edgemode='undirected')
 		nmg2 <- initEdgeAttribute (graph=nmg2, attribute.name='edgeType', attribute.type='char', default.value='undefined')
 		nmg2 <- initEdgeAttribute(nmg2, attribute.name = "Weight", attribute.type = "numeric", default.value = 0.0)
		nmg2 = addEdge (as.vector(edgefile$Gene.1, mode="character"), as.vector(edgefile$Gene.2, mode="character"), nmg2) 
		edgeData (nmg2, as.vector(edgefile$Gene.1, mode="character"), as.vector(edgefile$Gene.2, mode="character"), attr='edgeType') <- as.character(edgefile$edgeType)
		edgeData (nmg2, as.vector(edgefile$Gene.1, mode="character"), as.vector(edgefile$Gene.2, mode="character"), attr='Weight') <-edgefile$Weight
		addGraphToGraph (Cy_Wind, nmg2)	
		redraw(Cy_Wind) 	
		cat("\n", filename[i], "\n") }
		cat("\n", filename) 	}
#
	load.an.edge <- function (Cy_Wind, Network.File) {
			filename=NULL
			 a = as.character(Network.File$Gene.1)
			 b = as.character(Network.File $Gene.2)
			nodenames <- unique(c(a,b))			
			edgeTypes <- levels(as.factor(Network.File$edgeType))
			subgraph2 <- new("graphNEL", nodes= nodenames, edgemode='undirected')
			subgraph2 <- initEdgeAttribute (graph= subgraph2, attribute.name='edgeType', attribute.type='char', default.value='undefined')
 			subgraph2 <- initEdgeAttribute(subgraph2, attribute.name = "Weight", attribute.type = "numeric", default.value = 0.0)
			subgraph2 = addEdge (as.vector(Network.File$Gene.1, mode="character"), as.vector(Network.File$Gene.2, mode="character"), subgraph2) 
			edgeData (subgraph2, as.vector(Network.File$Gene.1, mode="character"), as.vector(Network.File$Gene.2, mode="character"), attr='edgeType') <- as.character(Network.File$edgeType)
			edgeData (subgraph2, as.vector(Network.File$Gene.1, mode="character"), as.vector(Network.File$Gene.2, mode="character"), attr='Weight') <- Network.File$Weight
			addGraphToGraph (Cy_Wind, subgraph2)	
			redraw(Cy_Wind) 	
			showGraphicsDetails(Cy_Wind, TRUE)
			}
			###
#
	loadedges <- function (Cy_Wind, Network.File) {
			filename=NULL
			 a = as.character(Network.File$Gene.1)
			 b = as.character(Network.File $Gene.2)
			nodenames <- unique(c(a,b))			
			edgeTypes <- levels(as.factor(Network.File$edgeType))
			sepedges <- dlply(Network.File, .(edgeType)) 
			subgraph2 <- new("graphNEL", nodes= nodenames, edgemode='undirected')
			subgraph2 <- initEdgeAttribute (graph= subgraph2, attribute.name='edgeType', attribute.type='char', default.value='undefined')
 			subgraph2 <- initEdgeAttribute(subgraph2, attribute.name = "Weight", attribute.type = "numeric", default.value = 0.0)
			#
			for(i in 1:length(sepedges))	{
				filename[i] <- paste(edgeTypes[i], noquote("edges"), sep=" ", collapse=NULL)
				edgefile <- data.frame(sepedges[i])
				names(edgefile) <- names(Network.File)
				subgraph2 = addEdge (as.vector(edgefile$Gene.1, mode="character"), as.vector(edgefile$Gene.2, mode="character"), subgraph2) 
				edgeData (subgraph2, as.vector(edgefile$Gene.1, mode="character"), as.vector(edgefile$Gene.2, mode="character"), attr='edgeType') <- as.character(edgefile$edgeType)
				edgeData (subgraph2, as.vector(edgefile$Gene.1, mode="character"), as.vector(edgefile$Gene.2, mode="character"), attr='Weight') <-edgefile$Weight
				addGraphToGraph (Cy_Wind, subgraph2)	
				redraw(Cy_Wind) 	
				cat("\n", filename[i], "\n") 
					}
				cat("\n", filename) 	
				showGraphicsDetails(Cy_Wind, TRUE)
				}
				#
#				
plot.Ratio <- function (plotcolnumber)	{
	cydata <- new.CytoscapeWindow (names(cytoscape.file[plotcolnumber]), graph=mydata)
		setVisualStyle (cydata, "default")
	node.sizes     = c (135, 130, 108, 75, 35, 75, 108, 130, 135)
	setDefaultNodeShape(cydata, "ellipse")
	setDefaultNodeColor (cydata, '#C9C9C9') # gray 79
	setDefaultNodeSize  (cydata, 30) # for grey non-data nodes
	setDefaultNodeFontSize(cydata, 18)
	setDefaultNodeBorderWidth (cydata, 1.8)
	setDefaultNodeBorderColor (cydata, '#888888')  # gray 
	setDefaultEdgeLineWidth (cydata, 3)
    setDefaultEdgeColor (cydata, '#FFFFFF')  # white
    #
    		#	RATIO is plotted
			#	Blue is negative: Yellow positive, Green in middle
			#   Note - can plot ratio or ratio.N (normalized)
			#
		size.control.points = c (-50.0, -15.0, -5.0, 0.0, 5.0, 15.0, 50.0)
		color.control.points = c (-50.0, -10.0, -5.0, -2.25, 0.0, 2.25, 5.0, 10.0, 50.0)
		ratio.colors = c ('0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
		displayGraph (cydata)
		layoutNetwork (cydata, 'grid') 
		setNodeColorRule (cydata, names(cytoscape.file[plotcolnumber]), color.control.points, ratio.colors, mode='interpolate')
		setNodeSizeRule (cydata, names(cytoscape.file[plotcolnumber]), size.control.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (cydata,  "#CC00FF") 
		new.style.name = 'ratio.heat.map'
		return(new.style.name)
		return(cydata)
    }	
   #
plot.Intensity <- function (plotcolnumber) {
	cydata <- new.CytoscapeWindow (names(cytoscape.file[plotcolnumber]), graph=mydata)
		setVisualStyle (cydata, "default")
	node.sizes     = c (135, 130, 108, 75, 35, 75, 108, 130, 135)
	setDefaultNodeShape(cydata, "ellipse")
	setDefaultNodeColor (cydata, '#C9C9C9') # gray 79
	setDefaultNodeSize  (cydata, 30) # for grey non-data nodes
	setDefaultNodeFontSize(cydata, 18)
	setDefaultNodeBorderWidth (cydata, 1.8)
	setDefaultNodeBorderColor (cydata, '#888888')  # gray 
	setDefaultEdgeLineWidth (cydata, 3)
    setDefaultEdgeColor (cydata, '#888888')  # gray
    #
    		#	INTENSITY is plotted
			#   For intensity, white in middle
			# 	and fraction of max value is biggest
			#
		Intensity.Values <- cytoscape.file[plotcolnumber]  # set to intensity or normalized intensity
		maxint <- max(Intensity.Values) 
		minint <- min(Intensity.Values)
		icolors <- c('0099FF', '#007FFF','#00BFFF', '#00CCFF', '00FFFF', '#FFFFFF', '#FFFF7E', '#FFFF00', 'FFE600', 'FFD700', 'FFCC00')
		displayGraph (cydata)
		layoutNetwork (cydata, 'grid') 
		#  Some rules to set the color and size depending on the values of intensity
		if (maxint>abs(minint)) {
			setNodeColorRule (cydata, names(cytoscape.file[plotcolnumber]), c (-(maxint+1), -(maxint/5), -(maxint/10), -(maxint*0.045), 0.0, (maxint*0.045), (maxint/10), (maxint/5), (maxint+1)), icolors, mode='interpolate') 
			icontrol.points = c (-(maxint+1), -(maxint*0.3), -(maxint/10), 0.0, (maxint/10), (maxint*0.3), (maxint+1))
			}
		if (maxint<abs(minint)) {
			setNodeColorRule (cydata, names(cytoscape.file[plotcolnumber]), c ((minint-1), (minint/5), (minint/10), (minint*0.045), 0.0, -(minint*0.045), -(minint/10), -(minint/5), -(minint-1)), icolors, mode='interpolate') 
			icontrol.points = c ((minint-1), (minint*0.3), (minint/10), 0.0, -(minint/10), -(minint*0.3), abs(minint-1))
			}
		setNodeSizeRule (cydata, names(cytoscape.file[plotcolnumber]), icontrol.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (cydata,  "#CC00FF") 
		displayGraph (cydata)
		layoutNetwork (cydata, 'grid') 
		new.style.name = 'intensity.heat.map'
		return(new.style.name)
		return(cydata)
 }   
#
plot.StringCS <- function (stringfilename) {
			cat("\n","\n","\t", "STRING file name?","\n","\t")
		stringfilename <- readLines(con = stdin(), n = 1)  # Test stringfilename <- "VignetteNodesString.txt"
		stnet <- read.table(stringfilename, header=TRUE, sep = "\t", comment.char = "", na.strings='', stringsAsFactors=FALSE, fill=TRUE)
			names(stnet)[1]="node1"  # to edit out the 'X.' or "#" 
			# fix up the gene names 
			stnet$node1 <- sapply(stnet$node1, get.gene) 
			stnet$node2 <- sapply(stnet$node2, get.gene)  
			# must make these factors to use levels()
			stnet$node1=as.factor(stnet$node1)
			stnet$node2=as.factor(stnet$node2)	 
			stnodes <- unique(c(levels(stnet[1,1]), levels(stnet[1,2])))
	# 	Prune data file
			stn <- stnet[,c(1:2)]
			names(stn) <- c('Gene.1', 'Gene.2')
			stn$experimental <- stnet$experimental
			stn$knowledge <- stnet$knowledge
			stn$homology <- stnet$homology
			stn$combined_score <- stnet$combined_score  #  skip next one for scale()
			# stn$Str.interaction <- "String" 
	#	Set up graph 		
			 stgraph <- new("graphNEL", nodes=stnodes, edgemode='undirected')
			 stgraph <- initEdgeAttribute (graph= stgraph, attribute.name='edgeType', attribute.type='char', default.value='String')
 			 stgraph <- initEdgeAttribute(stgraph, attribute.name = "homology", attribute.type = 'numeric', default.value = 0.0)
 			 stgraph <- initEdgeAttribute(stgraph, attribute.name = "knowledge", 'numeric', 0.0) 
			 stgraph <- initEdgeAttribute (stgraph, 'experimental', 'numeric', 0.0) 
			 stgraph <- initEdgeAttribute (stgraph, 'combined_score', 'numeric', 0.0) 
			 #
			 stgraph <- addEdge (as.vector(stn$Gene.1, mode="character"), as.vector(stn$Gene.2, mode="character"), stgraph) 
			 edgeData (stgraph, as.vector(stn$Gene.1, mode="character"), as.vector(stn$Gene.2, mode="character"), attr='homology') <- stn$homology
			 edgeData (stgraph, as.vector(stn$Gene.1, mode="character"), as.vector(stn$Gene.2, mode="character"), attr='knowledge') <- stn$knowledge
			 edgeData (stgraph, as.vector(stn$Gene.1, mode="character"), as.vector(stn$Gene.2, mode="character"), attr='experimental') <- stn$experimental
		 	 edgeData (stgraph, as.vector(stn$Gene.1, mode="character"), as.vector(stn$Gene.2, mode="character"), attr='combined_score') <- stn$combined_score
		#	Plot
			unlist(strsplit(stringfilename, ".", fixed = TRUE)) -> x
 			namewindow <- paste(x[1], noquote(' String combined score network'), sep="", collapse=NULL)
			strw <<- new.CytoscapeWindow (namewindow, stgraph)
						 displayGraph (strw)
						 layoutNetwork (strw, 'grid')
			setDefaultEdgeLineWidth (strw, 3)
		    setDefaultBackgroundColor (strw, 'FFFFFF') # white
    		setDefaultEdgeColor (strw, '#000000')  # black
    		setDefaultEdgeSelectionColor(strw, "#88FF00")
    		setDefaultNodeSelectionColor(strw, "#FF3388")
    		edgecolors = c('#008000', '#FF0000', '#0000FF', '#000000') #  green; red; blue; (black not used)
    		edgeTypes <- c( 'homology', 'knowledge', 'experimental', 'combined_score') #  = String interaction types            
    		myarrows <- c ('No Arrow', 'No Arrow', 'No Arrow', 'No Arrow')
			#setEdgeTargetArrowRule(strw, 'edgeType', edgeTypes, myarrows, default='No Arrow')  
    		#setEdgeTargetArrowColorRule(strw, "edgeType",  edgeTypes, edgecolors, default.color='#000000')      
    		#setEdgeSourceArrowColorRule(strw, "edgeType",  edgeTypes, myarrows, default.color='#000000')  
 			setEdgeColorRule(strw, 'edgeType', colors=edgecolors, control.points=edgeTypes, mode='lookup', default.color='#888888')    # 
 			minWeight <- min(stn$combined_score)
			maxWeight <- max(stn$combined_score)
			edge.attribute.values <- as.vector(stn$combined_score)
			weight.scale=scale(edge.attribute.values, scale=T, center=T)
			weight.scale=as.vector(weight.scale)
				minScale <- min(weight.scale, na.rm=T)
				maxScale <- max(weight.scale, na.rm=T)
			scalefactor = maxScale + abs(minScale)
			line.widths <- 1.8*scalefactor*scale(weight.scale + (abs(minScale) + 0.25), scale=(maxScale + scalefactor), center=FALSE)
			line.widths <- as.vector(line.widths) 
 			setEdgeLineWidthRule (strw, 'combined_score', as.character(edge.attribute.values), as.character(line.widths))
			setLayoutProperties (strw, "kamada-kawai-noweight", list (edge_attribute='combined_score', weight_type=3, min_weight=minWeight, max_weight=maxWeight, layout_passes=50, iterations_pernode=50, distance_strength=8008, rest_length=60, disconnected_strength=0.01, disconnected_rest_length=500, anticollisionStrength=10))
  			displayGraph (strw)
			RCytoscape::layoutNetwork (strw,  "kamada-kawai-noweight")
			#
			StringCS.style.name  <- paste(namewindow, noquote('style'), sep=" ", collapse=NULL)
	copyVisualStyle (strw, 'default', StringCS.style.name)
    setVisualStyle (strw, StringCS.style.name)
}
#	
nodeDprops <- function (windowname) {
	node.sizes     = c (135, 130, 108, 75, 35, 75, 108, 130, 135)
	setDefaultBackgroundColor (windowname, '#FFFFFF', 'default') # white
	setDefaultNodeShape(windowname, "ellipse")
	setDefaultNodeColor (windowname, '#C9C9C9') # gray 79
	setDefaultNodeSize  (windowname, 30) # for grey non-data nodes
	setDefaultNodeFontSize(windowname, 18)
	setDefaultNodeBorderWidth (windowname, 1.8)
	setDefaultNodeBorderColor (windowname, '#888888')  # gray 
    setDefaultNodeSelectionColor(windowname, "#FF3388")
    # setNodeLabelRule(windowname, node.attribute.name)
    floatPanel (cy, 'Data Panel')
   }
#
nodeDprops.2 <- function (windowobj, cf) {
	setDefaultBackgroundColor (windowobj, '#7F7F7F', 'default') # grey 50
	setDefaultNodeShape(windowobj, "ELLIPSE")
	setDefaultNodeColor (windowobj, '#E0E0E0') # gray 88
	setDefaultNodeSize  (windowobj, 30) # for grey non-data nodes
	setDefaultNodeFontSize(windowobj, 22)
	setDefaultNodeLabelColor(windowobj, '#000000')  # black
	setDefaultNodeBorderWidth (windowobj, 1.8)
	setDefaultNodeBorderColor (windowobj, '#888888')  # gray 
    setDefaultNodeSelectionColor(windowobj, "#FF3388")
	molclasses <- c("unknown", "receptor tyrosine kinase",  "SH2 protein", "SH3 protein", "tyrosine kinase",  "SRC-family kinase",   "kinase", "transcription factor", "RNA binding protein")
	#  NOTE getNodeShapes(cy) returns node shapes in random order!  Define manually 
	 #	*12 for RCy2; 9 for RCy3
	 # there are now 24 nodeType classes
	nodeshapes <- c("ELLIPSE","ROUND_RECTANGLE", "RECTANGLE", "TRIANGLE", "HEXAGON", "DIAMOND", "OCTAGON", "PARALLELOGRAM", "VEE")
	if(length(sessionInfo()$otherPkgs$RCy3)==0) {
		nodeshapes <- c("ellipse", "round_rect", "rect", "triangle", "hexagon", "diamond", "octagon", "parallelogram", "vee")}
setNodeShapeRule (windowobj, node.attribute.name="nodeType", attribute.values=molclasses, node.shapes=nodeshapes, default.shape="ELLIPSE")
	if (length(cf[grep("RNA", cf$nodeType), 1])>0) {
	setNodeShapeDirect(windowobj, cf[grep("RNA", cf$nodeType), 1], nodeshapes[9])}
	if (length(cf[grep("transcription", cf$nodeType), 1])>0) {
	setNodeShapeDirect(windowobj, cf[grep("transcription", cf$nodeType), 1], nodeshapes[8])}
	if (length(cf[grep("acetyl", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("acetyl", cf$nodeType), 1], "#FF0000")} # red
	if (length(cf[grep("methyl", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("methyl", cf$nodeType), 1], "#005CE6")} # blue
	if (length(cf[grep("membrane", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("membrane", cf$nodeType), 1], "#6600CC")} # purple
	if (length(cf[grep("receptor", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("receptor", cf$nodeType), 1], "#6600CC")} # purple
	if (length(cf[grep("TM", cf$nodeType), 1])>0) {
	setNodeBorderColorDirect(windowobj, cf[grep("TM", cf$Domains), 1], "#6600CC")} # purple
	setNodeBorderWidthRule(windowobj, node.attribute.name="nodeType", attribute.values=c("deacetylase","acetyltransferase","demethylase","methyltransferase","membrane protein", "receptor tyrosine kinase", "G protein-coupled receptor"), line.widths=c(6,10,6,10,7,7,7), default.width=1.8)
	setDefaultNodeSelectionColor (windowobj,  "#CC00FF") 
    # setNodeLabelRule(windowobj, node.attribute.name)
    redraw(windowobj)
   }


edgeDprops.1 <- function(windowname)  {
	setDefaultEdgeLineWidth (windowname, 3)
    setDefaultEdgeColor (windowname, '#888888')  # gray
    setDefaultEdgeSelectionColor(windowname, "#FF6600")
    edgecolors = c('#FF0000', '#FF0000', '#33FFCC', '#008000', '#EE00EE', '#0000FF', '#CC00FF', '#008000',  '#006666', '#000000', '#888888', '#00C78C', '#0000FF', "#FCD116", '#0000FF', '#000000', "FF6699") 
    #  red; turquois; green; magenta; blue; violet; green;  bluegreen; black; gray; turquoiseblue 
    edgeTypes <- c("pp", "controls-phosphorylation-of", 'homology', 'knowledge', 'experimental', 'combined_score',  "Physical interactions","Pathway", "Predicted", "Genetic interactions", "merged" , "Shared protein domains", "negative correlation", "positive correlation", "intersect", "peptide", "controls-state-change-of")             
    myarrows <- c ('Arrow', 'Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', "Arrow")
	setEdgeTargetArrowRule(windowname, 'edgeType', edgeTypes, myarrows, default='No Arrow')  
    setEdgeTargetArrowColorRule(windowname, "edgeType",  edgeTypes, edgecolors, default.color='#FF0000')      
    setEdgeSourceArrowColorRule(windowname, "edgeType",  edgeTypes, myarrows, default.color='#FF0000')  
 	setEdgeColorRule(windowname, 'edgeType', edgeTypes, edgecolors, mode='lookup', default.color='#33CC33')    # formerly #888888
	}
	#
edgeDprops <- function(windowname)  {
	setDefaultEdgeLineWidth (windowname, 3)
    setDefaultEdgeColor (windowname, "#FFFFFF")  # white
    setDefaultEdgeSelectionColor(windowname, "#FF69B4")  # hotpink
	edgecolors <- col2hex(c("red", "red", "magenta", "violet", "purple",  "green", "green2", "green3",  "aquamarine2", "cyan", "turquoise2", "cyan2", "lightseagreen", "gold",  "blue", "yellow", "slategrey", "darkslategrey", "grey", "black", "orange", "orange2"))
	edgecolorsplus <- col2hex(c("deeppink", "red", "red", "magenta", "violet", "purple",  "green", "green2", "green3",  "aquamarine2", "cyan", "turquoise2", "cyan2", "lightseagreen", "gold",  "blue", "yellow", "slategrey", "darkslategrey", "grey", "black", "orange", "orange2", "orangered2"))
    #  red; turquois; green; magenta; blue; violet; green;  bluegreen; black; gray; turquoiseblue; orange 
    edgeTypes <- c("pp", "controls-phosphorylation-of", "controls-expression-of", "controls-transport-of",  "controls-state-change-of", "Physical interactions", "BioPlex", "in-complex-with",  'experiments',  'database',   "Pathway", "Predicted", "Genetic interactions", "correlation", "negative correlation", "positive correlation",  'combined_score', "merged" , "intersect", "peptide", 'homology', "Shared protein domains") 
    # 22 edgeTypes            
    myarrows <- c ('Arrow', 'Arrow', 'Arrow', 'Arrow', "Arrow", 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow', 'No Arrow')
	setEdgeTargetArrowRule(windowname, 'edgeType', edgeTypes, myarrows, default='No Arrow')  
    setEdgeTargetArrowColorRule(windowname, "edgeType",  edgeTypes, edgecolors, mode="lookup", default.color="#E6E6FA")      
    setEdgeSourceArrowColorRule(windowname, "edgeType",  edgeTypes, edgecolors, mode="lookup", default.color="#E6E6FA")   
 	setEdgeColorRule(windowname, 'edgeType', edgeTypes, edgecolors, mode='lookup', default.color="#FFFFFF")
 	directed <- c("pp", "controls-phosphorylation-of", "controls-expression-of", "controls-transport-of",  "controls-state-change-of")
 	all.window.edges <- getAllEdges(windowname)
 	directed.edges <- c(all.window.edges[grep("pp", all.window.edges)] , all.window.edges[grep("controls", all.window.edges)])
 	phos.edges <- c(directed.edges[grep("pp", directed.edges)] , directed.edges[grep("phos", directed.edges)])
 	setEdgeColorDirect(windowname, phos.edges, "#FF0000")
 	setEdgeTargetArrowShapeDirect(windowname, directed.edges, "ARROW")
}
# As of May, 2016, setEdgeLineWidthRule() permanently breaks all control over the edge vizprops.
#	This is a workaround that accomplish the same task. The factor can be set depending on the value of edge weights; a weight of 0.2 gives a width of 1. Note: setEdgeLineWidthRule() doesn't break but doesn't work July 2016.
setEdgeWidths <- function (windowobj, factor=5)	{
	alledges <- getAllEdges(windowobj)
	edgeweights <- sapply(alledges, function(x) getEdgeAttribute(windowobj, x, "Weight"))
	line.widths <- factor*abs(as.numeric(edgeweights))
	setEdgeLineWidthDirect(windowobj, alledges, line.widths)	
}	
setEdgeWidths.R <- function (windowobj, edgefile, factor=4.5)	{
	line.widths <- factor*abs(as.numeric(edgefile$Weight))
	setEdgeLineWidthRule(windowobj, edge.attribute.name="Weight", attribute.values=edgefile$Weight, line.widths=line.widths)
	redraw(windowobj)
}	
# log version
setEdgeWidths.log <- function (windowobj, factor=1)	{
	alledges <- getAllEdges(windowobj)
	edgeweights <- as.numeric(sapply(alledges, function(x) getEdgeAttribute(windowobj, x, "Weight")))
	line.widths <- log(abs(edgeweights)) + factor - min(log(abs(edgeweights)))
	setEdgeLineWidthDirect(windowobj, alledges, line.widths)	
}
# Function to set edge color and width for figures
# Uses edges named in R edgefile, then converts them to cy edges
setEdgeAppearance <- function(windowobj, edgefile.edges, edgecolor="#CCCCCC", edgewidth=1.4) {
  # default color is "grey80"
  cyedges <- getCyEdgeNames(edgefile.edges)
  if (!grepl("#", edgecolor)) {edgecolor <- col2hex(edgecolor)}
  setEdgeColorDirect(windowobj, cyedges, edgecolor)
  setEdgeLineWidthDirect(windowobj, cyedges, edgewidth)
}
#	
intensityprops <- function (windowname, cytoscape.file) {
		setVisualStyle (windowname, "default")
	print (getNodeAttributeNames (windowname))
	cat("\n","\n","\t", "Which attribute will set node size and color?")
	plotcol <- as.character(readLines(con = stdin(), n = 1))
	node.sizes     = c (135, 130, 108, 75, 35, 75, 108, 130, 135)
	Intensity.Values <- cytoscape.file[, plotcol]  # set to intensity or normalized intensity
		maxint <- max(Intensity.Values, na.rm=TRUE) 
		minint <- min(Intensity.Values, na.rm=TRUE)
		icolors <- c('0099FF', '#007FFF','#00BFFF', '#00CCFF', '00FFFF', '#FFFFFF', '#FFFF7E', '#FFFF00', 'FFE600', 'FFD700', 'FFCC00')
		displayGraph (windowname)
		#  Some rules to set the color and size depending on the values of intensity
		if (maxint>abs(minint)) {
			setNodeColorRule (windowname, names(cytoscape.file[plotcol]), c (-(maxint+1), -(maxint/5), -(maxint/10), -(maxint*0.045), 0.0, (maxint*0.045), (maxint/10), (maxint/5), (maxint+1)), icolors, mode='interpolate') 
			icontrol.points = c (-(maxint+1), -(maxint*0.3), -(maxint/10), 0.0, (maxint/10), (maxint*0.3), (maxint+1))
			}
		if (maxint<abs(minint)) {
			setNodeColorRule (windowname, names(cytoscape.file[plotcol]), c ((minint-1), (minint/5), (minint/10), (minint*0.045), 0.0, -(minint*0.045), -(minint/10), -(minint/5), -(minint-1)), icolors, mode='interpolate') 
			icontrol.points = c ((minint-1), (minint*0.3), (minint/10), 0.0, -(minint/10), -(minint*0.3), abs(minint-1))
			}
		setNodeSizeRule (windowname, names(cytoscape.file[plotcol]), icontrol.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (windowname,  "#CC00FF") 
		new.style.name = paste(plotcol, noquote(" intensity"),  sep="", collapse=NULL) 
		copyVisualStyle(windowname, 'default', new.style.name)
		setVisualStyle(windowname, new.style.name)
		redraw (windowname)
		return(windowname)
 }  
#  This one is not interactive and doesn't set style
intensityprops.2 <- function (windowname, cytoscape.file, plotcol="Total.Phosphorylation") {
		setVisualStyle (windowname, "default")
	print (getNodeAttributeNames (windowname))
	node.sizes     = c (135, 130, 108, 75, 35, 75, 108, 130, 135)
	Intensity.Values <- cytoscape.file[, plotcol]  # set to intensity or normalized intensity
		maxint <- max(Intensity.Values, na.rm=TRUE) 
		minint <- min(Intensity.Values, na.rm=TRUE)
		icolors <- c('0099FF', '#007FFF','#00BFFF', '#00CCFF', '00FFFF', '#FFFFFF', '#FFFF7E', '#FFFF00', 'FFE600', 'FFD700', 'FFCC00')
		displayGraph (windowname)
		#  Some rules to set the color and size depending on the values of intensity
		if (maxint>abs(minint)) {
			setNodeColorRule (windowname, names(cytoscape.file[plotcol]), c (-(maxint+1), -(maxint/5), -(maxint/10), -(maxint*0.045), 0.0, (maxint*0.045), (maxint/10), (maxint/5), (maxint+1)), icolors, mode='interpolate') 
			icontrol.points = c (-(maxint+1), -(maxint*0.3), -(maxint/10), 0.0, (maxint/10), (maxint*0.3), (maxint+1))
			}
		if (maxint<abs(minint)) {
			setNodeColorRule (windowname, names(cytoscape.file[plotcol]), c ((minint-1), (minint/5), (minint/10), (minint*0.045), 0.0, -(minint*0.045), -(minint/10), -(minint/5), -(minint-1)), icolors, mode='interpolate') 
			icontrol.points = c ((minint-1), (minint*0.3), (minint/10), 0.0, -(minint/10), -(minint*0.3), abs(minint-1))
			}
		setNodeSizeRule (windowname, names(cytoscape.file[plotcol]), icontrol.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (windowname,  "#CC00FF") 
		redraw (windowname)
		return(windowname)
 }
#
 #
 Z.intensityprops <- function (windowname, cytoscape.file) {
		setVisualStyle (windowname, "default")
	print (getNodeAttributeNames (windowname))
	cat("\n","\n","\t", "Which attribute will set node size and color?")
	plotcol <- as.character(readLines(con = stdin(), n = 1))
	node.sizes     = c(310, abs(seq.int(from=-300, to=300, length.out=11)), 310)
	node.sizes[which(node.sizes==0)] <- 20
	control.points <- seq.int(from=-3, to=3, length.out=11)
	Intensity.Values <- cytoscape.file[, plotcol]  # set to intensity or normalized intensity
	icolors <- c('0099FF', '#007FFF','#00BFFF', '#00CCFF', '00FFFF', '#FFFFFF', '#FFFF7E', '#FFFF00', 'FFE600', 'FFD700', 'FFCC00')
	setNodeColorRule (windowname, names(cytoscape.file[plotcol]), control.points, icolors, mode='interpolate') 
	setNodeSizeRule (windowname, names(cytoscape.file[plotcol]), control.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (windowname,  "#CC00FF") 
		new.style.name = paste(names(cytoscape.file[plotcol]), noquote(" intensity"),  sep="", collapse=NULL) 
		copyVisualStyle(windowname, 'default', new.style.name)
		setVisualStyle(windowname, new.style.name)
		displayGraph (windowname)
		return(windowname)
 }
#
Z.intensityprops.linear <- function (windowname, cytoscape.file) {
		setVisualStyle (windowname, "default")
	print (getNodeAttributeNames (windowname))
	cat("\n","\n","\t", "Which attribute will set node size?")
	plotsize <- as.character(readLines(con = stdin(), n = 1))
	node.sizes     = abs(seq.int(from=9, to=141, length.out=13))
	node.sizes[which(node.sizes==0)] <- 8
	control.points <- seq.int(from=-3, to=3, length.out=11)
	Intensity.Values <- cytoscape.file[, plotsize]  # set to intensity or normalized intensity
	setNodeSizeRule (windowname, names(cytoscape.file[plotsize]), control.points, node.sizes, mode='interpolate')
	cat("\n","\n","\t", "Which attribute will set node color?")
	plotcol <- as.character(readLines(con = stdin(), n = 1))
	icolors <- c('0099FF', '#007FFF','#00BFFF', '#00CCFF', '00FFFF', '#FFFFFF', '#FFFF7E', '#FFFF00', 'FFE600', 'FFD700', 'FFCC00')
	setNodeColorRule (windowname, names(cytoscape.file[plotcol]), control.points, icolors, mode='interpolate') 
		setDefaultNodeSelectionColor (windowname,  "#CC00FF") 
		new.style.name = paste(names(cytoscape.file[plotsize]), names(cytoscape.file[plotcol]),  sep=".", collapse=NULL) 
		if (!(add.style.name %in% getVisualStyleNames(cy)))	{copyVisualStyle(windowname, 'default', new.style.name)}
		setVisualStyle(windowname, new.style.name)
		displayGraph (windowname)
		return(windowname)
 }

#
ratioprops <- function (windowname, cytoscape.file, plotcol="Total") {
	if(!(plotcol %in% getNodeAttributeNames(windowname))){
	print (getNodeAttributeNames (windowname))
	cat("\n","\n","\t", "Which attribute will set node size and color?")
	plotcol <- as.character(readLines(con = stdin(), n = 1))}
	setVisualStyle (windowname, "default")
	limits <- range(cytoscape.file[, plotcol])
	#
	node.sizes     = c (135, 130, 108, 75, 35, 75, 108, 130, 135)
    		#	RATIO is plotted
			#	Blue is negative: Yellow positive, Green in middle
			#		
		size.control.points = c (-100.0, -15.0, -5.0, 0.0, 5.0, 15.0, 100.0)
		color.control.points = c (-100.0, -10.0, -5.0, -2.25, 0.0, 2.25, 5.0, 10.0, 100.0)
		if(limits[1] < min(size.control.points)) {
			size.control.points = c (limits[1], -15.0, -5.0, 0.0, 5.0, 15.0, 100.0)
			color.control.points = c (limits[1]-1, -10.0, -5.0, -2.25, 0.0, 2.25, 5.0, 10.0, 100.0)
		}
		if(limits[2] > max(size.control.points)) {
			size.control.points = c (limits[1], -15.0, -5.0, 0.0, 5.0, 15.0, limits[2])
			color.control.points = c (limits[1]-1, -10.0, -5.0, -2.25, 0.0, 2.25, 5.0, 10.0, limits[2]+1)
		}
		ratio.colors = c ('#0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
		# displayGraph (windowname)
		setNodeColorRule (windowname, names(cytoscape.file[plotcol]), color.control.points, ratio.colors, mode='interpolate')
		setNodeSizeRule (windowname, names(cytoscape.file[plotcol]), size.control.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (windowname,  "#CC00FF") 
		new.style.name = paste(plotcol, noquote("_ratio"),  sep="", collapse=NULL) 
		# if (!(new.style.name %in% getVisualStyleNames(cy)))	{copyVisualStyle(windowname, 'default', new.style.name)} # bsroken
		# setVisualStyle(windowname, new.style.name)
		# return(windowname)
    }


 #
 single_edgeprops <- function (windowname, edge.file)  {
	maxWeight <- max(edge.file[, "Weight"], na.rm=T)
	minWeight <- min(edge.file[, "Weight"], na.rm=T)
	edge.attribute.values <- edge.file[, "Weight"]
	weight.scale=scale(edge.attribute.values, scale=T, center=T)
		minScale <- min(weight.scale, na.rm=T)
		maxScale <- max(weight.scale, na.rm=T)
	scalefactor = maxScale + abs(minScale)
	# some tweaking may be required here
	line.widths <- (maxScale + scalefactor + 2)*scale(weight.scale + (abs(minScale) + 0.25), scale=(maxScale + scalefactor), center=FALSE)
	line.widths <- as.vector(line.widths)
	setVisualStyle(windowname, "default") # don't want to do this, would rather do the following, but only default style is set
			# cat("Cytoscape Visual Styles", "\n", "_________________", "\n")		
		 	# print(getVisualStyleNames(windowname))		
			# cat("\n","\t", "Which Visual Style to use? ","\n","\t") 
			# cystylename <- as.character(readLines(con = stdin(), n = 1))
			# EM.style.name  <- paste(cystylename, noquote('Edge Merge'), sep=" ", collapse=NULL)
			# copyVisualStyle (mgcy, cystylename, EM.style.name)
			# setVisualStyle (mgcy, EM.style.name)
    setEdgeLineWidthRule (windowname, "Weight", as.character(edge.attribute.values), as.character(line.widths))
 	displayGraph (windowname) 
 	showGraphicsDetails (windowname, TRUE)
 	setLayoutProperties (windowname, "kamada-kawai-noweight", list (edge_attribute='Weight', weight_type=3, min_weight=minWeight, max_weight=maxWeight, layout_passes=50, iterations_pernode=50, distance_strength=8008, rest_length=60, disconnected_strength=0.01, disconnected_rest_length=500, anticollisionStrength=10)) 
 	layoutNetwork (windowname, "kamada-kawai-noweight")
 	fitContent(windowname) 
 	 	#viz.styles <- getVisualStyleNames(windowname)
	    #EM.style.name <- paste(viz.styles[length(viz.styles)], noquote('Edge Merge'), sep=" ", collapse=NULL)
			# this should be the most recently created visual style; best if new style created after all viz props set in "default"		
				#copyVisualStyle (windowname, 'default', EM.style.name)
			    #setVisualStyle (windowname, EM.style.name)
		}	
#
se_edgeprops_nolayoutNetwork <- function (windowname, edge.file)  {
	maxWeight <- max(edge.file[, "Weight"], na.rm=T)
	minWeight <- min(edge.file[, "Weight"], na.rm=T)
	edge.attribute.values <- edge.file[, "Weight"]
	weight.scale=scale(edge.attribute.values, scale=T, center=T)
		minScale <- min(weight.scale, na.rm=T)
		maxScale <- max(weight.scale, na.rm=T)
	scalefactor = maxScale + abs(minScale)
	# some tweaking may be required here
	line.widths <- (maxScale + scalefactor + 2)*scale(weight.scale + (abs(minScale) + 0.25), scale=(maxScale + scalefactor), center=FALSE)
	line.widths <- as.vector(line.widths)
	setVisualStyle(windowname, "default") # don't want to do this, would rather do the following, but only default style is set
			# cat("Cytoscape Visual Styles", "\n", "_________________", "\n")		
		 	# print(getVisualStyleNames(windowname))		
			# cat("\n","\t", "Which Visual Style to use? ","\n","\t") 
			# cystylename <- as.character(readLines(con = stdin(), n = 1))
			# EM.style.name  <- paste(cystylename, noquote('Edge Merge'), sep=" ", collapse=NULL)
			# copyVisualStyle (mgcy, cystylename, EM.style.name)
			# setVisualStyle (mgcy, EM.style.name)
    setEdgeLineWidthRule (windowname, "Weight", as.character(edge.attribute.values), as.character(line.widths))
 	displayGraph (windowname) 
 	showGraphicsDetails (windowname, TRUE)
 	# setLayoutProperties (windowname, "kamada-kawai-noweight", list (edge_attribute='Weight', weight_type=3, min_weight=minWeight, max_weight=maxWeight, layout_passes=50, iterations_pernode=50, distance_strength=8008, rest_length=60, disconnected_strength=0.01, disconnected_rest_length=500, anticollisionStrength=10)) 
 	# layoutNetwork (windowname, "kamada-kawai-noweight")
 	# fitContent(windowname) 
 	# 	viz.styles <- getVisualStyleNames(windowname)
	#    EM.style.name <- paste(viz.styles[length(viz.styles)], noquote('Edge Merge'), sep=" ", collapse=NULL)
			# this should be the most recently created visual style; best if new style created after all viz props set in "default"		
	#			copyVisualStyle (windowname, 'default', EM.style.name)
	#		    setVisualStyle (windowname, EM.style.name)
		}
#
enrichmentprops <- function (windowname, cytoscape.file, plotcol="End.Fraction") {
	print (getNodeAttributeNames (windowname))
	cat("\n","\n", plotcol)
	cat("\t", "attribute will set node size and color!", "\n","\n")
	#plotcol <- as.character(readLines(con = stdin(), n = 1))
	setVisualStyle (windowname, "default")
	node.sizes     = c (10, 15, 20, 25, 30, 35, 63, 80, 130, 135, 140)
	setDefaultNodeShape(windowname, "ellipse")
	setDefaultNodeColor (windowname, '#C9C9C9') # gray 79
	setDefaultNodeSize  (windowname, 30) # for grey non-data nodes
	setDefaultNodeFontSize(windowname, 18)
	setDefaultNodeBorderWidth (windowname, 1.8)
	setDefaultNodeBorderColor (windowname, '#888888')  # gray 
	setDefaultEdgeLineWidth (windowname, 3)
    setDefaultEdgeColor (windowname, '#FFFFFF')  # white
    #
    		#	Enrichment RATIO is plotted
			#	Blue is negative (enriched in control): Yellow positive (enriched in test), Green in middle
			#   Note - can plot ratio or ratio.N (normalized)
			#
		size.control.points = c (-100, -50.0, -15.0, -2.25, -1.8, 0.0, 1.8, 2.25, 15.0, 50.0, 100)
		color.control.points = c (-50.0, -10.0, -5.0, -2.25, -1.8, 0.0, 1.8, 2.25, 5.0, 10.0, 50.0)
		ratio.colors = c ('#0099FF', '#007FFF','#00BFFF', '#00CCFF', '#00FFFF', '#00EE00', '#FFFF7E', '#FFFF00', '#FFE600', '#FFD700', '#FFCC00')
		displayGraph (windowname)
		setNodeColorRule (windowname, names(cytoscape.file[plotcol]), color.control.points, ratio.colors, mode='interpolate')
		setNodeSizeRule (windowname, names(cytoscape.file[plotcol]), size.control.points, node.sizes, mode='interpolate')
		setDefaultNodeSelectionColor (windowname,  "#CC00FF") 
		new.style.name = paste(plotcol, noquote(" ratio"),  sep="", collapse=NULL) 
		copyVisualStyle(windowname, 'default', new.style.name)
		setVisualStyle(windowname, new.style.name)
		return(windowname)
    }
#
PSP.child <- function(nodelist) {
	 # 	Get kinase substrate dataset into Cytoscape
	 # returns data frame; use 'newdf <- PSP.child(nodelistname)
	 ksnodes <- unique(c(levels(kinsub$kinase), levels(kinsub$substrate)))
	 ksgraph <- new("graphNEL", nodes=ksnodes, edgemode='directed')
	 ksgraph <- initNodeAttribute (graph=ksgraph,  attribute.name='Gene.Name', attribute.type='char', default.value='undefined')
	 nodeData (ksgraph, n=ksnodes, attr="Gene.Name") <- ksnodes
	 ksgraph <- initEdgeAttribute (graph=ksgraph, attribute.name='edgeType', attribute.type='char', default.value ='pp')
	 ksgraph <- initEdgeAttribute (graph=ksgraph, attribute.name='Weight', attribute.type='numeric', default.value = 0)
	 ksgraph <- addEdge (as.vector(kinsub$kinase, mode="character"), as.vector(kinsub$substrate, mode="character"), ksgraph)
	 edgeData (ksgraph, as.vector(kinsub$kinase, mode="character"), as.vector(kinsub$substrate, mode="character"), 'edgeType') <- 'pp' 
	 edgeData (ksgraph, as.vector(kinsub$kinase, mode="character"), as.vector(kinsub$substrate, mode="character"), 'Weight') <- 0.25 
	 cywind <- CytoscapeWindow ('PSP kinase-substrate', graph=ksgraph)
	 displayGraph (cywind) 
	 layoutNetwork (cywind, "grid") 
	 nodenames <- unique(levels(as.factor(nodelist)))
	 nodestoplot <- intersect(nodenames, ksnodes)
 	 clearSelection (cywind) # clear selection first
     selectNodes (cywind, nodestoplot) 
     # create an index so that this command can be run multiple times, creating a sub network graph with a new name each time
		j = 1
		cywinds <- getWindowList(cy)
		childname <- 'PSP kinase-substrate - Child..1'		
		if (childname %in% cywinds) { # childname = cywinds[cywinds %in% childname]
			j <- j + 1
			unlist(strsplit(childname, "..", fixed = TRUE)) -> x		
			childname <- paste(x[1], noquote("_PSP Child.."), j, sep="", collapse=NULL)		
			}
	   pspW <- createWindowFromSelection (cywind, childname, TRUE)
	  layoutNetwork (pspW, "kamada-kawai") 
	  redraw (pspW)
	  deleteWindow(cy, window.title="PSP kinase-substrate")  # if done with this now  
	###########################  keep this as one function for now
	#  get.PSP.child < function (pspsub) {
	# Get PSP Child network
	cat("Cytoscape Windows", "\n")	 
	getWindowList(cy)
		 pspsub <- existing.CytoscapeWindow(childname , copy=TRUE)  # 18 nodes 22 edges
		 pspG <- pspsub@graph
		 	slotNames(pspG)
		 	eda.names(pspG)
			psp.edges <- eda(pspG, 'interaction')
		psp.df <-data.frame(cbind(names(psp.edges), as.vector(psp.edges)))
		names(psp.df)[1:2] <- c("Edge", "edgeType")
		setDefaultBackgroundColor (cy, 'FFFFFF')
	 # Now need to split the edge GENE1 | GENE2 format into two columns like GM and String
		# "|" is a special character that causes problems
		# 		
		p0 <-sapply(psp.df$Edge, stripbar)
		p1 <-sapply(p0, get.gene.name)
		p2 <-sapply(p0, get.gene.name.2)
		psp.df$Gene.1 <-as.vector(p1)
		psp.df$Gene.2 <-as.vector(p2)
		# give pp a quanitative value
		psp.df$Weight <- 0.25  ####	 an arbitrary value
		psp.df <- psp.df[,c(3,4,5,2)]
		return (psp.df)
	}	
#
sub.network.1 <- function (cy.window, selection) {
	cat("Cytoscape Windows", "\n")
	print (	getWindowList(cy))
	cat ("\n", "\t", "Which Cytoscape Window contains the selected nodes?", "\n", "\t")
	cy.window <- readLines(con = stdin(), n = 1)
	w2 <- existing.CytoscapeWindow (cy.window,  copy.graph.from.cytoscape.to.R=TRUE)
	selection <- getSelectedNodes(w2)
	coords <- getNodePosition(w2, selection)
	new.window.title <- paste(cy.window, noquote("Sub-Network"), sep="_", collapse=NULL)
	c2 <- createWindowFromSelection (w2, new.window.title, TRUE)
	g2 <- getGraph (c2)
		  x = as.integer (sapply (coords, function (node.loc) return (node.loc$x)))
		  y = as.integer (sapply (coords, function (node.loc) return (node.loc$y)))
  	setNodePosition (c2, selection, x, y)
  	fitContent (c2)
  	showGraphicsDetails (c2, TRUE)
  	}
#
sub.Network <- function(nodes) {
		cat("Cytoscape Windows", "\n", "_________________", "\n")	 
		cywinds <- getWindowList(cy)
		cat(cywinds, sep = "\n")
		cat("\n","\t", "Which Cytoscape window contains the selected nodes? ","\n","\t") 
		cywindname <- as.character(readLines(con = stdin(), n = 1))
			w2 <- existing.CytoscapeWindow (cywindname,  copy.graph.from.cytoscape.to.R=TRUE)
		unlist(strsplit(cywindname, "..", fixed = TRUE)) -> x
		# create an index so that this command can be run multiple times, creating a sub network graph with a new name each time
		j = 1
		childname <- paste(x[1], noquote("_subnetwork..1"), sep="", collapse=NULL) 
		while (childname %in% cywinds) {
			j <- j + 1
			x[2] <- j
			childname <- paste(x[1], noquote("_subnetwork.."), j, sep="", collapse=NULL)		
			next }
		#
		selection <- getSelectedNodes(w2)
		cat (selection, sep = "\n") 
	    cat("\n", "Copy and paste nodes above to retrieve networks from STRING and GeneMANIA as .txt files")
		cat("\n","\t", "Write a file of node names for website network retrieval?","\n", "(T or F)", "\t")
			writeresponse <- as.logical(readLines(con = stdin(), n = 1))
			if (writeresponse == TRUE) {
			 unlist(strsplit(childname, "_", fixed = TRUE)) -> z
			 namefile <- paste(z[1],  j, noquote("_subgenes.txt"), sep="", collapse=NULL)
			 cat("\n", "  Use this file:", "\n", "\t", namefile, "\n", "to retrieve networks from STRING and GeneMANIA as .txt files")
			 write.table(sort(selection), file= namefile, sep="\t", eol = "\n", quote = FALSE, row.names=FALSE, col.names=FALSE)
			 }
		seledges <- getSelectedEdges (w2)
		coords <- getNodePosition(w2, selection)
		subW <- createWindowFromSelection (w2, childname, return.graph=TRUE)
		g2 <- getGraph (subW)
		  x1 = as.integer (sapply (coords, function (node.loc) return (node.loc$x)))
		  y1 = as.integer (sapply (coords, function (node.loc) return (node.loc$y)))
		#  set visual style to the last one created
		# NOTE:  request getVisualStyle = the active style in that window
		#viz.styles <- getVisualStyleNames(w2)
		#setVisualStyle (subW, viz.styles[length(viz.styles)])	
		setNodePosition (subW, selection, x1, y1)
	  	fitContent (subW)
  		showGraphicsDetails (subW, TRUE)
#
		 subW2 <- existing.CytoscapeWindow (childname,  copy.graph.from.cytoscape.to.R=TRUE)
		 return (subW2)
		}

subclone.net <- function(nodes, CytoscapeWindowTitle) {
	CytoscapeWindow <- existing.CytoscapeWindow (CytoscapeWindowTitle,  copy.graph.from.cytoscape.to.R=F)
		cywinds <- getWindowList(cy)
		unlist(strsplit(CytoscapeWindowTitle, "..", fixed = TRUE)) -> x
		# create an index so that this command can be run multiple times, creating a sub network graph with a new name each time
		j = 1
		childname <- paste(x[1], noquote("_subnetwork..1"), sep="", collapse=NULL) 
		while (childname %in% cywinds) {
			j <- j + 1
			x[2] <- j
			childname <- paste(x[1], noquote("_subnetwork.."), j, sep="", collapse=NULL)
					next }
		selection <- as.character(nodes)
		cat (selection, sep = "\n") 
	    cat("\n", "in")
		cat("\n","\t", CytoscapeWindowTitle, "\n")
		selectNodes(CytoscapeWindow, selection, preserve.current.selection=FALSE)
		coords <- getNodePosition(CytoscapeWindow, selection)
		subW <- createWindowFromSelection (CytoscapeWindow, childname, return.graph=TRUE)
		g2 <- getGraph (subW)
		  x1 = as.integer (sapply (coords, function (node.loc) return (node.loc$x)))
		  y1 = as.integer (sapply (coords, function (node.loc) return (node.loc$y)))
	
		setNodePosition (subW, selection, x1, y1)
	  	fitContent (subW)
  		showGraphicsDetails (subW, TRUE)
		 subW2 <- existing.CytoscapeWindow (childname,  copy.graph.from.cytoscape.to.R=TRUE)
		 return (subW2)
		}
#
#
hi.corr.edges <- function(cf.cor) {   # requires cf.cor; use corredges <- hi.corr.edges()
	 cordfall = NULL
	 for (i in 1:ncol(cf.cor)) {
		x <- which(abs(cf.cor[,i]) >= 0.9)
		if (length(x) >= 1)  {
			Gene.2 <- colnames(cf.cor)[i]
			correlation <- cf.cor[x,i]
			cordf <- data.frame(correlation)
			cordf$Gene.1 <- names(correlation)
			cordf$Gene.2 <- Gene.2
			if (i == 1) {cordfall <- cordf}
			 cordfall <- rbind(cordfall, cordf) }
		} 
	forward <- cordfall
	reverse <- cordfall[,c(1, 3, 2)]
	names(reverse) <- names(forward)
	dupgene <- which(forward$Gene.1 == forward$Gene.2)
	forward <- forward[-dupgene,]
	dupgener <- which(reverse$Gene.1 == reverse$Gene.2)
	reverse <- reverse[-dupgener,]
	corredges <- intersect(forward,reverse)
	return(corredges)
	}	
#
	add.corr.edges <- function (cy.window) {	# requires corredges		
			cat("Cytoscape Windows", "\n")
		print (	getWindowList(cy))
		cat ("\n", "\t", "Which Cytoscape Window contains the selected nodes?", "\n", "\t")
		cy.window <- readLines(con = stdin(), n = 1)
		w2 <- existing.CytoscapeWindow (cy.window,  copy.graph.from.cytoscape.to.R=TRUE)
		g2 <- getGraph (w2)
		selection <- getAllNodes (w2)
		g2 <- initEdgeAttribute (graph=g2, attribute.name='edgeType', attribute.type='char', default.value='correlation')		
		g2 <- initEdgeAttribute(g2, attribute.name = "correlation", attribute.type = 'numeric', default.value = 0.0)
			#must select only the nodes in the graph
		# selcor <- corredges[intersect(corredges$Gene.1, selection) & intersect(corredges$Gene.2, selection)]	
		# selcor <- corredges[(corredges$Gene.1 %in% selection) & (corredges$Gene.2 %in% selection)]  # none
		a1 = corredges$Gene.1 %in% selection
		a2 = corredges$Gene.2 %in% selection
		corredges[a1&a2,] -> edds  # 
		g2 <- addEdge (as.vector(edds$Gene.1, mode="character"), as.vector(edds$Gene.2, mode="character"), g2) 
		g2 <- initEdgeAttribute (graph=g2, attribute.name='edgeType', attribute.type='char', default.value='correlation')
		g2 <- initEdgeAttribute(g2, attribute.name = "correlation", attribute.type = 'numeric', default.value = 0.0)
		edgeData (g2, as.vector(edds$Gene.1, mode="character"), as.vector(edds$Gene.2, mode="character"), attr='correlation') <- edds$correlation
		# setGraph (c2, g2) didn't do anything
		addGraphToGraph (w2, g2)
}	
#
get.String.edges <- function (stnet)	{
  # use stn <- get.String.edges(stringfile)
	#  String file input and cleanup  
	#		- We want the same node IDs
		cat("Cytoscape Windows", "\n", "_________________", "\n")	 
		cywinds <- getWindowList(cy)
		cat(cywinds, sep = "\n")
		cat("\n","\t", "Which Cytoscape window contains nodes to plot String edges? ","\n","\t") 
			cywindname <- as.character(readLines(con = stdin(), n = 1))
			cw2 <- existing.CytoscapeWindow (cywindname,  copy.graph.from.cytoscape.to.R=TRUE)
			winnodes <- getAllNodes(cw2)
		cat("\n","\n","\t", "STRING file name?","\n","\t")
		stringfilename <- readLines(con = stdin(), n = 1)  # 
		stnet <- read.table(stringfilename, header=TRUE, sep = "\t", comment.char = "", na.strings='', stringsAsFactors=FALSE, fill=TRUE)
			names(stnet)[1]="node1"  # to edit out the 'X.' or "#" 
			# fix up the gene names 
			stnet$node1 <- sapply(stnet$node1, get.gene) 
			stnet$node2 <- sapply(stnet$node2, get.gene)  
			# must make these factors to use levels()
			# stnet$node1=as.factor(stnet$node1)
			# stnet$node2=as.factor(stnet$node2)	 
			#stnodes <- unique(c(levels(stnet[1,1]), levels(stnet[1,2])))
			# try this without factors
			stnodes <- unique(c(as.character(stnet$node1), as.character(stnet$node2)))
			flub <- setdiff(stnodes, winnodes) # setdiff( winnodes, stnodes) is the non-string node set
			# if there is a String flub
			if (length(flub) >= 1) { 
				cat("\n","\t", "The following String names do not match ", cywindname, " nodes:","\n","\t", flub)
				#  Just get rid of the offending nodes
				if (any(stnet$node1 %in% flub)) stnet <- stnet[-which(stnet$node1 %in% flub), ]
				if (any(stnet$node2 %in% flub)) stnet <- stnet[-which(stnet$node2 %in% flub), ]
								}
		# 	Rearrange data file		
			stn <- stnet[,c(1:2)]
			names(stn) <- c('Gene.1', 'Gene.2')
			stne <- stn
			stne$Weight <- stnet$experimental
			stne$edgeType <- "experimental"
			stnk <- stn
			stnk$Weight <- stnet$knowledge
			stnk$edgeType <- "knowledge"
			stnh <- stn
			stnh$Weight <- stnet$homology
			stnh$edgeType <- "homology"
			stncs <- stn
			stncs$Weight <- stnet$combined_score
			stncs$edgeType <- "combined_score"
			stn <- rbind (stne, stnk, stnh, stncs)
			nones <- which (stn$Weight==0)
			stn <- stn[-nones, ]		
			return (stn)	
			}
	#
String.edge.width.props <- function (Cy_Wind, Network.File)  {
 			minWeight <- min(Network.File$Weight)
			maxWeight <- max(Network.File$Weight)
			edge.attribute.values <- as.vector(Network.File$Weight)
			weight.scale=scale(edge.attribute.values, scale=T, center=T)
			weight.scale=as.vector(weight.scale)
				minScale <- min(weight.scale, na.rm=T)
				maxScale <- max(weight.scale, na.rm=T)
			scalefactor = maxScale + abs(minScale)
			line.widths <- scalefactor*scale(weight.scale + (abs(minScale) + 0.25), scale=(maxScale + scalefactor), center=FALSE)
			line.widths <- as.vector(line.widths) 
			setVisualStyle(Cy_Wind, "default") # don't want to do this but see below
 			setEdgeLineWidthRule (Cy_Wind, 'Weight', as.character(edge.attribute.values), as.character(line.widths))
 			# this creates a problem: node size and color are lost; style is set back to 'default'
 			viz.styles <- getVisualStyleNames(Cy_Wind)
			string.style.name <- paste(viz.styles[length(viz.styles)], noquote(" String"), sep="", collapse=NULL)
			# this should be the most recently created visual style			
 	 			displayGraph (Cy_Wind)
				fitContent (Cy_Wind)
				showGraphicsDetails(Cy_Wind, TRUE)
				copyVisualStyle (Cy_Wind, 'default', string.style.name)
			    setVisualStyle (Cy_Wind, string.style.name)
			}
#	
get.GM.edges <- function (gmnet) {  # text file from http://www.genemania.org
		cat("Cytoscape Windows", "\n", "_________________", "\n")	 
		cywinds <- getWindowList(cy)
		cat(cywinds, sep = "\n")
		cat("\n","\t", "Which Cytoscape window contains nodes to plot GeneMANIA edges? ","\n","\t") 
			cywindname <- as.character(readLines(con = stdin(), n = 1))
			cw2 <- existing.CytoscapeWindow (cywindname,  copy.graph.from.cytoscape.to.R=TRUE)
			winnodes <- getAllNodes(cw2)
		cat("\t", "GeneMANIA file name?","\n","\t")
		gmfilename <- readLines(con = stdin(), n = 1)  # Test gmfilename <- "_tst_genemania_network.txt"
		gmnet <- read.table(gmfilename, header=TRUE, sep = "\t", comment.char = "#", na.strings='', stringsAsFactors=FALSE, fill=TRUE)
			names(gmnet)[1]="Gene.1"  # to make sure it's correct
			gmnet$Gene.1 <- sapply(gmnet$Gene.1, get.gene) 
			gmnet$Gene.2 <- sapply(gmnet$Gene.2, get.gene)  
			# gmnet$Gene.1=as.factor(gmnet$Gene.1)
			# gmnet$Gene.2=as.factor(gmnet$Gene.2)
			 a = as.character(gmnet$Gene.1)
			 b = as.character(gmnet$Gene.2)
			gmnodes <- unique(c(a,b))
			flub <- setdiff(gmnodes, winnodes) # setdiff( winnodes, stnodes) is the non-GM node set
			# if there is an ID flub
			if (length(flub) >= 1) { 
				cat("\n","\t", "The following GM names do not match ", cywindname, " nodes:","\n","\t", flub)
				#  Just get rid of the offending nodes
				if (any(gmnet$Gene.1 %in% flub)) gmnet <- gmnet[-which(gmnet$Gene.1 %in% flub), ]
				if (any(gmnet$Gene.2 %in% flub)) gmnet <- gmnet[-which(gmnet$Gene.2 %in% flub), ]
								}
	# 	Prune data file
			gmn <- gmnet[,c(1:4)]
			names(gmn)[4] <- "edgeType"
			return(gmn)
			}
#
get.GM.edgefile.rat <- function (gmfilename, species="rat") {  
	# text file from http://www.genemania.org
	# for cross-species analysis, human gene names uppercase returned
	# 		so convert to rat convention
		# nodenames <- toupper(as.character(nodenames))
		gmnet <- read.table(gmfilename, header=TRUE, skip=6, sep = "\t", comment.char = "#", na.strings='', fill=TRUE, stringsAsFactors=FALSE)
			names(gmnet)[c(1:4)]=c("Gene.1", "Gene.2", "Weight", "edgeType")  
			if(species=="human") {
				gmnet$Gene.1 <- sapply(gmnet$Gene.1, toratname)
				gmnet$Gene.2 <- sapply(gmnet$Gene.2, toratname)  }
	# 	Prune data file
			gmn <- gmnet[,c(1:4)]
			return(gmn)
			}

#
GM.edge.width.props <- function (Cy_Wind, Network.File)  {
 			minWeight <- min(Network.File$Weight)
			maxWeight <- max(Network.File$Weight)
			edge.attribute.values <- as.vector(Network.File$Weight)
			weight.scale=scale(edge.attribute.values, scale=T, center=T)
			weight.scale=as.vector(weight.scale)
				minScale <- min(weight.scale, na.rm=T)
				maxScale <- max(weight.scale, na.rm=T)
			scalefactor = maxScale + abs(minScale)
			line.widths <- 1.8*scalefactor*scale(weight.scale + (abs(minScale) + 0.25), scale=(maxScale + scalefactor), center=FALSE)
			line.widths <- as.vector(line.widths) 
			setVisualStyle(Cy_Wind, "default") # don't want to do this but see below
 			setEdgeLineWidthRule (Cy_Wind, 'Weight', as.character(edge.attribute.values), as.character(line.widths))
 			# this creates a problem: node size and color are lost; style is set back to 'default'
 			viz.styles <- getVisualStyleNames(Cy_Wind)
			GM.style.name <- paste(viz.styles[length(viz.styles)], noquote("_GM"), sep="", collapse=NULL)
			# this should be the most recently created visual style			
 	 			displayGraph (Cy_Wind)
				fitContent (Cy_Wind)
				showGraphicsDetails(Cy_Wind, TRUE)
				copyVisualStyle (Cy_Wind, 'default', GM.style.name)
			    setVisualStyle (Cy_Wind, GM.style.name)
			}			
#	
#
  EDGE.MERGE <- function(stn, gmn, PSP.df) {
	#
	#	We want to sum the evidence for interactions, but not duplicate reversed node edges
	# 	
		st <- stn[stn$edgeType=="combined_score",]
	 #  this is all we reallly need for the merge
		stnodes <- unique(c(as.character(st$Gene.1), as.character(st$Gene.2)))
		gm <- gmn
		gmnodes <- unique(c(as.character(gm$Gene.1), as.character(gm$Gene.2)))
		sgnodes <- unique (c(stnodes, gmnodes))
		psp <- PSP.df	 
		 a = as.character(psp$Gene.1)
		 b = as.character(psp$Gene.2)
		pnodes <- unique(c(a,b))
		comnodes <- sgnodes[sgnodes %in% pnodes]
		extras <- pnodes[!(pnodes %in% sgnodes)]	# =	 setdiff(pnodes, sgnodes) 
			if (length(extras) >= 1) { 
				#  Get rid of the extra nodes
				psp <- psp[psp$Gene.1 %in% sgnodes, ]
				psp <- psp[psp$Gene.2 %in% sgnodes, ] 
				#	extra1 <- which(psp$Gene.1  %in% extras) 
				#	extra2 <- which(psp$Gene.2   %in% extras) 
				#psp <- psp[-extra1,]  
				#psp <- psp[-extra2,]	
				}
		sgp.c <- rbind(stn, gmn, psp)  # can plot this, but it's busy
		# sgpnodes <- unique(c(levels(sgp.c[1,1]), levels(sgp.c[1,2])))
	#
	 #	String and GM may have edges in common that are reversed
	 #	Therefore, test string edges in reverse
	 #	This is easist to do in a single column
	#
 	st$nodes.combined <- noquote(paste(st$Gene.1, st$Gene.2))
	st$nodes.reversed <- noquote(paste(st$Gene.2, st$Gene.1))
	gm$nodes.combined <- noquote(paste(gm$Gene.1, gm$Gene.2))
	gm$nodes.reversed <- noquote(paste(gm$Gene.2, gm$Gene.1)) 
	psp$nodes.combined <- noquote(paste(psp$Gene.1, psp$Gene.2)) 
	#
	#  Which edges in stn are the reverse of those in gmn?  = stn[sg.reverse,]
	sg.reverse <- st$nodes.combined %in% gm$nodes.reversed
	#  Reverse these  
	# Note that the matrix conversion is reqired because of factor level collision
	st.fix <- noquote(as.matrix(st[sg.reverse, c(2,1)]))
	if (nrow(st.fix) >0) {
		names(st.fix) <- c("Gene.1", "Gene.2")
		stnt=noquote(as.matrix(st))
		stnt[sg.reverse, c(1:2)] <- st.fix
		st[,c(1:2)]<-stnt[,c(1:2)] }
	# Revise the test columns 
	 	st$nodes.combined <- noquote(paste(st$Gene.1, st$Gene.2))
		st$nodes.reversed <- noquote(paste(st$Gene.2, st$Gene.1))
	#
	#  So, the merge should be :
	sg <- rbind(st, gm)
	 #
	 # now, PSP edges
	 # kinases are directional, so we only want psp forward edges 	# 
	 # which psp edges match the reverse of sg?
	 psf <- sg$nodes.reversed %in% psp$nodes.combined   # a few do = sg[psf,]
	 # Now reverse these 
	 sg.pspr <- sg[psf, c(2,1)]
	 names(sg.pspr) <- c("Gene.1", "Gene.2")
	 sg[psf, c(1:2)] <- sg.pspr
		sg.ord <- order(as.character(sg$Gene.1), as.character(sg$Gene.2))
	  	sg <-sg[sg.ord,]
		# This sets all kinases as Gene.1 and substrates as Gene.2
	 # Now bind them all together
	 sgp <- rbind(sg[,c(1:4)], psp[,c(1:4)])
	 sgp.ord <- order(as.character(sgp$Gene.1), as.character(sgp$Gene.2))
  	 sgp <-sgp[sgp.ord,]
	  # Now merge edges
 	 #
	 sgp.merged <- ddply(sgp, .(Gene.1, Gene.2), numcolwise(sum), na.rm=TRUE)  #  just the gene names and numerical values
	# test	sgp.c.merged <- ddply(sgp.c, .(Gene.1, Gene.2), numcolwise(sum), na.rm=TRUE)
	# sgp.c.merged is bigger, which means that edges are alligned in sgp
	sgp.merged$edgeType <- 'merged'
	# Remove auto-phosphorylation loops
		auto <- which (as.character(sgp.merged$Gene.1) == as.character(sgp.merged$Gene.2))
		if (length(auto) > 0) {
			sgp.mt <- sgp.merged[-auto,] } else sgp.mt <- sgp.merged
	return (sgp.mt)	
	}
#

mergeEdges.dir <- function(edgefile) {
          # define directional and non-directional edges
          directed <- c("pp", "controls-phosphorylation-of", "controls-expression-of", "controls-transport-of",  "controls-state-change-of")
          undirected <- c("Physical interactions", "BioPlex", "in-complex-with",  'experiments',  'database',   "Pathway", "Predicted", "Genetic interactions", "correlation", "negative correlation", "positive correlation",  'combined_score', "merged" , "intersect", "peptide", 'homology', "Shared protein domains")
          # check for nodes in reversed orientation for undirected edges
          undir.edges <- edgefile[-(edgefile$edgeType %in% directed),]
          # NEW: simply sort/order the nodes
          # this works:
          # for(i in 1:dim(undir.edges)[1])	{
          #	undir.edges[i, 1:2] <- sort(as.vector(undir.edges[i,1:2]))
          #	}
          # Better: Working non-loop:
          undir.edges[, 1:2] <- t(apply(undir.edges[,1:2], 1, function (x) sort(x)))
          # merge undirected edges
          undir.merged <- ddply(undir.edges, .(Gene.1, Gene.2), numcolwise(sum), na.rm=TRUE)	
          undir.merged$edgeType <- dlply(undir.edges, .(Gene.1, Gene.2), function(x) paste0(x[, "edgeType"]))	
          undir.merged$edgeType <- sapply(undir.merged$edgeType, function(x) paste(c(x), collapse=", "))
          # undir.merged$Directed <- FALSE	
          # merge directed edges
          dir.edges <- edgefile[edgefile$edgeType %in% directed,]
          dir.merged <- ddply(dir.edges, .(Gene.1, Gene.2), numcolwise(sum), na.rm=TRUE)
          dir.merged$edgeType <- dlply(dir.edges, .(Gene.1, Gene.2), function(x) paste0(x[, "edgeType"]))	
          dir.merged$edgeType <- sapply(dir.merged$edgeType, function(x) paste(c(x), collapse=", "))
          # dir.merged$Directed <- TRUE
          edgefile.merged <- rbind(dir.merged, undir.merged)
          # Remove auto-phosphorylation loops
          edgefile.merged <- remove.autophos(edgefile.merged)
          # edgefile.merged <- edgefile.merged[, c(1:2,5,3,4,6)]	
          return(edgefile.merged)
     }
#
# This version merges directed and undirected edges:
mergeEdges.1 <- function(edgefile) {
	# NEW: simply sort/order the nodes
	undir.edges <- edgefile
	undir.edges[, 1:2] <- t(apply(undir.edges[,1:2], 1, function (x) sort(x)))
	# merge edges; make undirected
	undir.merged <- ddply(undir.edges, .(Gene.1, Gene.2), numcolwise(sum), na.rm=TRUE)	
	undir.merged$edgeType <- "merged"
	undir.merged$Directed <- FALSE	
	# Remove auto-phosphorylation loops
	edgefile.merged <- remove.autophos(undir.merged)
	# edgefile.merged <- edgefile.merged[, c(1:2,5,3,4,6)]	
	return(edgefile.merged)
	}
mergeEdges <- function(edgefile) {
     # First sort/order the nodes
     undir.edges <- edgefile
     undir.edges[, 1:2] <- t(apply(undir.edges[,1:2], 1, function (x) sort(x)))
     # merge edges; make undirected
     undir.merged <- ddply(undir.edges, .(Gene.1, Gene.2), numcolwise(sum), na.rm=TRUE)	
     # Rather than name them, "merged", concatenate names as they were
     undir.merged$edgeType <- dlply(undir.edges, .(Gene.1, Gene.2), function(x) paste0(x[, "edgeType"]))	
     undir.merged$edgeType <- sapply(undir.merged$edgeType, function(x) paste(c(x), collapse=", "))
     #undir.merged$edgeType <- "merged"
     undir.merged$Directed <- FALSE	
     # Remove auto-phosphorylation loops
     edgefile.merged <- remove.autophos(undir.merged)
     # edgefile.merged <- edgefile.merged[, c(1:2,5,3,4,6)]	
     return(edgefile.merged)
}
# Modified from a function by Paul Shannon:
selectNodesConnectedBySelectedEdges <- function (cw) {
 selectedEdges = getSelectedEdges (cw)
 if (length (selectedEdges) == 1 && is.na (selectedEdges))
   return ()
 tokens1 = unlist (strsplit (selectedEdges, ' \\('))
 tokens2 = unlist (strsplit (tokens1, '\\) '))
 node.names = intersect (tokens2, nodes (cw@graph))
 if (length (node.names) > 0)
   selectNodes (cw, node.names)
 }
 #
selectEdgesConnectedBySelectedNodes <- function (cw) {
 selectedNodes = getSelectedNodes (cw)
 if (length (selectedNodes) == 1 && is.na (selectedNodes))
   return ()
 graphEdges <- getAllEdges(cw)  
 selectedEdges <- unlist(mapply(function(x) return(graphEdges [grep(x, graphEdges)]), selectedNodes)) 
 if (length (selectedEdges) > 0)
    selectEdges(cw, selectedEdges)
 } 
 # the following is superceded in RCy3 by getAdjacentEdgeNames
 get.adjacent.edge.names = function (cw, geneIDs) {
	all.edge.names = cy2.edge.names (cw@graph)
	all.edge.names.cyStyle = as.character (all.edge.names)
	indices.of.edges.with.geneIDs = c ()
	for (geneID in geneIDs) {
	geneID.regex.nodeA = sprintf ('^%s ', geneID)
	geneID.regex.nodeB = sprintf (' %s$ ', geneID)
	indices.A = grep (geneID.regex.nodeA, all.edge.names.cyStyle)
	indices.B = grep (geneID.regex.nodeB, all.edge.names.cyStyle)
	indices.of.edges.with.geneIDs = c (indices.of.edges.with.geneIDs, indices.A, indices.B)	
	} # for geneID
	return (as.character (all.edge.names) [indices.of.edges.with.geneIDs])
	} # get.edge.names
#	
getCyEdgeNames <- function(edgefile) {
	cyedges <- mapply(paste, edgefile $Gene.1, " (", edgefile $edgeType, ") ", edgefile $Gene.2, sep="")
	return(cyedges)
}
 #	
#	Deal with SFK_i:  Make edges identical to SFK
add.sfki.edges <- function(genelist, edgefile) {
		genelist <- as.character(genelist)
		edgegenelist <- unique(c(edgefile$Gene.1, edgefile$Gene.2))
		SFKi  <- genelist[grepl("_i", genelist)]
		# and also an SFK without _i
		unlist(strsplit(SFKi, "_i", fixed = TRUE)) -> y
		SFK <- genelist[genelist %in% sfklist]
		# duplicate or subsitute
			s1 <- edgefile[edgefile$Gene.1 %in% y, ]
			s1$Gene.1 <- sapply(s1$Gene.1, function (x) paste(x, "_i", sep="")) 
			s2 <- edgefile[edgefile$Gene.2 %in% y, ,]
			s2$Gene.2 <- sapply(s2$Gene.2, function (x) paste(x, "_i", sep="")) 
		if (length(SFKi > 0) & length(SFK > 0)) { edgefile  <- rbind (edgefile, s1, s2) } else
		if (length(SFKi > 0) & length(SFK = 0)) {
			 edgefile[edgefile$Gene.1 %in% y, ] <- s1 
			 edgefile[edgefile$Gene.2 %in% y, ] <- s2 } 
		# if there is an SFK_i that is NOT represented as an SFK in genelist
		# extra edges will result	 
		print(SFKi); print(SFK)
		return(edgefile)	 
		}
#
get.String.edgefile <- function (stringedgefile, nodenames)  {
		nodenames <- as.character(nodenames)
		stnet <- read.table(stringedgefile, header=TRUE, sep = "\t", comment.char = "", na.strings='', stringsAsFactors=FALSE, fill=TRUE)
			names(stnet)[1]="node1"  # to edit out the 'X.' or "#" 
			# fix up the gene names 
			stnet$node1 <- sapply(stnet$node1, get.gene) 
			stnet$node2 <- sapply(stnet$node2, get.gene)  
			stnodes <- unique(c(as.character(stnet$node1), as.character(stnet$node2)))
			flub <- setdiff(stnodes, nodenames) # setdiff( nodenames, stnodes) is the non-string node set
			# if there is a String flub
			if (length(flub) >= 1) { 
				cat("\n","\t", "The following String names do not match ","\n","\t", flub)
				#  Just get rid of the offending nodes
				if (any(stnet$node1 %in% flub)) stnet <- stnet[-which(stnet$node1 %in% flub), ]
				if (any(stnet$node2 %in% flub)) stnet <- stnet[-which(stnet$node2 %in% flub), ]
								}
		# 	Rearrange data file		
			stn <- stnet[,c(1:2)]
			names(stn) <- c('Gene.1', 'Gene.2')
			stne <- stn
			stne$Weight <- stnet$experimental
			stne$edgeType <- "experimental"
			stnk <- stn
			stnk$Weight <- stnet$knowledge
			stnk$edgeType <- "knowledge"
			stnh <- stn
			stnh$Weight <- stnet$homology
			stnh$edgeType <- "homology"
			stncs <- stn
			stncs$Weight <- stnet$combined_score
			stncs$edgeType <- "combined_score"
			stn <- rbind (stne, stnk, stnh, stncs)
			nones <- which (stn$Weight==0)
			stn <- stn[-nones, ]		
			return (stn)	
			}
			
get.GM.edgefile <- function (gmfilename, nodenames) {  # text file from http://www.genemania.org
		nodenames <- as.character(nodenames)
		gmnet <- read.table(gmfilename, header=TRUE, sep = "\t", comment.char = "#", na.strings='', stringsAsFactors=FALSE, fill=TRUE)
			names(gmnet)[c(1:4)]=c("Gene.1", "Gene.2", "Weight", "edgeType")  
			gmnet$Gene.1 <- sapply(gmnet$Gene.1, get.gene) 
			gmnet$Gene.2 <- sapply(gmnet$Gene.2, get.gene)  
			# gmnet$Gene.1=as.factor(gmnet$Gene.1)
			# gmnet$Gene.2=as.factor(gmnet$Gene.2)
			 a = as.character(gmnet$Gene.1)
			 b = as.character(gmnet$Gene.2)
			gmnodes <- unique(c(a,b))
			flub <- setdiff(gmnodes, nodenames) # setdiff( nodenames, stnodes) is the non-GM node set
			# if there is an ID flub
			if (length(flub) >= 1) { 
				cat("\n","\t", "The following GM names do not match ","\n","\t", flub)
				#  Just get rid of the offending nodes
				if (any(gmnet$Gene.1 %in% flub)) gmnet <- gmnet[-which(gmnet$Gene.1 %in% flub), ]
				if (any(gmnet$Gene.2 %in% flub)) gmnet <- gmnet[-which(gmnet$Gene.2 %in% flub), ]
								}
	# 	Prune data file
			gmn <- gmnet[,c(1:4)]
			return(gmn)
			}
#
get.GM.edgefile.2 <- function (gmfilename, nodenames) {  
	# text file from http://www.genemania.org
	# for cross-species analysis, human gene names uppercase returned
	# 		so uncomment the following line
		# nodenames <- toupper(as.character(nodenames))
		nodenames <-as.character(nodenames)
		gmnet <- read.table(gmfilename, header=TRUE, skip=6, sep = "\t", comment.char = "#", na.strings='', stringsAsFactors=FALSE, fill=TRUE)
			names(gmnet)[c(1:4)]=c("Gene.1", "Gene.2", "Weight", "edgeType")  
			# Omit the following for non-human genes
			# gmnet$Gene.1 <- sapply(gmnet$Gene.1, get.gene) 
			# gmnet$Gene.2 <- sapply(gmnet$Gene.2, get.gene)  
			# gmnet$Gene.1=as.factor(gmnet$Gene.1)
			# gmnet$Gene.2=as.factor(gmnet$Gene.2)
			 a = as.character(gmnet$Gene.1)
			 b = as.character(gmnet$Gene.2)
			gmnodes <- unique(c(a,b))
			flub <- setdiff(gmnodes, nodenames) # setdiff( nodenames, stnodes) is the non-GM node set
			# if there is an ID flub
			if (length(flub) >= 1) { 
				cat("\n","\t", "The following GM names do not match ","\n","\t", flub)
				#  Just get rid of the offending nodes
				if (any(gmnet$Gene.1 %in% flub)) gmnet <- gmnet[-which(gmnet$Gene.1 %in% flub), ]
				if (any(gmnet$Gene.2 %in% flub)) gmnet <- gmnet[-which(gmnet$Gene.2 %in% flub), ]
								}
	# 	Prune data file
			gmn <- gmnet[,c(1:4)]
			return(gmn)
			}
#
get.GM.edgefile.rat <- function (gmfilename, species="rat") {  
	# text file from http://www.genemania.org
	# for cross-species analysis, human gene names uppercase returned
	# 		so convert to rat convention
		# nodenames <- toupper(as.character(nodenames))
		gmnet <- read.table(gmfilename, header=TRUE, skip=6, sep = "\t", comment.char = "#", na.strings='', fill=TRUE, stringsAsFactors=FALSE)
			names(gmnet)[c(1:4)]=c("Gene.1", "Gene.2", "Weight", "edgeType")  
			if(species=="human") {
				gmnet$Gene.1 <- sapply(gmnet$Gene.1, toratname)
				gmnet$Gene.2 <- sapply(gmnet$Gene.2, toratname)  }
	# 	Prune data file
			gmn <- gmnet[,c(1:4)]
			return(gmn)
			}
#
# function to take a small network and return group corr edges from selectededges.df
select.edgefile.edges <- function(small.edge.file, big.edge.file) { 
		a = as.character(small.edge.file$Gene.1)
		b = as.character(small.edge.file$Gene.2)
		enodes <- unique(c(a,b))
		edges1 <- big.edge.file[(big.edge.file$Gene.1 %in% enodes) & (big.edge.file$Gene.2 %in% enodes),]
		return(edges1[,c(2:5,1)])		}
		#	
#
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
#	
#funcion to filter networks and to get first order connected nodes								
filter.edges.1 <- function(nodenames, edge.file) {
			nodenames <-as.character(nodenames)
			 a = as.character(edge.file$Gene.1)
			 b = as.character(edge.file$Gene.2)
			edgefile.nodes <- unique(c(a,b))
			flub <- setdiff(edgefile.nodes, nodenames) 
			# show pruned nodes (turned off)
			# if (length(flub) >= 1) { 
			#	cat("\n","\t", "The following GM names do not match ","\n","\t", flub) }
		sel.edges.1 <- edge.file[edge.file$Gene.1 %in% nodenames,]
		sel.edges.2 <- edge.file[edge.file$Gene.2%in% nodenames,]
		sel.edges <- rbind(sel.edges.1, sel.edges.2)
	if(dim(sel.edges)[1] == 0) {return(NA)} else {
		return(unique(sel.edges)) }
								}
#
combine.edges <- function(edgefile1, edgefile2)	{
	# Two network files may have edges in common that are reversed
	 #	Therefore, test edges in reverse
	 #	This is easist to do in a single column
 	edgefile1$nodes.combined <- noquote(paste(edgefile1$Gene.1, edgefile1$Gene.2))
	edgefile1$nodes.reversed <- noquote(paste(edgefile1$Gene.2, edgefile1$Gene.1))
 	edgefile2$nodes.combined <- noquote(paste(edgefile2$Gene.1, edgefile2$Gene.2))
	edgefile2$nodes.reversed <- noquote(paste(edgefile2$Gene.2, edgefile2$Gene.1))
	#  Which edges are the reverse?  = stn[sg.reverse,]
	reversed <- edgefile2$nodes.combined %in% edgefile1$nodes.reversed
	#  Reverse these  
	# Note that the matrix conversion is reqired because of factor level collision
	fix2 <- as.matrix(noquote(as.matrix(edgefile2[reversed, c(2,1)])))
	if (nrow(fix2) >0) {
		names(fix2) <- c("Gene.1", "Gene.2")
		fixededge2=noquote(as.matrix(edgefile2))
		fixededge2[reversed, c(1:2)] <- fix2
		edgefile2[,c(1:2)] <- fixededge2[,c(1:2)] }
	# Revise the test columns 
	 	edgefile2$nodes.combined <- noquote(paste(edgefile2$Gene.1, edgefile2$Gene.2))
		edgefile2$nodes.reversed <- noquote(paste(edgefile2$Gene.2, edgefile2$Gene.1))
	#
	#  merge  :
	combined <- rbind(edgefile1, edgefile2)
	combined <- combined[order(combined$Weight, decreasing=TRUE),]
	combined <- combined[!duplicated(combined[,c(1,2,4)]), ]
	return(combined[,c(1:4)])
}	
#
get.corr.edges <- function(cf.corfile, cutoff) {   
	 cordfall = NULL; cordf=NULL; x=NULL; Gene2list=NULL; Gene.2=NULL
	 for (i in 1:ncol(cf.corfile)) {
		x <- which(abs(cf.corfile[,i]) >= as.numeric(cutoff))
		x <- x[!names(x) %in% Gene2list]
		if (length(x) == 1)	{
			Gene.2 <- colnames(cf.corfile)[i]
			Gene2list <- c(Gene2list, Gene.2)
			correlation <- cf.corfile[x,i]
			cordf <- data.frame(cbind(correlation, names(x), Gene.2))
			names(cordf)[2] <- "Gene.1"
			if (i == 1) cordfall <- cordf else cordfall <- rbind(cordfall, cordf)
			}
		if (length(x) > 1)  {
			Gene.2 <- colnames(cf.corfile)[i]
			Gene2list <- c(Gene2list, Gene.2)
			correlation <- cf.corfile[x,i]
			cordf <- data.frame(correlation)
			cordf$Gene.1 <- names(correlation)
			cordf$Gene.2 <- Gene.2
			if (i == 1) cordfall <- cordf else cordfall <- rbind(cordfall, cordf)
			 }
		} 
	return(cordfall)
	}
#
get.corr.edges.2 <- function(cf.corfile, cutoff) {   
	 cordfall = NULL; cordf=NULL; x=NULL; Gene2list=NULL; Gene.2=NULL
	 for (i in 1:ncol(cf.corfile)) {
		x <- which(abs(cf.corfile[,i]) >= as.numeric(cutoff))
		x <- x[!names(x) %in% Gene2list]
		if (length(x) == 1)	{
			Gene.2 <- colnames(cf.corfile)[i]
			Gene2list <- c(Gene2list, Gene.2)
			Weight <- cf.corfile[x,i]
			cordf <- data.frame(cbind(Weight, names(x), Gene.2))
			names(cordf)[2] <- "Gene.1"
			if (i == 1) cordfall <- cordf else cordfall <- rbind(cordfall, cordf)
			}
		if (length(x) > 1)  {
			Gene.2 <- colnames(cf.corfile)[i]
			Gene2list <- c(Gene2list, Gene.2)
			Weight <- cf.corfile[x,i]
			cordf <- data.frame(Weight)
			cordf$Gene.1 <- names(Weight)
			cordf$Gene.2 <- Gene.2
			if (i == 1) cordfall <- cordf else cordfall <- rbind(cordfall, cordf)
			 }
		} 
		cordfall$edgeType[cordfall$Weight<0] <- "negative correlation"
		cordfall$edgeType[cordfall$Weight>0] <- "positive correlation"
		cordfall$Weight <- as.numeric(cordfall$Weight)
	return(cordfall)
	}
# create two functions to generaly selecte correlation edges based on t-SNE Groups
count.group.intersect <- function (genepep1, genepep2, groups.df=pepgroups.df)	{
			a <- groups.df[grepl(genepep1, groups.df$Gene.Name, fixed=T),]
			b <- groups.df[grepl(genepep2, groups.df$Gene.Name, fixed=T),]
			o <- intersect(a$group, b$group)
			groupintersect <- length(o) 
			return(groupintersect)
	}	
	#
#
# simple version for use with mapply, specify groups.df beforehand
cgi <- function (x, y) length(intersect(groups.df[which(groups.df$Gene.Name %in% x), "group"], groups.df[which(groups.df$Gene.Name %in% y), "group"]))
#
select.group.edges <- function(a.p, b.p, groups.df, edge.file) { 
	a <- groups.df[groups.df$Gene.Name %in% a.p,]
	b <- groups.df[groups.df$Gene.Name %in% b.p,]
	o <- intersect(a$group, b$group)
	apeps <- as.character(a[a$group %in% o, "Gene.Name"])
	bpeps <- as.character(b[b$group %in% o, "Gene.Name"])
	ab.edges.1 <- edge.file[edge.file $Gene.1 %in% apeps & edge.file $Gene.2 %in% bpeps,]
	ab.edges.2 <- edge.file[edge.file $Gene.1 %in% bpeps & edge.file $Gene.2 %in% apeps,]
	ab.edges <- rbind(ab.edges.1, ab.edges.2)
	if(dim(ab.edges)[1] == 0) return(NA) 
	for (i in 1:dim(ab.edges)[1]) { 
		 	ab.edges$count[i] <- count.group.intersect(ab.edges$Gene.1[i], ab.edges$Gene.2[i], groups.df) }
	ab.edges$Weight <- ab.edges$Weight * ab.edges$count
	ab.edges <- ab.edges[which(abs(as.numeric(ab.edges $Weight))!=0),]
	return (ab.edges)	}
#
# revised loop-free version of a function to select group edges 
	#(supercedes select.group.edges)
	# note: for VERY LARGE datasets, a further breakup is required	
	# ===>  
get.group.edges <- function (groups.df= tbigroups.df, cf.corfile= tb.cor, cutoff=0.5) {   
	# make a list sorted by group
	grouplist <- dlply(groups.df, .(group))
	grouplist<- grouplist[lapply(grouplist, nrow)>1] # TBI 737 s e  # =  1063  with SED
	# make each element a vector of names
	namelist <- llply(grouplist, function(x) unlist(x$Gene.Name))
	# make all combinations of each group's names and return list 
	edge.list.1 <- llply(namelist, function (x) data.frame(t(combn(as.character(x),2))))
	edge.df <- ldply(edge.list.1)
	# add weight using new vector method:
	edgelist <- as.list(NULL)
	for (i in 1:round(dim(edge.df)[1]/1000)) {
		cat(i, "\t")
		j = seq.int(1, dim(edge.df)[1], 1000)[i]
		edgelist[i] <- list(edge.df[j:(j+999),])
	 		info <- lookitup(edgelist[[i]]$X1, edgelist[[i]]$X2, cf.corfile)
	 		if (length(info)==0) next
		edgelist[[i]]$Weight <- info
					 }
	# do stragglers
	 	if (i == round(dim(edge.df)[1]/1000))	 	{
			edgelist[i+1] <- list(edge.df[(j+999):dim(edge.df)[1],])
			edgelist[[i+1]]$Weight <- lookitup(edgelist[[i+1]]$X1, edgelist[[i+1]]$X2, cf.corfile)
				cat("final", "\t", i+1, "\n")	}
	edge.df <- ldply(edgelist)
	edge.df$Weight <- as.numeric(as.character(edge.df$Weight))	
	names(edge.df) <- c("group.id", "Gene.1", "Gene.2", "Weight")
	# identify edges for Cytoscape
	edge.df$edgeType <- "no correlation"
	edge.df$edgeType[edge.df$Weight<0.5] <- "negative correlation"
	edge.df$edgeType[edge.df$Weight>0.5] <- "positive correlation"
	selectededges.df <- edge.df[which(abs(as.numeric(edge.df$Weight))>=cutoff),]
	return(selectededges.df)
}
# looping version 
get.group.edges.1 <- function (groups.df=pepgroups.df, cf.corfile=nbpeps.cor, cutoff=0.5) {
	# make a list sorted by group
	grouplist <- dlply(groups.df, .(group))
	grouplist<- grouplist[lapply(grouplist, nrow)>1]
	# make each element a vector of names
	namelist <- llply(grouplist, function(x) unlist(x$Gene.Name))
	# make all combinations of each group's names and return data frame in a single step
	edge.df <- ldply(namelist, function (x) data.frame(t(combn(as.character(x),2))))
	# add weight
	for (i in 1:(dim(edge.df)[1])) {
		edge.df$Weight[i] <- lookup(edge.df$X1[i], edge.df$X2[i], corrtable=cf.corfile)
		}
	edge.df$Weight <- as.numeric(as.character(edge.df$Weight))	
	names(edge.df) <- c("group.id", "Gene.1", "Gene.2", "Weight")
	# identify edges for Cytoscape
	edge.df$edgeType <- "no correlation"
	edge.df$edgeType[edge.df$Weight<0] <- "negative correlation"
	edge.df$edgeType[edge.df$Weight>0] <- "positive correlation"
	return(edge.df[which(abs(as.numeric(edge.df$Weight))>=cutoff),])
}
#
#  Function to make an adjacency matrix, much more efficient than writing an edge file
#		Start with list of peptide or gene names that represent groups
#		Use:  ldnorm.adj <- rbind.fill.matrix(llply(esp.n.r, make.adj.mat))
#			  see LINCS_17.R
	make.adj.mat <- function(list.element)		{
			list.el.mat <- matrix(1, nrow=length(list.element), ncol=length(list.element))
			rownames(list.el.mat) <- list.element
			colnames(list.el.mat) <- list.element
			return(list.el.mat)
			}

		#
corr.edges.style <- function(windowname, edgefile) {
			# Set up Cytoscape to make positive correlation yellow, negative blue
		edgeTypes <- c("negative correlation", "positive correlation", "Protein")
		corredgecolors = c('#0000FF', "#FCD116", '#888888') 
    		#   blue; sign yellow; gray; 
   		myarrows <- c ('No Arrow', 'No Arrow', "No Arrow")
		setDefaultBackgroundColor(cy,'#888888', 'default' )
		# set up (have to make a graph first)
 	   	setEdgeTargetArrowRule(windowname, 'edgeType', edgeTypes, myarrows, default='No Arrow')  
	    setEdgeTargetArrowColorRule(windowname, "edgeType",  edgeTypes, corredgecolors, default.color='#FF0000')      
	 	setEdgeColorRule(windowname, 'edgeType', edgeTypes, corredgecolors, mode='lookup', default.color='#FDF8FF') 	# defalult zinc white
	 	line.widths <- 4.5*abs(as.numeric(edgefile$Weight))
		setEdgeLineWidthRule(windowname, "Weight", attribute.values=as.character(edgefile$Weight), line.widths, default.width=1.2)
		redraw(windowname)
		add.style.name <- paste("Corr Edges Style",length(getWindowList(cy)))
		if (!(add.style.name %in% getVisualStyleNames(cy)))	{ copyVisualStyle (windowname, "default", add.style.name) }
		hidePanel(cy, "Results")
}	
#  For Protein-peptide edges:
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



go.enrichment = function (genes, ontology, universe=character(0), pvalue=0.05, annotation='org.Hs.eg.db', conditionalSearch=TRUE)
{
   params = new ("GOHyperGParams", geneIds=genes,  ontology=ontology, annotation=annotation,
                 universeGeneIds=universe, pvalueCutoff = pvalue, conditional=conditionalSearch,
                 testDirection = "over")
   hgr <<- hyperGTest (params)
   return (hgr)

} 
#------------------------------------------------------------------------------------------------------------------------
#
#	Functions (Paul Shannon)
#
# go.enrichment
#------------------------------------------------------------------------------------------
# modified by MG to deal with zero return
#
tabular.go.enrichment =  function (genes, ontology, pvalue, geneUniverse=character (0))
{
  write (sprintf ('about to call go.enrichment with %d genes, ontolog=%s, pvalue=%s', length (genes), ontology, pvalue), stderr ())
  hgr = go.enrichment (genes, ontology, geneUniverse, pvalue)
  write (sprintf ('back from call to go.enrichment'), stderr ())
  if (dim(summary(hgr))[1]==0) {return(tbl)} else {
  tbl <<- summary (hgr)
  tbl <<- tbl [, c (7, 1, 2, 4, 5, 6)]
  colnames (tbl) [2] <<- 'GOID'
  rownames (tbl) <<- NULL
  virgin.tbl = TRUE
  sample.row =  list (Term='bogus', GOID='bogus', Pvalue=0.0, Expected=0, Count=0, Size=0, ID='bogus', Gene='bogus') #, Depth=99)
  df = data.frame (sample.row, stringsAsFactors=FALSE)

  for (r in 1:nrow (tbl)) {
    #write (sprintf ('assembling gene-based table, summary row %d', r), stderr ())
    # print (tbl [r,])
    goTerm = tbl [r, 2]
    gene.ids = geneIdsByCategory (hgr) [[goTerm]]
    for (gene.id in gene.ids) {
      old.row = tbl [r,]
      rownames (old.row) = NULL
      gene.symbol = geneSymbol (gene.id)
      #depth = go.depth (goTerm)
      new.row = cbind (old.row, Id=gene.id, zz=gene.symbol, stringsAsFactors=FALSE) # Depth=depth, stringsAsFactors=FALSE)
      colnames (new.row) = c ('Term', 'GOID', 'Pvalue', 'Expected', 'Count', 'Size', 'ID', 'Gene') #, 'Depth')
      if (virgin.tbl) {
        df [1,] = new.row
        virgin.tbl = FALSE
        }
      else
        df = rbind (df, new.row)
      } # for gene.id
    } # for r

  return (df)
}
} # tabular.go.enrichment
#------------------------------------------------------------------------------------------
geneSymbol = function (ids)
{
  result = list ()
  for (id in ids) {
    gene = NA
    if (id == '6287')
      gene = 'SAA@'
    else if (id %in% ls (org.Hs.egSYMBOL))
      gene = get (id, org.Hs.egSYMBOL)
    result[[id]] = gene
    }

  return (result)

} # geneSymbol
#----------------------------------------------------------------------------------------------------
# function to summarize GO terms
#
extract.GO <- function (datafile)	{
	 d <- datafile
	a <- dlply (d, .(Term))
	for (i in 1:(length(a))) {		
		a1 <- data.frame(a[i])
		names(a1) <- names(d)
		if (i == 1) {
			Genes <- paste(noquote(a1$Gene))
			go.df <- a1[1, c(1,5)]
			go.df$Genes <- paste(noquote(Genes), collapse = ", ") }
		Genes <- paste(noquote(a1$Gene))
		go.df2 <- a1[1, c(1,5)]
		go.df2$Genes <- paste(noquote(Genes), collapse = ", ")
		go.df <- rbind (go.df, go.df2)
		cat(i, "\t", Genes ) }
		return (go.df[order(go.df$Count, decreasing=TRUE),])	 
	}		  
	#
# Function to extract node names from, e.g.:
#	"ValidatedObjectAndEditString: validatedObject=ERBB3, editString=null"
strip.cy.goo <- function(test) {
		t1 <- unlist(strsplit(test, "Object="))
		t2 <- sapply(t1[2:length(t1)], function (x) (strsplit(x, ", ")))
		return(ldply(t2)$V1)
		}
#
# Remove auto-phosphorylation loops
remove.autophos <- function(edgefile)	{
		auto <- which (as.character(edgefile $Gene.1) == as.character(edgefile $Gene.2))
		if (length(auto) > 0) {
			newedgefile <- edgefile[-auto,] } else newedgefile <- edgefile
	return (newedgefile)	
}
remove.autophos.cy <- function(windowobj)	{
		win.edges <- getAllEdges(windowobj)
		gene1 <- sapply(win.edges, function(x) unlist(strsplit(x, " "))[1])
		gene2 <- sapply(win.edges, function(x) rev(unlist(strsplit(x, " ")))[1])
		auto <- which(mapply(identical, gene1, gene2))
		if (length(auto) > 0) {
			selectEdges(windowobj, win.edges[auto])
			deleteSelectedEdges(windowobj)   }
		}

# More functions
source("pepnetfunctions.R")		