#
# Tables and libraries required for MG's PSP RCy commands and scripts
#
#  use install.packages("package_name") to download the package
#  or, for bioconductor packages:
#
		# install.packages("qgraph")
		# library("qgraph")
 		# install.packages('fdrtool')
 		# library('fdrtool')
 		# library(sos)
				# 	findFn("cosine", maxPages=2, sortby="MaxScore")
#lib
	library(stringr)
	 library(Hmisc)
	 library(cluster)
	# library(adegenet)
	#library(bio3d) 
	library("RUnit")
	library(vegan)
	library(tsne)
	library(Rtsne)
	library ("gplots")
	library(devtools)
	library(genlasso)
#	bioconductor packages
# 	source("http://www.bioconductor.org/biocLite.R")
# 	biocLite("RCytoscape") 
	library(impute)
	library(PMA)
	# library(RCytoscape) # To use RCy3, loaded in MGRCyFunctions.R
	library('org.Hs.eg.db')
	library (GO.db)
	library (Category)
	library (GOstats)	
    library("Biostrings")
    library("PSICQUIC")
    library("AnnotationHub")
    library (RefNet)
    # library("KODAMA")
    library(preprocessCore)
    library(STRINGdb)
    library(KEGGlincs)
    # library(statnet)
    # library(RCyjs)
    # library(BrowserViz)
    # library(rcytoscapejs)
    library(glmnet)
#	last load these to make sure their commands are not masked
	library("data.table")
	library(igraph)
	#library(ndexr)
	library(plyr)
	library(rgl)
#
	# source("/Users/Mark_Grimes/Documents/_Work/R_/MGFunctions.R")
	# source("/Users/Mark_Grimes/Documents/_Work/R_/MGRCyFunctions.R")
	#
#
	# enzyme.sites<-read.table("/Users/Mark_Grimes/Documents/_Work/Phosphoproteomics/altered_enzyme9-12-13.txt", header=T, skip=0, sep = "\t", na.strings='', stringsAsFactors =FALSE, quote = "", fill=TRUE)
#
#  PSP data files
	#
	# load("/Users/Mark_Grimes/Documents/_Work/R_/PSP_data.RData")
	# load("PSP_data.RData")						
	# enzyme.sites<-read.table("enzym_act_inh_sites.txt", header=T, sep = "\t", na.strings='', fill=TRUE) # new version
	# names(enzyme.sites)[4]<-"RESIDUE_NUMBER"  # because we are using the group residue table
	# enzyme.sites[grepl("YES1", enzyme.sites$GENE_SYMBOL), "EFFECT_NAME"]  <- "enzymatic inhibition" # I looked up the paper on this
	# 	New enzyme.sites file
	# 		from Jon Kornhauser, 1/27/12
	#enzyme.sites<-read.table("enzym_act_inh_sites_20120127.txt", header=T, sep = "\t", na.strings='', fill=TRUE) 
	# enzyme.sites<-read.table("/Users/Mark_Grimes/Documents/_Work/Phosphoproteomics/Regulatory_sites_7Aug2013", header=T, skip=3, sep = "\t", na.strings='', quote = "", fill=TRUE) 
# enzyme.sites[grepl("YES1", enzyme.sites$GENE_SYMBOL), "EFFECT_NAME"]
	# NOTE: 
	 # enzyme.sites.new[grepl("FYN", enzyme.sites.new$GENE_SYMB) & grepl(420, enzyme.sites.new$MOD_RSD)  &  enzyme.sites.new$ORG == as.character("human"), "ON_FUNCTION"]  
	 # returns "molecular association, regulation"
	 # enzyme.sites.new[grepl("Fyn", enzyme.sites.new$GENE_SYMB) & grepl(420, enzyme.sites.new$MOD_RSD)  &  enzyme.sites.new$ORG == as.character("mouse"), "ON_FUNCTION"]
	# [1] enzymatic activity, induced
	# load("PSPData2.RData")	
	
	kinasesubstrateraw <- read.table("Kinase_Substrate_Dataset_050316", header=TRUE, skip=3, stringsAsFactors =FALSE, sep = "\t", na.strings='', fill=TRUE)
	#  make this generic: assume if there is a relationship in one species, it is conserved.
	kinasesubstrateraw -> kinsub
	if (any(is.na(kinsub$GENE))) {
		kinsub[which(is.na(kinsub$GENE)),"GENE"] <- as.character(kinsub[which(is.na(kinsub$GENE)),"KINASE"]) }
	if (any(is.na(kinsub$SUB_GENE))) {
		kinsub[which(is.na(kinsub$SUB_GENE)),"SUB_GENE"] <- as.character(kinsub[which(is.na(kinsub$SUB_GENE)),"SUBSTRATE"])	}
	kinase <- toupper(kinsub$GENE)
	substrate <- toupper(kinsub$SUB_GENE)
	kinsub <- data.frame(kinase,substrate)
	kinsub <- unique(kinsub)
	names(kinsub) <- c("Gene.1", "Gene.2")
	kinsub$Weight <- 0.2
	kinsub$edgeType <- "pp"
#
#	PathwayCommons
	#
	pcname <- "/Users/Mark_Grimes/Documents/_Work/R_/_LINCS/_Networks/Pathway\ Commons.6.All.EXTENDED_BINARY_SIF_part1_edges.tsv"
	pcname.lt <- "/Users/mark_lindsay_grimes/Documents/_Work/R_/_LINCS/_Networks/Pathway\ Commons.6.All.EXTENDED_BINARY_SIF_part1_edges.tsv"
	if(length(dir("/Users/Mark_Grimes/Documents/_Work/R_/_LINCS/_Networks/"))==0) pcname=pcname.lt	
				pcnet <- read.table(pcname, sep = "\t", skip = 0, header=TRUE, blank.lines.skip=T, fill=T, quote="\"", dec=".", comment.char = "", stringsAsFactors=F)
				# very large, may be a server job?
				# dim 4053116       5
				# need to filter interactions
					# want 'in-complex-with', NOT:'interacts-with', "controls-phosphorylation-of", "controls-state-change-of" "in-complex-with", "controls-expression-of", "controls-transport-of"
					# "neighbor-of" = Proteins are participants or controlers of the same interaction.(?)
			#
			interactions <- c('in-complex-with',  "controls-phosphorylation-of", "controls-state-change-of", "controls-expression-of", "controls-transport-of")
			pcnet <- pcnet[pcnet$INTERACTION_TYPE %in% interactions,]  
			# dim 1910638       5
			pcnet$Weight <- 0.2
			pcnet <- pcnet[,c(1,3,6,2,4)]
			names(pcnet) <- c("Gene.1", "Gene.2", "Weight", "edgeType", "Source")
			
	psiq <- PSICQUIC()
	
	enzyme.sites<-read.table("altered_enzyme9-12-13.txt", header=T, skip=0, sep = "\t", na.strings='', stringsAsFactors =FALSE, quote = "", fill=TRUE)	
	source("MGFunctions.R")
	source("MGRCyFunctions.R")

	# laptop:
	# enzyme.sites<-read.table("/Users/mglap/Documents/_Work/Phosphoproteomics/altered_enzyme9-12-13.txt", header=T, skip=0, sep = "\t", na.strings='', stringsAsFactors =FALSE, quote = "", fill=TRUE) # # # move this to same directory as script
	# regulatory.sites<-read.table("/Users/Mark_Grimes/Documents/_Work/Phosphoproteomics/Regulatory_sites_03Sept2013.txt", header=T, skip=3, sep = "\t", na.strings='', quote = "", fill=TRUE) 
	# # # move this to same directory as script
	regulatory.sites<-read.table("Regulatory_sites_03Sept2013.txt", header=T, skip=3, sep = "\t", na.strings='', quote = "", fill=TRUE) #	
#	
#
#		PSP.map<-read.table("PSP_HUGO4.txt", header=T, sep = "\t", na.strings='', fill=TRUE)
#		kinsub <- read.table("kin sub HUGO_CDK1.txt", header=TRUE, sep = "\t", na.strings='', fill=TRUE)
#		#		
#		save(PSP.map, kinsub, enzyme.sites, file = "PSP_data.RData")
#		save(PSP.map, enzyme.sites, kinsub, downstream.effect.sites, file="PSPData2.RData")
#		this lis loaded in "MGFunctions.R
