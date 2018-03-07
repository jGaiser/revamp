# Author: Mark Grimes  
# Purpose: to manipulate mass spectrometry data from CST pipeline.
# These are the functions that make the script work.
# For R 2.15.2 only
	#	if ([[theTextStorage string] length] > 120000) {
	#		breakSyntaxHighlighting = 1;
	#	}
#
#  Saved lists : 	save(rtklist, sfklist, sfk_ilist, sh2sh3s, ykinases, sh2list, Yphosphatases, sh3list, htsfactors, kins, rnaproteins, acetyltransferases, deacetylases, demethylases, methyltransferases, phosphatases, file="protein_families.RData")
	load(file="protein_families.RData")
#  
 	#
 	 fungenes <- unique(c(ykinases, sh2list, sfk_ilist, sh2sh3s, Yphosphatases, sh3list)) # 417 genes
	 #
# Get the plyr package; see http://had.co.nz/plyr/ >install.packages("plyr")
# Then run: 
library(plyr)
	printf <- function (thing) print(noquote(sprintf(thing)))			
# Function to select columns from spreadsheets:
get.colnames <- function(colnames) {
	cat("Enter column numbers to keep, one at a time, followed by <return>:","\n","\t")
		col.names <- read.table(stdin())
		col.names<-as.numeric(col.names$V1)
		return(col.names)	}
#
# Function to make names consistent:
	make.const.name <- function(namesv) {  
 		fixnames = c("gene_symbol", "geneSymbol",  "sitePosStr", "Site.Position",  "proteinID", "Protein.Id")
 		correctnames = c("Gene.Name", "Gene.Name",  "site_position", "site_position", "protein_id", "protein_id")
 		if (any(fixnames %in% namesv)) {
 		namesv.new <- mapvalues(namesv, fixnames[fixnames %in% namesv], correctnames[fixnames %in% namesv])	
 		return (namesv.new)
 		} else return(namesv)	}

#
# Function (modified) to split gene names and return first gene name single element 
# Excel makes dates out of some of the gene names so we have to fix this, and replace CDC2 with CDK1
# 		Note: OCT2, OCT4 are also problematic, modify the function if these are in data
get.gene.name<-function(cell){
	x<-unlist(strsplit(as.character(cell), ";"))
	if (x[1] == "CDC2"){
		x[1] <- "CDK1"}
	if (x[1] == "7-Sep") {
		x[1] <- "SEPT7"}
	if (x[1] == "2-Sep") {
		x[1] <- "SEPT2"}		
	if (x[1] == "2-Oct") {
		x[1] <- "OCT2"}
	if (x[1] == "4-Oct") {
		x[1] <- "OCT4"}
	return(x[1]) 
	}
get.gene.name.2<-function(cell){
	x<-unlist(strsplit(as.character(cell), ";"))
	if (x[2] == "CDC2"){
		x[2] <- "CDK1"}
	if (x[2] == "7-Sep") {
		x[2] <- "SEPT7"}
	if (x[2] == "2-Sep") {
		x[2] <- "SEPT2"}		
	if (x[2] == "2-Oct") {
		x[2] <- "OCT2"}
	if (x[2] == "4-Oct") {
		x[2] <- "OCT4"}
	return(x[2]) 
	}	
#
# A general version of get.gene.name; 
# use with sapply	
get.gene <- function(cell) {  
		fixgenes = c("CDC2", "2-Sep", "3-Sep", "4-Sep", "5-Sep", "7-Sep", "8-Sep", "9-Sep", "1-Oct", "2-Oct", "3-Oct", "4-Oct", "6-Oct", "7-Oct", "11-Oct", "1-Mar", "2-Mar", "3-Mar", "4-Mar", "5-Mar", "6-Mar", "7-Mar", "8-Mar", "9-Mar", "10-Mar", "11-Mar", "C11orf58", 'C17orf57', 'C3orf10',  'C7orf51', "C11orf59", "C4orf16")
		corrects = c("CDK1", "SEPT2", "SEPT3", "SEPT4", "SEPT5", "SEPT7", "SEPT8", "SEPT9", "POU2F1", "POU2F2", "POU5F1", "POU5F1", "POU3F1", "POU3F2", "POU2F3", "MARCH1", "MARCH2", "MARCH3", "MARCH4", "MARCH5", "MARCH6", "MARCH7", "MARCH8", "MARCH9", "MARCH10", "MARCH11", "SMAP", "EFCAB13", "BRK1", "NYAP1", "LAMTOR1", 'AP1AR')
		x<-unlist(strsplit(as.character(cell), ";"))	
		for (i in 1:length(fixgenes)) {
			if (x[1] == fixgenes[i]) {
			x[1] <- corrects[i] 
			return (x[1])
			} 
		}
	 return(x[1]) }
# This is used to simplify the gene name list
get.gene.2 <- function(cell) {  
		fixgenes = c("CDC2", "2-Sep", "3-Sep", "4-Sep", "5-Sep", "7-Sep", "8-Sep", "9-Sep", "1-Oct", "2-Oct", "3-Oct", "4-Oct", "6-Oct", "7-Oct", "11-Oct", "1-Mar", "2-Mar", "3-Mar", "4-Mar", "5-Mar", "6-Mar", "7-Mar", "8-Mar", "9-Mar", "10-Mar", "11-Mar", "C11orf58", 'C17orf57', 'C3orf10',  'C7orf51', "C11orf59", "C4orf16")
		corrects = c("CDK1", "SEPT2", "SEPT3", "SEPT4", "SEPT5", "SEPT7", "SEPT8", "SEPT9", "POU2F1", "POU2F2", "POU5F1", "POU5F1", "POU3F1", "POU3F2", "POU2F3", "MARCH1", "MARCH2", "MARCH3", "MARCH4", "MARCH5", "MARCH6", "MARCH7", "MARCH8", "MARCH9", "MARCH10", "MARCH11", "SMAP", "EFCAB13", "BRK1", "NYAP1", "LAMTOR1", 'AP1AR')
		x<-unlist(strsplit(as.character(cell), ";"))	
		for (i in 1:length(fixgenes)) {
			if (x[1] == fixgenes[i]) {
			x[1] <- corrects[i] 
			return (x[1])
			} 
		}
	 	if ((length(x)>1) & !(x[1] %in% datafile$Gene.Name) & (x[2] %in% datafile$Gene.Name)) return( x[2]) else
		return(x[1]) 		}
#  This function fixes only Excel substitutions and a few other misnamings
fix.excel.1 <- function(cell) {  
		fixgenes = c("CDC2", "2-Sep", "3-Sep", "4-Sep", "5-Sep", "7-Sep", "8-Sep", "9-Sep", "1-Oct", "2-Oct", "3-Oct", "4-Oct", "6-Oct", "7-Oct", "11-Oct", "1-Mar", "2-Mar", "3-Mar", "4-Mar", "5-Mar", "6-Mar", "7-Mar", "8-Mar", "9-Mar", "10-Mar", "11-Mar", "C11orf58", 'C17orf57', 'C3orf10',  'C7orf51', "C11orf59", "C4orf16", "10-Sep", "11-Sep", "15-Sep", "6-Sep")
		corrects = c("CDK1", "SEPT2", "SEPT3", "SEPT4", "SEPT5", "SEPT7", "SEPT8", "SEPT9", "POU2F1", "POU2F2", "POU5F1", "POU5F1", "POU3F1", "POU3F2", "POU2F3", "MARCH1", "MARCH2", "MARCH3", "MARCH4", "MARCH5", "MARCH6", "MARCH7", "MARCH8", "MARCH9", "MARCH10", "MARCH11", "SMAP", "EFCAB13", "BRK1", "NYAP1", "LAMTOR1", 'AP1AR', "SEPT10", "SEPT11", "SEPT15", "SEPT6")
		cellv <- unlist(strsplit(as.character(cell), "; "))
		if (any(fixgenes %in% cellv)) {
		cellv.new <- gsub(fixgenes[fixgenes %in% cellv], corrects[fixgenes %in% cellv], cellv)	
		return (paste(cellv.new, collapse="; "))
		} else return(cell)	}
		#
fix.excel <- function(cell) {  
  fixgenes = c("CDC2", "2-Sep", "3-Sep", "4-Sep", "5-Sep", "7-Sep", "8-Sep", "9-Sep", "1-Oct", "2-    Oct", "3-Oct", "4-Oct", "6-Oct", "7-Oct", "11-Oct", "1-Mar", "2-Mar", "3-Mar", "4-Mar", "5-Mar", "6-Mar",     "7-Mar", "8-Mar", "9-Mar", "10-Mar", "11-Mar", "C11orf58", 'C17orf57', 'C3orf10',  'C7orf51', "C11orf59",     "C4orf16", "10-Sep", "11-Sep", "15-Sep", "6-Sep", '14-Sep')
  corrects = c("CDK1", "SEPT2", "SEPT3", "SEPT4", "SEPT5", "SEPT7", "SEPT8", "SEPT9", "POU2F1",     "POU2F2", "POU5F1", "POU5F1", "POU3F1", "POU3F2", "POU2F3", "MARCH1", "MARCH2", "MARCH3", "MARCH4",     "MARCH5", "MARCH6", "MARCH7", "MARCH8", "MARCH9", "MARCH10", "MARCH11", "SMAP", "EFCAB13", "BRK1",     "NYAP1", "LAMTOR1", 'AP1AR', "SEPT10", "SEPT11", "SEPT15", "SEPT6", "SEPT14")
  cellv <- unlist(strsplit(as.character(cell), "; "))
  if (any(fixgenes %in% cellv)) {
    cellv.new <- gsub(fixgenes[fixgenes %in% cellv], corrects[fixgenes %in% cellv], cellv)        
    return (paste(cellv.new, collapse="; "))
  } else return(cell)    }
# This function returns a vector of values originally separated by semicolins in spreadsheet cells, 
#	stripped of all characters except commas 
get.sites <- function (cell) {
	cell <- gsub("\\t", "\\", cell, fixed=F)
    #determine if there are two or more phosphorylations
    if (any(grepl(",", cell))) {
    	multiphos <- TRUE
    	cellv <- unlist(strsplit(cell, "; ", fixed = TRUE))
		cellv <- gsub("[[:punct:]]", "", cellv)
		cellv <- gsub( "\\s", ", ", cellv) } else {
	multiphos <- FALSE	
    	cellv <- unlist(strsplit(cell, "; ", fixed = TRUE))
		cellv <- gsub("[[:punct:]]", "", cellv)
		}
    return(cellv)
}
#
# This pair of functions removes isoforms of proteins, returning only the gene/sites without isoforms and undefined gene names
simplify.gene <- function(gene, protein)	{
	pv <- unlist(strsplit(protein, "; ", fixed = TRUE))
	if (length(pv) > 1) {
	gv <- unlist(strsplit(gene, "; ", fixed = TRUE))
	if (grepl("iso", pv[1])) {genev = gv} else {
		if (any(grepl("iso", pv))) { genev <- gv[-grep("iso", pv)] } else genev = gv }
	if (grepl("LOC", genev[1])) {genev = genev} else {
		if (any(grepl("LOC", genev))) { genev <- genev[-grep("LOC", genev)] }  else genev = genev } 
	if (length(genev)==1) {genev = genev} else {
		if (any(grepl("RP11-", genev))) { genev <- genev[-grep("RP11-", genev)] }  else genev = genev } 
	return (paste(genev, collapse="; ")) } else return(gene)
}
simplify.site <- function(gene, protein, site)	{
	pv <- unlist(strsplit(protein, "; ", fixed = TRUE))
	if (length(pv) > 1) {
	gv <- unlist(strsplit(gene, "; ", fixed = TRUE))
	sv <- unlist(strsplit(site, "; ", fixed = TRUE))
	if (grepl("iso", pv[1])) {sitev1 = sv} else {
		if (any(grepl("iso", pv))) {sitev1 <- sv[-grep("iso", pv)]} else sitev1 = sv}   
	if (grepl("LOC", gv[1])) {sitev1 = sitev1} else {
		if (any(grepl("LOC", gv))) { sitev1 <- sitev1[-grep("LOC", gv)] }  else sitev1 = sitev1 } 
	if (length(gv)==1) {sitev1 = sitev1} else {
		if (any(grepl("RP11-", gv))) { sitev1 <- sitev1[-grep("RP11-", gv)] }  else sitev1 = sitev1 } 
	sitev <- get.sites(sitev1)   
	return (paste(sitev, collapse="; ")) } else return(get.sites(site))
}
# Functions to extract Gene.Name from Protein.Id & Fix reversal of sequence symbols
extract.name <- 	function(cell) {    
		x<-unlist(strsplit(as.character(cell), "\\|"))	
			return(x[2])			}
substar <- function(cell) { 			
			modcell <- str_replace_all(cell, "\\#", "*")
			return(modcell)			}
subMhash <- function(cell) { 			
			modcell <- str_replace_all(cell, "M\\*", "M#")
			return(modcell)			}
subKtilde <- function(cell) { 			
			modcell <- str_replace_all(cell, "K\\*", "K~")
			return(modcell)			}
#
name.peptide.1 <- function (genes, sites)	{
	genes.v <- unlist(strsplit(genes, "; ", fixed = TRUE))
	sites.v <- unlist(strsplit(sites, "; ", fixed = TRUE))
	Peptide.v <- as.character(noquote(paste(genes.v[1:length(genes.v)], sites.v[1:length(sites.v)], sep=" ")))
	Peptide <- paste(Peptide.v, collapse="; ")
	return(Peptide)
}
#
name.peptide <- function (genes, modification="p", sites)	{
	genes.v <- unlist(strsplit(genes, "; ", fixed = TRUE))
	sites.v <- unlist(strsplit(sites, "; ", fixed = TRUE))
	Peptide.v <- as.character(noquote(paste(genes.v[1:length(genes.v)], modification, sites.v[1:length(sites.v)], sep=" ")))
	Peptide <- paste(Peptide.v, collapse="; ")
	return(Peptide)
}
#  For peptide name in the format "GENE p 11N"
extract.genes.from.clist <- function (clusterlist.element) {
		element <- clusterlist.element[1]
		genes <- unique(sapply(as.character(element$Gene.Name),  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1]))
		return(genes)
			} 
extract.peps.from.clist <- function (clusterlist.element) {
		element <- clusterlist.element[1]
		return(as.character(element$Gene.Name))
			}
extract.genes.from.peplist <- function (clusterlist.element) {
		element <- clusterlist.element
		genes <- unique(sapply(element,  function (x) unlist(strsplit(x, " ",  fixed=TRUE))[1]))
		return(genes)
			}
#
#__________
 pick.unique <- function(genes) {
 			gene.v <- unlist(strsplit(as.character(genes), ";"))
			gene.v <- gsub(" ", "", gene.v, fixed=F)
 			genes <- 	paste(unique(gene.v), collapse=";")
 			return (genes)
 			}
 			# returns genes, separated by ";" no space
 pick.unique.2 <- function(sites) {
 			site.v <- unlist(strsplit(as.character(sites), ","))
			site.v <- gsub(" ", "", site.v, fixed=F)
			sites <- 	paste(unique(site.v), collapse=", ")
 			return (sites)
 			}			
 # returns sites separated by ", " one space	
 #
 #	Funcjtions to use org.Hs.egUNIPROT, then org.Hs.egGENENAME to go from UNIPROT to ENTREZ to GENENAME
	library('org.Hs.eg.db')
	sym2geneID <- function (sym) {
		if (sym %in% (keys(org.Hs.egSYMBOL2EG))) {
			return(org.Hs.egSYMBOL2EG[[sym]])
			} else return (NA)
		}
	geneID2sym <- function (geneID) {
		if (geneID %in% (keys(org.Hs.egSYMBOL))) {
			return(org.Hs.egSYMBOL[[geneID]])
			} else return (NA)
		}
	UNI.tab <- toTable(org.Hs.egUNIPROT)
	uni2geneID <- function (uni) {
		x<-unlist(strsplit(as.character(uni), ";"))
		uni <- x[1]
		if (uni %in% UNI.tab$uniprot_id) {
			generow <- which(UNI.tab$uniprot_id == as.character(uni)) 
			return(UNI.tab[generow[1], 1])
			} else return (NA)
		} 		# e.g. uni="P12931" = SRC
#
strippercent <- function(residue.raw) {    
			residue <- gsub("\\%", "\\", residue.raw, fixed=F) 
			return(residue)			}
stripbar <- function(cell) {    
			modcell <- gsub("\\|", "\\;", cell, fixed=F) 
			return(modcell)			}
stripdash <- function(cell) { 			
			modcell <- gsub("\\-", "\\", cell, fixed=F) 
			return(modcell)			}
strip.dot <- function(cell) { 			
			modcell <- gsub("\\.", "", cell, fixed=F) 
			return(modcell)			}
subsemi <- function(cell) { 			
			modcell <- gsub("\\;", "\\,", cell, fixed=F) 
			return(modcell)			}
strip.t <- function(cell) {
			modcell <- gsub("\\t", "\\", cell, fixed=F) 
			return(modcell)			}
strip.sigma <- function(cell) {
			modcell <- gsub("\\§", "\\", cell, fixed=F) 
			return(modcell)			}
stripslash <- function(cell) { 			
			modcell <- gsub("\\\"", "\\", cell, fixed=F) 
			return(modcell)			}			
strip_score <- function(cell) { 			
			modcell <- gsub("\\_", "", cell, fixed=F) 
			return(modcell)			}			
strip_i <- function(cell) { 			
			modcell <- gsub("_i", "", cell, fixed=F) 
			return(modcell)			}		
	strip.1 <- function(cell) { 			
			modcell <- gsub("\\.1", "", cell, fixed=F) 
			return(modcell)			}
	strip.2 <- function(cell) { 			
			modcell <- gsub("\\.2", "", cell, fixed=F) 
			return(modcell)			}
#				
get.sites.1 <- function (cell) 
{
    modcell <- strippercent(cell)
	    cell <- modcell
    modcell <- strip.sigma(cell)
	    cell <- modcell
    modcell <- strip.t(cell)
	    cell <- modcell
    modcell <- subsemi(cell)
	    cell <- modcell
    modcell <- stripslash(cell)
	    cell <- modcell
    modcell <- stripslash(cell)
    return(modcell)
}
#
find.row.robust =function(gene, residue) {			
	enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
	if(length(enzymerow) >= 1) {
	 		return(enzymerow) } else return(NA)	}
unraw <- function(residue.raw) {
			if (grepl("%", residue.raw)){
			residue <- gsub("\\%", "\\", residue.raw, fixed=F)} else residue = residue.raw			}
# The following will replace a factor (such as a column in a data.frame) with its levels.
unfactor <- function(factor) {
  levels(factor)[as.numeric(factor)]
}
find.effect <- function(gene, residue, enzymerow) {
		inhibitory=FALSE
		activating=FALSE
		enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
		# because not all sites are listed in enzyme.sites, "enzymerow" may not be assigned, have value = integer(0)
		# logical if here to test if rownumber has a value
		if (length(enzymerow) >= 1) {
			# proceed if there is a site to look up
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			inhibitory=TRUE
			print("found an inhibitory site")  
			return(inhibitory) } else {	
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			activating=TRUE
			print("found an activating site") 
			return(activating)} 
		}}}
#
# working towards modifying the data 
#
find.effect.2 <- function(gene, residue, enzymerow) {
		inhibitory=FALSE
		activating=FALSE
		enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
		# because not all sites are listed in enzyme.sites, "enzymerow" may not be assigned, have value = integer(0)
		# logical if here to test if rownumber has a value
		if (length(enzymerow) >= 1) {
			# proceed if there is a site to look up
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			inhibitory=TRUE
			effect.name <-"inhibitory site"  
			return(inhibitory) } else {	
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			activating=TRUE
			effect.name <- "activating site" 
			return(activating)} 
			}
		  }
		}
find.effect.3 <- function(gene, residue, enzymerow) {
		inhibitory=FALSE
		activating=FALSE
		effect.name=NA
		#look up effect		
		enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
		# because not all sites are listed in enzyme.sites, "enzymerow" may not be assigned, have value = integer(0)
		# logical if here to test if rownumber has a value
		if (length(enzymerow) >= 1) {
			# proceed if there is a site to look up
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			inhibitory=TRUE
			effect.name <-"inhibitory site"  
			} else {	
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			activating=TRUE
			effect.name <- "activating site" 
			} 
			}
		  return(effect.name)}
		}	
multisite <- function (residue) {
	resvec = unlist(strsplit(as.character(residue), ","))
	return(resvec)}
find.effect.4 <- function(gene, residue, enzymerow) {
		inhibitory=FALSE
		activating=FALSE
		effect.name=NA	
		#	multiple sites are handled next	
		if (grepl(",", residue)){
 			resvec = unlist(strsplit(as.character(residue), ","))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
 				if (length(enzymerow) >= 1) {
				# proceed if there is a site to look up
				if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
				(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
				inhibitory=TRUE
				effect.name <-"inhibitory site"  
				} else {	
				if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
				(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
				activating=TRUE
				effect.name <- "activating site" 
				} }	
		  return(effect.name)}}} else		# back to single residues, repeating the code
		enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
		# because not all sites are listed in enzyme.sites, "enzymerow" may not be assigned, have value = integer(0)
		# logical if here to test if rownumber has a value
		if (length(enzymerow) >= 1) {
			# proceed if there is a site to look up
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			inhibitory=TRUE
			effect.name <-"inhibitory site"  
			} else {	
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			activating=TRUE
			effect.name <- "activating site" 
			} 
			}
		  return(effect.name)}
		}
find.effect.5 <- function(gene, residue, enzymerow) {
		inhibitory=FALSE
		activating=FALSE
		effect.name="unknown"	
		#	multiple sites are handled next	
		if (grepl(",", residue)){
 			resvec = unlist(strsplit(as.character(residue), ","))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
 				if (length(enzymerow) >= 1) {
				# proceed if there is a site to look up
				if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
				(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
				inhibitory=TRUE
				effect.name <-"inhibitory site"  
				} else {	
				if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
				(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
				activating=TRUE
				effect.name <- "activating site" 
				} }	
		  return(effect.name)}}} else		# back to single residues, repeating the code
		enzymerow <- which((enzyme.sites$GENE_SYMBOL == as.character(gene)) & (enzyme.sites$RESIDUE_NUMBER == as.character(residue))) 
		# because not all sites are listed in enzyme.sites, "enzymerow" may not be assigned, have value = integer(0)
		# logical if here to test if rownumber has a value
		if (length(enzymerow) >= 1) {
			# proceed if there is a site to look up
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic inhibition") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			inhibitory=TRUE
			effect.name <-"inhibitory site"  
			} else {	
			if ((enzyme.sites[enzymerow, "EFFECT_NAME"] =="enzymatic activation") & 
			(enzyme.sites[enzymerow, "MODIFICATION_TYPE"] == "PHOSPHORYLATION")) {
			activating=TRUE
			effect.name <- "activating site" 
			} 
			}
		  } else effect.name = "unknown"
		return(effect.name)}
change.sign<- function(site, var1) {
		if (site == "inhibitory site")	{
			var1 <- -as.numeric(var1)
		} else if (site != "inhibitory site")  {
			var1 <- as.numeric(var1)}
			}	
			#						 		 
find.Y_inhibitory  <- function(gene, residue) {
		inhibitory=FALSE
		activating=FALSE
		effect.name="unknown"	
		sfklist  <- c("BLK", "FGR", "FRK", "FYN", "HCK", "LCK", "LYN", "PTK6", "SRC", "YES1" ) 
		# PTK6 is BRK; SRM lacks C-term reg region
		#	multiple sites are handled next	
		if (grepl(",", residue)){
 			resvec = unlist(strsplit(as.character(residue), ","))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) > 490)	inhibitory=TRUE
 				residue <- paste( noquote("Y"), residue, sep="", collapse=NULL)  # come back to this later if not pY data
 				a = downstream.effect.sites[grepl(gene, downstream.effect.sites$Gene.Name) & grepl(residue, downstream.effect.sites$Residue),]
				effect.name = as.character(a$Effect)
				if("enzymatic inhibition" %in% effect.name) inhibitory=TRUE
				if("inhibition" %in% effect.name) inhibitory=TRUE
				if("enzymatic activation" %in% effect.name)activating=TRUE # may be useful later
				if("activation" %in% effect.name) activating=TRUE
				} 
				return(inhibitory)} else		
		  # back to single residues, repeating the code
		  if (!grepl(",", residue)){
 				if (gene %in% sfklist & as.numeric(residue) > 490)	inhibitory=TRUE
 				residue <- paste( noquote("Y"), residue, sep="", collapse=NULL)  # come back to this later if not pY data
 				a = downstream.effect.sites[grepl(gene, downstream.effect.sites$Gene.Name) & grepl(residue, downstream.effect.sites$Residue),]
				effect.name = as.character(a$Effect)
				if("enzymatic inhibition" %in% effect.name) inhibitory=TRUE
				if("inhibition" %in% effect.name) inhibitory=TRUE
				if("enzymatic activation" %in% effect.name)activating=TRUE # may be useful later
				if("activation" %in% effect.name) activating=TRUE
		  return(inhibitory)	}	  } 
		  #\
find.Y_inhibitory.2  <- function(gene, residue) {
	# new version as of 1/25/12
		inhibitory=FALSE
		activating=FALSE
		effect.name="unknown"	
		sfklist  <- c("BLK", "FGR", "FRK", "FYN", "HCK", "LCK", "LYN", "PTK6", "SRC", "YES1" ) # PTK6 is BRK; SRM lacks C-term reg region
		#	multiple sites are handled next	
		if (grepl(",", residue)){
 			resvec = unlist(strsplit(as.character(residue), ","))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) > 490)	inhibitory=TRUE
 				residue <- paste( noquote("Y"), residue, sep="", collapse=NULL)  # come back to this later if not pY data
 				a = ENZYME.sites[grepl(gene, ENZYME.sites$GENE.SYMBOL) & grepl(residue, ENZYME.sites$RSD),]
				effect.name = as.character(a$Effect)
				if("inhibitory" %in% effect.name) inhibitory=TRUE
				if("inhibition" %in% effect.name) inhibitory=TRUE
				if("activating" %in% effect.name)activating=TRUE # may be useful later
				if("activation" %in% effect.name) activating=TRUE
				} 
				return(inhibitory)} else		
		  # back to single residues, repeating the code
		  if (!grepl(",", residue)){
 				if (gene %in% sfklist & as.numeric(residue) > 490)	inhibitory=TRUE
 				residue <- paste( noquote("Y"), residue, sep="", collapse=NULL)  # come back to this later if not pY data
 				a = ENZYME.sites[grepl(gene, ENZYME.sites$GENE.SYMBOL) & grepl(residue, ENZYME.sites$RSD),]
				effect.name = as.character(a$Effect)
				if("inhibitory" %in% effect.name) inhibitory=TRUE
				if("inhibition" %in% effect.name) inhibitory=TRUE
				if("activating" %in% effect.name)activating=TRUE # may be useful later
				if("activation" %in% effect.name) activating=TRUE
		  return(inhibitory)	}	  } 
#\
		  #
find.inhibitory.1  <- function(gene, residue) {
		inhibitory=FALSE
		activating=FALSE
		effect.name="unknown"	
		sfklist  <- c("BLK", "FGR", "FRK", "FYN", "HCK", "LCK", "LYN", "PTK6", "SRC", "YES1" ) 
		# PTK6 is BRK; SRM lacks C-term reg region
		#	multiple sites are handled next	
		if (grepl(",", residue)){
 			resvec = unlist(strsplit(as.character(residue), ","))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) >= 490)	inhibitory=TRUE
 				a = enzyme.sites[grepl(gene, enzyme.sites$GENE_SYMBOL) & grepl(residue, enzyme.sites$RESIDUE_NUMBER) &  enzyme.sites$ORGANISM == as.character(species), ]
				effect.name = as.character(a$EFFECT_NAME)
				if("enzymatic inhibition" %in% effect.name) inhibitory=TRUE
				if("enzymatic activation" %in% effect.name) activating=TRUE
				} 
				return(inhibitory)} else		
		  # back to single residues, repeating the code
		  if (!grepl(",", residue)){
 				residue <- gsub("\\ ", "\\", residue, fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) >= 490)	inhibitory=TRUE
 				a = enzyme.sites[grepl(gene, enzyme.sites$GENE_SYMBOL) & grepl(residue, enzyme.sites$RESIDUE_NUMBER) &  enzyme.sites$ORGANISM == as.character(species), ]
				effect.name = as.character(a$EFFECT_NAME)
				if("enzymatic inhibition" %in% effect.name) inhibitory=TRUE
				if("enzymatic activation" %in% effect.name) activating=TRUE
		  return(inhibitory)	}	  } 
  #\
 # next version for enzyme.sites = "altered_enzyme9-12-13.txt": 
 find.inhibitory  <-function(gene, residue, specie="human") {
		inhibitory=FALSE
		activating=FALSE
		effect.name="unknown"	
		# sfklist  <- c("BLK", "FGR", "FRK", "FYN", "HCK", "LCK", "LYN", "PTK6", "SRC", "YES1" ) 
		# PTK6 is BRK; SRM lacks C-term reg region
		#	multiple sites are handled next	
		if (grepl(",", residue)){
			multiphos <- TRUE
 			resvec = unlist(strsplit(as.character(residue), ";"))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) >= 490)	inhibitory=TRUE
 				a = enzyme.sites[grepl(gene, enzyme.sites$Gene.Symbol) & (grepl(residue, enzyme.sites$Residue) | grepl((as.numeric(residue) + 1), enzyme.sites$Residue) | grepl((as.numeric(residue) + 2), enzyme.sites$Residue)) &  enzyme.sites$Species == as.character(species), ]
				effect.name = as.character(a$Effect)
				if("enzymatic activity, inhibited" %in% effect.name) inhibitory=TRUE
				if("enzymatic activity, induced" %in% effect.name) activating=TRUE
				} 
				return(inhibitory)} else		
		  # back to single residues, repeating the code
		  if (!grepl(",", residue)){
 				residue <- gsub("\\ ", "\\", residue, fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) >= 490)	inhibitory=TRUE
 				a = enzyme.sites[grepl(gene, enzyme.sites$Gene.Symbol) & (grepl(residue, enzyme.sites$Residue) | grepl((as.numeric(residue) + 1), enzyme.sites$Residue) | grepl((as.numeric(residue) + 2), enzyme.sites$Residue)) &  enzyme.sites$Species == as.character(specie), ]
				effect.name = as.character(a$Effect)
				if("enzymatic activity, inhibited" %in% effect.name) inhibitory=TRUE
				if("enzymatic activity, induced" %in% effect.name) activating=TRUE
		  return(inhibitory)	}	  }
  #\
#
 # This version uses the updated regulatory.sites file (Regulatory_sites_03Sept2013.txt) and format from PSP: 
	find.inhibitory.new  <- function(gene, residue) {
		inhibitory=FALSE
		activating=FALSE
		effect.name="unknown"	
		sfklist  <- c("BLK", "FGR", "FRK", "FYN", "HCK", "LCK", "LYN", "PTK6", "SRC", "YES1" ) 
		# PTK6 is BRK; SRM lacks C-term reg region
		#	multiple sites are handled next	
		if (grepl(",", residue)){
			multiphos <- TRUE
 			resvec = unlist(strsplit(as.character(residue), ";"))
 			for (q in 1: length(resvec)){
 				residue <- gsub("\\ ", "\\", resvec[q], fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) >= 490)	inhibitory=TRUE
 				a = regulatory.sites[grepl(gene, regulatory.sites$GENE_SYMB) & (grepl(residue, regulatory.sites$MOD_RSD) | grepl((as.numeric(residue) + 1), regulatory.sites$MOD_RSD)) &  regulatory.sites$ORG == as.character(species), ]
				effect.name = as.character(a$ON_FUNCTION)
				if("enzymatic activity, inhibited" %in% effect.name) inhibitory=TRUE
				if("enzymatic activity, induced" %in% effect.name) activating=TRUE
				} 
				return(inhibitory)} else		
		  # back to single residues, repeating the code
		  if (!grepl(",", residue)){
 				residue <- gsub("\\ ", "\\", residue, fixed=F)
 				if (gene %in% sfklist & as.numeric(residue) >= 490)	inhibitory=TRUE
 				a = regulatory.sites[grepl(gene, regulatory.sites$GENE_SYMB) &  (grepl(residue, regulatory.sites$MOD_RSD) | grepl((as.numeric(residue) + 1), regulatory.sites$MOD_RSD)) &  regulatory.sites$ORG == as.character(species), ]
				effect.name = as.character(a$EFFECT_NAME)
				if("enzymatic activity, inhibited" %in% effect.name) inhibitory=TRUE
				if("enzymatic activity, induced" %in% effect.name) activating=TRUE
		  return(inhibitory)	}	  } 
  #  #
#
# This function uses the simplifed Peptide.Name, which requires simplify.gene, simplify.site, name.peptide
# 	Also requires lookup.effect, enzyme.sites in the format of altered_enzyme9-12-13.txt
return.effect  <- function(Peptide.Name, specie="human") {
		effect.name="unknown"	
 		pepvec = unlist(strsplit(as.character(Peptide.Name), "; "))
		for (i in 1: length(pepvec)){
			#	multiple sites are handled first	
			if (grepl(",", pepvec[i]))	{
				multivec <- unlist(strsplit(as.character(pepvec), ", "))
				gene <- unlist(strsplit(as.character(multivec), " "))[1]
				residue <- unlist(strsplit(as.character(multivec), " "))[2]
				effect.name <- lookup.effect(gene, residue, specie)
				if (any(grepl("inhibited", effect.name ))) {return(effect.name[1]); break }
				residue <- multivec[2]
				effect.name <- lookup.effect(gene, residue, specie)
				if (any(grepl("inhibited", effect.name ))) {return(effect.name[1]); break } } else {
			# singly phosphorylated peptides	
			gene <- unlist(strsplit(as.character(pepvec), " "))[1]
			residue <- unlist(strsplit(as.character(pepvec), " "))[2]
			effect.name <- lookup.effect(gene, residue, specie)
				if (any(grepl("inhibited", effect.name ))) {return(effect.name[1]); break}
 				if (any(grepl("induced", effect.name ))) {return(effect.name)[1]; break}
 				}}
 			return(effect.name[1]) }
 #			
#
 #
lookup.effect <- function (gene, residue, specie="human")		{
	enzyme.sites$Effect <- noquote(as.character(enzyme.sites$Effect))
		effect.name="unknown"	
		specie=as.character(specie)
 		if (gene %in% sfklist & as.numeric(residue) >= 490)	{
 					effect.name <- "enzymatic activity, inhibited"
 					return(effect.name) }
 			residue <- as.numeric(as.character(residue))		
 			a = enzyme.sites[grepl(gene, enzyme.sites$Gene.Symbol) & as.numeric(gsub("[[:upper:]]", "", enzyme.sites$Residue))==residue &  enzyme.sites$Species == specie, ]
 			if (dim(a)[1]>0) {effect.name[1] <- a$Effect[1]}
			a1 = enzyme.sites[grepl(gene, enzyme.sites$Gene.Symbol) & as.numeric(gsub("[[:upper:]]", "", enzyme.sites$Residue))==(residue+1) &  enzyme.sites$Species == specie, ]
 			if (dim(a1)[1]>0) {effect.name[2] <- a1$Effect[1]}
			a2 = enzyme.sites[grepl(gene, enzyme.sites$Gene.Symbol) & as.numeric(gsub("[[:upper:]]", "", enzyme.sites$Residue))==(residue+2) &  enzyme.sites$Species == specie, ]
			if (dim(a2)[1]>0) {effect.name[3] <- a2$Effect[1]}
		  if (any(grepl("activity", effect.name))) {
		  	return(effect.name[grep("activity", effect.name)][1]) } else return(effect.name) }	   
 #  #
#
calc.ratio <- function(control, treated) {
	if ((is.na(control) & is.na(treated))) {
		ratio=NA
		return(ratio) } else
	control[is.na(control)] <- 0
	treated[is.na(treated)] <- 0
	if (control==treated) {
		ratio=1
		return(ratio) } else
	if (control > treated) {
		if (treated==0) {
			ratio=-50
			return(ratio) } else
		if (control==0) {
			ratio=-10 
			return(ratio) } else
		if ((control < 0) & (treated < 0)) {
			ratio <- -(treated/control)
			return(ratio) } else
		if (treated < 0) {
			diff.f <- (treated - control) 	#  signal is the difference; a negative number
			denom.f <- (treated + control) 	#  positive if treated is more neg than control pos
			if (denom.f > 0) {
				ratio=-(diff.f/treated) 
				return(ratio) } else
			if (denom.f < 0) {
				ratio=(diff.f/control) 
				return(ratio) } } else
		ratio <- -1/(treated/control)		
		return(ratio) }
	if (control < treated) {		
		if (control==0) {
			ratio=50
			return(ratio) } else
		if (treated==0) {
			ratio=10 
			return(ratio) } else
		if ((control < 0) & (treated < 0)) {
			ratio <- 1/(treated/control)
			return(ratio) } else
		if (control < 0) {
			diff.f <- (treated - control) 	#  a positive number
			denom.f <- (treated + control) 	#  positive if control is more neg than treated pos
			if (denom.f > 0) {
				ratio=-(diff.f/control) 
				return(ratio) } else
			if (denom.f < 0) {
				ratio=(diff.f/treated) 
				return(ratio) } } else
		ratio <- (treated/control)  
		return(ratio)}
	}
#						
# modify calc.ratio to exclude the possibility of negative values in data and limit values to =/-100
calc.ratio.2 <- function(control, treated) {
	if ((is.na(control) & is.na(treated))) {
		ratio=NA
		return(ratio) } else
	if (control==treated) {
		ratio=1
		return(ratio) } else
	if (control > treated) {
		if (treated==0) {
			ratio=-100
			return(ratio) } else
		ratio <- -1/(treated/control)		
		if (ratio >= -100) return(ratio) else return(-100) }
	if (control < treated) {		
		if (control==0) {
			ratio=100
			return(ratio) } else
		ratio <- (treated/control)  
		if (ratio <=100) return(ratio) else return(100) }
	}
#
# use log2 for this verion:
calc.log2.ratio <- function(control, treated, limit=log2(1024)) {
	if ((is.na(control) & is.na(treated))) {
		ratio=NA
		return(ratio) } else
	if (control==treated) {
		ratio=1
		return(ratio) } else
	if (control > treated) {
		if (treated==0) {
			ratio=-limit
			return(ratio) } else
		ratio <- log2(treated/control) - 1	
		if (ratio >= -limit) return(ratio) else return(-limit) }
	if (control < treated) {		
		if (control==0) {
			ratio= limit
			return(ratio) } else
		ratio <- log2(treated/control) + 1 
		if (ratio <= limit) return(ratio) else return(limit) }
	}
#  This one does returns fractions between 0 and 1
calc.log2.ratio.0 <- function(control, treated, limit=log2(1024)) {
	if ((is.na(control) & is.na(treated))) {
		ratio=NA
		return(ratio) } else
	if (control==treated) {
		ratio=1
		return(ratio) } else
	if (control > treated) {
		if (treated==0) {
			ratio=-limit
			return(ratio) } else
		ratio <- log2(treated/control) 	
		if (ratio >= -limit) return(ratio) else return(-limit) }
	if (control < treated) {		
		if (control==0) {
			ratio= limit
			return(ratio) } else
		ratio <- log2(treated/control) 
		if (ratio <= limit) return(ratio) else return(limit) }
	}
#
#  The following is for merging data from two runs of the same sample
# 	use: 	datafile$SY5Y <- mapply(merge2cols, colv1=datafile[,7], colv2=datafile[,9]) #  
merge2cols <- function (colv1, colv2) {
	newcolv=NA
	if (is.na(colv1) & is.na(colv2)) {
		newcolv=NA 
		return(newcolv)} else
	if (is.na(colv1) | is.na(colv2)) {
		newcolv <- sum(colv1, colv2, na.rm=TRUE)
		return(newcolv) } else
	if (colv1 == 0 | colv2 == 0) {
		newcolv <- colv1 + colv2
		return(newcolv)} else
	newcolv <- (colv1 + colv2)/2
	return(newcolv) }
	#
# This function is for merging .x and .y columns after merging based on a single column
mergeXYcols <- function (df) {
	df <- df[,order(names(df))]
  newcolv=NULL; xcol=NULL; ycol=NULL; newcolname=NULL; xcolname=NULL
  newdf <- data.frame(NULL)
  datadf <- df[,!grepl(".x", names(df), fixed=TRUE)]
  datadf <- datadf[,!grepl(".y", names(datadf), fixed=TRUE)]
  for (i in 1:length(names(df)[grepl(".x", names(df))])) {
  	xcolname <- names(df)[grepl(".x", names(df), fixed=TRUE)][i]
  	ycolname <- names(df)[grepl(".y", names(df), fixed=TRUE)][i]
  	newcolname <- gsub(".x", "", xcolname, fixed=TRUE)
	xcol <- df[, xcolname]
	ycol <- df[, ycolname]
	ycol[is.na(ycol)] <- xcol[is.na(ycol)]
	if (i==1) {newdf <- data.frame(ycol, stringsAsFactors=FALSE)} else {
	newdf[, i] <- ycol}
	names(newdf)[i] <- newcolname
	 } 
   return(cbind(newdf,datadf))
	}
	#
# Function to ddpply on Peptide.Sequence
	# due to M# within a single data set, ddply will have be be done on Peptide.Sequence
collapse.peptide <- function(df, datacols)	{
	if (class(df[,datacols[1]]) == "character" | class(df[,datacols[1]]) == "factor") {
	  	df[,datacols] <- sapply(df[,datacols], function(x) as.numeric(as.character(x)))	  }
	if (class(df[,datacols[1]]) == "integer") {
	  	df[,datacols] <- sapply(df[,datacols], function(x) as.numeric(x))	  }
	datadf <- df [, c(which(names(df)=="Peptide.Sequence"), datacols)]
	nondatadf <- df[, -datacols] 
	newdatadf <- ddply(datadf, .(Peptide.Sequence), numcolwise(sum.na))
	newnondatadf <- nondatadf[!duplicated(nondatadf$Peptide.Sequence),]
	newdf <- merge(newnondatadf, newdatadf, by="Peptide.Sequence", sort=F, all=TRUE)
	return(newdf)
	}
	#
# modify collapse.peptide for LINCS lung cancer project
	# Get best site name for peptides; requires get.longest.name:
get.longest.name <- function(seq, site.df) {
	allsitenames <- site.df[which(site.df$Peptide.Sequence==seq), "site_position"]
	sitename <- allsitenames[which(str_length(allsitenames)==max(str_length(allsitenames)))]
	return(sitename[1])
	}	
get.longest.seq <- function(pepname, nondata.df) {
	allseqs <- nondata.df[which(nondata.df$Peptide.Name==pepname), "sequence"]
	allpepseqs <- gsub("#", "", allseqs)
	Peptide.Sequence <-allpepseqs[which(str_length(allpepseqs)==max(str_length(allpepseqs)))]
	longseq <- allseqs[which(allpepseqs==Peptide.Sequence)]
	return(list(longseq[1], Peptide.Sequence[1]))
	}
#	
collapse.peptide.lc <- function(df, collapse.colname, datacols)	{
	if (class(df[,datacols[1]]) == "character" | class(df[,datacols[1]]) == "factor") {
	  	df[,datacols] <- sapply(df[,datacols], function(x) as.numeric(as.character(x)))	  }
	if (class(df[,datacols[1]]) == "integer") {
	  	df[,datacols] <- sapply(df[,datacols], function(x) as.numeric(x))	  }
	if(collapse.colname=="sequence") {
		df$Peptide.Sequence <-gsub("#", "", df$sequence) 
		collapse.colname="Peptide.Sequence"} 	
	datadf <- df [, c(which(names(df)==collapse.colname), datacols)]
	nondatadf <- df[, -datacols] 
	newdatadf <- ddply(datadf, collapse.colname, numcolwise(sum.na.3))
	newnondatadf <- nondatadf[!duplicated(nondatadf[,collapse.colname]),]
	if(collapse.colname=="Peptide.Sequence") {	
		for(i in 1:length(newdatadf[,collapse.colname])){
		seq=newdatadf[,collapse.colname][i]
		sitename <- get.longest.name(seq, nondatadf)
		newnondatadf[which(newnondatadf[,collapse.colname]==seq), "site_position"] <- sitename
		}} else {
		for(i in 1:length(newdatadf[,collapse.colname])){
		pepname=newdatadf[,collapse.colname][i]
		newseq <- get.longest.seq(pepname, nondatadf)
		newnondatadf[which(newnondatadf[,collapse.colname]== pepname), "sequence"] <- newseq[[1]]
		newnondatadf[which(newnondatadf[,collapse.colname]== pepname), "Peptide.Sequence"]<-newseq[[2]]
		}
	newdf <- merge(newnondatadf, newdatadf, by=collapse.colname, sort=F, all=TRUE)
	return(newdf)
	}}
#	
collapse.peptide.lc.1 <- function(df, datacols)	{
	if (class(df[,datacols[1]]) == "character" | class(df[,datacols[1]]) == "factor") {
	  	df[,datacols] <- sapply(df[,datacols], function(x) as.numeric(as.character(x)))	  }
	if (class(df[,datacols[1]]) == "integer") {
	  	df[,datacols] <- sapply(df[,datacols], function(x) as.numeric(x))	  }
	df$Peptide.Sequence <-gsub("#", "", df$sequence)  	
	datadf <- df [, c(which(names(df)=="Peptide.Sequence"), datacols)]
	nondatadf <- df[, -datacols] 
	newdatadf <- ddply(datadf, .(Peptide.Sequence), numcolwise(sum.na.3))
	newnondatadf <- nondatadf[!duplicated(nondatadf$Peptide.Sequence),]
	for(i in 1:length(newdatadf$Peptide.Sequence)){
		seq=newdatadf$Peptide.Sequence[i]
		sitename <- get.longest.name(seq, nondatadf)
		newnondatadf[which(newnondatadf$Peptide.Sequence==seq), "site_position"] <- sitename
	}
	newdf <- merge(newnondatadf, newdatadf, by="Peptide.Sequence", sort=F, all=TRUE)
	return(newdf)
	}
#
# Function to convert to PSP "Site Sequence" format
convert.peptide <- function (peptide.sequence)	{
	x <- unlist(strsplit(peptide.sequence, "*", fixed = TRUE))
	for (i in 1:(length(x)-1))	{
			str_sub(x[i], -1, -1) <- tolower(substring(x[i], nchar(x[i])))
			}
		xnew <- paste(x, sep="", collapse="")	
		return(xnew)		
	}
#
# Method to do find overlapping peptide sequences; requires library("Biostrings")
merge.peptides <- function (pdata, gene) 	{	
	if (dim(pdata[grepl(gene, pdata$Gene.Name, fixed=TRUE), ])[1] == 0) return(NA) else {
   m=NULL
   pdata.new <- pdata
   pepgroup=NULL; pepm=NULL; duprows=NULL; duplist=as.list(NULL)
   dupes=NULL; duprows=NULL; sumdup=NULL; largepep=NULL; smallerpep=NULL
	if (dim(pdata[grepl(gene, pdata$Gene.Name, fixed=TRUE), ])[1] > 1) {
		gene <- paste(gene, " ", sep = "")
		pepgroup <- pdata[grepl(gene, pdata$Peptide.Name, fixed=TRUE), "Peptide.Sequence"]
		pepgroup <- pepgroup[order(nchar(sort(pepgroup)))]
		pepm <- matrix(nrow =length(pepgroup), ncol = length(pepgroup))
		rownames(pepm) <- pepgroup
		colnames(pepm) <- pepgroup
		for (n in 1:length(pepgroup)) {	
			pepm[n,] <- vcountPattern(AAString(pepgroup[n]), AAStringSet(pepgroup), with.indels=TRUE, max.mismatch=0, algorithm="auto")		}
		duprows <- which(rowSums(pepm)>1)
		if (length(duprows)==0)  return(NA)
		dupes <- pepm[duprows, ]
		if (length(duprows)==1)	{
			dupelist <- names(dupes[dupes==1])
			return(dupelist)
		} else {
		dupes <- dupes[order(nchar(rownames(dupes))),]
		# Create a list of matching peptides
		dupelist <- as.list(NULL)
		dupelist[[1]] <-(names(dupes[1,grep(1, dupes[1,], fixed=TRUE)]))		
		for (m in 2:length(duprows)) { 
			newvector <- names(dupes[m,grep(1, dupes[m,], fixed=TRUE)])
			if (any(newvector %in% unlist(dupelist))) {
				if (length(dupelist)==1) {dupelist[[1]] <- unique(c(dupelist[[1]], newvector)) } else {
					names(dupelist) <- 1:length(dupelist)
					ld <- ldply(dupelist, paste, collapse=" ")  #  yes
					# use ld$.id
					compep <-newvector[newvector %in% unlist(dupelist)][1]
					# must include: fixed=TRUE
					comvectorid <- ld[grep(compep, ld$V1, , fixed=TRUE), ".id"]
					dupelist[[comvectorid]] <- unique(c(dupelist[[comvectorid]], newvector)) }
				} else dupelist[[m]] <- newvector }}
				} 	 }
			return(dupelist) }
#  This was a difficult function to write!
#
# LINCS verxion; "inclusive" switch included
merge.peptides.lc <- function (pdata, gene, inclusive) 	{	
	if (dim(pdata[grepl(gene, pdata$Gene.Name, fixed=TRUE), ])[1] <= 1) return(NA) else {
   m=NULL
   pdata.new <- pdata
   pepgroup=NULL; pepm=NULL; duprows=NULL; duplist=as.list(NULL)
   dupes=NULL; duprows=NULL; sumdup=NULL; largepep=NULL; smallerpep=NULL
	if (dim(pdata[grepl(gene, pdata$Gene.Name, fixed=TRUE), ])[1] > 1) {
		gene <- paste(gene, " ", sep = "")
		pepgroup <- pdata[grepl(gene, pdata$Peptide.Name, fixed=TRUE), "Peptide.Sequence"]
		if(length(pepgroup)<=1) return(NA)
		pepgroup <- strip.dot(pepgroup)
		pepgroup <- pepgroup[order(nchar(pepgroup))]
		pepm <- matrix(nrow =length(pepgroup), ncol = length(pepgroup))
		rownames(pepm) <- pepgroup
		colnames(pepm) <- pepgroup
		for (n in 1:length(pepgroup)) {	
			pepm[n,] <- vcountPattern(AAString(pepgroup[n]), AAStringSet(pepgroup), with.indels=TRUE, max.mismatch=0, algorithm="auto")		}
		duprows <- which(rowSums(pepm)>1)
		if (length(duprows)==0)  return(NA)
		dupes <- pepm[duprows, ]
		if (length(duprows)==1)	{
		dupelist <- names(dupes[dupes==1])
		if (length(grep(dupelist[1], dupelist, fixed=T)) >2) {
			if (!inclusive) return(NA)
			dupelist.ls <- as.list(NULL)
			for (m in 2:length(dupelist)) { 
				dupelist.ls[[m-1]] <- c(dupelist[1], dupelist[m]) }
			return (dupelist.ls) } else {
			if (length(grep(dupelist[1], dupelist, fixed=T))==1) {
				dupelist <- names(dupes[dupes==1])
				return(dupelist) }}
		} else {
		if (!inclusive) return(NA)	
		dupes <- dupes[order(nchar(rownames(dupes))),]
		# Create a list of matching peptides
		dupelist <- as.list(NULL)
		dupelist[[1]] <-(names(dupes[1,grep(1, dupes[1,], fixed=TRUE)]))		
		for (m in 2:length(duprows)) { 
			newvector <- names(dupes[m,grep(1, dupes[m,], fixed=TRUE)])
			if (any(newvector %in% unlist(dupelist))) {
				if (length(dupelist)==1) {dupelist[[1]] <- unique(c(dupelist[[1]], newvector)) } else {
					names(dupelist) <- 1:length(dupelist)
					ld <- ldply(dupelist, paste, collapse=" ")  #  yes
					# use ld$.id
					compep <-newvector[newvector %in% unlist(dupelist)][1]
					# must include: fixed=TRUE
					comvectorid <- ld[grep(compep, ld$V1, , fixed=TRUE), ".id"]
					dupelist[[comvectorid]] <- unique(c(dupelist[[comvectorid]], newvector)) }
				} else dupelist[[m]] <- newvector }}
				} 	 }
			return(dupelist) }	
#	
mergepeps.multi <- function(df, inclusive)	{
	genes <-  df$Gene.Name
	df$pepseq.nd <- strip.dot(df$Peptide.Sequence)
	dupelist		 <- as.list(NULL)
	dupelist.ls		 <- as.list(NULL)
	duped.peps.list  <- as.list(NULL)
	old.data.df 	<- data.frame(NULL)
	old.data 		 <- as.list(NULL)
	old.data.list 	 <- as.list(NULL)
	new.data 		 <- as.list(NULL)
	new.data.df 	<- data.frame(NULL)
	merged.peps.list <- as.list(NULL)
	for (z in (1:length(genes)))		{
		cat("z = ", z, "\t", genes[z], "\n")
		old.data <- as.list(NULL)
		old.data.df 	<- data.frame(NULL)
		new.data <- as.list(NULL)
		new.data.df 	<- data.frame(NULL)
		dupelist <- merge.peptides.lc(df, genes[z], inclusive)
		dupelist.ls		 <- as.list(NULL)
		duped.peps.list[[z]] <- dupelist
		names(duped.peps.list)[z] <- genes[z]
		if (length(dupelist)==1 & class(dupelist)=="logical") {
			 old.data.list[[z]] <- NA
			 names(old.data.list)[z] <- genes[z] 
			 merged.peps.list[[z]] <- NA	
			 names(merged.peps.list)[z] <- genes[z]
			 }
		if (class(dupelist)=="character")	{
			largepep <- dupelist[which(nchar(dupelist)==max(nchar(dupelist)))]
			smallerpep <- dupelist %w/o% largepep
			old.data.df <- df[df$pepseq.nd %in% dupelist,]
			sumdup <- numcolwise(sum.na.3)(df[df$pepseq.nd %in% dupelist,]) 
			old.data.list[[z]] <- old.data.df
			names(old.data.list)[z] <- genes[z]
			# smaller peptides may merge with different larger ones 
			new.data.df <- cbind(df[df$pepseq.nd ==largepep, 1:6], sumdup)	
			merged.peps.list[[z]] <- new.data.df	
			names(merged.peps.list)[z] <- genes[z]		}
		if	(class(dupelist)=="list")	{
			for (i in 1:length(dupelist))	{
				if (length(dupelist[[i]])==0) next
				largepep <- dupelist[[i]][[which(nchar(dupelist[[i]])==max(nchar(dupelist[[i]])))[1]]]
				smallerpep <- dupelist[[i]] %w/o% largepep
				cat("i = ", i, "\t")
				old.data[[i]] <- df[which(df$pepseq.nd %in% dupelist[[i]]),]
				#old.data[[i]] <- df[match(dupelist[[i]], df$pepseq.nd),]
				sumdup <- numcolwise(sum.na.3)(df[df$pepseq.nd %in% dupelist[[i]],])
			new.data[[i]] <- cbind(df[which(df$pepseq.nd==largepep), 1:6], sumdup)
			} 
			old.data.list[[z]] <- ldply(old.data)
			names(old.data.list)[z] <- genes[z] 
			merged.peps.list[[z]] <- ldply(new.data)	
			names(merged.peps.list)[z] <- genes[z]
			cat("\n") }
	}
	return (list(old.data.list, merged.peps.list))	}
	#

			#
# Map HUGO names from PSP names
#
map.HUGO.name <- function(pspname, species) {
	PSProw <- which((PSP.map$PSP_NAME == as.character(pspname)) & (PSP.map$ORGANISM == as.character(species)))
	if (length(PSProw) >= 1) {
		mapped.name <-as.character(PSP.map[PSProw, "GENE_SYMBOL"])
		return(mapped.name[1])
		} else return("not ID mapped")
	}	
#	This works but numbers end up as characters and factors
zero.to.NA <- function(cytoscape.file) {
	cf <- cytoscape.file
	zer0 <- which(cf==0, arr.ind = TRUE)
	cfNA <- as.matrix(cf)
	cfNA[zer0] <- NA
	cfNA <- data.frame(cfNA)
	return(cfNA)
}
# This one is better, returns data frame from pre-edited file containing data, not factors
#
NA.zero <- function(data.file) {
	cf <- data.file[2:ncol(data.file)]
	zer0 <- which(cf==0, arr.ind = TRUE)
	cfNA <- as.matrix(cf)   # makes "numbers into characters" unless Gene.Name col is removed
	cfNA <- replace (cfNA, zer0, NA)
	cf <- cbind(data.file[,1], data.frame(cfNA))
 	names(cf)[1] <- "Gene.Name"
	return(cf)
}
	# Convert zero to NA preserving the first two columns:
NA.zero.2 <- function(data.file) {
	cf <- data.file[3:ncol(data.file)]
	zer0 <- which(cf==0, arr.ind = TRUE)
	cfNA <- as.matrix(cf)   # makes "numbers into characters" unless Gene.Name col is removed
	cfNA <- replace (cfNA, zer0, NA)
	cf <- cbind(data.file[,1:2], data.frame(cfNA))
 	names(cf)[1:2] <- c("Peptide",         "Inhibitory.Site")
	return(cf)
}
# But for correlations, need a matrix
cor.NA <- function(cytoscape.file) {
	cf <- cytoscape.file[2:ncol(cytoscape.file)]
	zer0 <- which(cf==0, arr.ind = TRUE)
	cfNA <- as.matrix(cf)   # makes "numbers into characters" unless Gene.Name col is removed
	cfNA <- replace (cfNA, zer0, NA)
	cf.cor <- cor(cfNA, use = "pairwise.complete.obs", method = "pearson")
	# cr.cor <- cor(cfNA, use = "na.or.complete", method = "pearson") # need to understand difference here
	return(cf.cor)
}
# 
zero.NA <- function(cytoscape.file) {
	cf <- cytoscape.file[2:ncol(cytoscape.file)]
	cf[is.na(cf)] <- 0
	cf <- cbind(cytoscape.file$First.Gene.Name, cf)
	names(cf)[1] <- "First.Gene.Name"
	return(cf)
}
#		
zero.NA.1 <- function(cytoscape.file) {
			cf <- cytoscape.file[2:ncol(cytoscape.file)]
			cf[is.na(cf)] <- 0
			cf <- cbind(cytoscape.file$'Gene.Name', cf)
			names(cf)[1] <- "Gene.Name"
			return(cf)
			}
#
	all0NA <- function(df) {
		df.na <- apply(df, 1, function(x) all(x==0))
		df[df.na,] <- NA
		return(df)
	}
#
prep.tbl <- function (cytoscape.file) {
	#	Prune data file for distance matrix / omit Total
	print(names(cytoscape.file))
	cat("\n","\t", "Set data columns for correlation calucations. ","\n","\t","First column number? ")
	firstcolnumber <- as.numeric(readLines(con = stdin(), n = 1))
	cat("\n","\t","Last column number? ")
	lastcolnumber <- as.numeric(readLines(con = stdin(), n = 1))
	datacols=c(firstcolnumber:lastcolnumber)
	datacol.names <- names(cytoscape.file[datacols])
	cat("\t", "data columns", "\n")
	print(datacol.names)
	 cydata <-cytoscape.file[, c(datacols)]
	 cytbl <- NA.zero(cydata)
	    tbl=data.matrix(cytbl)
		rownames (tbl) = cytoscape.file$First.Gene.Name
	return(tbl)
	}
dist.Euclid <- function (tbl) {
	# Compute Distance Matrix
		dm = as.matrix (dist (tbl), method = "euclidean")  # default method
	# set NA to two orders of magnitude higher than max distance
		dmz <- dm
		dmz[is.na(dmz)] <- 100*max(dmz, na.rm=T)
		dmz.ord <- cmdscale(dmz, k=3) 
		dev.new()
		plot(dmz.ord,  main="Euclidian Distance", xlab="")
		return(dmz.ord)	
		}
	#
dist.Pearson <- function (tbl) {
	#	Use 1 - abs(Pearson Correlation) as distance	
		cf.cor <- cor(t(tbl), use = "pairwise.complete.obs", method = "pearson")
		dissimilarity <- 1 - abs(cf.cor)
		distance <- as.dist(dissimilarity)
		dpz <- distance
		# set NA to two orders of magnitude higher than max distance
		dpz[is.na(dpz)] <- 100*max(dpz, na.rm=T)
		dpz.ord <- cmdscale(dpz, k=3) 
		dev.new()
		plot(dpz.ord,  main="Dissimilarity = 1 - Abs(Pearson Correlation)", xlab="")
		return(dpz.ord)
		}   
#
dist.Spearman <- function (tbl) {
	#	Use 1 - abs(Spearman Correlation) as distance	
		cf.cor <- cor(t(tbl), use = "pairwise.complete.obs", method = "spearman")
		dissimilarity <- 1 - abs(cf.cor)
		distance <- as.dist(dissimilarity)
		dpz <- distance
		# set NA to two orders of magnitude higher than max distance
		dpz[is.na(dpz)] <- 100*max(dpz, na.rm=T)
		dpz.ord <- cmdscale(dpz, k=3) 
		dev.new()
		plot(dpz.ord,  main="Dissimilarity = 1 - Abs(Spearman Correlation)", xlab="")
		return(dpz.ord)
		} 
#
	getpdbid <- function(pdbfile)	{
		sel  <- read.pdb(pdbfile)
		sel.df  <- data.frame(sel$atom)
		resno <- sel.df$resno
		sel.id  <- key[key$g.number %in% resno, ]
		return (sel.id$Gene.Name)
		}
	#
	"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
	without <- function(x, y) x[!x %in% y] #--  x without y
	nmissing <- function(x) sum(is.na(x))
	filled <- function (x) {length(x) - nmissing(x)}
	mean.na <- function(x) mean(x, na.rm=TRUE)
	max.na <- function(x) max(x, na.rm=TRUE)
	min.na <- function(x) min(x, na.rm=TRUE)
	 sd.na <- function(x) sd(x, na.rm=TRUE)
	strReverse <- function(x)  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
	sum.na <- function(x) {
		z <- sum(x, na.rm=TRUE)
		if (z==0) return(NA) else(return(z))	 }
	sum.na.2 <- function(x) {
		z <- sum(x, na.rm=TRUE)
		if (all(is.na(x))) return(NA) else(return(z))	 }		
	sum.na.3 <- function(x) {
		if (all(is.na(x))) return(NA) else {
		z <- sum(x[!duplicated(x)], na.rm=TRUE)
		(return(z))	 }}	
		#
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
#
#  function to convert rat gene names to human, preserving numbers
# use: sapply(genes, tohumanname)
tohumanname <- function(string) {
	str.v <- unlist(str_split(string, ""))
	str.u <- toupper(str.v)
	str.c <- str_c(str.u, collapse="")
	return(str.c)
}	
# function to convert human all caps to rat title case
toratname <- function(string) {
	string1 <- tolower(string)
	string2 <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2" ,string1, perl=TRUE)
	return(string2)
}	
#		 
clust.eval.1 <- function(clusterlist) {
	evaluation <- data.frame(0)
	names(evaluation)[1] <- "Group"
	for (i in 1:length(clusterlist)) {
		evaluation[i,1] <- i
		at = data.frame(tbl.sc[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ])
		if (length(clusterlist[[i]]$Gene.Name)>1) {
		atpeps = data.frame(tbl.sc[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ])
		acol <- names(which(colSums(at, na.rm=TRUE) > 0))
		if (length(acol)>1) {
		at <- at[,which(colSums(at, na.rm=TRUE) > 0)]
				cat("at", i, "\n")
		atpeps <- atpeps[,which(colSums(atpeps, na.rm=TRUE) > 0)]
		atsinglepeps <- atpeps[which(rowSums(atpeps, na.rm=TRUE) == 1),]
		cat("Group", i, "\n")
		totalpeps <- sum(atpeps, na.rm=TRUE)
		# Count number of missing values
		# Apply to every column in a data frame
		# numcolwise(nmissing)(at)
		# measure the fraction of NAs in the data
		fract.na <-  100*(sum(numcolwise(nmissing)(at)) / (dim(at)[1]*dim(at)[2]))
		fract.singlepeps <- 100*(nrow(atsinglepeps) / dim(at)[1]) 
		singlegenesamples <- sum((nrow(at) - numcolwise(nmissing)(at)) == 1)
		fract.singlegenesamples <- 100*(singlegenesamples/dim(at)[2])
		#
		#	A measure of incompleteness of each set:
			evaluation$percent.NA[i] <- fract.na
			evaluation$percent.singlepeps[i] <- fract.singlepeps
			evaluation$percent.singlesamples[i] <- fract.singlegenesamples
			evaluation$total.peptides[i] <- totalpeps
			evaluation$no.samples[i] <- length(acol)
			#evaluation$Index[i] <- (evaluation$total.peptides[i]/evaluation$no.samples[i])/evaluation$percent.NA[i]
			} else {
			evaluation$percent.NA[i] = 0; evaluation$percent.singlepeps[i] = 100
			evaluation$percent.singlesamples[i] <- 0
			evaluation$total.peptides[i] <- sum(tbl[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ], na.rm=TRUE)
			evaluation$no.samples[i] <- length(acol)
			#evaluation$Index[i] <- -1
			} # end if single column (sample).
			} else {
			evaluation$percent.NA[i] = 0; evaluation$percent.singlepeps[i] = 0
			evaluation$percent.singlesamples[i] <- 100
			evaluation$total.peptides[i] <- sum(tbl[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ], na.rm=TRUE)
 			evaluation$no.samples[i] <- length(which(is.na(at)==FALSE))
 			# evaluation$Index[i] <- -1 
 			} # end if single row (gene). 
			evaluation$no.genes[i] <- length(clusterlist[[i]]$Gene.Name)			
			} 
			#
	#  Total peptides scaled to percent NA = saturation after subtracting single peptides
		realpeps <- evaluation$total.peptides - (evaluation$total.peptides * evaluation$percent.singlepeps/100)
		saturation <- realpeps - (realpeps * evaluation$percent.NA/100)
	#	Percent single samples from is a measure of samples that did not contribute to the correlation   
		realsamples <- evaluation$no.samples- (evaluation$no.samples * evaluation$percent.singlesamples/100)
		evaluation$Index  <- (saturation * realsamples) 
		eval.sort <- evaluation[order(-evaluation$Index, evaluation$percent.singlepeps, evaluation$percent.NA), c("Group", "no.genes", "Index", "total.peptides", "percent.singlepeps", "no.samples", "percent.singlesamples", "percent.NA")]
		 return(eval.sort)	
	}	# end clust.eval
#
#
# for intensity and including negative values: 
nbclust.eval <- function(clusterlist, tbl.sc) {
	evaluation <- data.frame(0)
	names(evaluation)[1] <- "Group"
	key  <- data.frame(1:length(rownames(tbl.sc)))
	key$Gene.Name <- rownames(tbl.sc)
	for (i in 1:length(clusterlist)) {
		cat("Starting Group", i, "\n")
		evaluation[i,1] <- i
		#
		evaluation$no.genes[i] <- length(clusterlist[[i]]$Gene.Name)
		if(length(clusterlist[[i]]$Gene.Name) == 1) { 
			at = data.frame(tbl.sc[clusterlist[[i]]$Gene.Name, ])
			 } else {
		at = data.frame(tbl.sc[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ]) }
		# get rid of ratios for evaluation calculations and take absolute value
		if(any (grepl("to", names(at)))) at = abs(at [,-grep("to", names(at))])
		# previous use: at <- at[-which(apply(at, 1, filled) == 0),]
		# better: at[, which(numcolwise(filled)(at) != 0)]
		# names() doesn't work with single column
		if (length(which(numcolwise(filled)(at) != 0)) > 1) {
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])  
		evaluation$no.samples[i] <- length(acol)
		at <- at[, acol] } else evaluation$no.samples[i] <- 1
		evaluation$total.signal[i] <- sum(abs(at), na.rm=TRUE)
		if (length(which(numcolwise(filled)(at) != 0)) == 1 || length(clusterlist[[i]]$Gene.Name) == 1) {
					evaluation$culled.by.slope[i] <- length(clusterlist[[i]]$Gene.Name) 
					evaluation$percent.NA[i] <- 0
					evaluation$percent.singlesamplegenes[i] <- 100
					evaluation$percent.singlegenesamples[i] <- 100
				} else	{
				evaluation$percent.NA[i] <-  100*(sum(numcolwise(nmissing)(at)) / (dim(at)[1]*dim(at)[2]))
				#singlesamplegenes <- at[, which(numcolwise(filled)(at) == 1 )]
				singlesamplegenes <- at[which(apply(at, 1, filled) == 1),]
				evaluation$percent.singlesamplegenes[i] <- 100*(nrow(singlesamplegenes) / dim(at)[1]) 
				singlegenesamples <- sum(numcolwise(filled)(at) == 1)
				evaluation$percent.singlegenesamples[i] <- 100*(singlegenesamples/dim(at)[2])
				cluster.mo <- at[order(-as.vector(colwise(sum.na)(data.frame(t(abs(at)))))), order(-as.vector(numcolwise(sum.na)(data.frame(abs(at)))))]
				slope <- apply(cluster.mo, 1, get.slope.a)
				badslope <- c(names(which(is.na(slope))), names(which(slope > 0)))
				evaluation$culled.by.slope[i] <- length(badslope)
				#
				cat("\n", length(badslope), "genes culled by slope", "\n")
			}		}	 
	#  Total signal scaled to percent NA = intensity
		cleargenes <- evaluation$no.genes - evaluation$culled.by.slope # may be 0
		realsamples <- evaluation$no.samples - (evaluation$no.samples * evaluation$percent.singlegenesamples/100) # may be 0
		intensity <- evaluation$total.signal - (evaluation$total.signal * evaluation$percent.NA/100)
		# calibrate intensity according to real samples and clear genes
			# - goal is to reward tightly focussed groups, but not too small or too big
		evaluation$Index  <- intensity * (1 + realsamples) * (1 + cleargenes) / (1 + evaluation$percent.NA)
		eval.sort <- evaluation[order(-evaluation$Index, evaluation$percent.NA), c("Group", "no.genes",  "culled.by.slope", "percent.singlesamplegenes","no.samples", "percent.singlegenesamples", "total.signal", "percent.NA", "Index" )] 
		 return(eval.sort)	
	}
	# END nbclust.eval	 
#		 
# modify to focus on the density of data per gene/site
lincsclust.eval <- function(clusterlist, tbl.sc) {
	evaluation <- data.frame(0)
	names(evaluation)[1] <- "Group"
	key  <- data.frame(1:length(rownames(tbl.sc)))
	key$Gene.Name <- rownames(tbl.sc)
	for (i in 1:length(clusterlist)) {
		cat("Starting Group", i, "\n")
		evaluation[i,1] <- i
		#
		evaluation$no.genes[i] <- length(clusterlist[[i]]$Gene.Name)
		if(length(clusterlist[[i]]$Gene.Name) == 1) { 
			at = data.frame(tbl.sc[clusterlist[[i]]$Gene.Name, ])
			 } else {
		at = data.frame(tbl.sc[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ]) }
		# get rid of ratios for evaluation calculations and take absolute value
		if(any (grepl("to", names(at)))) at = abs(at [,-grep("to", names(at))])
		# previous use: at <- at[-which(apply(at, 1, filled) == 0),]
		# better: at[, which(numcolwise(filled)(at) != 0)]
		# names() doesn't work with single column
		if (length(which(numcolwise(filled)(at) != 0)) > 1) {
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])  
		evaluation$no.samples[i] <- length(acol)
		at <- at[, acol] } else evaluation$no.samples[i] <- 1
		evaluation$total.signal[i] <- sum(abs(at), na.rm=TRUE)
		if (length(which(numcolwise(filled)(at) != 0)) == 1 || length(clusterlist[[i]]$Gene.Name) == 1) {
					evaluation$culled.by.slope[i] <- length(clusterlist[[i]]$Gene.Name) 
					evaluation$percent.NA[i] <- 0
					evaluation$percent.singlesamplegenes[i] <- 100
					evaluation$percent.singlegenesamples[i] <- 100
				} else	{
				evaluation$percent.NA[i] <-  100*(sum(numcolwise(nmissing)(at)) / (dim(at)[1]*dim(at)[2]))
				#singlesamplegenes <- at[, which(numcolwise(filled)(at) == 1 )]
				singlesamplegenes <- at[which(apply(at, 1, filled) == 1),]
				evaluation$percent.singlesamplegenes[i] <- 100*(nrow(singlesamplegenes) / dim(at)[1]) 
				singlegenesamples <- sum(numcolwise(filled)(at) == 1)
				evaluation$percent.singlegenesamples[i] <- 100*(singlegenesamples/dim(at)[2])
				cluster.mo <- at[order(-as.vector(colwise(sum.na)(data.frame(t(abs(at)))))), order(-as.vector(numcolwise(sum.na)(data.frame(abs(at)))))]
				slope <- apply(cluster.mo, 1, get.slope.a)
				badslope <- c(names(which(is.na(slope))), names(which(slope > 0)))
				evaluation$culled.by.slope[i] <- length(badslope)
				#
				cat("\n", length(badslope), "genes culled by slope", "\n")
			}		}	 
	#  Total signal scaled to percent NA = intensity
		cleargenes <- evaluation$no.genes - evaluation$culled.by.slope # may be 0
		realsamples <- evaluation$no.samples - (evaluation$no.samples * evaluation$percent.singlegenesamples/100) # may be 0
		intensity <- evaluation$total.signal - (evaluation$total.signal * evaluation$percent.NA/100)
		# calibrate intensity according to real samples and clear genes
			# - goal is to reward a high density of appropriate data
		evaluation$intensity <- intensity
		evaluation$Index  <- ((1 + realsamples) * (1 + cleargenes) / (1 + evaluation$percent.NA))/evaluation$no.genes
		eval.sort <- evaluation[order(-evaluation$Index, evaluation$percent.NA), c("Group", "no.genes",  "culled.by.slope", "percent.singlesamplegenes","no.samples", "percent.singlegenesamples", "total.signal", "percent.NA", "intensity", "Index" )] 
		 return(eval.sort)	
	}
# end lincsclust.eval
#
clust.eval <- function(clusterlist, tbl.sc) {
	evaluation <- data.frame(0)
	names(evaluation)[1] <- "Group"
	key  <- data.frame(1:length(rownames(tbl.sc)))
	key$Gene.Name <- rownames(tbl.sc)
	for (i in 1:length(clusterlist)) {
		cat("Starting Group", i, "\n")
		evaluation[i,1] <- i
		#
		evaluation$no.genes[i] <- length(clusterlist[[i]]$Gene.Name)
		#
		at = data.frame(tbl.sc[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ])
		# at <- at[-which(apply(at, 1, filled) == 0),]
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])
		evaluation$no.samples[i] <- length(acol)
		at <- at[, acol]
		evaluation$total.signal[i] <- sum(at, na.rm=TRUE)
		if ((length (acol) == 1) || (length(clusterlist[[i]]$Gene.Name) == 1)) {
					evaluation$culled.by.slope[i] <- length(clusterlist[[i]]$Gene.Name) 
					evaluation$percent.NA[i] <- 0
					evaluation$percent.singlesamplegenes[i] <- 100
					evaluation$percent.singlegenesamples[i] <- 100
				} else	{
				evaluation$percent.NA[i] <-  100*(sum(numcolwise(nmissing)(at)) / (dim(at)[1]*dim(at)[2]))
				singlesamplegenes <- at[which(apply(at, 1, filled) == 1),]
				evaluation$percent.singlesamplegenes[i] <- 100*(nrow(singlesamplegenes) / dim(at)[1]) 
				singlegenesamples <- sum(numcolwise(filled)(at) == 1)
				evaluation$percent.singlegenesamples[i] <- 100*(singlegenesamples/dim(at)[2])
				cluster.mo <- at[order(-as.vector(colwise(sum.na)(data.frame(t(at))))), order(-as.vector(numcolwise(sum.na)(data.frame(at))))]
				slope <- apply(cluster.mo, 1, get.slope.a)
				badslope <- c(names(which(is.na(slope))), names(which(slope > 0)))
				evaluation$culled.by.slope[i] <- length(badslope)
				#
				cat("\n", length(badslope), "genes culled by slope", "\n")
			}		}	 
	#  Total signal scaled to percent NA = intensity
		cleargenes <- evaluation$no.genes - evaluation$culled.by.slope # may be 0
		realsamples <- evaluation$no.samples - (evaluation$no.samples * evaluation$percent.singlegenesamples/100) # may be 0
		intensity <- evaluation$total.signal - (evaluation$total.signal * evaluation$percent.NA/100)
		# calibrate intensity according to real samples and clear genes
			# - goal is to reward tightly focussed groups, but not too small or too big
		evaluation$Index  <- intensity * (1 + realsamples) * (1 + cleargenes) / (1 + evaluation$percent.NA)
		eval.sort <- evaluation[order(-evaluation$Index, evaluation$percent.NA), c("Group", "no.genes",  "culled.by.slope", "percent.singlesamplegenes","no.samples", "percent.singlegenesamples", "total.signal", "percent.NA", "Index" )] 
		 return(eval.sort)	
	}
# end clust.eval
#
singleclust.eval <-  function(cluster.df, tbl.sc) {
	evaluation <- data.frame(0)
	names(evaluation)[1] <- "Group"
	key  <- data.frame(1:length(rownames(tbl.sc)))
	key$Gene.Name <- rownames(tbl.sc)
	# for (i in 1:length(clusterlist)) {
	#	cat("Starting Group", i, "\n")
		i=1
		evaluation[i,1] <- i
		#
		evaluation$no.genes[i] <- length(cluster.df$Gene.Name)
		#
		at = data.frame(tbl.sc[key$Gene.Name %in% cluster.df$Gene.Name, ])
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])
		evaluation$no.samples[i] <- length(acol)
		at <- at[, acol]
		evaluation$total.signal[i] <- sum(at, na.rm=TRUE)
		if ((length (acol) == 1) || (length(cluster.df$Gene.Name) == 1)) {
					evaluation$culled.by.slope[i] <- length(clusterlist[[i]]$Gene.Name) 
					evaluation$percent.NA[i] <- 0
					evaluation$percent.singlesamplegenes[i] <- 100
					evaluation$percent.singlegenesamples[i] <- 100
				} else	{
				evaluation$percent.NA[i] <-  100*(sum(numcolwise(nmissing)(at)) / (dim(at)[1]*dim(at)[2]))
				singlesamplegenes <- at[which(apply(at, 1, filled) == 1),]
				evaluation$percent.singlesamplegenes[i] <- 100*(nrow(singlesamplegenes) / dim(at)[1]) 
				singlegenesamples <- sum(numcolwise(filled)(at) == 1)
				evaluation$percent.singlegenesamples[i] <- 100*(singlegenesamples/dim(at)[2])
				cluster.mo <- at[order(-as.vector(colwise(sum.na)(data.frame(t(at))))), order(-as.vector(numcolwise(sum.na)(data.frame(at))))]
				slope <- apply(cluster.mo, 1, get.slope.a)
				badslope <- c(names(which(is.na(slope))), names(which(slope > 0)))
				evaluation$culled.by.slope[i] <- length(badslope)
				#
				cat("\n", length(badslope), "genes culled by slope", "\n")
			}			 
	#  Total signal scaled to percent NA = intensity
		cleargenes <- evaluation$no.genes - evaluation$culled.by.slope # may be 0
		realsamples <- evaluation$no.samples - (evaluation$no.samples * evaluation$percent.singlegenesamples/100) # may be 0
		intensity <- evaluation$total.signal - (evaluation$total.signal * evaluation$percent.NA/100)
		# calibrate intensity according to real samples and clear genes
			# - goal is to reward tightly focussed groups, but not too small or too big
		evaluation$Index  <- intensity * (1 + realsamples) * (1 + cleargenes) / (1 + evaluation$percent.NA)
		eval.sort <- evaluation[order(-evaluation$Index, evaluation$percent.NA), c("Group", "no.genes",  "culled.by.slope", "percent.singlesamplegenes","no.samples", "percent.singlegenesamples", "total.signal", "percent.NA", "Index" )] 
		 return(eval.sort)	
	}
# end singleclust.eval
#
clust.data <- function(clusterlist, tbl) {
	clust.list <- as.list(NULL)
	key  <- data.frame(1:length(rownames(tbl)))
	key$Gene.Name <- rownames(tbl)
	for (i in 1:length(clusterlist)) {
		at = data.frame(tbl[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ])
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])
		if(length(acol)  == 1) {
			ats <- data.frame(cbind (rownames(at), as.numeric(at[, acol])))
			names(ats) <- c("Gene.Name", acol)
			}
		if(length(acol) >= 2) {
			ats <- cbind(rownames(at), at[, acol])
			names(ats)[1] <- "Gene.Name" }
		clust.list[[i]] <- ats
		}
		return (clust.list)	}
		#
clust.data.sc <- function(clusterlist) {
	clust.list <- as.list(NULL)
	for (i in 1:length(clusterlist)) {
		at = data.frame(tbl.sc[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ])
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])
		if(length(acol)  == 1) {
			ats <- data.frame(cbind (rownames(at), as.numeric(at[, acol])))
			names(ats) <- c("Gene.Name", acol)
			}
		if(length(acol) >= 2) {
			ats <- cbind(rownames(at), at[, acol])
			names(ats)[1] <- "Gene.Name" }
		clust.list[[i]] <- ats
		}
		return (clust.list)	}	
		#
clust.data.from.vector <- function(vector, tbl) {
	key  <- data.frame(1:length(rownames(tbl)))
	key$Gene.Name <- rownames(tbl)
		at = data.frame(tbl[key$Gene.Name %in% vector, ])
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])
		if(length(acol)  == 1) {
			ats <- data.frame(cbind (rownames(at), as.numeric(at[, acol])))
			names(ats) <- c("Gene.Name", acol)
			}
		if(length(acol) >= 2) {
			ats <- cbind(rownames(at), at[, acol])
			names(ats)[1] <- "Gene.Name" }
		clust.data <- ats
		return (clust.data)	}
	# end clust.data.from.vector
# requires data table with rownames 
clust.data.from.vec <- function(vec, tbl) {
	if(class(vec)=="list") {vec <- unlist(vec)}
	at <- tbl[vec,]
		acol <- names(at[,which(numcolwise(filled)(at) != 0)])
		if(length(acol)  == 1) {
			ats <- data.frame(cbind (rownames(at), as.numeric(at[, acol])))
			names(ats) <- c("Gene.Name", acol)
			}
		if(length(acol) >= 2) {
			ats <- cbind(rownames(at), at[, acol])
			names(ats)[1] <- "Gene.Name" }
		clust.data <- ats
		return (clust.data)	}
	#
nbclust.data <- function(clusterlist, tbl) {
	clust.list <- as.list(NULL)
	key  <- data.frame(1:length(rownames(tbl)))
	key$Gene.Name <- rownames(tbl)
	for (i in 1:length(clusterlist)) {
		if(length(clusterlist[[i]]$Gene.Name) == 1) { 
			at = data.frame(tbl[clusterlist[[i]]$Gene.Name, ]) 
			} else {
		at = data.frame(tbl[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ]) }
		# get rid of ratios for evaluation calculations and take absolute value
		if(any (grepl("to", names(at)))) at = abs(at [,-grep("to", names(at))])
		# names() doesn't work with single column
		if (length(which(numcolwise(filled)(at) != 0)) > 1) {
			acol <- names(at[,which(numcolwise(filled)(at) != 0)])  
			at <- at[, acol] 
			ats <- cbind(rownames(at), at[, acol]) } else {
			at <- data.frame(at[,-which(numcolwise(filled)(at) == 0)])
			acol <- names(which(colSums(at, na.rm=T)>0))
			ats <- data.frame(cbind (clusterlist[[i]]$Gene.Name, as.numeric(at[, acol])))
			rownames(ats) <- clusterlist[[i]]$Gene.Name }
		names(ats) <- c("Gene.Name", acol) 
		clust.list[[i]] <- ats
		}
		return (clust.list)	}
	# modify function to make clusters from data
	# 	to deal with single gene/student clusters
		#
biobclust.data <- function(clusterlist, tbl) {
	clust.list <- as.list(NULL)
	key  <- data.frame(1:length(rownames(tbl)))
	key$Gene.Name <- rownames(tbl)
	for (i in 1:length(clusterlist)) {
		at = data.frame(tbl[key$Gene.Name %in% clusterlist[[i]]$Gene.Name, ])
		if (dim(at)[2] == 1) {
			Gene.Name <- clusterlist[[i]]$Gene.Name
			acol = rownames(at)
			ats <- cbind(data.frame(Gene.Name), t(at))
			ats <- ats[,!is.na(ats)]
			rownames(ats)=NULL } else {
		acol <- names(at[,which(numcolwise(filled)(at) != 0)]) 
		if(length(acol)  == 1) {
			ats <- data.frame(cbind (rownames(at), as.numeric(at[, acol])))
			names(ats) <- c("Gene.Name", acol)
			}
		if(length(acol) >= 2) {
			ats <- cbind(rownames(at), at[, acol])
			names(ats)[1] <- "Gene.Name" } }
		clust.list[[i]] <- ats
		}
		return (clust.list)	}	
		# end biobclust.data
	#
# function to subset groups of peptides (or genes) into separate groups that positively correlate with each other
#	
get.corr.groups <- function (peptides, cf.corfile, cutoff) {
		corgroups.list=as.list(NULL)
		# create correlation matrix containing just peptides
		subcor <- cf.corfile[rownames(cf.corfile) %in% peptides, colnames(cf.corfile) %in% peptides]
		subcor.list <- apply(subcor, 2, function (x) which (x >= cutoff))
		for (i in 1:length(subcor.list)) {
			set <- subcor.list[names(subcor.list) %in% intersect(names(subcor.list[[i]]), names(subcor.list))]
			if (any(names(set) %in% unlist(corgroups.list))) next 
			# get all the positively-correlating peptides
			pc <- unique(as.character(c(unlist(llply(set, names)), names(set))))
			corgroups.list[[i]] <- pc			
			}
	# corgroups.list <- corgroups.list[!sapply(corgroups.list, is.null)] # next line gets these
	corgroups.list <- corgroups.list[which(sapply(corgroups.list, length)>0)]
	return(corgroups.list)		
}
#  helper functions to find a value in a correlation matrix
lookup <- function (Gene.1, Gene.2, corrtable=nbpeps.cor) {
 	corrmatrix <- corrtable[as.character(Gene.1), as.character(Gene.2)]
 	return(corrmatrix)  # returns matrix or value
 }
 lookupvec <- function (vector, corrtable=nbpeps.cor) {
 	Gene.1 <- vector[1]
 	Gene.2 <- vector[2]
 	corrmatrix <- corrtable[as.character(Gene.1), as.character(Gene.2)]
 	return(corrmatrix)  # returns matrix or value
 }
# this on returns a vector: 
 lookitup <- function (Gene.1, Gene.2, corrtable=nbpeps.cor) {
 	rowind <- match(as.character(Gene.1), rownames(corrtable))
 	colind <- match(as.character(Gene.2), colnames(corrtable))
 	corrmatrix <- corrtable[rowind, colind]
 	return(diag(corrmatrix))  
 }
# simple version for mapply
#  x and y are row and col names, in quotes
liu <- function (x, y) as.vector(diag(corrtable[x, y]))
#

#  Function to retrieve values from a key in which the first column is the name and the second is the value
#  use: sapply(clusters.cf$Gene.Name, retrieve, a.df=c.g.count)
retrieve <- 	function(x, a.df) {
	vx <- a.df[match(x, a.df[,1]), 2]
	return (vx)
	}
	#

##  This function takes the argument of a cluster data index, e.g., span3.clusters[26], from clust.data
# 		Graphs heat map and returns ordered data frame for saving in file
#			requires library ("gplots")
graph.cluster <- function(cluster.data.index) 	{
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		cluster.dftemp <- data.frame(cluster.mo)
		Gene.Name <- as.vector(rownames(cluster.mo))
		cluster.dfo <- base::cbind(Gene.Name, cluster.dftemp)
	cluster.mo[is.na(cluster.mo)] <- 0		
	rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="Lab", interpolate = "linear")
		# royal blue to yellow
		palette(c("black", rbyheatcolors(max(cluster.mo)/min(cluster.m, na.rm=TRUE))))			
	dev.new() # dev.new(width=2+0.1*ncol(cluster.mo), height=2.5+0.125*nrow(cluster.mo)) # can resize manually
 	heatmap.2(cluster.mo, dendrogram="none", trace="none", labRow=cluster.dfo$Gene.Name, labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(5.2, 6)) 
	return (cluster.dfo) }
	}
	# end graph.cluster
 graph.cluster.2 <- function(cluster.data.index) 	{
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(abs(cluster.df[,2:ncol(cluster.df)]))
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		cluster.dftemp <- data.frame(cluster.mo)
		Gene.Name <- as.vector(rownames(cluster.mo))
		cluster.dfo <- base::cbind(Gene.Name, cluster.dftemp)
	cluster.mo[is.na(cluster.mo)] <- 0		
	rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="Lab", interpolate = "linear")
		# royal blue to yellow
		palette(c("black", rbyheatcolors(255)))
	dev.new() # dev.new(width=2+0.1*ncol(cluster.mo), height=2.5+0.125*nrow(cluster.mo)) # can resize manually
 	heatmap.2(cluster.mo, dendrogram="none", trace="none", labRow=cluster.dfo$Gene.Name, labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(5.2, 6)) 
	return (cluster.dfo) }
	}
	#
# third version scales so that minimally expressed genes show up	
graph.cluster.3 <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(abs(cluster.df[,2:ncol(cluster.df)]))
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		cluster.dftemp <- data.frame(cluster.mo)
		Gene.Name <- as.vector(rownames(cluster.mo))
		cluster.dfo <- base::cbind(Gene.Name, cluster.dftemp)
	scalefactor = max.na(cluster.m)/min(cluster.m, na.rm=T) 
	cluster.scale <- scalefactor*(cluster.mo) 
	cluster.scale.2 <- cluster.scale + max.na(cluster.scale)/500
	 cluster.scale.2[is.na(cluster.scale.2)] <- 0
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="Lab", interpolate = "linear")
		# royal blue to yellow
		palette(c("black", rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.scale.2, dendrogram="none", trace="none", labRow=cluster.dfo$Gene.Name, labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(10, 6)) 
	return (cluster.dfo) }
	}
# End graph.cluster.3
graph.cluster.3b <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(abs(cluster.df[,2:ncol(cluster.df)]))
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		cluster.dftemp <- data.frame(cluster.mo)
		Gene.Name <- as.vector(rownames(cluster.mo))
		cluster.dfo <- base::cbind(Gene.Name, cluster.dftemp)
	scalefactor = max.na(cluster.m)/min(cluster.m, na.rm=T) 
	cluster.scale <- scalefactor*(cluster.mo) 
	cluster.scale.2 <- cluster.scale + max.na(cluster.scale)/500
	 cluster.scale.2[is.na(cluster.scale.2)] <- 0
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c("black", rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.scale.2, dendrogram="none", trace="none", labRow=cluster.dfo$Gene.Name, labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(12, 7)) 
	return (cluster.dfo) }
	}
graph.cluster.3bwide <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(abs(cluster.df[,2:ncol(cluster.df)]))
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		cluster.dftemp <- data.frame(cluster.mo)
		Gene.Name <- as.vector(rownames(cluster.mo))
		cluster.dfo <- base::cbind(Gene.Name, cluster.dftemp)
	scalefactor = max.na(cluster.m)/min(cluster.m, na.rm=T) 
	cluster.scale <- scalefactor*(cluster.mo) 
	cluster.scale.2 <- cluster.scale + max.na(cluster.scale)/500
	 cluster.scale.2[is.na(cluster.scale.2)] <- 0
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c("black", rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.scale.2, dendrogram="none", trace="none", labRow=cluster.dfo$Gene.Name, labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(12, 20)) 
	return (cluster.dfo) }
	}
	#
graph.cluster.3c <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(abs(cluster.df[,2:ncol(cluster.df)]))
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		cluster.dftemp <- data.frame(cluster.mo)
		Gene.Name <- as.vector(rownames(cluster.mo))
		cluster.dfo <- base::cbind(Gene.Name, cluster.dftemp)
	scalefactor = max.na(cluster.m)/min(cluster.m, na.rm=T) 
	cluster.scale <- scalefactor*(cluster.mo) 
	cluster.scale.2 <- cluster.scale + max.na(cluster.scale)/500
	 cluster.scale.2[is.na(cluster.scale.2)] <- 0
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c("black", rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.scale.2, dendrogram="row", trace="none", labRow=cluster.dfo$Gene.Name, labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=TRUE, keysize=0.85, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(12, 7)) 
	return (cluster.dfo) }
	}
	#
make.clusterlist <- function(tsnedata, toolong, tbl.sc)	{
		tsne.span2 <- spantree(dist(tsnedata), toolong=toolong)
		tsnedata.disc2 <-  distconnected(dist(tsnedata), toolong = toolong, trace = TRUE)  # test
		cat ("threshold dissimilarity", toolong, "\n", max(tsnedata.disc2), " groups","\n")
		ordiplot(tsnedata)
				#lines(tsne.span2, tsnedata)
		ordihull(tsnedata, tsnedata.disc2, col="red", lwd=2)	
		# Find groups
		tsnedata.span2.df <- data.frame(rownames(tbl.sc))
		names(tsnedata.span2.df) <- "Gene.Name"
		tsnedata.span2.df$group <- tsnedata.disc2
		tsnedata.span2.list <- dlply(tsnedata.span2.df, .(group))  # GROUP LIST  !
		return(tsnedata.span2.list)	
	}
	# end make.clusterlist	
#
#  for use with data frame row
	get.slope<- function(cluster.row) {
		linmod <- lm(1:(ncol(cluster.row)-1)~as.numeric(cluster.row[1,2:ncol(cluster.row)]))
		slope <-  coefficients(linmod)[2]
		return(slope)
	}
	#
# different versions for sapply
	get.slope.a <- function(cluster.row) {
		lrow <-  length(cluster.row)
		if(filled(cluster.row)==0) {slope = 1000} else {
		linmod <- lm(1:lrow~as.numeric(cluster.row))
		slope <-  coefficients(linmod)[2] }
		return(slope)
	}
	get.slope.b <- function(cluster.row) {
			lrow <-  length(cluster.row)
			if(filled(cluster.row)==0) {slope = NA} else {
			linmod <- lm(1:lrow~as.numeric(cluster.row))
			slope <-  coefficients(linmod)[2] }
			return(slope)
		}
# 
#	These are a group of 'cull' functions  
#		for more information see: ?subset; RSiteSearch("outliers")
#		chisq.test requires vectors of the same length, so use slope
#	Note: use scaled data for best results
#
cullbyslope <- function(cluster.data.index)  {
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
		slope <- apply(cluster.mo, 1, get.slope.a)
		badslope <- c(names(which(is.na(slope))), names(which(slope > 0)))
		cluster.trim <-  cluster.mo[!(rownames(cluster.mo) %in% badslope),]
		cluster.trim <- cluster.trim[, which(colSums(cluster.trim, na.rm=TRUE) != 0)]
		cluster.dftemp <- data.frame(cluster.trim)
		Gene.Name <- as.vector(rownames(cluster.trim))
		cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
		return(cluster.trimdf)
	}
	}  # end clullbyslope
#  This makes sense
#
cullbottom <- function(cluster.data.index)  {
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
		cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	genemeans <- rowMeans(cluster.mo, na.rm=TRUE)
	genesums <- rowSums(cluster.mo, na.rm=TRUE)
	 meancull <- names(genemeans[which(genemeans < (mean(genemeans) - sd(genemeans)))])
	 sumcull <- names(genesums[which(genesums < (mean(genesums) - sd(genesums)))])
	cluster.trim <-  cluster.mo[!(rownames(cluster.mo) %in% unique(c(meancull, sumcull))),]
		cluster.trim <- cluster.trim[, which(colSums(cluster.trim, na.rm=TRUE) != 0)]
		cluster.dftemp <- data.frame(cluster.trim)
	Gene.Name <- as.vector(rownames(cluster.trim))
	cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
	return(cluster.trimdf)	}
}
cullhalf <- function(cluster.data.index)  {
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
		cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	genemeans <- rowMeans(cluster.mo, na.rm=TRUE)
	genesums <- rowSums(cluster.mo, na.rm=TRUE)
	 meancull <- names(genemeans[which(genemeans < mean(genemeans))])
	 sumcull <- names(genesums[which(genesums < mean(genesums))])
	cluster.trim <-  cluster.mo[!(rownames(cluster.mo) %in% unique(c(meancull, sumcull))),]
		cluster.trim <- cluster.trim[, which(colSums(cluster.trim, na.rm=TRUE) != 0)]
		cluster.dftemp <- data.frame(cluster.trim)
	Gene.Name <- as.vector(rownames(cluster.trim))
	cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
	return(cluster.trimdf)	}
}
#  Hypothesis-driven cull:  cull by presence of samples containing a particular gene
#
cullbygene <- function(cluster.data.index, gene.name) {
		cluster.trim1=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
	cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 	cluster.trim1 <- cluster.mo[, !is.na(cluster.mo[gene.name, ])]
	 	cluster.trim2 <- cluster.trim1[which(rowSums(cluster.trim1, na.rm=TRUE) != 0),]
	 	cluster.dftemp <- data.frame(cluster.trim2)
		Gene.Name <- as.vector(rownames(cluster.trim2))
		cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
		return(cluster.trimdf)
	}	
}
#
cullssgenes.1 <- function(cluster.data.index)	{
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
		cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 	singlesamplerows <- which(apply(cluster.mo, 1, filled) == 1)
		#singlegenesamples <- which(apply(t(cluster.mo), 1, filled) == 1)
		cluster.trim <- cluster.mo[-singlesamplerows, ]#-singlegenesamples]
	 	cluster.dftemp <- data.frame(cluster.trim)
		Gene.Name <- as.vector(rownames(cluster.trim))
		cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
		return(cluster.trimdf)
	}	
}
#  This version cuts singlegenesampes too 
cullssgenes <- function(cluster.data.index)	{
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
		cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 	singlesamplerows <- which(apply(cluster.mo, 1, filled) == 1)
		singlegenesamples <- which(apply(t(cluster.mo), 1, filled) == 1)
		if (length(singlesamplerows) > 0) {
			cluster.trim <- cluster.mo[-singlesamplerows, ] 
			if (length(singlegenesamples) > 0) {
				cluster.trim <- cluster.trim[, -singlegenesamples] 
			}	} else if (length(singlegenesamples) > 0) {
				cluster.trim <- cluster.mo[, -singlegenesamples] } else cluster.trim=cluster.mo
	 	cluster.dftemp <- data.frame(cluster.trim)
		Gene.Name <- as.vector(rownames(cluster.trim))
		cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
		return(cluster.trimdf)
	}	
}
#
	cullsinglesamples <- function(cluster.data.index)	{
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
		cluster.df <- data.frame(cluster.data.index)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	rownames(cluster.m) <- cluster.df$Gene.Name
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 	# singlesamplerows <- which(apply(cluster.mo, 1, filled) == 1)
		singlegenesamples <- which(apply(t(cluster.mo), 1, filled) == 1)
		cluster.trim <- cluster.mo[, -singlegenesamples]
	 	cluster.dftemp <- data.frame(cluster.trim)
		Gene.Name <- as.vector(rownames(cluster.trim))
		cluster.trimdf <- base::cbind(Gene.Name, cluster.dftemp)
		return(cluster.trimdf)
	}	
}
#
clust.common <- function(clust.list1, clust.list2)	{
	tt.dd <- matrix(data = NA, nrow = length(clust.list1), ncol = length(clust.list2), byrow = FALSE, dimnames = NULL)
	for (i in 1:length(clust.list1)) {
		for (j in 1:length(clust.list2)) {
			a <- clust.list1[[i]]$Gene.Name
			b <- clust.list2[[j]]$Gene.Name
			common <- intersect (a, b)
			if (length (common) >= 1 )  {
				tt.dd[i,j] <- length (common) }
			if (length (common) >= 3 )  {	
				cat(i, ",", j, "\t", common, "\n") 
				if(any(fungenes %in% common)) print ("     ^-------NOTE!")	}
				}
			}
		return(tt.dd)	}
	# use: t.d <- clust.common(clust.list1, clust.list2)
#
# New version: avoids loops, returns all intersects
#
list.common <- function (list1, list2, keeplength=3) {
	parse <- lapply(list1, function (y) sapply(list2,  function(x) intersect(x, y)))
	dims <- lapply(parse, function (x) sapply(x, length))
	keep <- which(sapply(dims, sum) > keeplength)
	pare <- parse[keep]
	prune <- lapply(pare, function (y) return (y[which(sapply(y, function (x) which(length(x) > keeplength )) > 0)]))
	newlist <- unlist(prune, recursive=FALSE)
	return(newlist)
}
#
	#
combine.clusters<- function (clust.data.index.1, clust.data.index.2) {
		cdf1 <- data.frame(clust.data.index.1)
		cdf2 <- data.frame(clust.data.index.2)
		combind <- plyr::join(cdf1, cdf2, by='Gene.Name', type="full")
		return(combind)	}
		#
combine.clusters.sc <- function (clust.data.sc.index.1, clust.data.sc.index.2) {
		cdf1 <- data.frame(clust.data.sc.index.1)
		cdf2 <- data.frame(clust.data.sc.index.2)
		combind1 <- plyr::join(cdf1, cdf2, by='Gene.Name', type="full")
		combind2 <- cullbyslope(combind1)
		return(combind2)	}
#
findgene <- function(gene, clusterlist) {
	cl.df <- ldply(clusterlist)
	return(cl.df[grepl(gene, cl.df$Gene.Name),])  
	}
#
addtolist <- function(clusterlist, newgenevector, newname) {
	cl.df <- ldply(clusterlist)
	cl.df <- cl.df[,-1]
	new.df <- data.frame(newgenevector)
	names(new.df) <- "Gene.Name"
	new.df$group <- newname
	cl.df2 <- rbind(cl.df, new.df)
	cl2.list <- dlply(cl.df2, .(group))
	return(cl2.list)
}
# for student response files:
numletts <-	function(df) {    
			df <- as.data.frame(sapply(df,gsub,pattern="A",replacement=1))
 			df <- as.data.frame(sapply(df,gsub,pattern="B",replacement=2))
 			df <- as.data.frame(sapply(df,gsub,pattern="C",replacement=3))
 			df <- as.data.frame(sapply(df,gsub,pattern="D",replacement=4))
 			df <- as.data.frame(sapply(df,gsub,pattern="E",replacement=5))
			return(df)			}
			#
 graph.clust5 <- function(cluster.data.index) {
	cluster.df1 <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df1)))) {
		cluster.df	<- cluster.df1[, -grep("Gene.Name", colnames(cluster.df1))] } else {
			cluster.df <- cluster.df1
		}
	if (ncol(cluster.df1) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	if (any(grepl("Gene.Name", colnames(cluster.df1)))) {
		rownames(cluster.m) <- cluster.df1$Gene.Name } else {
			rownames(cluster.m) <- rownames(cluster.df) }
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="none", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(12, 6)) 
	return (cluster.mo) }
	}
	#
# unscaled, larger margins:	
graph.clust5b <- function(cluster.data.index) {
	cluster.df1 <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df1)))) {
		cluster.df	<- cluster.df1[, -grep("Gene.Name", colnames(cluster.df1))] } else {
			cluster.df <- cluster.df1
		}
	if (ncol(cluster.df1) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	if (any(grepl("Gene.Name", colnames(cluster.df1)))) {
		rownames(cluster.m) <- cluster.df1$Gene.Name } else {
			rownames(cluster.m) <- rownames(cluster.df) }
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="none", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(14, 11)) 
	return (cluster.mo) }
	}
# with dentrogram:
graph.clust5d <- function(cluster.data.index) {
	cluster.df1 <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df1)))) {
		cluster.df	<- cluster.df1[, -grep("Gene.Name", colnames(cluster.df1))] } else {
			cluster.df <- cluster.df1
		}
	if (ncol(cluster.df1) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	if (any(grepl("Gene.Name", colnames(cluster.df1)))) {
		rownames(cluster.m) <- cluster.df1$Gene.Name } else {
			rownames(cluster.m) <- rownames(cluster.df) }
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(14, 11)) 
	return (cluster.mo) }
	}
# scale the graph:	
graph.clust6 <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="none", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, hclustfun=NULL, scale="row", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1.8, density.info='histogram', denscol="green", main="Cluster Heatmap", margins=c(12, 10)) 
	return (cluster.mo) }
	}
	#  Add dendrogram:
graph.clust6b <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1, density.info='histogram', cexRow = 1, cexCol = 1.25, denscol="green", main=names(cluster.data.index)[1], margins=c(12, 18)) 
	return (cluster.mo) }
	}
#
# Deal with NA in the way done for tsne dissimilarity
#
graph.clust6c <-  function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=1, density.info='histogram', cexRow = 1, cexCol = 1.25, denscol="green", main=names(cluster.data.index)[1], margins=c(12, 18)) 
	return (cluster.mo) }
	}
#
graph.clust6d <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
	 #rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="Lab", interpolate = "linear")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=0.85, density.info='histogram', cexRow = 1, cexCol = 1, denscol="green", main=names(cluster.data.index)[1], margins=c(14, 10)) 
	return (cluster.mo) }
	}
#
graph.clust6d.l <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	# take out order
	cluster.mo <- cluster.m
	# cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 #rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=0.85, density.info='histogram', cexRow = 1, cexCol = 1, denscol="green", main=names(cluster.data.index)[1], margins=c(14, 10)) 
	return (cluster.mo) }
	}
# Smaller font for tall graphs
graph.clust6d.la <- function(cluster.data.index) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	# take out order
	cluster.mo <- cluster.m
	# cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 #rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	dev.new()  # can resize manually
 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=0.5, density.info='histogram', cexRow = 0.8, cexCol = 1, denscol="green", main=names(cluster.data.index)[1], margins=c(14, 10)) 
	return (cluster.mo) }
	}
# This version makes a pdf file:
graph.clust6d.l2 <- function(cluster.data.index, filename) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	# take out order
	cluster.mo <- cluster.m
	# cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 #rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	# resize according to size
	pdf(file=filename, width=dim(cluster.mo)[2]/6, height=dim(cluster.mo)[1]/6)  
	 	heatmap.2(cluster.mo, dendrogram="row", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=TRUE, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=0.85, key.par=list(pin=c(2,2), pty="s"),  density.info='histogram', cexRow = 1, cexCol = 1, denscol="green", main=names(cluster.data.index)[1], margins=c(14, 10)) 
		dev.off()
	return (cluster.mo) }
	}
#
#  version of pdf file maker that doesn't sort
graph.clust6d.l3 <- function(cluster.data.index, filename) {
	cluster.df <- data.frame(cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) < 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df)
	#cluster.m[ is.nan (cluster.m) ] <- 0	
	#cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	# take out order
	cluster.mo <- cluster.m
	# cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	 #rbyheatcolors <- colorRampPalette(colors=c('#0000FF',  '#FFFF00'), bias=0.25, space="rgb", interpolate = "spline")
	 rbyheatcolors <- colorRampPalette(colors=c('#3333FF', '#FFFF00'), bias=0.25, space="rgb", interpolate = "linear")
		# royal blue to yellow
		palette(c(rbyheatcolors(500)))
	# resize according to size
	pdf(file=filename, width=dim(cluster.mo)[2]/6, height=dim(cluster.mo)[1]/6)  
	 	heatmap.2(cluster.mo, dendrogram="none", trace="none", symbreaks=TRUE, na.color="black", labRow=rownames(cluster.mo), labCol=colnames(cluster.mo), Rowv=NA, Colv=NA, distfun=dist2, hclustfun=hclust, scale="none", col=palette(), colsep=NULL, rowsep=NULL, sepwidth=c(0,0), revC=FALSE, keysize=0.85, key.par=list(pin=c(2,2), pty="s"),  density.info='histogram', cexRow = 1, cexCol = 1, denscol="green", main=names(cluster.data.index)[1], margins=c(14, 12)) 
		dev.off()
	return (cluster.mo) }
	}
#
deblandify <- function(data.file, limit) {
	bigneg <- which(data.file<=-limit, arr.ind = TRUE)
	bigpos <- which(data.file>=limit, arr.ind = TRUE)
	data.file[bigneg] <-  -limit
	data.file[bigpos] <-  limit
	return(data.file)
}
#
hclust2 <- function(z) hclust(z, method="single")
#
dist2 <- function(m) {
	dm <- dist(m, method = "euclidean", diag = FALSE, upper = FALSE)
		dm[is.na(dm)] <- 2*max(abs(dm), na.rm=TRUE)
	return(dm)
	}	
		#
# different approach to evals
		# set up functions:
		allEBF <- function (x) rowMeans(x[,grepl("EBF", names(x))], na.rm=TRUE)
		videoEBF <- function (x) { 
			if (class(x[, grepl("video.EBF", names(x))]) == "data.frame") {
			y = rowMeans(x[,grepl("video.EBF", names(x))], na.rm=TRUE) } else {
			y = x[,grepl("video.EBF", names(x))] } 
			return(y)
			}
		notesEBF <- function (x) rowMeans(x[,grepl("notes.EBF", names(x))], na.rm=TRUE)
		examsEBF <- function (x) rowMeans(x[,grepl("exams.EBF", names(x))], na.rm=TRUE)
		studyEBF <- function (x) { 
			if (class(x[, grepl("studyguides.EBF", names(x))]) == "data.frame") {
			y = rowMeans(x[,grepl("studyguides.EBF", names(x))], na.rm=TRUE) } else {
			y = x[,grepl("studyguides.EBF", names(x))] } 
			return(y)
			} 
		ecbEBF <- function (x) { 
			if (class(x[, grepl("ecb3movies.EBF", names(x))]) == "data.frame") {
			y = rowMeans(x[,grepl("ecb3movies.EBF", names(x))], na.rm=TRUE) } else {
			y = x[,grepl("ecb3movies.EBF", names(x))] } 
			return(y)
			}
		cogindex <- function (x) rowMeans(x[,grepl("Cognitive.Index", names(x))], na.rm=TRUE)
		howstudy <- function (x) rowMeans(x[,grepl("Cognitive.Index", names(x))], na.rm=TRUE)
		online <- function (x) rowMeans(x[,grepl("online.access", names(x))], na.rm=TRUE)
		book <- function (x) rowMeans(x[,grepl("read.book", names(x))], na.rm=TRUE)
		work <- function (x) rowMeans(x[,grepl("work", names(x))], na.rm=TRUE)
		interest <- function (x) rowMeans(x[,grepl("biology.interest", names(x))], na.rm=TRUE)
		#
	bclust.eval <- function(clust.data.list)	{
		# options(error=recover)
		meandf <- ldply(clust.data.list, colwise(mean.na))
		subdf <- laply(clust.data.list, dim)
		colnames(subdf)[1:2] <- c("No.Students", "Filled.Columns")
		subdf <- cbind(subdf, meandf[,c("Z.Total", "Clicker.Points", "Test.Slope", "Normalized.Gain", "Higher.Cognitive.Index.Pretest", "SEX", "ETHN" )]) 
		# includes SEX ETHN 
		subdf $Cog.Ind.Mean <- cogindex(meandf)	
		subdf $Video.EBF <- videoEBF(meandf)	
		subdf $notes.EBF <- notesEBF(meandf)	
		subdf $Studyguides.EBF <- studyEBF(meandf)	
		subdf $Exams.EBF <- examsEBF(meandf)	
		subdf $ECB3Movies.EBF <- ecbEBF(meandf)	
		subdf $EBF.Total <- allEBF(meandf)
		subdf $Study <- howstudy(meandf)
		subdf $Book.Use <- book(meandf)
		subdf $Online.Use <- online(meandf)
		subdf $Work <- work(meandf)
		subdf $Interest <- interest(meandf)
		eval.sort <- subdf[order(subdf$EBF.Total* subdf$Z.Total, decreasing =T),] 
		return(eval.sort)	
	}
		#
# neu function to sort major differences in clusters
	# NOTE: apply scale() first TO DATA BEFORE CLUSTERING (use tbl.sc)
	# so that all columns are equalized, then differences in clusters will be revealed
hilo.eval <- function(scaled.cluster.data.index)  {
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
	cluster.df <- data.frame(scaled.cluster.data.index)
	cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] #
	cluster.df <- cluster.df[, -grep("interest", colnames(cluster.df))]  # low resolution due to lots of interest!
	cluster.df <- cluster.df[, -grep("Higher.Cognitive.Index.M", colnames(cluster.df))] # sometimes obcures other data
	cluster.df <- cluster.df[, -grep("ecb3", colnames(cluster.df))]  # low resolution due to not much use (students may have book cd)
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
	cluster.m <- data.matrix(cluster.df[,2:ncol(cluster.df)])
	cluster.m[ is.nan (cluster.m) ] <- 0	
	cluster.m[ is.na (cluster.m) ] <- 0
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	hicols <- colnames(cluster.mo)[1:5]
	lowcols <- colnames(cluster.mo)[(dim(cluster.mo)[2]-4):dim(cluster.mo)[2]]
	return(cluster.mo[,c(hicols, lowcols)])
	}	}
#
hilo.overview <- function(scaled.cluster.data.index)  {
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
	cluster.df <- data.frame(scaled.cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (ncol(cluster.df) <= 2) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
		subdf <- cluster.df[, -grep("MT", colnames(cluster.df))] #
		subdf $Cog.Ind.Mean <- cogindex(cluster.df)	
		subdf $Video.EBF <- videoEBF(cluster.df)	
		subdf $notes.EBF <- notesEBF(cluster.df)	
		subdf $Studyguides.EBF <- studyEBF(cluster.df)	
		subdf $Exams.EBF <- examsEBF(cluster.df)	
		subdf $ECB3Movies.EBF <- ecbEBF(cluster.df)	
		subdf $EBF.Total <- allEBF(cluster.df)
		subdf $Study <- howstudy(cluster.df)
		subdf $Book.Use <- book(cluster.df)
		subdf $Online.Use <- online(cluster.df)
		subdf $Work <- work(cluster.df)
		subdf $Interest <- interest(cluster.df)
		#
	cluster.m <- data.matrix(subdf)
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	hicols <- colnames(cluster.mo)[1:5]
	lowcols <- colnames(cluster.mo)[(dim(cluster.mo)[2]-4):dim(cluster.mo)[2]]
	return(cluster.mo[,c(hicols, lowcols)])
	}	}
	
overview <- function(scaled.cluster.data.index)  {
		cluster.trim=NULL; cluster.df=NULL; cluster.dftemp=NULL; cluster.mo=NULL
		# options(error=recover)
	cluster.df <- data.frame(scaled.cluster.data.index)
	if (any(grepl("Gene.Name", colnames(cluster.df)))) {
		cluster.df	<- cluster.df[, -grep("Gene.Name", colnames(cluster.df))] }
	if (nrow(cluster.df) == 1) {
		cat ("\n","This is a single sample cluster!", "\n") 
		return (cluster.df) } else {
		subdf <- cluster.df[, -grep("MT", colnames(cluster.df))] #
		subdf $Cog.Ind.Mean <- cogindex(cluster.df)	
		subdf $Video.EBF <- videoEBF(cluster.df)	
		subdf $notes.EBF <- notesEBF(cluster.df)	
		subdf $Studyguides.EBF <- studyEBF(cluster.df)	
		subdf $Exams.EBF <- examsEBF(cluster.df)	
		subdf $ECB3Movies.EBF <- ecbEBF(cluster.df)	
		subdf $EBF.Total <- allEBF(cluster.df)
		subdf $Study <- howstudy(cluster.df)
		subdf $Book.Use <- book(cluster.df)
		subdf $Online.Use <- online(cluster.df)
		subdf $Work <- work(cluster.df)
		subdf $Interest <- interest(cluster.df)
		#
	cluster.m <- data.matrix(subdf)
	rownames(cluster.m) <- rownames(cluster.df)
	cluster.mo <- cluster.m[order(-as.vector(colwise(sum.na)(data.frame(t(cluster.m))))), order(-as.vector(numcolwise(sum.na)(data.frame(cluster.m))))]
	hicols <- colnames(cluster.mo)[1:5]
	lowcols <- colnames(cluster.mo)[(dim(cluster.mo)[2]-4):dim(cluster.mo)[2]]
	return(cluster.mo)
	}	}	
	#
get.score.25 <- function (test.srf, test.key) {
		score <- as.vector(NULL)
	for (i in 1:nrow(test.srf)) { 
		score[i] <- 2*length(which(as.character(t(test.srf[i,4:28])) == as.character(test.key[1:25,2]))) }
	names(test.srf)[1] <- 'Student.ID'
	test.srf$Score <- score
	result <- test.srf[, c(1, ncol(test.srf))]
 	return(result)
}	
get.score.50 <- function (test.srf, test.key) {
		score <- as.vector(NULL)
	for (i in 1:nrow(test.srf)) { 
		score[i] <- 2*length(which(as.character(t(test.srf[i,4:53])) == as.character(test.key[1:50,2]))) }
	test.srf$Score <- score
	result <- test.srf[, c(1, ncol(test.srf))]
 	return(result)
}	
calc.enrichment <- function(control, test) {
	control <- 	abs(control)
	test    <- 	abs(test)
	if ((is.na(control) & is.na(test))) {
		ratio=NA
		return(ratio) } else
	control[is.na(control)] <- 0
	test[is.na(test)] <- 0
	if (control==test) {
		ratio=1
		return(ratio) } else
	if (control > test) {
		if (test==0) {
			ratio=-100
			return(ratio) } else
		ratio <- -1/(test/control)		
		return(ratio) }
	if (control < test) {		
		if (control==0) {
			ratio=100
			return(ratio) } else
		ratio <- (test/control)  
		return(ratio)}
	}	
		#
			#
	
# plotting fuctions
#	
# Color schemes for points:
#	limegreen inside forestgreen; red inside red3; dodgerblue inside blue; mediumpurple1 inside purple2, magenta inside magenta3, orange inside darkorange3, yellow inside black with error bars in dark gold (gold3)
#
#pch = 21:25 can be colored and filled with different colors: col gives the border color and bg the background color 
#	•	pch = 21: filled circle, 
#	•	pch = 22: filled square, 
#	•	pch = 23: filled diamond, 
#	•	pch = 24: filled triangle point-up, 
#	•	pch = 25: filled triangle point down.
plot.gradient <- function(x, y, color, adjust=0) {
	dev.new(width=6, height=5)
	par(mai=c(1.2, 1.2, 0.82, 0.42))
	y.lim <- max.na(y)+0.1*max.na(y)+adjust
	if (color == "purple") {
	plot(x, y, type="o", pch=25, cex=1.4, col="purple2", bg="mediumpurple", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "blue") {
	plot(x, y, type="o", pch=24, cex=1.4, col="blue", bg="dodgerblue", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "darkblue") {
	plot(x, y, type="o", pch=21, cex=1.8, col="navyblue", bg="royalblue1", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "green") {
	plot(x, y, type="o", pch=22, cex=1.8, col="forestgreen", bg="limegreen", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "red") {
	plot(x, y, type="o", pch=21, cex=1.8, col="red3", bg="red", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "orange") {
	plot(x, y, type="o", pch=22, cex=1.8, col="darkorange3", bg="orange", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "magenta") {
	plot(x, y, type="o", pch=21, cex=1.8, col="magenta3", bg="magenta", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }
	if (color == "black" | color == "yellow") {
	plot(x, y, type="o", pch=23, cex=1.8, col="black", bg="yellow", lwd=3, cex.lab=2, cex.axis=1.5, bty="l", tck=-0.02, xlab="gradient fraction", ylab="percent in whole cell", ylim=c(0, y.lim)) }	
	minor.tick(nx=5, ny=2, tick.ratio=0.25)
	}

plot.errorbars <- function (x, MEAN, SEM, color) {
	# error bars
	x=as.numeric(x)
	segments(x0=x, y0=MEAN-SEM, x1=1:24, y1=MEAN+SEM, lwd=2, col= color)
	# 1. The lower bar
	segments(x0=x - 0.1, y0=MEAN-SEM, x1=1:24 + 0.1, y1=MEAN-SEM, lwd=2, col= color, lend=2)
	# 2. The upper bar
	segments(x0=x - 0.1, y0=MEAN+SEM, x1=1:24 + 0.1, y1=MEAN+SEM, lwd=2, col= color,  lend=2)
}
points.gradient <- function(x, y, color) {
	y.lim <- max.na(y)+0.1*max.na(y)
	if (color == "purple") {
	points(x, y, type="o", pch=25, cex=1.4, col="purple2", bg="mediumpurple", lwd=3) }
	if (color == "blue") {
	points(x, y, type="o", pch=24, cex=1.4, col="blue", bg="dodgerblue", lwd=3) }
	if (color == "darkblue") {
	points(x, y, type="o", pch=21, cex=1.8, col="navyblue", bg="royalblue1", lwd=3 ) }
	if (color == "green") {
	points(x, y, type="o", pch=22, cex=1.8, col="forestgreen", bg="limegreen", lwd=3) }
	if (color == "red") {
	points(x, y, type="o", pch=21, cex=1.8, col="red3", bg="red", lwd=3 ) }
	if (color == "orange") {
	points(x, y, type="o", pch=22, cex=1.8, col="darkorange3", bg="orange", lwd=3 ) }
	if (color == "magenta") {
	points(x, y, type="o", pch=21, cex=1.8, col="magenta3", bg="magenta", lwd=3 ) }
	if (color == "black" | color == "yellow") {
	points(x, y, type="o", pch=23, cex=1.8, col="black", bg="yellow", lwd=3 ) }
	if (color == "grey") { 	points(x, y, type="o", pch=18, cex=1, col="grey", lwd=3)}
		}

add.legend <- function(labels, pt.colors, place="topleft") {
	fgcols <- c("purple2", "blue", "navyblue", "forestgreen", "red3", "darkorange3", "magenta3", "black")
	bkgcols <- c("purple", "dodgerblue", "royalblue1", "limegreen", "red", "orange", "magenta", "yellow")
	colnames <- c("purple", "blue", "darkblue", "green", "red", "orange", "magenta", "black")
	pchs <- c(25,24,21,22,21,22,21,23)
	coltable <- data.frame(colnames, fgcols, bkgcols, pchs, stringsAsFactors=FALSE)
	selection=subset(coltable, subset=coltable$colnames %in% pt.colors)
	selection=selection[match(pt.colors, selection$colnames),]
	legend(place, labels, col=selection$fgcols, pt.bg=selection$bkgcols, pch=selection$pch, lwd=3, cex=1.6, bty="n")
	}
##	
label.fractions <- function (yposition) {
	#  graph LYS, E1-3 on gradient graphs
	# discard 7 drops, collect 23, 20, 20, 20, ~25 = 134 drops
	# 6 drop fractions * 23 fractions = 138 drops/gradient
	frs <- 	as.numeric(1:23)
	drops <- as.numeric(1:23)*6
	edrops <- as.numeric(c(7, 30, 50, 70, 90, 110))
	app <- approx(x=drops, y=frs, xout=edrops,  method="linear", rule=2)	
	segments(x0=app$y[1:5], y0=yposition, x1=c(app$y[2:5],23), y1= yposition, lwd=6, lend=1, col=c(c("darkblue","blueviolet","magenta","red","orange")))
	points(x=app$y[1:5], y=rep(yposition, length(app$y)-1), pch="|", cex=1.2, col=c("darkblue","blueviolet","magenta","red","orange"))
	# make labels
	labels <- c("lys", "E1", "E2", "E3", "cyt")
	text(x=c(3.08, 6.66666667, 10, 13.33333, 18), y=yposition-0.08*yposition, labels=labels, cex=1.4, col=c("darkblue","blueviolet","magenta","red","orange"))
	}
# Error propagation functions
# for addition/subtraction the error is the square root of the sum of squares
# for multiplication/division the error is the square root of the fractional uncertainties sqared
	#
	multerror <- function (mean1, err1, mean2, err2) {
		fracterr1 <- (err1/mean1)^2
		fracterr2 <- (err2/mean2)^2
		err12 <- sqrt(fracterr1 + fracterr2)
		return(err12)
	}
	adderror <- function (err1, err2) {
		err12 <- sqrt(err1^2 + err2^2)
		return(err12)
	}
# barplot errorbars:
bp.errorbars <- function (barplot, means, stderrs, color="darkgrey") {
	fr <-(barplot[2]-barplot[1])/10
	segments(barplot, means-stderrs, bp, means+stderrs, lwd=2.5, col=color, lend=2)
	# 1. The lower bar
	segments(barplot - fr, means-stderrs, bp + fr, means-stderrs, lwd=3, col=color, lend=2)
	# 2. The upper bar
	segments(barplot - fr, means+stderrs, bp + fr, means+stderrs, lwd=3, col=color, lend=2)
	}	
# These three functions give identical results in test cases:
cosineDist <- function(x){
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2, na.rm=T) %*% t(rowSums(x^2, na.rm=T))))) 
	}
#				
## extract cosine dissimilarity between columns; use t(x) for this one
 cosineD <- function(x) {
     y <- t(x) %*% x
     res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
     return(res)	}
# cosine function from lsa package
cosine <- function( x, y )
{
    return ( crossprod(x,y) / sqrt( crossprod(x)*crossprod(y) ) )
}
# Use: cos.dis <- 1 - 1-pairwiseCosine(t(testt), t(testt))
pairwiseCosine <- function(matrix1,matrix2) {
    apply(matrix1,2,function(x){
        apply(matrix2,2,cosine,x)
    })
}
# Look up ambiguous gene names and find the one best represented by other peptides in the data
# version to add no. ids using table(), sort, unique, return only actual gene names
ambig.lookup <- function (x, uniq.tab) {
	if(any(!is.na(names(uniq.tab[x])))) {
		maxone <- names(uniq.tab[x])[which(uniq.tab[x]==max.na(uniq.tab[x]))]
		if(length(maxone)>1) {maxone <- paste(sort(unique(maxone)) %w/o% maxone[!is.na(str_extract(unique(maxone), "[[:punct:]]"))], collapse="; ")}
	} else {maxone <- paste(sort(unique(x)) %w/o% x[!is.na(str_extract(unique(x), "[[:punct:]]"))], collapse="; ")}
	return(unique(maxone))
}

func <- function(nodename)	{
	return(func.key[grep(nodename, func.key$Gene.Name),])
}
fix.Inf <- function(df)	{
		bad.neg.inf <- which(df==-Inf, arr.ind = TRUE)
		bad.pos.inf.df <- which(df==Inf, arr.ind = TRUE)
		secMax <- sort(unlist(df)[which(unlist(df) !=Inf)], decreasing=TRUE)[1]   
		if (dim(bad.neg.inf)[1]>=1) {
			secMin <- sort(unlist(df)[which(unlist(df) !=-Inf)], decreasing=FALSE)[1]
			if(secMin>0) {secMin = -2*secMax}
				df.neg.fix <- replace (df, bad.neg.inf, 2*secMin) 
				bad.pos.inf <- which(df.neg.fix==Inf, arr.ind = TRUE)
				if (dim(bad.pos.inf)[1]>1) {
					df1 <- df.neg.fix
					secMax <- sort(unlist(df.neg.fix)[which(unlist(df.neg.fix) !=Inf)], decreasing=TRUE)[1]   
					bad.pos.inf <- which(df.neg.fix==Inf, arr.ind = TRUE)
					df.pos.fix <- replace (df.neg.fix, bad.pos.inf, 2*secMax) 
					df.fix <- df.pos.fix
					}
			}	
		if (dim(bad.neg.inf)[1]==0 & dim(bad.pos.inf.df)[1]>=1) {
			df.pos.fix <- replace (df, bad.pos.inf.df, 2*secMax) 
			df.fix <- df.pos.fix			
			}
		return(df.fix)
}

#
Inf.to.NA <- function(df)	{
		bad.neg.inf <- which(df==-Inf, arr.ind = TRUE)
		bad.pos.inf <- which(df==Inf, arr.ind = TRUE)
		df <- replace (df, bad.neg.inf, NA)	
		df <- replace (df, bad.pos.inf, NA)	
		return(df)
}
