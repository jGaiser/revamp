var ALL_GENES = ["ABCC5", "ABCF1", "ABI1", "ABL1", "ABL2", "ABLIM1", "ACAA2", "ACACA", "ACAD9", "ACADS", "ACADSB", "ACADVL", "ACAT1", "ACLY", "ACO1", "ACO2", "ACSL3", "ACTB", "ACTC1", "ACTN1", "ACTN4", "ACTR2", "ACTR3", "ACTR6", "ACTR8", "ADAR", "ADD1", "ADRM1", "AFAP1", "AFAP1L2", "AFF1", "AGFG1", "AHCY", "AHNAK", "AIMP2", "AKAP10", "AKAP12", "AKAP13", "AKAP8", "AKAP8L", "AKR1B1", "AKT1", "AKT1S1", "AKT2", "ALB", "ALDH1A1", "ALDOA", "ALDOC", "ALK", "ALMS1", "ALYREF", "ANK1", "ANK2", "ANKHD1", "ANKRD50", "ANKRD54", "ANKS1A", "ANLN", "ANP32A", "ANP32E", "ANXA1", "ANXA2", "ANXA4", "ANXA5", "AP1B1", "AP1M1", "AP2A1", "AP2B1", "APC", "APCS", "APLP2", "APOA1", "APOE", "APP", "ARAF", "ARAP3", "ARCN1", "ARF1", "ARFGEF1", "ARHGAP18", "ARHGAP21", "ARHGAP29", "ARHGAP32", "ARHGAP5", "ARHGEF11", "ARHGEF12", "ARHGEF17", "ARHGEF2", "ARHGEF5", "ARID1A", "ARID2", "ARID3B", "ARL3", "ARRB1", "ASH1L", "ASH2L", "ASPH", "ASXL2", "ATAD2", "ATF7", "ATG16L1", "ATIC", "ATP1A1", "ATP5A1", "ATP5B", "ATP5J", "ATP5O", "ATR", "ATRX", "ATXN2", "ATXN2L", "AUTS2", "AXIN1", "AXL", "BAD", "BAG3", "BAG4", "BAIAP2L1", "BASP1", "BAZ1A", "BAZ2A", "BCAR1", "BCAR3", "BCL11A", "BCL11B", "BCL7C", "BCL9", "BCL9L", "BCLAF1", "BCOR", "BCR", "BGN", "BIN1", "BLM", "BMI1", "BOD1L1", "BPTF", "BRAF", "BRCA1", "BRD1", "BRD2", "BRD3", "BRD4", "BRD7", "BRIP1", "BRPF1", "BRPF3", "BRSK1", "BRWD1", "BTBD1", "BTF3", "BYSL", "BZW2", "C10orf12", "C10orf54", "C11orf58", "C19orf43", "C2CD5", "CACTIN", "CAD", "CALM3", "CALR", "CAMK1", "CAMK2A", "CAMK2D", "CAMK2G", "CAMSAP1", "CAND1", "CANX", "CAP1", "CAPRIN1", "CAPZA1", "CAPZA2", "CAPZB", "CASKIN2", "CAST", "CAV1", "CAV2", "CBFB", "CBL", "CBLB", "CBLL1", "CBX4", "CBX5", "CBX8", "CCAR1", "CCAR2", "CCDC50", "CCDC59", "CCDC6", "CCDC82", "CCDC94", "CCND2", "CCNL1", "CCT2", "CCT4", "CCT5", "CCT7", "CCT8", "CD2AP", "CD2BP2", "CD44", "CDC37", "CDC42", "CDC42EP4", "CDC5L", "CDCA8", "CDH1", "CDK1", "CDK11B", "CDK13", "CDK14", "CDK17", "CDK2", "CDK5", "CEBPB", "CENPF", "CENPT", "CEP170", "CERS2", "CFL1", "CGN", "CHD1", "CHD1L", "CHD7", "CHD8", "CHD9", "CHEK2", "CHRAC1", "CIR1", "CIRBP", "CIT", "CKB", "CLASP1", "CLASP2", "CLDN1", "CLDN7", "CLIC1", "CLIC5", "CLK2", "CLOCK", "CLTA", "CLTC", "CNBP", "CNN3", "CNNM1", "COG5", "COIL", "COL17A1", "COL25A1", "COL4A4", "COL6A3", "COPA", "COPB1", "COPB2", "COX5A", "CPD", "CPNE1", "CPSF6", "CPSF7", "CPT2", "CREBBP", "CRK", "CRKL", "CRYZ", "CSDA", "CSE1L", "CSNK1G1", "CSNK2A1", "CSNK2A2", "CSTF2", "CSTF2T", "CTAGE5", "CTDP1", "CTNNA1", "CTNNB1", "CTNND1", "CTR9", "CTTN", "CUL1", "CUL5", "CUX1", "CWC15", "CYFIP1", "DARS", "DARS2", "DAZAP1", "DBI", "DBN1", "DCAF5", "DCAF6", "DCLK1", "DCN", "DCP1A", "DCP2", "DCTD", "DCTN1", "DCUN1D5", "DCXR", "DDB1", "DDX1", "DDX17", "DDX23", "DDX24", "DDX39B", "DDX3X", "DDX42", "DDX5", "DDX56", "DEK", "DGCR14", "DHX15", "DHX16", "DHX29", "DHX33", "DHX38", "DHX9", "DIAPH1", "DIDO1", "DLC1", "DLD", "DLG1", "DLGAP4", "DLST", "DNAJA1", "DNAJA3", "DNAJC30", "DNAJC8", "DNM2", "DNMT3A", "DOCK1", "DOCK4", "DOCK5", "DOCK7", "DOK1", "DPF2", "DSC2", "DSG2", "DSP", "DST", "DSTN", "DTNA", "DTX2", "DUT", "DVL3", "DYNC1H1", "DYNC1I2", "DYNC1LI1", "DYNC1LI2", "DYNLL1", "DYRK2", "DYSF", "E2F2", "ECH1", "ECT2", "EEA1", "EEF1A1", "EEF1A2", "EEF1D", "EEF1E1", "EEF1G", "EEF2", "EEPD1", "EFHD1", "EGFR", "EHBP1", "EHBP1L1", "EHHADH", "EHMT2", "EIF1", "EIF1AX", "EIF2S1", "EIF2S2", "EIF2S3", "EIF3A", "EIF3B", "EIF3C", "EIF3D", "EIF3E", "EIF3G", "EIF3L", "EIF4A1", "EIF4A3", "EIF4B", "EIF4E2", "EIF4G1", "EIF4G3", "EIF4H", "EIF5A", "EIF5B", "EIF6", "ELAVL1", "ELMSAN1", "ENAH", "ENO1", "ENSA", "EP300", "EP400", "EPB41", "EPB41L1", "EPB41L2", "EPB41L3", "EPB41L4A", "EPHA2", "EPHB2", "EPHB4", "EPN1", "EPN2", "EPPK1", "EPRS", "EPS15L1", "EPS8", "EPS8L1", "EPS8L2", "ERBB2", "ERBB2IP", "ERBB3", "ERCC5", "ERF", "ERP29", "ERRFI1", "ESYT1", "EWSR1", "EXOC2", "EXOC4", "EXOC5", "EZH2", "EZR", "F11R", "FABP5", "FAF1", "FAM129B", "FAM193B", "FAM54B", "FAM65B", "FARP2", "FAS", "FASN", "FAU", "FBL", "FBN1", "FBXL19", "FCHSD2", "FEN1", "FER", "FGA", "FGFRL1", "FH", "FHDC1", "FHL3", "FHOD3", "FIGNL1", "FIP1L1", "FKBP1A", "FKBP2", "FKBP3", "FLII", "FLNA", "FLNB", "FLNC", "FMNL1", "FMR1", "FN1", "FNBP4", "FOSB", "FOXK1", "FRMD4A", "FRS2", "FUBP1", "FUS", "FXR2", "FYN", "FZD6", "G3BP1", "G3BP2", "G6PD", "GAB1", "GAB2", "GAK", "GANAB", "GAPDH", "GARS", "GATAD1", "GATAD2A", "GATAD2B", "GEMIN7", "GFPT2", "GIGYF1", "GLIS3", "GLUD1", "GNB2L1", "GNL1", "GNPNAT1", "GOLGA3", "GOT1", "GOT2", "GPD2", "GPRC5A", "GRB2", "GRPEL1", "GSE1", "GSK3A", "GSK3B", "GSN", "GSPT1", "GSTP1", "GTF2F1", "GTF2I", "GTF3C1", "GTF3C2", "GTPBP1", "H1F0", "H1FX", "H2AFX", "H3F3A", "HABP4", "HADH", "HADHA", "HADHB", "HARS2", "HAUS6", "HBA2", "HBB", "HBD", "HCFC1", "HCK", "HDAC1", "HDAC3", "HDAC5", "HDGF", "HDLBP", "HELZ", "HERC1", "HGS", "HINT1", "HIPK1", "HIPK3", "HIRA", "HIST1H1C", "HIST1H1D", "HIST1H1E", "HIST1H2AA", "HIST1H2BB", "HIST1H2BC", "HIST1H2BK", "HIST1H3A", "HIST1H4A", "HIST3H3", "HLTF", "HMGA1", "HMGB1", "HMGB2", "HMGCS1", "HN1", "HNRNPA0", "HNRNPA1", "HNRNPA2B1", "HNRNPA3", "HNRNPAB", "HNRNPC", "HNRNPD", "HNRNPF", "HNRNPH1", "HNRNPH2", "HNRNPH3", "HNRNPK", "HNRNPL", "HNRNPM", "HNRNPR", "HNRNPU", "HNRNPUL1", "HNRNPUL2", "HNRPDL", "HNRPLL", "HSD17B4", "HSF1", "HSP90AA1", "HSP90AB1", "HSP90B1", "HSPA12B", "HSPA1B", "HSPA1L", "HSPA2", "HSPA4", "HSPA5", "HSPA6", "HSPA8", "HSPA9", "HSPB1", "HSPD1", "HSPE1", "HSPH1", "HTATSF1", "HUWE1", "HYOU1", "IARS", "IDH1", "IDH3A", "IDH3B", "IFNGR1", "IGF1R", "IGF2BP3", "IK", "IKZF1", "IL6ST", "ILF2", "ILF3", "IMMT", "IMPDH2", "INADL", "INCENP", "INF2", "INPP5D", "INPPL1", "IPO5", "IQGAP1", "IQSEC1", "IRF2BP1", "IRS1", "IRS2", "ITCH", "ITGA6", "ITGB1", "ITGB4", "ITIH2", "ITPR3", "ITSN1", "ITSN2", "IWS1", "JAK2", "KANK1", "KANSL1", "KARS", "KAT6A", "KAT6B", "KAT7", "KCMF1", "KCT2", "KDM1A", "KDM2B", "KHDRBS1", "KHDRBS2", "KHSRP", "KIAA0101", "KIAA1217", "KIAA1429", "KIAA1598", "KIDINS220", "KIF13B", "KIF14", "KIF18B", "KIF1B", "KIF1C", "KIF22", "KIF23", "KIF5B", "KIRREL", "KLC4", "KLF10", "KLHDC10", "KMT2A", "KMT2C", "KMT2D", "KPNA3", "KPNB1", "KRT1", "KRT18", "KRT19", "KRT6A", "KRT7", "KRT8", "KRT9", "LAMA5", "LAMB1", "LARP1", "LARP4B", "LARP7", "LAS1L", "LASP1", "LATS1", "LBR", "LCK", "LDHA", "LDHB", "LIG3", "LIMA1", "LIMD1", "LIMK2", "LMNA", "LMNB1", "LMNB2", "LMO7", "LPP", "LRCH1", "LRCH4", "LRP6", "LRPPRC", "LRRC16A", "LRRC40", "LRRC41", "LSG1", "LSM1", "LSM14A", "LSM14B", "LSM4", "LYAR", "LYN", "MAGED1", "MAGED2", "MAGI1", "MAML3", "MAN1C1", "MAP1B", "MAP2", "MAP2K1", "MAP2K2", "MAP2K4", "MAP2K6", "MAP3K11", "MAP4K4", "MAP4K5", "MAP7D2", "MAPK1", "MAPK11", "MAPK12", "MAPK13", "MAPK14", "MAPK3", "MAPK7", "MAPK9", "MAPKAPK2", "MAPRE1", "MAPRE2", "MARCKS", "MARCKSL1", "MARK1", "MARK2", "MATR3", "MBD3", "MBOAT2", "MBP", "MCM3", "MCM4", "MCM5", "MCM7", "MCPH1", "MDC1", "MDH1", "MDH2", "MDM1", "MEAF6", "MED1", "MED13L", "MED14", "MEF2D", "MEPCE", "MERTK", "MET", "METTL15", "MEX3C", "MFAP1", "MKI67", "MKI67IP", "MKNK1", "MLF1", "MLH1", "MLLT4", "MME", "MOB4", "MORF4L2", "MPDZ", "MPP5", "MPRIP", "MRE11A", "MRPL1", "MRPL12", "MSH2", "MSI1", "MSI2", "MSN", "MST1R", "MTA2", "MTHFD1", "MTHFD1L", "MTMR10", "MTOR", "MTPN", "MUC1", "MVP", "MYBBP1A", "MYC", "MYCBP2", "MYCT1", "MYH10", "MYH9", "MYL12A", "MYO18A", "MYO1B", "MYO1C", "MYO1D", "MYO1E", "MYO9A", "MYO9B", "MYOF", "MYSM1", "NAALADL2", "NAMPT", "NAP1L1", "NARS", "NAT10", "NAV1", "NAV2", "NBN", "NCBP1", "NCBP2", "NCK1", "NCKAP1", "NCL", "NCOA2", "NCOA3", "NCOA5", "NCOA6", "NCOR1", "NCOR2", "NDC80", "NDE1", "NDNL2", "NDRG1", "NEBL", "NEDD9", "NEK6", "NEK7", "NELFB", "NFIA", "NFIB", "NFIC", "NFKB1", "NGEF", "NHP2L1", "NKX2-1", "NME1-NME2", "NME3", "NNT", "NOLC1", "NONO", "NOP2", "NOP56", "NOP58", "NOS3", "NPEPPS", "NPM1", "NRGN", "NSD1", "NSUN2", "NT5DC1", "NUDT21", "NUMA1", "NUMB", "NUP107", "NUP133", "NUP153", "NUP214", "NUP35", "NUP88", "NUP93", "NUP98", "NUPL1", "NUPL2", "NUSAP1", "OAS3", "OAT", "ODF3", "OPTN", "ORC1", "ORC2", "OSBPL11", "OTUD4", "OXSR1", "P4HB", "PABPC1", "PABPC4", "PABPN1", "PACS1", "PAG1", "PAK4", "PAK6", "PAPD4", "PAPOLA", "PAPOLG", "PARD3", "PARK7", "PARP1", "PARP4", "PATL1", "PAWR", "PCBP1", "PCBP2", "PCF11", "PCM1", "PCNA", "PCNP", "PCNT", "PDCD11", "PDCD4", "PDCD6IP", "PDE4D", "PDGFRA", "PDHA2", "PDIA3", "PDIA4", "PDIA6", "PDLIM2", "PDLIM5", "PDPK1", "PDS5B", "PDXDC1", "PECAM1", "PELI2", "PELP1", "PES1", "PEX14", "PFDN1", "PFKP", "PFN1", "PFN2", "PGAM1", "PGD", "PGK1", "PGRMC1", "PGRMC2", "PHB", "PHF10", "PHF2", "PHF3", "PIK3C3", "PIK3R1", "PIK3R2", "PIK3R3", "PIKFYVE", "PIP4K2C", "PITPNA", "PITPNM1", "PJA1", "PKM", "PKN1", "PKN2", "PKP2", "PKP3", "PLCH1", "PLEC", "PLEKHA1", "PLEKHA4", "PLEKHA5", "PLEKHA6", "PLK1", "PLK1S1", "PLS3", "PLXNC1", "PML", "PNN", "PNPO", "PNPT1", "POGZ", "POLA2", "POLR1A", "POLR2B", "POLR3B", "POLR3C", "POLR3E", "POM121", "PON2", "PPA1", "PPA2", "PPFIA1", "PPFIBP1", "PPFIBP2", "PPIB", "PPIG", "PPIL2", "PPIL4", "PPM1G", "PPP1CA", "PPP1R12A", "PPP1R13B", "PPP1R16A", "PPP1R7", "PPP2R5A", "PPP2R5C", "PPP2R5E", "PPP6R3", "PRC1", "PRDM10", "PRDX1", "PRDX2", "PRDX3", "PRDX6", "PRKAA1", "PRKACA", "PRKAR2A", "PRKCA", "PRKCD", "PRKCDBP", "PRKCI", "PRKCQ", "PRKCSH", "PRKCZ", "PRKD1", "PRKD2", "PRKDC", "PRKG1", "PROSER2", "PRPF3", "PRPF31", "PRPF38B", "PRPF40A", "PRPF4B", "PRR3", "PRRC2A", "PSAT1", "PSEN1", "PSIP1", "PSMA2", "PSMA3", "PSMA4", "PSMA6", "PSMA7", "PSMB1", "PSMB2", "PSMB6", "PSMC1", "PSMC2", "PSMC5", "PSMC6", "PSMD14", "PSMD2", "PSMD9", "PSMF1", "PSPC1", "PTBP1", "PTBP3", "PTGES3", "PTK2", "PTMA", "PTPN1", "PTPN11", "PTPN12", "PTPN14", "PTPN23", "PTPN6", "PTPRA", "PTPRF", "PTPRK", "PTRF", "PUM1", "PUM2", "PVRL1", "PVRL2", "PXN", "PYCR1", "PYGB", "PYGL", "PYGO2", "QARS", "QKI", "RAB10", "RABEP1", "RAC1", "RACGAP1", "RAD51C", "RAD54B", "RAD54L2", "RAI1", "RALY", "RAN", "RANBP1", "RANBP10", "RANBP2", "RANBP9", "RANGAP1", "RAPGEF1", "RAPGEF5", "RAPH1", "RASA1", "RASAL2", "RAVER1", "RB1", "RB1CC1", "RBAK", "RBBP6", "RBL1", "RBL2", "RBM10", "RBM14", "RBM15", "RBM15B", "RBM22", "RBM26", "RBM3", "RBM33", "RBM39", "RBM4", "RBM47", "RBM7", "RBM8A", "RBMX", "RBMXL1", "RCC2", "RECQL", "REM2", "REPIN1", "REPS2", "RFC1", "RGS12", "RHBDD1", "RHEB", "RICTOR", "RIF1", "RIN2", "RIPK1", "RLIM", "RNASEL", "RNF19B", "RNF4", "RNPC3", "ROBO1", "ROCK1", "ROCK2", "RPA1", "RPL10A", "RPL11", "RPL12", "RPL13", "RPL14", "RPL15", "RPL18A", "RPL19", "RPL23", "RPL23A", "RPL26", "RPL26L1", "RPL27", "RPL27A", "RPL29", "RPL3", "RPL30", "RPL31", "RPL35", "RPL35A", "RPL36A", "RPL37", "RPL37A", "RPL4", "RPL5", "RPL6", "RPL7", "RPL7A", "RPL8", "RPL9", "RPLP2", "RPN1", "RPS10", "RPS11", "RPS12", "RPS13", "RPS15A", "RPS17", "RPS2", "RPS20", "RPS21", "RPS25", "RPS27A", "RPS28", "RPS29", "RPS3", "RPS3A", "RPS4X", "RPS6", "RPS6KA3", "RPS6KA4", "RPS6KB1", "RPS7", "RPS8", "RPS9", "RRAGC", "RRP12", "RRP15", "RRS1", "RSRC1", "RTF1", "RTKN", "RTN4", "RUFY1", "RUNX1", "RUVBL1", "RUVBL2", "RYBP", "S100A11", "SAE1", "SAFB", "SAFB2", "SAP130", "SARS", "SART1", "SART3", "SASH1", "SATB1", "SBF1", "SCAF11", "SCAF8", "SCEL", "SCFD1", "SCP2", "SCPEP1", "SCRIB", "SDC4", "SDCBP", "SDE2", "SDHA", "SDPR", "SEC23A", "SEC31A", "SEPT10", "SEPT6", "SEPT9", "SERBP1", "SERPINA1", "SET", "SETD1A", "SETD1B", "SETDB1", "SF1", "SF3A1", "SF3B1", "SF3B2", "SF3B4", "SFN", "SFPQ", "SGOL2", "SH3GLB2", "SH3KBP1", "SHANK2", "SHB", "SHC1", "SHMT2", "SIN3A", "SIN3B", "SIPA1L1", "SIRT1", "SIRT6", "SIRT7", "SKA3", "SKIL", "SLC12A8", "SLC20A1", "SLC20A2", "SLC25A22", "SLC25A3", "SLC25A5", "SLC25A6", "SLC2A1", "SLC39A3", "SLC4A1AP", "SLC7A11", "SLC7A2", "SLC9A3R1", "SLITRK5", "SMARCA4", "SMARCA5", "SMARCC1", "SMARCC2", "SMC1A", "SMC2", "SMC3", "SMG1", "SMG6", "SMG7", "SMYD5", "SND1", "SNF8", "SNIP1", "SNRNP200", "SNRNP70", "SNRPA", "SNRPA1", "SNRPB", "SNRPC", "SNRPD1", "SNRPD3", "SNRPN", "SNW1", "SNX4", "SOCS4", "SOD1", "SOD2", "SON", "SORBS1", "SORBS2", "SORD", "SOS1", "SP100", "SP3", "SPAG1", "SPAG9", "SPATS2L", "SPECC1", "SPEN", "SPP1", "SPRY1", "SPRY2", "SPTAN1", "SPTBN1", "SQRDL", "SQSTM1", "SRC", "SRCIN1", "SREK1IP1", "SRP14", "SRP54", "SRP9", "SRRM1", "SRRM2", "SRRT", "SRSF1", "SRSF2", "SRSF3", "SRSF4", "SRSF5", "SRSF6", "SRSF9", "SSB", "SSBP1", "SSBP2", "SSBP3", "SSFA2", "SSH2", "SSH3", "SSR3", "SSX2IP", "STAT1", "STAT3", "STAT5A", "STAU1", "STIM1", "STIP1", "STK10", "STK11", "STK3", "STK38L", "STK39", "STK4", "STX12", "STX7", "STXBP5", "SUMO2", "SUPT5H", "SUPT6H", "SUV39H1", "SVIL", "SYAP1", "SYK", "SYMPK", "SYNCRIP", "SYNJ1", "SYNPO", "SYNPO2", "SYPL1", "TAB2", "TAF12", "TAF15", "TAF1C", "TAF6L", "TAGLN", "TAGLN2", "TALDO1", "TANC1", "TANK", "TAOK1", "TARDBP", "TARS", "TBC1D1", "TBC1D14", "TBC1D2", "TBC1D4", "TBC1D5", "TBCA", "TBL1XR1", "TCEB3", "TCERG1", "TCF12", "TCOF1", "TCP1", "TEC", "TERF2IP", "TFAP4", "TFG", "THOC2", "THOC5", "THRAP3", "TJP1", "TJP2", "TKT", "TLK2", "TLN1", "TMED9", "TMEM154", "TMPO", "TNK1", "TNK2", "TNKS1BP1", "TNPO1", "TNPO3", "TNRC6B", "TNS1", "TNS2", "TNS3", "TOM1L1", "TOMM34", "TOMM70A", "TOP2A", "TOX4", "TP53BP1", "TP53I11", "TPI1", "TPM1", "TPM3", "TPM4", "TPR", "TPT1", "TPX2", "TRA2B", "TRAP1", "TRIM2", "TRIM24", "TRIM25", "TRIM28", "TRIM33", "TRIO", "TRIP10", "TRIP11", "TRIP12", "TRIP6", "TSC2", "TSC22D4", "TTC28", "TTN", "TUBA1A", "TUBA1B", "TUBA4A", "TUBB", "TUBB4B", "TUFM", "TWF2", "TWISTNB", "TXN", "TXNDC5", "TYK2", "TYRO3", "U2AF1", "U2SURP", "UACA", "UBA1", "UBA52", "UBE2D3", "UBE2E3", "UBE2K", "UBE2N", "UBE2T", "UBE3A", "UBR5", "UBTF", "UBXN7", "UFL1", "UFM1", "UGDH", "UGP2", "UIMC1", "UMPS", "UPF1", "UPF3B", "URI1", "USP13", "USP24", "USP3", "USP5", "USP6NL", "USP9X", "UTP15", "UVRAG", "VAPA", "VAV1", "VBP1", "VCL", "VCP", "VDAC1", "VDAC2", "VDAC3", "VHL", "VIM", "VPS13C", "VPS26B", "VPS35", "VRK1", "VRK2", "WAC", "WARS", "WASL", "WDR1", "WDR33", "WDR70", "WEE1", "WHSC1", "WIPF2", "WIZ", "WNK1", "XPR1", "XRCC5", "XRCC6", "XRN2", "YAP1", "YARS", "YARS2", "YBX1", "YBX2", "YES1", "YLPM1", "YWHAB", "YWHAE", "YWHAG", "YWHAQ", "YWHAZ", "ZBTB20", "ZBTB4", "ZBTB7B", "ZC3H14", "ZC3H18", "ZC3H7A", "ZC3HAV1", "ZC3HC1", "ZCCHC11", "ZCCHC6", "ZDHHC5", "ZFC3H1", "ZFP91", "ZFPM1", "ZFR", "ZHX2", "ZKSCAN4", "ZMYM4", "ZNF174", "ZNF24", "ZNF326", "ZNF511", "ZNF532", "ZNF622", "ZNF638", "ZNF644", "ZNF652", "ZNF768", "ZRANB2", "ZYX", "ZZEF1", "AP1AR", "ZNF185", "ATG16L2", "CEP128", "DNAJC17", "DPYSL3", "EPS8L3", "MARK3", "PERP", "RALGAPA1", "TJP3", "ATP13A1", "SPR", "BCL10", "CCDC86", "GGCX", "ECHS1", "PCID2", "SHROOM3", "FOXO1", "HIST2H2BF", "PHB2", "PPP1R10", "PMM2", "TWF1", "AIM1", "UBAP2L", "CDK9", "ABCF3", "POP1", "TMEM194A", "TXNRD1", "EPB41L5", "IFT43", "DOCK10", "DOCK6", "PPAT", "CHAMP1", "FNIP1", "FRMD6", "GPRC5C", "UBAP2", "AVL9", "EXPH5", "VPS37B", "MTMR14", "GDI2", "GLO1", "ZBTB34", "ANXA8", "CLU", "LAT", "SLC4A7", "ATP5L", "GPI", "NANS", "WBP11", "ASPN", "PRELP", "SIPA1L2", "ST18", "ZNF292", "SNX29", "ATP2B1", "ANXA3", "PLEKHA7", "PPIA", "STAG2", "UHRF1BP1L", "CACNB1", "SPECC1L", "PAIP1", "KIAA0754", "NCAPH2", "SAMD9", "CHMP7", "LIMCH1", "PRRC2C", "SNX3", "ST5", "GOLGA4", "PDCL", "TACC1", "RAB3GAP1", "MCFD2", "TCEAL3", "HIVEP1", "BRI3", "FAM65A", "TANC2", "N4BP2", "ZNF280C", "AVEN", "COTL1", "NSUN5", "NT5C3A", "COL18A1", "OSBPL10", "TROAP", "QSOX1", "FARSA", "EZH1", "L3MBTL3", "FRMD8", "PEX6", "ATPIF1", "CLDN2", "RILPL1", "RNF26", "MTPAP", "WDR7", "PHIP", "SRRM3", "IDH2", "PHACTR2", "TOP2B", "HECTD1", "MYOZ1", "WWC2", "ESYT2", "TNFAIP3", "CDK12", "MYLK", "PHF14", "CDC42EP1", "KRT4", "BRIX1", "DSTYK", "PITRM1", "RNPEP", "ARPC2", "FGD5", "MFF", "WHSC1L1", "ARHGEF40", "BCAP31", "DDR1", "UTRN", "KIAA0556", "AAK1", "PPIP5K1", "CNN2", "USO1", "RPTOR", "HBS1L", "CECR5", "FAM103A1", "LBH", "MISP", "OGFR", "PPAN", "CCNY", "IRX5", "GPR126", "MACF1", "SMTN", "SLC25A1", "GAPVD1", "LZTS2", "MICAL3", "DCPS", "SAMD1", "SCYL1", "CNOT11", "COL4A1", "LSR", "DOK4", "RPRD2", "ZNF827", "AKAP2", "CCDC92", "USP39", "CSDE1", "EI24", "CCS", "DMTN", "ST13", "PRMT1", "RBFOX2", "FXR1", "PRDM2", "ESRP2", "FAT4", "WDR47", "C19orf47", "NEO1", "PPP1R9A", "RAB25", "GSTM3", "MYO6", "PDZD2", "DCDC2", "DHRS2", "SCLY", "DNPEP", "NFKBIL1", "PPP1R13L", "TMEM201", "VWA5A", "SAT1", "SLK", "CEP170B", "PDAP1", "SEC62", "DIS3L2", "SERPINA3", "ACOT9", "AKR1C1", "PLXND1", "CDYL", "EML1", "IBA57", "PHAX", "RSL1D1", "TFIP11", "PUS7", "RPGR", "C11orf68", "RAB1A", "RPS18", "FAT1", "NAA50", "PTTG1IP", "MED8", "CLINT1", "NIPSNAP1", "SLITRK6", "HLA-A", "PKP4", "GORASP2", "CARD6", "HMGN3", "SAMSN1", "BAZ1B", "CASK", "SLC19A1", "SLC30A6", "PAFAH1B2", "BCAT2", "SARM1", "MAP3K10", "TRAPPC1", "ALDH4A1", "LPIN1", "YTHDC1", "CSRP1", "DIEXF", "FANCA", "HN1L", "NEK1", "IGHG1", "SARNP", "FKBP4", "KMT2B", "SZT2", "TSN", "GIGYF2", "EIF2A", "PHKB", "LAMA3", "ANGEL2", "C1D", "GFPT1", "FRK", "EEFSEC", "PTPN2", "SEPT7", "RPL13A", "RAB13", "FAHD2A", "FAM50A", "MEX3D", "ZNF579", "NOL7", "C1orf106", "LAMTOR1", "ZAK", "FOXK2", "DDX31", "DHX36", "RPL17", "ATXN10", "BBX", "TXLNG", "PEA15", "SYNRG", "S100A6", "COX7A2", "FAM98B", "MIF", "NCS1", "POR", "ZC3H13", "NAPG", "GNL3L", "AFP", "GBAS", "KIAA0930", "DBNL", "DCP1B", "FLVCR1", "GLG1", "PDZK1IP1", "PI4KA", "RSRC2", "SUV420H1", "POSTN", "TDRD3", "GTSE1", "TRMT1L", "VIPAS39", "TSSC4", "CTDSPL2", "DAB2IP", "KRT13", "HMOX1", "MTM1", "TMEM230", "KANSL2", "PHYH", "SUN2", "XRCC1", "LURAP1L", "MYH11", "PEAK1", "GPR157", "CYB5R1", "NUCB1", "RNF111", "CELF1", "ITIH4", "WWC1", "AHNAK2", "GAB3", "CDC26", "HEXIM2", "GPS1", "IL17RA", "AQP4", "RHBDF1", "RSF1", "NCOA1", "DDHD1", "FAM195A", "WDR20", "NACA", "SREK1", "DLGAP5", "RCOR1", "CRTC2", "ADD3", "PPP1R14B", "RASAL1", "CEP164", "ARID1B", "TUBB3", "UBE2E2", "OCRL", "RAB21", "SLTM", "THUMPD1", "DYRK1A", "PAICS", "PHF20L1", "RRBP1", "UTP14A", "ZNF219", "NCAPG", "LAD1", "TLK1", "CDK18", "RRAGD", "STAU2", "LGALS4", "FBXL18", "TXNRD2", "RAD50", "ADAM9", "PA2G4", "PPP2CA", "TACC2", "AMPD2", "AGTRAP", "ELF2", "SMARCAD1", "NSFL1C", "EIF4G2", "SUPT16H", "DHCR7", "FAM73A", "ZNF318", "CHAF1A", "S100A8", "NKD2", "PTPN13", "SEC22B", "SRGAP1", "DLG5", "CDC42BPB", "SMEK1", "MAP3K9", "C1orf198", "GNPAT", "MSANTD4", "SERINC5", "TRIOBP", "KIAA0907", "RPS19", "UQCRH", "C19orf53", "TOR1AIP1", "CKAP2L", "ECHDC1", "CCDC9", "HELZ2", "API5", "ZGPAT", "COX5B", "TLE3", "SERPINH1", "CAMSAP3", "CYB561", "LUZP1", "AIFM1", "CALML3", "URB2", "IKBKG", "SLC12A7", "DBF4B", "KANK2", "PBRM1", "SAMD4B", "SHANK3", "SNAP29", "FTSJD2", "PEF1", "BLVRA", "SLC25A10", "ABCC2", "PANK2", "TAP1", "SMARCD2", "IQSEC2", "VARS", "ETV3", "STAT6", "FAM83B", "SZRD1", "RNF34", "DMXL2", "SARG", "BIRC6", "GPBP1", "C8orf33", "ZC2HC1A", "GART", "ETFA", "PYCR2", "FAM20B", "CAT", "TMEM214", "TAF6", "GRHL2", "ARIH2", "C12orf10", "RBM42", "FAM120B", "FOXRED1", "BCKDHB", "DYRK3", "GAS2L1", "COBL", "NIPBL", "TRMT1", "USP36", "WDR37", "SGMS2", "LEMD2", "PUS10", "TBC1D30", "SNX1", "TXLNA", "ARFGEF2", "TRAF7", "DISP2", "VGLL4", "NES", "REPS1", "TRMT5", "AFAP1L1", "CCDC14", "TESK2", "FANCD2", "BAZ2B", "HEXB", "MBTD1", "HMGB3", "CNP", "PSMB9", "DDI2", "NBEAL2", "TPD52", "BRD8", "RABL6", "S100A2", "TBCB", "ZNF598", "PPP2R5D", "POLL", "LARP4", "RPL34", "SRA1", "SRSF12", "RCSD1", "ZNF106", "CA1", "FKBP15"];

var GENE_FAMILIES = [
  {value: "Yphosphatases",
   label: "tyrosine phosphatases"},
  {value: "acetyltransferases",
   label: "acetyltransferases"},
  {value: "bromoPs",
   label: "bromo- and BET-domain-containin proteins"},
  {value: "chromps",
   label: "chromatin-associated proteins"},
  {value: "cytoskeletonPs",
   label: "cytoskeleton-associated proteins"},
  {value: "deacetylases",
   label: "deacetylases"},
  {value: "demethylases",
   label: "demethylases"},
  {value: "endosome.ps",
   label: "endosome-associated proteins"},
  {value: "gpcrproteins",
   label: "G-protein-coupled receptors"},
  {value: "hsps",
   label: "heat shock proteins"},
  {value: "htsfactors",
   label: "transcription factors"},
  {value: "kins",
   label: "kinases"},
  {value: "membraneproteins",
   label: "membrane proteins"},
  {value: "methylbindingPs",
   label: "methyl-protein binding proteins"},
  {value: "methyltransferases",
   label: "methyltransferases"},
  {value: "pSbindingPs",
   label: "phospho-serine binding proteins"},
  {value: "phosphatases",
   label: "protein phosphatases"},
  {value: "ptbPs",
   label: "PTB-domain-containin proteins"},
  {value: "rnabindingproteins",
   label: "RNA binding proteins (HUGO)"},
  {value: "rnaprocproteins",
   label: "RNA processing proteins"},
  {value: "rnaproteins",
   label: "RNA-binding proteins (HPRD)"},
  {value: "rnaspliceproteins",
   label: "RNA splicing proteins"},
  {value: "rtklist",
   label: "receptor tyrosine kinases"},
  {value: "sfklist",
   label: "SRC family kinases"},
  {value: "sh2list",
   label: "SH2-domain-containing proteins"},
  {value: "sh2sh3s",
   label: "proteins with both SH2 and SH3 domains"},
  {value: "sh3list",
   label: "SH3-domain-containing proteins"},
  {value: "stkins",
   label: "serine/threonine kinases"},
  {value: "stphosphatases",
   label: "serine/threonine phosphatases"},
  {value: "tudors",
   label: "tudor-domain-containing proteins"},
  {value: "ubbindingendPs",
   label: "ubiquitin-binding proteins"},
  {value: "ykinases",
   label: "tyrosine kinases"},
]

module.exports = {
  ALL_GENES: ALL_GENES,
  GENE_FAMILIES: GENE_FAMILIES
};