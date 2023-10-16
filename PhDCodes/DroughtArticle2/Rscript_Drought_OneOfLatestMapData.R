# Update the final QTL with the new map_DF

# Load the file 

# Set working directory 

setwd("C:/Users/Koua/sciebo/My_RProjects/Dought Experiment/Rproject_Drought_Article_1")

MAP_D_F <- read.delim("Data/Update_Map_D_F/MAP_D_F.txt")

head(MAP_D_F,5)
dim(MAP_D_F)
tail(MAP_D_F)

MAP_D_F <- MAP_D_F[,c(-7)]
MAP_D_F[22423:22425,1:5]

MAP_D_F <- MAP_D_F[1:22423,]
tail(MAP_D_F)

unique(MAP_D_F$pos==MAP_D_F$Pos_F)

is.array(MAP_D_F$pos)==is.array(MAP_D_F$posF) # good

# Load The QTL final result from Article 1 Supplementary files

Table_Sxl1 <- read.delim("Data/Update_Map_D_F/Table_Sxl1.txt")
head(Table_Sxl1)
dim(Table_Sxl1)

# Merge the two files

MergeTSxl1_Map_DF <- merge(Table_Sxl1, MAP_D_F,by.x = "Marker_code", by.y = "m_code", 
                           all = TRUE, all.x = TRUE, all.y = TRUE)

head(MergeTSxl1_Map_DF)

MergeTSxl1_Map_DF_QTls <- subset(MergeTSxl1_Map_DF, TraitsMarkercb != "NA")

View(MergeTSxl1_Map_DF_QTls)

nrow(subset(MergeTSxl1_Map_DF_QTls, marker!="NA"))

write.table(MergeTSxl1_Map_DF_QTls, "Data/Update_Map_D_F/MergeTSxl1_Map_DF_QTls.txt", row.names=F,quote=F,sep="\t")


# For the interaction QTL

InteractionQTL <- read.delim("Data/Update_Map_D_F/InteractionQTL.txt")

dim(InteractionQTL)

head(InteractionQTL,2)

MergeInterQTL_Map_DF <- merge (InteractionQTL, MAP_D_F,by.x = "M_code", by.y = "m_code", 
                               all = TRUE, all.x = TRUE, all.y = TRUE)

MergeInterQTL_Map_DF <- subset(MergeInterQTL_Map_DF, Marker	 != "NA")

View(MergeInterQTL_Map_DF)

nrow(subset(MergeInterQTL_Map_DF, marker!="NA"))


write.table(MergeInterQTL_Map_DF, "Data/Update_Map_D_F/MergeInterQTL_Map_DF.txt", row.names=F,quote=F,sep="\t")

# for Pleitric QTL

Table_Sxl8 <- read.delim("Data/Update_Map_D_F/Table_Sxl8.txt")

dim(Table_Sxl8)

head(Table_Sxl8,2)

MergePleioTable_Sxl8_Map_DF <- merge (Table_Sxl8, MAP_D_F,by.x = "SNP.Markers.elements", by.y = "m_code", 
                                      all = TRUE, all.x = TRUE, all.y = TRUE)

MergePleioTable_Sxl8_Map_DF <- subset(MergePleioTable_Sxl8_Map_DF, Marker.names	 != "NA")

write.table(MergePleioTable_Sxl8_Map_DF, "Data/Update_Map_D_F/MergePleioTable_Sxl8_Map_DF.txt",
            row.names=F,quote=F,sep="\t")

# For Candidate genes of interaction Qtls

Table_Sxl7_intQTLGenes <- read.delim("Data/Update_Map_D_F/Table_Sxl7_intQTLGenes.txt")

head(Table_Sxl7_intQTLGenes,5)
dim(Table_Sxl7_intQTLGenes)

MergeSxl7_intQTLGenes_Map_DF <- merge (Table_Sxl7_intQTLGenes, MAP_D_F, by.x = "Marker.Peak.in.the.region", by.y = "m_code", 
                                       all = TRUE, all.x = TRUE, all.y = TRUE)

head(MergeSxl7_intQTLGenes_Map_DF,5)
MergeSxl7_intQTLGenes_Map_DF <- subset(MergeSxl7_intQTLGenes_Map_DF, Marker.Peak.in.the.region2	 != "NA")

write.table(MergeSxl7_intQTLGenes_Map_DF, "Data/Update_Map_D_F/MergeSxl7_intQTLGenes_Map_DF.txt",
            row.names=F,quote=F,sep="\t")

