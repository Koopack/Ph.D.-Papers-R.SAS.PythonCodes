# Update the final QTL with the new map_DF

# Load the file 

# Get the MAP_D_F data

MAP_D_F <- read.delim("C:/Users/Koua/sciebo/My_RProjects/Dought Experiment/Rproject_Drought_Article_1/Data/Update_Map_D_F/MAP_D_F.txt")

head(MAP_D_F,5)
dim(MAP_D_F)
tail(MAP_D_F)

MAP_D_F <- MAP_D_F[,c(-7)]
MAP_D_F[22423:22425,1:5]

MAP_D_F <- MAP_D_F[1:22423,]
tail(MAP_D_F)

unique(MAP_D_F$pos==MAP_D_F$Pos_F)

is.array(MAP_D_F$pos)==is.array(MAP_D_F$posF) # Good

# Load The QTL final result from Article 2 Supplementary files

Table_Sxl2_QTLall <- read.delim("Data/Update_Map_D_F_Article2/Table_Sxl2_QTLall.txt")
head(Table_Sxl2_QTLall)
tail(Table_Sxl2_QTLall)
dim(Table_Sxl2_QTLall)

# Merge the two files

Merge_Table_Sxl2_QTLall_Map_DF <- merge(Table_Sxl2_QTLall, MAP_D_F,by.x = "Marker", by.y = "m_code", 
                           all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_Table_Sxl2_QTLall_Map_DF)
dim(Merge_Table_Sxl2_QTLall_Map_DF)

Merge_Table_Sxl2_QTLall_Map_DF_Only <- subset(Merge_Table_Sxl2_QTLall_Map_DF, Marker_name != "NA") # worked but kept some NA

dim(Merge_Table_Sxl2_QTLall_Map_DF_Only)
View(Merge_Table_Sxl2_QTLall_Map_DF_Only)

nrow(subset(Merge_Table_Sxl2_QTLall_Map_DF_Only, Marker_name!="NA"))

write.table(Merge_Table_Sxl2_QTLall_Map_DF_Only, "Data/Update_Map_D_F_Article2/Merge_Table_Sxl2_QTLall_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")


# For interaction QTLs

# Load the files

InterQTLall <- read.delim("Data/Update_Map_D_F_Article2/InterQTLall.txt")

head(InterQTLall)
tail(InterQTLall)
dim(InterQTLall)

# Merge the two files

Merge_InterQTLall_Map_DF <- merge(InterQTLall, MAP_D_F,by.x = "M_code", by.y = "m_code", 
                                        all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_InterQTLall_Map_DF)
dim(Merge_InterQTLall_Map_DF)

Merge_InterQTLall_Map_DF_Only <- subset(Merge_InterQTLall_Map_DF, Trait!= "NA") # worked but kept some NA

dim(Merge_InterQTLall_Map_DF_Only)
View(Merge_InterQTLall_Map_DF_Only)

nrow(subset(Merge_InterQTLall_Map_DF_Only, marker.y!="NA"))

write.table(Merge_InterQTLall_Map_DF_Only, "Data/Update_Map_D_F_Article2/Merge_InterQTLall_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")

# For pleitropic and stable QTL

# Load the files
  
TSxl5_Stable_PleioQTL <- read.delim("Data/Update_Map_D_F_Article2/TSxl5_Stable_PleioQTL.txt")

head(TSxl5_Stable_PleioQTL)
tail(TSxl5_Stable_PleioQTL)
dim(TSxl5_Stable_PleioQTL)

# Merge the two files

Merge_TSxl5_Stable_PleioQTL_Map_DF <- merge(TSxl5_Stable_PleioQTL, MAP_D_F,by.x = "Marker", by.y = "m_code", 
                                  all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_TSxl5_Stable_PleioQTL_Map_DF)
dim(Merge_TSxl5_Stable_PleioQTL_Map_DF)

Merge_TSxl5_Stable_PleioQTL_Map_DF_Only <- subset(Merge_TSxl5_Stable_PleioQTL_Map_DF, Marker_name!= "NA") # worked but kept some NA

dim(Merge_TSxl5_Stable_PleioQTL_Map_DF_Only)
View(Merge_TSxl5_Stable_PleioQTL_Map_DF_Only)

nrow(subset(Merge_TSxl5_Stable_PleioQTL_Map_DF_Only, marker!="NA"))

write.table(Merge_TSxl5_Stable_PleioQTL_Map_DF_Only, "Data/Update_Map_D_F_Article2/Merge_TSxl5_Stable_PleioQTL_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")


# # For SPAD and YII epistatic QTL

# Load the files

Table_Sxl8Epis_Corr <- read.delim("Data/Update_Map_D_F_Article2/Table_Sxl8Epis_Corr.txt")

head(Table_Sxl8Epis_Corr)
tail(Table_Sxl8Epis_Corr)
dim(Table_Sxl8Epis_Corr)

# Merge the two files

Merge_Table_Sxl8Epis_Corr_Map_DF <- merge(Table_Sxl8Epis_Corr, MAP_D_F,by.x = "M_code", by.y = "m_code", 
                                            all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_Table_Sxl8Epis_Corr_Map_DF)
dim(Merge_Table_Sxl8Epis_Corr_Map_DF)

Merge_Table_Sxl8Epis_Corr_Map_DF_Only <- subset(Merge_Table_Sxl8Epis_Corr_Map_DF, Traits!= "NA") # worked but kept some NA

dim(Merge_Table_Sxl8Epis_Corr_Map_DF_Only)
View(Merge_Table_Sxl8Epis_Corr_Map_DF_Only)

nrow(subset(Merge_Table_Sxl8Epis_Corr_Map_DF_Only, marker!="NA"))

write.table(Merge_Table_Sxl8Epis_Corr_Map_DF_Only, "Data/Update_Map_D_F_Article2/Merge_Table_Sxl8Epis_Corr_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")

# for the second marker

Merge_Table_Sxl8Epis_Corr_Map_DF2 <- merge(subset(Merge_Table_Sxl8Epis_Corr_Map_DF_Only, marker!="NA"), 
                                           MAP_D_F,by.x = "M_code_2", by.y = "m_code", 
                                          all = TRUE, all.x = TRUE, all.y = TRUE)


head(Merge_Table_Sxl8Epis_Corr_Map_DF2)
dim(Merge_Table_Sxl8Epis_Corr_Map_DF2)

Merge_Table_Sxl8Epis_Corr_Map_DF2_Only <- subset(Merge_Table_Sxl8Epis_Corr_Map_DF2, Traits!= "NA") # worked but kept some NA

dim(Merge_Table_Sxl8Epis_Corr_Map_DF2_Only)
View(Merge_Table_Sxl8Epis_Corr_Map_DF2_Only)

nrow(subset(Merge_Table_Sxl8Epis_Corr_Map_DF2_Only, Pos_F.y!="NA"))

write.table(Merge_Table_Sxl8Epis_Corr_Map_DF2_Only, "Data/Update_Map_D_F_Article2/Merge_Table_Sxl8Epis_Corr_Map_DF2_Only.txt", row.names=F,quote=F,sep="\t")
