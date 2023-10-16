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

# Load The QTL final results 

QTLs_4Env13TraitRTassel <- read_excel("Data/MTAs_Drought_Art3_NDynamic.xlsx", sheet = "QTLs_4Env13TraitRTassel")
head(QTLs_4Env13TraitRTassel)
tail(QTLs_4Env13TraitRTassel)
dim(QTLs_4Env13TraitRTassel)

# Merge the two files QTLs_4Env13TraitRTassel  and MAP_D_F

Merge_QTL13Traits_Map_DF <- merge(QTLs_4Env13TraitRTassel, MAP_D_F,by.x = "Marker", by.y = "m_code", 
                           all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_QTL13Traits_Map_DF)
dim(Merge_QTL13Traits_Map_DF)

Merge_QTL13Traits_Map_DF_Only <- subset(Merge_QTL13Traits_Map_DF, TraitTreament != "NA")
dim(Merge_QTL13Traits_Map_DF_Only)
View(Merge_QTL13Traits_Map_DF_Only)

nrow (subset(Merge_QTL13Traits_Map_DF_Only, TraitTreament!="NA"))

write.table(Merge_QTL13Traits_Map_DF_Only, "Data/Update_Map_D_F_Article3/Merge_QTL13Traits_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")


# For the Qtls of 18 Traits in R Studio

# Load the files AllQTLsRstudio18Traits

AllQTLsRstudio18Traits <- read_excel("Data/MTAs_Drought_Art3_NDynamic.xlsx", sheet = "AllQTLsRstudio18Traits")

head(AllQTLsRstudio18Traits)
tail(AllQTLsRstudio18Traits)
dim(AllQTLsRstudio18Traits)

# Merge the two files

Merge_AllQTLsRstudio18Traits_Map_DF <- merge(AllQTLsRstudio18Traits, MAP_D_F,by.x = "Marker", by.y = "m_code", 
                                        all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_AllQTLsRstudio18Traits_Map_DF)
dim(Merge_AllQTLsRstudio18Traits_Map_DF)

Merge_AllQTLsRstudio18Traits_Map_DF_Only <- subset(Merge_AllQTLsRstudio18Traits_Map_DF, Trait!= "NA") 
dim(Merge_AllQTLsRstudio18Traits_Map_DF_Only)
View(Merge_AllQTLsRstudio18Traits_Map_DF_Only)

nrow(subset(Merge_AllQTLsRstudio18Traits_Map_DF_Only, Trait!="NA"))

write.table(Merge_AllQTLsRstudio18Traits_Map_DF_Only, "Data/Update_Map_D_F_Article3/Merge_AllQTLsRstudio18Traits_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")

# Load the two Data udated that you will merge

QTLs_4Env13Traits_upd <- read_excel("Data/MTAs_Drought_Art3_NDynamic.xlsx", sheet = "QTLs_4Env13Traits_upd")
head(QTLs_4Env13Traits_upd)
tail(QTLs_4Env13Traits_upd)
dim(QTLs_4Env13Traits_upd)

AllQTLsRstudio18Traits_upd <- read_excel("Data/MTAs_Drought_Art3_NDynamic.xlsx", sheet = "AllQTLsRstudio18Traits_upd")
head(AllQTLsRstudio18Traits_upd)

QTLs_4Env13Tr_6Env18Traits <- merge(QTLs_4Env13Traits_upd, AllQTLsRstudio18Traits_upd,by.x = "EnvGwasTraitMarker", by.y = "EnvGwasTraitMarker", 
                                             all = TRUE, all.x = TRUE, all.y = TRUE)

tail(QTLs_4Env13Tr_6Env18Traits)
dim(QTLs_4Env13Tr_6Env18Traits)


write.table(QTLs_4Env13Tr_6Env18Traits, "Data/Update_Map_D_F_Article3/QTLs_4Env13Tr_6Env18Traits.txt", row.names=F,quote=F,sep="\t")


# Saturday, 10th April 2021 Update the interaction QTLs for drought article 3

GWAS_int_Ds_Rf_TassLogP3 <- read.delim("Data/Update_Map_D_F_Article3/GWAS_int_Ds_Rf_TassLogP3.txt")

head(GWAS_int_Ds_Rf_TassLogP3)
tail(GWAS_int_Ds_Rf_TassLogP3)
dim(GWAS_int_Ds_Rf_TassLogP3)

# Merge the two files GWAS_int_Ds_Rf_TassLogP3  and MAP_D_F

GWAS_int_Ds_Rf_TassLogP3_Map_DF <- merge(GWAS_int_Ds_Rf_TassLogP3, MAP_D_F,by.x = "Marker", by.y = "m_code", 
                                  all = TRUE, all.x = TRUE, all.y = TRUE)

head(GWAS_int_Ds_Rf_TassLogP3_Map_DF)
dim(GWAS_int_Ds_Rf_TassLogP3_Map_DF)

GWAS_int_Ds_Rf_TassLogP3_Map_DF_Only <- subset(GWAS_int_Ds_Rf_TassLogP3_Map_DF, Treatment != "NA")
dim(GWAS_int_Ds_Rf_TassLogP3_Map_DF_Only)
View(GWAS_int_Ds_Rf_TassLogP3_Map_DF_Only)

nrow (subset(GWAS_int_Ds_Rf_TassLogP3_Map_DF_Only, Treatment!="NA"))

write.table(GWAS_int_Ds_Rf_TassLogP3_Map_DF_Only, "Data/Update_Map_D_F_Article3/GWAS_int_Ds_Rf_TassLogP3_Map_DF_Only.txt", row.names=F,quote=F,sep="\t")


