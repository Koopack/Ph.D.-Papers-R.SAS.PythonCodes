# Rscript_Drought_Article3_HeatMapNew

# Modif on Saturday, 26th August 2023 -

# Heatmap Expressionpartern

#https://davetang.org/muse/2018/05/15/making-a-heatmap-in-r-with-the-pheatmap-package/ 

install.packages("pheatmap")
# load package
install.packages("pheatmap")
library(pheatmap)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
library(BiocManager)




Dat_for_Heatmap <- read.delim("Data/Dat_for_Heatmap.txt")

head(Dat_for_Heatmap)
pheatmap(Dat_for_Heatmap)

row.names(Dat_for_Heatmap) <- Dat_for_Heatmap[,1]

Dat_for_Heatmap <- Dat_for_Heatmap[,-1]
head(Dat_for_Heatmap)

# data <- read.delim(example_file, header=T, row.names="gene")
# data_subset <- as.matrix(data[rowSums(data)>50000,])

data_subset <- as.matrix(Dat_for_Heatmap)

pheatmap(data_subset) # Perfect, you just had to well prepare the data

?pheatmap()


# Chor plot of gene ontology classification

OntologyBP_and_MF <- read.delim("Data/OntologyBP_and_MF.txt")
head(OntologyBP_and_MF,2)



# mtcars$car = row.names(mtcars)
# p = ggplot(mtcars, aes(x=car, y=mpg, fill=mpg)) +
#   geom_bar(binwidth=1, stat=’identity’) +theme_light() +
#   scale_fill_gradient(low=’red’, high=’white’, limits=c(5,40)) +
#   theme(axis.title.y=element_text(angle=0))
# p + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

# in simple barplot

p = ggplot(OntologyBP_and_MF, aes(x = Functional_Category, y = Genes_in_list, fill = Genes_in_list)) +
  ylab("Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,7)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

# in coord polar plot

windows(10,10)
p = ggplot(OntologyBP_and_MF, aes(x = Functional_Category, y = Genes_in_list, fill = Genes_in_list)) +
  ylab("Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,7)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

p + coord_polar() 

colnames(OntologyBP_and_MF) [2] <- "Ontology_Class"
colnames(OntologyBP_and_MF) [6] <- "mLog_FDR"
dim(OntologyBP_and_MF)
tail(OntologyBP_and_MF,8)
OntologyBP_and_MF <- OntologyBP_and_MF[1:30,]

p = ggplot(OntologyBP_and_MF, aes(x = Ontology_Class, y = Genes_in_list, fill = mLog_FDR)) +
  ylab("Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,7)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1, face="bold"))+
  coord_polar()


########################

ClassGO_Heatmap <- read_excel("Data/OntologyBP_and_MF_allGenes.xlsx", sheet = "ClassGO")

head(ClassGO_Heatmap)

row.names(ClassGO_Heatmap) <- ClassGO_Heatmap[,1]

ClassGO_Heatmap <- ClassGO_Heatmap[,-1]
head(ClassGO_Heatmap)

# data <- read.delim(example_file, header=T, row.names="gene")
# data_subset <- as.matrix(data[rowSums(data)>50000,])

data_subset <- as.matrix(Dat_for_Heatmap)

pheatmap(data_subset) # Perfect, you just had to well prepare the data

?pheatmap()


# chor plot of gene ontology classification

ClassGO_Heatmap <- read_excel("Data/OntologyBP_and_MF_allGenes.xlsx", sheet = "ClassGO")
head(ClassGO_Heatmap,2)



# mtcars$car = row.names(mtcars)
# p = ggplot(mtcars, aes(x=car, y=mpg, fill=mpg)) +
#   geom_bar(binwidth=1, stat=’identity’) +theme_light() +
#   scale_fill_gradient(low=’red’, high=’white’, limits=c(5,40)) +
#   theme(axis.title.y=element_text(angle=0))
# p + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

# in simple barplot
colnames(ClassGO_Heatmap)[1] <- "Genes_in_list"
View(ClassGO_Heatmap)
p = ggplot(subset(ClassGO_Heatmap,Genes_in_list>10), aes(x = GO_category, y = Genes_in_list, fill = Percenatge)) +
  ylab("Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,9)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

# in coord polar plot

windows(10,10)
p = ggplot(OntologyBP_and_MF, aes(x = Functional_Category, y = Genes_in_list, fill = Genes_in_list)) +
  ylab("Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,7)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

p + coord_polar()

colnames(OntologyBP_and_MF) [2] <- "Ontology_Class"
colnames(OntologyBP_and_MF) [6] <- "mLog_FDR"
dim(OntologyBP_and_MF)
tail(OntologyBP_and_MF,8)
OntologyBP_and_MF <- OntologyBP_and_MF[1:30,]

p = ggplot(subset(ClassGO_Heatmap,Genes_in_list>10), aes(x=reorder(GO_category, Genes_in_list), y = Genes_in_list, fill = Percenatge)) +
  ylab("Genes in the list") +
  xlab("GO class")+
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,9)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=10,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face="bold", size=8)) +
  coord_polar()

# Saturday, 26th August 2023 ------
# Renew the ClassGO_Heatmap in Barplot

p = ggplot(subset(ClassGO_Heatmap,Genes_in_list>10), aes(x = GO_category, y = Genes_in_list, fill = Percenatge)) +
  ylab("Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,9)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) # ok 

# No the GO_category on Y axis

ggplot(subset(ClassGO_Heatmap,Genes_in_list>10), aes(x = Genes_in_list, y = GO_category , fill = Percenatge)) +
  ylab("GO category") +
  xlab("Number of Genes in the list") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,9)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) # ok

# Now try to order the percentages

ggplot(subset(ClassGO_Heatmap,Genes_in_list>10), aes(x = Genes_in_list, y = reorder (GO_category, Genes_in_list) , 
  fill = Percenatge)) + ylab("GO category") +
  xlab("Percentage of Genes in the list (%)") +
  geom_bar(binwidth=1, stat = "identity") +theme_light() +
  scale_fill_gradient(low = "white", high="red", limits=c(0,9)) +
  theme(axis.title.y=element_text(angle=0))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold", angle = 90))+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) 




# End Modif

ClassGO_Heatmap

# p + coord_polar() + aes(x=reorder(GO_category, Genes_in_list))
# text with angle to avoid name overlap

# Monday 12th April 2021 ----

# Genes expresssion Parterns -----

Data_WheatBro24Genes_sub <- read_excel("Data/Expression_Patern.xlsx", sheet = "Data_WheatBro24Genes_sub")

Data_WheatBro24Genes_sub <- as.data.frame(Data_WheatBro24Genes_sub)
head(Data_WheatBro24Genes_sub, 2)

row.names(Data_WheatBro24Genes_sub) <- Data_WheatBro24Genes_sub[,1]

Data_WheatBro24Genes_sub <- Data_WheatBro24Genes_sub[,-1]
head(Data_WheatBro24Genes_sub)

# data <- read.delim(example_file, header=T, row.names="gene")
# data_subset <- as.matrix(data[rowSums(data)>50000,])

Data_WheatBro24Genes_sub <- as.matrix(Data_WheatBro24Genes_sub)

windows(10,7)
pheatmap(Data_WheatBro24Genes_sub)

# The expression Pattern from ebi uk database https://www.ebi.ac.uk/gxa/experiments

#colorramppalette r

# Load the data

Data_EbiUk_24Genes <- read_excel("Data/Expression_Patern.xlsx", sheet = "Data_EbiUk_24Genes")
head(Data_EbiUk_24Genes, 2)
Data_EbiUk_24Genes <- as.data.frame(Data_EbiUk_24Genes)
head(Data_EbiUk_24Genes, 2)

# Set the row names
row.names(Data_EbiUk_24Genes) <- Data_EbiUk_24Genes[,1]
head(Data_EbiUk_24Genes, 2)
Data_EbiUk_24Genes <- Data_EbiUk_24Genes[,-1]
head(Data_EbiUk_24Genes, 2)

row.names(Data_EbiUk_24Genes) <- Data_EbiUk_24Genes[,1]

Data_EbiUk_24Genes <- Data_EbiUk_24Genes[,-1]
head(Data_EbiUk_24Genes)

Data_EbiUk_24Genes_t <- t(Data_EbiUk_24Genes)
head(Data_EbiUk_24Genes_t)

# data <- read.delim(example_file, header=T, row.names="gene")
# data_subset <- as.matrix(data[rowSums(data)>50000,])

Data_EbiUk_24Genes_t <- as.matrix(Data_EbiUk_24Genes_t)

# Replace all the NA by 0

# d[is.na(d)] <- 0

Data_EbiUk_24Genes_t[is.na(Data_EbiUk_24Genes_t)] <- 0
head(Data_EbiUk_24Genes_t)

windows(10,7)
pheatmap(Data_EbiUk_24Genes_t) # Great 

# Change the colorramp https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r 

pheatmap(Data_EbiUk_24Genes_t, color = colorRampPalette(c("white", "red"))(50)) 
#Great but keep the default colro for consistency

pheatmap(test, color = colorRampPalette(c("yellow", "white", "blue"))(50))
pheatmap(Data_EbiUk_24Genes_t, color = "RdYlBu")


