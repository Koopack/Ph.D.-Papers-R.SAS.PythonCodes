#Yields Stability Analysis 

GGEBiplot(Data=YStabAmmiallEnv)
# Transform Row names
# how to?
samp2 <- samp[,-1]
rownames(samp2) <- samp[,1]

rownames(YStabAmmiallEnv)<-YStabAmmiallEnv[,1]
head(YStabAmmiallEnv)

GGEBiplot(Data=YStabAmmiallEnv)
# Discrimitiveness Vs Representativeness show a clear picture
#1- take the first column as genotype name
#2- just biplot the environment YStabAmmiallEnv[,c(2:10)]

GGEBiplot(Data=YStabAmmiallEnv[,c(2:10)])
#Rename column 
setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))

setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))

setnames(YStabAmmiallEnv, old= c("HN_NF_Env1", "HN_WF_Env2", "LN_NF_Env3", "HN_NF_Env4", "HN_WF_Env5", "LN_NF_Env6", "HN_NF_Env7", "HN_WF_En8", "LN_NF_Env9"), new=c("Env1", "Env2", "Env3", "Env4", "Env5", "Env6", "Env7", "Env8", "Env9"))

GGEBiplot(Data=YStabAmmiallEnv[,c(2:10)])

rownames(YStabAmmiallMega)<-YStabAmmiallEnv[,1]
tail(YStabAmmiallMega)

### Interesting !!!!
add function into aggregate function 

YearEnvsd=aggregate(YLD~ENV+BLOCK, data = Mean.forGEARAnalysis, function(x) c(sd=sd(x)))
YearEnvrange=aggregate(YLD~ENV+BLOCK, data = Mean.forGEARAnalysis, function(x) c(range=range(x)))
YearEnvMin=aggregate(YLD~ENV+BLOCK, data = Mean.forGEARAnalysis, function(x) c(min=min(x)))
YearEnvsd=aggregate(YLD~ENV+BLOCK, data = Mean.forGEARAnalysis, function(x) c(sd=sd(x)))
YearEnvMean=aggregate(YLD~ENV+BLOCK, data = Mean.forGEARAnalysis, FUN = mean)

YearEnvMean2=aggregate(YLD~ENV+BLOCK, data = Mean.forGEARAnalysis, FUN = max)
View(YStabAmmiallEnv)


##################

#Load the Data

All9CS46GenoforAmmi<-read.delim("Data/All9CS46GenoforAmmi.txt")
View(All9CS46GenoforAmmi)

# Load the mean value over CS

All9CS46GenoforAmmi9cs<-read.delim("Data/All9CS46GenoforAmmi9cs.txt")
# transform the Data through tididyverse
library(tidyverse)
head(All9CS46GenoforAmmi9cs,5)
All9CS46GenoforAmmi9csWide<-pivot_wider(All9CS46GenoforAmmi9cs, names_from = "ENV", values_from = "YLD")

All9CS46GenoforAmmi9cs_Geno_ENv<-All9CS46GenoforAmmi9cs[,c(1,2,5)]
head(All9CS46GenoforAmmi9cs_Geno_ENv,5)

All9CS46GenoforAmmi9cs_Geno_ENvWide<-pivot_wider(All9CS46GenoforAmmi9cs_Geno_ENv, names_from = "ENV", values_from = "YLD")
head(All9CS46GenoforAmmi9cs_Geno_ENvWide,5)
################### Didn´t work well

# Load the mean value over CS

All9CS46GenoforAmmi9cs_Geno_ENv<-read.delim("Data/All9CS46GenoforAmmi9cs_Geno_ENv.txt")
head(All9CS46GenoforAmmi9cs_Geno_ENv,5)

install.packages("GGEBiplotGUI")
library(GGEBiplotGUI)

GGEBiplot(Data=All9CS46GenoforAmmi9cs_Geno_ENv[,2:10]) #ok

# Discrimitiveness Vs Representativeness show a clear picture
#1- take the first column as genotype name
#2- just biplot the environment YStabAmmiallEnv[,c(2:10)]

row.names(All9CS46GenoforAmmi9cs_Geno_ENv)<-All9CS46GenoforAmmi9cs_Geno_ENv[,1]

GGEBiplot(Data=All9CS46GenoforAmmi9cs_Geno_ENv[,2:10]) # ok 

# Load the 3 Megas Cs

Mean3MegaCS46Ammi<-read.delim("Data/Mean3MegaCS46Ammi.txt")
head(Mean3MegaCS46Ammi,3)

row.names(Mean3MegaCS46Ammi)<-Mean3MegaCS46Ammi[,1]

GGEBiplot(Data=Mean3MegaCS46Ammi[,2:4]) # ok 

# We chose the model no scaling and 
# we centered by Tester centered G+GE
# dual metric preserving

install.packages("gge")
library(gge)

install.packages("devtools")
library(devtools)

gge(Data=Mean3MegaCS46Ammi[,2:4]) # Error in UseMethod("gge") : 
#no applicable method for 'gge' applied to an object of class "data.frame"

# Let us use the package description 

Matrix_Mean3MegaCS46Ammi<-as.matrix(Mean3MegaCS46Ammi)

## S3 method for class 'matrix'
gge(Matrix_Mean3MegaCS46Ammi, center = TRUE, scale = TRUE, gen.group = NULL,
    env.group = NULL, comps = c(1, 2), method = "svd")

gge_Matrix_Mean3MegaCS46Ammi<-gge(Matrix_Mean3MegaCS46Ammi[,2:4], center = TRUE, scale = TRUE, gen.group = NULL,env.group = NULL, comps = c(1, 2), method = "svd")

gge_Matrix_Mean3MegaCS46Ammi<-gge(Matrix_Mean3MegaCS46Ammi[,2:4], center = TRUE, scale = FALSE, gen.group = NULL,env.group = NULL, comps = c(1, 2), method = "svd")
# The scale = False doesn´t give good results
# S.V.P. Singular Value Decomposition and Partitioning 

biplot(gge_Matrix_Mean3MegaCS46Ammi, main="GGE biplot")

# change the genotype font size
biplot(gge_Matrix_Mean3MegaCS46Ammi, main="GGE biplot",cex.gen = 1, cex.env = 1)

# Have a look on the 3 D
biplot3d(gge_Matrix_Mean3MegaCS46Ammi, main="GGE biplot", cex.gen = 1, cex.env = 1)



