
# Ciros plot in R -----

library(circlize)

# The data for YII dorught and Rainfed for the coverage and interaction

# Load the start and End of Chromosome data file

Chr_Start_End <- read.delim("Data/Chr_Start_End.txt")

dim(Chr_Start_End)
head(Chr_Start_End)

# Start of the code ----

windows(6,6)
# Customized the color, initialize and add the genome or chromosome size-----
circos.clear()
col_text <- "grey40"
col_text2 <- "black" # for writing label
circos.par("track.height"=2.0,gap.degree=2,cell.padding=c(0,0,0,0))
circos.initialize(factors= as.factor(Chr_Start_End$chr),
                  xlim=matrix(c(rep(0,21),Chr_Start_End$End),ncol=2))

# genomes ( add the first plot ) ----
circos.track(ylim=c(0,1),panel.fun=function(x,y) {
  chr=CELL_META$sector.index
  xlim=CELL_META$xlim
  ylim=CELL_META$ylim
  circos.text(mean(xlim),mean(ylim),chr,cex=0.8,col=col_text2,
              facing="bending.inside",niceFacing=TRUE)
},bg.col="grey90",bg.border=F,track.height=0.1) # perfect ,  track.height gives the width of the chromosome


# genomes x axis (add the genome axis) depends on the genome size ----
brk <- c(100,200,300,400,500,600,700,800,900)*10^6
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  circos.axis(h="top",major.at=brk,labels=round(brk/10^6,1),labels.cex=0.4,
              col=col_text,labels.col=col_text,lwd=0.7,labels.facing="clockwise")
},bg.border=F) # Perfect


# Create the coverage data -----

# Add the layer of the GWAS signal -----

# Let us load the data file for qtl signal

# The data contains just the first marker from the anova epistatic table from SAS customized

Rf_YII_Cov <- read.delim("Data/Rf_YII_Cov.txt")
dim(Rf_YII_Cov)
# [1] 23674     4
head(Rf_YII_Cov,5)
cov_Rf <- Rf_YII_Cov
setnames(cov_Rf, old = c("pos", "Logp"), new = c("start","value"))

head(cov_Rf,5)
cov_Rf <- cov_Rf[,-4]
cov_Rf$end <- cov_Rf$start
cov_Rf <- cov_Rf[, c(1,2,4,3)]

# Coverage for control
circos.genomicTrack(data=cov_Rf,panel.fun=function(start,value,...) {
  circos.genomicLines(start,value,type="l",col="grey50",lwd=0.6)
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=100,y1=100,lwd=0.6,lty="11",col="grey90")
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=300,y1=300,lwd=0.6,lty="11",col="grey90")
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=500,y1=500,lwd=0.6,lty="11",col="grey90")
},track.height=0.08,bg.border=F)
#circos.yaxis(at=c(100,300),labels.cex=0.25,lwd=0,tick.length=0,labels.col=col_text,col="#FFFFFF") 

Ds_YII_Cov <- read.delim("Data/Ds_YII_Cov.txt")

dim(Ds_YII_Cov)
# [1] 23674     4
head(Ds_YII_Cov,5)
cov <- Ds_YII_Cov
cov_Ds <- cov
head(cov_Ds)
cov_Ds$end <- cov_Ds$start
cov_Ds <- cov_Ds[, c(1,2,4,3)]

# Coverage for drought

circos.genomicTrack(data=cov_Ds,panel.fun=function(start,value,...) {
  circos.genomicLines(start,value,type="l",col="grey50",lwd=1.5) # line width 1.5 like 0.6 is ok
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=100,y1=100,lwd=0.6,lty="11",col="grey90")
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=300,y1=300,lwd=0.6,lty="11",col="grey90")
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=500,y1=500,lwd=0.6,lty="11",col="grey90")
},track.height=0.08,bg.border=F)
#circos.yaxis(at=c(100,300),labels.cex=0.25,lwd=0,tick.length=0,labels.col=col_text,col="#FFFFFF") 

# This file contains all the markers including the one that were not our threshold set which logp=15
# The chromosome marker  Pos =pos2 ; start = end, value is logp, we gave the default value (13) to the marker 
# that did not meet the threshold. Bassically you can increase the threshold if you want to see feewer signal


# Load the interaction files

Ds_YII_InterLink <- read.delim("Data/Ds_YII_InterLink.txt")

head(Ds_YII_InterLink,5)


Ds_YII_InterLink_3A <- subset(Ds_YII_InterLink, Chr=="3A"|Chr_2=="3A")

dim(Ds_YII_InterLink_3A)

nuc1_3A <- Ds_YII_InterLink_3A[, c(2,3,4)]
nuc2_3A <- Ds_YII_InterLink_3A[, c(6,7,8)]


# Let us set the color


rcols <- "green"

circos.genomicLink(nuc1_3A,nuc2_3A, col=rcols,border=NA)

# not 3A

Ds_YII_InterLink_3A_NOT3A <- subset(Ds_YII_InterLink, Chr!="3A" & Chr_2!="3A")

dim(Ds_YII_InterLink_3A_NOT3A) # 6 9 perfect like verified in exell


nuc1_NOT3A <- Ds_YII_InterLink[, c(2,3,4)]
nuc2_NOT3A <- Ds_YII_InterLink[, c(6,7,8)]

rcols <- "gray60"
circos.genomicLink(nuc1_NOT3A,nuc2_NOT3A,col=rcols,border=NA)

# Link only interactions between chromososomes

Ds_YII_InterLink$TrueFalse <- Ds_YII_InterLink$Chr==Ds_YII_InterLink$Chr_2

head(Ds_YII_InterLink,5)


Ds_YII_InterLink_DifChrom <- subset(Ds_YII_InterLink, TrueFalse=="FALSE")

dim(Ds_YII_InterLink_DifChrom)
# [1] 29 11


# plot
Ds_YII_InterLink_DifChrom_3A <- subset(Ds_YII_InterLink_DifChrom, Chr=="3A"|Chr_2=="3A")

dim(Ds_YII_InterLink_DifChrom_3A)

nuc1_3A <- Ds_YII_InterLink_DifChrom_3A[, c(2,3,4)]
nuc2_3A <- Ds_YII_InterLink_DifChrom_3A[, c(6,7,8)]

# Let us set the color
rcols <- "green"
circos.genomicLink(nuc1_3A,nuc2_3A, col=rcols,border=rcols, lwd = 2)

# not 3A

Ds_YII_InterLink_DifChrom_3A_NOT3A <- subset(Ds_YII_InterLink_DifChrom, Chr!="3A" & Chr_2!="3A")

dim(Ds_YII_InterLink_DifChrom_3A_NOT3A) 


nuc1_NOT3A <- Ds_YII_InterLink_DifChrom[, c(2,3,4)]
nuc2_NOT3A <- Ds_YII_InterLink_DifChrom[, c(6,7,8)]

rcols <- "gray60"
circos.genomicLink(nuc1_NOT3A,nuc2_NOT3A,col=rcols,border=rcols, lwd = 1)
