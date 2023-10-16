
# Ciros plot in R -----

library(circlize)

# The data for YII combined dorught rainfed ((sig threshold = 15.5)

# Load the start and End of Chromosome data file

Chr_Start_End <- read.delim("Data/Chr_Start_End.txt")

dim(Chr_Start_End)
head(Chr_Start_End)

# Start of the code ----

# Customized the color, initialize and add the genome or chromosome size-----
circos.clear()
col_text <- "grey40"
circos.par("track.height"=2.0,gap.degree=2,cell.padding=c(0,0,0,0))
circos.initialize(factors= as.factor(Chr_Start_End$chr),
                  xlim=matrix(c(rep(0,21),Chr_Start_End$End),ncol=2))

# genomes ( add the first plot ) ----
circos.track(ylim=c(0,1),panel.fun=function(x,y) {
  chr=CELL_META$sector.index
  xlim=CELL_META$xlim
  ylim=CELL_META$ylim
  circos.text(mean(xlim),mean(ylim),chr,cex=0.8,col=col_text,
              facing="bending.inside",niceFacing=TRUE)
},bg.col="grey90",bg.border=F,track.height=0.1) # perfect ,  track.height gives the width of the chromosome


# genomes x axis (add the genome axis) depends on the genome size ----
brk <- c(100,200,300,400,500,600,700,800,900)*10^6
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  circos.axis(h="top",major.at=brk,labels=round(brk/10^6,1),labels.cex=0.4,
              col=col_text,labels.col=col_text,lwd=0.7,labels.facing="clockwise")
},bg.border=F) # Perfect

# Add the layer of the GWAS signal -----

# Let us load the data file for qtl signal

# The data contains just the first marker from the anova epistatic table from SAS customized

YII_Log15p5_Signal_cov <- read.delim("Data/YII_Log15p5_Signal_cov.txt")

dim(YII_Log15p5_Signal_cov)
# [1] 24735     6
head(YII_Log15p5_Signal_cov,5)

# Status    m_code chr     pos    pos2 LogP
# 1 Nosignal W135_CUVU  1A 1166230 1166230   13
# 2 Nosignal W135_EFQK  1A 1172794 1172794   13
# 3 Nosignal  W15_AQCB  1A 1176237 1176237   13
# 4 Nosignal  W15_AGOR  1A 1210956 1210956   13
# 5 Nosignal  W15_AGFN  1A 1211689 1211689   13

# This file contains all the markers including the one that were not our threshold set which logp=15
# The chromosome marker  Pos =pos2 ; start = end, value is logp, we gave the default value (13) to the marker 
# that did not meet the threshold. Bassically you can increase the threshold if you want to see feewer signal

# Create the coverage data -----

cov <- YII_Log15p5_Signal_cov[, c(3:6)]
setnames(cov, old = c("pos", "pos2", "LogP"), new = c("start","end", "value"))

head(cov,5)


circos.genomicTrack(data=cov,panel.fun=function(start,value,...) {
  circos.genomicLines(start,value,type="l",col="grey50",lwd=0.6)
  circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=100,y1=100,lwd=0.6,lty="11",col="grey90")
  circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=300,y1=300,lwd=0.6,lty="11",col="grey90")
  #circos.segments(x0=0,x1=max(Chr_Start_End$End),y0=500,y1=500,lwd=0.6,lty="11",col="grey90")
},track.height=0.08,bg.border=F)
circos.yaxis(at=c(100,300),labels.cex=0.25,lwd=0,tick.length=0,labels.col=col_text,col="#FFFFFF") 


# So on you can add multiple layer for qtls signals


# rearrangements (to chech the interaction result)

# Load the interaction files

Inter_epistasy_YII_sig15p5 <- read.delim("Data/Inter_epistasy_YII_sig15p5.txt")

head(Inter_epistasy_YII_sig15p5,5)

nuc1 <- Inter_epistasy_YII_sig15p5[, c(2,3,4)]
nuc2 <- Inter_epistasy_YII_sig15p5[, c(6,7,8)]

# Let us set the color

# col.pal = c("1A" ="red", "2A" ="green",	"3A" ="blue",	"4A" ="oragne",	"5A" ="grey",	"6A "="maroon",	"7A" ="chartreuse",
#             "1B" ="hotpink",	"2B" ="deeppink3",	"3B" ="skyblue1", "4B" ="violetred2",	"5B" ="maroon1",
#             "6B" ="orange3",	"7B" ="turquoise",	"1D" ="tomato2", "2D" ="snow4",	"3D" ="chocolate",	"4D" ="cadetblue",
#             "5D" ="yellow",	"6D" ="coral2",	"7D" ="tan3") 
# 
# circos.genomicLink(nuc1,nuc2,col=col.pal,border=NA) # didn´t work perfectly

rcols <- "grey60"
circos.genomicLink(nuc1,nuc2,col=rcols,border=NA) # perfect

circos.genomicLink(nuc1, nuc2, col = sample(1:5, 266, replace = TRUE), border = NA) # ok good but grey is better
circos.clear()