# plot multiple lines for Allele frequency
# You could do also like you did temperature date geomline but the procedure is long
#http://www.countbio.com/web_pages/left_object/R_for_biology/R_fundamentals/multiple_curves_R.html
# Call the Data 

Data_AlleleFreq_forR<-read.delim("Data/Data_AlleleFreq_forR.txt")

head(Data_AlleleFreq_forR,5)

Release_year<-Data_AlleleFreq_forR$Row.Labels
W135_AYGO_AA<-Data_AlleleFreq_forR$W135_AYGO_AA
windows(6,5)
par(bg = "#E8DDCB")
par(bg = "#CDCDCD")

#plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,110) )

plot(Release_year, W135_AYGO_AA, type="o", col="blue", pch=1, lty=1, ylim=c(0,20), ylab = "Allelele frequency" )


# # Add second curve to the same plot by calling points() and lines()
# # Use symbol '*' for points.
# points(xdata, y2, col="red", pch="*")
# lines(xdata, y2, col="red",lty=2)

# Add AYGO_BB
W135_AYGO_BB<-Data_AlleleFreq_forR$W135_AYGO_BB

points(Release_year, W135_AYGO_BB, col="red", pch=8)
lines(Release_year, W135_AYGO_BB, col="red",lty=2)

# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
# 
# 
# df = data.frame(date=c(rep(2008:2013, by=1)),
#                 value=c(303,407,538,696,881,1094))
# 
# barplot(df$value, main="TITLE", col="gray", ylab="People", xlab="Years")
# 
# plot(df)
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
# points(df)

# Start
plot(Release_year, W135_AYGO_AA, type="o", col="blue", pch="o", lty=1, ylim=c(0,20), 
     ylab = "Allelele frequency", xlab = " Year of release", 
     main = "Trend in haplotype bloc alleles frenquency over years")

rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")

points(Release_year, W135_AYGO_AA, col="blue", pch=1)
lines(Release_year, W135_AYGO_AA, col="blue",lty=1)

points(Release_year, W135_AYGO_BB, col="red", pch=8)
lines(Release_year, W135_AYGO_BB, col="red",lty=2)

W135_EQEO_AA<-Data_AlleleFreq_forR$W135_EQEO_AA

# # Add Third curve to the same plot by calling points() and lines()
# # Use symbol '+' for points.
# points(xdata, y3, col="dark red",pch="+")
# lines(xdata, y3, col="dark red", lty=3)

points(Release_year, W135_EQEO_AA, col="dark blue",pch=3)
lines(Release_year, W135_EQEO_AA, col="dark blue", lty=3)

W135_EQEO_BB<-Data_AlleleFreq_forR$W135_EQEO_BB
#http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r 

points(Release_year, W135_EQEO_BB, col="dark red",pch=6)
lines(Release_year, W135_EQEO_BB, col="dark red", lty=4)


# Add Legend

# # Adding a legend inside box at the location (2,40) in graph coordinates.
# # Note that the order of plots are maintained in the vectors of attributes.
# legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","black"),
#        pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

legend(1945,20,legend=c("W135_AYGO_CC","W135_AYGO_TT","W135_EQEO_CC", "W135_EQEO_TT"), col=c("blue","red","dark blue", "dark red"),
       pch=c(1,8,3,6),lty=c(1,2,3,4), ncol=1, text.font = 10, cex = 0.75) # Perfect

###############################
# First Haplotype on chromosome 5D

W15_ARZE_AA<-Data_AlleleFreq_forR$W15_ARZE_AA
W15_ARZE_BB<-Data_AlleleFreq_forR$W15_ARZE_BB

# Start
plot(Release_year, W15_ARZE_AA, type="o", col="blue", pch=1, lty=1, ylim=c(0,20), 
     ylab = "Allelele frequency", xlab = " Year of release", 
     main = "Trend in haplotype bloc alleles frenquency over years")

rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")

points(Release_year, W15_ARZE_AA, col="blue", pch=1)
lines(Release_year, W15_ARZE_AA, col="blue",lty=1)

points(Release_year, W15_ARZE_BB, col="red", pch=8)
lines(Release_year, W15_ARZE_BB, col="red",lty=2)

W15_ANKM_AA<-Data_AlleleFreq_forR$W15_ANKM_AA

# # Add Third curve to the same plot by calling points() and lines()
# # Use symbol '+' for points.
# points(xdata, y3, col="dark red",pch="+")
# lines(xdata, y3, col="dark red", lty=3)

points(Release_year, W15_ANKM_AA, col="dark blue",pch=3)
lines(Release_year, W15_ANKM_AA, col="dark blue", lty=3)

W15_ANKM_BB<-Data_AlleleFreq_forR$W15_ANKM_BB
#http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r 

points(Release_year, W15_ANKM_BB, col="dark red",pch=6)
lines(Release_year, W15_ANKM_BB, col="dark red", lty=4)


# Add Legend

# # Adding a legend inside box at the location (2,40) in graph coordinates.
# # Note that the order of plots are maintained in the vectors of attributes.
# legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","black"),
#        pch=c("o","*","+",6),lty=c(1,2,3), ncol=1)

legend(1945,20,legend=c("W15_ARZE_AA","W15_ARZE_BB","W15_ANKM_AA", "W15_ANKM_BB"), col=c("blue","red","dark blue", "dark red"),
       pch=c(1,8,3,6),lty=c(1,2,3,4), ncol=1, text.font = 10, cex = 0.75) # Perfect

####################################

# Second Haplotype 5D

W15_ASWO_AA<-Data_AlleleFreq_forR$W15_ASWO_AA
W15_ASWO_BB<-Data_AlleleFreq_forR$W15_ASWO_BB

# Start
plot(Release_year, W15_ASWO_AA, type="o", col="blue", pch=1, lty=1, ylim=c(0,20), 
     ylab = "Allelele frequency", xlab = " Year of release", 
     main = "Trend in haplotype bloc alleles frenquency over years")

rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")

points(Release_year, W15_ASWO_AA, col="blue", pch=1)
lines(Release_year, W15_ASWO_AA, col="blue",lty=1)

points(Release_year, W15_ASWO_BB, col="red", pch=8)
lines(Release_year, W15_ASWO_BB, col="red",lty=2)

W15_AROJ_AA<-Data_AlleleFreq_forR$W15_AROJ_AA

# # Add Third curve to the same plot by calling points() and lines()
# # Use symbol '+' for points.
# points(xdata, y3, col="dark red",pch="+")
# lines(xdata, y3, col="dark red", lty=3)

points(Release_year, W15_AROJ_AA, col="dark blue",pch=3)
lines(Release_year, W15_AROJ_AA, col="dark blue", lty=3)

W15_AROJ_BB<-Data_AlleleFreq_forR$W15_AROJ_BB

# Different shape in r
#http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r 

points(Release_year, W15_AROJ_BB, col="dark red",pch=6)
lines(Release_year, W15_AROJ_BB, col="dark red", lty=4)


# Add Legend

# # Adding a legend inside box at the location (2,40) in graph coordinates.
# # Note that the order of plots are maintained in the vectors of attributes.
# legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","black"),
#        pch=c("o","*","+",6),lty=c(1,2,3), ncol=1)

legend(1945,20,legend=c("W15_ASWO_AA","W15_ASWO_BB","W15_AROJ_AA", "W15_AROJ_BB"), col=c("blue","red","dark blue", "dark red"),
       pch=c(1,8,3,6),lty=c(1,2,3,4), ncol=1, text.font = 10, cex = 0.75) # Perfect