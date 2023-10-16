# Correlation analysis Article 1
# Changed on Monday 10th MAy 2021
# I just change the variable names because the correlation doesn´t care about KN or KNms
Blues_17_18_article1<-read.delim("Data/Blues_17_18_article1.txt")

# Let´s change the variable names

colnames(Blues_17_18_article1)

# [1] "Year"      "TreatEnv"  "Genotypes" "Bio"       "GNms"      "GNSp"      "GY"        "HI"       
# [9] "PH"        "ShBio"     "SNms"      "TKW"       "Hardn"     "NDF"       "PC"        "SC"       
# [17] "Sedi"      "HSr"       "HGr"       "LGr"       "LRr"   

setnames(Blues_17_18_article1, old = c("Year","TreatEnv","Genotypes","Bio","GNms","GNSp","GY",
                                       "HI","PH", "ShBio","SNms","TKW","Hardn","NDF","PC","SC",
                                       "Sedi","HSr","HGr","LGr","LRr"),
         new = c("Year","TreatEnv","Genotypes","PBW","KN","KNSp","GY","HI","PH",
                 "SDW","SN","TKW","Hardn","NDF","GPC","GSC","SED","HSr","HGr","LGr","LRr"))

dim(Blues_17_18_article1)
str(Blues_17_18_article1)
View(Blues_17_18_article1)
# Control17
windows(8,8)
chart.Correlation(Blues_17_18_article1[1:200,c(4:12, 14:16)], histogram = T, method = "pearson")# No DevTrait here
# Dstress17
chart.Correlation(Blues_17_18_article1[201:400,c(4:12, 14:16,18:21)], histogram = T, method = "pearson")

# Control18
windows(8,8)
chart.Correlation(Blues_17_18_article1[401:600,c(4:12, 14:16)], histogram = T, method = "pearson")# No DevTrait here
# Dstress18
chart.Correlation(Blues_17_18_article1[601:800,c(4:12, 14:16,20,21)], histogram = T, method = "pearson")

#Combined control
chart.Correlation(Blues_17_18_article1[c(1:200,401:600),c(4:12, 14:16)], histogram = T, method = "pearson")

#Combined D_stress
chart.Correlation(Blues_17_18_article1[c(201:400,601:800),c(4:12, 14:16,18:21)], histogram = T, method = "pearson")
