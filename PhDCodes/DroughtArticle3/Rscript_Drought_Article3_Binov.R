# Breeding progress

# Load the data containing the year of release


Drought_RY <- read.delim("Data/Blues_17_18_article1_Brinov.txt")

head(Drought_RY,2)
dim(Drought_RY)

Drought_RY_lab <- Drought_RY[1:200, c(3,4,5,6,7)]
head(Drought_RY_lab)

# Now merge the data

mergeRefBio<-merge(Refgeno2017, Biomass_Kg_ha,by.x = "Reference", by.y = "Reference", 
                   all = TRUE, all.x = TRUE, all.y = TRUE)

dim(DataBlue_all18_recode)
dim(Drought_RY_lab)

merge_RY_lab_Blue_recode<-merge(Drought_RY_lab, DataBlue_all18_recode,by.x = "Genotypes", by.y = "Genotypes", 
                   all = TRUE, all.x = TRUE, all.y = TRUE)
dim(merge_RY_lab_Blue_recode)
head(merge_RY_lab_Blue_recode)
tail(merge_RY_lab_Blue_recode)

# create New combination of Treatments by paste

merge_RY_lab_Blue_recode$TreatEnV_N <- paste(merge_RY_lab_Blue_recode$Water,merge_RY_lab_Blue_recode$Nitrogen)
merge_RY_lab_Blue_recode$TreatEnV_N <- paste0(merge_RY_lab_Blue_recode$Water,"_",merge_RY_lab_Blue_recode$Nitrogen)

levels(merge_RY_lab_Blue_recode$TreatEnV_N)

str(merge_RY_lab_Blue_recode$TreatEnV_N)

merge_RY_lab_Blue_recode$TreatEnV_N <- as.factor(merge_RY_lab_Blue_recode$TreatEnV_N)

# Plot the Brinov in circular
# ggplot(eventdata, aes(x = factor(months), fill = season)) + 
#   geom_histogram()+
#   coord_polar()

ggplot(merge_RY_lab_Blue_recode, aes(x = Year_Rel,  fill = TreatEnV_N)) + 
  geom_histogram(bins = 30)+
  coord_polar()

ggplot(merge_RY_lab_Blue_recode, aes(x = Year_Rel,  fill = TreatEnV_N)) + 
  geom_line()+
  coord_polar()

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Seedyield,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Grain yield") + ggtitle ("Grain yield ") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

# ggplot(paulists_by_year, aes(x = year, y = confessions)) +
#   geom_line() + (aes(x=year, y = confessions)) + geom_point()

ggplot(merge_RY_lab_Blue_recode, aes(x = Year_Rel, y = Seedyield, fill = TreatEnV_N)) +
  geom_line(color = "blue") + (aes(x=Year_Rel, y = Seedyield)) + geom_point()


# Try again
# https://zachbogart.com/post/learning-r-polar-coords/

install.packages("rattle.data")
library(rattle.data)

# aus_temp <- weather %>%
#   select(Date, Temp9am, Temp3pm) %>%
#   pivot_longer(cols = Temp9am:Temp3pm, names_to="temp", values_to="value") %>% 
#   ggplot(aes(x=Date)) +
#   geom_line(aes(y=value, color=temp)) +
#   labs(x="",y="Temperature (Celsius)")
# 
# aus_temp + coord_polar()

str(merge_RY_lab_Blue_recode)

merge_RY_lab_Blue_recode %>%
ggplot(aes(x = Year_Rel)) +
  geom_line(aes(y = Spad, color = TreatEnV_N)) +
  labs(x="Year of release",y="Grain yield") + 
  coord_polar() +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

# Let us breack the y-scale
merge_RY_lab_Blue_recode %>%
  ggplot(aes(x = Year_Rel)) +
  geom_line(aes(y = Spad, color = TreatEnV_N), linejoin = "bevel", size = 1) +
  labs(x="Year of release",y="SPAD Value") + 
  coord_polar() +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12)) +
  scale_x_continuous( breaks = c( 1960, 1970, 1980, 1990, 2000, 2010)) +
  theme(plot.background = element_rect(fill = "gray80"))

merge_RY_lab_Blue_recode %>%
  ggplot(aes(x = Year_Rel)) +
  geom_line(aes(y = NUEGrain, color = TreatEnV_N), linejoin = "bevel", size = 1) +
  labs(x="Year of release",y="SPAD Value") + 
  coord_polar() +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12)) +
  scale_x_continuous( breaks = c( 1960, 1970, 1980, 1990, 2000, 2010)) +
  theme(plot.background = element_rect(fill = "gray80"))

head(merge_RY_lab_Blue_recode)


merge_RY_lab_Blue_recode %>%
  ggplot(aes(x = Year_Rel)) +
  geom_line(aes(y = NHI, color = TreatEnV_N), linejoin = "bevel", size = 1) +
  labs(x="Year of release",y="SPAD Value") + 
  coord_polar() +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12)) +
  scale_x_continuous( breaks = c( 1960, 1970, 1980, 1990, 2000, 2010)) +
  theme(plot.background = element_rect(fill = "gray80"))