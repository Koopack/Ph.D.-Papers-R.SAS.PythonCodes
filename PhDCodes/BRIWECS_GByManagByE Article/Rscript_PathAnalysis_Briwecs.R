# Run The path analysis

#install.packages(c("lavaan", "semPlot", "OpenMx", "tidyverse", "knitr", "kableExtra","GGally"))

# if(!require(somepackage)){
#   install.packages("somepackage")
#   library(somepackage)
# }
# New way to jump up

if(!require(c("lavaan", "semPlot", "OpenMx", "tidyverse", "knitr", "kableExtra","GGally")))
  {
  install.packages(c("lavaan", "semPlot", "OpenMx", "tidyverse", "knitr", "kableExtra","GGally"))
  library(lavaan)
  library(semPlot)
  library(OpenMx)
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(GGally)
}

# First let us load the newly corrected Briwecs corrected data

#### you can Create a vector to your project

briwecs_corrected<- read.delim("Data/BriwecsAverage3Yearsalldata.txt")

# Let´s have a look on the appearance of the Data
head(briwecs_corrected)
briwecs_corrected[1:10,1:15]
dim(briwecs_corrected)
str(briwecs_corrected)
# Well! the data looks fine

# Let us set the first model with all the variable as prdictor for  yields.

# We checked the assumption for reggesssion analysis. 

# With an observation dataframe 

briwecs_corrected_HN_NF<-briwecs_corrected[briwecs_corrected$Treatment%in%"HN_NF",]

briwecs_corrected_HN_WF<-briwecs_corrected[briwecs_corrected$Treatment%in%"HN_WF",]

briwecs_corrected_LN_NF<-briwecs_corrected[briwecs_corrected$Treatment%in%"LN_NF",]

# Let us write the model

# Variables in my data
# Heading_date	Spad	Yellow_Rust	PH	Spikes_per_m2	Kernels_per_spike	Kernels_per_m2	TKW	HI
# Biomass_per_m2	Grain_yield	Crude_Protein	SC	Sedimentation

# model <-'
# mpg ~ hp + gear + cyl + disp + carb + am + wt
# hp ~ cyl + disp + carb
# '

model_HN_NF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_NF <- cfa(model_HN_NF, data = briwecs_corrected_HN_NF)
fit_HN_NF <- cfa(model_HN_NF, sample.cov=cor(briwecs_corrected_HN_NF[,3:16]), sample.nobs=220)
#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_HN_NF, fit.measures = TRUE, standardized=T,rsquare=T)

# Let us Build a Structural Equation Model (SEM)
# semPaths(fit, 'std', layout = 'circle')

semPaths(fit_HN_NF, 'std', layout = 'circle')

# Regressions:
#                Estimate      Std.Err  z-value  P(>|z|)    Std.lv      Std.all
# Grain_yield ~                                                                 
# Heading_date          0.824    0.231    3.570    0.000        0.824    0.110
# Spad                  0.285    0.133    2.141    0.032        0.285    0.064
# Yellow_Rust          -2.709    0.564   -4.801    0.000       -2.709   -0.158
# PH                   -0.055    0.061   -0.905    0.365       -0.055   -0.031
# Spikes_per_m2         0.003    0.012    0.213    0.831        0.003    0.009
# Kernels_pr_spk       -0.213    0.135   -1.578    0.114       -0.213   -0.115
# Kernels_per_m2       -0.000    0.000   -1.312    0.190       -0.000   -0.112
# TKW                   0.160    0.147    1.088    0.277        0.160    0.047
# HI                  161.345   14.224   11.343    0.000      161.345    0.510
# Biomass_per_m2        0.026    0.004    7.342    0.000        0.026    0.447
# Crude_Protein        -1.057    1.160   -0.911    0.362       -1.057   -0.035
# SC                    1.261    0.610    2.067    0.039        1.261    0.062
# Sedimentation        -0.126    0.082   -1.544    0.123       -0.126   -0.056


# In the table above the std.all gives the contribution (dirrect impact) 
# standardized parameter estimate whereas Std.lv gives the unstadarzed parametr estimates

# So NO NEED TO RUN IT WITH correlation matrix
#Co__HN_NF=cor(briwecs_corrected_HN_NF[,3:16])
###########################################################
# Let us run grain yield with yield components
model_HN_NF <-'
Grain_yield~ a*Spikes_per_m2+b*Kernels_per_spike+c*Kernels_per_m2+
d*TKW+e*HI+f*Biomass_per_m2+g*SC
'
fit_HN_NF <- cfa(model_HN_NF, data = briwecs_corrected_HN_NF)

summary(fit_HN_NF, fit.measures = TRUE, standardized=T,rsquare=T)


#Let us break the yield components with intermediate path

model_HN_NF_mediate <-'
Grain_yield~ Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+TKW+HI+Biomass_per_m2+SC
HI~Spikes_per_m2+Kernels_per_spike+Kernels_per_m2
SC~TKW
' # the first line should have all the variable

fit_HN_NF_mediate <- cfa(model_HN_NF_mediate, data = briwecs_corrected_HN_NF)

summary(fit_HN_NF_mediate, fit.measures = TRUE, standardized=T,rsquare=T)


# O r

model_HN_NF_mediate <-'
Grain_yield~ HI+Biomass_per_m2+SC
HI~Spikes_per_m2+Kernels_per_spike+Kernels_per_m2
SC~TKW
' 


Grain_yield~ Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
  TKW+HI+Biomass_per_m2+SC



############################################################
#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_HN_NF2, fit.measures = TRUE, standardized=T,rsquare=T)



## Comment A genotype with hihg harvest index is desired!

# Let us run for HN_WF

model_HN_WF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_WF <- cfa(model_HN_WF, data = briwecs_corrected_HN_WF)

#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_HN_WF, fit.measures = TRUE, standardized=T,rsquare=T)


# Let us run for LN_NF

model_LN_NF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_LN_NF <- cfa(model_LN_NF, data = briwecs_corrected_LN_NF)

#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_LN_NF, fit.measures = TRUE, standardized=T,rsquare=T)

#########################################################################

# Let us check for on ly yield component

model_HN_NF <-'
Grain_yield~ Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_NF <- cfa(model_HN_NF, data = briwecs_corrected_HN_NF)

# by sem

#results3<-sem(model2,data=raw,se="bootstrap",bootstrap=100)
fit_HN_NF_sem<-sem(model_HN_NF,data=briwecs_corrected_HN_NF) # Without se boot strap they yield the same result with cfa()
summary(fit_HN_NF, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_HN_NF_sem)
#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)



# Let us run for HN_WF

model_HN_WF <-'
Grain_yield~ Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_WF <- cfa(model_HN_WF, data = briwecs_corrected_HN_WF)

#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_HN_WF, fit.measures = TRUE, standardized=T,rsquare=T)
##############################################################################################

# Let us run for LN_NF

model_LN_NF <-'
Grain_yield~ Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_LN_NF <- cfa(model_LN_NF, data = briwecs_corrected_LN_NF)

#View the results
#summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

summary(fit_LN_NF, fit.measures = TRUE, standardized=T,rsquare=T)


semPaths(fit_HN_NF, 'std', layout = 'spring')


####################################
# Resume the path analysis with correlations

model_HN_NF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_NF <- cfa(model_HN_NF, sample.cov=cor(briwecs_corrected_HN_NF[,3:16]), sample.nobs=220)

summary(fit_HN_NF, fit.measures = TRUE, standardized=T,rsquare=T)

cor(briwecs_corrected_HN_NF[,3:16])

# For HN_WF

model_HN_WF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_WF <- cfa(model_HN_WF, sample.cov=cor(briwecs_corrected_HN_WF[,3:16]), sample.nobs=220)

summary(fit_HN_WF, fit.measures = TRUE, standardized=T,rsquare=T)

cor(briwecs_corrected_HN_WF[,3:16])


# For LN_NF

model_LN_NF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_LN_NF <- cfa(model_LN_NF, sample.cov=cor(briwecs_corrected_LN_NF[,3:16]), sample.nobs=220)

summary(fit_LN_NF, fit.measures = TRUE, standardized=T,rsquare=T)

cor(briwecs_corrected_LN_NF[,3:16])

# Let us check the Path analysis in older and in newer released genotype 
BriwecsAverage3YearsallRealYear<-read.delim("data/BriwecsAverage3YearsallRealYear.txt")

str(BriwecsAverage3YearsallRealYear)

# With an observation dataframe
# Less than 1980 and more than 2005
BriwecsAverage3YearsallRealYear_1980<-subset(BriwecsAverage3YearsallRealYear,BriwecsAverage3YearsallRealYear$Year_Release>2005)

BriwecsAverage3YearsallRealYear_2005<-subset(BriwecsAverage3YearsallRealYear,BriwecsAverage3YearsallRealYear$Year_Release>2005)

#briwecs_corrected[briwecs_corrected$Treatment%in%"HN_NF",]


model_HN_NF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models
fit_HN_NF <- cfa(model_HN_NF, data=BriwecsAverage3YearsallRealYear_1980[BriwecsAverage3YearsallRealYear_1980$Treatment%in%"HN_NF",])

summary(fit_HN_NF, fit.measures = TRUE, standardized=T,rsquare=T)

# For >2005
fit_HN_NF <- cfa(model_HN_NF, data=BriwecsAverage3YearsallRealYear_2005[BriwecsAverage3YearsallRealYear_2005$Treatment%in%"HN_NF",])
summary(fit_HN_NF, fit.measures = TRUE, standardized=T,rsquare=T)


# For HN_WF

model_HN_WF <-'
Grain_yield~ Heading_date+Spad+Yellow_Rust+PH+Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+
TKW+HI+Biomass_per_m2+Crude_Protein+SC+Sedimentation
'
# Fit the model
# fit <- cfa(model, data = mtcars)
#fitting confirmatory factor analysis models

fit_HN_WF <- cfa(model_HN_WF, sample.cov=cor(briwecs_corrected_HN_WF[,3:16]), sample.nobs=220)

summary(fit_HN_WF, fit.measures = TRUE, standardized=T,rsquare=T)

cor(briwecs_corrected_HN_WF[,3:16])

model_HN_NF_mediate <-'
Grain_yield~ HI+Biomass_per_m2+SC+Year_Release
HI~Spikes_per_m2+Kernels_per_spike+Kernels_per_m2+Year_Release
SC~TKW+Year_Release'

# For 1980
fit_HN_NF_mediate <- cfa(model_HN_NF_mediate, data=BriwecsAverage3YearsallRealYear_1980[BriwecsAverage3YearsallRealYear_1980$Treatment%in%"HN_NF",])

summary(fit_HN_NF_mediate, fit.measures = TRUE, standardized=T,rsquare=T)

# For >2005
fit_HN_NF_mediate <- cfa(model_HN_NF_mediate, data=BriwecsAverage3YearsallRealYear_2005[BriwecsAverage3YearsallRealYear_2005$Treatment%in%"HN_NF",])
summary(fit_HN_NF_mediate, fit.measures = TRUE, standardized=T,rsquare=T)


