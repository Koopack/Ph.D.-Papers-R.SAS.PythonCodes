
# Date : 21st March 2021 
# Ap4 stom cond kinetic dynamic analysis

DataForKineticDynamicAnalysis <- read.delim("Data/DataForKineticDynamicAnalysis.txt")

head(DataForKineticDynamicAnalysis,5)

levels(DataForKineticDynamicAnalysis$BBCH_Range)
str(DataForKineticDynamicAnalysis)

DataForKineticDynamicAnalysis$BBCH_Range <- as.factor(DataForKineticDynamicAnalysis$BBCH_Range)

DataForKineticDynamicAnalysis_Ds <- subset(DataForKineticDynamicAnalysis, TreatEnv = "D_stress")
DataForKineticDynamicAnalysis_K  <- subset(DataForKineticDynamicAnalysis, TreatEnv = "Control")

str(DataForKineticDynamicAnalysis_Ds)

# to knit

xfun::write_utf8()
