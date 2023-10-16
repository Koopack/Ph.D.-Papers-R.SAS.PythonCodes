# Comparison of Regression coefficients of preeding progress

dim(BriwecsAverage3YearsallRealYear)
summary(BriwecsAverage3YearsallRealYear)
# for Heading_date

Reg_Heading_date_CS123<-lm(Heading_date ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Heading_date_CS123)
Reg_Heading_date_CS23<-lm(Heading_date ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Heading_date_CS23)

# For Spad

Reg_Spad_CS123<-lm(Spad ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Spad_CS123)
Reg_Spad_CS23<-lm(Spad ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Spad_CS23)


# For Yellow_Rust
Reg_Yellow_Rust_CS123<-lm(Yellow_Rust ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Yellow_Rust_CS123)
Reg_Yellow_Rust_CS23<-lm(Yellow_Rust ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Yellow_Rust_CS23)

# For PH
Reg_PH_CS123<-lm(PH ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_PH_CS123)
Reg_PH_CS23<-lm(PH ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_PH_CS23)

# For Spikes_per_m2
Reg_Spikes_per_m2_CS123<-lm(Spikes_per_m2 ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Spikes_per_m2_CS123)
Reg_Spikes_per_m2_CS23<-lm(Spikes_per_m2 ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Spikes_per_m2_CS23)

# For Kernels_per_spike
Reg_Kernels_per_spike_CS123<-lm(Kernels_per_spike ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Kernels_per_spike_CS123)
Reg_Kernels_per_spike_CS23<-lm(Kernels_per_spike ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Kernels_per_spike_CS23)

# For Kernels_per_m2
Reg_Kernels_per_m2_CS123<-lm(Kernels_per_m2 ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Kernels_per_m2_CS123)
Reg_Kernels_per_m2_CS23<-lm(Kernels_per_m2 ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Kernels_per_m2_CS23)

# For TKW
Reg_TKW_CS123<-lm(TKW ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_TKW_CS123)
Reg_TKW_CS23<-lm(TKW ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_TKW_CS23)

# For HI
Reg_HI_CS123<-lm(HI ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_HI_CS123)
Reg_HI_CS23<-lm(HI ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_HI_CS23)

# For Biomass_per_m2
Reg_Biomass_per_m2_CS123<-lm(Biomass_per_m2 ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Biomass_per_m2_CS123)
Reg_Biomass_per_m2_CS23<-lm(Biomass_per_m2 ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Biomass_per_m2_CS23)

# For Grain_yield
Reg_Grain_yield_CS123<-lm(Grain_yield ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Grain_yield_CS123)
Reg_Grain_yield_CS23<-lm(Grain_yield ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Grain_yield_CS23)

# For Crude_Protein
Reg_Crude_Protein_CS123<-lm(Crude_Protein ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Crude_Protein_CS123)
Reg_Crude_Protein_CS23<-lm(Crude_Protein ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Crude_Protein_CS23)

# For SC
Reg_SC_CS123<-lm(SC ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_SC_CS123)
Reg_SC_CS23<-lm(SC ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_SC_CS23)

# For Sedimentation
Reg_Sedimentation_CS123<-lm(Sedimentation ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear)
summary(Reg_Sedimentation_CS123)
Reg_Sedimentation_CS23<-lm(Sedimentation ~ Year_Release * Treatment, data = BriwecsAverage3YearsallRealYear[221:660,])
summary(Reg_Sedimentation_CS23)



# How is the breeding progress evoluate?
head(BriwecsAverage3YearsallRealYear,5)

BriwecsAverage3YearsallRealYear_old<-subset(BriwecsAverage3YearsallRealYear,BriwecsAverage3YearsallRealYear$Year_Release<2000)
head(BriwecsAverage3YearsallRealYear_old,5)
BriwecsAverage3YearsallRealYear_old$OldNew<-rep("Old", nrow(BriwecsAverage3YearsallRealYear_old))

BriwecsAverage3YearsallRealYear_new<-subset(BriwecsAverage3YearsallRealYear,BriwecsAverage3YearsallRealYear$Year_Release>=2000)
head(BriwecsAverage3YearsallRealYear_new,5)
BriwecsAverage3YearsallRealYear_new$OldNew<-rep("new", nrow(BriwecsAverage3YearsallRealYear_new))

BriwecsAverage3YearsallRealYear_ON<-rbind(BriwecsAverage3YearsallRealYear_old,BriwecsAverage3YearsallRealYear_new)
dim(BriwecsAverage3YearsallRealYear_ON)

str(BriwecsAverage3YearsallRealYear_ON$BRISONr)

# Let us now compare the speed of the bredding progress under each Cs.

#For GY

# all Data
Reg_Grain_yield_SlopComp_ON<-lm(Grain_yield ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON)
summary(Reg_Grain_yield_SlopComp_ON)

#HN_NF
Reg_Grain_yield_SlopComp_ON_HN_NF<-lm(Grain_yield ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_NF",])
summary(Reg_Grain_yield_SlopComp_ON_HN_NF)

#HN_WF
Reg_Grain_yield_SlopComp_ON_HN_WF<-lm(Grain_yield ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_WF",])
summary(Reg_Grain_yield_SlopComp_ON_HN_WF)


#For Heading_date
# all Data
Reg_Heading_date_SlopComp_ON<-lm(Heading_date ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON)
summary(Reg_Heading_date_SlopComp_ON)

#HN_NF
Reg_Heading_date_SlopComp_ON_HN_NF<-lm(Heading_date ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_NF",])
summary(Reg_Heading_date_SlopComp_ON_HN_NF)

#HN_WF
Reg_Heading_date_SlopComp_ON_HN_WF<-lm(Heading_date ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_WF",])
summary(Reg_Heading_date_SlopComp_ON_HN_WF)


#For Spad
# all Data
Reg_Spad_SlopComp_ON<-lm(Spad ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON)
summary(Reg_Spad_SlopComp_ON)

#HN_NF
Reg_Spad_SlopComp_ON_HN_NF<-lm(Spad ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_NF",])
summary(Reg_Spad_SlopComp_ON_HN_NF)

#HN_WF
Reg_Spad_SlopComp_ON_HN_WF<-lm(Spad ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_WF",])
summary(Reg_Spad_SlopComp_ON_HN_WF)


#For Yellow_Rust
# all Data
Reg_Yellow_Rust_SlopComp_ON<-lm(Yellow_Rust ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON)
summary(Reg_Yellow_Rust_SlopComp_ON)

#HN_NF
Reg_Yellow_Rust_SlopComp_ON_HN_NF<-lm(Yellow_Rust ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_NF",])
summary(Reg_Yellow_Rust_SlopComp_ON_HN_NF)

#HN_WF
Reg_Yellow_Rust_SlopComp_ON_HN_WF<-lm(Yellow_Rust ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_WF",])
summary(Reg_Yellow_Rust_SlopComp_ON_HN_WF)

#For Biomass_per_m2
# all Data
Reg_Biomass_per_m2_SlopComp_ON<-lm(Biomass_per_m2 ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON)
summary(Reg_Biomass_per_m2_SlopComp_ON)

#HN_NF
Reg_Biomass_per_m2_SlopComp_ON_HN_NF<-lm(Biomass_per_m2 ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_NF",])
summary(Reg_Biomass_per_m2_SlopComp_ON_HN_NF)

#HN_WF
Reg_Biomass_per_m2_SlopComp_ON_HN_WF<-lm(Biomass_per_m2 ~ Year_Release * OldNew, data = BriwecsAverage3YearsallRealYear_ON[BriwecsAverage3YearsallRealYear_ON$Treatment%in%"HN_WF",])
summary(Reg_Biomass_per_m2_SlopComp_ON_HN_WF)

plot(BriwecsAverage3YearsallRealYear_old$Biomass_per_m2 ~ BriwecsAverage3YearsallRealYear_old$Year_Release)
abline(Reg_Biomass_per_m2_SlopComp_ON_HN_NF)

write.table(BriwecsAverage3YearsallRealYear_ON, "Data/BriwecsAverage3YearsallRealYear_ON.txt", sep = "\t", quote = FALSE,  row.names = FALSE)
