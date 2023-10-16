# Compared the correlation coefficients (test the statistical difference beween two correlations from independant samples)
# Testing independent correlations: r12 is di???erent from r34
#https://www.rdocumentation.org/packages/psych/versions/1.9.12.31/topics/r.test

install.packages("psych")
library(psych)

# r.test(r12 = 0.25, r34 = 0.5, n= 100)
#r.test(rhnnf = 0.16, rhnwf = 0.02, n= 220)# doesn´t work because doesn´t use rhnnf...

# For spikes per meter square

#HN-NF and HN-WF
r.test(r12 = 0.16, r34 = 0.02, n= 220) #test the difference between two independent correlations
# Correlation tests 
# Call:r.test(n = 220, r12 = 0.16, r34 = 0.02)
# Test of difference between two independent correlations 
# z value 1.47    with probability  0.14
# r.test(r12 = 0.16, r34 = 0.2, n= 220)

#HN-NF and LN-NF
r.test(r12 = 0.16, r34 = 0.2, n= 220)
# Correlation tests 
# Call:r.test(n = 220, r12 = 0.16, r34 = 0.2)
# Test of difference between two independent correlations 
# z value 0.43    with probability  0.67

#HN-WF and LN-NF
r.test(r12 = 0.02, r34 = 0.2, n= 220)
# Correlation tests 
# Call:r.test(n = 220, r12 = 0.16, r34 = 0.2)
# Test of difference between two independent correlations 
# z value 0.43    with probability  0.67

####################################

# For Kernels per Spikes

#HN-NF and HN-WF
r.test(r12 = 0.44, r34 = 0.23, n= 220) #test the difference between two independent correlations

#HN-NF and LN-NF
r.test(r12 = 0.44, r34 = 0.38, n= 220)


#HN-WF and LN-NF
r.test(r12 = 0.23, r34 = 0.38, n= 220)

#####################################

# For Kernels per Spikes

#HN-NF and HN-WF
r.test(r12 = 0.55, r34 = 0.36, n= 220)

#HN-NF and LN-NF
r.test(r12 = 0.55, r34 = 0.56, n= 220)


#HN-WF and LN-NF
r.test(r12 = 0.36, r34 = 0.56, n= 220)

#####################################

# For TKW

#HN-NF and HN-WF
r.test(r12 = 0.42, r34 = 0.1, n= 220)

#HN-NF and LN-NF
r.test(r12 = 0.42, r34 = 0.4, n= 220)


#HN-WF and LN-NF
r.test(r12 = 0.1, r34 = 0.4, n= 220)

#####################################

# For Harvest index HI

#HN-NF and HN-WF
r.test(r12 = 0.78, r34 = 0.7, n= 220)

#HN-NF and LN-NF
r.test(r12 = 0.78, r34 = 0.67, n= 220)


#HN-WF and LN-NF
r.test(r12 = 0.7, r34 = 0.67, n= 220)

#####################################

# For Harvest index Biomass per m2

#HN-NF and HN-WF
r.test(r12 = 0.74, r34 = 0.21, n= 220)

#HN-NF and LN-NF
r.test(r12 = 0.74, r34 = 0.53, n= 220)


#HN-WF and LN-NF
r.test(r12 = 0.21, r34 = 0.53, n= 220)



