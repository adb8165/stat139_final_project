# First Look at the Property Assessment Data
# 4 Apr 2016

# Load data
propdata = read.csv('Property_Assessment_2014.csv')

# See column names
names(propdata)

# Plot some features
# Histogram of Total Assessment Value
hist(propdata$AV_TOTAL,breaks = 100)

# a very complexing distribution of years buildings were built...
hist(propdata$YR_BUILT,breaks = 100)

# Histogram of Number of Floors
hist(propdata$NUM_FLOORS,breaks = 100)

# Histogram of Number of Rooms
hist(propdata$R_TOTAL_RMS,breaks = 100)