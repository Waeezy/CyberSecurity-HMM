# TERM PROJECT REDO - GROUP 6
#Authors:
#Wael Yakoub Agha - 301348817
#Rabie Ali - 301320609
#Sam Gouneili - 301306477
#Giovanni Hosang - 301295511 
#Russell Wong - 301310883
#Jack Wright - 301315736

#Packages 
library(dplyr)
library(ggplot2)
library(corrplot)

setwd("WORKING_DIRECTORY")

path <- file.path(getwd(), "TrainData.txt")
tbl <- read.table(path, header = TRUE, sep = ",", quote = "\"", dec = ".")

# ~~~~~ PART 1 FORMATTING LOGIC ~~~~~

# Will be using the "mice" library (Multivariate Imputation via Chained Equations) to conduct imputations
# on our dataset and fill in the NA values with plausible results. 

## Fix the dates fortmat
tbl$Date <- as.POSIXlt(tbl$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
tbl$Time <- strptime(tbl$Time, format="%H:%M:%S")
tbl$Time <- format(tbl$Time, "%H:%M:%S")
tbl$Time <- as.list(tbl$Time)

# ~~~ DEBUG ~~~
# Sample 1 year so this runs faster 
#tbl <- subset(tbl, tbl$Date >= "2006-01-01" & tbl$Date < "2006-12-31")

# Check # of NA 
numNA <- is.na(tbl)
sum(numNA) # 50112
mean(numNA) # ~ 0.36% of data set is NA 

# Check # of NA of all Cols 
sapply(tbl, function(x) sum(is.na(x)))
# Selection of ALL attributes EXCEPT 'Date' and 'Time' 
# Was giving issues when computing imputations 
temp_tbl<- select(tbl, Global_active_power, Global_reactive_power, Voltage, Global_intensity, Sub_metering_1, Sub_metering_2,Sub_metering_3)

# ~~~ DEBUG ~~~
# sapply(dat, function(x) sum(is.na(x)))

# Imputation section, initialize method and predictor matrix. 
init = mice(temp_tbl, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# Set which Imputation method to use. Options: 
# - "pmm" : Used for numeric Data
# - "logreg" : used for binary data i.e. "Yes, No"
# - "polyreg" : non-numeric data i.e. "High, medium, low"
# - "polr" 
meth"Global_active_power","Global_reactive_power", "Voltage","Global_intensity")]= "pmm" 
meth[c("Sub_metering_1","Sub_metering_2", "Sub_metering_3")]= "pmm"

# Set RNG seed to get same rand number
set.seed(420)
# m=5 is how many Candidate donors to get. Of which MICE will choose a candidate from set to use 
imputed = mice(temp_tbl, method=meth, predictorMatrix=predM, m=5)
imputed <- complete(imputed)

# ~~~ DEBUG ~~~ 
# Shows # NA vals in dataset. 
# EXPECTED TO RETURN 0 FOR ALL COLUMNS
sapply(imputed, function(x) sum(is.na(x)))


# Add back Date and Time to Imputed table
times<- select(tbl, Date, Time)
tbl_f <- data.frame(times, imputed$Global_active_power, imputed$Global_reactive_power, imputed$Voltage, imputed$Global_intensity, imputed$Sub_metering_1, imputed$Sub_metering_2, imputed$Sub_metering_3)

# Unlist elements to prep for export 
tmp<- tbl_f
tmp$Time <- unlist(tmp$Time)
tmp$Date <- as.character(tmp$Date)


# ~~~ DEBUG ~~~
is.list(tmp$Date)
is.list(tmp$Time)
is.list(tmp$Global_active_power)
is.list(tmp$Global_reactive_power)
is.list(tmp$Voltage)
is.list(tmp$Global_intensity)
is.list(tmp$Sub_metering_1)
       
# EXPORT DATA 
write.table(tmp,"FixedData.txt", sep=",", row.names =  FALSE)
# ~~~~~ Null values removed ~~~~~ #
       
       
## ~~~ Finding Correlation and plotting ~~~ #       
## Read Data ##
path <- file.path(getwd(), "FixedData.txt")
tbl_f <- read.table(path, header = TRUE, sep = ",", quote = "\"", dec = ".")

## Fix the dates fortmat
tbl_f$Date <- strptime(tbl_f$Date, format="%Y-%m-%d")
tbl_f$Date <- as.POSIXlt(tbl_f$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
tbl_f$Time <- strptime(tbl_f$Time, format="%H:%M:%S")
tbl_f$Time <- format(tbl_f$Time, "%H:%M:%S")
tbl_f$Time <- as.list(tbl_f$Time)
       
#####
## PART 1 ##
###

# correlation coefficients for A
corAB <- cor(tbl_f$imputed.Global_active_power, tbl_f$imputed.Global_reactive_power, method="pearson")
corAC <- cor(tbl_f$imputed.Global_active_power, tbl_f$imputed.Voltage, method="pearson")
corAD <- cor(tbl_f$imputed.Global_active_power, tbl_f$imputed.Global_intensity, method="pearson")
corAE <- cor(tbl_f$imputed.Global_active_power, tbl_f$imputed.Sub_metering_1, method="pearson")
corAF <- cor(tbl_f$imputed.Global_active_power, tbl_f$imputed.Sub_metering_2, method="pearson")
corAG <- cor(tbl_f$imputed.Global_active_power, tbl_f$imputed.Sub_metering_3, method="pearson")

# correlation coefficients for B
corBC <- cor(tbl_f$imputed.Global_reactive_power, tbl_f$imputed.Voltage, method="pearson")
corBD <- cor(tbl_f$imputed.Global_reactive_power, tbl_f$imputed.Global_intensity, method="pearson")
corBE <- cor(tbl_f$imputed.Global_reactive_power, tbl_f$imputed.Sub_metering_1, method="pearson")
corBF <- cor(tbl_f$imputed.Global_reactive_power, tbl_f$imputed.Sub_metering_2, method="pearson")
corBG <- cor(tbl_f$imputed.Global_reactive_power, tbl_f$imputed.Sub_metering_3, method="pearson")

# correlation coefficients for C
corCD <- cor(tbl_f$imputed.Voltage, tbl_f$imputed.Global_intensity, method="pearson")
corCE <- cor(tbl_f$imputed.Voltage, tbl_f$imputed.Sub_metering_1, method="pearson")
corCF <- cor(tbl_f$imputed.Voltage, tbl_f$imputed.Sub_metering_2, method="pearson")
corCG <- cor(tbl_f$imputed.Voltage, tbl_f$imputed.Sub_metering_3, method="pearson")

# correlation coefficients for D
corDE <- cor(tbl_f$imputed.Global_intensity, tbl_f$imputed.Sub_metering_1 , method="pearson")
corDF <- cor(tbl_f$imputed.Global_intensity, tbl_f$imputed.Sub_metering_2 , method="pearson")
corDG <- cor(tbl_f$imputed.Global_intensity, tbl_f$imputed.Sub_metering_3 , method="pearson")

# correlation coefficients for E
corEF <- cor(tbl_f$imputed.Sub_metering_1, tbl_f$imputed.Sub_metering_2, method="pearson")
corEG <- cor(tbl_f$imputed.Sub_metering_1, tbl_f$imputed.Sub_metering_3, method="pearson")

# correlation coefficients for F
corFG <- cor(tbl_f$imputed.Sub_metering_2, tbl_f$imputed.Sub_metering_3, method="pearson")

# Loading the matrices
rownames <- c("Gloabl_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2","Sub_metering_3")
colnames <- c("Gloabl_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2","Sub_metering_3")

A <- c(1, corAB, corAC, corAD, corAE, corAF, corAG)
B <- c(0, 1, corBC, corBD, corBE, corBF, corBG)
C <- c(0, 0, 1, corCD, corCE, corCF, corCG)
D <- c(0, 0, 0, 1, corDE, corDF, corDG)
E <- c(0, 0, 0, 0, 1, corEF, corEG)
F <- c(0, 0, 0, 0, 0, 1, corFG)
G <- c(0, 0, 0, 0, 0, 0, 1) 

daytah <- structure(list(A,B,C,D,E,F,G), .Names = colnames, row.names = rownames, class = "data.frame")
corrplot::corrplot(as.matrix(daytah), method = 'color', addCoef.col = "grey")

#####################################################################################

## weekend data

#2006-12-17, Sunday
Global_Intensity_wkd <- tbl_f$imputed.Global_intensity[c(397:1836)]
rec_pwr_wkd <- tbl_f$imputed.Global_reactive_power[c(397:1836)]
ac_pwr_wkd <- tbl_f$imputed.Global_active_power[c(397:1836)]
vltg_wkd <- tbl_f$imputed.Voltage[c(397:1836)]
subm1_wkd <- tbl_f$imputed.Sub_metering_1[c(397:1836)]
subm2_wkd <- tbl_f$imputed.Sub_metering_2[c(397:1836)]
subm3_wkd <- tbl_f$imputed.Sub_metering_3[c(397:1836)]

#2006-12-23, Saturday
glb_int2_wkd <- tbl_f$imputed.Global_intensity[c(9037:10476)]
rec_pwr2_wkd <- tbl_f$imputed.Global_reactive_power[c(9037:10476)]
ac_pwr2_wkd <- tbl_f$imputed.Global_active_power[c(9037:10476)]
vltg2_wkd <- tbl_f$imputed.Voltage[c(9037:10476)]
subm1b_wkd <- tbl_f$imputed.Sub_metering_1[c(9037:10476)]
subm2b_wkd <- tbl_f$imputed.Sub_metering_2[c(9037:10476)]
subm3b_wkd <- tbl_f$imputed.Sub_metering_3[c(9037:10476)]

#2006-12-24, Sunday
glb_int3_wkd <- tbl_f$imputed.Global_intensity[c(10476:11915)]
rec_pwr3_wkd <- tbl_f$imputed.Global_reactive_power[c(10476:11915)]
ac_pwr3_wkd <- tbl_f$imputed.Global_active_power[c(10476:11915)]
vltg3_wkd <- tbl_f$imputed.Voltage[c(10476:11915)]
subm1c_wkd <- tbl_f$imputed.Sub_metering_1[c(10476:11915)]
subm2c_wkd <- tbl_f$imputed.Sub_metering_2[c(10476:11915)]
subm3c_wkd <- tbl_f$imputed.Sub_metering_3[c(10476:11915)]

#plotting Global_Intensity weekends
plot(Global_Intensity_wkd, type="l", col="blue")
lines(glb_int2_wkd, type="l", col="red")
lines(glb_int3_wkd, type="l", col="yellow")

#Plotting all the features on the same graph (weekend)
par(mfrow=c(2,4))
plot(Global_Intensity_wkd[c(200:400)], type="l", col="blue")
plot(rec_pwr_wkd[c(200:400)], type="l", col="red")
plot(ac_pwr_wkd[c(200:400)], type="l", col="black")
plot(vltg_wkd[c(200:400)], type="l", col="green")
plot(subm1_wkd[c(200:400)], type="l", col="orange")
plot(subm2_wkd[c(200:400)], type="l", col="purple")
plot(subm3_wkd[c(200:400)], type="l", col="grey")

#####################################################################################

## weekdays data

#2006-12-18, Monday to 2006-12-22, Friday
#Global Intensity
Global_Intensity_wk <- tbl_f$imputed.Global_intensity[c(1836:3275)] #Mon
glb_int2_wk <- tbl_f$imputed.Global_intensity[c(3275:4714)]					#Tue
glb_int3_wk <- tbl_f$imputed.Global_intensity[c(4714:6153)]					#Wed
glb_int4_wk <- tbl_f$imputed.Global_intensity[c(6153:7592)]					#Thu
glb_int5_wk <- tbl_f$imputed.Global_intensity[c(7592:9031)]					#Fri

#Global Reactive Power
rec_pwr_wk <- tbl_f$imputed.Global_reactive_power[c(1836:3275)]           #Mon
rec_pwr2_wk <- tbl_f$imputed.Global_reactive_power[c(3275:4714)]					#Tue
rec_pwr3_wk <- tbl_f$imputed.Global_reactive_power[c(4714:6153)]					#Wed
rec_pwr4_wk <- tbl_f$imputed.Global_reactive_power[c(6153:7592)]					#Thu
rec_pwr5_wk <- tbl_f$imputed.Global_reactive_power[c(7592:9031)]					#Fri

#Global Active Power
ac_pwr_wk <- tbl_f$imputed.Global_active_power[c(1836:3275)]          #Mon
ac_pwr2_wk <- tbl_f$imputed.Global_active_power[c(3275:4714)]					#Tue
ac_pwr3_wk <- tbl_f$imputed.Global_active_power[c(4714:6153)]					#Wed
ac_pwr4_wk <- tbl_f$imputed.Global_active_power[c(6153:7592)]					#Thu
ac_pwr5_wk <- tbl_f$imputed.Global_active_power[c(7592:9031)]					#Fri

#Voltage
vltg_wk <- tbl_f$imputed.Voltage[c(1836:3275)]          #Mon
vltg2_wk <- tbl_f$imputed.Voltage[c(3275:4714)]					#Tue
vltg3_wk <- tbl_f$imputed.Voltage[c(4714:6153)]					#Wed
vltg4_wk <- tbl_f$imputed.Voltage[c(6153:7592)]					#Thu
vltg5_wk <- tbl_f$imputed.Voltage[c(7592:9031)]					#Fri

#Submetering 1
subm1_wk <- tbl_f$imputed.Sub_metering_1[c(1836:3275)]          #Mon
subm1b_wk <- tbl_f$imputed.Sub_metering_1[c(3275:4714)]					#Tue
subm1c_wk <- tbl_f$imputed.Sub_metering_1[c(4714:6153)]					#Wed
subm1d_wk <- tbl_f$imputed.Sub_metering_1[c(6153:7592)]					#Thu
subm1e_wk <- tbl_f$imputed.Sub_metering_1[c(7592:9031)]					#Fri

#Submetering2
subm2_wk <- tbl_f$imputed.Sub_metering_2[c(1836:3275)]          #Mon
subm2b_wk <- tbl_f$imputed.Sub_metering_2[c(3275:4714)]					#Tue
subm2c_wk <- tbl_f$imputed.Sub_metering_2[c(4714:6153)]					#Wed
subm2d_wk <- tbl_f$imputed.Sub_metering_2[c(6153:7592)]					#Thu
subm2e_wk <- tbl_f$imputed.Sub_metering_2[c(7592:9031)]					#Fri

#Submetering 3
subm3_wk <- tbl_f$imputed.Sub_metering_3[c(1836:3275)]          #Mon
subm3b_wk <- tbl_f$imputed.Sub_metering_3[c(3275:4714)]					#Tue
subm3c_wk <- tbl_f$imputed.Sub_metering_3[c(4714:6153)]					#Wed
subm3d_wk <- tbl_f$imputed.Sub_metering_3[c(6153:7592)]					#Thu
subm3e_wk <- tbl_f$imputed.Sub_metering_3[c(7592:9031)]					#Fri

       
#Plotting Global_Intensity weekdays
plot(Global_Intensity_wk, type="l", col="blue")
lines(glb_int2_wk, type="l", col="red")
lines(glb_int3_wk, type="l", col="black")
lines(glb_int4_wk, type="l", col="green")
lines(glb_int5_wk, type="l", col="orange")
       
#Plotting all the features on the same graph (weekday)
par(mfrow=c(2,4))
plot(Global_Intensity_wk[c(200:400)], type="l", col="blue")
plot(rec_pwr_wk[c(200:400)], type="l", col="red")
plot(ac_pwr_wk[c(200:400)], type="l", col="black")
plot(vltg_wk[c(200:400)], type="l", col="green")
plot(subm1_wk[c(200:400)], type="l", col="orange")
plot(subm2_wk[c(200:400)], type="l", col="purple")
plot(subm3_wk[c(200:400)], type="l", col="grey")


####
## PART 2 ##
####

#Mean Weekend
Global_Intensity_wkd_mean <- mean(Global_Intensity_wkd)
glb_int2_wkd_mean <- mean(glb_int2_wkd)
glb_int3_wkd_mean <- mean(glb_int3_wkd)

rec_pwr_wkd_mean <- mean(rec_pwr_wkd)
rec_pwr2_wkd_mean <- mean(rec_pwr2_wkd)
rec_pwr3_wkd_mean <- mean(rec_pwr3_wkd)

ac_pwr_wkd_mean <- mean(ac_pwr_wkd)
ac_pwr2_wkd_mean <- mean(ac_pwr2_wkd)
ac_pwr3_wkd_mean <- mean(ac_pwr3_wkd)

vltg_wkd_mean  <- mean(vltg_wkd)
vltg2_wkd_mean  <- mean(vltg2_wkd)
vltg3_wkd_mean  <- mean(vltg3_wkd)

subm1_wkd_mean <- mean(subm1_wkd)
subm1b_wkd_mean <- mean(subm1b_wkd)
subm1c_wkd_mean <- mean(subm1c_wkd)

subm2_wkd_mean <- mean(subm2_wkd)
subm2b_wkd_mean <- mean(subm2b_wkd)
subm2c_wkd_mean <- mean(subm2c_wkd)

subm3_wkd_mean <- mean(subm3_wkd)
subm3b_wkd_mean <- mean(subm3b_wkd)
subm3c_wkd_mean <- mean(subm3c_wkd)

#Mean Weekday
Global_Intensity_wk_mean <- mean(Global_Intensity_wk)
glb_int2_wk_mean <- mean(glb_int2_wk)
glb_int3_wk_mean <- mean(glb_int3_wk)
glb_int4_wk_mean <- mean(glb_int4_wk)
glb_int5_wk_mean <- mean(glb_int4_wk)

rec_pwr_wk_mean <- mean(rec_pwr_wk)
rec_pwr2_wk_mean <- mean(rec_pwr2_wk)
rec_pwr3_wk_mean <- mean(rec_pwr3_wk)
rec_pwr4_wk_mean <- mean(rec_pwr4_wk)
rec_pwr5_wk_mean <- mean(rec_pwr5_wk)

ac_pwr_wk_mean <- mean(ac_pwr_wk)
ac_pwr2_wk_mean <- mean(ac_pwr2_wk)
ac_pwr3_wk_mean <- mean(ac_pwr3_wk)
ac_pwr4_wk_mean <- mean(ac_pwr4_wk)
ac_pwr5_wk_mean <- mean(ac_pwr5_wk)

vltg_wk_mean  <- mean(vltg_wk)
vltg2_wk_mean  <- mean(vltg2_wk)
vltg3_wk_mean  <- mean(vltg3_wk)
vltg4_wk_mean  <- mean(vltg4_wk)
vltg5_wk_mean  <- mean(vltg5_wk)

subm1_wk_mean <- mean(subm1_wk)
subm1b_wk_mean <- mean(subm1b_wk)
subm1c_wk_mean <- mean(subm1c_wk)
subm1d_wk_mean <- mean(subm1d_wk)
subm1e_wk_mean <- mean(subm1e_wk)

subm2_wk_mean <- mean(subm2_wk)
subm2b_wk_mean <- mean(subm2b_wk)
subm2c_wk_mean <- mean(subm2c_wk)
subm2d_wk_mean <- mean(subm2d_wk)
subm2e_wk_mean <- mean(subm2e_wk)

subm3_wk_mean <- mean(subm3_wk)
subm3b_wk_mean <- mean(subm3b_wk)
subm3c_wk_mean <- mean(subm3c_wk)
subm3d_wk_mean <- mean(subm3d_wk)
subm3e_wk_mean <- mean(subm3e_wk)

# #Print Mean
# #Sun
# Global_Intensity_wkd_mean
# rec_pwr_wkd_mean
# ac_pwr_wkd_mean
# vltg_wkd_mean
# subm1_wkd_mean
# subm2_wkd_mean
# subm3_wkd_mean
# 
# #Mon
# Global_Intensity_wk_mean
# rec_pwr_wk_mean
# ac_pwr_wk_mean
# vltg_wk_mean
# subm1_wk_mean
# subm2_wk_mean
# subm3_wk_mean
# 
# #Tue
# glb_int2_wk_mean
# rec_pwr2_wk_mean
# ac_pwr2_wk_mean
# vltg2_wk_mean
# subm1b_wk_mean
# subm2b_wk_mean
# subm3b_wk_mean
# 
# #Wed
# glb_int3_wk_mean
# rec_pwr3_wk_mean
# ac_pwr3_wk_mean
# vltg3_wk_mean
# subm1c_wk_mean
# subm2c_wk_mean
# subm3c_wk_mean
# 
# #Thu
# glb_int4_wk_mean
# rec_pwr4_wk_mean
# ac_pwr4_wk_mean
# vltg4_wk_mean
# subm1d_wk_mean
# subm2d_wk_mean
# subm3d_wk_mean
# 
# #Fri
# glb_int5_wk_mean
# rec_pwr5_wk_mean
# ac_pwr5_wk_mean
# vltg5_wk_mean
# subm1e_wk_mean
# subm2e_wk_mean
# subm3e_wk_mean
# 
# #Sat
# glb_int2_wkd_mean
# rec_pwr2_wkd_mean
# ac_pwr2_wkd_mean
# vltg2_wkd_mean
# subm1b_wkd_mean
# subm2b_wkd_mean
# subm3b_wkd_mean
# 
# #Sun2
# glb_int3_wkd_mean
# rec_pwr3_wkd_mean
# ac_pwr3_wkd_mean
# vltg3_wkd_mean
# subm1c_wkd_mean
# subm2c_wkd_mean
# subm3c_wkd_mean


install.packages("devtools")
library(devtools) 
install_github("vqv/ggbiplot")
library(ggbiplot)

## Principal Component Analysis

#Preparing columns for PCA computation
colnames(tbl_f) <- c("Date",
                     "Time",
                     "Global_active_power",
                     "Global_reactive_power",
                     "Voltage",
                     "Global_intensity",
                     "Sub_metering_1",
                     "Sub_metering_2",
                     "Sub_metering_3")


#Computing PCA
tbl_f.pca <- prcomp(tbl_f[c("Global_active_power",
                            "Global_reactive_power",
                            "Voltage",
                            "Global_intensity",
                            "Sub_metering_1",
                            "Sub_metering_2",
                            "Sub_metering_3")], center = TRUE, scale = TRUE)

summary(tbl_f.pca)
str(tbl_f.pca)


##Plotting PCA

#takes a long time. Don't run it if you don't have to. 
#Look at the reports for graphs

##NOTE: If getting "invalid graphics state" error enter this command:
## > dev.off()
ggbiplot(tbl_f.pca)

ggbiplot(tbl_f.pca, labels=rownames(tbl_f))

ggbiplot(tbl_f.pca,ellipse=TRUE)

ggbiplot(tbl_f.pca,choices=c(1,3))
ggbiplot(tbl_f.pca,choices=c(1,4))
ggbiplot(tbl_f.pca,choices=c(1,5))
ggbiplot(tbl_f.pca,choices=c(1,6))
ggbiplot(tbl_f.pca,choices=c(1,7))

ggbiplot(tbl_f.pca,choices=c(2,3))
ggbiplot(tbl_f.pca,choices=c(2,4))
ggbiplot(tbl_f.pca,choices=c(2,5))
ggbiplot(tbl_f.pca,choices=c(2,6))
ggbiplot(tbl_f.pca,choices=c(2,7))

ggbiplot(tbl_f.pca,choices=c(3,4))
ggbiplot(tbl_f.pca,choices=c(3,5))
ggbiplot(tbl_f.pca,choices=c(3,6))
ggbiplot(tbl_f.pca,choices=c(3,7))

ggbiplot(tbl_f.pca,choices=c(4,5))
ggbiplot(tbl_f.pca,choices=c(4,6))
ggbiplot(tbl_f.pca,choices=c(4,7))

ggbiplot(tbl_f.pca,choices=c(5,6))
ggbiplot(tbl_f.pca,choices=c(5,7))

ggbiplot(tbl_f.pca,choices=c(6,7))

####### End of PART 2 #############
       
#################################################
       
       # Jack's Part Training and Testing
       
#################################################

path <- file.path(getwd(), "FixedData.txt")
tbl <- read.table(path, header = TRUE, sep = ",", quote = "\"", dec = ".")

# Line 21998 Start Jan 01 2007
# Line 1074638 end Dec 31 2008

#Seperate the data set into Training (2 years, Jan 2007 - Dec 2008) and Test (1 year, Jan 2009 - Dec 2009). The indexes represent the years.
#Use only trainData without the window for original results.

trainData <- tbl[21998:1074636, ]
trainDatatmp$Time <- strptime(trainDatatmp$Time, format="%H:%M:%S")
trainDatatmp$Time <- format(trainDatatmp$Time, "%H:%M:%S")
trainDatatmp$Time <- as.list(trainDatatmp$Time)

trainData <- subset(trainDatatmp, (trainDatatmp$Time < "15:00:00" & trainDatatmp$Time > "12:00:00"))


testData <- tbl[1074637:1556445, ]
testData$Time <- strptime(testData$Time, format="%H:%M:%S")
testData$Time <- format(testData$Time, "%H:%M:%S")
testData$Time <- as.list(testData$Time)


testData <- subset(testData, (testData$Time < "15:00:00" & testData$Time > "12:00:00"))


#Create a depmixS4 model using different numbers of states. We will create 2 models that are univariate and 2 that are multivariate based on PCA data from part 3.

#The feature used is Global Intensity worked up until 10

TrainingModel4INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 4, family=gaussian())
FitTrain4INT <- fit(TrainingModel4INT)

TrainingModel5INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 5, family=gaussian())
FitTrain5INT <- fit(TrainingModel5INT)

TrainingModel6INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 6, family=gaussian())
FitTrain6INT <- fit(TrainingModel6INT)

TrainingModel7INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 7, family=gaussian())
FitTrain7INT <- fit(TrainingModel7INT)

TrainingModel8INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 8, family=gaussian())
FitTrain8INT <- fit(TrainingModel8INT)

TrainingModel9INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 9, family=gaussian())
FitTrain9INT <- fit(TrainingModel9INT)

TrainingModel10INT <- depmix(response = imputed.Global_intensity~1,data = trainData, nstates = 10, family=gaussian())
FitTrain10INT <- fit(TrainingModel10INT)

#The feature used is Global Active Power

TrainingModel4GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 4, family=gaussian())
FitTrain4GAP <- fit(TrainingModel4GAP)

TrainingModel5GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 5, family=gaussian())
FitTrain5GAP <- fit(TrainingModel5GAP)

TrainingModel6GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 6, family=gaussian())
FitTrain6GAP <- fit(TrainingModel6GAP)

TrainingModel7GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 7, family=gaussian())
FitTrain7GAP <- fit(TrainingModel7GAP)

TrainingModel8GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 8, family=gaussian())
FitTrain8GAP <- fit(TrainingModel8GAP)

TrainingModel9GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 9, family=gaussian())
FitTrain9GAP <- fit(TrainingModel9GAP)

TrainingModel10GAP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 10, family=gaussian())
FitTrain10GAP <- fit(TrainingModel10GAP)

#The feature Global Reactive Power did not converge for any number of states tested

TrainingModel4GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 4, family=gaussian())
FitTrain4GRP <- fit(TrainingModel4GRP)

TrainingModel5GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 5, family=gaussian())
FitTrain5GRP <- fit(TrainingModel5GRP)

TrainingModel6GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 6, family=gaussian())
FitTrain6GRP <- fit(TrainingModel6GRP)

TrainingModel7GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 7, family=gaussian())
FitTrain7GRP <- fit(TrainingModel7GRP)

TrainingModel8GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 8, family=gaussian())
FitTrain8GRP <- fit(TrainingModel8GRP)

TrainingModel9GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 9, family=gaussian())
FitTrain9GRP <- fit(TrainingModel9GRP)

TrainingModel10GRP <- depmix(response = imputed.Global_active_power~1,data = trainData, nstates = 10, family=gaussian())
FitTrain10GRP <- fit(TrainingModel10GRP)

#The feature used is Voltage

TrainingModel4VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 4, family=gaussian())
FitTrain4VLT <- fit(TrainingModel4VLT)

TrainingModel5VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 5, family=gaussian())
FitTrain5VLT <- fit(TrainingModel5VLT)

TrainingModel6VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 6, family=gaussian())
FitTrain6VLT <- fit(TrainingModel6VLT)

TrainingModel7VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 7, family=gaussian())
FitTrain7VLT <- fit(TrainingModel7VLT)

TrainingModel8VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 8, family=gaussian())
FitTrain8VLT <- fit(TrainingModel8VLT)

TrainingModel9VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 9, family=gaussian())
FitTrain9VLT <- fit(TrainingModel9VLT)

TrainingModel10VLT <- depmix(response = imputed.Voltage~1,data = trainData, nstates = 10, family=gaussian())
FitTrain10VLT <- fit(TrainingModel10VLT)

# The two features used are Global Active Power and Global Reactive Power

TrainingModel4GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 4, family=list(gaussian(),gaussian()))
FitTrain4GRP_GAP <- fit(TrainingModel4GRP_GAP)

TrainingModel5GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 5, family=list(gaussian(),gaussian()))
FitTrain5GRP_GAP <- fit(TrainingModel5GRP_GAP)

TrainingModel6GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 6, family=list(gaussian(),multinomial()))
FitTrain6GRP_GAP <- fit(TrainingModel6GRP_GAP)

TrainingModel7GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 7, family=list(gaussian(),gaussian()))
FitTrain7GRP_GAP <- fit(TrainingModel7GRP_GAP)

TrainingModel8GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 8, family=list(gaussian(),gaussian()))
FitTrain8GRP_GAP <- fit(TrainingModel8GRP_GAP)

TrainingModel9GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 9, family=list(gaussian(),gaussian()))
FitTrain8GRP_GAP <- fit(TrainingModel9GRP_GAP)

TrainingModel10GRP_GAP <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1),data = trainData, nstates = 10, family=list(multinomial(),multinomial()))
FitTrain10GRP_GAP <- fit(TrainingModel10GRP_GAP)


#The two features used are Global Reactive Power and Global Intensity

TrainingModel4GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 4, family=list(gaussian(),gaussian()))
FitTrain4GRP_INT <- fit(TrainingModel4GRP_INT)

TrainingModel5GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 5, family=list(gaussian(),multinomial()))
FitTrain5GRP_INT <- fit(TrainingModel5GRP_INT)

TrainingModel6GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 6, family=list(gaussian(),gaussian()))
FitTrain6GRP_INT <- fit(TrainingModel6GRP_INT)

TrainingModel7GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 7, family=list(gaussian(),gaussian()))
FitTrain7GRP_INT <- fit(TrainingModel7GRP_INT)

TrainingModel8GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 8, family=list(gaussian(),gaussian()))
FitTrain8GRP_INT <- fit(TrainingModel8GRP_INT)

TrainingModel9GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 9, family=list(gaussian(),gaussian()))
FitTrain9GRP_INT <- fit(TrainingModel9GRP_INT)

TrainingModel10GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = trainData, nstates = 10, family=list(gaussian(),gaussian()))
FitTrain10GRP_INT <- fit(TrainingModel10GRP_INT)

# The two features used are Global Active Power and Intensity

TrainingModel4GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 4, family=list(gaussian(),gaussian()))
FitTrain4GAP_INT <- fit(TrainingModel4GAP_INT)

TrainingModel5GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 5, family=list(gaussian(),gaussian()))
FitTrain5GAP_INT <- fit(TrainingModel5GAP_INT)

TrainingModel6GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 6, family=list(gaussian(),gaussian()))
FitTrain6GAP_INT <- fit(TrainingModel6GAP_INT)

TrainingModel7GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 7, family=list(gaussian(),gaussian()))
FitTrain7GAP_INT <- fit(TrainingModel7GAP_INT)

TrainingModel8GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 8, family=list(gaussian(),gaussian()))
FitTrain8GAP_INT <- fit(TrainingModel8GAP_INT)

TrainingModel9GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 9, family=list(gaussian(),gaussian()))
FitTrain9GAP_INT <- fit(TrainingModel9GAP_INT)

TrainingModel10GAP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 10, family=list(gaussian(),gaussian()))
FitTrain10GAP_INT <- fit(TrainingModel10GAP_INT)

#The two features used are Voltage and Global Intensity

TrainingModel4VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 4, family=list(gaussian(),gaussian()))
FitTrain4VLT_INT <- fit(TrainingModel4VLT_INT)

TrainingModel5VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 5, family=list(gaussian(),gaussian()))
FitTrain5VLT_INT <- fit(TrainingModel5VLT_INT)

TrainingModel6VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 6, family=list(gaussian(),gaussian()))
FitTrain6VLT_INT <- fit(TrainingModel6VLT_INT)

TrainingModel7VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 7, family=list(gaussian(),gaussian()))
FitTrain7VLT_INT <- fit(TrainingModel7VLT_INT)

TrainingModel8VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 8, family=list(gaussian(),gaussian()))
FitTrain8VLT_INT <- fit(TrainingModel8VLT_INT)

TrainingModel9VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 9, family=list(gaussian(),gaussian()))
FitTrain9VLT_INT <- fit(TrainingModel9VLT_INT)

TrainingModel10VLT_INT <- depmix(response = list(imputed.Voltage~1, imputed.Global_intensity~1),data = trainData, nstates = 10, family=list(gaussian(),gaussian()))
FitTrain10VLT_INT <- fit(TrainingModel10VLT_INT)

#The three features used are Global Active Power, Global Reactive Power, and Global Intensity

TrainingModel4GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 4, family=list(gaussian(),gaussian(), gaussian()))
FitTrain4GAP_GRP_INT <- fit(TrainingModel4GAP_GRP_INT)

TrainingModel5GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 5, family=list(gaussian(),gaussian(), gaussian()))
FitTrain5GAP_GRP_INT <- fit(TrainingModel5GAP_GRP_INT)

TrainingModel6GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 6, family=list(gaussian(),gaussian(), gaussian()))
FitTrain6GAP_GRP_INT <- fit(TrainingModel6GAP_GRP_INT)

TrainingModel7GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 7, family=list(gaussian(),gaussian(), gaussian()))
FitTrain7GAP_GRP_INT <- fit(TrainingModel7GAP_GRP_INT)

TrainingModel8GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 8, family=list(gaussian(),gaussian(), gaussian()))
FitTrain8GAP_GRP_INT <- fit(TrainingModel8GAP_GRP_INT)

TrainingModel9GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 9, family=list(gaussian(),gaussian(), gaussian()))
FitTrain9GAP_GRP_INT <- fit(TrainingModel9GAP_GRP_INT)

TrainingModel10GAP_GRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_active_power~1, imputed.Global_intensity~1),data = trainData, nstates = 10, family=list(gaussian(),gaussian(), gaussian()))
FitTrain10GAP_GRP_INT <- fit(TrainingModel10GAP_GRP_INT)

# Calculate Log likelihood values and BIC values of the training models

TrainingModel4INTLL <- logLik(FitTrain4INT,method="classification",na.allow=TRUE)
TrainingModel5INTLL <- logLik(FitTrain5INT,method="classification",na.allow=TRUE)
TrainingModel6INTLL <- logLik(FitTrain6INT,method="classification",na.allow=TRUE)
TrainingModel7INTLL <- logLik(FitTrain7INT,method="classification",na.allow=TRUE)
TrainingModel8INTLL <- logLik(FitTrain8INT,method="classification",na.allow=TRUE)
TrainingModel10INTLL <- logLik(FitTrain10INT,method="classification",na.allow=TRUE) # Didn't converge

TrainingModel4GAPLL <- logLik(FitTrain4GAP,method="classification",na.allow=TRUE)
TrainingModel5GAPLL <- logLik(FitTrain5GAP,method="classification",na.allow=TRUE)
TrainingModel6GAPLL <- logLik(FitTrain6GAP,method="classification",na.allow=TRUE)
TrainingModel7GAPLL <- logLik(FitTrain7GAP,method="classification",na.allow=TRUE)
TrainingModel8GAPLL <- logLik(FitTrain8GAP,method="classification",na.allow=TRUE)
TrainingModel10GAPLL <- logLik(FitTrain10GAP,method="classification",na.allow=TRUE)

TrainingModel4GRPLL <- logLik(FitTrain4GRP,method="classification",na.allow=TRUE)
TrainingModel5GRPLL <- logLik(FitTrain5GRP,method="classification",na.allow=TRUE)
TrainingModel6GRPLL <- logLik(FitTrain6GRP,method="classification",na.allow=TRUE)
TrainingModel7GRPLL <- logLik(FitTrain7GRP,method="classification",na.allow=TRUE)
TrainingModel8GRPLL <- logLik(FitTrain8GRP,method="classification",na.allow=TRUE)
TrainingModel10GRPLL <- logLik(FitTrain10GRP,method="classification",na.allow=TRUE)

TrainingModel4VLTLL <- logLik(FitTrain4VLT,method="classification",na.allow=TRUE)
TrainingModel5VLTLL <- logLik(FitTrain5VLT,method="classification",na.allow=TRUE)
TrainingModel6VLTLL <- logLik(FitTrain6VLT,method="classification",na.allow=TRUE)
TrainingModel7VLTLL <- logLik(FitTrain7VLT,method="classification",na.allow=TRUE)
TrainingModel8VLTLL <- logLik(FitTrain8VLT,method="classification",na.allow=TRUE)
TrainingModel10VLTLL <- logLik(FitTrain10VLT,method="classification",na.allow=TRUE)

TrainingModel4GRP_GAPLL <- logLik(FitTrain4GRP_GAP,method="classification",na.allow=TRUE)
TrainingModel5GRP_GAPLL <- logLik(FitTrain5GRP_GAP,method="classification",na.allow=TRUE)
TrainingModel6GRP_GAPLL <- logLik(FitTrain6GRP_GAP,method="classification",na.allow=TRUE)
TrainingModel7GRP_GAPLL <- logLik(FitTrain7GRP_GAP,method="classification",na.allow=TRUE) #Didn't converge
TrainingModel8GRP_GAPLL <- logLik(FitTrain8GRP_GAP,method="classification",na.allow=TRUE) #Didn't converge
TrainingModel10GRP_GAPLL <- logLik(FitTrain10GRP_GAP,method="classification",na.allow=TRUE) # Didn't converge

TrainingModel4GRP_INTLL <- logLik(FitTrain4GRP_INT,method="classification",na.allow=TRUE)
TrainingModel5GRP_INTLL <- logLik(FitTrain5GRP_INT,method="classification",na.allow=TRUE)
TrainingModel6GRP_INTLL <- logLik(FitTrain6GRP_INT,method="classification",na.allow=TRUE)
TrainingModel7GRP_INTLL <- logLik(FitTrain7GRP_INT,method="classification",na.allow=TRUE)
TrainingModel8GRP_INTLL <- logLik(FitTrain8GRP_INT,method="classification",na.allow=TRUE) # Didn't converge
TrainingModel10GRP_INTLL <- logLik(FitTrain10GRP_INT,method="classification",na.allow=TRUE) # Didn't converge

TrainingModel4GAP_INTLL <- logLik(FitTrain4GAP_INT,method="classification",na.allow=TRUE)
TrainingModel5GAP_INTLL <- logLik(FitTrain5GAP_INT,method="classification",na.allow=TRUE)
TrainingModel6GAP_INTLL <- logLik(FitTrain6GAP_INT,method="classification",na.allow=TRUE)
TrainingModel7GAP_INTLL <- logLik(FitTrain7GAP_INT,method="classification",na.allow=TRUE)
TrainingModel8GAP_INTLL <- logLik(FitTrain8GAP_INT,method="classification",na.allow=TRUE) # Didn't converge
TrainingModel10GAP_INTLL <- logLik(FitTrain10GAP_INT,method="classification",na.allow=TRUE) # Didn't converge

TrainingModel4VLT_INTLL <- logLik(FitTrain4VLT_INT,method="classification",na.allow=TRUE)
TrainingModel5VLT_INTLL <- logLik(FitTrain5VLT_INT,method="classification",na.allow=TRUE)
TrainingModel6VLT_INTLL <- logLik(FitTrain6VLT_INT,method="classification",na.allow=TRUE)
TrainingModel7VLT_INTLL <- logLik(FitTrain7VLT_INT,method="classification",na.allow=TRUE)
TrainingModel8VLT_INTLL <- logLik(FitTrain8VLT_INT,method="classification",na.allow=TRUE)
TrainingModel10VLT_INTLL <- logLik(FitTrain10VLT_INT,method="classification",na.allow=TRUE)

TrainingModel4GAP_GRP_INTLL <- logLik(FitTrain4GAP_GRP_INT,method="classification",na.allow=TRUE)
TrainingModel5GAP_GRP_INTLL <- logLik(FitTrain5GAP_GRP_INT,method="classification",na.allow=TRUE)
TrainingModel6GAP_GRP_INTLL <- logLik(FitTrain6GAP_GRP_INT,method="classification",na.allow=TRUE)
TrainingModel7GAP_GRP_INTLL <- logLik(FitTrain7GAP_GRP_INT,method="classification",na.allow=TRUE)
TrainingModel8GAP_GRP_INTLL <- logLik(FitTrain8GAP_GRP_INT,method="classification",na.allow=TRUE)
TrainingModel10GAP_GRP_INTLL <- logLik(FitTrain10GAP_GRP_INT,method="classification",na.allow=TRUE)




# Calculate BIC values for the different training fits

TrainingModel4INTBIC <- BIC(FitTrain4INT)
TrainingModel5INTBIC <- BIC(FitTrain5INT)
TrainingModel6INTBIC <- BIC(FitTrain6INT)
TrainingModel7INTBIC <- BIC(FitTrain7INT)
TrainingModel8INTBIC <- BIC(FitTrain8INT)
TrainingModel10INTBIC <- BIC(FitTrain10INT)

TrainingModel4GAPBIC <- BIC(FitTrain4GAP)
TrainingModel5GAPBIC <- BIC(FitTrain5GAP)
TrainingModel6GAPBIC <- BIC(FitTrain6GAP)
TrainingModel7GAPBIC <- BIC(FitTrain7GAP)
TrainingModel8GAPBIC <- BIC(FitTrain8GAP)
TrainingModel10GAPBIC <- BIC(FitTrain10GAP)

TrainingModel4VLTBIC <- BIC(FitTrain4VLT)
TrainingModel5VLTBIC <- BIC(FitTrain5VLT)
TrainingModel6VLTBIC <- BIC(FitTrain6VLT)
TrainingModel7VLTBIC <- BIC(FitTrain7VLT)
TrainingModel8VLTBIC <- BIC(FitTrain8VLT)
TrainingModel10VLTBIC <- BIC(FitTrain10VLT)

TrainingModel4GRP_GAPBIC <- BIC(FitTrain4GRP_GAP)
TrainingModel5GRP_GAPBIC <- BIC(FitTrain5GRP_GAP)
TrainingModel6GRP_GAPBIC <- BIC(FitTrain6GRP_GAP)
TrainingModel7GRP_GAPBIC <- BIC(FitTrain7GRP_GAP)
TrainingModel8GRP_GAPBIC <- BIC(FitTrain8GRP_GAP)
TrainingModel10GRP_GAPBIC <- BIC(FitTrain10GRP_GAP)

TrainingModel4GRP_INTBIC <- BIC(FitTrain4GRP_INT)
TrainingModel5GRP_INTBIC <- BIC(FitTrain5GRP_INT)
TrainingModel6GRP_INTBIC <- BIC(FitTrain6GRP_INT)
TrainingModel7GRP_INTBIC <- BIC(FitTrain7GRP_INT)
TrainingModel8GRP_INTBIC <- BIC(FitTrain8GRP_INT)
TrainingModel10GRP_INTBIC <- BIC(FitTrain10GRP_INT)

TrainingModel4GAP_INTBIC <- BIC(FitTrain4GAP_INT)
TrainingModel5GAP_INTBIC <- BIC(FitTrain5GAP_INT)
TrainingModel6GAP_INTBIC <- BIC(FitTrain6GAP_INT)
TrainingModel7GAP_INTBIC <- BIC(FitTrain7GAP_INT)
TrainingModel8GAP_INTBIC <- BIC(FitTrain8GAP_INT)
TrainingModel10GAP_INTBIC <- BIC(FitTrain10GAP_INT)

TrainingModel4VLT_INTBIC <- BIC(FitTrain4VLT_INT)
TrainingModel5VLT_INTBIC <- BIC(FitTrain5VLT_INT)
TrainingModel6VLT_INTBIC <- BIC(FitTrain6VLT_INT)
TrainingModel7VLT_INTBIC <- BIC(FitTrain7VLT_INT)
TrainingModel8VLT_INTBIC <- BIC(FitTrain8VLT_INT)
TrainingModel10VLT_INTBIC <- BIC(FitTrain10VLT_INT)

TrainingModel4GAP_GRP_INTBIC <- BIC(FitTrain4GAP_GRP_INT)
TrainingModel5GAP_GRP_INTBIC <- BIC(FitTrain5GAP_GRP_INT)
TrainingModel6GAP_GRP_INTBIC <- BIC(FitTrain6GAP_GRP_INT)
TrainingModel7GAP_GRP_INTBIC <- BIC(FitTrain7GAP_GRP_INT)
TrainingModel8GAP_GRP_INTBIC <- BIC(FitTrain8GAP_GRP_INT)
TrainingModel10GAP_GRP_INTBIC <- BIC(FitTrain10GAP_GRP_INT)

# Create model with the same amount of states for Test Data and Fit it with the respective Trained model for the best univariate and multivariate model


TestModelNINT <- depmix(response = imputed.Global_intensity~1,data = testData, nstates = 6, family=gaussian())
FitTrainNINT <- setpars(TestModelNINT, getpars(FitTrain6INT))

TestModelNGAP <- depmix(response = imputed.Global_active_power~1,data = testData, nstates = 8, family=gaussian())
FitTrainNGAP <- setpars(TestModelNGAP, getpars(FitTrain8GAP))

TestModelNGRP <- depmix(response = imputed.Global_reactive_power~1,data = testData, nstates = 4, family=gaussian())
FitTrainNGRP <- setpars(TestModelNGRP, getpars(FitTrain4GRP))

TestModelNGRP_INT <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = testData, nstates = 4, family=list(gaussian(),gaussian()))
FitTrainNGRP_INT <- setpars(TestModelNGRP_INT, getpars(FitTrain4GRP_INT))

TestModelNGAP_GRP_INT <- depmix(response = list(imputed.Global_active_power~1, imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = testData, nstates = 4, family=list(gaussian(),gaussian(),gaussian()))
FitTrainNGAP_GRP_INT <- setpars(TestModelNGAP_GRP_INT, getpars(FitTrain4GAP_GRP_INT))


#Calculate LogLike and BIC values

TestModelNINTLL <- logLik(FitTrainNINT,method="classification",na.allow=TRUE)
TestModelNINTLL


TestModelNGAPLL <- logLik(FitTrainNGAP,method="classification",na.allow=TRUE)
TestModelNGAPLL

TestModelNGRPLL <- logLik(FitTrainNGRP,method="classification",na.allow=TRUE)
TestModelNGRPLL

TestModelNGRP_INTLL <- logLik(FitTrainNGRP_INT,method="classification",na.allow=TRUE)
TestModelNGRP_INTLL

TestModelNGAP_GRP_INTLL <- logLik(FitTrainNGAP_GRP_INT,method="classification",na.allow=TRUE)
TestModelNGAP_GRP_INTLL

TestModelNINTBIC <- BIC(FitTrainNINT)
TestModelNINTBIC

TestModelNGAPBIC <- BIC(FitTrainNGAP)
TestModelNGAPBIC

TestModelNGRP_INTBIC <- BIC(FitTrainNGRP_INT)
TestModelNGRP_INTBIC

TestModelNGAP_GRP_INTBIC <- BIC(FitTrainNGAP_GRP_INT)
TestModelNGAP_GRP_INTBIC

#################################################
       
       #End of testing and training
       
#################################################
  
       
#################################################
       
#ANOTHER SHOT AT THIS - GIO  START  4-1 
       
###############################################
library(dplyr)
library(ggplot2)
library(corrplot)

setwd("working-directory")

path1 <- file.path(getwd(), "test1.txt")
path2 <- file.path(getwd(), "test2.txt")
path3 <- file.path(getwd(), "test3.txt")
path4 <- file.path(getwd(), "test4.txt")
path5 <- file.path(getwd(), "test5.txt")

# tbl1 <- read.csv(path1, header = TRUE, sep = ",", quote = "\"", dec = ".")
# tbl1 <- read.table(path2, header = TRUE, sep = ",", quote = "\"", dec = ".")
# tbl1 <- read.table(path3, header = TRUE, sep = ",", quote = "\"", dec = ".")
# tbl1 <- read.table(path4, header = TRUE, sep = ",", quote = "\"", dec = ".")
tbl1 <- read.table(path5, header = TRUE, sep = ",", quote = "\"", dec = ".")


window_avg <- function(table_col, window_size) {
  # define anomalies lists 
  overall_anomalies = table_col
  window_anomalies = table_col
  average_anomalies = table_col
  # define overall variables 
  overall_avg = mean(table_col)
  overall_std = sd(table_col)
  thresh_max = overall_avg + 3*overall_std
  thresh_min = overall_avg - 3*overall_std
  counter = 0
  # check if each value is an overall anomaly
  for (i in 1:length(table_col)){
    value <- table_col[i]
    overall_anomaly = FALSE
    window_anomaly = FALSE
    average_anomaly = FALSE
    if (value > thresh_max | value < thresh_min){
      # value is an overall anomaly
      overall_anomaly = TRUE
    }
    else{
      overall_anomalies[i] = NA
    }
    # check if each value is a window anomaly
    if (i < (length(table_col) - window_size)) {
      # create the window and find the moving average
      window_limit <- i + window_size - 1
      sub_set = table_col[c(i:window_limit)]
      # check if value is more than avg + 3*std
      window_avg = mean(sub_set)
      window_sd = mean(sub_set)
      thresh_max_win = window_avg + 3*window_sd
      thresh_min_win = window_avg - 3*window_sd
      if (value > thresh_max_win | value < thresh_min_win){
        # value is a window anomaly
        window_anomaly = TRUE
      }
      else{
        window_anomalies[i] <- NA
      }
      # check if value is an average anomaly
      window_max = max(sub_set)
      window_min = min(sub_set)
      thresh_avg_max = (window_max+window_min)/2 + 3*window_sd
      thresh_avg_min = (window_max+window_min)/2 - 3*window_sd
      if (window_avg > thresh_avg_max | window_avg < thresh_avg_min){
        # value is a average anomaly
        average_anomaly = TRUE
      }
      else{
        average_anomalies[i] <- NA
      }
    }
    # if (overall_anomaly) {
    #   counter <- counter + 1
    # }
    # if (window_anomaly){
    #   counter <- counter + 1
    # }
    # if (average_anomaly){
    #   counter <- counter + 1
    # }
  }
  return(average_anomalies)
}
##################################################################################
###################################################################################
# weekday range = 2/12/2009 which is a wednesday
rng <- c(594:2033)
glb_int1 <- tbl1$Global_intensity[rng]
rec_pwr1 <- tbl1$Global_reactive_power[rng]
ac_pwr1 <- tbl1$Global_active_power[rng]
vltg1 <- tbl1$Voltage[rng]
subm1_1 <- tbl1$Sub_metering_1[rng]
subm2_1 <- tbl1$Sub_metering_2[rng]
subm3_1 <- tbl1$Sub_metering_3[rng]

par(mfrow=c(2,4))
overall_anomalies <- window_avg(glb_int1, 200)
plot(glb_int1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(rec_pwr1, 200)
plot(rec_pwr1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(ac_pwr1, 200)
plot(ac_pwr1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(vltg1, 200)
plot(vltg1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(subm1_1, 200)
plot(subm1_1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(subm2_1, 200)
plot(subm2_1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(subm3_1, 200)
plot(subm3_1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")

################################################################################
################################################################################
# # weekend day range = 5/12/2009 which is a Saturday
rng <- c(4914:6353)
glb_int1 <- tbl1$Global_intensity[rng]
rec_pwr1 <- tbl1$Global_reactive_power[rng]
ac_pwr1 <- tbl1$Global_active_power[rng]
vltg1 <- tbl1$Voltage[rng]
subm1_1 <- tbl1$Sub_metering_1[rng]
subm2_1 <- tbl1$Sub_metering_2[rng]
subm3_1 <- tbl1$Sub_metering_3[rng]

par(mfrow=c(2,4))
overall_anomalies <- window_avg(glb_int1, 200)
plot(glb_int1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(rec_pwr1, 200)
plot(rec_pwr1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(ac_pwr1, 200)
plot(ac_pwr1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(vltg1, 200)
plot(vltg1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(subm1_1, 200)
plot(subm1_1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(subm2_1, 200)
plot(subm2_1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")
overall_anomalies <- window_avg(subm3_1, 200)
plot(subm3_1, type="l", col="blue")
lines(overall_anomalies, col="red", type="p")

##############################################################
####################	End of Gio's Part 4-1		  ############    
##############################################################           
       
################################################################
       #PART 4-2 START
###############################################################

## Part 4 - 2
library(dplyr)
library(ggplot2)
library(mice)
library(depmix)
setwd("working-directory")

## IMPORTING TEST DATA ##
       
path1 <- file.path(getwd(), "test1Clean.txt")
path2 <- file.path(getwd(), "test2Clean.txt")
path3 <- file.path(getwd(), "test3Clean.txt")
path4 <- file.path(getwd(), "test4Clean.txt")
path5 <- file.path(getwd(), "test5Clean.txt")


test1 <- read.table(path1, header = TRUE, sep = ",", quote = "\"", dec = ".")
test2 <- read.table(path2, header = TRUE, sep = ",", quote = "\"", dec = ".")
test3 <- read.table(path3, header = TRUE, sep = ",", quote = "\"", dec = ".")
test4 <- read.table(path4, header = TRUE, sep = ",", quote = "\"", dec = ".")
test5 <- read.table(path5, header = TRUE, sep = ",", quote = "\"", dec = ".")

##Fixing Dates

#Test1
## Fix the dates fortmat
test1$Date <- strptime(test1$Date, format="%Y-%m-%d")
test1$Date <- as.POSIXlt(test1$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
test1$Time <- strptime(test1$Time, format="%H:%M:%S")
test1$Time <- format(test1$Time, "%H:%M:%S")
test1$Time <- as.list(test1$Time)

#Test2
## Fix the dates fortmat
test2$Date <- strptime(test2$Date, format="%Y-%m-%d")
test2$Date <- as.POSIXlt(test2$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
test2$Time <- strptime(test2$Time, format="%H:%M:%S")
test2$Time <- format(test2$Time, "%H:%M:%S")
test2$Time <- as.list(test2$Time)

#Test3
## Fix the dates fortmat
test3$Date <- strptime(test3$Date, format="%Y-%m-%d")
test3$Date <- as.POSIXlt(test3$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
test3$Time <- strptime(test3$Time, format="%H:%M:%S")
test3$Time <- format(test3$Time, "%H:%M:%S")
test3$Time <- as.list(test3$Time)

#Test4
## Fix the dates fortmat
test4$Date <- strptime(test4$Date, format="%Y-%m-%d")
test4$Date <- as.POSIXlt(test4$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
test4$Time <- strptime(test4$Time, format="%H:%M:%S")
test4$Time <- format(test4$Time, "%H:%M:%S")
test4$Time <- as.list(test4$Time)

#Test5
## Fix the dates fortmat
test5$Date <- strptime(test5$Date, format="%Y-%m-%d")
test5$Date <- as.POSIXlt(test5$Date, format="%d/%m/%Y")
# Properly formats time as numbers so they can be compared 
test5$Time <- strptime(test5$Time, format="%H:%M:%S")
test5$Time <- format(test5$Time, "%H:%M:%S")
test5$Time <- as.list(test5$Time)





##Subset Time Period
#Test 1
#subsetting date
test1_sub <-subset(test1, (test1$Date >= "2009-12-20"  & test1$Date <= "2009-12-26")) #sunday start
#subsetting time
test1_sub <- subset(test1_sub, (test1_sub$Time >= "03:20:00" & test1_sub$Time < "06:40:00"))

#Test 2
test2_sub <-subset(test2, (test2$Date >= "2009-12-20"  & test2$Date <= "2009-12-26")) #sunday start
#subsetting time
test2_sub <- subset(test2_sub, (test2_sub$Time >= "03:20:00" & test2_sub$Time < "06:40:00"))

#Test 3
test3_sub <-subset(test3, (test3$Date >= "2009-12-20"  & test3$Date <= "2009-12-26")) #sunday start
#subsetting time
test3_sub <- subset(test3_sub, (test3_sub$Time >= "03:20:00" & test3_sub$Time < "06:40:00"))

#Test 4
test4_sub <-subset(test4, (test4$Date >= "2009-12-20"  & test4$Date <= "2009-12-26")) #sunday start
#subsetting time
test4_sub <- subset(test4_sub, (test4_sub$Time >= "03:20:00" & test4_sub$Time < "06:40:00"))

#Test 5
test5_sub <-subset(test5, (test5$Date >= "2009-12-20"  & test5$Date <= "2009-12-26")) #sunday start
#subsetting time
test5_sub <- subset(test5_sub, (test5_sub$Time >= "03:20:00" & test5_sub$Time < "06:40:00"))

test5_sub



###m1
testNUnivariate1 <- depmix(response = imputed.Global_intensity~1,data = test1_sub, nstates = 6, family=gaussian())
testNFitUnivariate1 <- setpars(testNUnivariate1, getpars(FitTrain6INT))
#print(testNUnivariate1)
#print(testNFitUnivariate1)
logLik(testNFitUnivariate1,method="classification",na.allow=TRUE)
BIC(testNFitUnivariate1)


testNUnivariate2 <- depmix(response = imputed.Global_intensity~1,data = test2_sub, nstates = 6, family=gaussian())
testNFitUnivariate2 <- setpars(testNUnivariate2, getpars(FitTrain6INT))
#print(testNUnivariate2)
#print(testNFitUnivariate2)
logLik(testNFitUnivariate2,method="classification",na.allow=TRUE)
BIC(testNFitUnivariate2)


testNUnivariate3 <- depmix(response = imputed.Global_intensity~1,data = test3_sub, nstates = 6, family=gaussian())
testNFitUnivariate3 <- setpars(testNUnivariate3, getpars(FitTrain6INT))
#print(testNUnivariate3)
#print(testNFitUnivariate3)
logLik(testNFitUnivariate3,method="classification",na.allow=TRUE)
BIC(testNFitUnivariate3)


testNUnivariate4 <- depmix(response = imputed.Global_intensity~1,data = test4_sub, nstates = 6, family=gaussian())
testNFitUnivariate4 <- setpars(testNUnivariate4, getpars(FitTrain6INT))
#print(testNUnivariate4)
#print(testNFitUnivariate4)
logLik(testNFitUnivariate4,method="classification",na.allow=TRUE)
BIC(testNFitUnivariate4)


testNUnivariate5 <- depmix(response = imputed.Global_intensity~1,data = test5_sub, nstates = 6, family=gaussian())
testNFitUnivariate5 <- setpars(testNUnivariate5, getpars(FitTrain6INT))
#print(testNUnivariate5)
#print(testNFitUnivariate5)
logLik(testNFitUnivariate5,method="classification",na.allow=TRUE)
BIC(testNFitUnivariate5)



#M1
testNMultivariate1 <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = test1_sub, nstates = 4, family=list(gaussian(),gaussian()))
testNFitMultivariate1 <- setpars(testNMultivariate1, getpars(FitTrain4GRP_INT))
#print(testNMultivariate1)
#print(testNFitMultivariate1)
logLik(testNFitMultivariate1,method="classification",na.allow=TRUE)
BIC(testNFitMultivariate1)

#m2
testNMultivariate2 <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = test2_sub, nstates = 4, family=list(gaussian(),gaussian()))
testNFitMultivariate2 <- setpars(testNMultivariate2, getpars(FitTrain4GRP_INT))
#print(testNMultivariate2)
#print(testNFitMultivariate2)
logLik(testNFitMultivariate2,method="classification",na.allow=TRUE)
BIC(testNFitMultivariate2)


#m3
testNMultivariate3 <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = test3_sub, nstates = 4, family=list(gaussian(),gaussian()))
testNFitMultivariate3 <- setpars(testNMultivariate3, getpars(FitTrain4GRP_INT))
#print(testNMultivariate3)
#print(testNFitMultivariate3)
logLik(testNFitMultivariate3,method="classification",na.allow=TRUE)
BIC(testNFitMultivariate3)

#m4
testNMultivariate4 <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = test4_sub, nstates = 4, family=list(gaussian(),gaussian()))
testNFitMultivariate4 <- setpars(testNMultivariate4, getpars(FitTrain4GRP_INT))
#print(testNMultivariate4)
#print(testNFitMultivariate4)
logLik(testNFitMultivariate4,method="classification",na.allow=TRUE)
BIC(testNFitMultivariate4)


#m5
testNMultivariate5 <- depmix(response = list(imputed.Global_reactive_power~1, imputed.Global_intensity~1),data = test5_sub, nstates = 4, family=list(gaussian(),gaussian()))
testNFitMultivariate5 <- setpars(testNMultivariate5, getpars(FitTrain4GRP_INT))
#print(testNMultivariate5)
#print(testNFitMultivariate5)
logLik(testNFitMultivariate5,method="classification",na.allow=TRUE)
BIC(testNFitMultivariate5)


       
       
       