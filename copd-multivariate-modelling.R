## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------
############################################################## 
# Housekeeping Use for All Analyses #
##############################################################
date() # Current system time and date. 
Sys.time() # Current system time and date (redundant).
R.version.string # R version and version release date.
options(digits=6) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width. 
ls() # List all objects in the working # directory.
rm(list = ls()) # CAUTION: Remove all files in the #working directory. If this action is not desired, use rm() one-by-one to remove the objectsthat are not needed.
ls.str() # List all objects with finite detail.
getwd() # Identify the current working directory
setwd("C:/Users/linan/Documents/GitHub/R-COPD-regression-modelling") # Set to a new working directory. # Note the single forward slash and double # quotes. # This new directory should be the directory # where the data file is located, otherwise # the data file will not be found.
getwd()# Confirm the working directory.
list.files()# List files at the PC directory
.libPaths()# Library pathname
.Library# Library pathname.
sessionInfo() # R version, locale, and packages.
search()# Attached packages and objects.
searchpaths() # Attached packages and objects.

###############################################################


## -------------------------------------------------------------------------------------------------------
## Libraries
library(dplyr)
library(Hmisc)
library(gmodels)
library(ggplot2)
library(tidyverse)
library(mctest)
library(DataExplorer) # for EDA


## -------------------------------------------------------------------------------------------------------
# import data
copd <- read.table(file="COPD_student_dataset.csv", header=TRUE, dec=".", sep = ",")


## -------------------------------------------------------------------------------------------------------
# Data exploration 
str(copd)
head(copd)


## -------------------------------------------------------------------------------------------------------
#Numeric data
copd$AGE <- as.numeric(copd$AGE)
copd$MWT1 <- as.numeric(copd$MWT1)
copd$MWT2 <- as.numeric(copd$MWT2)
copd$MWT1Best<-as.numeric(copd$MWT1Best)
copd$FEV1PRED <- as.numeric(copd$FEV1PRED)
copd$FVCPRED <- as.numeric(copd$FVCPRED)
copd$CAT <- as.numeric(copd$CAT)

## -------------------------------------------------------------------------------------------------------
#Categorical cata
copd$AGEquartiles <- as.factor(copd$AGEquartiles)
copd$copd <- as.factor(copd$copd)
copd$gender <- as.factor(copd$gender)
copd$Diabetes <- as.factor(copd$Diabetes)
copd$smoking <- as.factor(copd$smoking)
copd$muscular <- as.factor(copd$muscular)
copd$hypertension <- as.factor(copd$hypertension)
copd$AtrialFib <- as.factor(copd$AtrialFib)
copd$IHD <- as.factor(copd$IHD)


## -------------------------------------------------------------------------------------------------------
#Check each data types
str(copd)


## -------------------------------------------------------------------------------------------------------
plot_intro(copd)

## -------------------------------------------------------------------------------------------------------
#Missing value
plot_missing(copd, group=c("Good"=1.0), theme_config=list(text = element_text(size = 16)))


## -------------------------------------------------------------------------------------------------------
# Create variable comorbid
comorbid <- length(copd$Diabetes) #create a variable with length similar with Diabetes variable
comorbid[copd$Diabetes ==1 | copd$muscular == 1 | copd$hypertension ==1 | copd$AtrialFib ==1 | copd$IHD == 1] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid)


## -------------------------------------------------------------------------------------------------------
copd$comorbid <- comorbid

## -------------------------------------------------------------------------------------------------------
str(copd)


## -------------------------------------------------------------------------------------------------------
# bar charts of discrete features
plot_bar(copd)


## ----create repeated crosstable for categorical data----------------------------------------------------
#Crosstabs of categorical data
# Assuming 'cat_vars' contains the names of categorical variables
cat_vars <- c("gender", "COPDSEVERITY", "copd", "smoking", "Diabetes", "muscular", "hypertension", "AtrialFib", "IHD","comorbid")  

# Create repeated CrossTable for each categorical variable
for(var in cat_vars) {
  cat(sprintf("Variable: %s\n", var))  
  cat_table <- CrossTable(copd[, var], prop.chisq = FALSE)
  print(cat_table)
}


## -------------------------------------------------------------------------------------------------------
num_var <- c("AGE", "PackHistory", "MWT1", "MWT2", "MWT1Best", "FEV1", "FEV1PRED", "FVC", "FVCPRED","CAT","HAD","SGRQ") 


## ----examine outlier dlookr-----------------------------------------------------------------------------
# use dlookr to examine outliers
library(dlookr)
plot_outlier(copd,
             diagnose_outlier(copd) %>%
                filter(outliers_cnt > 0) %>%
                select(variables) %>%
                unlist())


## -------------------------------------------------------------------------------------------------------
summary(copd)


## -------------------------------------------------------------------------------------------------------
# histogram of numeric variables
for(var in num_var) {
  hist(copd[[var]], main = paste("Histogram of", var), xlab = var)
}


## -------------------------------------------------------------------------------------------------------
copd$CAT[copd$CAT>40] <- NA


## -------------------------------------------------------------------------------------------------------
summary(copd$MWT1Best)

## -------------------------------------------------------------------------------------------------------
subset(copd, MWT1Best>650)

## -------------------------------------------------------------------------------------------------------
subset(copd, MWT1Best<150)

## -------------------------------------------------------------------------------------------------------
subset(copd, HAD>50)


## -------------------------------------------------------------------------------------------------------
# Subset the rows based on the condition
subset_copd <- copd[copd$MWT1Best < 650 & copd$MWT1Best > 150, ]
subset_copd <- subset_copd[subset_copd$CAT<40,]
subset_copd <- subset_copd[subset_copd$HAD<50,]

# Check the dimensions of the subsetted data
dim(subset_copd)
summary(subset_copd)


## -------------------------------------------------------------------------------------------------------
my_data<-subset_copd[,c("AGE","PackHistory","FEV1","FEV1PRED","FVC","FVCPRED","MWT1","MWT2","MWT1Best","CAT","HAD","SGRQ")]
cor_matrix <- cor(my_data, use = "complete.obs")

## -------------------------------------------------------------------------------------------------------
round(cor_matrix,4)


## -------------------------------------------------------------------------------------------------------
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+FVCPRED+MWT1+MWT2+MWT1Best+CAT+HAD+SGRQ, data=subset_copd, pch=20,cex=1)

## -------------------------------------------------------------------------------------------------------
num_vars1 <- c("AGE", "PackHistory", "MWT1", "MWT2", "MWT1Best") 
num_vars2 <- c("FEV1", "FEV1PRED", "FVC", "FVCPRED")
num_vars3 <- c("CAT","HAD","SGRQ")


## -------------------------------------------------------------------------------------------------------
# Create boxplots for each numerical variable
par(mfrow = c(1, length(num_vars1)), mar = c(5, 4, 4, 2))  # Adjusting margins

for(i in 1:length(num_vars1)) {
  boxplot(subset_copd[[num_vars1[i]]], main = paste("Boxplot of", num_vars1[i]), 
          ylab = num_vars1[i], col = "skyblue", border = "black")
}

par(mfrow = c(1, 1))  # Reset to single-panel plot

## -------------------------------------------------------------------------------------------------------
# Create boxplots for each numerical variable
par(mfrow = c(1, length(num_vars2)), mar = c(5, 4, 4, 2))  # Adjusting margins

for(i in 1:length(num_vars2)) {
  boxplot(subset_copd[[num_vars2[i]]], main = paste("Boxplot of", num_vars2[i]), 
          ylab = num_vars2[i], col = "skyblue", border = "black")
}

par(mfrow = c(1, 1))  # Reset to single-panel plot

## -------------------------------------------------------------------------------------------------------
# Create boxplots for each numerical variable
par(mfrow = c(1, length(num_vars3)), mar = c(5, 4, 4, 2))  # Adjusting margins

for(i in 1:length(num_vars3)) {
  boxplot(subset_copd[[num_vars3[i]]], main = paste("Boxplot of", num_vars3[i]), 
          ylab = num_vars3[i], col = "skyblue", border = "black")
}

par(mfrow = c(1, 1))  # Reset to single-panel plot


## -------------------------------------------------------------------------------------------------------
# Subset the rows based on the condition
subset_copd <- subset_copd[subset_copd$AGE>55 & subset_copd$AGE<85,]
subset_copd <- subset_copd[subset_copd$PackHistory<95,]
subset_copd <- na.omit(subset_copd)


## -------------------------------------------------------------------------------------------------------
# Create boxplots for each numerical variable
par(mfrow = c(1, length(num_vars1)), mar = c(5, 4, 4, 2))  # Adjusting margins
for(i in 1:length(num_vars1)) {
  boxplot(subset_copd[[num_vars1[i]]], main = paste("Boxplot of", num_vars1[i]), 
          ylab = num_vars1[i], col = "skyblue", border = "black")
}
par(mfrow = c(1, 1))  # Reset to single-panel plot


## -------------------------------------------------------------------------------------------------------
# Check the dimensions of the subset data
dim(subset_copd)
describe(subset_copd)
summary(subset_copd)


## -------------------------------------------------------------------------------------------------------
cor_matrix <- cor(my_data, use = "complete.obs")

round(cor_matrix,4)

## -------------------------------------------------------------------------------------------------------
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+FVCPRED+MWT1+MWT2+MWT1Best+CAT+HAD+SGRQ, data=subset_copd, pch=20,cex=1)

## -------------------------------------------------------------------------------------------------------
pairs(~AGE+FEV1+FVC+MWT1Best+CAT+HAD+SGRQ, data=subset_copd, pch=20,cex=1)


## ----count mean and sd----------------------------------------------------------------------------------
# Initialize vectors to store results
means <- numeric()
stds <- numeric()

# Loop through each column in subset_copd
for (col in names(subset_copd)) {
  # Check if the column is numeric
  if (is.numeric(subset_copd[[col]])) {
    # Calculate mean and standard deviation
    mean_value <- mean(subset_copd[[col]])
    sd_value <- sd(subset_copd[[col]])
    
    # Print or store results
    cat("Column:", col,"\n")
    cat("Mean:", mean_value, "\n")
    cat("Standard deviation:", sd_value, "\n\n")
    
    # Store results in vectors
    means <- c(means, mean_value)
    stds <- c(stds, sd_value)
  }
}

# Print overall means and standard deviations
cat("Overall Means: \n")
print(means)

cat("Overall standard deviation: \n")
print(stds)


## -------------------------------------------------------------------------------------------------------
# List of independent variables (replace with your actual variable names)
independent_vars <- c("AGE", "PackHistory", "FEV1", "FVC", "MWT1Best","CAT","HAD","gender","COPDSEVERITY","comorbid","Diabetes","hypertension","muscular","AtrialFib","IHD","smoking")

# Dependent variable
dependent_var <- "SGRQ"  # Replace with your dependent variable name

# Perform linear regression for each independent variable
for(var in independent_vars) {
  formula <- paste(dependent_var, "~", var)
  model <- lm(formula, data = subset_copd)
  cat("Linear Regression Summary for", var, ":\n")
  print(summary(model))
  cat("95% CI", var, ":\n")
  print(confint(model))
  cat("\n")
}


## -------------------------------------------------------------------------------------------------------
sgrq_model <- lm(SGRQ~AGE+gender+FEV1+FVC+CAT+MWT1Best+COPDSEVERITY+HAD+comorbid+smoking+Diabetes+hypertension+muscular+AtrialFib+IHD, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel1 <- predict(sgrq_model)
residualsgrqmodel1 <- residuals(sgrq_model)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model)

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model)


## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model, method = "VIF")


## -------------------------------------------------------------------------------------------------------
sgrq_model_1 <- lm(SGRQ~FEV1+FVC+CAT, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_1)


## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_1)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel1 <- predict(sgrq_model_1)
residualsgrqmodel1 <- residuals(sgrq_model_1)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_1)

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_1)


## -------------------------------------------------------------------------------------------------------
sgrq_model_2 <- lm(SGRQ~AGE+gender+COPDSEVERITY+FEV1+FVC+CAT+MWT1Best+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_2)


## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_2)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel2 <- predict(sgrq_model_2)
residualsgrqmodel2 <- residuals(sgrq_model_2)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_2)


## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2)

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="VIF")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="TOL")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="Wi")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="Leamer")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="CVIF")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="Klein")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="IND1")

## -------------------------------------------------------------------------------------------------------
imcdiag(sgrq_model_2, method="IND2")


## -------------------------------------------------------------------------------------------------------
sgrq_model_3 <- lm(SGRQ~AGE+gender+FEV1+FVC+CAT+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_3)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_3)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel3 <- predict(sgrq_model_3)
residualsgrqmodel3 <- residuals(sgrq_model_3)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_3)


## -------------------------------------------------------------------------------------------------------
sgrq_model_4 <- lm(SGRQ~AGE+gender+COPDSEVERITY+FVC+CAT+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_4)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_4)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel4 <- predict(sgrq_model_4)
residualsgrqmodel4 <- residuals(sgrq_model_4)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_4)


## -------------------------------------------------------------------------------------------------------
sgrq_model_5 <- lm(SGRQ~AGE+gender+COPDSEVERITY+FEV1+CAT+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_5)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_5)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel5 <- predict(sgrq_model_5)
residualsgrqmodel5 <- residuals(sgrq_model_5)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_5)


## -------------------------------------------------------------------------------------------------------
sgrq_model_6 <- lm(SGRQ~AGE+gender+COPDSEVERITY+CAT+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_6)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_6)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel6 <- predict(sgrq_model_6)
residualsgrqmodel6 <- residuals(sgrq_model_6)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_6)


## -------------------------------------------------------------------------------------------------------
sgrq_model_7 <- lm(SGRQ~AGE+gender+FEV1+CAT+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_7)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_7)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel7 <- predict(sgrq_model_7)
residualsgrqmodel7 <- residuals(sgrq_model_7)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_7)


## -------------------------------------------------------------------------------------------------------
sgrq_model_8 <- lm(SGRQ~AGE+gender+FVC+CAT+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_8)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_8)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel8 <- predict(sgrq_model_8)
residualsgrqmodel8 <- residuals(sgrq_model_8)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_8)

## -------------------------------------------------------------------------------------------------------
pairs(~FEV1+FVC+CAT+SGRQ+HAD+MWT1Best, data=subset_copd, pch=20,cex=1)


## -------------------------------------------------------------------------------------------------------
sgrq_model_9 <- lm(SGRQ~CAT+HAD, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_9)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_9)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel9 <- predict(sgrq_model_9)
residualsgrqmodel9 <- residuals(sgrq_model_9)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_9)


## -------------------------------------------------------------------------------------------------------
sgrq_model_10 <- lm(SGRQ~CAT+HAD+MWT1Best, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(sgrq_model_10)

## -------------------------------------------------------------------------------------------------------
confint(sgrq_model_10)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel10 <- predict(sgrq_model_10)
residualsgrqmodel10 <- residuals(sgrq_model_10)


## -------------------------------------------------------------------------------------------------------
plot(sgrq_model_10)


## -------------------------------------------------------------------------------------------------------
mlr1 <- lm(SGRQ~CAT+HAD+AGE+gender, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(mlr1)

## -------------------------------------------------------------------------------------------------------
confint(mlr1)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel9 <- predict(mlr1)
residualsgrqmodel9 <- residuals(mlr1)


## -------------------------------------------------------------------------------------------------------
plot(mlr1)


## -------------------------------------------------------------------------------------------------------
mlr2 <- lm(SGRQ~CAT+HAD+AGE+gender+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(mlr2)

## -------------------------------------------------------------------------------------------------------
confint(mlr2)


## -------------------------------------------------------------------------------------------------------
predictedsgrqmodel9 <- predict(mlr2)
residualsgrqmodel9 <- residuals(mlr2)


## -------------------------------------------------------------------------------------------------------
plot(mlr2)


## -------------------------------------------------------------------------------------------------------
mlr3 <- lm(SGRQ~CAT+HAD+AGE+MWT1Best+gender+comorbid+Diabetes+hypertension+AtrialFib+IHD, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(mlr3)

## -------------------------------------------------------------------------------------------------------
confint(mlr3)


## -------------------------------------------------------------------------------------------------------
predictedmlr3 <- predict(mlr3)
residualsmlr3 <- residuals(mlr3)


## -------------------------------------------------------------------------------------------------------
plot(mlr3)

## -------------------------------------------------------------------------------------------------------
imcdiag(mlr3)


## -------------------------------------------------------------------------------------------------------
mlr4 <- lm(SGRQ~CAT+HAD+AGE+MWT1Best+gender+Diabetes+hypertension, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(mlr4)

## -------------------------------------------------------------------------------------------------------
confint(mlr4)


## -------------------------------------------------------------------------------------------------------
predictedmlr4 <- predict(mlr4)
residualsmlr4 <- residuals(mlr4)


## -------------------------------------------------------------------------------------------------------
plot(mlr4)


## -------------------------------------------------------------------------------------------------------
mlr5 <- lm(SGRQ~CAT+HAD+AGE+gender+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(mlr5)

## -------------------------------------------------------------------------------------------------------
confint(mlr5)


## -------------------------------------------------------------------------------------------------------
predictedmlr5 <- predict(mlr5)
residualsmlr5 <- residuals(mlr5)


## -------------------------------------------------------------------------------------------------------
plot(mlr5)

## -------------------------------------------------------------------------------------------------------
imcdiag(mlr5)


## -------------------------------------------------------------------------------------------------------
mlr6 <- lm(SGRQ~CAT+HAD+AGE+FEV1+gender+comorbid, data=subset_copd)


## -------------------------------------------------------------------------------------------------------
summary(mlr6)

## -------------------------------------------------------------------------------------------------------
confint(mlr6)


## -------------------------------------------------------------------------------------------------------
predictedmlr6 <- predict(mlr6)
residualsmlr6 <- residuals(mlr6)


## -------------------------------------------------------------------------------------------------------
plot(mlr6)

