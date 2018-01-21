# credit card and personal information
# problem statement: some of the customer defaulted in making the payment on balance due


getwd()

setwd("E:\\Vineeth\\R-DS")
getwd()

fulldata <- read.csv("Log-Reg-Case-Study.csv")

View(fulldata) # open fulldata to ensure all the rows and columns are imported

# data partition to create modeldata and validation data
# set.seed to randomly sort the data
# 70% data is cpoied to modeldata and the remaining to validation data

set.seed(123)
nrow(fulldata)
train <- sample(1:nrow(fulldata), nrow(fulldata)*.7)

sample(train)

test = -train

modeldata <- fulldata[train,]
validationdata <- fulldata[test,]

#************************************************************************

# step-1 - Univariate analysis
# Summary returns the min,avg.,1st,2nd & 3rd quartile of numeric variable
# for categorial variables, it gives the count by category(upto 6 category, rest as others)
# missing value will be displayed as blanks or NAs(for eg. refer "Housing" output)
levels(modeldata$Purpose_Credit_Taken)
summary(modeldata)

# outlier Identification
quantile(modeldata$Age, 0.995) # display the 99.5th percentile value
quantile(modeldata$age, 0.999) # displays the 99.9th percentile value

# Outlier capping
# If condition to change all age value above 75 as 75

summary(modeldata$Age)
modeldata$Age <- ifelse(modeldata$Age > 75,75,modeldata$Age)
summary(modeldata$Age)
# missing value imputation
# if condition to fill the missing value in housing

summary(modeldata$Housing)
modeldata$Housing[modeldata$Housing == ""] <- "A152"
# for categorial variable, missing value can be replaced with the category with the highest fre
summary(modeldata$Housing)

#***********************************************************************

### Step 2-Bivariate analysis
# table will create a pivot table with the first parameter as row and second as column
# like a pivot table, aggregate provides the option to see summary by count(length), avg. etc

# variable reduction
?table # table uses cross classifying factor to build a contigency table of the counts
# at each combination of factor level
?aggregate # split the data into subsets,computes summary statistics and
# returns the result in a convenient form.
table(modeldata$Num_Dependents,modeldata$Default_On_Payment)
aggregate(modeldata$Num_Dependents, by = list(modeldata$Num_Dependents,
                                              modeldata$Default_On_Payment),length)

# missing value imputation
table(modeldata$Job_Status,modeldata$Default_On_Payment)
aggregate(modeldata$Job_Status, by = list(modeldata$Job_Status,
                                          modeldata$Default_On_Payment),length)
modeldata$Job_Status[modeldata$Job_Status==""] <- "A174"
summary(modeldata$Job_Status)

#*************************************************************************
# category grouping for dummy variable creation
# set 1 - convert categorial variable to dummy variable

# Dummy variable for job status

modeldata$dummy_job_status_A171 <- ifelse(modeldata$Job_Status=="A171",1,0)
modeldata$dummy_job_status_A172 <- ifelse(modeldata$Job_Status=="A172",1,0)
modeldata$dummy_job_status_A173 <- ifelse(modeldata$Job_Status=="A173",1,0)

# Dummy variable for purpose_credit_taken
table(modeldata$Purpose_Credit_Taken,modeldata$Default_On_Payment)

modeldata$dummy_purpose_credit_taken_low <- ifelse(modeldata$Purpose_Credit_Taken == "P41" |
                                                     modeldata$Purpose_Credit_Taken == "P43" |
                                                     modeldata$Purpose_Credit_Taken == "P48",1,0)

modeldata$dummy_purpose_credit_taken_high <- ifelse(modeldata$Purpose_Credit_Taken=="P40" |
                                                      modeldata$Purpose_Credit_Taken=="P49" |
                                                      modeldata$Purpose_Credit_Taken=="P45" |
                                                      modeldata$Purpose_Credit_Taken=="P50" |
                                                      modeldata$Purpose_Credit_Taken=="P46",1,0)
# Dummy variable for status_checking_account

table(modeldata$Status_Checking_Accnt,modeldata$Default_On_Payment)

modeldata$dummy_status_checking_account_high <- ifelse(modeldata$Status_Checking_Accnt=="S11",1,0)

modeldata$dummy_status_checking_account_medium <- ifelse(modeldata$Status_Checking_Accnt=="S12",1,0)

#Dummy variable for credit_history 
table(modeldata$Credit_History,modeldata$Default_On_Payment)

modeldata$dummy_credit_History_high <- ifelse(modeldata$Credit_History=="A30" |
                                                modeldata$Credit_History=="A31",1,0)
modeldata$dummy_credit_History_low <- ifelse(modeldata$Credit_History=="A34",1,0)

# Dummy variable for Years_At_Present_Employement

table(modeldata$Years_At_Present_Employment,modeldata$Default_On_Payment)

modeldata$dummy_Years_At_Present_Employment_high <- ifelse(modeldata$Years_At_Present_Employment=="E71" |
                                                             modeldata$Years_At_Present_Employment=="E72",1,0)
modeldata$dummy_Years_At_Present_Employment_medium <- ifelse(modeldata$Years_At_Present_Employment=="E73",1,0)

# Dummy variable for maritial_status_gender

table(modeldata$Marital_Status_Gender,modeldata$Default_On_Payment)

modeldata$dummy_maritial_status_gender <- ifelse(modeldata$Marital_Status_Gender=="A91" |
                                                   modeldata$Marital_Status_Gender=="A92",1,0)

# Dummy variable for other_debtors_Guarantors
table(modeldata$Other_Debtors_Guarantors,modeldata$Default_On_Payment)

modeldata$dummy_other_debtors_Guarantors <- ifelse(modeldata$Other_Debtors_Guarantors=="A103",1,0)

# Dummy variable for housing
table(modeldata$Housing,modeldata$Default_On_Payment)

modeldata$dummy_housing <- ifelse(modeldata$Housing == "A152",0,1)

#Dummy Variable for foriegn worker
table(modeldata$Foreign_Worker,modeldata$Default_On_Payment)
modeldata$dummy_Foreign_Worker <- ifelse(modeldata$Foreign_Worker == "A201",1,0)

View(modeldata)

# set 2- convert numeric variable to dummy variables
# Numeric variables can be passed as is to the model or as dummy variables
# Check both options and includes the one that shows better performance

# Dummy variable for age
table(modeldata$Age,modeldata$Default_On_Payment)

modeldata$dummy_age_group <- ifelse(modeldata$Age < 30,1,0)

# Dummy variable for credit ammount

modeldata$dummy_credit_amount <- ifelse(modeldata$Credit_Amount < 5000,0,1)

# Dummy variable for current_address_yrs 
modeldata$dummy_current_address_yrs <- ifelse(modeldata$Current_Address_Yrs==1,0,1)

# Num_Credit and Num_Dependents do not show singnificant relationship with the dep.var.

#***************************************************************************************

###Step 3 - Multicollinearity Check

# install.packages("car") # first run library to activate car
# but if it is not installed then install packages 
install.packages("car")
library(car) # R package for multicollinearity analysis

# lm stands for linear model
# All variables to be analyzed are to be included in equation, with dataset's name at the end
attach(modeldata)
vif_output <- lm(Default_On_Payment ~ dummy_job_status_A171 + dummy_job_status_A172 + dummy_job_status_A173 + dummy_purpose_credit_taken_high + dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_Years_At_Present_Employment_medium + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_Foreign_Worker + dummy_age_group + dummy_credit_amount + dummy_current_address_yrs + Duration_in_Months, data = modeldata)

# view vif_output

vif(vif_output)

# Dummy_job_status_A172 & Dummy_job_status_A173 have VIF > 2 -> 1 variables to be removed

# Regress both variables seprately and retain the one with higher R-square

multicoll_out1 <- lm(Default_On_Payment ~ dummy_job_status_A172, data = modeldata)
multicoll_out2 <- lm(Default_On_Payment ~ dummy_job_status_A173, data = modeldata)
summary(multicoll_out1)
summary(multicoll_out2)
# Remove Dummy_job_status_A173 & Repeat VIf

vif_output <- lm(Default_On_Payment ~ dummy_job_status_A171 + dummy_job_status_A172 + dummy_purpose_credit_taken_high + dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_Years_At_Present_Employment_medium + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_age_group + dummy_credit_amount + dummy_current_address_yrs + Duration_in_Months, data = modeldata)

vif(vif_output)

# Dummy_purpose_credit_taken_high & Dummy_purpose_credit_taken_low have vif > 1.5

# Regress both variables separately and retain the one with higher R-square

multicoll_out3 <- lm(Default_On_Payment ~ dummy_purpose_credit_taken_high , data=modeldata)

multicoll_out4 <- lm(Default_On_Payment ~ dummy_purpose_credit_taken_low , data = modeldata)

summary(multicoll_out3)
summary(multicoll_out4)

vif_output <- lm(Default_On_Payment ~ dummy_job_status_A171 + dummy_job_status_A172 + dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_Years_At_Present_Employment_medium + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_age_group + dummy_credit_amount + dummy_current_address_yrs + Duration_in_Months , data=modeldata)

vif(vif_output)
# All vif values < 1.5 hence good to proceed to next step

LogReg_output <- glm(Default_On_Payment ~ dummy_job_status_A171 + dummy_job_status_A172 + dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_Years_At_Present_Employment_medium + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_age_group + dummy_credit_amount + dummy_current_address_yrs + Duration_in_Months ,family = binomial(logit), data=modeldata)
summary(LogReg_output,direction="forward")
LogReg_output <- glm(Default_On_Payment ~ dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_age_group + dummy_current_address_yrs + Duration_in_Months ,family = binomial(logit), data=modeldata)
summary(LogReg_output,direction="forward")
LogReg_output <- glm(Default_On_Payment ~ dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_current_address_yrs + Duration_in_Months ,family = binomial(logit), data=modeldata)
summary(LogReg_output,direction="forward")

modeldata$Predicted_Probability <- predict(LogReg_output,modeldata,type="response")
View(modeldata)

#####validation data######

validationdata$Age <- ifelse(validationdata$Age > 75,75,validationdata$Age)
validationdata$Housing[validationdata$Housing == ""] <- "A152"
validationdata$Job_Status[validationdata$Job_Status==""] <- "A174"

# Dummy variable for job status

validationdata$dummy_job_status_A171 <- ifelse(validationdata$Job_Status=="A171",1,0)
validationdata$dummy_job_status_A172 <- ifelse(validationdata$Job_Status=="A172",1,0)
validationdata$dummy_job_status_A173 <- ifelse(validationdata$Job_Status=="A173",1,0)

# Dummy variable for purpose_credit_taken
table(validationdata$Purpose_Credit_Taken,validationdata$Default_On_Payment)

validationdata$dummy_purpose_credit_taken_low <- ifelse(validationdata$Purpose_Credit_Taken == "P41" |
                                                          validationdata$Purpose_Credit_Taken == "P43" |
                                                          validationdata$Purpose_Credit_Taken == "P48",1,0)

validationdata$dummy_purpose_credit_taken_high <- ifelse(validationdata$Purpose_Credit_Taken=="P40" |
                                                           validationdata$Purpose_Credit_Taken=="P49" |
                                                           validationdata$Purpose_Credit_Taken=="P45" |
                                                           validationdata$Purpose_Credit_Taken=="P50" |
                                                           validationdata$Purpose_Credit_Taken=="P46",1,0)
# Dummy variable for status_checking_account

table(validationdata$Status_Checking_Accnt,validationdata$Default_On_Payment)

validationdata$dummy_status_checking_account_high <- ifelse(validationdata$Status_Checking_Accnt=="S11",1,0)

validationdata$dummy_status_checking_account_medium <- ifelse(validationdata$Status_Checking_Accnt=="S12",1,0)

#Dummy variable for credit_history 
table(validationdata$Credit_History,validationdata$Default_On_Payment)

validationdata$dummy_credit_History_high <- ifelse(validationdata$Credit_History=="A30" |
                                                     validationdata$Credit_History=="A31",1,0)
validationdata$dummy_credit_History_low <- ifelse(validationdata$Credit_History=="A34",1,0)

# Dummy variable for Years_At_Present_Employement

table(validationdata$Years_At_Present_Employment,validationdata$Default_On_Payment)

validationdata$dummy_Years_At_Present_Employment_high <- ifelse(validationdata$Years_At_Present_Employment=="E71" |
                                                                  validationdata$Years_At_Present_Employment=="E72",1,0)
validationdata$dummy_Years_At_Present_Employment_medium <- ifelse(validationdata$Years_At_Present_Employment=="E73",1,0)

# Dummy variable for maritial_status_gender

table(validationdata$Marital_Status_Gender,validationdata$Default_On_Payment)

validationdata$dummy_maritial_status_gender <- ifelse(validationdata$Marital_Status_Gender=="A91" |
                                                        validationdata$Marital_Status_Gender=="A92",1,0)

# Dummy variable for other_debtors_Guarantors
table(validationdata$Other_Debtors_Guarantors,validationdata$Default_On_Payment)

validationdata$dummy_other_debtors_Guarantors <- ifelse(validationdata$Other_Debtors_Guarantors=="A103",1,0)

# Dummy variable for housing
table(validationdata$Housing,validationdata$Default_On_Payment)

validationdata$dummy_housing <- ifelse(validationdata$Housing == "A152",0,1)

#Dummy Variable for foriegn worker
table(validationdata$Foreign_Worker,validationdata$Default_On_Payment)
validationdata$dummy_Foreign_Worker <- ifelse(validationdata$Foreign_Worker == "A201",1,0)

View(validationdata)

# Dummy variable for age
table(validationdata$Age,validationdata$Default_On_Payment)

validationdata$dummy_age_group <- ifelse(validationdata$Age < 30,1,0)

# Dummy variable for credit ammount

validationdata$dummy_credit_amount <- ifelse(validationdata$Credit_Amount < 5000,0,1)

# Dummy variable for current_address_yrs 
validationdata$dummy_current_address_yrs <- ifelse(validationdata$Current_Address_Yrs==1,0,1)

#Multicollinearity check

vif_output <- lm(Default_On_Payment ~ dummy_purpose_credit_taken_low + dummy_status_checking_account_high + dummy_status_checking_account_medium + dummy_credit_History_high + dummy_credit_History_low + dummy_Years_At_Present_Employment_high + dummy_maritial_status_gender + dummy_other_debtors_Guarantors + dummy_housing + dummy_Foreign_Worker + dummy_current_address_yrs + Duration_in_Months, data=validationdata)
vif(vif_output)

validationdata$Predicted_Probability <- predict(LogReg_output,validationdata,type="response")
View(validationdata)

modeldata$Final_Prediction <- ifelse(modeldata$Predicted_Probability > 0.3,1,0)
validationdata$Final_Prediction <- ifelse(validationdata$Predicted_Probability > 0.3,1,0)
