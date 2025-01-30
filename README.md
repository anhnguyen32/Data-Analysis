## Data-Analysis

library(data.table)
library(tidyverse)
library(caret)
library(ggplot2)



getwd()
setwd('/Users/admin/Documents/Analytics Strategy/AY24 CBA')
data<-fread('INF002v4.csv',stringsAsFactors = T)
summary(data)

# Problems
- LOS is an important factor for hospitals to allocate resources and for patients to estimate their cost and insurance claims
- The estimation is done using the mean LOS in each CCSR diagnosis.


# Objectives:
- estimate the LOS more accurately upon admission than using the mean LOS 
- help hospital allocate the resources better 
- help patients estimate the cost based on estimated LOS

-> predictive model

# Tools
- descriptive analysis 
- predictive analysis by linear regression, CART and Random Forest


# Step After
- Check and clean data
- Remove unused data 
- Calculate descriptive
- Run each models 

# Variables:
- Y1: LOS 
- Y2: Est cost

- X:Age, Gender, Type of admissions, Patient disposition, APR.DRG.Description, Severity of illness (descrip), risk of mortality, medical surgical description 

# Initial reviews
- What is the avg los of total record and the avg charges and cosr of patients
- How much is the dif between charge and cost(est charge)
- What is the mean los of these x variables (groups)
- The differences of avg los by each group and by total
- Initial comments are that severity of illness and risk of mortality, medical surgery and emergency will affect heavily on LOS 

 
sum(is.na(data)) # no NA
data<- data[Gender != "U"]



# avg los by CCSR Diagnosis :9.5 days , min is 1, median is 6 , 

summary(data$Length.of.Stay)
outliers <- data$Length.of.Stay[data$Length.of.Stay < 1 | data$Length.of.Stay > 23]
percentage_outliers <- (length(outliers)/ nrow(data)) * 100
print(percentage_outliers)

IQR <- IQR(data$Length.of.Stay)


data_no_outlier <- data[data$Length.of.Stay <23]
summary(data_no_outlier$Length.of.Stay)

# diff between 
data$Total.Charges  <- as.numeric(gsub(",", "", data$Total.Charges))
data$Total.Costs  <- as.numeric(gsub(",", "", data$Total.Costs))


data$Dif_actual_predict_cost <- data$Total.Charges-data$Total.Costs

summary(data$Dif_actual_predict_cost)
count_higheractualcost <- sum(data$Dif_actual_predict_cost < 0)
summary(data$Payment.Typology.1)

library(ggplot2)
library(dplyr)
freq_table <- data %>%
  count(Payment.Typology.1) %>%
  arrange(desc(n)) 

ggplot(freq_table, aes(x = reorder(Payment.Typology.1, -n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +  # Add text labels above bars
  labs(title = "Distribution of Payment Typology",
       x = "Payment Typology",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())



# descriptive stats all datasets

# mean, variance, median los by hospital service area : differences also 
des_area <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Hospital.Service.Area]

des_area_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Hospital.Service.Area]

# mean, variance, median los by age group : pp from 30 to 49 and 50 to 69 has highest avg los 
des_age <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Age.Group]



des_age_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Age.Group]


# mean, variance, median  los by gender: men and women have little difference

des_gen <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Gender]

des_gen_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Gender]

# mean, variance, median  by race: Black/african has significanr higher avg los 
des_race <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Race]

des_race_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Race]



# mean, variance, median  by ethnicity ; Unknown gr has higher -> can skip this variable

des_eth <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Ethnicity]

des_eth_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Ethnicity]

# mean, variance, median  los by admission : also big # among groups

des_admission <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Type.of.Admission]

des_admission_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Type.of.Admission]


severity_distribution <- table(data$Type.of.Admission, data$APR.Severity.of.Illness.Description)
severity_df <- as.data.frame(severity_distribution)
colnames(severity_df) <- c("AdmissionType", "Severity", "Count")

print(severity_distribution) #Extreme and major contribute most out of emergency type -> These could be where hospitals should allocate resources in 

ggplot(severity_df, aes(x = AdmissionType, y = Count, fill = Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Admissions by Severity and Type",
       x = "Admission Type",
       y = "Number of Admissions") +
  theme_minimal()

mortality_distribution <- table(data$Type.of.Admission, data$APR.Risk.of.Mortality)
mortality_df <- as.data.frame(mortality_distribution)
colnames(mortality_df) <- c("AdmissionType", "Mortality", "Count")

print(mortality_distribution) #Extreme and major contribute most out of emergency type -> These could be where hospitals should allocate resources in 

ggplot(mortality_df, aes(x = AdmissionType, y = Count, fill = Mortality)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Admissions by Mortality and Type",
       x = "Admission Type",
       y = "Number of Admissions") +
  theme_minimal()



# mean, variance, median  los of disposition : Differences among types of disposition

des_dispo <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Patient.Disposition]

des_dispo_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Patient.Disposition]

# mean, variance, median vg los by drg : Differences among types of disposition
des_drg <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.DRG.Description]

des_drg_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.DRG.Description]



#mean, variance, median los by severity: Differences among types of severity

des_severity <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.Severity.of.Illness.Description]

des_severity_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.Severity.of.Illness.Description]

#mean, variance, median  by risk of mortality : Differences among types of mortality

des_mortality <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.Risk.of.Mortality]

des_mortality_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.Risk.of.Mortality]


#mean, variance, median los by medical surgical: also different
des_surgical <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.Medical.Surgical.Description]


des_surgical_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = APR.Medical.Surgical.Description]


# mean, variance, median  by emergency: When removing outliers, 2 types have no differences


des_emergency <- data[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Emergency.Department.Indicator]

des_emergency_no <- data_no_outlier[, .(
  mean = mean(Length.of.Stay, na.rm = TRUE),
  median = median(Length.of.Stay, na.rm = TRUE),
  sd = sd(Length.of.Stay, na.rm = TRUE),
  min = min(Length.of.Stay, na.rm = TRUE),
  max = max(Length.of.Stay, na.rm = TRUE),
  n = .N
), by = Emergency.Department.Indicator]



# clean data for modeling:

#combine 6 factors have 1 record into 1 factor Others
#PANCREAS TRANSPLANT, 
#ALLOGENEIC BONE MARROW TRANSPLANT
#AUTOLOGOUS BONE MARROW TRANSPLANT OR T-CELL IMMUNOTHERAPY
#UNGROUPABLE
#HEART AND/OR LUNG TRANSPLANT
#IMPLANTABLE HEART ASSIST SYSTEMS


# final X variables: hospital area, age, race, admissions, disposition, drg, severity, mortality, surgical
data_model <- data[,c('Hospital.Service.Area', 'Age.Group','Race','Length.of.Stay','Patient.Disposition','Type.of.Admission',
                      'APR.DRG.Description','APR.Risk.of.Mortality','APR.Severity.of.Illness.Description')]

## remove empty cells
empty_cells <- sum(data_model == "")
print(empty_cells)

cleaned_data_model <- data_model[rowSums(data_model == "") == 0, ]
# Define the levels to combine
levels_to_combine <- c("PANCREAS TRANSPLANT", 
                       "ALLOGENEIC BONE MARROW TRANSPLANT",
                       "AUTOLOGOUS BONE MARROW TRANSPLANT OR T-CELL IMMUNOTHERAPY",
                       "UNGROUPABLE",
                       "HEART AND/OR LUNG TRANSPLANT",
                       "IMPLANTABLE HEART ASSIST SYSTEMS")

cleaned_data_model$APR.DRG.Description <- factor(cleaned_data_model$APR.DRG.Description, 
                                                 levels = c(levels(cleaned_data_model$APR.DRG.Description), "Others"))

cleaned_data_model$APR.DRG.Description[cleaned_data_model$APR.DRG.Description %in% levels_to_combine] <- "Others"
summary(cleaned_data_model)

cleaned_data_model_no <- cleaned_data_model[cleaned_data_model$Length.of.Stay<23]





# test with LINEAR
library(car)
lm_model <- lm(Length.of.Stay ~ ., data=cleaned_data_model)
summary(lm_model)

lm_model_no <- lm(Length.of.Stay ~ ., data=cleaned_data_model_no)
summary(lm_model_no)

vif(lm_model)

# Prepare data for modeling

install.packages("caTools")
library(caTools)
# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# 70% trainset. Stratify on Y = mpg. Caution: Sample size only 32 in this example.
train <- sample.split(Y = cleaned_data_model$Length.of.Stay, SplitRatio = 0.7)
trainset <- subset(cleaned_data_model, train == T)
testset <- subset(cleaned_data_model, train == F)

train_no <- sample.split(Y = cleaned_data_model_no$Length.of.Stay, SplitRatio = 0.7)
trainset_no <- subset(cleaned_data_model_no, train_no == T)
testset_no <- subset(cleaned_data_model_no, train_no == F)



# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$Length.of.Stay)
summary(testset$Length.of.Stay)

summary(trainset_no$Length.of.Stay)
summary(testset_no$Length.of.Stay)

# Develop model on trainset
lm_prd_model <- lm(Length.of.Stay ~ ., data = trainset)
summary(lm_prd_model)
residuals(lm_prd_model) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.lm_prd_model.train <- sqrt(mean(residuals(lm_prd_model)^2))  # RMSE on trainset based on lm_prd_model model.
summary(abs(residuals(lm_prd_model)))

# Apply model from trainset to predict on testset.
predict.lm_prd_model.test <- predict(lm_prd_model, newdata = testset)
testset.error <- testset$Length.of.Stay - predict.lm_prd_model.test


# Develop model on trainset w/o outliers
lm_prd_model_no <- lm(Length.of.Stay ~ ., data = trainset_no)
summary(lm_prd_model_no)
residuals(lm_prd_model_no) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.lm_prd_model_no.train <- sqrt(mean(residuals(lm_prd_model_no)^2))  # RMSE on trainset based on lm_prd_model model.
summary(abs(residuals(lm_prd_model)))

# Apply model from trainset to predict on testset.
predict.lm_prd_model_no.test <- predict(lm_prd_model_no, newdata = testset_no)
testset_no.error <- testset_no$Length.of.Stay - predict.lm_prd_model_no.test


# running cart
library(rpart)
library(rpart.plot)
set.seed(2020) 
los_cart <- rpart(Length.of.Stay ~ . , data = trainset, method = 'anova', cp = 0)
printcp(los_cart)  
plotcp(los_cart)

CVerror.cap <- los_cart $cptable[which.min(los_cart $cptable[,"xerror"]), "xerror"] + 
  los_cart $cptable[which.min(los_cart $cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree price.cart1.
i <- 1; j<- 4
while (los_cart$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the identified min CP value and the CP above if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(los_cart$cptable[i,1] * los_cart$cptable[i-1,1]), 1)

# Get best tree based on 10 fold CV with 1 SE
los_cart.best <- prune(los_cart, cp = cp.opt)

tail(printcp(los_cart.best))


rpart.plot(los_cart.best, main = "Pruned Regression Tree", type = 4, extra = 101)

# Make predictions on the test set

predicted_LOS <- predict(los_cart.best, newdata = testset)
mse <- mean((testset$Length.of.Stay - predicted_LOS)^2)
print(paste("Mean Squared Error on Test Set:", mse))


node_numbers <- los_cart.best$frame$var
terminal_nodes <- node_numbers[node_numbers == "<leaf>"]
print(terminal_nodes)


##CART w/o outliers
los_cart_no <- rpart(Length.of.Stay ~ . , data = trainset_no, method = 'anova', cp = 0)
CVerror.cap <- los_cart_no $cptable[which.min(los_cart_no $cptable[,"xerror"]), "xerror"] + 
  los_cart_no $cptable[which.min(los_cart_no$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (los_cart_no$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt_no= ifelse(i > 1, sqrt(los_cart_no$cptable[i,1] * los_cart_no$cptable[i-1,1]), 1)
los_cart_no.best <- prune(los_cart_no, cp = cp.opt)
predicted_LOS_no <- predict(los_cart_no.best, newdata = testset_no)
mse <- mean((testset_no$Length.of.Stay - predicted_LOS_no)^2)
print(paste("Root Mean Squared Error on Test Set:", sqrt(mse)))





#Random Forest

install.packages("randomForest")
library(randomForest)
model <- randomForest(Length.of.Stay ~ ., data = data_model, importance = TRUE)

importance_values <- importance(model)
print(importance_values)

RF <- randomForest(Length.of.Stay ~ . , data=trainset)
RF.yhat <- predict(RF, newdata = testset)
RMSE.test.RF <- round(sqrt(mean((testset$Length.of.Stay - RF.yhat)^2)))


num_trees <- RF$ntree
print(paste("Number of trees in the Random Forest:", num_trees))


RSF <- floor(8/3)

##Random Forest w/o outliers

RF_no <- randomForest(Length.of.Stay ~ . , data=trainset_no)
RF_no.yhat <- predict(RF_no, newdata = testset_no)
RMSE.test.RF_no <- round(sqrt(mean((testset_no$Length.of.Stay - RF_no.yhat)^2)))


