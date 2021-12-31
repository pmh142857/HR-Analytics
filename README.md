# HR Analytics
HR Analytics and Employee performance IBM use of model classifiers to predict whether an employee is likely to quit or not.

## Add library
```
library(VIM)
library(plotrix)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(shiny)
library(DT)
library(shinydashboard)
library(shinythemes)
library(ggthemes)
library(ggcorrplot)
library(cowplot)
library(caret)
library(rpart)
library(corrplot)
library(pROC)
```
## Preview
![1a](https://user-images.githubusercontent.com/56477300/147836224-0f6f3276-3861-4172-9065-e2f332eab84c.jpg)
![1b](https://user-images.githubusercontent.com/56477300/147836227-0920bf00-c338-4204-ac48-2ae1b9980f74.jpg)
![1c](https://user-images.githubusercontent.com/56477300/147836228-667d7109-a2fa-401e-bee0-164fb0dd27ca.jpg)
![1d](https://user-images.githubusercontent.com/56477300/147836229-c7aa6197-fcbf-4b40-a337-81736ef4dfd1.jpg)
![2a](https://user-images.githubusercontent.com/56477300/147836230-33c295ac-1611-448e-892c-56e4ef4a9af2.jpg)
![2b](https://user-images.githubusercontent.com/56477300/147836231-b45e462d-067c-4ab1-9333-d66892de438f.jpg)
![2c](https://user-images.githubusercontent.com/56477300/147836232-2fb28074-610c-40be-aa47-b9fb4a9b004e.jpg)
![4](https://user-images.githubusercontent.com/56477300/147836233-80dd5fb7-e871-4aa5-a660-d9037af7d784.jpg)
![4b](https://user-images.githubusercontent.com/56477300/147836234-98987aa4-9eb6-47b1-9d3b-c391cab4ca5d.jpg)
![5](https://user-images.githubusercontent.com/56477300/147836235-954329c7-1dec-4732-b0a2-6e0355c94867.jpg)
![5a](https://user-images.githubusercontent.com/56477300/147836236-4aa28330-75ba-45dd-842e-b332d8543679.jpg)
![5b](https://user-images.githubusercontent.com/56477300/147836237-c3095d26-f722-4b28-a61c-0f2623b91c52.jpg)
## Create decision tree and define importance of variables
![6](https://user-images.githubusercontent.com/56477300/147836239-1eab53d6-56f6-4d9a-9a8c-3c8ea3693cba.jpg)
```
data_ibm <- data
trainIndex <- createDataPartition(data_ibm$Attrition, p=0.80,list=FALSE, times=1)
train <- data_ibm[trainIndex,]
test <- data_ibm[-trainIndex,]

prop_train <- train %>% select(Attrition) %>%
  group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(Attrition) %>% 
  group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))
rpart.tree <- rpart(Attrition ~ ., data=train)

var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL
colorCount <- length(unique(var_imp$features))

feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  geom_label(aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) + 
  labs(title="", x="Features", y="Importance")
```
## Logistic Regression
### Build the model in some simple steps as follows:
- Determine the independent variables
- Incorporating dependent variable "Attrition" in the model
- Find the variables most important in determining employee attrition
- Convert the model's data type from "character" to "formula"
- Incorporate tranining data into formulas and build models

### Step 1: 35 Variables
```
# First, combine 30 variable dependencies to find meaningful variables
independentvariables1=c("Age", "BusinessTravel", "DailyRate", "Department", "DistanceFromHome", "Education", "EducationField", "EnvironmentSatisfaction", "Gender", "HourlyRate", "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked", "OverTime", "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")
Model1=paste(independentvariables1,collapse="+")
Model_1=paste("Att~",Model1)
formula=as.formula(Model_1)

# Run Training Model1
Trainingmodel1=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel1=step(object = Trainingmodel1,direction = "both")
summary(Trainingmodel1)
```
### Step 2: 17 Variables
```
# After the first scoring iteration, choose the most significant value based on the P-value, which is 17 variables
# Combine 17 dependent variables to find meaningful variables
independentvariables2=c("BusinessTravel", "DistanceFromHome", "EnvironmentSatisfaction","JobInvolvement","JobRole","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TotalWorkingYears", "TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole", "YearsSinceLastPromotion","YearsWithCurrManager")
Model2=paste(independentvariables2,collapse="+")
Model_2=paste("Att~",Model2)
formula=as.formula(Model_2)

# Run Training Model2
Trainingmodel2=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel2=step(object = Trainingmodel2,direction = "both")
summary(Trainingmodel2)
```
### Step 3: 16 Variables
```
# After the second iteration of the calculation, remove the JobRole variable because the JobRoleSales Representative is the only JobRole that matters
# So we remove JobRole as a variant for our model.
# Combine 16 variable dependencies to find meaningful variables
independentvariables3=c("BusinessTravel", "DistanceFromHome", "EnvironmentSatisfaction","JobInvolvement","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TotalWorkingYears", "TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole", "YearsSinceLastPromotion","YearsWithCurrManager")
Model3=paste(independentvariables3,collapse="+")
Model_3=paste("Att~",Model3)
formula=as.formula(Model_3)

# Run Training Model3
Trainingmodel3=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel3=step(object = Trainingmodel3,direction = "both")
summary(Trainingmodel3)
```
### Step 4: 13 Variables
```
# After the third scoring iteration, WorkLifeBalance, YearsAtCompany, YearsWithCurrManager
# Combine 13 variable dependencies to find meaningful variables
independentvariables4=c("BusinessTravel", "DistanceFromHome", "EnvironmentSatisfaction","JobInvolvement","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TotalWorkingYears", "TrainingTimesLastYear","YearsInCurrentRole", "YearsSinceLastPromotion")
Model4=paste(independentvariables4,collapse="+")
Model_4=paste("Att~",Model4)
formula=as.formula(Model_4)

# Run Training Model4
Trainingmodel4=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel4=step(object = Trainingmodel4,direction = "both")
summary(Trainingmodel4)
```
## Graph the ROC for Training Model 1->4 and determine the AUC
![7](https://user-images.githubusercontent.com/56477300/147836240-40e42b90-37c6-4973-84c7-bc248b076710.jpg)
```
troc1=roc(response=Trainingmodel1$y,predictor = Trainingmodel1$fitted.values,plot=T)
troc1$auc

troc2=roc(response=Trainingmodel2$y,predictor = Trainingmodel2$fitted.values,plot=T)
troc2$auc

troc3=roc(response=Trainingmodel3$y,predictor = Trainingmodel3$fitted.values,plot=T)
troc3$auc

troc4=roc(response=Trainingmodel4$y,predictor = Trainingmodel4$fitted.values,plot=T)
troc4$auc
```
[Datasets](https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset)

## Thank You 🐞
- any kind of constructive suggestions are welcome , Please upvote this if you find it useful 💬


