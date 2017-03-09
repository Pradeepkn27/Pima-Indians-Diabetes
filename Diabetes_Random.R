#Random Forest

# Load library
library(randomForest)
library(caret)


#read data
diabetes_pima = read.csv('E:\\Pradeep_pc_backup\\Pradeep_pc_backup\\Data_Science_Material\\Data_science_certification\\pima-indians-diabetes.csv')

names(diabetes_pima) <- c("numpreg", "plasmacon", "bloodpress", "skinfold", "seruminsulin", "BMI", 
                          "pedigreefunction", "age", "classvariable")

diabetes_pima = as.data.frame(diabetes_pima)

#Checking for NA
table(is.na(diabetes_pima))


summary(diabetes_pima)

cor(diabetes_pima[,unlist(lapply(diabetes_pima, is.numeric))])

# No of positive and negative cases
table(diabetes_pima$classvariable)


boxplot(diabetes_pima$age~diabetes_pima$classvariable,data=diabetes_pima, main="Number of diabetes", 
         xlab="Diabetes positive or negative", ylab="Age")
        
boxplot(diabetes_pima$numpreg~diabetes_pima$classvariable,data=diabetes_pima, main="Number of diabetes", 
        xlab="Diabetes positive or negative", ylab="numpreg")


#removing zeros in some of the variables



# Sample the data into training and test data
sample.ind <- sample(2, 
                     nrow(diabetes_pima),
                     replace = T,
                     prob = c(0.6,0.4))


diabetes_pima.data.dev <- diabetes_pima[sample.ind==1,]
diabetes_pima.data.val <- diabetes_pima[sample.ind==2,]


rf.form <- as.formula(as.factor(classvariable) ~ numpreg + plasmacon + bloodpress + skinfold + seruminsulin + BMI + pedigreefunction + age )

diabetes_pima.data.dev.rf <- randomForest(rf.form,
                                          diabetes_pima.data.dev,
                                          ntree=500,
                                          importance=T)

print(diabetes_pima.data.dev.rf) 

plot(diabetes_pima.data.dev.rf)

# Variable Importance Plot
varImpPlot(diabetes_pima.data.dev.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)


var.imp <- data.frame(importance(diabetes_pima.data.dev.rf,
                                 type=2))

# Predicting response variable
diabetes_pima.data.dev$predicted.response <- (predict(diabetes_pima.data.dev.rf ,diabetes_pima.data.dev))

# Create Confusion Matrix
table(predict=diabetes_pima.data.dev$predicted.response, truth=diabetes_pima.data.dev$classvariable)


diabetes_pima.data.val$predicted.response <- (predict(diabetes_pima.data.dev.rf ,diabetes_pima.data.val))
table(predict=diabetes_pima.data.val$predicted.response, truth=diabetes_pima.data.val$classvariable)


p <- predict(diabetes_pima.data.dev.rf, newdata=diabetes_pima.data.val)
pr <- prediction(p, diabetes_pima.data.val$classvariable)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


diabetes_pima[,"classvariable"] = as.factor(diabetes_pima[,"classvariable"])
set.seed(1234)

# Cross Validation with value of k = 10


# load the library
library(caret)
# define training control
train_control <- trainControl(method="cv", number=10)
# train the model
model <- train(classvariable~., data=diabetes_pima, trControl=train_control, method="rf", tuneGrid=NULL)
# summarize results
print(model)
