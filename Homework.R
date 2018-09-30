# ===================== HEAD ===================== 
# Definitions of variables
# titanic.train:  data.frame of the titanic data. Used to train the machine learning model
# titanic.test:   data.frame of the titanic data. Used to test the machine learning model. 
# titanic.full:   data.frame which is the result of the merge of titanic.train and titanic.test.

# Called packages
# install.packages("randomForest")
library(randomForest)

# ===================== READ DATA ===================== 

# Set working directory
setwd("~/Documents/Data science dojo/Titanic Kaggle public")

# Import csv file and define titanic.train and titanic.test
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test  <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# Check dimensions, name of columns=features, and structure of titanic.train and titanic.test
ncol(titanic.train)
ncol(titanic.test)
names(titanic.train)
names(titanic.test)
str(titanic.test)

# ===================== EDIT titanic.train and titanic.test ===================== 

# Create a common column IsTrainset in titanic.train and titanic.test,
# in order to differenciate them in the merged table titanic.full.
titanic.train$IsTrainset <- TRUE
titanic.test$IsTrainset <- FALSE

# Create Survived column in titanic.test and fill in with NA.
titanic.test$Survived <- NA

# Check columns of titanic.train and titanic.test before merging
names(titanic.train)
names(titanic.test)

# ===================== CREATE titanic.full ===================== 

# Create titanic.full by merging datasets titanic.train and titanic.test. Check the results
titanic.full <- rbind(titanic.train,  titanic.test)
head(titanic.full)
dim(titanic.train)
dim(titanic.test)
dim(titanic.full)

# ===================== CLEAN COMMON DATA of titanic.train AND titanic.test THROUGH titanic.full ===================== 

# ===================== CLEAN COMMON DATA - as.factor for category in titanic.full ===================== 

# Check structure of titanic.full
# Change type of Pclass, Sex and Embarked as factor in titanic.full. Check results
# TODO Why Embarked should be put as factor type? 
# Answer: it was not done to Cabin because each observation is unique, and not a class with multiple observations
#str(titanic.full)
titanic.full$Pclass   <- as.factor(titanic.full$Pclass)
titanic.full$Sex      <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- factor(titanic.full$Embarked, exclude = NA)
#str(titanic.full)

# ===================== CLEAN COMMON DATA - Embarked IN titanic.full ===================== 

# Show lines in titanic.full where Embarked data is missing and substitute the missing data by S.
# Check result
table(titanic.full$Embarked == "")

  # Option 1
  titanic.full[ titanic.full$Embarked == "", "Embarked"] <- "S"

  # Option 2
  # Define the feature on which the prediction of Embarked rely on.
  # Apply a linear model to predict the missing data of Embarked
  #titanic.full[(titanic.full$Embarked == ""), "Embarked"] <- NA
  #embarked.equation = "Embarked ~ Pclass + Sex + Age + SibSp + Parch + Fare"
  #embarked.predictMissingData.model <- lm( formula = embarked.equation, data = titanic.full[-is.na(titanic.full$Embarked), ])

  # Find lines where Embarked is missing in titanic.full and show only certain columns.
  # Extract from the linear model the predicted values of Fare, and show its row.
  #missingEmbarked.row <- titanic.full[is.na(titanic.full$Embarked), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
  #prediction.embarked <- predict( embarked.predictMissingData.model, newdata = missingEmbarked.row)
  
  # Substitute missing values by the predictions, and check that no more rows with missing exists
  #titanic.full[ is.na(titanic.full$Embarked), "Embarked"] <- prediction.embarked
  
table(titanic.full$Embarked == "")

# ===================== CLEAN COMMON DATA - Age IN titanic.full ===================== 

# Show lines in titanic.full where Age data is missing, compute age median of available data according to sex.
# Substitute the missing data by the median.
# TODO the split of the median vs sex decrease the precision from 0.77033 to 0.76555
# Check result
table(is.na(titanic.full$Age))

  # Option 1
  #median.female <- median(titanic.full[ titanic.full$Sex == "female" , "Age" ], na.rm = TRUE)
  #median.male   <- median(titanic.full[ titanic.full$Sex == "male"   , "Age" ], na.rm = TRUE)
  
  #titanic.full[ is.na(titanic.full$Age) & titanic.full$Sex == "female", "Age"] <- median.female
  #titanic.full[ is.na(titanic.full$Age) & titanic.full$Sex == "male", "Age"]   <- median.male

  #Option 2
  upper.whisker.female.age <- boxplot.stats(titanic.full[ titanic.full$Sex == "female" , "Age" ])$stats[5]
  upper.whisker.male.age   <- boxplot.stats(titanic.full[ titanic.full$Sex == "male" , "Age" ])$stats[5]

  outlier.booleanFilter.female.age <- titanic.full$Age < upper.whisker.female.age
  outlier.booleanFilter.male.age   <- titanic.full$Age < upper.whisker.male.age
  
  # Define the feature on which the prediction of Age rely on.
  # Apply a linear model to predict the missing data of Age
  age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
  female.age.predictMissingData.model <- lm( formula = age.equation, data = titanic.full[outlier.booleanFilter.female.age, ])
  male.age.predictMissingData.model   <- lm( formula = age.equation, data = titanic.full[outlier.booleanFilter.male.age, ])
  
  # Find lines where Fare is missing in titanic.full and show only certain columns.
  # Extract from the linear model the predicted values of Fare, and show its row.
  missingAge.female.row <- titanic.full[is.na(titanic.full$Age) & titanic.full$Sex == "female", c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
  missingAge.male.row   <- titanic.full[is.na(titanic.full$Age) & titanic.full$Sex == "male", c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
  
  prediction.age.female <- predict( female.age.predictMissingData.model, newdata = missingAge.female.row)
  prediction.age.male <-   predict( male.age.predictMissingData.model, newdata = missingAge.male.row)
  
  # Substitute missing values by the predictions, and check that no more rows with missing exists
  titanic.full[ is.na(titanic.full$Age) & titanic.full$Sex == "female", "Age"] <- prediction.age.female
  titanic.full[ is.na(titanic.full$Age) & titanic.full$Sex == "male", "Age"]   <- prediction.age.male
  
table(is.na(titanic.full$Age))

# ===================== CLEAN COMMON DATA - Fare IN titanic.full ===================== 

# Show lines in titanic.full where Fare data is missing and substitute the missing data by median.
# Check result
table(is.na(titanic.full$Fare))
  # Option 1
  #titanic.full[ is.na(titanic.full$Fare), "Fare"] <- median(titanic.full$Fare, na.rm = TRUE)

  # Option 2
  upper.whisker.fare <- boxplot.stats(titanic.full$Fare)$stats[5]
  outlier.booleanFilter.fare <- titanic.full$Fare < upper.whisker.fare
  
  # Define the feature on which the prediction of Fare rely on.
  # Apply a linear model to predict the missing data of Fare.
  fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
  fare.predictMissingData.model <- lm( formula = fare.equation, data = titanic.full[outlier.booleanFilter.fare, ])
  
  # Find lines where Fare is missing in titanic.full and show only certain columns.
  # Extract from the linear model the predicted values of Fare, and show its row.
  missingFare.row <- titanic.full[is.na(titanic.full$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
  prediction.fare <- predict( fare.predictMissingData.model, newdata = missingFare.row)
 
  # Substitute missing values by the predictions, and check that no more rows with missing exists
  titanic.full[ is.na(titanic.full$Fare), "Fare"] <- prediction.fare

table(is.na(titanic.full$Fare))



# ===================== SPLIT BACK OUT DATA FROM PARTIALLY CLEANED titanic.full INTO titanic.train AND titanic.test ===================== 

# Update titanic.train and titanic.test with the cleaned data from titanic.full
titanic.train <- titanic.full[titanic.full$IsTrainset == TRUE, ]
titanic.test  <- titanic.full[titanic.full$IsTrainset == FALSE, ]

# Clean Survived data of titanic.train by converting as factor.
# Survived data in titanic.test = NA and cannot be considered as factor.
titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

# ===================== RUN randomForest WITH titanic.full ===================== 

# Create parameters for random forest, and convert into formula type.
survived.equation <- "Survived ~  Sex + Age + SibSp + Parch + Fare + Embarked" # Select some feature
# survived.equation <- "Survived ~ ." # Use all features, but it does not work
survived.formula  <- as.formula(survived.equation)

# Create randomForest
# TODO are the random bagging is done in randomForest()? What is the relation with the previous splitting
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodsize = 0.01*nrow(titanic.test), importance = TRUE)

# TODO NOT USED
# feature.equation <- "Pclass + Sex + SibSp + Parch + Fare + Embarked" 

# Store the prediction results in Survived
Survived <- predict(titanic.model, newdata = titanic.test)

# Plot importance and variance
importance(titanic.model)
varImpPlot(titanic.model)


# TODO Add GINI and other metrics

# ===================== EXPORT PREDICTION RESULTS IN DATAFRAME AND CSV ===================== 

# Create empty data.frame output.df to store results with PassengerID and Survived
output.df <- data.frame(PassengerId = integer(nrow(titanic.test)), Survived=factor(nrow(titanic.test)))
output.df$PassengerId <- titanic.test$PassengerId
output.df$Survived <- Survived

# Export the results output.df into csv file
# kaggle_submission_1.csv     Delivery 1: 
# kaggle_submission_2.csv     Delivery 2: 
# kaggle_submission_3.csv     Delivery 3: Added split on the median Age when cleaning the Age
# kaggle_submission_4.csv     Delivery 4: Added Age to the randomForest model
# kaggle_submission_5.csv     Delivery 5: Prediction done for missing data of Age and Fare
# kaggle_submission_6.csv     Delivery 6: CANCELED Prediction done for missing data of Embarked

# write.csv(output.df, file = "kaggle_submission_5.csv", row.names = FALSE)

# ===================== END ===================== 