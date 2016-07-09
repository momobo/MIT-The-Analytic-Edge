setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")
# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.

names(train)
# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer
train = read.csv("train2016.csv", na.strings = c("NA",""))
test  = read.csv("test2016.csv",  na.strings = c("NA",""))

# get list of questions i.e. all columns starting with a 'Q'
questions = grep("^Q",names(train))

# loop over all the questions
for (i in questions) {
  # convert the ith question, note double brackets [[ notation ...
  train[[i]] = addNA(train[[i]]) 
}
questions = grep("^Q",names(test))

# loop over all the questions
for (i in questions) {
  # convert the ith question, note double brackets [[ notation ...
  test[[i]] = addNA(test[[i]]) 
}

train$Income  <- addNA(train$Income)
test$Income   <- addNA(test$Income)


relevant <- c('Q98197', 'Q98869', 'Q99480', 'Q100010', 'Q100562', 'Q100680', 'Q101162', 
              'Q102089', 'Q102674', 'Q103293', 'Q106272', 'Q106388', 'Q108343', 'Q108617', 
              'Q108754', 'Q109244', 'Q109367', 'Q112512', 'Q113181', 'Q114152', 'Q115195', 
              'Q115611', 'Q116441', 'Q116601', 'Q116953', 'Q118117', 'Q119851', 'Q120014', 
              'Q120379', 'Q120650', 'Q122771', 'Q123464', 'Q123621')

SimpleMod = glm(Party ~ Q98197+ Q98869+ Q99480+ Q100010+ Q100562+ Q100680+ Q101162+ 
                  Q102089+ Q102674+ Q103293+ Q106272+ Q106388+ Q108343+ Q108617+ 
                  Q108754+ Q109244+ Q109367+ Q112512+ Q113181+ Q114152+ Q115195+ 
                  Q120379+ Q120650+ Q122771+ Q123464+ Q123621+ Q115611+ Q116441+
                  Q116601+ Q116953+ Q118117+ Q119851+ Q120014+ Q120379+ Q120650+ 
                  Q122771+ Q123464+ Q123621 + Income + YOB , data=train, family=binomial)
summary(SimpleMod)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog3.csv", row.names=FALSE,quote = FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!
