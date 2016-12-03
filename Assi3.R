#Import data
datafile <- read.dta("peace.dta")

#Ensure replicability
set.seed(1)

#Split data
train <- sample(c(1:124), 62, replace = FALSE)

#DEFINE THE TRAINING SET 
trainx1 <- datafile$un2int[train] 
trainx2 <- datafile$wartype[train] 
trainx3 <- datafile$treaty[train] 
trainx4 <- datafile$wardur[train] 

trainy <- datafile$pbs2s3[train] 

#DEFINE THE TEST SET 
testx1 <- datafile$un2int[-train] 
testx2 <- datafile$wartype[-train] 
testx3 <- datafile$treaty[-train] 
testx4 <- datafile$wardur[-train] 

testy <- datafile$pbs2s3[-train] 

#Model Fitting
model1 <- glm(trainy ~ trainx1 +trainx2 + trainx3 + trainx4, family = binomial)
summary(model1)

### USE THE MODEL TO PREDICT THE "TRAIN" SET DATA
predicted_trainy <- predict(model1, type = "response") 
#predicted_trainy
summary(predicted_trainy)

## CALCULATE TRAINING ERRORS 
training_errors <- trainy - predicted_trainy 

# WHAT ARE THE MEAN PREDICTED ERRORS 
mean(training_errors) 

#SWAP IN THE TEST SET DATA and CALL IT "NEWDATA"
newdata = data.frame(trainx1=testx1, trainx2=testx2, trainx3=testx3, trainx4=testx4) 

### USE THE MODEL TO PREDICT THE TEST SET DATA
predicted_testy <- predict(model1, new = newdata, type = "response") 
#predicted_testy
summary(predicted_testy)

## CALCULATE TEST ERRORS 
test_errors <- testy - predicted_testy 

# WHAT ARE THE MEAN PREDICTED ERRORS 
mean(test_errors) 

#CONFUSION MATRIX
glm.pred=rep("0",62)
glm.pred[predicted_testy >.5]="1"
table(glm.pred, testy)

#COUNTS
sum(trainx1 == 0)
sum(trainx1 == 1)
sum(testx1 == 0)
sum(testx1 == 1)

sum(datafile$pbs2s3 == 0)
sum(datafile$pbs2s3 == 1)
sum(trainy == 0)
sum(trainy == 1)
sum(testy == 0)
sum(testy == 1)
sum(predicted_testy < 0.5)
sum(predicted_testy > 0.5)

