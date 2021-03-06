# For our first example we will use the 'bands' dataset
View(bands)
summary(bands)

# We need to do some cleaning on this data
myData = as.data.frame(unclass(bands))
summary(myData)
myData[myData == "?"]=NA
summary(myData)

# Many of the predictors seem useless and can probably
# be dropped for a first model effort. In particular:

myData$X1 = NULL
myData$X2 = NULL
myData$X3 = NULL
myData$X4 = NULL
myData$X6 = NULL
myData$X7 = NULL
myData$X8 = NULL
myData$X9 = NULL
myData$X12 = NULL
myData$X13 = NULL
myData$X18 = NULL

# We need to reset the levels on some of the variables
levels(myData$X10)=c("COATED","COATED","COATED","UNCOATED","UNCOATED")
levels(myData$X11)=c("COATED","COATED","COVER","COVER","UNCOATED","UNCOATED")

for(i in 10:28){
  myData[,i] = as.numeric(as.character(myData[,i]))
}

# Several more variables are of questionable value - 
# leave out in this frist draft of a model

myData$X32 = NULL
myData$X33 = NULL
myData$X39 = NULL
myData$X19 = NULL

# Finally, there are a few low frequency values to be removed

myData = myData[myData$X11!= 'COVER',]
myData = myData[myData$X15!= 'Motter70',]
myData = na.omit(myData)

train = sample(342,250)

# Let's first try Logistic Regression
lr.bands = glm(X40~., data=myData, subset=train, family = binomial)
lr.pred = rep(0,92)
lr.probs = predict(lr.bands, myData[-train,], type="response")
lr.pred[lr.probs>.5]=1
table(lr.pred,myData[-train,]$X40)

# Next we'll try Bagging - let's see how many predictors we have
dim(myData)

# Attach the 'randomForest' library
bag.bands = randomForest(X40~., data=myData,subset=train,mtry=24,importance=TRUE)

# The predict function will give us the actual class prediction, 
# not probs
bag.bands.pred = predict(bag.bands,myData[-train,],type="class")
table(bag.bands.pred,myData[-train,]$X40)

# Comparable to LR. Let's try a Random Forest with 6 predictors
rf.bands = randomForest(X40~., data=myData,subset=train,mtry=6,importance=TRUE)
rf.bands.pred = predict(rf.bands,myData[-train,],type="class")
table(rf.bands.pred,myData[-train,]$X40)

# Try to build a classifier for the wine quality datasets ...





