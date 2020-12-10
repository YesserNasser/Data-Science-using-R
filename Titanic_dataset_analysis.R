# load the data 
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add a survived varaiable to the test set to be able to combine both files
# train has a variable survived and test doesn't
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])


#combine data sets
data.combined <- rbind(train, test.survived)

# data types - structure of the data
str(data.combined)

# change the pclass column to a factor (for purpose of machine learning)
data.combined$pclass <- as.factor(data.combined$pclass)
data.combined$survived <- as.factor(data.combined$survived)
data.combined$sex <- as.factor(data.combined$sex)
data.combined$ticket <- as.factor(data.combined$ticket)
data.combined$cabin <- as.factor(data.combined$cabin)
data.combined$embarked <- as.factor(data.combined$embarked)
data.combined$name <- as.factor(data.combined$name)
str(data.combined)

# take a look at the gross survival rates
table(data.combined$survived)
table(data.combined$pclass)

# load up ggplot2 package to use visualization
library(ggplot2)

# Hypothesis - rich survived?
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill=factor(survived))) +
  geom_bar(width=0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill="Survived")

# examine the first few names in the trainig dataset
head(as.character(train$name))

# how many unique names are there across both train and test?
length(unique(as.character(data.combined$name)))


# two duplicate names
# first, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# take a look at the records in the combined data set (%in% == if is in)
data.combined[which(data.combined$name %in% dup.names),]

# whats up with Miss. and Mr.?
library(stringr)
#Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$name,"Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$name,"Mrs.")),]
mrses[1:5,]

males <- data.combined[which(data.combined$sex == "male"),]
males[1:5,]

# add title (Miss. Mr. Master. ... to data frame to extract correlations)

# function to extract title
extractTitle <- function(name){
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else if (length(grep("Master.", name)) > 0){
    return("Master.")
  } else {
    return("Other")
  }
}

# c = combine 
titles <- NULL
for (i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x=title, fill = survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "survived")

table(data.combined$sex)

ggplot(data.combined[1:891,], aes(x=sex, fill=survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined$age)
summary(data.combined[1:891,"age"]) 

ggplot(data.combined[1:891,], aes(x=age, fill=survived))+
  facet_wrap(~sex + pclass) +
  geom_bar(width = 10) +
  xlab("Age")+
  ylab("Total Count")

boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_bar(width = 5) + 
  ggtitle("Age for Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count")

misses_alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses_alone$age)

summary(data.combined$sibsp)
length(unique(data.combined$sibsp))

data.combined$sibsp <- as.factor(data.combined$sibsp)
ggplot(data.combined[1:891,], aes(x=sibsp, fill=survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Tittle") + 
  xlab("Sibsp") + 
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# creating a family size feature:
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

ggplot(data.combined[1:891,],aes(x=family.size, fill=survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

str(data.combined$ticket)
# convert ticket into string
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]

subdata <- substr(data.combined$ticket,1,1)
subdata[1:20]

ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket,1,1))
length(unique(ticket.first.char))

data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=survived)) + 
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

summary(data.combined$fare)
length(unique(data.combined$fare))

ggplot(data.combined[1:891,], aes(x=fare, fill=pclass)) +
  geom_bar(width=5) + 
  ggtitle("Combined Fare Distribution") + 
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50)

ggplot(data.combined[1:891,], aes(x=fare, fill=survived)) +
  geom_bar(width=5) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

str(data.combined$cabin)

data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]

# replace all the empty with a "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]

cabin.first.char <- as.factor(substr(data.combined$cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

#Add this as variable to data.combined
data.combined$cabin.first.char <- as.factor(cabin.first.char)

ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill="Survived")

# folks with multiple cabin - detect all the cabin with space " " as an indicator of multiple cabin 
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x=cabin.multiple, fill=survived)) +
  geom_bar()+
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# survivability and where you got onboard
str(data.combined$embarked)
levels(data.combined$embarked)

ggplot(data.combined[1:891,], aes(x=embarked, fill=survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Survivability with respect to where embarked title") +
  xlab("embarked level") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "survived")

# Building model
library(randomForest)

#train a random Forest with the default parametrs using pclass & title
rf.train.1 <- data.combined[1:891, c("pclass", "title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x=rf.train.1,y=rf.label, importance = TRUE, ntree=1000)
rf.1
varImpPlot(rf.1)

###    Confusion matrix:
##                  0   1 class.error ------- (true labels )
##      0         536  13  0.02367942
##      1         174 168  0.50877193
##
##      |
##      |
## (prediction)

# Train a random Forest using pclass, title, sibsp
rf.train.2 <- data.combined[1:891, c("pclass","title","sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y=rf.label, importance=TRUE, ntree=1000)
rf.2
varImpPlot(rf.2)


# Train a random Forest using pclass, title, sibsp, parch
rf.train.4 <- data.combined[1:891, c("pclass","title","sibsp","parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree=1000)
rf.4
varImpPlot(rf.4)


# train a random Forest using pclass, title, family.size
rf.train.5 <- data.combined[1:891, c("pclass","title","family.size")]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, importance = TRUE, ntree=1000)
rf.5
varImpPlot(rf.5)


# Cross Validation
# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("pclass","title","family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId=rep(892:1309), Survived=rf.5.preds)
write.csv(submit.df, file = "RF_SUB_20200812_1.csv", row.names = FALSE)

library(caret)
library(doSNOW) #submit parallel jobs

# research has shown that 10-fold CV repeated 10 times is the best place to start
# however there are no hard and fast rules - this is where the esperience of the 

# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This 
# is known as stratified cross validation and generally provides better results.

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k=10, times=10) #create 10 different folds

table(rf.label)
342/549
table(rf.label[cv.10.folds[[33]]])
308/494

# Set up caret's trainControl object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

# Set up doSNOW package for multi-core training. This is helpful as we are going
# to be training a lot of trees.
cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)
 
# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x=rf.train.5, y=rf.label, method="rf", tuneLength = 3, 
                   ntree = 1000, trControl = ctrl.1)

# Shutdown Cluster
stopCluster(cl)

# Check out results
rf.5.cv.1

######################################
## 5-fold CV repeated 10 times
######################################

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k=5, times=10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x=rf.train.5, y=rf.label, method="rf", tuneLength=3,
                  ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2

 
######################################
## 3-fold CV repeated 10 times
######################################

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k=3, times=10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x=rf.train.5, y=rf.label, method="rf", tuneLength=3,
                   ntree = 64, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3


############################################
## Modeling 2
############################################

library(rpart)
library(rpart.plot)

rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30,
                    trControl=ctrl)
  
  # shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("pclass","title","family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type =0, extra=1, under=TRUE)

# observation form the tree:
# 1- title Mr. and other are predicted to perish
# 2- title Master, Miss & Mrs. in 1st and 2nd class are predicted to survive
# 3- Miss. and Mrs. and Master. in 3rd class with family sizes equal to 5,6,8, &11
# are predicted to perish with 100% rate 
# 4- Titles of Master, Miss and Mrs in 3rd class with family sizes not equal
# 6,6,8,11 are predicted to survive with 59.6% accuracy

table(data.combined$title)


data.combined[1:25, "name"]

#split names

name.splits <- str_split(data.combined$name, ",")
name.splits[1]

last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# add name to dataframe
data.combined$last.name <- last.names

# split for title
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
name.splits[1:10]
titles <- sapply(name.splits, "[", 2)
unique(titles)

data.combined[which(titles=="the"),]

titles[titles %in% c("Dona.","the")] <- "Lady." #===> which(titles == Dona. | Titles == "the")
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# make title as a factor
data.combined$new.title <- as.factor(titles)

# visualize new version of title
ggplot(data.combined[1:891,], aes(x=new.title, fill=survived)) +
  geom_bar()+
  facet_wrap(~pclass)+
  ggtitle("Survived Rates for new.title by pclass")

# Collapse title based on visual analysis
indexes <- which(data.combined$new.title=="Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."


# visualize
ggplot(data.combined[1:891,], aes(x = new.title, fill=survived)) +
  geom_bar()+
  facet_wrap(~pclass)+
  ggtitle("Survived rates for collapsed new.title by pclass")

features <- c("pclass","new.title","family.size")
rpart.train.2 <- data.combined[1:891, features]

rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

prp(rpart.2.cv.1$finalModel, type =0, extra=1, under=TRUE)


# Dive in on 1st class Mr.
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)


indexes <- which(data.combined$new.title == "Mr." &
                   data.combined$sex == "female")
data.combined$new.title[indexes] <- "Mrs."

length(which(data.combined$sex == "female" &
               data.combined$new.title == "Master." |
               data.combined$new.title == "Mr."))

indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]

# surviving 1st class
summary(first.mr.df[first.mr.df$survived=="1",])
View(first.mr.df[first.mr.df$survived=="1",])

# take a look at some of the high fares
indexes <- which(data.combined$ticket == "PC 17755" |
                   data.combined$ticket == "PC 17611"|
                   data.combined$ticket == "113760")

View(data.combined[indexes,])

# Visualization survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x=fare, fill=survived))+
  geom_density(alpha=0.5)+
  ggtitle("1st Class 'Mr.' Survival Rates by fare")

# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$ticket)


for (i in 1:length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "fare"]/length(party.indexes)
  
  for (k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# refresh 1st class Mr. dataframe
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# Visualize new features
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x=ticket.party.size, fill=survived)) +
  geom_density(alpha=0.5)+
  ggtitle("Survived Rates 1st Class Mr. by ticket.party.size")
  
# Visualize new features
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x=avg.fare, fill=survived)) +
  geom_density(alpha=0.5)+
  ggtitle("Survived Rates 1st Class Mr. by ticket.party.size")

# Hypothesis - Ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

data.combined[is.na(data.combined$avg.fare),]


indexes <- with(data.combined, which(pclass == "3" & title == "Mr." & family.size == "1" & ticket != "3701"))


similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# use Mean to fill the na value
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840


#############################################################
###                   Scaling data 
#############################################################
# Leverage caret's preProcess Function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)


# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)


# just for the 1st class
indexes <- which(data.combined$pclass=="1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$avg.fare[indexes])


##############################################################
###  train the model with new features
##############################################################

# see if engineering has made any differnce
features <- c("pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

#plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

