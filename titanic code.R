# Loading raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable in test set to combine the train and test data
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine dataset
data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Survival rates
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

# Change Pclass to factor
train$Pclass <- as.factor(train$Pclass)
str(train)
train$Survived <- as.factor(train$Survived)

# Load ggplot2 for visualization
library(ggplot2)
ggplot(train, aes(x = Pclass, fill=Survived)) + 
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs (fill="Survived")

# First few names in the train set
head(as.character(train$Name))

# Length of the unique names
length(unique(as.character(data.combined$Name)))

# Check the duplicated names 
dups <- duplicated(as.character(data.combined$Name))
dup.names <- as.character(data.combined[which(dups), "Name"])

# Take a look at the duplicated names in the combined dataset
data.combined[which(data.combined$Name %in% dup.names),]

# Correlation with other variables
library(dplyr)
library(stringr)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

extractTitle <- function(Name) {
  Name <- as.character(Name)
  if (length(grep("Miss.", Name)) >0){
    return ("Miss.")
  } else if  (length(grep("Master.", Name)) >0){
    return ("Master.") 
  } else if  (length(grep("Mrs.", Name)) >0){
    return ("Mrs.") 
  } else if  (length(grep("Mr.", Name)) >0){
    return ("Mr.") 
  } else {
    return ("Other")
  }
}  
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

# Sex, Pclass and survival visualization
data.combined$title <- as.factor(titles)
ggplot(data.combined[1:891,], aes(x = title, fill = Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")

# Check if Master is title for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

# Checking the Miss title
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

# Young girls travelling alone
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$age)

length(which(misses.alone$Age <=14.5))

# Sibsp variable
summary(data.combined$SibSp)
length(unique(data.combined$SibSp))

# Sibsp as a factor

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Sibsp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

# Parch variable as a factor visualization
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

# Creating a family size feature
temp.sibsp <- c(train$SibSp , test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch +1)

ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("family.size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

# Ticket variable as a string rather than factor
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]
ticket.fchar <- ifelse(data.combined$Ticket == "", " ", 
                       substr(data.combined$Ticket, 1, 1))
unique(ticket.fchar)

# ticket.fchar as a factor for analysis and visualization
data.combined$ticket.fchar <- as.factor(ticket.fchar)

ggplot(data.combined[1:891,], aes(x = ticket.fchar, fill = Survived))+
  geom_bar()+
  ggtitle("Ticket first character")+
  xlab("ticket.fchar")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

ggplot(data.combined[1:891,], aes(x = ticket.fchar, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("ticket.fchar")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

ggplot(data.combined[1:891,], aes(x = ticket.fchar, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("ticket.fchar")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

# Fare
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Fare distribution
ggplot(data.combined, aes(x = Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Fare distribution")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,200)

# Checking it's predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,50)+
  labs(fill="Survived")

# Cabin analysis
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)

# Replace empty cabins with U
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#Cabin first character
cabin.fchar <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.fchar)
levels(cabin.fchar)

# Add it to combined dataset and visualize
data.combined$cabin.fchar <- cabin.fchar

ggplot(data.combined[1:891,], aes(x = cabin.fchar, fill = Survived))+
  geom_bar()+
  ggtitle("cabin first character")+
  xlab("cabin.fchar")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

ggplot(data.combined[1:891,], aes(x = cabin.fchar, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("cabin first character survival")+
  xlab("cabin.fchar")+
  ylab("Total Count")+
  ylim(0,500)+
  labs(fill="Survived")

# multiple cabins
data.combined$Cabin.multiple <- as.factor(ifelse
                                          (str_detect(data.combined$Cabin, " "),
                                           "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("multiple cabin survival")+
  xlab("cabin.multiple")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

# Embarked variable
str(data.combined$Embarked)
data.combined$Embarked <- as.factor(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], 
       aes(x = Embarked, fill = Survived))+
  geom_bar()+
  ggtitle("embarked survival rate")+
  xlab("embarked")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + title)+
  ggtitle("embarked survival rate")+
  xlab("embarked")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

# Exploratory Modeling ----
# Random Forest training using pclass and title
library(randomForest)
rf.train1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived) 

set.seed(1234)
rf1 <- randomForest(x=rf.train1, y=rf.label, importance = TRUE, ntree = 1000)
rf1

# Random Forest training using pclass, title and sibsp
rf.train2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]
set.seed(1234)
rf2 <- randomForest(x=rf.train2, y=rf.label, importance = TRUE, ntree = 1000)
rf2

# Random Forest training using pclass, title, sibsp and parch
rf.train3 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]
set.seed(1234)
rf3 <- randomForest(x=rf.train3, y=rf.label, importance = TRUE, ntree = 1000)
rf3
  

# Random Forest training using pclass, title and family.size
rf.train4 <- data.combined[1:891, c("Pclass", "title", "family.size")]
set.seed(1234)
rf4 <- randomForest(x=rf.train4, y=rf.label, importance = TRUE, ntree = 1000)
rf4

# Subsetting test data
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]

# Prediction
rf4.pred <- predict(rf4, test.submit.df)
table(rf4.pred)

# Creating csv file for submission
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf4.pred)

write.csv(submit.df, file="rf20240802.csv", row.names = FALSE)

install.packages("caret")
install.packages("doSNOW")
library(caret)
library(doSNOW)

set.seed(2345)
cv.10folds <- createMultiFolds(rf.label, k=3, times = 10)

# Checking stratification
table(rf.label)

# Set up caretControl object
ctrl1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                      index = cv.10folds)

# Set up doSNOW package for multi-core training
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

set.seed(3456)
rf4.cv1 <- train(x=rf.train4, y=rf.label, method="rf", tunelength=2,
                 ntree=500, trControl=ctrl1)

# Shutdown cluster
stopCluster(cl)

# Check the results
rf4.cv1

# Modelling using decision tree ----
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(4, type = "SOCK")
  registerDoSNOW(cl) 
  
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart", tunelength=30, 
                    trControl=ctrl)
  # Shutdown cluster
  stopCluster(cl)
  
  return(rpart)
}

# Grab features
features <- c("Pclass", "title", "family.size")
rpart.train1 <- data.combined[1:891, features]

# Run CV
rpart1.cv1 <- rpart.cv(4563, rpart.train1, rf.label, ctrl1)
rpart1.cv1

prp(rpart1.cv1$finalModel, type = 0, under = 1, extra = TRUE)

# Title table
table(data.combined$title)

#Parse out lastname and title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]

last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

#Add last name to the dataframe
data.combined$last.name <- last.names

# Titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

data.combined[which(titles=="the"),]

# Reasign titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkeer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major")] <- "Officer."
table(titles)

data.combined$new.title <- as.factor(titles)

# Visualize new title
ggplot(data.combined[1:891,], aes(x=new.title, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("new.title survival rate")

# Combine titles
ladies <- which(data.combined$new.title == "Lady.")
data.combined$new.title[ladies] <- "Mrs."

men <- which(data.combined$new.title == "Dr." |
            data.combined$new.title == "Officer." |
            data.combined$new.title == "Rev." |
            data.combined$new.title == "Sir.")
data.combined$new.title[men] <- "Mr."

ggplot(data.combined[1:891,], aes(x=new.title, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("new.title survival rate")

# Grab features
features <- c("Pclass", "family.size", "new.title")
rpart.train2 <- data.combined[1:891, features]

#Run cv
rpart2.cv1 <- rpart.cv(4563, rpart.train2, rf.label, ctrl1)
rpart2.cv1

#plot
prp(rpart2.cv1$finalModel, type = 0, under = 1, extra = TRUE)

#First class Mr.
first.class.mr <- which(data.combined$new.title=="Mr." &
                          data.combined$Pclass=="1")
first.class.mr_df <- data.combined[first.class.mr,]
summary(first.class.mr_df)

# female found
first.class.mr_df[first.class.mr_df$Sex=="female",]

# Update new.title feature
indexes <- which(data.combined$new.title=="Mr." &
                   data.combined$Sex=="female")
data.combined$new.title[indexes] <- "Mrs."

#Refresh data frame
first.class.mr <- which(data.combined$new.title=="Mr." &
                          data.combined$Pclass=="1")
first.class.mr_df <- data.combined[first.class.mr,]

#Survival rate for first class "Mr." by fare
ggplot(first.class.mr_df, aes(x="Fare", fill=Survived)+
  geom_density(alpha=0.5)+
  ggtitle("1st class 'Mr.' survival rate by fare"))

# features based on all passengers with the same ticket
ticket.group.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)){
  current.ticket <- ticket[i]
  group.indexes <- which(data.combined$Ticket==current.ticket)
  current.avg.fare <- data.combined[group.indexes[1], "Fare"] / 
    length(group.indexes)
  
  for (k in 1:length(group.indexes)) {
    ticket.group.size[group.indexes[k]] <- length(group.indexes)
    avg.fare[group.indexes[k]] <- current.avg.fare
  }
} 

data.combined$ticket.group.size <- ticket.group.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class "Mr." data frame
first.class.mr_df <- data.combined[first.class.mr]
summary(first.class.mr_df)

#Visualize new features
ggplot(first.class.mr_df[first.class.mr_df$Survived !="None",], 
       aes(x= ticket.group.size, fill=Survived))
  geom_density(alpha=0.5)
  ggtitle("1st class 'Mr.' survival rate by ticket.group.size")
  
  ggplot(first.class.mr_df[first.class.mr_df$Survived !="None",], 
         aes(x= avg.fare, fill=Survived))
  geom_density(alpha=0.5)
  ggtitle("1st class 'Mr.' survival rate by avg.fare")

summary(data.combined$avg.fare)

# one NA
data.combined[is.na(data.combined$avg.fare),]

# Get records for similar passengers
indexes <- with(data.combined, which(Title=="Mr." & Pclass=="3" & 
                                       family.size=="1" & Ticket!="3701"))

simmilar.na.passengers <- data.combined[indexes,]

summary(simmilar.na.passengers$avg.fare)

#Assign na the mean value
data.combined[is.na(avg.fare), "avg.fare"] <- 7.717

# Leverage caret's preProcess function to normalise data
preproc.data.combined <- data.combined[, c("ticket.group.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method= c("center","scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

cor(postproc.data.combined$ticket.group.size, postproc.data.combined$avg.fare)

# feature engineering
features <- c("Pclass", "new.title", "family.size", "ticket.group.size", "avg.fare")

rpart.train3 <- data.combined[1:891, features]

#Run cv
rpart3.cv1 <- rpart.cv(4563, rpart.train3, rf.label, ctrl1)
rpart3.cv1

#plot
prp(rpart3.cv1$finalModel, type = 0, under = 1, extra = TRUE)

#Subsetting test records and features
test.submit.df <- data.combined[892:1309, features]

# Making predictions
rpart3.pred <- predict(rpart3.cv1$finalModel, test.submit.df, type="class")
table(rpart3.pred)

# csv file for submission
submit.df <- data.frame(PassengerId=rep(892:1309), Survived=rpart3.pred)

write.csv(submit.df, file = rpart20240806.csv, row.names = FALSE)


