#Biological data and we need to find if he/she is N:normal;S:suspected;P:pathologic
#Read Data
data <- read.csv("Cardiotocographic.csv")
str(data)
data$NSPF <- as.factor(data$NSP)

#Partition dataset for training,validation and test
set.seed(1234)
ind <- sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
train <- data[ind==1,]
validate <- data[ind==2,]

#Decision Tree with Party Package
library(party)
#for illustration we are going to use only first 3 coloumns
tree <- ctree(NSPF~LB+AC+FM,data=train,controls = ctree_control(mincriterion=0.99,minsplit = 500))
#mincriterion=confidence level;minsplit=split after how many nodes  
tree
#19 nodes
#to see the tree
plot(tree)

#Predict
predict(tree,validate,type="response")
predict(tree,validate,type="prob")

#decision tree with r part package
library(rpart)
library(rpart.plot)
tree_1 <- rpart(NSPF~AC+LB+FM,data=train)
tree_1
rpart.plot(tree_1,extra=2)
#use extra to get diffrent info in the same tree

#Predict
predict(tree_1,validate,type="class")

#Missclassification error
tab <- table(predict(tree,train),train$NSPF)
tab
1-sum(diag(tab))/sum(tab)
#if we would have used all the varriables it would give better results
#error on validate data
tab1 <- table(predict(tree,validate),validate$NSPF)
tab1
1-sum(diag(tab1))/sum(tab1)
