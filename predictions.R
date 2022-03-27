library(randomForest)

library(tidyverse)

Q1<-read.csv("christchurch-data.csv")
str(Q1) #have a look at the columns of the data

#chchsub<- subset(Q1, subset = Area.Unit  %in% c("Cathedral Square.","Sydenham.","Riccarton.","Linwood East.","Shirley East.","Hagley Park.","Northcote.","Linwood.","Hornby North.","Avon Loop."))
#subset the dataset to meet the Area.unit column equal to the top 10 areas
#chchsub
#write.csv(chchsub,"chch_top10.csv", row.names = FALSE)
chchsubClean <- Q1 %>% filter(Q1$Occurrence.Day.Of.Week == 'Monday'|Q1$Occurrence.Day.Of.Week == 'Tuesday'      #one using mon-sun, removing the 'unknown' days
                                   |Q1$Occurrence.Day.Of.Week == 'Wednesday'|Q1$Occurrence.Day.Of.Week == 'Thursday'
                                   |Q1$Occurrence.Day.Of.Week == 'Friday'|Q1$Occurrence.Day.Of.Week == 'Saturday'|Q1$Occurrence.Day.Of.Week == 'Sunday')

chchfinalclean <- chchsubClean %>% filter(chchsubClean$Occurrence.Hour.Of.Day == '1'|chchsubClean$Occurrence.Hour.Of.Day == '2'|chchsubClean$Occurrence.Hour.Of.Day == '3'|chchsubClean$Occurrence.Hour.Of.Day == '4'
                                          |chchsubClean$Occurrence.Hour.Of.Day == '5'|chchsubClean$Occurrence.Hour.Of.Day == '6'|chchsubClean$Occurrence.Hour.Of.Day == '7'|chchsubClean$Occurrence.Hour.Of.Day == '8'
                                          |chchsubClean$Occurrence.Hour.Of.Day == '9'|chchsubClean$Occurrence.Hour.Of.Day == '10'|chchsubClean$Occurrence.Hour.Of.Day == '11'|chchsubClean$Occurrence.Hour.Of.Day == '12'|chchsubClean$Occurrence.Hour.Of.Day == '13'
                                          |chchsubClean$Occurrence.Hour.Of.Day == '14'|chchsubClean$Occurrence.Hour.Of.Day == '15'|chchsubClean$Occurrence.Hour.Of.Day == '16'|chchsubClean$Occurrence.Hour.Of.Day == '17'|chchsubClean$Occurrence.Hour.Of.Day == '18'
                                          |chchsubClean$Occurrence.Hour.Of.Day == '19'|chchsubClean$Occurrence.Hour.Of.Day == '20'|chchsubClean$Occurrence.Hour.Of.Day == '21'|chchsubClean$Occurrence.Hour.Of.Day == '22'|chchsubClean$Occurrence.Hour.Of.Day == '23')
# removing the 99's from occurence hour of the day

chchsubset <- chchfinalclean%>% 
  select('ANZSOC.Division', 'Area.Unit', 'Occurrence.Day.Of.Week', 'Occurrence.Hour.Of.Day')

write.csv(chchsubset,"chch_top10_crime.csv", row.names = FALSE)

library(fastDummies) # library to create the dummy variables

y <- as.numeric(chchsubset$Occurrence.Hour.Of.Day)

a <- chchsubset %>% select(ANZSOC.Division)
b <- chchsubset %>% select(Area.Unit)
c <- chchsubset %>% select(Occurrence.Day.Of.Week)
a<- dummy_cols(a,
               remove_first_dummy = TRUE)# creating dummy variables - removing first dummy to prevent 'dummy variable trap'
b<- dummy_cols(b,
               remove_first_dummy = TRUE)
c<- dummy_cols(c,
               remove_first_dummy = TRUE)

a <- a %>% select(-ANZSOC.Division)  # removing first column - original values, leaving us with only the dummy variables
b <- b%>% select(-Area.Unit)
c <- c%>% select(-Occurrence.Day.Of.Week)


final <- cbind(a,b,c) # col bind to join the 3 dummy variables


#################################################################################
ranfor <- cbind(a,b,c,y) # joining all 4 together
names(ranfor) <- make.names(names(ranfor)) # renames the columns to be able to be used by the functions in r

set.seed(1234)
train = sample (1:nrow(ranfor), round(0.7*nrow(ranfor))) # randomly selected 70% for training
test = ranfor[-train,]  # 30% for testing

library(tree) 
tree.chch = tree(y~., data = ranfor, subset = train) # initial run of decision tree function
plot(tree.chch)# plotting the tree
text(tree.chch, pretty=0) #adding text to the tree


cv.chch = cv.tree(tree.chch)  # k-fold cross-validation experiment
plot(cv.chch$size, cv.chch$dev, type="b") # comparing the deviance to the size (lowest = best) for pruning the tree
cv.chch

prune.chch = prune.tree(tree.chch, best = 3)  # now we prune the tree
plot(prune.chch)    # plotting the pruned tree
text(prune.chch, pretty = 0)

pd.prune.chch = predict(prune.chch, newdata = test)
mse.test.prune = mean((pd.prune.chch-test[,'y'])^2) # finding the MSE for analysing its prediction ability
mse.test.prune

#################################################################################


#train = sample (1:nrow(chchsubset), round(0.7*nrow(chchsubset)))
#test = chchsubset[-train]

ranfor <- cbind(a,b,c,y)
names(ranfor) <- make.names(names(ranfor)) # renames the columns to be able to be used by the functions in r

set.seed(1234)
train = sample (1:nrow(ranfor), round(0.7*nrow(ranfor))) 
test = ranfor[-train,]
#################################################################################

#write.csv(test,"test.csv", row.names = FALSE) this was just for visualising the data

#bagging
bag.chch <- randomForest(y~.,
                         data = ranfor,
                         mtry = ncol(ranfor)-1,
                         subset = train)
bag.chch

#predict
yhat.bag <- predict(bag.chch, newdata = test)

#MSE
mse.test.bag = mean((yhat.bag-test[,'y'])^2)  #MSE calculation
mse.test.bag
###################################
plot(bag.chch) #plotting the first model to see which 'ntree' had the lowest MSE
which.min(bag.chch$mse) #finds the lowest MSE and at what tree value #203
set.seed(1234)
bag.chch.ntree <- randomForest(y~.,
                               data = ranfor,
                               mtry = ncol(ranfor)-1, 
                               subset = train,
                               ntree = 203)  #bagging with 203 trees
bag.chch.ntree

yhat.bag.ntree <- predict(bag.chch.ntree, newdata = test)
#MSE
mse.test.ntree = mean((yhat.bag.ntree-test[,'y'])^2) #MSE calculation
mse.test.ntree

varImpPlot(bag.chch.ntree) #  # visualing which categorical variable was the most important
##################################

#random forest done with a smaller mtry value which is reducing the amount of columns chosen for predicting
bag.chch.bag.rf <- randomForest(y~.,
                                data = ranfor,
                                mtry = 4, # decreasing mtry to decrease mean squared error
                                subset = train,
                                ntree = 352)
bag.chch.bag.rf

yhat.bag.bag.rf <- predict(bag.chch.bag.rf, newdata = test)
#MSE
mse.test.bag.rf = mean((yhat.bag.bag.rf-test[,'y'])^2) #MSE calculation
mse.test.bag.rf

varImpPlot(bag.chch.bag.rf) # visualing which categorical variable was the most important

#########################################################
