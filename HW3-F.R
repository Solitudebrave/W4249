Adult<-read.csv('/Users/fanheng/Dropbox/2014 Spring/HW3data.csv',head=F)

# Identify missing value
Adult.factor <- as.matrix(Adult[,lapply(Adult,class)=="factor"]) 
Adult.factor.nospace <- gsub(pattern=" ",x=Adult.factor,replacement="")
Adult[,lapply(Adult,class)=="factor"] <- data.frame(Adult.factor.nospace)
Adult[Adult=="?"] <- NA

#Dealing with missing value
Adult[,lapply(Adult,class)=="factor"] <- lapply(Adult[,lapply(Adult,class)=="factor"],factor)
names(Adult) <- c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","label")
levels(Adult$native_country) <- c("Asia","North America","Asia","South America","North Ameirca","South America","North America","South America","North America","Europe","Europe","Europe","Europe","North America","North America","Europe", "North America", "Unknown", "Europe", "Asia", "Asia", "Europe","Europe", "North America","Asia","Asia","North America", "North America", "North America","South America", "Aisa", "Europe","Europe","North America","Europe","Unknown","Asia","Asia","North America","North America","Asia","Europe")
Adult <- subset(Adult,select=-c(education_num,fnlwgt))
 #imputation
require(missForest)
Adult[,-13] <- missForest(Adult[,-13],maxiter=2)$ximp

{ #directly deleting
#Adult <- na.omit(Adult)}

#split into training set and test set
mid <- round(nrow(Adult)/2)
train.Adult <- Adult[1:mid,]
test.Adult <- Adult[(mid+1):nrow(Adult),]

#==================================================================================
# LDA
names(train.Adult)
install.packages("MASS")
require(MASS)
LDA.Adult<-lda(label~., data=train.Adult)
lda.pred<-predict(LDA.Adult, newdata=test.Adult)
class(lda.pred$class)
sum(lda.pred$class!=test.Adult$label)
nrow(test.Adult)
err.lda<-sum(lda.pred$class!=test.Adult$label)/nrow(test.Adult)

#==================================================================================================
#QDA
#Build a sparse matrix to transform categorical varibles to dummy variables.
require(useful)
Y <- rep(1,nrow(train.Adult))
Train.x <- train.Adult[,1:12]
Train.Y <- train.Adult[,13]
Dt <- data.frame(Y,Train.x)
Train.X <- build.x(Y~.,Dt,contrasts=T)
Y <- rep(1,nrow(test.Adult))
Test.x <- test.Adult[,1:12]
Test.Y <- test.Adult[,13]
Dt2 <- data.frame(Y,Test.x)
Test.X <- build.x(Y~.,Dt2,contrasts=T)
train.new<-data.frame(train.Adult$label, Train.X[,-1])
test.new<-data.frame(test.Adult$label, Test.X)[,-1]


# Use the sparse matrix and label to test the correlation of response and coviriates.
Adult.matx<-data.matrix(rbind(Train.X,Test.X))
Adult.matx<-Adult.matx[,-1]
Y.test<-Adult$label=='<=50K'
Y.test<-Y.test+0
cor(Adult.matx,Y.test)
a <- cor(Adult.matx,Y.test)
rownames(a)[abs(a)>0.2]

#by correlation, we use the following variables: "age"                              NA                                 "marital_statusMarried-civ-spouse"
#[4] "marital_statusNever-married"      "occupationExec-managerial"        "relationshipOwn-child"           
#[7] "sexMale"                          "capital_gain"                     "hours_per_week"  

Adult.df<-data.frame(Adult.matx, Adult$label)
train.Adult.qda<-Adult.df[1:(30162/2),]
test.Adult.qda<-Adult.df[(30162/2+1):30162,]

train.qda<-subset(train.Adult.qda,select=c(age, workclassPrivate,marital_statusMarried.civ.spouse,marital_statusNever.married,occupationExec.managerial,relationshipOwn.child,sexMale,capital_gain,hours_per_week,Adult.label))
test.qda<-subset(test.Adult.qda,select=c(age, workclassPrivate,marital_statusMarried.civ.spouse,marital_statusNever.married,occupationExec.managerial,relationshipOwn.child,sexMale,capital_gain,hours_per_week,Adult.label))

require("MASS")
QDA.Adult<-qda(Adult.label~., data=train.qda)
qda.pred<-predict(QDA.Adult, newdata=test.qda)
class(qda.pred$class)
err.qda<-sum(qda.pred$class!=test.qda$Adult.label)/nrow(test.qda)
list(err.lda=err.lda, err.qda=err.qda)

qda.buildx<-qda(Train.X, Train.Y)
qda.pred<-predict(qda.buildx, newdata=Test.x)

require(DMwR)
train.new<-SMOTE(label~.,train.Adult, perc.over=200, perc.under=100)


