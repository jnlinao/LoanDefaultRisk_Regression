
#Reading in Data
app_df <- read.csv('data/application_data.csv')
#Remove scientific notation
options(scipen=999)
#Package Import 
library(ggplot2) 
#Check the proportion of the two type of loan
#nrow(app_df[app_df$NAME_CONTRACT_TYPE == 'Cash loans',])
#Revolving loans has 29279 out of 307511 and Cash loans has 278232 out of 307511






pre_data <- read.csv("data/previous_application.csv")
options(gsubfn.engine = "R")
library(sqldf)

status <- sqldf("select app_df.SK_ID_CURR, NAME_CONTRACT_STATUS, count(*) as num
      from pre_data left join app_df on pre_data.SK_ID_CURR = app_df.SK_ID_CURR
      group by 1,2")
status_num <- sqldf("select SK_ID_CURR,
  case when NAME_CONTRACT_STATUS = 'Approved' then num end as Pre_approved_num,
  case when NAME_CONTRACT_STATUS = 'Canceled' then num end as Pre_canceled_num,
  case when NAME_CONTRACT_STATUS = 'Refused' then num end Pre_refused_num,
  case when NAME_CONTRACT_STATUS = 'Unused offer' then num end Pre_unused_offer_num from status
  ")
status_num <- sqldf("select SK_ID_CURR, sum(Pre_approved_num) as Pre_approved_num, sum(Pre_canceled_num) as Pre_canceled_num, sum(Pre_refused_num) as Pre_refused_num, sum(Pre_unused_offer_num) as Pre_unused_offer_num
                    from status_num group by 1")
status_num[is.na(status_num)] <- 0
status_num <- status_num[-c(1),]
total <- sqldf("select * from app_df left join status_num on app_df.SK_ID_CURR = status_num.SK_ID_CURR")




# Manually choose some columns 
# The removal standard1 is: correlation with target based on subjectivity，features like whether leave
#   a phone number is likely not correlated with target variables.
# The removal standard2 is: interpretation in our project, features like whether provide document 1, 
#   document 2 is not interpretable as we don't know what are these documents.
#
data <- total[,c('TARGET','NAME_CONTRACT_TYPE','CODE_GENDER','FLAG_OWN_CAR','FLAG_OWN_REALTY','CNT_CHILDREN','AMT_INCOME_TOTAL','DAYS_REGISTRATION',
 'AMT_CREDIT','AMT_ANNUITY','AMT_GOODS_PRICE','NAME_INCOME_TYPE','NAME_EDUCATION_TYPE','NAME_FAMILY_STATUS','EXT_SOURCE_2',
 'NAME_HOUSING_TYPE','DAYS_BIRTH','DAYS_EMPLOYED','OWN_CAR_AGE','OCCUPATION_TYPE','CNT_FAM_MEMBERS',
 'REGION_RATING_CLIENT','REGION_RATING_CLIENT_W_CITY','ORGANIZATION_TYPE','APARTMENTS_AVG','YEARS_BUILD_AVG',
 'Pre_approved_num','Pre_canceled_num','Pre_refused_num','Pre_unused_offer_num')]



#Percentage of Missing Values in each column
p <- function(x) {sum(is.na(x))/length(x)*100}
per.na <- sort(apply(data, 2, p), decreasing = T)
# >40%
length(per.na[per.na > 40])
per.na[per.na > 40]

#Deleting columns w/ >40% missing values
data1 = data[,!sapply(data, function(x) mean(is.na(x)))>0.4]
dim(data1)
names(data1)
# still have na data in the file, this step only remove column with na data more than 40%






#Before Outlier Removal: AMT_INCOME_TOTAL
target_bar<-barplot(table(data$TARGET))
table(data$TARGET)


#Before Outlier Removal: AMT_ANNUITY
ggplot(data1) +
  aes(x = AMT_ANNUITY) +
  geom_histogram(bins = 50L, fill = "#0c4c8a") +
  theme_minimal()

#Before Outlier Removal: DAYS_EMPLOYED
ggplot(data1) +
  aes(x = DAYS_EMPLOYED) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#Before Outlier Removal: DAYS_REGISTRATION
ggplot(data1) +
  aes(x = DAYS_REGISTRATION) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#Before Outlier Removal: CNT_FAM_MEMBERS
ggplot(data1) +
  aes(x = CNT_FAM_MEMBERS) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()





#Boxplot: AMT_INCOME_TOTAL
ggplot(app_df) +
  aes(x = "", y = AMT_INCOME_TOTAL) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Boxplot: CNT_CHILDREN
ggplot(app_df) +
  aes(x = "", y = CNT_CHILDREN) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Boxplot: AMT_CREDIT
ggplot(app_df) +
  aes(x = "", y = AMT_CREDIT) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Boxplot: AMT_GOODS_PRICE
ggplot(app_df) +
  aes(x = "", y = AMT_GOODS_PRICE) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Boxplot: AMT_ANNUITY
ggplot(app_df) +
  aes(x = "", y = AMT_ANNUITY) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


#Boxplot: DAYS_REGISTRATION
ggplot(app_df) +
  aes(x = "", y = DAYS_REGISTRATION) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()




names(data1)
#  Capping AMT_INCOME_TOTAL
col_amt.inc.tot <- data1$AMT_INCOME_TOTAL
qnt <- quantile(col_amt.inc.tot, probs=c(.25, .75), na.rm = T)
caps <- quantile(col_amt.inc.tot, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col_amt.inc.tot, na.rm = T)
col_amt.inc.tot[col_amt.inc.tot < (qnt[1] - H)] <- caps[1]
col_amt.inc.tot[col_amt.inc.tot > (qnt[2] + H)] <- caps[2]
data1$AMT_INCOME_TOTAL <- col_amt.inc.tot

#  Capping DAYS_REGISTRATION. 
#  Original this feature always negative, here we convert it to positive.
#  We made this conversion just to simplify interpretation while not affect the performance of the model. 
col.day.reg <- abs(data1$DAYS_REGISTRATION)
qnt <- quantile(col.day.reg, probs=c(.25, .75), na.rm = T)
caps <- quantile(col.day.reg, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col.day.reg, na.rm = T)
col.day.reg[col.day.reg < (qnt[1] - H)] <- caps[1]
col.day.reg[col.day.reg > (qnt[2] + H)] <- caps[2]
data1$DAYS_REGISTRATION <- col.day.reg

#  Capping AMT_CREDIT
col.amt.cre <- data1$AMT_CREDIT
qnt <- quantile(col.amt.cre, probs=c(.25, .75), na.rm = T)
caps <- quantile(col.amt.cre, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col.amt.cre, na.rm = T)
col.amt.cre[col.amt.cre < (qnt[1] - H)] <- caps[1]
col.amt.cre[col.amt.cre > (qnt[2] + H)] <- caps[2]
data1$AMT_CREDIT <- col.amt.cre

#  Capping AMT_ANNUITY
col.amt.ann <- data1$AMT_ANNUITY
qnt <- quantile(col.amt.ann, probs=c(.25, .75), na.rm = T)
caps <- quantile(col.amt.ann, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col.amt.ann, na.rm = T)
col.amt.ann[col.amt.ann < (qnt[1] - H)] <- caps[1]
col.amt.ann[col.amt.ann > (qnt[2] + H)] <- caps[2]
data1$AMT_CREDIT <- col.amt.ann

#  Capping AMT_GOODS_PRICE
col.amt.pre <- data1$AMT_GOODS_PRICE
qnt <- quantile(col.amt.pre, probs=c(.25, .75), na.rm = T)
caps <- quantile(col.amt.pre, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col.amt.pre, na.rm = T)
col.amt.pre[col.amt.pre < (qnt[1] - H)] <- caps[1]
col.amt.pre[col.amt.pre > (qnt[2] + H)] <- caps[2]
data1$AMT_CREDIT <- col.amt.pre
max(data1$DAYS_BIRTH)/365
min(data1$DAYS_BIRTH)/365

#  Capping DAYS_BIRTH. Originally this feature is also negative, here we convert it to positive.
col.day.bir <- abs(data1$DAYS_BIRTH)
qnt <- quantile(col.day.bir, probs=c(.25, .75), na.rm = T)
caps <- quantile(col.day.bir, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col.day.bir, na.rm = T)
col.day.bir[col.day.bir < (qnt[1] - H)] <- caps[1]
col.day.bir[col.day.bir > (qnt[2] + H)] <- caps[2]
data1$DAYS_BIRTH <- col.day.bir

#  Capping DAYS_BIRTH. Originally this feature is also negative, here we convert it to positive.
col.day.emp <- abs(data1$DAYS_EMPLOYED)
qnt <- quantile(col.day.emp, probs=c(.25, .75), na.rm = T)
caps <- quantile(col.day.emp, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(col.day.emp, na.rm = T)
col.day.emp[col.day.emp < (qnt[1] - H)] <- caps[1]
col.day.emp[col.day.emp > (qnt[2] + H)] <- caps[2]
data1$DAYS_EMPLOYED <- col.day.emp


names(data1)



# here we remove rows contain na value
data1 <- data1[complete.cases(data1), ]
sum(is.na(data1))
names(data1)


library(mlr)
#install.packages("mlr")
names(data1)
str(data1)

data1 <- data1[data1$NAME_INCOME_TYPE %in% c('Commercial associate','Pensioner','State servant','Working'), ] 
#  Create dummy variables on NAME_CONTRACT_TYPE
data1$NAME_CONTRACT_TYPE <- as.factor(data1$NAME_CONTRACT_TYPE)
data1$NAME_CONTRACT_TYPE <- as.numeric(data1$NAME_CONTRACT_TYPE) -1
data1$NAME_CONTRACT_TYPE
names(data1)
#  Create dummy variables on gender
CODE_GENDER <- createDummyFeatures(data1$CODE_GENDER, cols = "CODE_GENDER")
names(CODE_GENDER)<- c('CODE_GENDER_F','CODE_GENDER_M')
CODE_GENDER_F <-CODE_GENDER[,1]

#  Create dummy variables on FLAG_OWN_CAR
data1$FLAG_OWN_CAR <- as.factor(data1$FLAG_OWN_CAR)
data1$FLAG_OWN_CAR <- as.numeric(data1$FLAG_OWN_CAR) -1

#  Create dummy variables on FLAG_OWN_REALTY
data1$FLAG_OWN_REALTY  <- as.factor(data1$FLAG_OWN_REALTY )
data1$FLAG_OWN_REALTY  <- as.numeric(data1$FLAG_OWN_REALTY ) -1
#
library(plyr)
count(data1, "NAME_INCOME_TYPE")
#  For NAME_INCOME_TYPE feature, there are 8 categories: Businessman(10), Commercial associate(71426)
#    Maternity leave(5), Pensioner(55184), State servant(21629), Student(18), Unemployed(22), 
#    Working(158268)
#  Here we only keep Commercial associate, Pensioner, State servant and Working 4 categories while
#   removing the rest categories to avoid sparsity.
library(dplyr)


count(data1, NAME_INCOME_TYPE)
NAME_INCOME_TYPE <- createDummyFeatures(data1$NAME_INCOME_TYPE, cols = "NAME_INCOME_TYPE")
NAME_INCOME_TYPE <- NAME_INCOME_TYPE[,c(1,3,4)]
str(NAME_INCOME_TYPE)
names(NAME_INCOME_TYPE)<- c('NAME_INCOME_TYPE_Commercial_associate',
                           'NAME_INCOME_TYPE_State_servant', 'NAME_INCOME_TYPE_Working')

# For NAME_EDUCATION_TYPE, there are 5 type and here we change them into ordinal data 
names(data1)
count(data1, NAME_EDUCATION_TYPE)
data1$NAME_EDUCATION_TYPE[data1$NAME_EDUCATION_TYPE == 'Lower secondary'] <- '1'
data1$NAME_EDUCATION_TYPE[data1$NAME_EDUCATION_TYPE == 'Secondary / secondary special'] <- '2'
data1$NAME_EDUCATION_TYPE[data1$NAME_EDUCATION_TYPE == 'Incomplete higher'] <- '3'
data1$NAME_EDUCATION_TYPE[data1$NAME_EDUCATION_TYPE == 'Higher education'] <- '4'
data1$NAME_EDUCATION_TYPE[data1$NAME_EDUCATION_TYPE == 'Academic degree'] <- '5'
data1$NAME_EDUCATION_TYPE <- as.numeric(data1$NAME_EDUCATION_TYPE)

# NAME_FAMILY_STATUS 
count(data1, NAME_FAMILY_STATUS)
NAME_FAMILY_STATUS <- createDummyFeatures(data1$NAME_FAMILY_STATUS, cols = "NAME_FAMILY_STATUS")
names(NAME_FAMILY_STATUS)<- c('NAME_FAMILY_STATUS_Civil_marriage','NAME_FAMILY_STATUS_Married',           'NAME_FAMILY_STATUS_Separated','NAME_FAMILY_STATUS_Single_not_married','NAME_FAMILY_STATUS_Widow')
NAME_FAMILY_STATUS <- NAME_FAMILY_STATUS[,c(1,3,5)]
str(NAME_FAMILY_STATUS)


# NAME_HOUSING_TYPE
names(data1)
count(data1,NAME_HOUSING_TYPE)


NAME_HOUSING_TYPE <- createDummyFeatures(data1$NAME_HOUSING_TYPE, cols = "NAME_HOUSING_TYPE")
str(NAME_HOUSING_TYPE)
NAME_HOUSING_TYPE <- NAME_HOUSING_TYPE[,1:5]
names(NAME_HOUSING_TYPE)<- c('NAME_HOUSING_TYPE_Coop_apartment','NAME_HOUSING_TYPE_House_apartment',
                              'NAME_HOUSING_TYPE_Municipal_apartment','NAME_HOUSING_TYPE_Office_apartment','NAME_HOUSING_TYPE_Rented_apartment')
names(data1)
# too many types, just drop(or classfier them based one some industry)
data1 <- subset(data1, select = -c(OCCUPATION_TYPE))
data1 <- subset(data1, select = -c(ORGANIZATION_TYPE))
names(data1)
data1 <- subset(data1, select = -c(CODE_GENDER,NAME_INCOME_TYPE
                                     ,NAME_FAMILY_STATUS,NAME_HOUSING_TYPE
                                     ))
names(data1)
data1 <- cbind(data1,CODE_GENDER_F,NAME_INCOME_TYPE,
                NAME_FAMILY_STATUS,NAME_HOUSING_TYPE
                ) 
names(data1)
str(data1)






names(data1)
count(data1,"TARGET")
library(rpart)
#install.packages("ROSE")
library(ROSE)
str(data1)
data.fail <- subset(data1, data1$TARGET == 1)
data.succ <- subset(data1, data1$TARGET == 0)
set.seed(10)
train.succ <- sample(1:nrow(data.succ),nrow(data.succ)*0.5)
train.fail <- sample(1:nrow(data.fail),nrow(data.fail)*0.5)
dat.train <- rbind(data.succ[train.succ,],data.fail[train.fail,])
dat.test <- rbind(data.succ[-train.succ,],data.fail[-train.fail,])
dim(dat.train)
dim(dat.test)
?barplot
target_bar<-barplot(table(dat.train$TARGET), main="training class distribution")
table(dat.test$TARGET)
dat.train <- ovun.sample(TARGET ~ ., data = dat.train, method = "under", p = 0.5)$data
dim(dat.train)
table(dat.train$TARGET)
target_bar<-barplot(table(dat.train$TARGET), main="training class distribution after undersampling")
#Now the ratio is 1:1


glm.model <- glm(TARGET~., data = dat.train, family = "binomial")
# forward wrapper feature selection
#null.model <- glm(TARGET~1)
#forward.result <- step(object = null.model,direction = "forward",scope=formula(glm.model))
#summary(forward.result)
#  Check the performance of glm model
summary(glm.model)
exp(coef(glm.model))
yhat.train <- predict(glm.model, dat.train, 
                      type = "response")
yhat.train.plus.act <- cbind(yhat.train, 
                             dat.train$TARGET)
yhat.train.class <- ifelse(yhat.train > 0.5, 1, 0)
tab.lr1.train <- table(dat.train$TARGET, 
                       yhat.train.class, 
                       dnn = c("Actual","Predicted"))
tab.lr1.train
1-mean(dat.train$TARGET != yhat.train.class)
class0_acc <- tab.lr1.train[1,1]/(tab.lr1.train[1,1]+tab.lr1.train[2,1])
class0_acc
class1_acc <- tab.lr1.train[2,2]/(tab.lr1.train[1,2]+tab.lr1.train[2,2])
class1_acc

# On test dataset

yhat.test <- predict(glm.model, dat.test, 
                      type = "response")
yhat.test.plus.act <- cbind(yhat.test, 
                             dat.test$TARGET)
yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)
tab.lr1.test <- table(dat.test$TARGET, 
                       yhat.test.class, 
                       dnn = c("Actual","Predicted"))
tab.lr1.test
overall_accuracy <- 1-mean(dat.test$TARGET != yhat.test.class)
overall_accuracy
class0_acc <- tab.lr1.test[1,1]/(tab.lr1.test[1,1]+tab.lr1.test[2,1])
class0_acc
class1_acc <- tab.lr1.test[2,2]/(tab.lr1.test[1,2]+tab.lr1.test[2,2])
class1_acc

#install.packages("tree")
library(tree)
library(rpart)
str(dat.train)
dat.train1 <- dat.train
dat.train1[,1] <- as.factor(dat.train1[,1])
dat.test1 <- dat.test
dat.test1[,1] <- as.factor(dat.test1[,1])
tree.model <- rpart(TARGET~., data = dat.train1, control = rpart.control(cp = 0.001))
tree.model
tree.pred.tr <- predict(tree.model, dat.train1, type = "class")
tree.train.confusion <- table(dat.train1$TARGET, tree.pred.tr,
      dnn = c("Actual", "Predicted"))
tree.train.confusion

tree.train.confusion[1,1]/(tree.train.confusion[1,1]+tree.train.confusion[2,1])
tree.train.confusion[2,2]/(tree.train.confusion[1,2]+tree.train.confusion[2,2])

1-mean(dat.train1$TARGET != tree.pred.tr)

tree.pred.tst <- predict(tree.model, dat.test1, type = "class")
tree.test.confusion <- table(dat.test1$TARGET, tree.pred.tst,
      dnn = c("Actual", "Predicted"))
tree.test.confusion
tree.test.confusion[1,1]/(tree.test.confusion[1,1]+tree.test.confusion[2,1])
tree.test.confusion[2,2]/(tree.test.confusion[1,2]+tree.test.confusion[2,2])
1-mean(dat.test$TARGET != tree.pred.tst)




library(class)
names(dat.train)
dat.train.x <- dat.train[,2:29]
dat.train.y <- dat.train[,1]
dat.test.x <- dat.test[,2:29]
dat.test.y <- dat.test[,1]

out1 <- knn(dat.train.x, dat.test.x, dat.train.y, k=4)

tab.knn1 <- table(dat.test.y, out1,
                  dnn = c("Actual", "Predicted"))
tab.knn1
knn1.err <- 1-mean(dat.test.y != out1)
knn1.err
class0_acc_knn <- tab.knn1[1,1]/(tab.knn1[1,1]+tab.knn1[2,1])
class0_acc_knn
class1_acc_knn <- tab.knn1[2,2]/(tab.knn1[1,2]+tab.knn1[2,2])
class1_acc_knn



#install.packages('randomForest')
library(randomForest)
set.seed(223344)
bag.train.10 <- randomForest(TARGET ~ ., 
                             data = dat.train, 
                             mtry = 20, ntree = 1
                             )
bag.train.10

yhat.train <- predict(bag.train.10, dat.train)
yhat.train.class <- ifelse(yhat.train > 0.5, 1, 0)
tab.train.rf <- table(dat.train$TARGET, yhat.train.class)
tab.train.rf
class0_acc_rf.train <- tab.train.rf[1,1]/(tab.train.rf[1,1]+tab.train.rf[2,1])
class0_acc_rf.train
class1_acc_rf.train <- tab.train.rf[2,2]/(tab.train.rf[1,2]+tab.train.rf[2,2])
class1_acc_rf.train
err.bag10 <- mean(dat.train$TARGET != yhat.train.class)
1 - err.bag10

yhat.bag.10 <- predict(bag.train.10, dat.test)
yhat.test.class <- ifelse(yhat.bag.10 > 0.5, 1, 0)
tab.bag.10 <- table(dat.test$TARGET, yhat.test.class, dnn=c("Actual", "Predicted"))
tab.bag.10
class0_acc_rf <- tab.bag.10[1,1]/(tab.bag.10[1,1]+tab.bag.10[2,1])
class0_acc_rf
class1_acc_rf <- tab.bag.10[2,2]/(tab.bag.10[1,2]+tab.bag.10[2,2])
class1_acc_rf
err.bag10 <- mean(dat.test$TARGET != yhat.test.class)
1 - err.bag10



library(e1071)
nb.fit <- naiveBayes(TARGET ~ ., data = dat.train)
nb.fit

nb.class.train <- predict(nb.fit, newdata = dat.train)
tab.nb.train <- table(dat.train$TARGET, nb.class.train,
                 dnn = c("Actual", "Predicted"))
tab.nb.train
tab.nb.train[1,1]/(tab.nb.train[1,1]+tab.nb.train[2,1])
tab.nb.train[2,2]/(tab.nb.train[1,2]+tab.nb.train[2,2])
err.nb.train <- mean(dat.train$TARGET != nb.class.train)
1-err.nb

nb.class <- predict(nb.fit, newdata = dat.test)
nb.class
tab.nb <- table(dat.test$TARGET, nb.class,
                 dnn = c("Actual", "Predicted"))
tab.nb
tab.nb[1,1]/(tab.nb[1,1]+tab.nb[2,1])
tab.nb[2,2]/(tab.nb[1,2]+tab.nb[2,2])
err.nb <- mean(dat.test$TARGET != nb.class)
1-err.nb




