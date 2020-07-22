View(employeeearnings)
head(employeeearnings)
install.packages('dplyr')
library(dplyr)
names(employeeearnings) <- tolower(names(employeeearnings))
head(employeeearnings)
install.packages('stringr')
library(stringr)
names(employeeearnings)<-str_replace_all(names(employeeearnings), c(" " = "." , "," = "" ))
names(employeeearnings)<-str_replace_all(names(employeeearnings), c("/" = "." , "," = "" ))

dim(employeeearnings)

list_na <- colnames(employeeearnings)[ apply(employeeearnings, 2, anyNA) ] #List of NA values

# Create mean
average_missing <- apply(employeeearnings[,colnames(employeeearnings) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing


# Create a new variable with the mean - mutate()
employeeearnings_replace <- employeeearnings %>%
  mutate(replace_mean_regular  = ifelse(is.na(regular), average_missing[1], regular),
         replace_mean_retro = ifelse(is.na(retro), average_missing[2], retro),
         replace_mean_other  = ifelse(is.na(other), average_missing[3], other),
         replace_mean_overtime = ifelse(is.na(overtime), average_missing[4], overtime),
         replace_mean_injured  = ifelse(is.na(injured), average_missing[5], injured),
         replace_mean_detail = ifelse(is.na(detail), average_missing[6], detail),
         replace_mean_quinn.education.incentive  = ifelse(is.na(quinn.education.incentive), average_missing[7], quinn.education.incentive))

sum(is.na(employeeearnings_replace$regular))
sum(is.na(employeeearnings_replace$replace_mean_regular))

View(employeeearnings_replace)
str(employeeearnings_replace)
employeeearnings_replace$replace_total.earnings <- employeeearnings_replace$replace_mean_regular + employeeearnings_replace$replace_mean_retro+ employeeearnings_replace$replace_mean_other+ employeeearnings_replace$replace_mean_overtime+ employeeearnings_replace$replace_mean_injured+ employeeearnings_replace$replace_mean_detail+ employeeearnings_replace$replace_mean_quinn.education.incentive
df = subset(employeeearnings_replace, select = -c(regular,retro,other,overtime,injured,detail,quinn.education.incentive,total.earnings) )
View(df)
#Summary fits
lm.fit.reg<- lm(replace_total.earnings~replace_mean_regular,data = df)
summary(lm.fit.reg)

confint(lm.fit.reg)
plot(replace_total.earnings~replace_mean_regular,data = employeeearnings_drop, main = 'earnings vs regular')
abline(lm.fit.reg)

lm.fit.ret<- lm(replace_total.earnings~replace_mean_retro,data = df)
summary(lm.fit.ret)

confint(lm.fit.ret)
plot(replace_total.earnings~replace_mean_retro,data = df, main = 'earnings vs retro')
abline(lm.fit.ret)

lm.fit.oth<- lm(replace_total.earnings~replace_mean_other,data = df)
summary(lm.fit.oth)

confint(lm.fit.oth)
plot(replace_total.earnings~replace_mean_other,data = df, main = 'earnings vs other')
abline(lm.fit.oth)

lm.fit.ovt<- lm(replace_total.earnings~replace_mean_overtime,data = df)
summary(lm.fit.ovt)

confint(lm.fit.ovt)
plot(replace_total.earnings~replace_mean_overtime,data = df, main = 'earnings vs overtime')
abline(lm.fit.ovt)

lm.fit.inj<- lm(replace_total.earnings~replace_mean_injured,data = df)
summary(lm.fit.inj)

confint(lm.fit.inj)
plot(replace_total.earnings~replace_mean_injured,data = df, main = 'earnings vs injured')
abline(lm.fit.inj)

lm.fit.det<- lm(replace_total.earnings~replace_mean_detail,data = df)
summary(lm.fit.det)

confint(lm.fit.det)
plot(replace_total.earnings~replace_mean_detail,data = df, main = 'earnings vs detail')
abline(lm.fit.det)

lm.fit.qu<- lm(replace_total.earnings~replace_mean_quinn.education.incentive,data = df)
summary(lm.fit.qu)

confint(lm.fit.qu)
plot(replace_total.earnings~replace_mean_quinn.education.incentive,data = df, main = 'earnings vs quinn')
abline(lm.fit.qu)

#splitting dataset into train 75% test 25%
set.seed(123)
train=sample(nrow(df),nrow(df)*3/4)

employ.train<-df[train,]

employ.test<-df[-train,]

#final model using train data and test data for final prediction
lm.fit.final<- lm(replace_total.earnings~replace_mean_regular+replace_mean_overtime,data = df)

#compare RSE and R^2 from the final model using training data
summary(lm.fit.final)
confint(lm.fit.finall)

#predicted costs on train and test set
employ.train$predicted_total.earnings<-predict(lm.fit.final,employ.train)
#plot(employ.train$predicted_total.earnings)

#MSE - Mean squared error
mean(sum((employ.train$predicted_total.earnings-employ.train$replace_total.earnings)^2))

#MAPE - Mean absolute percentage error
mean(abs(employ.train$replace_total.earnings-employ.train$predicted_total.earnings)/employ.train$replace_total.earnings)*100

#RSE - residual squared error
sqrt(sum((employ.train$predicted_total.earnings-employ.train$replace_total.earnings)^2)/
       (nrow(employ.train)-3))
#R^2
rss <- sum((employ.train$predicted_total.earnings - employ.train$replace_total.earnings) ^ 2)  ## residual sum of squares
tss <- sum((employ.train$replace_total.earnings - mean(employ.train$replace_total.earnings)) ^ 2)  ## total sum of squares
print(rsq <- 1 - rss/tss)

#Adjusted R^2
rsq.adj<-rsq-(1-rsq)*(2/(150-2-1)) #p/(n-p-1)
rsq.adj

#FINAL performance assessment, RSE and R^2 on test set
employ.test$predicted_total.earnings<-predict(lm.fit.final,employ.test)
#plot(employ.test$predicted_total.earnings)


#MSE
mean(sum((employ.test$predicted_total.earnings-employ.test$replace_total.earnings)^2))

#mean(sum((Cred.train$predicted_balance-Cred.train$Balance)^2))

#MAPE - Mean absolute percentage error
mean(abs(employ.test$replace_total.earnings-employ.test$predicted_total.earnings)/employ.test$replace_total.earnings)*100

#RSE
sqrt(sum((employ.test$predicted_total.earnings-employ.test$replace_total.earnings)^2)/
       (nrow(employ.test)-3))

#R^2

summary(lm.fit.final)
plot(lm.fit.finall)

rss <- sum((employ.train$predicted_total.earnings - employ.train$total.earnings) ^ 2)  ## residual sum of squares
tss <- sum((employ.train$total.earnings - mean(employ.train$total.earnings)) ^ 2)  ## total sum of squares
print(rsq <- 1 - rss/tss)

rss <- sum((employ.test$predicted_total.earnings - employ.test$replace_total.earnings) ^ 2)  ## residual sum of squares
tss <- sum((employ.test$replace_total.earnings - mean(employ.test$replace_total.earnings)) ^ 2)  ## total sum of squares
print(rsq <- 1 - rss/tss)

#Adjusted R^2
rsq.adj<-rsq-(1-rsq)*(2/(50-2-1)) #p/(n-p-1)
rsq.adj

par(mfrow=c(2,2))
plot(lm.fit.final)
