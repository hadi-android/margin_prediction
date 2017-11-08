setwd("~/Predictive Analytics/Top-line revenue/margin/regression/2014-2017")
library(xlsx)
library(caret)
regdata = read.xlsx("margin-regdata.xlsx", sheetIndex = 1)
regdata = regdata[complete.cases(regdata$Year),]
regdata = regdata[c(13:57),]

predictors = c("Associates",                "Principals",                "Counsels",                  "Partners",                  "Law.Clerks.Paralegals",
               "Legal.support.services",    "Marketing...Bus..Developm", "Boardroom.Services",        "Business.Centre",           "Document.Specialist.Centr",
               "Human.Resources",           "Professional.Growth...Mgm", "Reception",                 "Records",                   "Risk.Management")

margindata = regdata
margindata = margindata[,predictors]
margindata$WorkAmtTotal = regdata$WorkAmtTotal

lmfit = lm(WorkAmtTotal~., data=margindata)
summary(lmfit)

actual = margindata$WorkAmtTotal
pred = lmfit$fitted.values
MAE = 100*mean(abs(actual-pred)/actual)
MAE

lmfit_summary = summary(lmfit)
write.xlsx(lmfit_summary$coefficients,file="coeffs_reg.xlsx")
# summary(lmfit2)
# lmpredictors = predictors(lmfit2)
# print(lmpredictors)
# 
# a = margindata[,lmpredictors]
# b = data.frame(margindata$MarginPercent)
# colnames(b) = "MarginPercent"

# margindata2 = cbind(a,b)

# actual = margindata2$MarginPercent
# pred = lmfit2$fitted.values
# MAE = mean(abs(actual-pred))
# MAE

# margindata2$pred = pred

margindata$pred = pred
write.xlsx(margindata, file="revenue_percent_regression.xlsx")

library(DAAG)
cvfit = cv.lm(data=margindata, lmfit, m=3) # 3 fold cross-validation
# cvfit
MAE = 100*mean(abs(cvfit$cvpred-cvfit$WorkAmtTotal)/cvfit$WorkAmtTotal)
MAE

cvfit = cv.lm(data=margindata, lmfit2, m=3) # 3 fold cross-validation
cvfit
MAE = mean(abs(cvfit$cvpred-cvfit$MarginPercent))
MAE

MAE2 = mean(abs(cvfit$Predicted-cvfit$MarginPercent))
MAE2

#####################################################
### cross validation
predictors = predictors[-1]
margindata2 = margindata[,predictors]
margindata2$MarginPercent = margindata$MarginPercent
set.seed(1234567)
k=3
folds = createFolds(margindata2$MarginPercent, k, returnTrain = T)
# folds[[1]]
coefs = list()
MAE = numeric(k)
for (i in 1:k){
  inTrain = folds[[i]]
  data_tr = margindata2[inTrain,]
  data_te = margindata2[-inTrain, -dim(margindata2)[2]]
  # data_te = margindata2[-inTrain,]
  
  lmfit_cv = lm(MarginPercent ~., data = data_tr)
  coefs[[i]]=coef(lmfit_cv)
  actual = data_tr$MarginPercent
  pred = predict(lmfit_cv, newdata = data_te)
  MAE[i] = mean(abs(actual-pred))
}
print(MAE)
print(mean(MAE))
data_tr = margindata[inTrain,]
data_te = margindata[-inTrain,]

lmfit = train(MarginPercent~., data = data_tr, method="lm")
summary(lmfit)
coef(lmfit)
# 
# actual = data_te$MarginPercent
# data_te = data_te[,-9]
# pred = predict(lmfit, newdata = data_te)
# MAE = mean(abs(actual-pred))
# print(MAE)
