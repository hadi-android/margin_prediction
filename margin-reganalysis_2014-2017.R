setwd("~/Predictive Analytics/Top-line revenue/margin/regression/margin_prediction")
library(xlsx)
library(caret)
regdata = read.xlsx("margin_regdata_major_departments.xlsx", sheetIndex = 1)
# NAcount = sapply(regdata, function(x) sum(is.na(x)))
# NAcount
regdata = regdata[complete.cases(regdata$Year),]
regdata = regdata[c(13:57),]

margindata = regdata
margindata$MarginPercent = 100*regdata$Margin_Sum/regdata$WorkAmtTotal
margindata = margindata[,-which(names(margindata)=="Margin_Sum")]
margindata = margindata[,-which(names(margindata)=="WorkAmtTotal")]
margindata = margindata[,-which(names(margindata)=="Year")]
margindata = margindata[,-which(names(margindata)=="Month")]
# margindata = margindata[,-which(names(margindata)=="Professional.Growth...Mgm")]

# names(margindata)

lmfit = lm(MarginPercent~., data=margindata)
summary(lmfit)
lmfit_summary = summary(lmfit)
coefs = data.frame(lmfit_summary$coefficients)
coefs$name = row.names(coefs)
coefs = coefs[order(coefs$Pr...t..),]
actual = margindata$MarginPercent
pred = lmfit$fitted.values
MAE = mean(abs(actual-pred))
MAE

# margindata$pred = pred
# lmfit_summary = summary(lmfit)
# write.xlsx(lmfit_summary$coefficients,file="coeffs_margin_allpredictors.xlsx")

lmfit2 = step(lmfit, direction = 'both')
summary(lmfit2)

actual = margindata$MarginPercent
pred = lmfit2$fitted.values
MAE = mean(abs(actual-pred))
MAE

predictors = names(lmfit2$coefficients)
lmfit_summary = summary(lmfit2)
coefs = data.frame(lmfit_summary$coefficients)
coefs$name = row.names(coefs)
coefs = coefs[order(coefs$Pr...t..),]
print(coefs)
# write.xlsx(lmfit_summary$coefficients,file="coeffs_margin_noPGM.xlsx")
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

predictors = predictors[-1]
margindata2 = margindata[,predictors]
margindata2$MarginPercent = margindata$MarginPercent

library(DAAG)
cvfit = cv.lm(data=margindata, lmfit, m=3) # 3 fold cross-validation
# cvfit
MAE = mean(abs(cvfit$cvpred-cvfit$MarginPercent))
MAE

cvfit = cv.lm(data=margindata, lmfit2, m=3) # 3 fold cross-validation
cvfit
MAE = mean(abs(cvfit$cvpred-cvfit$MarginPercent))
MAE

MAE2 = mean(abs(cvfit$Predicted-cvfit$MarginPercent))
MAE2

#####################################################
### cross validation
browser()
lmfit2 = lm(MarginPercent~., data=margindata2)
summary(lmfit2)
margindata2 = margindata2[,-which(names(margindata2)=="Professional.Growth...Mgm")]
lmfit3 = lm(MarginPercent~., data=margindata2)
summary(lmfit3)
lmfit_summary = summary(lmfit3)
coefs = data.frame(lmfit_summary$coefficients)
coefs$name = row.names(coefs)
coefs = coefs[order(coefs$Pr...t..),]
write.xlsx(lmfit_summary$coefficients, file="coefs_margin_noPGM.xlsx")

cvfit = cv.lm(data=margindata, lmfit3, m=3) # 3 fold cross-validation
cvfit
MAE = mean(abs(cvfit$cvpred-cvfit$MarginPercent))
MAE

set.seed(1234567)
k=3

folds = createFolds(margindata$MarginPercent, k, returnTrain = T)
# folds[[1]]
coefs = list()
MAE = numeric(k)
for (i in 1:k){
  inTrain = folds[[i]]
  data_tr = margindata[inTrain,]
  data_te = margindata[-inTrain, -dim(margindata)[2]]
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
