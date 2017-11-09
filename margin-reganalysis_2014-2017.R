setwd("~/Predictive Analytics/Top-line revenue/margin/regression/margin_prediction")
library(xlsx)
library(caret)
regdata = read.xlsx("margin-regdata_fin_it.xlsx", sheetIndex = 1)
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


M = dim(margindata)[2]-1
r2 = numeric(M)
r2a = numeric(M)
coef = numeric(M)
for (i in 1:M){
  lm1 = lm(margindata$MarginPercent~margindata[,i], data = margindata)
  lm1 = summary(lm1)
  r2[i] = lm1$r.squared
  r2a[i] = lm1$adj.r.squared
  coef[i] = lm1$coefficients[2,1]
}

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

output = cbind(regdata$Year,regdata$Month, margindata,pred)
write.xlsx(output, file="regression-margin-fin-it-opt.xlsx")
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
# write.xlsx(lmfit_summary$coefficients,file="coeffs_margin_fin_it_opt.xlsx")
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

data_tr = margindata[c(1:40),]
data_te = margindata[c(41:45),]
lmtrain = lm(MarginPercent~., data=data_tr)
summary(lmtrain)

actual = data_tr$MarginPercent
pred_tr = predict(lmtrain, data_tr)
print(mean(abs(pred_tr-actual)))

pred_te = predict(lmtrain, newdata=data_te)
actual = data_te$MarginPercent
MAE = mean(abs(pred_te-actual))
print(MAE)

lmfit_summary = summary(lmtrain)
write.xlsx(lmfit_summary$coefficients, file="coefs_margin_fin_it_split.xlsx")
pred = pred_tr
output_tr = cbind(data_tr,pred)
output_tr$partition = "train"

pred = pred_te
output_te = cbind(data_te,pred)
output_te$partition = "test"
output = rbind(output_tr,output_te)
write.xlsx(output, file="regression-margin-fin-it_split.xlsx")

