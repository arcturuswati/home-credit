setwd("C:/Users/swati/Desktop/Everything else/Kaggle/HomeCreditDefaultRisk/input")

######  application_{train|test}.csv
# 
# This is the main table, broken into two files for Train (with TARGET) and Test (without TARGET).
# Static data for all applications. One row represents one loan in our data sample.

######  bureau.csv
# 
# All client's previous credits provided by other financial institutions that were reported to Credit Bureau (for clients who have a loan in our sample).
# For every loan in our sample, there are as many rows as number of credits the client had in Credit Bureau before the application date.

#####  bureau_balance.csv
# 
# Monthly balances of previous credits in Credit Bureau.
# This table has one row for each month of history of every previous credit reported to Credit Bureau - i.e the table has (#loans in sample * # of relative previous credits * # of months where we have some history observable for the previous credits) rows.

#####  POS_CASH_balance.csv
# 
# Monthly balance snapshots of previous POS (point of sales) and cash loans that the applicant had with Home Credit.
# This table has one row for each month of history of every previous credit in Home Credit (consumer credit and cash loans) related to loans in our sample - i.e. the table has (#loans in sample * # of relative previous credits * # of months in which we have some history observable for the previous credits) rows.

##### credit_card_balance.csv
# 
# Monthly balance snapshots of previous credit cards that the applicant has with Home Credit.
# This table has one row for each month of history of every previous credit in Home Credit (consumer credit and cash loans) related to loans in our sample - i.e. the table has (#loans in sample * # of relative previous credit cards * # of months where we have some history observable for the previous credit card) rows.

##### previous_application.csv
# 
# All previous applications for Home Credit loans of clients who have loans in our sample.
# There is one row for each previous application related to loans in our data sample.

##### installments_payments.csv
# 
# Repayment history for the previously disbursed credits in Home Credit related to the loans in our sample.
# There is a) one row for every payment that was made plus b) one row each for missed payment.
# One row is equivalent to one payment of one installment OR one installment corresponding to one payment of one previous Home Credit credit related to loans in our sample.

library(dplyr)
test <- read.csv('application_test.csv') # applications test data
train <- read.csv('application_train.csv') # applications train data
bureau <- read.csv('bureau.csv') # bureau data before the application date
bureau_bal <- read.csv('bureau_balance.csv') # 
cc_bal <- read.csv('credit_card_balance.csv')
installments <- read.csv('installments_payments.csv')
pos_cash_bal <- read.csv('POS_CASH_balance.csv')
prev_app <- read.csv('previous_application.csv')

a <- data.frame(summary(train[, -2])) %>% select(-Var1) %>% dplyr::rename(Train = Freq, Var1 = Var2) %>% 
  mutate(Var2 = trimws(sapply(strsplit(as.character(Train), split = ":", fixed = T), function(x) x[1]))
         , Train = sapply(strsplit(as.character(Train), split = ":", fixed = T), function(x) x[2])) %>% 
  arrange(Var1, Var2)

b <- data.frame(summary(test)) %>% select(-Var1) %>% dplyr::rename(Test = Freq, Var1 = Var2) %>% 
  mutate(Var2 = trimws(sapply(strsplit(as.character(Test), split = ":", fixed = T), function(x) x[1]))
         , Test = sapply(strsplit(as.character(Test), split = ":", fixed = T), function(x) x[2])) %>% 
  arrange(Var1, Var2)

c <- a %>% full_join(b) %>% filter(!(is.na(Train) & is.na(Test)))

write.csv(c, 'applicationDistribution.csv')
naCheck <- foreach(i = 1:dim(train)[2], .combine = rbind) %do% {
  data.frame(variable = names(train)[i], pNAs = round(sum(is.na(train[, i]))*100/dim(train)[1], 1))
}

train.corr <- train[complete.cases(train), -1]


for(i in 1:121) train.corr[ , i] <- as.numeric(train.corr[, i])

targetCorr <- cor(train.corr)

which(sapply(1:121, function(x) sum(is.na(targetCorr[, x]))) > 100)

targetCorr <- targetCorr[-c(4, 22, 96), -c(4, 22, 96)]


library(corrplot)
pdf("C:/Users/swati/Desktop/Everything else/Kaggle/HomeCreditDefaultRisk/CorrelationCluster.pdf")
corrplot(targetCorr, order = "hclust", tl.cex = 0.4, main = "Hierachical cluster order") # hierarchical clustering order
corrplot(targetCorr, order = "FPC", tl.cex = 0.4, main = "First principal component order") # First principal component order
corrplot(targetCorr, order = "AOE", tl.cex = 0.4, main = "Angular order of eigen vectors") # Angular order of eigen vector tan(e1/e1)
dev.off()

table(bureau$CREDIT_CURRENCY)

bureau2 <- bureau %>% group_by(SK_ID_CURR, CREDIT_ACTIVE) %>% 
  dplyr::summarise(count = n(), creditamount = sum(AMT_CREDIT_SUM)
                   , creditsumdebt = sum(AMT_CREDIT_SUM_DEBT)
                   , maxoverdue = max(AMT_CREDIT_MAX_OVERDUE))

train2 <- train %>% left_join(bureau %>% dplyr::rename(AMT_ANNUITY_b = AMT_ANNUITY))

test2 <- test %>% left_join(bureau %>% dplyr::rename(AMT_ANNUITY_b = AMT_ANNUITY))
