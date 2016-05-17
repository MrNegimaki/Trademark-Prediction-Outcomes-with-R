## Download trademark cases from years 1870 - 2012

set.seed(93)
temp <- tempfile()
download.file("https://data.uspto.gov/data3/trademark/casefile/economics/2012/case_file.csv.zip", temp)
library(data.table)
library(dplyr)
case2012 <- unzip(temp, files = "case_file.csv") %>% fread()
rm(temp)

## Remove all date-related features except for year filed 

library(lubridate)
alldate <- ymd(case2012$filing_dt)
alldate <- year(alldate)
case2012in <- select(case2012, ends_with("in"))
case2012cd <- select(case2012, ends_with("cd"))
case2012 <- bind_cols(case2012in, case2012cd)
rm(case2012cd)
rm(case2012in)

## Remove all trademarks registered before 1980 to create more predictive model.

case2012$year = alldate
case2012 <- subset(case2012, year >= 1980)
case2012$year = NULL

## Clean mark_draw_cd to remove NULL and single-character strings. Seperate into Character Type and Size

library(tidyr)
case2012d <- subset(case2012, mark_draw_cd != "")
case2012d <- subset(case2012d, mark_draw_cd != "1")
case2012d <- subset(case2012d, mark_draw_cd != "2")
case2012d <- subset(case2012d, mark_draw_cd != "3")
case2012d <- subset(case2012d, mark_draw_cd != "4")
draw_cd <- separate(case2012d,mark_draw_cd, c("draw_cd1","draw_cd2"), sep = 1)
case2012 <- draw_cd
rm(case2012d)
rm(draw_cd)

## Create dependent variable is_passed -- including all status codes above and including 700 (Registered). 
## Also remove features that slow tree-making process without contributing significantly.

case2012$is_passed <- case2012$cfh_status_cd >= 700
case2012$cfh_status_cd = NULL
case2012$exm_office_cd = NULL
case2012$reg_cancel_cd = NULL
case2012$use_afdv_acc_in = NULL
case2012$incontest_ack_in = NULL

## Undersample to address class imbalance.

case2012TRUE <- subset(case2012, is_passed == TRUE)
case2012TRUE_EVEN <- sample_frac(case2012TRUE, size = 0.7971)
case2012FALSE <- subset(case2012, is_passed == FALSE)
case2012 <- bind_rows(case2012TRUE_EVEN, case2012FALSE)
rm(list = setdiff(ls(), "case2012"))

## Split sample into training and test sets. Perform pre-cross-validation model and plot it.

library(rpart)
library(rpart.plot)
library(caTools)
sampleSplit = sample.split(case2012$is_passed, SplitRatio = 0.75)
caseTrain = subset(case2012, sampleSplit == TRUE)
caseTest = subset(case2012, sampleSplit == FALSE)
rm(case2012)
rm(sampleSplit)
caseTree = rpart(is_passed ~ ., data = caseTrain, method = "class", control = rpart.control(minbucket = 25))
prp(caseTree, extra = 2, varlen = 0)

## Remove any new levels before prediction. Test goodness-of-fit to test set.

new <- which(!(caseTest$draw_cd2 %in% levels(caseTrain$draw_cd2)))
caseTest$draw_cd2[new] <- NA
predictCase = predict(caseTree, newdata = caseTest, type = "class")
table(caseTest$is_passed, predictCase) ## 83.7% fit-to-model

## Create ROC curve to determine AUC.

library(ROCR)
predictROC = predict(caseTree, newdata = caseTest)
pred = prediction(predictROC[,2],caseTest$is_passed)
perf = performance(pred,"tpr","fpr")
plot(perf, colorize = TRUE)
performance(pred, "auc")@y.values ## .837 AUC

## Determine best control parameter with cross validation. Re-draw tree accordingly and test goodness of fit to test set.

library(caret)
library(e1071)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp=seq(0,1,0.01))
caseTrain$is_passed = as.factor(caseTrain$is_passed)
caseTrain$draw_cd1 = as.factor(caseTrain$draw_cd1)
caseTrain$draw_cd2 = as.factor(caseTrain$draw_cd2)
train(is_passed ~ ., data = caseTrain, method = "rpart", trControl = fitControl, tuneGrid = cartGrid) ## cp = 0.08
caseTreeCV = rpart(is_passed ~ ., data = caseTrain, method = "class", control = rpart.control(cp = 0.08)) ## control parameter not effective, and plots only to one feature with prp() function.
new <- which(!(caseTest$draw_cd2 %in% levels(caseTrain$draw_cd2)))
caseTest$draw_cd2[new] <- NA
predictCV = predict(caseTreeCV, newdata = caseTest, type = "class")
table(caseTest$is_passed, predictCV) ## 83.7% accuracy

## Create ROC curve for cross-validated model.

predictROCV = predict(caseTreeCV, newdata = caseTest)
predCV = prediction(predictROCV[,2],caseTest$is_passed)
perfcv = performance(predCV,"tpr","fpr")
plot(perfcv, colorize = TRUE)
performance(predCV, "auc")@y.values ## 83.7% AUC

## Download 2015 dataset and use 2013 - 2015 years as second test set.

temp <- tempfile()
download.file("https://data.uspto.gov/data3/trademark/casefile/economics/2015/case_file.csv.zip", temp)
case2015 <- unzip(temp, files = "case_file.csv") %>% fread()
rm(temp)
alldate <- ymd(case2015$filing_dt)
alldate <- year(alldate)
case2015in <- select(case2015, ends_with("in"))
case2015cd <- select(case2015, ends_with("cd"))
case2015 <- bind_cols(case2015in, case2015cd)
rm(case2015cd)
rm(case2015in)
case2015$is_passed <- case2015$cfh_status_cd >= 700
case2015$cfh_status_cd = NULL
case2015$exm_office_cd = NULL
case2015$year = alldate
case2015d <- subset(case2015, year > 2012)
case2015d <- subset(case2015d, mark_draw_cd != "")
case2015d <- subset(case2015d, mark_draw_cd != "1")
case2015d <- subset(case2015d, mark_draw_cd != "2")
case2015d <- subset(case2015d, mark_draw_cd != "3")
case2015d <- subset(case2015d, mark_draw_cd != "4")
draw_cd <- separate(case2015d,mark_draw_cd, c("draw_cd1","draw_cd2"), sep = 1)
case2015d <- draw_cd
rm(draw_cd)
new <- which(!(case2015d$draw_cd2 %in% levels(caseTrain$draw_cd2)))
case2015d$draw_cd2[new] <- NA
predtest = predict(caseTreeCV, newdata = case2015d, type = "class")
table(case2015d$is_passed, predtest) ## 71.7% fit to 2015 (84.1% Sens, 61.8% Spec)

## Create ROC Curve for 2013 - 2015 test data.

predictROC2 = predict(caseTreeCV, newdata = case2015d)
pred2 = prediction(predictROC2[,2],case2015d$is_passed)
perf2 = performance(pred2,"tpr","fpr")
plot(perf2, colorize = TRUE)
performance(pred2, "auc")@y.values ## .730 AUC