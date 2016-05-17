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
draw_cd <- separate(case2012d, mark_draw_cd, c("draw_cd1","draw_cd2"), sep = 1)
case2012 <- draw_cd
rm(case2012d)
rm(draw_cd)

## Create dependent variable is_passed -- including all status codes above and including 700 (Registered). 
## Also remove features that slow tree-making process without contributing significantly.

case2012$is_passed <- case2012$cfh_status_cd >= 700
case2012$cfh_status_cd = NULL
case2012$exm_office_cd = NULL
case2012$ir_first_refus_in = NULL
case2012$ir_priority_in = NULL
case2012$related_other_in = NULL
case2012$reg_cancel_cd = NULL
case2012$ir_status_cd = NULL

## Undersample to address class imbalance. Reduce sample further to prevent randomForest() from running forever.

case2012TRUE <- subset(case2012, is_passed == TRUE)
case2012TRUE_EVEN <- sample_frac(case2012TRUE, size = 0.7971)
case2012FALSE <- subset(case2012, is_passed == FALSE)
case2012 <- bind_rows(case2012TRUE_EVEN, case2012FALSE) %>% sample_frac(size = 0.04)
rm(list = setdiff(ls(), "case2012"))

## Split sample into training and test sets. Perform pre-cross-validation model and plot it. 

library(caTools)
split = sample.split(case2012$is_passed, SplitRatio = 0.75)
caseTrain = subset(case2012, split == TRUE)
caseTest = subset(case2012, split == FALSE)
rm(case2012)
rm(split)
library(randomForest)
caseTest$is_passed = as.factor(caseTest$is_passed)
caseTrain$is_passed = as.factor(caseTrain$is_passed)
caseTest$draw_cd1 = as.factor(caseTest$draw_cd1)
caseTrain$draw_cd1 = as.factor(caseTrain$draw_cd1)
caseTest$draw_cd2 = as.factor(caseTest$draw_cd2) ## Must remove draw_cd2: RandomForest won't accept factor levels >= 53
caseTrain$draw_cd2 = as.factor(caseTrain$draw_cd2)
caseTest$draw_cd2 = NULL
caseTrain$draw_cd2 = NULL
caseTrain <- subset(caseTrain, draw_cd1 != "6")
caseTrain$draw_cd1 <- droplevels(caseTrain$draw_cd1)
caseTreeTrain = randomForest(is_passed ~ ., data = caseTrain, nodesize = 25, ntree = 200)

## Test goodness-of-fit to test set.

predictForest = predict(caseTreeTrain, newdata = caseTest)
table(caseTest$is_passed, predictForest) ## 84.4% Accuracy

## Create ROC curve to determine AUC.

library(ROCR)
predictROC = predict(caseTreeTrain, newdata = caseTest, type = "prob")
pred = prediction(predictROC[,2],caseTest$is_passed)
perf = performance(pred,"tpr","fpr")
plot(perf, colorize = TRUE)
performance(pred, "auc")@y.values ## 82.5% AUC

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
case2015$ir_first_refus_in = NULL
case2015$ir_priority_in = NULL
case2015$related_other_in = NULL
case2015$reg_cancel_cd = NULL
case2015$ir_status_cd = NULL
case2015$year = alldate
case2015d <- subset(case2015, year > 2012)
case2015d <- subset(case2015d, mark_draw_cd != "")
case2015d <- subset(case2015d, mark_draw_cd != "1")
case2015d <- subset(case2015d, mark_draw_cd != "2")
case2015d <- subset(case2015d, mark_draw_cd != "3")
case2015d <- subset(case2015d, mark_draw_cd != "4")
case2015d$year = NULL
case2015d$is_passed <- as.factor(case2015d$is_passed)
draw_cd <- separate(case2015d, mark_draw_cd, c("draw_cd1","draw_cd2"), sep = 1)
case2015d <- draw_cd
case2015d$draw_cd2 = NULL
case2015d <- subset(case2015d, draw_cd1 != "6")
caseTrain <- subset(caseTrain, draw_cd1 != "0")
caseTrain <- subset(caseTrain, draw_cd1 != "1")
case2015d$draw_cd1 <- as.factor(case2015d$draw_cd1)
rm(draw_cd)
caseTrain <- droplevels(caseTrain)
caseTreeTrain = randomForest(is_passed ~ ., data = caseTrain, nodesize = 25, ntree = 200)
predForest2 = predict(caseTreeTrain, newdata = case2015d, type = "class")
table(case2015d$is_passed, predForest2) ## 72.7% fit to 2015 (82.8% Sens, 64.6% Spec)

## Create ROC Curve for 2013 - 2015 test data.

predictROC2 = predict(caseTreeTrain, newdata = case2015d, type = "prob")
pred2 = prediction(predictROC2[,2],case2015d$is_passed)
perf2 = performance(pred2,"tpr","fpr")
plot(perf2, colorize = TRUE)
performance(pred2, "auc")@y.values ## 70.1% AUC