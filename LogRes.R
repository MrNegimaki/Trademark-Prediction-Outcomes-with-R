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
alldate <- ymd(case2012$registration_dt)
alldate <- year(alldate)
case2012in <- select(case2012, ends_with("in"))
case2012cd <- select(case2012, ends_with("cd"))
case2012 <- bind_cols(case2012in, case2012cd)
rm(case2012cd)
rm(case2012in)

## Create dependent variable is_passed -- including all status codes above and including 700 (Registered). 

case2012 <- mutate(case2012, combi = lb_itu_cur_in + lb_itu_file_in)
case2012$is_passed <- case2012$cfh_status_cd >= 700
case2012$cfh_status_cd = NULL
case2012$year = alldate
case2012$reg_cancel_cd = as.factor(case2012$reg_cancel_cd)

## Remove all trademarks registered before 1980 to create more predictive model.

case2012 <- subset(case2012, year >= 1980)

## Clean mark_draw_cd to remove NULL and single-character strings. Seperate into Character Type and Size

library(tidyr)
case2012d <- subset(case2012, mark_draw_cd != "")
case2012d <- subset(case2012d, mark_draw_cd != "1")
case2012d <- subset(case2012d, mark_draw_cd != "2")
case2012d <- subset(case2012d, mark_draw_cd != "3")
case2012d <- subset(case2012d, mark_draw_cd != "4")
draw_cd <- separate(case2012d, mark_draw_cd, c("draw_cd1","draw_cd2"), sep = 1)
case2012 <- draw_cd
case2012$draw_cd1 <- as.factor(case2012$draw_cd1)
case2012$draw_cd2 <- as.factor(case2012$draw_cd2)
rm(case2012d)
rm(draw_cd)

## Undersample to address class imbalance.

case2012TRUE <- subset(case2012, is_passed == TRUE)
case2012TRUE_EVEN <- sample_frac(case2012TRUE, size = 0.0035)
case2012FALSE <- subset(case2012, is_passed == FALSE)
case2012 <- bind_rows(case2012TRUE_EVEN, case2012FALSE)
rm(list = setdiff(ls(), "case2012"))

## Split sample into training and test sets.

library(caTools)
split = sample.split(case2012$is_passed, SplitRatio = 0.75)
caseTrain = subset(case2012, split == TRUE)
caseTest = subset(case2012, split == FALSE)
rm(case2012)
rm(split)

## Determine whether feature engineering has any effect.

logPassed1 <- glm(is_passed ~ lb_itu_cur_in + lb_itu_file_in, data = caseTrain, family = binomial) ## AIC: 7556
logPassed2 <- glm(is_passed ~ combi, data = caseTrain, family = binomial) ## AIC: 7554
logPassed3 <- glm(is_passed ~ lb_itu_cur_in + lb_itu_file_in + draw_cd2, data = caseTrain, family = binomial) ## AIC: 7391
logPassed4 <- glm(is_passed ~ combi + draw_cd2, data = caseTrain, family = binomial) ## AIC: 7390

## Create ROC curve to determine best threshold with logPassed3. Test against dependent variable in training and test set.

library(ROCR)
PredictTrain = predict(logPassed3, type = "response", newdata = caseTrain)
ROCpred <- prediction(PredictTrain, caseTrain$is_passed)
ROCperf <- performance(ROCpred, "tpr", "fpr")
plot(ROCperf)
plot(ROCperf, colorize = TRUE)
plot(ROCperf, colorize = TRUE, print.cutoffs.at = c(0.81,0.82), text.adj = c(-0.2,1.7)) ## choose 0.82
table(caseTrain$is_passed, PredictTrain < 0.82) ## .491 fit to model
predtest = predict(logPassed2, type = "response", newdata = caseTest)
table(caseTest$is_passed, predtest < 0.82) ## .860 fit to model
rm(list = setdiff(ls(), "logPassed3"))

## Download 2015 dataset and use 2013 - 2015 years as second test set.

temp <- tempfile()
download.file("https://data.uspto.gov/data3/trademark/casefile/economics/2015/case_file.csv.zip", temp)
case2015 <- unzip(temp, files = "case_file.csv") %>% fread()
rm(temp)
alldate <- ymd(case2015$registration_dt)
alldate <- year(alldate)
case2015in <- select(case2015, ends_with("in"))
case2015cd <- select(case2015, ends_with("cd"))
case2015 <- bind_cols(case2015in, case2015cd)
rm(case2015cd)
rm(case2015in)
case2015$is_passed <- case2015$cfh_status_cd >= 700
case2015 <- mutate(case2015, combi = lb_itu_cur_in + lb_itu_file_in)
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
predtest2 = predict(logPassed3, type = "response", newdata = case2015d)
table(case2015d$is_passed, predtest2 < 0.82) ## 60% accuracy, with 60% sens and 29% spec

## Create ROC curve for 2013-2015 test data.

library(ROCR)
pred2 = prediction(predtest2,case2015d$is_passed)
perf2 = performance(pred2,"tpr","fpr")
plot(perf2, colorize = TRUE)
performance(pred2, "auc")@y.values ## .556 AUC