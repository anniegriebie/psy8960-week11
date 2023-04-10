## Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(doParallel)
set.seed(331)

## Data Import and Cleaning
gssoriginal_tbl <- read_sav("GSS2016.sav")%>%
  rename(workhours = MOSTHRS) %>%
  filter(complete.cases(workhours)) %>%
  select(-'HRS1', -'HRS2')

gss_tbl <- gssoriginal_tbl[, colSums(is.na(gssoriginal_tbl)) < .75 *nrow(gssoriginal_tbl)] %>%
  mutate(workhours = as.integer(workhours))

## Analysis 

rows <- sample(nrow(gss_tbl))
shuffled_data <- gss_tbl[rows,]
split <-round(nrow(shuffled_data) *0.75)
train_gss_tbl <- shuffled_data[1:split, ] %>%
  mutate_all(as.numeric)
test_gss_tbl <- shuffled_data[(split +1): nrow(shuffled_data),] 

folds <- createFolds(train_gss_tbl$workhours, 10)

tic()
OLS <- train(
  workhours ~ .,
  train_gss_tbl,
  method = "lm",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
)
timeOLSnp <- toc()

hov_cor_1 <- cor(
  predict(OLS, test_gss_tbl, na.action=na.pass),
  test_gss_tbl$workhours
)^2

tic()
ElasticNet <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "glmnet",
    na.action = na.pass,
    preProcess = c("center", "scale", "nzv", "medianImpute"),
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
timeENnp <- toc()
hov_cor_2 <- cor(
  predict(ElasticNet, test_gss_tbl, na.action = na.pass),
  test_gss_tbl$workhours
)^2

tic()
RandomForest <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "ranger",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
timeRFnp <- toc()
hov_cor_3 <- cor(
  predict(RandomForest, test_gss_tbl, na.action = na.pass),
  test_gss_tbl$workhours
)^2

tic()
boost <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "xgbLinear",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
timeboostnp <- toc()
hov_cor_4 <- cor(
  predict(boost, test_gss_tbl, na.action = na.pass),
  test_gss_tbl$workhours
)^2

#changing to 14 because running on supercomputer. Tested and process would be killed with more than 14 cores on Friday.
local_cluster <- makeCluster(14)
registerDoParallel(local_cluster)

tic()
OLS <- train(
  workhours ~ .,
  train_gss_tbl,
  method = "lm",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
)
timeOLS <- toc()

hov_cor_1 <- cor(
  predict(OLS, test_gss_tbl, na.action=na.pass),
  test_gss_tbl$workhours
)^2

tic()
ElasticNet <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "glmnet",
    na.action = na.pass,
    preProcess = c("center", "scale", "nzv", "medianImpute"),
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
timeEN <- toc()
hov_cor_2 <- cor(
  predict(ElasticNet, test_gss_tbl, na.action = na.pass),
  test_gss_tbl$workhours
)^2

tic()
RandomForest <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "ranger",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
timeRF <- toc()
hov_cor_3 <- cor(
  predict(RandomForest, test_gss_tbl, na.action = na.pass),
  test_gss_tbl$workhours
)^2

tic()
boost <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "xgbLinear",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
timeboost <- toc()
hov_cor_4 <- cor(
  predict(boost, test_gss_tbl, na.action = na.pass),
  test_gss_tbl$workhours
)^2

stopCluster(local_cluster)
registerDoSEQ()

## Publication
summary(resamples(list(OLS, ElasticNet, RandomForest, boost)))
resample_sum <- summary(resamples(list(OLS, ElasticNet, RandomForest, boost)))

Table3 <- tibble(
  algo = c("lm","Elastic Net","Random Forest","Xtreme Gradient Boost"),
  cv_rsq = str_remove(
    format(round(
    resample_sum$statistics$Rsquared[,"Mean"],2
  ), nsmall=2),"^0"),
  ho_rsq = str_remove(c(
    format(round(hov_cor_1,2),nsmall=2),
    format(round(hov_cor_2,2),nsmall=2),
    format(round(hov_cor_3,2),nsmall=2),
    format(round(hov_cor_4,2),nsmall=2)
  ),"^0")
) 


Table4 <- tibble(
  algo= c("lm", "Elastic Netc", "Random Forest", "Xtreme Gradient Boost"),
  supercomputer = c(timeOLSnp$callback_msg, timeENnp$callback_msg, timeRFnp$callback_msg, timeboostnp$callback_msg),
  "supercomputer-14" = c(timeOLS$callback_msg, timeEN$callback_msg, timeRF$callback_msg, timeboost$callback_msg)
)

#output
write_csv(Table3, "table3.csv")
write_csv(Table4, "table4.csv")

#1. The model that benefited the most from moving to the supercomputer was the Xtreme Gradient boost model. Without parralllel processing the Xtreme Gradient boost model has by far the longest run time, and then on the supercomputer the runtime was reduced by about 10x which was the greatest reduction compared to any of the other models. I think this has to do with the complexity of the model and the large number of hyperparameters associated with the Xtreme Gradient Boost model thus having more cores with the supercomputer allowed for the greatest amount of improvement. The other models with fewer hyperparameters did not need the larger number of cores and were already running basically as fast as they could with the fewer cores used in the non-supercomputing parallelization model.

#2. I believe the relationship between time and the number of cores used is that as the number of cores used increases, the time it takes to run the models decreases. I used 7 cores on my personal computer and doubled that to be 14 cores on the supercomputer. I did not use more than 14 cores o the supercomputer because although the amdsmall MSI partition has 128 cores if I used a number larger than 14 the batch would fail due to overuse of cores. I am not sure if that was happening because there were many other people also using MSI. I assume if I was able to use more than 14 cores the time it would take to run the models would decrease even more. So number of cores is related to efficiency. 

#3.I would choose to use the Random Forest model because it has the highest hoR^2 value output across all tables. Additionally, the Random Forest model took substantially less time than the Xtreme Gradient boost model(the other model that had a high coR^2 value) when the models were not parralelized. In parallelization these the Random Forest Model and Xtreme Gradient Boost models ran at relatively comparable times, and both were relatively short times when run on the supercomputer (within about 2 seconds of each other). Therefore due to the Random Forest having the highest hoR^2 value across models and running at seemingly more reasonable times for both the non-parralelized models and parallelized outputs, I would choose the Random Forest model. Additionally, if given the option I would choose to run a Random Forest Model using parallelization on a Supercomputer because that makes the models run the fastest of the different options for runnin the models. 