## Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(doParallel)
set.seed(331)

## Data Import and Cleaning
gssoriginal_tbl <- read_sav("../data/GSS2016.sav")%>%
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


local_cluster <- makeCluster(126)
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
  cv_rsq = str_remove(round(
    resample_sum$statistics$Rsquared[,"Mean"],2
  ),"^0"),
  ho_rsq = str_remove(c(
    format(round(hov_cor_1,2),nsmall=2),
    format(round(hov_cor_2,2),nsmall=2),
    format(round(hov_cor_3,2),nsmall=2),
    format(round(hov_cor_3,2),nsmall=2)
  ),"^0")
) 


Table4 <- tibble(
  algo= c("lm", "Elastic Netc", "Random Forest", "Xtreme Gradient Boost"),
  supercomputer = c(timeOLSnp$callback_msg, timeENnp$callback_msg, timeRFnp$callback_msg, timeboostnp$callback_msg),
  "supercomputer-126" = c(timeOLS$callback_msg, timeEN$callback_msg, timeRF$callback_msg, timeboost$callback_msg)
)

#output
write_csv(Table3, "table3.csv")
write_csv(Table4, "table4.csv")