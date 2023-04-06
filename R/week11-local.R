## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(doParallel)
set.seed(331)

## Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav")%>%
  rename(workhours = MOSTHRS) %>%
  filter(complete.cases(workhours))

#deleting other two work hours columns from dataset, changes variable number by two as a check. 
gss_tbl[ ,c('HRS1', 'HRS2')] <- list(NULL)

#removing variables with less than 75% missingless fixed this from last week, was selecting those with more than .75 missingness
gss_tbl <- gss_tbl[, colSums(is.na(gss_tbl)) < .75 *nrow(gss_tbl)] %>%
  mutate(workhours = as.integer(workhours))

##Visualization 

#using ggplot to dispalce histogram of workhours, keeping default number of bins at 30
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "workhours", y="frequency", title= "Univariate Distribution of Workhours")

## Analysis 

rows <- sample(nrow(gss_tbl))
shuffled_data <- gss_tbl[rows,]
split <-round(nrow(shuffled_data) *0.75)
train_gss_tbl <- shuffled_data[1:split, ] %>%
  mutate_all(as.numeric)
test_gss_tbl <- shuffled_data[(split +1): nrow(shuffled_data),] 

folds <- createFolds(train_gss_tbl$workhours, 10)

#The following models are "original" meaning no parallelization
#adding tic and toc to beginning and end of each model to benchmark, assigning the tic toc output to a variable for each model so it can be called later in the publicaiton section 
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


#initializing parallelization with same models
local_cluster <- makeCluster(7)
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

# turn off parallel processing
stopCluster(local_cluster)
registerDoSEQ()

## Publication
summary(resamples(list(OLS, ElasticNet, RandomForest, boost)))
resample_sum <- summary(resamples(list(OLS, ElasticNet, RandomForest, boost)))

table1_tbl <- tibble(
  algo = c("lm","Elastic Net","Random Forest","Xtreme Gradient Boost"),
  cv_rsq = str_remove(round(
    resample_sum$statistics$Rsquared[,"Mean"],2
  ),"^0"),
  ho_rsq = str_remove(c(
    format(round(hov_cor_1,2),nsmall=2),
    format(round(hov_cor_2,2),nsmall=2),
    format(round(hov_cor_3,2),nsmall=2),
    format(round(hov_cor_4,2),nsmall=2)
  ),"^0")
) 

#creating table2_tbl by calling the time elapsed from the variables created using benchmarking 

table2_tbl <- tibble(
  algo= c("lm", "Elastic Netc", "Random Forest", "Xtreme Gradient Boost"),
  original = c(timeOLSnp$callback_msg, timeENnp$callback_msg, timeRFnp$callback_msg, timeboostnp$callback_msg),
  parrallelized = c(timeOLS$callback_msg, timeEN$callback_msg, timeRF$callback_msg, timeboost$callback_msg)
)

#1. The model that benefited the most from parallelization was the Xtremem gradient boost model, it had the greatest number of seconds in runtime reduced. I think part of this has to do with how long it runs without parallel processing, so it has more room for improvement than some of the other models. 
#2. The difference between the fastest parallelized model (Elastic Net) and the slowest parallelized model (Random Forest) was about 51 seconds. I think this was because the lm and elastic net models have less parameters and take a shorter amount of time than the other two models in the non-parallelized version. 
#3. If my supervisor asked me to pick a model for use in a production model I think I would choose the Xtreme Gradient Boost when done using parallelization. The Xtreme Gradient Boost has the highest k-fold CV R^2 value, but takes a very long time to run without parallel processing. However, if parallel processing is an option then it runs takes a very similar amount of time to run as the Random Forest model which also has a high R^2 (although lower than the Xtreme Gradient Boost model) thus I think it is best to use the model with the higher R^2 and therefore suggest the Xtreme Gradient Boost model. 
