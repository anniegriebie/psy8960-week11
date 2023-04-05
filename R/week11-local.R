## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(doParallel)

#turn on parallel processing
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

## Data Import and Cleaning

gss_tbl <- read_sav("../data/GSS2016.sav")%>%
  rename(workhours = MOSTHRS) %>%
  filter(complete.cases(workhours))

gss_tbl <- gss_tbl[, colSums(is.na(gss_tbl)) < .75 *nrow(gss_tbl)] %>%
  mutate(workhours = as.integer(workhours))


##Visualization 

ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "workhours", y="frequency", title= "Univariate Distribution of Workhours")

## Analysis

set.seed(331)

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

## Publication

algo = c("OLS", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting")
cv_rsq <- c( str_remove(round(OLSR, 2), pattern = "^0"),
             str_remove(round(ElasticR, 2), pattern = "^0"),
             str_remove(round(RandomR, 2), pattern = "^0"),
             str_remove(round(boostR, 2), pattern = "^0")
)

ho_rsq <- c( str_remove(round(OLSho, 2), pattern = "^0"),
             str_remove(round(Elasticho, 2), pattern = "^0"),
             str_remove(round(Randomho, 2), pattern = "^0"),
             str_remove(round(boostho, 2), pattern = "^0")
)


summary(resamples(list(OLS, ElasticNet, RandomForest, boost)))
resample_sum <- summary(list(OLS, ElasticNet, RandomForest, boost))

table1_tbl <- tibble(
  algo= c("lm", "Elastic Netc", "Random Forest", "Xtreme Gradient Boost"),
  cv_rsq = str_remove(round(resample_sum$statistic$Rsquared [ ,"Mean"], 2), 
                      "^0"),
  ho_rsq= str_remove(c(
    format(round(hov_cor_1, 2), nsmall = 2),
    format(round(hov_cor_2, 2), nsmall = 2),
    format(round(hov_cor_3, 2), nsmall =2),
    format(round(hov_cor_4, 2), small =2)
  ), "^0")
)

# turn off parallel processing
stopCluster(local_cluster)
registerDoSEQ()

table2_tbl <- tibble(
  algo= c("lm", "Elastic Netc", "Random Forest", "Xtreme Gradient Boost"),
  original
  parrallelized
)


