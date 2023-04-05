## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

## Data Import and Cleaning

gss_tbl <- read_sav("../data/GSS2016.sav")%>%
  rename(workhours = HRS1) %>%
  filter(complete.cases(workhours))

gss_tbl <- gss_tbl[, which(colMeans(!is.na(gss_tbl)) >= 0.75)] %>%
  mutate_all(as.numeric)


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

OLS <- train(
  workhours ~ .,
  train_gss_tbl,
  method = "lm",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
)
OLSR<-OLS$results$Rsquared
predictOLS <- predict(OLS, test_gss_tbl, na.action = na.pass)
OLSho <-(cor(test_gss_tbl$workhours, predictOLS))^2

ElasticNet <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "glmnet",
    na.action = na.pass,
    preProcess = c("center", "scale", "nzv", "medianImpute"),
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
ElasticR <-mean(ElasticNet$results$Rsquared)
predictElastic <- predict(ElasticNet, test_gss_tbl, na.action = na.pass)
Elasticho <-(cor(test_gss_tbl$workhours, predictElastic))^2

RandomForest <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "ranger",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
RandomR<- mean(RandomForest$results$Rsquared)
predictRandom <- predict(RandomForest, test_gss_tbl, na.action = na.pass)
Randomho <-(cor(test_gss_tbl$workhours, predictRandom))^2

boost <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "xgbLinear",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
boostR <- mean(boost$results$Rsquared)
predictboost <- predict(boost, test_gss_tbl, na.action = na.pass)
boostho <-(cor(test_gss_tbl$workhours, predictboost))^2

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

table1_tbl <- tibble(
  algo, cv_rsq, ho_rsq
)
