# library(caret)
library(gbm)
# library(xgboost)
# library(pROC)
library(rattle)

train.clean <- read.csv("data/clean_training_data.csv", stringsAsFactors=FALSE)

set.seed(0)
individual_users <- unique(train.clean$srch_id)
sample_users_pool <- sample(individual_users, (length(individual_users) * .5))

sample.train <- unique(sort(sample_users_pool[1:length(sample_users_pool)/2]))
# sample.test <- unique(sort(sample_users_pool[(length(sample_users_pool)/2)+1:length(sample_users_pool)]))

train.sample <- train.clean[train.clean$srch_id %in% sample.train,]
# test.sample <- train.clean[train.clean$srch_id %in% sample.test,]

# rm(train.clean, individual_users, sample_users_pool, sample.train, sample.test)
rm(train.clean, individual_users, sample_users_pool, sample.train )


test.clean <- read.csv("data/clean_testing_data.csv", stringsAsFactors=FALSE)

## Set up the initial model for click_bool
# Get the initial variables from the text file
variable.names <- scan("selected_features.txt", what="", sep="\n")

## Create formulas for the models
# Prediction of clicking on a hotel
click_bool.formula <- paste( "click_bool", '~', paste( variable.names, collapse=' + ' ) )
click_bool.formula <- as.formula(click_bool.formula)

# Prediction of booking a hotel
booking_bool.formula <- paste( "booking_bool", '~', paste( variable.names, collapse=' + ' ), '+ click_bool' )
booking_bool.formula <- as.formula(booking_bool.formula)

# Prediction of the ranking of the hotels
position.formula <- paste( "booking_bool", '~', paste( variable.names, 
                                                       collapse=' + ' ), 
                           '+ click_bool + booking_bool' )
position.formula <- as.formula(position.formula)


## Model each prediction
gbm_click_bool <- gbm(formula = click_bool.formula, distribution = "bernoulli",
                      data = train.sample, n.trees = 100, cv.folds = 3, n.cores = 6)

test.clean$click_bool <- predict.gbm(object = gbm_click_bool, newdata = test.clean,
                                  type = "response", n.trees = 100)

gbm_booking_bool <- gbm(formula = booking_bool.formula, distribution = "bernoulli",
                      data = train.sample, n.trees = 100, cv.folds = 3, n.cores = 6)

test.clean$booking_bool <- predict.gbm(object = gbm_booking_bool, newdata = test.clean,
                                     type = "response", n.trees = 100)

gbm_position <- gbm(formula = position.formula, 
                    distribution = list(name = "pairwise", group = srch_id,
                                        metric = ndcg, max.rank = 40),
                    data = train.sample, n.trees = 100, cv.folds = 3, n.cores = 6)

test.clean$position <- predict.gbm(object = gbm_position, newdata = test.clean,
                                   type = "response", n.trees = 100)
