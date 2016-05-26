library(xgboost)
library(caret)
library(rattle)

train.clean <- read.csv("data/clean_training_data.csv", stringsAsFactors=FALSE)
# source('preprocessing.R')

set.seed(0)
individual_users <- unique(train.clean$srch_id)
sample_users_pool <- sample(individual_users, (length(individual_users) * .2))

sample.train <- unique(sort(sample_users_pool[1:length(sample_users_pool)/2]))
sample.test <- unique(sort(sample_users_pool[(length(sample_users_pool)/2)+1:length(sample_users_pool)]))

train.sample <- train.clean[train.clean$srch_id %in% sample.train,]
test.sample <- train.clean[train.clean$srch_id %in% sample.test,]

rm(train.clean, individual_users, sample_users_pool, sample.train, sample.test)
# rm(train.clean, individual_users, sample_users_pool, sample.train )

## Set up the initial model for click_bool
# Get the initial variables from the text file
variable.names <- scan("selected_features.txt", what="", sep="\n")

## Create formulas for the models---------
# Prediction of clicking on a hotel
click_bool.formula <- paste( "click_bool", '~', paste( variable.names, collapse=' + ' ) )
click_bool.formula <- as.formula(click_bool.formula)

# Prediction of booking a hotel
booking_bool.formula <- paste( "booking_bool", '~', paste( variable.names, collapse=' + ' ), '+ click_bool' )
booking_bool.formula <- as.formula(booking_bool.formula)

# Prediction of the ranking of the hotels
position.formula <- paste( "position", '~', paste( variable.names, 
                                                   collapse=' + ' ), 
                           '+ click_bool + booking_bool' )
position.formula <- as.formula(position.formula)

## Model each prediction------------------------
xgboost.click_bool <- xgboost(data = sapply(train.sample[variable.names], as.numeric), 
                              label = train.sample$click_bool,
                              params = list(objective = "binary:logistic",
                                            eta = 0.001,
                                            max.depth = 10,
                                            nthread = 6),
                              nrounds = 20,
                              verbose = 0)

xgboost.click_bool.pred <- predict(object = xgboost.click_bool, 
                                   newdata = sapply(test.sample[variable.names], as.numeric))

test.sample$click_bool.pred <- ifelse(xgboost.click_bool.pred > 0.489, 1, 0)

# confusionMatrix(data = test.sample$click_bool.pred, 
#                 reference = test.sample$click_bool)

xgboost.booking_bool <- xgboost(data = sapply(train.sample[c(variable.names,'click_bool')], as.numeric), 
                              label = train.sample$booking_bool,
                              params = list(objective = "binary:logistic",
                                            eta = 0.001,
                                            max.depth = 10,
                                            nthread = 6),
                              nrounds = 20,
                              verbose = 0)

xgboost.booking_bool.pred <- predict(object = xgboost.booking_bool, 
                                   newdata = sapply(test.sample[c(variable.names,'click_bool')], as.numeric))

test.sample$booking_bool.pred <- ifelse(xgboost.booking_bool.pred > 0.50, 1, 0)

# confusionMatrix(data = test.sample$booking_bool.pred, 
#                 reference = test.sample$booking_bool)

xgboost.position <- xgboost(data = sapply(train.sample[c(variable.names,
                                                         'click_bool', 
                                                         'booking_bool')], 
                                          as.numeric), 
                                label = train.sample$position,
                                params = list(objective = "rank:pairwise",
                                              eta = 0.001,
                                              max.depth = 10,
                                              nthread = 6,
                                              eval_metric = 'ndcg'),
                                nrounds = 20,
                                verbose = 0)

xgboost.position.pred <- predict(object = xgboost.position, 
                                     newdata = sapply(test.sample[c(variable.names,
                                                                    'click_bool', 
                                                                    'booking_bool')], 
                                                      as.numeric))
test.sample$position.pred <- xgboost.position.pred
test.sample <- test.sample[order(test.sample$srch_id, test.sample$position.pred, decreasing = TRUE),]
test.sample$position.order <- ave(test.sample$position.pred, test.sample$srch_id, FUN = seq_along)
test.sample <- test.sample[order(test.sample$srch_id, test.sample$position.order),]
check <- data.frame(cbind(test.sample$srch_id, test.sample$position, test.sample$position.order))