library(xgboost)
library(caret)

source('preprocessing_Kaggle.R')
source('processing_test_data_Kaggle.R')

set.seed(0)
individual_users <- unique(train.clean$srch_id)
sample_users_pool <- sample(individual_users, (length(individual_users) * .10))

sample.train <- unique(sort(sample_users_pool))

train.sample <- train.clean[train.clean$srch_id %in% sample.train,]

# Remove bulky data from memory
rm(train.clean, individual_users, sample_users_pool, sample.train, sample.test)

## Set up the initial model for click_bool
# Get the initial variables from the text file
variable.names <- scan("selected_features.txt", what="", sep="\n")

## Model each prediction------------------------
## Model the click boolean
xgboost.click_bool <- xgboost(data = sapply(train.sample[variable.names], as.numeric), 
                              label = train.sample$click_bool,
                              params = list(objective = "binary:logistic",
                                            eta = 0.01,
                                            max.depth = 40,
                                            nthread = 6),
                              nrounds = 3,
                              verbose = 0)
# Predict click_bool
xgboost.click_bool.pred <- predict(object = xgboost.click_bool, 
                                   newdata = sapply(test.sample[variable.names], as.numeric))
# Add to dataset
test.sample$click_bool.pred <- ifelse(xgboost.click_bool.pred > 0.489, 1, 0)

# Remove
rm(xgboost.click_bool, xgboost.click_bool.pred)


## Model the booking boolean
xgboost.booking_bool <- xgboost(data = sapply(train.sample[c(variable.names,'click_bool')], as.numeric), 
                              label = train.sample$booking_bool,
                              params = list(objective = "binary:logistic",
                                            eta = 0.01,
                                            max.depth = 40,
                                            nthread = 6),
                              nrounds = 3,
                              verbose = 0)
# predict booking_bool
xgboost.booking_bool.pred <- predict(object = xgboost.booking_bool, 
                                   newdata = sapply(test.sample[c(variable.names,'click_bool')], as.numeric))
# Add to dataset
test.sample$booking_bool.pred <- ifelse(xgboost.booking_bool.pred > 0.50, 1, 0)

# Remove
rm(xgboost.booking_bool, xgboost.booking_bool.pred)

## Model the ranking
xgboost.position <- xgboost(data = sapply(train.sample[c(variable.names,
                                                         'click_bool', 
                                                         'booking_bool')], 
                                          as.numeric), 
                                label = train.sample$position,
                                params = list(objective = "rank:pairwise",
                                              eta = 0.01,
                                              max.depth = 40,
                                              nthread = 6,
                                              eval_metric = 'ndcg'),
                                nrounds = 3,
                                verbose = 0)
# Predict the position in the ranking
xgboost.position.pred <- predict(object = xgboost.position, 
                                     newdata = sapply(test.sample[c(variable.names,
                                                                    'click_bool', 
                                                                    'booking_bool')], 
                                                      as.numeric))
test.sample$position.pred <- xgboost.position.pred
test.sample <- test.sample[order(test.sample$srch_id, test.sample$position.pred),]
test.sample$position.order <- ave(test.sample$position.pred, test.sample$srch_id, FUN = seq_along)
test.sample <- test.sample[order(test.sample$srch_id, test.sample$position.order),]
final_output <- cbind(test.sample$srch_id, test.sample$prop_id)
write.csv(final_output, 'Kaggle_Output_for_eval.csv')
