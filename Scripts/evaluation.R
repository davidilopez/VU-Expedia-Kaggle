library(xgboost)
library(caret)

## Read the data
train.clean <- read.csv('./data/clean_training_data.csv', header = TRUE, stringsAsFactors = FALSE)

# Generate rrandom samples for a training and a testing sets
set.seed(0)
# get a list of individual users
individual_users <- unique(train.clean$srch_id)
# get X% of all the users to test on
X = 0.10
sample_users_pool <- sample(individual_users, (length(individual_users) * X))

# make pools of users, dividing X evenly between training and testing
sample.train <- unique(sort(sample_users_pool[1:length(sample_users_pool)/2]))
sample.test <- unique(sort(sample_users_pool[(length(sample_users_pool)/2)+1:length(sample_users_pool)]))

# create datasets for training and testing
train.sample <- train.clean[train.clean$srch_id %in% sample.train,]
test.sample <- train.clean[train.clean$srch_id %in% sample.test,]

# Remove all the extra stuff
rm(train.clean, individual_users, sample_users_pool, sample.train, sample.test)

# Get the initial variables from the text file
variable.names <- scan("./selected_features.txt", what="", sep="\n")

## Create formulas for the models           DEPRECATED ---------
# # Prediction of clicking on a hotel
# click_bool.formula <- paste( "click_bool", '~', paste( variable.names, collapse=' + ' ) )
# click_bool.formula <- as.formula(click_bool.formula)
# 
# # Prediction of booking a hotel
# booking_bool.formula <- paste( "booking_bool", '~', paste( variable.names, collapse=' + ' ), '+ click_bool' )
# booking_bool.formula <- as.formula(booking_bool.formula)
# 
# # Prediction of the ranking of the hotels
# position.formula <- paste( "position", '~', paste( variable.names, 
#                                                    collapse=' + ' ), 
#                            '+ click_bool + booking_bool' )
# position.formula <- as.formula(position.formula)

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
# Check the results
confusionMatrix(data = test.sample$click_bool.pred,
                reference = test.sample$click_bool)
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
# Check the results
confusionMatrix(data = test.sample$booking_bool.pred,
                reference = test.sample$booking_bool)
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
check <- data.frame(cbind(test.sample$srch_id, test.sample$click_bool, test.sample$booking_bool, test.sample$position.order))
