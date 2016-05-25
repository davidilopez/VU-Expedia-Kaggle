library(rattle)
library(plyr)
library(dplyr)
library(gbm)

## Load the data
train <- read.csv("data/training_set_VU_DM_2014.csv", stringsAsFactors=FALSE)

## Turn all the factor/boolean variables into factor class
# train$site_id <- as.factor(train$site_id)
# train$visitor_location_country_id <- as.factor(train$visitor_location_country_id)
# train$prop_country_id <- as.factor(train$prop_country_id)
# train$prop_id <- as.factor(train$prop_id)
# train$prop_brand_bool <- as.factor(train$prop_brand_bool)
# train$promotion_flag <- as.factor(train$promotion_flag)
# train$srch_destination_id <- as.factor(train$srch_destination_id)
# train$srch_saturday_night_bool <- as.factor(train$srch_saturday_night_bool)
# train$random_bool <- as.factor(train$random_bool)
# train$click_bool <- as.factor(train$click_bool)
# train$booking_bool <- as.factor(train$booking_bool)

train$site_id <- as.numeric(train$site_id)
train$visitor_location_country_id <- as.numeric(train$visitor_location_country_id)
train$prop_country_id <- as.numeric(train$prop_country_id)
train$prop_id <- as.numeric(train$prop_id)
train$prop_brand_bool <- as.numeric(train$prop_brand_bool)
train$promotion_flag <- as.numeric(train$promotion_flag)
train$srch_destination_id <- as.numeric(train$srch_destination_id)
train$srch_saturday_night_bool <- as.numeric(train$srch_saturday_night_bool)
train$random_bool <- as.numeric(train$random_bool)
train$click_bool <- as.numeric(train$click_bool)
train$booking_bool <- as.numeric(train$booking_bool)

## Turn the date variables into POSIX class
train$date_time <- as.POSIXct(train$date_time)

## Turn all numeric variables to numeric class
train$srch_id <- as.numeric(train$srch_id)
train$visitor_hist_starrating <- as.numeric(train$visitor_hist_starrating)
train$visitor_hist_adr_usd <- as.numeric(train$visitor_hist_adr_usd)
train$prop_review_score <- as.numeric(train$prop_review_score)
train$prop_location_score1 <- as.numeric(train$prop_location_score1)
train$prop_location_score2 <- as.numeric(train$prop_location_score2)
train$prop_log_historical_price <- as.numeric(train$prop_log_historical_price)
train$price_usd <- as.numeric(train$price_usd)
train$srch_length_of_stay <- as.numeric(train$srch_length_of_stay)
train$srch_booking_window <- as.numeric(train$srch_booking_window)
train$srch_adults_count <- as.numeric(train$srch_adults_count)
train$srch_children_count <- as.numeric(train$srch_children_count)
train$srch_query_affinity_score <- as.numeric(train$srch_query_affinity_score)
train$orig_destination_distance <- as.numeric(train$orig_destination_distance)
train$gross_bookings_usd <- as.numeric(train$gross_bookings_usd)
train$position <- as.numeric(train$position)


## Remove missing values from the competitor comparison variables and turn into
## factor classes
train$comp1_rate[train$comp1_rate == "NULL"] <- 0
train$comp1_inv[train$comp1_inv == "NULL"] <- 0
train$comp1_rate_percent_diff[train$comp1_rate_percent_diff == "NULL"] <- 0

train$comp2_rate[train$comp2_rate == "NULL"] <- 0
train$comp2_inv[train$comp2_inv == "NULL"] <- 0
train$comp2_rate_percent_diff[train$comp2_rate_percent_diff == "NULL"] <- 0

train$comp3_rate[train$comp3_rate == "NULL"] <- 0
train$comp3_inv[train$comp3_inv == "NULL"] <- 0
train$comp3_rate_percent_diff[train$comp3_rate_percent_diff == "NULL"] <- 0

train$comp4_rate[train$comp4_rate == "NULL"] <- 0
train$comp4_inv[train$comp4_inv == "NULL"] <- 0
train$comp4_rate_percent_diff[train$comp4_rate_percent_diff == "NULL"] <- 0

train$comp5_rate[train$comp5_rate == "NULL"] <- 0
train$comp5_inv[train$comp5_inv == "NULL"] <- 0
train$comp5_rate_percent_diff[train$comp5_rate_percent_diff == "NULL"] <- 0

train$comp6_rate[train$comp6_rate == "NULL"] <- 0
train$comp6_inv[train$comp6_inv == "NULL"] <- 0
train$comp6_rate_percent_diff[train$comp6_rate_percent_diff == "NULL"] <- 0

train$comp7_rate[train$comp7_rate == "NULL"] <- 0
train$comp7_inv[train$comp7_inv == "NULL"] <- 0
train$comp7_rate_percent_diff[train$comp7_rate_percent_diff == "NULL"] <- 0

train$comp8_rate[train$comp8_rate == "NULL"] <- 0
train$comp8_inv[train$comp8_inv == "NULL"] <- 0
train$comp8_rate_percent_diff[train$comp8_rate_percent_diff == "NULL"] <- 0

# train$comp1_inv <- as.factor(train$comp1_inv)
# train$comp2_inv <- as.factor(train$comp2_inv)
# train$comp3_inv <- as.factor(train$comp3_inv)
# train$comp4_inv <- as.factor(train$comp4_inv)
# train$comp5_inv <- as.factor(train$comp5_inv)
# train$comp6_inv <- as.factor(train$comp6_inv)
# train$comp7_inv <- as.factor(train$comp7_inv)
# train$comp8_inv <- as.factor(train$comp8_inv)
# 
# train$comp1_rate <- as.factor(train$comp1_rate)
# train$comp2_rate <- as.factor(train$comp2_rate)
# train$comp3_rate <- as.factor(train$comp3_rate)
# train$comp4_rate <- as.factor(train$comp4_rate)
# train$comp5_rate <- as.factor(train$comp5_rate)
# train$comp6_rate <- as.factor(train$comp6_rate)
# train$comp7_rate <- as.factor(train$comp7_rate)
# train$comp8_rate <- as.factor(train$comp8_rate)

train$comp1_inv <- as.numeric(train$comp1_inv)
train$comp2_inv <- as.numeric(train$comp2_inv)
train$comp3_inv <- as.numeric(train$comp3_inv)
train$comp4_inv <- as.numeric(train$comp4_inv)
train$comp5_inv <- as.numeric(train$comp5_inv)
train$comp6_inv <- as.numeric(train$comp6_inv)
train$comp7_inv <- as.numeric(train$comp7_inv)
train$comp8_inv <- as.numeric(train$comp8_inv)

train$comp1_rate <- as.numeric(train$comp1_rate)
train$comp2_rate <- as.numeric(train$comp2_rate)
train$comp3_rate <- as.numeric(train$comp3_rate)
train$comp4_rate <- as.numeric(train$comp4_rate)
train$comp5_rate <- as.numeric(train$comp5_rate)
train$comp6_rate <- as.numeric(train$comp6_rate)
train$comp7_rate <- as.numeric(train$comp7_rate)
train$comp8_rate <- as.numeric(train$comp8_rate)

train$comp1_rate_percent_diff <- as.numeric(train$comp1_rate_percent_diff)
train$comp2_rate_percent_diff <- as.numeric(train$comp2_rate_percent_diff)
train$comp3_rate_percent_diff <- as.numeric(train$comp3_rate_percent_diff)
train$comp4_rate_percent_diff <- as.numeric(train$comp4_rate_percent_diff)
train$comp5_rate_percent_diff <- as.numeric(train$comp5_rate_percent_diff)
train$comp6_rate_percent_diff <- as.numeric(train$comp6_rate_percent_diff)
train$comp7_rate_percent_diff <- as.numeric(train$comp7_rate_percent_diff)
train$comp8_rate_percent_diff <- as.numeric(train$comp8_rate_percent_diff)


## Get the aggregated property location score

train$prop_location_score1[is.na(train$prop_location_score1)] <- 0
train$prop_location_score2[is.na(train$prop_location_score2)] <- 0
train$prop_location_score_aggregate <- train$prop_location_score1 + train$prop_location_score2

## Remove missing values from the property review score
train$prop_review_score[is.na(train$prop_review_score)] <- 0

## Get a testing subset of the data by selecting the users with IDs which modulo 
## 4 is equal to 0.

# test_subset <- train[train$srch_id %% 4 == 0,]
# train_subset <- train[train$srch_id %% 4 != 0,]

train[is.na(train)] <- 0

## Get the mean historic price
train$prop_historical_price <- exp(train$prop_log_historical_price)

## Calculate the difference between historic price and current price
train$prop_delta_price <- train$price_usd - train$prop_historical_price

## Get the price order within the same user search
train <- train[order(train$srch_id, train$price_usd),]
train$price_order <- ave(train$price_usd, train$srch_id, FUN = seq_along)

## Get the target date for reservation
train$target_date <- train$date_time + (((train$srch_booking_window * 60) * 60) * 24)
train$target_month <- months(train$target_date)
train$target_month_num <- match(train$target_month, month.name)
