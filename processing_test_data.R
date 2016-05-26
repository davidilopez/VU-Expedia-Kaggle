library(rattle)
library(plyr)
library(dplyr)
library(gbm)

## Load the data
test <- read.csv("data/test_set_VU_DM_2014.csv", stringsAsFactors=FALSE)

## Turn the date variables into POSIX class
test$date_time <- as.POSIXct(test$date_time)

# Turn all the factor/boolean variables into factor class
test$site_id <- as.factor(test$site_id)
test$visitor_location_country_id <- as.factor(test$visitor_location_country_id)
test$prop_country_id <- as.factor(test$prop_country_id)
test$prop_id <- as.factor(test$prop_id)
test$prop_brand_bool <- as.factor(test$prop_brand_bool)
test$promotion_flag <- as.factor(test$promotion_flag)
test$srch_destination_id <- as.factor(test$srch_destination_id)
test$srch_saturday_night_bool <- as.factor(test$srch_saturday_night_bool)
test$random_bool <- as.factor(test$random_bool)

# test$site_id <- as.numeric(test$site_id)
# test$visitor_location_country_id <- as.numeric(test$visitor_location_country_id)
# test$prop_country_id <- as.numeric(test$prop_country_id)
# test$prop_id <- as.numeric(test$prop_id)
# test$prop_brand_bool <- as.numeric(test$prop_brand_bool)
# test$promotion_flag <- as.numeric(test$promotion_flag)
# test$srch_destination_id <- as.numeric(test$srch_destination_id)
# test$srch_saturday_night_bool <- as.numeric(test$srch_saturday_night_bool)
# test$random_bool <- as.numeric(test$random_bool)

## Turn the date variables into POSIX class
test$date_time <- as.POSIXct(test$date_time)

## Turn all numeric variables to numeric class
test$srch_id <- as.numeric(test$srch_id)
test$visitor_hist_starrating <- as.numeric(test$visitor_hist_starrating)
test$visitor_hist_adr_usd <- as.numeric(test$visitor_hist_adr_usd)
test$prop_review_score <- as.numeric(test$prop_review_score)
test$prop_location_score1 <- as.numeric(test$prop_location_score1)
test$prop_location_score2 <- as.numeric(test$prop_location_score2)
test$prop_log_historical_price <- as.numeric(test$prop_log_historical_price)
test$price_usd <- as.numeric(test$price_usd)
test$srch_length_of_stay <- as.numeric(test$srch_length_of_stay)
test$srch_booking_window <- as.numeric(test$srch_booking_window)
test$srch_adults_count <- as.numeric(test$srch_adults_count)
test$srch_children_count <- as.numeric(test$srch_children_count)
test$srch_query_affinity_score <- as.numeric(test$srch_query_affinity_score)
test$orig_destination_distance <- as.numeric(test$orig_destination_distance)

## Remove missing values from the competitor comparison variables and turn into
## factor classes
test$comp1_rate[test$comp1_rate == "NULL"] <- 0
test$comp1_inv[test$comp1_inv == "NULL"] <- 0
test$comp1_rate_percent_diff[test$comp1_rate_percent_diff == "NULL"] <- 0

test$comp2_rate[test$comp2_rate == "NULL"] <- 0
test$comp2_inv[test$comp2_inv == "NULL"] <- 0
test$comp2_rate_percent_diff[test$comp2_rate_percent_diff == "NULL"] <- 0

test$comp3_rate[test$comp3_rate == "NULL"] <- 0
test$comp3_inv[test$comp3_inv == "NULL"] <- 0
test$comp3_rate_percent_diff[test$comp3_rate_percent_diff == "NULL"] <- 0

test$comp4_rate[test$comp4_rate == "NULL"] <- 0
test$comp4_inv[test$comp4_inv == "NULL"] <- 0
test$comp4_rate_percent_diff[test$comp4_rate_percent_diff == "NULL"] <- 0

test$comp5_rate[test$comp5_rate == "NULL"] <- 0
test$comp5_inv[test$comp5_inv == "NULL"] <- 0
test$comp5_rate_percent_diff[test$comp5_rate_percent_diff == "NULL"] <- 0

test$comp6_rate[test$comp6_rate == "NULL"] <- 0
test$comp6_inv[test$comp6_inv == "NULL"] <- 0
test$comp6_rate_percent_diff[test$comp6_rate_percent_diff == "NULL"] <- 0

test$comp7_rate[test$comp7_rate == "NULL"] <- 0
test$comp7_inv[test$comp7_inv == "NULL"] <- 0
test$comp7_rate_percent_diff[test$comp7_rate_percent_diff == "NULL"] <- 0

test$comp8_rate[test$comp8_rate == "NULL"] <- 0
test$comp8_inv[test$comp8_inv == "NULL"] <- 0
test$comp8_rate_percent_diff[test$comp8_rate_percent_diff == "NULL"] <- 0

test$comp1_inv <- as.factor(test$comp1_inv)
test$comp2_inv <- as.factor(test$comp2_inv)
test$comp3_inv <- as.factor(test$comp3_inv)
test$comp4_inv <- as.factor(test$comp4_inv)
test$comp5_inv <- as.factor(test$comp5_inv)
test$comp6_inv <- as.factor(test$comp6_inv)
test$comp7_inv <- as.factor(test$comp7_inv)
test$comp8_inv <- as.factor(test$comp8_inv)

test$comp1_rate <- as.factor(test$comp1_rate)
test$comp2_rate <- as.factor(test$comp2_rate)
test$comp3_rate <- as.factor(test$comp3_rate)
test$comp4_rate <- as.factor(test$comp4_rate)
test$comp5_rate <- as.factor(test$comp5_rate)
test$comp6_rate <- as.factor(test$comp6_rate)
test$comp7_rate <- as.factor(test$comp7_rate)
test$comp8_rate <- as.factor(test$comp8_rate)

# test$comp1_inv <- as.numeric(test$comp1_inv)
# test$comp2_inv <- as.numeric(test$comp2_inv)
# test$comp3_inv <- as.numeric(test$comp3_inv)
# test$comp4_inv <- as.numeric(test$comp4_inv)
# test$comp5_inv <- as.numeric(test$comp5_inv)
# test$comp6_inv <- as.numeric(test$comp6_inv)
# test$comp7_inv <- as.numeric(test$comp7_inv)
# test$comp8_inv <- as.numeric(test$comp8_inv)
# 
# test$comp1_rate <- as.numeric(test$comp1_rate)
# test$comp2_rate <- as.numeric(test$comp2_rate)
# test$comp3_rate <- as.numeric(test$comp3_rate)
# test$comp4_rate <- as.numeric(test$comp4_rate)
# test$comp5_rate <- as.numeric(test$comp5_rate)
# test$comp6_rate <- as.numeric(test$comp6_rate)
# test$comp7_rate <- as.numeric(test$comp7_rate)
# test$comp8_rate <- as.numeric(test$comp8_rate)

test$comp1_rate_percent_diff <- as.numeric(test$comp1_rate_percent_diff)
test$comp2_rate_percent_diff <- as.numeric(test$comp2_rate_percent_diff)
test$comp3_rate_percent_diff <- as.numeric(test$comp3_rate_percent_diff)
test$comp4_rate_percent_diff <- as.numeric(test$comp4_rate_percent_diff)
test$comp5_rate_percent_diff <- as.numeric(test$comp5_rate_percent_diff)
test$comp6_rate_percent_diff <- as.numeric(test$comp6_rate_percent_diff)
test$comp7_rate_percent_diff <- as.numeric(test$comp7_rate_percent_diff)
test$comp8_rate_percent_diff <- as.numeric(test$comp8_rate_percent_diff)


## Get the aggregated property location score

test$prop_location_score1[is.na(test$prop_location_score1)] <- 0
test$prop_location_score2[is.na(test$prop_location_score2)] <- 0
test$prop_location_score_aggregate <- test$prop_location_score1 + test$prop_location_score2

## Remove missing values from the property review score
test$prop_review_score[is.na(test$prop_review_score)] <- 0

## Get a testing subset of the data by selecting the users with IDs which modulo 
## 4 is equal to 0.

# test_subset <- test[test$srch_id %% 4 == 0,]
# test_subset <- test[test$srch_id %% 4 != 0,]

test[is.na(test)] <- 0

## Get the mean historic price
test$prop_historical_price <- exp(test$prop_log_historical_price)

## Calculate the difference between historic price and current price
test$prop_delta_price <- test$price_usd - test$prop_historical_price

## Get the price order within the same user search
test <- test[order(test$srch_id, test$price_usd),]
test$price_order <- ave(test$price_usd, test$srch_id, FUN = seq_along)

## Get the target date for reservation
test$target_date <- test$date_time + (((test$srch_booking_window * 60) * 60) * 24)
test$target_month <- months(test$target_date)
test$target_month_num <- as.factor(match(test$target_month, month.name))
