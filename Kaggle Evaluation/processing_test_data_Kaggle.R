library(plyr)
library(dplyr)

## Load the data
test.sample <- read.csv("./data/Kaggke data/test.csv", stringsAsFactors=FALSE)

## Turn the date variables into POSIX class
test.sample$date_time <- as.POSIXct(test.sample$date_time)

test.sample$site_id <- as.numeric(test.sample$site_id)
test.sample$visitor_location_country_id <- as.numeric(test.sample$visitor_location_country_id)
test.sample$prop_country_id <- as.numeric(test.sample$prop_country_id)
test.sample$prop_id <- as.numeric(test.sample$prop_id)
test.sample$prop_brand_bool <- as.numeric(test.sample$prop_brand_bool)
test.sample$promotion_flag <- as.numeric(test.sample$promotion_flag)
test.sample$srch_destination_id <- as.numeric(test.sample$srch_destination_id)
test.sample$srch_saturday_night_bool <- as.numeric(test.sample$srch_saturday_night_bool)
test.sample$random_bool <- as.numeric(test.sample$random_bool)

## Turn all numeric variables to numeric class
test.sample$srch_id <- as.numeric(test.sample$srch_id)
test.sample$visitor_hist_starrating <- as.numeric(test.sample$visitor_hist_starrating)
test.sample$visitor_hist_adr_usd <- as.numeric(test.sample$visitor_hist_adr_usd)
test.sample$prop_review_score <- as.numeric(test.sample$prop_review_score)
test.sample$prop_location_score1 <- as.numeric(test.sample$prop_location_score1)
test.sample$prop_location_score2 <- as.numeric(test.sample$prop_location_score2)
test.sample$prop_log_historical_price <- as.numeric(test.sample$prop_log_historical_price)
test.sample$price_usd <- as.numeric(test.sample$price_usd)
test.sample$srch_length_of_stay <- as.numeric(test.sample$srch_length_of_stay)
test.sample$srch_booking_window <- as.numeric(test.sample$srch_booking_window)
test.sample$srch_adults_count <- as.numeric(test.sample$srch_adults_count)
test.sample$srch_children_count <- as.numeric(test.sample$srch_children_count)
test.sample$srch_query_affinity_score <- as.numeric(test.sample$srch_query_affinity_score)
test.sample$orig_destination_distance <- as.numeric(test.sample$orig_destination_distance)

## Remove missing values from the competitor comparison variables and turn into
## factor classes
test.sample$comp1_rate[test.sample$comp1_rate == "NULL"] <- 0
test.sample$comp1_inv[test.sample$comp1_inv == "NULL"] <- 0
test.sample$comp1_rate_percent_diff[test.sample$comp1_rate_percent_diff == "NULL"] <- 0

test.sample$comp2_rate[test.sample$comp2_rate == "NULL"] <- 0
test.sample$comp2_inv[test.sample$comp2_inv == "NULL"] <- 0
test.sample$comp2_rate_percent_diff[test.sample$comp2_rate_percent_diff == "NULL"] <- 0

test.sample$comp3_rate[test.sample$comp3_rate == "NULL"] <- 0
test.sample$comp3_inv[test.sample$comp3_inv == "NULL"] <- 0
test.sample$comp3_rate_percent_diff[test.sample$comp3_rate_percent_diff == "NULL"] <- 0

test.sample$comp4_rate[test.sample$comp4_rate == "NULL"] <- 0
test.sample$comp4_inv[test.sample$comp4_inv == "NULL"] <- 0
test.sample$comp4_rate_percent_diff[test.sample$comp4_rate_percent_diff == "NULL"] <- 0

test.sample$comp5_rate[test.sample$comp5_rate == "NULL"] <- 0
test.sample$comp5_inv[test.sample$comp5_inv == "NULL"] <- 0
test.sample$comp5_rate_percent_diff[test.sample$comp5_rate_percent_diff == "NULL"] <- 0

test.sample$comp6_rate[test.sample$comp6_rate == "NULL"] <- 0
test.sample$comp6_inv[test.sample$comp6_inv == "NULL"] <- 0
test.sample$comp6_rate_percent_diff[test.sample$comp6_rate_percent_diff == "NULL"] <- 0

test.sample$comp7_rate[test.sample$comp7_rate == "NULL"] <- 0
test.sample$comp7_inv[test.sample$comp7_inv == "NULL"] <- 0
test.sample$comp7_rate_percent_diff[test.sample$comp7_rate_percent_diff == "NULL"] <- 0

test.sample$comp8_rate[test.sample$comp8_rate == "NULL"] <- 0
test.sample$comp8_inv[test.sample$comp8_inv == "NULL"] <- 0
test.sample$comp8_rate_percent_diff[test.sample$comp8_rate_percent_diff == "NULL"] <- 0

test.sample$comp1_inv <- as.numeric(test.sample$comp1_inv)
test.sample$comp2_inv <- as.numeric(test.sample$comp2_inv)
test.sample$comp3_inv <- as.numeric(test.sample$comp3_inv)
test.sample$comp4_inv <- as.numeric(test.sample$comp4_inv)
test.sample$comp5_inv <- as.numeric(test.sample$comp5_inv)
test.sample$comp6_inv <- as.numeric(test.sample$comp6_inv)
test.sample$comp7_inv <- as.numeric(test.sample$comp7_inv)
test.sample$comp8_inv <- as.numeric(test.sample$comp8_inv)

test.sample$comp1_rate <- as.numeric(test.sample$comp1_rate)
test.sample$comp2_rate <- as.numeric(test.sample$comp2_rate)
test.sample$comp3_rate <- as.numeric(test.sample$comp3_rate)
test.sample$comp4_rate <- as.numeric(test.sample$comp4_rate)
test.sample$comp5_rate <- as.numeric(test.sample$comp5_rate)
test.sample$comp6_rate <- as.numeric(test.sample$comp6_rate)
test.sample$comp7_rate <- as.numeric(test.sample$comp7_rate)
test.sample$comp8_rate <- as.numeric(test.sample$comp8_rate)

test.sample$comp1_rate_percent_diff <- as.numeric(test.sample$comp1_rate_percent_diff)
test.sample$comp2_rate_percent_diff <- as.numeric(test.sample$comp2_rate_percent_diff)
test.sample$comp3_rate_percent_diff <- as.numeric(test.sample$comp3_rate_percent_diff)
test.sample$comp4_rate_percent_diff <- as.numeric(test.sample$comp4_rate_percent_diff)
test.sample$comp5_rate_percent_diff <- as.numeric(test.sample$comp5_rate_percent_diff)
test.sample$comp6_rate_percent_diff <- as.numeric(test.sample$comp6_rate_percent_diff)
test.sample$comp7_rate_percent_diff <- as.numeric(test.sample$comp7_rate_percent_diff)
test.sample$comp8_rate_percent_diff <- as.numeric(test.sample$comp8_rate_percent_diff)


## Get the aggregated property location score

test.sample$prop_location_score1[is.na(test.sample$prop_location_score1)] <- 0
test.sample$prop_location_score2[is.na(test.sample$prop_location_score2)] <- 0
test.sample$prop_location_score_aggregate <- test.sample$prop_location_score1 + test.sample$prop_location_score2

## Remove missing values from the property review score
test.sample$prop_review_score[is.na(test.sample$prop_review_score)] <- 0

test.sample[is.na(test.sample)] <- 0

## Get the mean historic price
test.sample$prop_historical_price <- exp(test.sample$prop_log_historical_price)

## Calculate the difference between historic price and current price
test.sample$prop_delta_price <- test.sample$price_usd - test.sample$prop_historical_price

## Get the price order within the same user search
test.sample <- test.sample[order(test.sample$srch_id, test.sample$price_usd),]
test.sample$price_order <- ave(test.sample$price_usd, test.sample$srch_id, FUN = seq_along)

## Get the target date for reservation
test.sample$target_date <- test.sample$date_time + (((test.sample$srch_booking_window * 60) * 60) * 24)
test.sample$target_month <- months(test.sample$target_date)
test.sample$target_month_num <- as.factor(match(test.sample$target_month, month.name))
