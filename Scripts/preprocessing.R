library(plyr)
library(dplyr)

## Load the data
train.clean <- read.csv("data/training_set_VU_DM_2014.csv", stringsAsFactors=FALSE)

## Turn the date variables into POSIX class
train.clean$date_time <- as.POSIXct(train.clean$date_time)

## Turn all numeric variables to numeric class
train.clean$srch_id <- as.numeric(train.clean$srch_id)
train.clean$visitor_hist_starrating <- as.numeric(train.clean$visitor_hist_starrating)
train.clean$visitor_hist_adr_usd <- as.numeric(train.clean$visitor_hist_adr_usd)
train.clean$prop_review_score <- as.numeric(train.clean$prop_review_score)
train.clean$prop_location_score1 <- as.numeric(train.clean$prop_location_score1)
train.clean$prop_location_score2 <- as.numeric(train.clean$prop_location_score2)
train.clean$prop_log_historical_price <- as.numeric(train.clean$prop_log_historical_price)
train.clean$price_usd <- as.numeric(train.clean$price_usd)
train.clean$srch_length_of_stay <- as.numeric(train.clean$srch_length_of_stay)
train.clean$srch_booking_window <- as.numeric(train.clean$srch_booking_window)
train.clean$srch_adults_count <- as.numeric(train.clean$srch_adults_count)
train.clean$srch_children_count <- as.numeric(train.clean$srch_children_count)
train.clean$srch_query_affinity_score <- as.numeric(train.clean$srch_query_affinity_score)
train.clean$orig_destination_distance <- as.numeric(train.clean$orig_destination_distance)
train.clean$gross_bookings_usd <- as.numeric(train.clean$gross_bookings_usd)
train.clean$position <- as.numeric(train.clean$position)
train.clean$site_id <- as.numeric(train.clean$site_id)
train.clean$visitor_location_country_id <- as.numeric(train.clean$visitor_location_country_id)
train.clean$prop_country_id <- as.numeric(train.clean$prop_country_id)
train.clean$prop_id <- as.numeric(train.clean$prop_id)
train.clean$prop_brand_bool <- as.numeric(train.clean$prop_brand_bool)
train.clean$promotion_flag <- as.numeric(train.clean$promotion_flag)
train.clean$srch_destination_id <- as.numeric(train.clean$srch_destination_id)
train.clean$srch_saturday_night_bool <- as.numeric(train.clean$srch_saturday_night_bool)
train.clean$random_bool <- as.numeric(train.clean$random_bool)
train.clean$click_bool <- as.numeric(train.clean$click_bool)
train.clean$booking_bool <- as.numeric(train.clean$booking_bool)


## Remove missing values from the competitor comparison variables and turn into
## factor classes
train.clean$comp1_rate[train.clean$comp1_rate == "NULL"] <- 0
train.clean$comp1_inv[train.clean$comp1_inv == "NULL"] <- 0
train.clean$comp1_rate_percent_diff[train.clean$comp1_rate_percent_diff == "NULL"] <- 0

train.clean$comp2_rate[train.clean$comp2_rate == "NULL"] <- 0
train.clean$comp2_inv[train.clean$comp2_inv == "NULL"] <- 0
train.clean$comp2_rate_percent_diff[train.clean$comp2_rate_percent_diff == "NULL"] <- 0

train.clean$comp3_rate[train.clean$comp3_rate == "NULL"] <- 0
train.clean$comp3_inv[train.clean$comp3_inv == "NULL"] <- 0
train.clean$comp3_rate_percent_diff[train.clean$comp3_rate_percent_diff == "NULL"] <- 0

train.clean$comp4_rate[train.clean$comp4_rate == "NULL"] <- 0
train.clean$comp4_inv[train.clean$comp4_inv == "NULL"] <- 0
train.clean$comp4_rate_percent_diff[train.clean$comp4_rate_percent_diff == "NULL"] <- 0

train.clean$comp5_rate[train.clean$comp5_rate == "NULL"] <- 0
train.clean$comp5_inv[train.clean$comp5_inv == "NULL"] <- 0
train.clean$comp5_rate_percent_diff[train.clean$comp5_rate_percent_diff == "NULL"] <- 0

train.clean$comp6_rate[train.clean$comp6_rate == "NULL"] <- 0
train.clean$comp6_inv[train.clean$comp6_inv == "NULL"] <- 0
train.clean$comp6_rate_percent_diff[train.clean$comp6_rate_percent_diff == "NULL"] <- 0

train.clean$comp7_rate[train.clean$comp7_rate == "NULL"] <- 0
train.clean$comp7_inv[train.clean$comp7_inv == "NULL"] <- 0
train.clean$comp7_rate_percent_diff[train.clean$comp7_rate_percent_diff == "NULL"] <- 0

train.clean$comp8_rate[train.clean$comp8_rate == "NULL"] <- 0
train.clean$comp8_inv[train.clean$comp8_inv == "NULL"] <- 0
train.clean$comp8_rate_percent_diff[train.clean$comp8_rate_percent_diff == "NULL"] <- 0

train.clean$comp1_inv <- as.numeric(train.clean$comp1_inv)
train.clean$comp2_inv <- as.numeric(train.clean$comp2_inv)
train.clean$comp3_inv <- as.numeric(train.clean$comp3_inv)
train.clean$comp4_inv <- as.numeric(train.clean$comp4_inv)
train.clean$comp5_inv <- as.numeric(train.clean$comp5_inv)
train.clean$comp6_inv <- as.numeric(train.clean$comp6_inv)
train.clean$comp7_inv <- as.numeric(train.clean$comp7_inv)
train.clean$comp8_inv <- as.numeric(train.clean$comp8_inv)

train.clean$comp1_rate <- as.numeric(train.clean$comp1_rate)
train.clean$comp2_rate <- as.numeric(train.clean$comp2_rate)
train.clean$comp3_rate <- as.numeric(train.clean$comp3_rate)
train.clean$comp4_rate <- as.numeric(train.clean$comp4_rate)
train.clean$comp5_rate <- as.numeric(train.clean$comp5_rate)
train.clean$comp6_rate <- as.numeric(train.clean$comp6_rate)
train.clean$comp7_rate <- as.numeric(train.clean$comp7_rate)
train.clean$comp8_rate <- as.numeric(train.clean$comp8_rate)

train.clean$comp1_rate_percent_diff <- as.numeric(train.clean$comp1_rate_percent_diff)
train.clean$comp2_rate_percent_diff <- as.numeric(train.clean$comp2_rate_percent_diff)
train.clean$comp3_rate_percent_diff <- as.numeric(train.clean$comp3_rate_percent_diff)
train.clean$comp4_rate_percent_diff <- as.numeric(train.clean$comp4_rate_percent_diff)
train.clean$comp5_rate_percent_diff <- as.numeric(train.clean$comp5_rate_percent_diff)
train.clean$comp6_rate_percent_diff <- as.numeric(train.clean$comp6_rate_percent_diff)
train.clean$comp7_rate_percent_diff <- as.numeric(train.clean$comp7_rate_percent_diff)
train.clean$comp8_rate_percent_diff <- as.numeric(train.clean$comp8_rate_percent_diff)


## Get the aggregated property location score

train.clean$prop_location_score1[is.na(train.clean$prop_location_score1)] <- 0
train.clean$prop_location_score2[is.na(train.clean$prop_location_score2)] <- 0
train.clean$prop_location_score_aggregate <- train.clean$prop_location_score1 + train.clean$prop_location_score2

## Remove missing values from the property review score
train.clean$prop_review_score[is.na(train.clean$prop_review_score)] <- 0

train.clean[is.na(train.clean)] <- 0

## Get the mean historic price
train.clean$prop_historical_price <- exp(train.clean$prop_log_historical_price)

## Calculate the difference between historic price and current price
train.clean$prop_delta_price <- train.clean$price_usd - train.clean$prop_historical_price

## Get the price order within the same user search
train.clean <- train.clean[order(train.clean$srch_id, train.clean$price_usd),]
train.clean$price_order <- ave(train.clean$price_usd, train.clean$srch_id, FUN = seq_along)

## Get the target date for reservation
train.clean$target_date <- train.clean$date_time + (((train.clean$srch_booking_window * 60) * 60) * 24)
train.clean$target_month <- months(train.clean$target_date)
train.clean$target_month_num <- as.factor(match(train.clean$target_month, month.name))
