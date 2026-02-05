# Clear workspace and console
rm(list = ls())
cat("\014")
set.seed(123)


library(pacman)
p_load(dplyr, psych)

# Load data
airbnb_raw <- read.csv("listings.csv", stringsAsFactors = TRUE)
airbnb <- airbnb_raw

str(airbnb)
summary(airbnb)


# Data Visualisation Before cleaning

png("raw_boxplot_price.png")
boxplot(airbnb_raw$price, main="Raw Price Boxplot", ylab="Price (£)", col="orange")
dev.off()


png("raw_hist_price.png", width = 1200, height = 800)

hist(airbnb_raw$price,
     breaks = 50,
     main = "Histogram of Raw Airbnb Prices",
     xlab = "Price (£)",
     col = "lightblue",
     border = "white")

dev.off()

png("raw_boxplot_roomtype_price.png", width=900, height=600)
boxplot(price ~ room_type, data=airbnb_raw, main="Raw Price by Room Type",
        xlab="Room Type", ylab="Price (£)", col=c("lightblue","lightgreen","pink"), las=2)
dev.off()

png("raw_boxplot_minimum_nights.png")
boxplot(airbnb_raw$minimum_nights, main="Raw Minimum Nights Distribution", col="purple")
dev.off()


png("raw_boxplot_number_reviews.png")
boxplot(airbnb_raw$number_of_reviews, main="Raw Number of Reviews Distribution",
        col="lightgreen")
dev.off()

png("raw_boxplot_neighbourhood_price.png", width=1400, height=800)
boxplot(price ~ neighbourhood, data=airbnb_raw, main="Raw Price by Neighbourhood",
        xlab="Neighbourhood", ylab="Price (£)", col="lightblue", las=2, outline=FALSE)
dev.off()

numeric_vars_raw <- c("price","minimum_nights","number_of_reviews",
                      "reviews_per_month","calculated_host_listings_count","availability_365")

png("raw_pairs_panels.png", width=1000, height=1000)
psych::pairs.panels(airbnb_raw[numeric_vars_raw], main="Raw Pairwise Relationships")
dev.off()

# Missing Values

airbnb$neighbourhood_group <- NULL

median_rpm <- median(airbnb$reviews_per_month, na.rm=TRUE)
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <- median_rpm

airbnb$last_review <- as.Date(airbnb$last_review)
airbnb$last_review[is.na(airbnb$last_review)] <- as.Date("2000-01-01")

# Data Cleaning

airbnb <- airbnb %>% filter(price > 0)
airbnb <- airbnb %>% filter(minimum_nights <= 60)

airbnb <- unique(airbnb)

# Remove extreme price outliers above £300
airbnb <- airbnb %>% filter(price <= 300)

png("boxplot_price_cleaned.png")
boxplot(airbnb$price, main="Price After Removing Outliers")
dev.off()

# Feature Engineering

max_date <- max(airbnb$last_review)
airbnb$days_since_review <- as.numeric(max_date - airbnb$last_review)
airbnb$has_review <- ifelse(airbnb$last_review == as.Date("2000-01-01"), 0, 1)

airbnb$log_price <- log(airbnb$price + 1)
airbnb$log_number_reviews <- log(airbnb$number_of_reviews + 1)
airbnb$log_host_listings <- log(airbnb$calculated_host_listings_count + 1)

airbnb$room_type <- factor(airbnb$room_type)
airbnb$neighbourhood <- factor(airbnb$neighbourhood)

# Price buckets (quantile-based)
airbnb$price_bucket_5 <- cut(
  airbnb$price,
  breaks = quantile(airbnb$price, probs = seq(0,1,0.20)),
  include.lowest = TRUE,
  labels = c("Very Low", "Low", "Medium", "High", "Very High")
)

# Display ranges
for (b in levels(airbnb$price_bucket_5)) {
  cat("\n", b, "bucket range:\n")
  print(summary(airbnb$price[airbnb$price_bucket_5 == b]))
}

# ---------------------------
# Visualisation (cleaned)
# ---------------------------

png("price_buckets_counts.png")
barplot(table(airbnb$price_bucket_5), col=c("skyblue","green","yellow","orange","red"),
        main="Count of Listings in Price Buckets", ylab="Number of Listings")
dev.off()

png("hist_price_cleaned.png")
hist(airbnb$price, breaks=50, main="Histogram of Cleaned Prices", xlab="Price (£)",
     col="skyblue", border="white")
dev.off()

png("hist_log_price.png", width = 1200, height = 800)
hist(airbnb$log_price,
     breaks = 40,
     main = "Log-Transformed Price Distribution",
     xlab = "log(Price)",
     col = "#8A2BE2",          # cleaner professional purple
     border = "white",
     cex.main = 1.8,
     cex.lab = 1.4,
     cex.axis = 1.3)

rug(airbnb$log_price, col = "black", alpha = 0.2)
dev.off()

png("boxplot_neighbourhood_log_price.png", width=1400, height=800)
boxplot(log_price ~ neighbourhood, data=airbnb, main="Log Price by Neighbourhood",
        xlab="Neighbourhood", ylab="Log Price", col="lightblue", las=2, outline=FALSE)
dev.off()

png("boxplot_roomtype_price_cleaned.png")
boxplot(price ~ room_type, data=airbnb, main="Price by Room Type",
        xlab="Room Type", ylab="Price (£)", col="orange")
dev.off()


numeric_vars <- c("price","minimum_nights","number_of_reviews",
                  "reviews_per_month","calculated_host_listings_count","availability_365")

png("pairspanels_cleaned.png", width=1000, height=1000)
psych::pairs.panels(airbnb[numeric_vars])
dev.off()

# Train Validation Test Split

airbnb <- airbnb[sample(1:nrow(airbnb)), ]

n <- nrow(airbnb)
train_size <- round(n * 0.60)
valid_size <- round(n * 0.20)
test_size  <- n - train_size - valid_size

train_airbnb <- airbnb[1:train_size, ]
valid_airbnb <- airbnb[(train_size + 1):(train_size + valid_size), ]
test_airbnb  <- airbnb[(train_size + valid_size + 1):n, ]

write.csv(airbnb,       "airbnb_cleaned_full.csv", row.names=FALSE)
write.csv(train_airbnb, "airbnb_train.csv",        row.names=FALSE)
write.csv(valid_airbnb, "airbnb_valid.csv",        row.names=FALSE)
write.csv(test_airbnb,  "airbnb_test.csv",         row.names=FALSE)

