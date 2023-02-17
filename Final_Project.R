### Project starting

library(readr)
library(dplyr)

setwd("/Users/tanya/Desktop/Универ/KSE магистратура/Term III/Data Visualization")
df <- read.csv("air.csv")
View(df)
df$Price <- as.numeric(df$Price)

# So, in general:
# 1. House properties: (type of property, bedrooms, bathrooms, accomodates, beds, size etc)
# 2. Reviews on the listing: (all types of reviews + number of reviews)
# 3. Host: their profile, responsiveness etc (host response rating, how long this person is the host, host verification type, name of the host, if profile picture included etc)
# 4. Additional services: Amenities, cleaning fee, extra people price, minimum nights, cancelation policy
# 5. Location + neighborhood description
# 6. Text analysis of its text description + name

### 3. HOST PROFILE ANALYSIS

## 3.1. Checking if hosts with profile pictures have higher prices
price_without_profile_picture <- df %>%
  filter(startsWith(Host.Picture.Url, "https://a0.muscache.com/im/pictures/") == TRUE) %>% 
  summarise(mean_without_profile_picture = mean(Price))
price_without_profile_picture
# When the host doesn't have a profile picture the average price of the listing is 132.9837
price_with_profile_picture <- df %>%
  filter(startsWith(Host.Picture.Url, "https://a0.muscache.com/im/pictures/") == FALSE) %>% 
  summarise(mean_with_profile_picture = mean(Price))
price_with_profile_picture
# When the host does have a profile picture the average price of the listing is 145.1476
145.1476 - 132.9837
#So, hosts with profile pictures on average have $12.1639 higher prices.


## 3.2. Checking if experienced hosts have higher prices
df$Host.Since <- as.Date(df$Host.Since)
min(df$Host.Since)
max(df$Host.Since)

df$hosting_time <- 

