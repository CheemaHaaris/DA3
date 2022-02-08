
############################################################################################################################

library(tidyverse)
library(ggplot2)
library(modelsummary)
library(fixest)
library(caret)
library(gridExtra)
library(data.table)
library(ranger)
library(pdp)
library(gbm)
library(rattle)

############################################################################################################################

#### Importing the data ####

df <- fread('listings.csv.gz')


# Subsetting data for number of guests

df <- df %>% filter( accommodates >= 2 & accommodates <=6)


#### Factored Variables ####

# Property Type 

check <- data.table(table(df$property_type))

check <- check[order(-N)]

check

# justify

df <- df %>% filter(property_type %in% c("Entire rental unit", "Private room in rental unit",
                                         "Entire residential home", "Private room in residential home",
                                         "Entire loft", "Entire condominium (condo)", 
                                         "Entire serviced apartment"))

df <- df %>% mutate( f_property_type = factor(property_type) )

# neighbourhood_cleansed

df <- df %>% mutate( f_neighbourhood_cleansed = factor(neighbourhood_cleansed) )

# room type

table(df$room_type)  

df <- df %>% mutate(f_room_type = factor(room_type))

############################################################################################################################

#### Numeric Variables ####


# bathrooms - extracting from text var

df <- df %>% mutate(
                     bathroom = factor(gsub( " .*$", "", df$bathrooms_text)))

df <- df %>% filter( bathroom != "Half-bath" & bathroom != "Private")

df$bathroom <- as.numeric(df$bathroom)


# add new numeric columns from certain columns

numericals <- c("accommodates","bathroom","bedrooms",
                "beds","number_of_reviews","review_scores_rating", "review_scores_accuracy",
                "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication",
                "review_scores_location", "review_scores_value","reviews_per_month","minimum_nights")

df <- df %>%
        mutate_at(vars(all_of(numericals)), lst("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)

#create days since first review

df <- df %>%
  mutate(
          n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                      as.Date(first_review ,format="%Y-%m-%d")))

############################################################################################################################

#### Binary Variables ####

# binaries # has_availability, instant_bookable

df <- df %>% mutate(
                    d_instant_bookable = ifelse(instant_bookable == 't', 1, 0),
                    d_superhost = ifelse(host_is_superhost == 't',1,0))


# binaries 

# identifying the relevant amenities

# extracting the amenities into a column using regex, string splitting and unlisting

amenities <- strsplit(df$amenities,',') %>% unlist() 

amenities <- gsub("[^a-zA-Z]", "", amenities)

amenities <- data.table(unique(amenities))

amenities <- amenities[order(unique(amenities))]

df$amenities <- strsplit(df$amenities, ',')
df$amenities <- tolower(gsub("[^a-zA-Z]", "", df$amenities))

# Testing proportions of relevant amenities

#table(grepl("oven|stove", df$amenities))
#table(grepl("apple tv|netflix|chromecast", df$amenities))
#table(grepl("centralheating|centralairconditioning", df$amenities)) 
#table(grepl("clothing", df$amenities))
#table(grepl("workspace|desk", df$amenities))
#table(grepl("wifi", df$amenities)) 
#table(grepl("kitchen", df$amenities)) 
#table(grepl("pool|hottub", df$amenities))
#table(grepl("gym", df$amenities)) 
#table(grepl("waterfront", df$amenities))
#table(grepl("refrigerator", df$amenities))
#table(grepl("beachfront", df$amenities))
#table(grepl("game", df$amenities))

amenity <- list( 
                oven_stove = "oven|stove",
                streaming = "apple tv|netflix|chromecast",
                central_conditioning = "centralheating|centralairconditioning",
                desk = "workspace|desk",
                wifi = "wifi",
                kitchen = "kitchen",
                pool = "pool|hottub",
                gym = "gym",
                waterfront = "waterfront|beachfront",
                refrigerator = "refrigerator",
                game = "game" )
 
for(i in names(amenity)) df[[i]] <- ifelse(grepl(amenity[[i]], df$amenities)==TRUE,1,0)

# renaming dummy vars



dummies <- names(df)[seq(96,106)]
df <- df %>%
            mutate_at(vars(all_of(dummies)), list("d"= as.numeric))


dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


############################################################################################################################

#### Price (Target Variable) ####

df <- df %>% mutate( eur_price_day = as.numeric(str_sub(price, 2)))

############################################################################################################################

#### Selecting the relevant variables ####


# keep columns if contain d_, n_,f_, eur_ 
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^eur_.*"))

############################################################################################################################

# target var

P95 <- function(x){ quantile(x,.95,na.rm=T)}

datasummary( eur_price_day ~ Mean + Median + Min + Max + P25 + P75 + P95 , data = df )

df <- df %>%
            mutate(ln_price = log(eur_price_day))

# numerics

summary(df)

# Squares and further values to create

df <- df %>%
            mutate(n_accommodates2=n_accommodates^2, 
                   ln_accommodates=log(n_accommodates) ,
                   ln_accommodates2=log(n_accommodates)^2,
                   ln_beds = log(n_beds),
                   ln_bedrooms = log(n_bedrooms),
                   ln_number_of_reviews = log(n_number_of_reviews + 1),
                   ln_reviews_per_month = log(n_reviews_per_month + 1),
            )


# discrete numeric vars

# checking distributions and applying cutoffs to convert to factored var
#datasummary(factor(n_minimum_nights) ~ N , data = df  )

df <- df %>%
  mutate( f_beds = cut(n_beds, c(1,2,3,4,12), labels=c(1,2,3,4), right = F),
          f_bedrooms = cut(n_bedrooms, c(1,2,3,10), labels=c(1,2,3), right = F),
          f_number_of_reviews = cut(n_number_of_reviews, c(0,10,50,733), labels=c(0,1,2), right = F),
          f_minimum_nights= cut(n_minimum_nights, c(1,2,3,1124),
                                labels=c(1,2,3), right = F))


# factored vars

#datasummary(f_property_type ~ N , data = df  )

#datasummary(f_neighbourhood_cleansed ~ N , data = df  )

#datasummary(f_room_type ~ N , data = df  )


# Change Infinite values with NaNs
for (j in 1:ncol(df) ) data.table::set(df, which(is.infinite(df[[j]])), j, NA)

###############################################################################################################

# Checking missing values in vars

to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#Problem: same rows missing for review-based columns
#Option 1 - creating a flag and imputing,
#Option 2 - dropping rows to see if better prediction

# dropping rows with no price (target vars)

df <- df %>%
  drop_na(eur_price_day)

# imputing for variables that are not too important
df <- df %>%
  mutate(
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds),
    ln_beds = ifelse(is.na(ln_beds), log(n_accommodates), ln_beds),
    n_bedrooms =  ifelse(is.na(n_bedrooms), median(n_bedrooms, na.rm = T), n_bedrooms), 
    ln_bedrooms =  ifelse(is.na(ln_bedrooms), log(median(n_bedrooms, na.rm = T)), ln_bedrooms), 
    f_beds=ifelse(is.na(f_beds),1, f_beds),
    f_bedrooms=ifelse(is.na(f_bedrooms),1, f_bedrooms),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    f_minimum_nights = ifelse(is.na(f_minimum_nights),1,f_minimum_nights))


# filtering for option 2
df_new <- df %>%
  drop_na(n_review_scores_checkin)

check2 <- df %>% filter(is.na(n_review_scores_checkin))

to_filter <- sapply(check2, function(x) sum(is.na(x)))
to_filter[to_filter > 0]  

# all missing values in df are present in the rows containing missing for n_review_scores_checkin


############################################################################################################
# Option 1

df <- df %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    ln_reviews_per_month =  ifelse(is.na(ln_reviews_per_month), log(median(n_reviews_per_month, na.rm = T)), ln_reviews_per_month),
    flag_review_scores_rating = ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_combined = ifelse(is.na(n_review_scores_checkin),1, 0),
    n_review_scores_accuracy = ifelse(is.na(n_review_scores_accuracy), median(n_review_scores_accuracy, na.rm = T), n_review_scores_accuracy),
    n_review_scores_cleanliness = ifelse(is.na(n_review_scores_cleanliness), median(n_review_scores_cleanliness, na.rm = T), n_review_scores_cleanliness),
    n_review_scores_checkin = ifelse(is.na(n_review_scores_checkin), median(n_review_scores_checkin, na.rm = T), n_review_scores_checkin),
    n_review_scores_communication = ifelse(is.na(n_review_scores_communication), median(n_review_scores_communication, na.rm = T), n_review_scores_communication),
    n_review_scores_location = ifelse(is.na(n_review_scores_location), median(n_review_scores_location, na.rm = T), n_review_scores_location),
    n_review_scores_value = ifelse(is.na(n_review_scores_value), median(n_review_scores_value, na.rm = T), n_review_scores_value)
                                   )

to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]   



#######################################################################################################


# redo features
# Create variables, measuring the time since: squared, cubic, logs
df <- df %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=-n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_review_scores_accuracy = log(n_review_scores_accuracy),
    ln_review_scores_cleanliness = log(n_review_scores_cleanliness),
    ln_review_scores_checkin = log(n_review_scores_checkin),
    ln_review_scores_communication = log(n_review_scores_communication),
    ln_review_scores_location = log(n_review_scores_location),
    ln_review_scores_value = log(n_review_scores_value),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )



####################################################################################################################

# Option 2 - checking missing

to_filter <- sapply(df_new, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# redo features
# Create variables, measuring the time since: squared, cubic, logs
df_new <- df_new %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=-n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_review_scores_accuracy = log(n_review_scores_accuracy),
    ln_review_scores_cleanliness = log(n_review_scores_cleanliness),
    ln_review_scores_checkin = log(n_review_scores_checkin),
    ln_review_scores_communication = log(n_review_scores_communication),
    ln_review_scores_location = log(n_review_scores_location),
    ln_review_scores_value = log(n_review_scores_value),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )


########################################################################################################################################################################

# copy a variable - purpose later, see at variable importance
df <- df %>% mutate(n_accommodates_copy = n_accommodates)
df_new <- df_new %>% mutate(n_accommodates_copy = n_accommodates)

