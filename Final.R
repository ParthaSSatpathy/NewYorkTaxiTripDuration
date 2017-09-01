library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#Sys.setenv("plotly_username"="pssatpathy")
#Sys.setenv("plotly_api_key"="ljLTDoxWUdnoO7GTk67w")
setwd("C:/Users/parth/Desktop/Git/Projects/NewYorkTaxiTripDuration")
train_df <- read.csv("C:/Users/parth/Desktop/Git/Projects/NewYorkTaxiTripDuration/train.csv",header = T)
test_df <- read.csv("C:/Users/parth/Desktop/Git/Projects/NewYorkTaxiTripDuration/test.csv",header=T)

head(train_df)

str(train_df)

summary(train_df)

head(test_df)

sum(is.na(test_df)) #0
#test_df$trip_duration <- NULL
#combine_df <- rbind(train_df,test_df)

train_df %>%
  group_by(vendor_id) %>%
  summarise(count=n()/1000) %>%
  ggplot(aes(x=vendor_id,y=count)) +
  geom_bar(stat = 'identity',aes(fill=vendor_id)) +
  geom_text(stat = 'identity',aes(label=count),vjust=-1,fontface = "bold") +
  ylab('No of records in Thousands') 

library(lubridate)
train_df$pickup_day <- date(train_df$pickup_datetime)
train_df$pickup_hour <- hour(train_df$pickup_datetime)
#train_df$pickup_week <- week(train_df$pickup_datetime)
train_df$pickup_month <- month(train_df$pickup_datetime)

train_df %>%
  group_by(pickup_month) %>%
  summarise(count=n()/1000) %>%
  ggplot(aes(x=pickup_month,y=count)) +
  geom_bar(stat = 'identity',aes(fill=pickup_month,color='red')) +
  geom_text(stat = 'identity',aes(label=count),vjust=-1,fontface = "bold") +
  ylab('No of records in Thousands') +
  scale_fill_gradientn(colours = brewer.pal(4,'Set1'))

# People tend to take more number of trips starting from March. 
# Why though?- Weather change, More traffic, More work on job?

#train_df$pickup_hour <- train_df$pickup_hour
train_df$pickup_month <- factor(train_df$pickup_month)

train_df %>%
  group_by(pickup_month,pickup_hour) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=pickup_hour,y=count,color=pickup_month)) +
  geom_line(size=1) +
  #geom_text(stat = 'identity',aes(label=count),vjust=-1,fontface = "bold") +
  ylab('No of records')+
  scale_fill_gradientn(colours = brewer.pal(4,'Set2'))

# We can see that people mostly take cabs in evening after office hours (and during dinner time).
# After midnight number of rides are decreasing.

train_df %>%
  group_by(passenger_count) %>%
  summarise(count=round(n()),1) %>%
  ggplot(aes(x=passenger_count,y=count)) +
  geom_bar(stat = 'identity',aes(fill=passenger_count)) +
  geom_text(stat = 'identity',aes(label=count),vjust=-1,fontface = "bold") +
  ylab('No of records') +
  scale_fill_gradientn(colours = brewer.pal(4,'Set3'))
# People mostly travel alone. There are few cases where no of travellers like 7,8,9 with 
# values 3,1,1. They seem to be outliers. We will consider them as 6. 
# Also there are 60 cases of taxi with 0 passengers. 
# Does that mean the car went for maintainance? For now we will consider them as 1.

train_df$passenger_count[train_df$passenger_count > 6] <- 6
train_df$passenger_count[train_df$passenger_count == 0] <- 1

require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')

library(rMaps)
map <- Leaflet$new()
map$setView(c(40.75042, -73.98928), zoom = 12)
map$tileLayer(provider = 'Stamen.Watercolor')
for(i in 1:100){
  map$marker(
    c(train_df$pickup_latitude[i], train_df$pickup_longitude[i])
    #bindPopup = 'Hi. I am a popup'
  )
}

map
``
library(plotrix)
temp <-
  train_df %>%
  group_by(store_and_fwd_flag) %>%
  summarise(count=n(),1) 
slices <- temp$count
labels <- temp$store_and_fwd_flag
pie3D(slices,labels = labels, theta = 1.2, main = "Pie chart of Flag")


# Latitude and Longitude Plots
train_df %>%
  filter(pickup_longitude > -74.05 & pickup_longitude < -73.7) %>%
  ggplot(aes(pickup_longitude)) +
  geom_histogram(fill="red",bins = 40)

train_df %>%
  filter(dropoff_longitude > -74.05 & dropoff_longitude < -73.7) %>%
  ggplot(aes(dropoff_longitude)) +
  geom_histogram(fill = "blue", bins = 40)

train_df %>%
  filter(pickup_latitude > 40.6 & pickup_latitude < 40.9) %>%
  ggplot(aes(pickup_latitude)) +
  geom_histogram(fill = "red", bins = 40)

train_df %>%
  filter(dropoff_latitude > 40.6 & dropoff_latitude < 40.9) %>%
  ggplot(aes(dropoff_latitude)) +
  geom_histogram(fill = "blue", bins = 40)


library(plotly)

x <- log(train_df$trip_duration)
#x <- train_df$trip_duration
fit <- density(x)

plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
  add_trace(x = fit$x, y = fit$y, type = 'scatter',mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))
rm(x)
# ggplot(train_df,aes(x=trip_duration)) +
#   geom_histogram(fill = 'red',bins = 150) +
#   scale_x_log10() +
#   scale_y_sqrt() 
# 
# summary(train_df$trip_duration)

# Most of the rides corrspond to 16 minutes 
# The distribution is log-normal
# There are few outliers with outliers.
# There is a peak near to 10^5 seconds
# Let's explore the outliers
train_df %>%
  arrange(desc(trip_duration)) %>%
  filter(trip_duration > 85000)%>%
  ggplot(aes(x=trip_duration)) +
  geom_histogram(fill = 'blue')

# We will limit the trip duration to 90000

train_df$trip_duration[train_df$trip_duration > 90000] <- 90000

# Also there are few records fr which the duration is below 10 secnds- Too suspicious
# We will set the lower limit to 10

train_df$trip_duration[train_df$trip_duration < 11] <- 10

################################################################
# Let's incorporate the weather data
weather <- read.csv("weather_data_nyc_centralpark_2016.csv",header = T)
#weather <- as.tibble(fread("weather_data_nyc_centralpark_2016.csv"))

# We turn the date into a lubridate object and convert the traces (“T”) of rain and snow 
# into small numeric # amounts. 
# We also save the maximum and minimum temperature (beware the typo!) in a shorter form:

library(lubridate)
library(dplyr)
#library(tibble)
#library(data.table)
colnames(weather)[1] <- "pickup_day"
weather <- weather %>%
  mutate(pickup_day = dmy(pickup_day),
         rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
         s_fall = as.numeric(ifelse(snow.fall == "T", "0.01", snow.fall)),
         s_depth = as.numeric(ifelse(snow.depth  == "T", "0.01", snow.depth)),
         all_precip = s_fall + rain,
         has_snow = (s_fall > 0) | (s_depth > 0),
         has_rain = rain > 0,
         max_temp = maximum.temerature,
         min_temp = minimum.temperature)

foo <- weather %>%
  select(pickup_day, rain, s_fall, all_precip, has_snow, has_rain, s_depth, max_temp, min_temp)

train_df <- left_join(train_df, foo, by = "pickup_day")

train_df %>%
  group_by(pickup_day) %>%
  count() %>%
  ggplot(aes(pickup_day,n/1e3)) +
  geom_line(size = 1.5, color = "red") +
  labs(x = "", y = "Kilo trips per day")

train_df %>%
  group_by(pickup_day) %>%
  summarise(trips = n(),
            snow_fall = mean(s_fall),
            rain_fall = mean(rain),
            all_precip = mean(all_precip)) %>%
  ggplot(aes(pickup_day, snow_fall)) +
  geom_line(color = "blue", size = 1.5) +
  labs(x = "", y = "Snowfall") +
  scale_y_sqrt() +
  scale_x_date(limits = ymd(c("2015-12-28", "2016-06-30")))

train_df %>%
  group_by(pickup_day) %>%
  summarise(trips = n(),
            snow_depth = mean(s_depth)) %>%
  ggplot(aes(pickup_day, snow_depth)) +
  geom_line(color = "purple", size = 1.5) +
  labs(x = "", y = "Snow depth") +
  scale_y_sqrt() +
  scale_x_date(limits = ymd(c("2015-12-29", "2016-06-30")))

train_df %>%
  group_by(pickup_day, has_snow) %>%
  summarise(duration = mean(trip_duration),
            all_precip = mean(all_precip)) %>%
  ggplot(aes(all_precip, duration, color = has_snow)) +
  geom_jitter(width = 0.04, size = 2) +
  scale_x_sqrt() +
  scale_y_log10() +
  labs(x = "Amount of total precipitation", y = "Average trip duration")

############################################################
# Co-rrelation MAtrix
library(corrplot)
train_df %>%
  select(-id, -pickup_datetime, -dropoff_datetime, -pickup_day, -has_snow,-has_rain) %>%
  mutate(passenger_count = as.integer(passenger_count),
         vendor_id = as.integer(vendor_id),
         store_and_fwd_flag = as.integer(as.factor(store_and_fwd_flag)),
         pickup_month = as.integer(pickup_month))%>% #,
  #has_snow = as.integer(has_snow),
  #has_rain = as.integer(has_rain)) %>%
  select(trip_duration, everything()) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", method="circle", diag=FALSE)

#write.csv(train_df,"")
test_df$pickup_day <- date(test_df$pickup_datetime)
test_df$pickup_hour <- hour(test_df$pickup_datetime)
train_df$wday <- wday(train_df$pickup_datetime)
test_df$wday <- wday(test_df$pickup_datetime)
test_df <- left_join(test_df, foo, by = "pickup_day")

train_final <- 
  train_df %>%
  select(passenger_count,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,
         pickup_hour,all_precip,s_depth,max_temp,min_temp,wday)
test_final <-
  test_df %>%
  select(passenger_count,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,
         pickup_hour,all_precip,s_depth,max_temp,min_temp,wday)

write.csv(train_final,"train_final.csv",row.names = F)
write.csv(test_final,"test_final.csv",row.names = F)

x <- read.csv("train_final.csv")

y_train <- log(train_df$trip_duration + 1)

train_final <- cbind(train_final,y_train)

library(caret)
set.seed(4321)
trainIndex <- createDataPartition(train_final$y_train, p = 0.8, list = FALSE, times = 1)

train_final <- train_final[trainIndex,]
valid <- train_final[-trainIndex,]

library(xgboost)

foo <- train_final %>% select(-y_train)
bar <- valid %>% select(-y_train)

dtrain <- xgb.DMatrix(as.matrix(foo),label = train_final$y_train)
dvalid <- xgb.DMatrix(as.matrix(bar),label = valid$y_train)
dtest <- xgb.DMatrix(as.matrix(test_final))

xgb_params <- list(colsample_bytree = 0.7, #variables per tree 
                   subsample = 0.7, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 5, #tree levels
                   eta = 0.3, #shrinkage
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   seed = 4321
)

watchlist <- list(train=dtrain, valid=dvalid)
set.seed(4321)
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   print_every_n = 5,
                   watchlist = watchlist,
                   nrounds = 60)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 5, nrounds=20)

imp_matrix <- data.frame(xgb.importance(feature_names = colnames(train_final %>% select(-y_train)), model = gb_dt))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")

test_preds <- predict(gb_dt,dtest)
test_id <- read.csv("sample_submission.csv",header = T)
pred <- test_id %>%
  mutate(trip_duration = exp(test_preds) - 1)

pred %>% write.csv('submit.csv',row.names = F)
