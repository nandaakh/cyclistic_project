library(tidyverse)
library(lubridate)
library(dplyr)
library(geosphere)
library(data.table)

# import files
loc_file <- file.path("Documents/r_work/caseStudy_1", fsep = "/")
folder_file <- list.files(loc_file, full.names = TRUE, pattern = "*.csv")
moded_df <- map_df(folder_file, ~read_csv(.))

# DATA WRANGLING
# add some columns for further analysis
data_df <- moded_df %>%
  mutate(
    ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins")),
    ride_distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat)),
    ride_year = year(started_at),
    day_of_week = weekdays(started_at),
    hour_of_day = hour(started_at)
  )

# DATA CLEANING
# Filter rides not starting from a station, end at station, also eliminate data that has value "<= 0" for both duration and distance
cleanData_df <- data_df %>%
  filter(!is.na(start_station_name)) %>%
  filter(!is.na(end_station_name)) %>%
  filter(ride_duration > 0) %>%
  filter(ride_distance > 0)

# Remove if there any duplicate data
cleanData_df %>% distinct()
cyclistic_df <- cleanData_df #create "cyclistic_data" dataframe, for further analysis
sum(is.na(cyclistic_df)) # Taking moment if there is still missing data
summary(is.na(cyclistic_df))

# ANALYZE PHASE
# Identifying each membership cathegory
member_df <- cyclistic_df %>%
  select(member_casual) %>%
  group_by(member_casual) %>%
  count() %>%
  arrange()

# We have 1,677,058 casual rider and 2,587,853 annual member in this dataset
# plotting into pie chart
slices <- c(1677058, 2587853)
lbls <- c("casual", "members")
colors <- c("#99D1FF", "#393B8F")
pie(slices, labels = lbls, main = "Total Users", col = colors)
legend("bottomright", lbls, fill = colors)

# Identify which day has more customer on each cathegory
day_df <- cyclistic_df %>%
  select(day_of_week) %>%
  group_by(day_of_week) %>%
  count() %>%
  arrange()
head(day_df)

# Plotting into bar graph, using `facet_wrap` to see which day has higher user on each cathegory
ggplot(data = cyclistic_df) +
  geom_bar(mapping = aes(x=day_of_week, fill = day_of_week)) +
  facet_wrap(~member_casual) +
  labs(title = "Cyclstic Data per Day", x="Customer Type", y="Count")

# create month column to represent month on each dataset
data_months_df <- cyclistic_df %>% mutate(Months=
                                            case_when(
                                              month(cyclistic_df$started_at)==01 ~ "Jan",
                                              month(cyclistic_df$started_at)==02 ~ "Feb",
                                              month(cyclistic_df$started_at)==03 ~ "Mar",
                                              month(cyclistic_df$started_at)==04 ~ "Apr",
                                              month(cyclistic_df$started_at)==05 ~ "May",
                                              month(cyclistic_df$started_at)==06 ~ "Jun",
                                              month(cyclistic_df$started_at)==07 ~ "Jul",
                                              month(cyclistic_df$started_at)==08 ~ "Aug",
                                              month(cyclistic_df$started_at)==09 ~ "Sep",
                                              month(cyclistic_df$started_at)==10 ~ "Oct",
                                              month(cyclistic_df$started_at)==11 ~ "Nov",
                                              month(cyclistic_df$started_at)==12 ~ "Dec"
                                            ))

# Identify which month has more customer on each cathegory
month_df <- data_months_df %>%
  select(Months) %>%
  group_by(Months) %>%
  count() %>%
  arrange()
head(month_df)

# Plotting into bar graph, using `facet_wrap` to see which month has higher user on each cathegory
ggplot(data = data_months_df) +
  geom_bar(mapping = aes(x=Months, fill=Months)) +
  facet_wrap(~member_casual) +
  labs(title = "Cyclistic Data per Month", x="Customer Type", y="Count")

# Now identify the trends in 4 seasons from 12 months, by separating the 12 months into respective season
season_df <- data_months_df %>% mutate(Seasons =
                                                  case_when(
                                                    month(data_months_df$started_at)==10|
                                                      month(data_months_df$started_at)==11 ~ "Autumn",
                                                    month(data_months_df$started_at)==12|
                                                      month(data_months_df$started_at)==01|
                                                      month(data_months_df$started_at)==02 ~ "Winter",
                                                    month(data_months_df$started_at)==03|
                                                      month(data_months_df$started_at)==04|
                                                      month(data_months_df$started_at)==05 ~ "Spring",
                                                    month(data_months_df$started_at)==06|
                                                      month(data_months_df$started_at)==07|
                                                      month(data_months_df$started_at)==08 ~ "Summer",
                                                    month(data_months_df$started_at)==09 ~ "Autumn"
                                                  ))
colnames(season_df) # Now dataset has 17 columns

# Plotting this trend into graph bar to get insight
ggplot(data = season_df) +
  geom_bar(mapping = aes(x=Seasons, fill="brown")) +
  facet_wrap(~member_casual) +
  labs(title = "Cyclistic Trends per Season", x="Customer Type", y="Count")

# Identify which bike usage by both cathegory
bike_type_df <- season_df %>%
  select(rideable_type, Months) %>%
  group_by(Months, rideable_type) %>%
  count() %>%
  arrange()
head(bike_type_df)

#Plotting this finding into graph bar
ggplot(data = season_df) +
  geom_bar(mapping = aes(x=rideable_type, fill=rideable_type)) +
  facet_wrap(~member_casual) +
  labs(title = "Bike Type per Month", x="Bike Type", y="Count")

# identify which user has the farthest mileage on each month
distance_df <- data_months_df %>%
  group_by(
    member_casual, Months
  ) %>%
  summarize(
    n_trip_month=n(),
    avg_duration_month=mean(ride_distance),
    total_duration_month=sum(ride_distance)
  )

head(distance_df)

# Now try to find average, minimum and maximum ride_distance from each group of member
summary_ride <- cyclistic_df %>%
  group_by(member_casual) %>%
  summarize(number_of_rides=n(),
            ride_average=mean(ride_duration),
            distance_average=mean(ride_distance),
            ride_min=min(ride_duration),
            ride_max=max(ride_duration))
head(summary_ride)

# Export some dataframe into local storage as dataset for further analysis and visualization purpose
fwrite(season_df, "cyclistic_clean_data.csv")
fwrite(bike_type_df, "bike_type.csv")
fwrite(bicycles, "bike_type_member.csv")
fwrite(day_df, "day_of_week.csv")
fwrite(member_df, "total_user.csv")
fwrite(month_df, "ride_by_month.csv")
fwrite(summary_ride, "summary_ride.csv")
fwrite(distance_df, "ride_length_summary.csv")
