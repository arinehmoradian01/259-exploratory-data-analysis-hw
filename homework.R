# Arineh Moradian

# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file


# Read_weather function assigned to tibble
read_weather <- function(station) {
  tibble_files <- paste0("us-weather-history/", station, ".csv")
  data <- read_csv(tibble_files) %>%
    mutate(date = as.Date(date), station = station)
  return(data)
}

# Checking "read_weather" by glimpsing a single station's file

glimpse(read_weather("KCLT"))





# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"


# reading in all 10 stations- saving as 'ds', binding rows in code because of map_dfr() & map()

ds <- map(stations, read_weather) %>% bind_rows()




# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 


# creating a factor called 'city' based on station variable- station (level), city (label)
ds <- ds %>%
  mutate(city = factor(station, levels = stations, labels = cities))

# checking 365 days of data for each city

fct_count(ds$city)




# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree


# writing a function to convert f to c, make sure to round 

convert_fahrenheit_to_celcius <- function(fahrenheit) {
  round((fahrenheit - 32) * 5 / 9, 1)
}

# now we mutate!! this is to convert all of the temperature

ds <- ds %>%
  mutate(across(contains("temp"), convert_fahrenheit_to_celcius))

glimpse(ds)






### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.


# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

# counting the number of extreme temperature days, grouping data set by city, sorting in descending order

extreme_temperature_days <- ds %>%
  filter(actual_min_temp == record_min_temp | actual_max_temp == record_max_temp) %>%
  group_by(city) %>%
  summarise(extreme_temperature_days = n()) %>%
  arrange(desc(extreme_temperature_days))

print(extreme_temperature_days)


# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 


# pulling out the month from the date- making month a factor
ds <- ds %>%
  mutate(month = factor(month(date), levels = 1:12, labels = month.name))

# splitting the tibble by month (into a list of tibbles)!

tibble_by_month <- split(ds, ds$month)


# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before


# creating a for loop to determine correlation between actual precipitation and the average precipitation/ between the actual/average mins/maxes

for (m in levels(ds$month)) {
  loop_data <- ds %>% filter(month == m)
  
  precipitation_data <- cor(loop_data$actual_precipitation, loop_data$average_precipitation, use = "complete.obs")
  minimum_temp <- cor(loop_data$actual_min_temp, loop_data$average_min_temp, use = "complete.obs")
  max_temp <- cor(loop_data$actual_max_temp, loop_data$average_max_temp, use = "complete.obs")
  # to show the correlations
  print(paste(m, "Correlation-Precipitation:", precipitation_data))
  print(paste(m, "Correlation-Minimum Temperature:", minimum_temp))
  print(paste(m, "Correlation-Maximum Temperature:", max_temp))
}



# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this


# box plots of all of the numeric variables in the data set
plot_boxplot(ds, by = "city")
plot_boxplot(ds, by = "month")

# correlation to investigate between the continuous variables only
plot_correlation(ds, maxcat = 5)




# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

# scatterplot of the actual mean temperature by date, 3 columns for each city when you use facet_wrap

ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point() +
  facet_wrap(~ city, ncol = 3) +
  theme_minimal()




# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month
