install.packages("tidyverse")
install.packages("plotly")

library("tidyverse")
library ("plotly")

#Datafile
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")


# Load the map data
map_world <- map_data("world")

# Merge the data and map
map_join_data <- map_world %>%
  left_join(unicef_indicator_2, by = c("region" = "country"))


#worldmaps
ggplot(data = map_join_data) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_gradient(name = "Child Malnutrition Index", low = "red", high = "orange") +
  labs(title = "Global Child Malnutrition Trends", x = "Longitude", y = "Latitude", fill = "BMI") +
  theme_minimal() +
  theme(legend.position = "right")

#Print
print(map_world)

#map2_Time series 
time_series_data <- unicef_indicator_2 %>%
  mutate(time_period = year(mdy(time_period))) %>%  # Convert time_period to year
  filter(time_period >= 1998 & time_period <= 2022) %>%
  group_by(time_period) %>%
  summarize(avg_obs_value = mean(obs_value, na.rm = TRUE))

plot_ly(time_series_data, x = ~time_period, y = ~avg_obs_value) %>%
  add_trace(type = "scatter", mode = "lines") %>%
  layout(title = "Trend of Average Child Malnutrition Index Over Time",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Average BMI"))

#print_Time_series
print(time_series_data)

#map3
time_series_data_1 <- unicef_indicator_2 %>%
  group_by(country, time_period) %>%  # Group by both country and time period
  summarize(obs_value = mean(obs_value, na.rm = TRUE)) %>%
  mutate(time_period = str_extract(time_period, "\\d{4}"))%>%
  filter(country %in% c("Egpyt", "India", "Bangladesh", "Afghanistan", "Uganda", "Brazil", "Central African Republic", "Cuba", "Georgia"))  # Select a few countries for clarity
# Interactive Plotly Visualization
plot_ly(time_series_data_1, x = ~time_period, y = ~obs_value, color = ~country) %>%
  add_trace(type = "scatter", mode = "lines+markers", 
            marker = list(size = 8, opacity = 0.7),
            hovertemplate = paste("<b>Country: %{color}</b><br>",
                                  "Year: %{x}<br>",
                                  "Average BMI: %{y:.2f}<extra></extra>")) %>%
  layout(title = "Country Trends- Average Child BMI Over Time",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Average BMI"),
         legend = list(title = list(text = "Country")))


#scatter_plot

filtered_data <- unicef_indicator_2 %>%
  filter(sex == "Total") %>%
  group_by(time_period, country) %>%
  summarize(obs_value = mean(obs_value, na.rm = TRUE))

plot_ly(filtered_data, x = ~time_period, y = ~country, size = ~obs_value, 
        text = ~paste("Country:", country, "<br>Year:", time_period, "<br>Average BMI:", obs_value),
        hoverinfo = 'text',
        type = 'scatter', mode = 'markers') %>%
  layout(title = "Average Child Malnutrition Index by Country and Time Period",
         xaxis = list(title = "Time Period"),
         yaxis = list(title = "Country"))


#barchart

filtered_data <- unicef_indicator_2 %>%
  filter(sex == "Female") %>%
  group_by(time_period, country) %>%
  summarize(obs_value = mean(obs_value, na.rm = TRUE))%>%
  mutate(time_period = str_extract(time_period, "\\d{4}"))


plot_ly(filtered_data, x = ~time_period, y = ~obs_value, color = ~country, type = 'bar', 
        hovertemplate = paste("<b>Country: %{color}</b><br>",
                              "Year: %{x}<br>",
                              "Average BMI: %{y:.2f}<extra></extra>")) %>%
  layout(title = "Average Female Child Malnutrition Rate by Country and Time Period",
         xaxis = list(title = "Time Period"),
         yaxis = list(title = "Average BMI"))

