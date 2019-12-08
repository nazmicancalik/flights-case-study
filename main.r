library(ggplot2)
library(tidyverse)
library(magrittr)

airlines_data <- read.csv("data/airlines.csv")
airports_data <- read.csv("data/airports.csv")
flights_lax_data <- read.csv("data/flightsLAX.csv")


# Merge two data tables. 
flights <- merge(x=flights_lax_data, y=airlines_data, by.x="AIRLINE", by.y = "IATA_CODE")
flights <- flights %>% 
  rename(
    AIRLINE_CODE = AIRLINE,
    AIRLINE = AIRLINE.y
  )
flights <- as.data.table(flights)
ggplot(data = flights, mapping = aes(x = DAY_OF_WEEK, y = ARRIVAL_DELAY)) + geom_point()

ggplot(data = flights, mapping = aes(x = AIRLINE, y = (DEPARTURE_DELAY + ARRIVAL_DELAY))) + geom_line()

ggplot(data = flights, mapping = aes(x = MONTH, y = (DEPARTURE_DELAY + ARRIVAL_DELAY))) +geom_point() + 
  facet_wrap(facets = vars(AIRLINE))

ggplot(data=flights, aes(x=AIRLINE_CODE, y=(DEPARTURE_DELAY + ARRIVAL_DELAY))) +
  geom_bar(stat="identity")

# Totally new trial
flights %>% 
  group_by(AIRLINE_CODE) %>% 
    summarise(mean_arrival_delay = mean(ARRIVAL_DELAY, na.rm=TRUE)) %>% 
      ggplot(aes(x = AIRLINE_CODE, y = mean_arrival_delay, fill = AIRLINE_CODE)) +
      geom_bar(stat = "identity") +
      theme_classic() +
      labs(
        x = "Airline Company",
        y = "Average arrival delay",
        title = paste(
          "Average arrival delay according to companies"
        )
      )

flights %>% 
  group_by(AIRLINE_CODE) %>% 
  summarise(mean_departure_delay = mean(DEPARTURE_DELAY, na.rm=TRUE)) %>% 
  ggplot(aes(x = AIRLINE_CODE, y = mean_departure_delay, fill = AIRLINE_CODE)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Airline Company",
    y = "Average departure delay",
    title = paste(
      "Average arrival delay according to companies"
    )
  )

summed_flights_by_airport <- flights[,.(ORIGIN_AIRPORT.Sum =sum(ORIGIN_AIRPORT)),by=AIRPORT]

