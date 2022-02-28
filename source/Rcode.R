install.packages('plyr', repos = "http://cran.us.r-project.org")

# Load data set

Incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

setwd("~/Documents/info201code/a3-Trangtran62")

# Summary: number of features and what are they
ncol(Incarceration)
nrow(Incarceration)
features <- colnames(Incarceration)
print(features)

library(dplyr)
library(RcppRoll)
library(tidyverse)

#Select data with specific features to work with
df <- Incarceration %>%
  select(year, state, county_name, total_pop_15to64, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop, total_jail_adm, total_jail_dis, total_prison_pop, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, white_prison_pop, aapi_prison_adm, black_prison_adm, latinx_prison_adm, native_prison_adm, other_race_prison_adm, white_prison_adm)

View(df)

# Data wrangling to get 5 values summarizing the data set
# What is the largest/smallest total prison population in a year?
lagest_prison_population <- df %>%
  group_by(year) %>%
  summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
  filter(total_prison_pop == max(total_prison_pop)) %>%
  summarise(total_prison_pop)

largest_prison_population <- lagest_prison_population$total_prison_pop

smallest_prison_population <- df %>%
  group_by(year) %>%
  summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
  filter(total_prison_pop > 0) %>% # There are no data in 2017 and 2018, so the total of prison population in those two years can be 0.
  filter(total_prison_pop == min(total_prison_pop)) %>%
  summarise(total_prison_pop)

smallest_prison_population <- smallest_prison_population$total_prison_pop

# What is the largest/smallest prison population over total population ratio in a year?
largest_prison_population_ratio <- df %>%
  group_by(year) %>%
  mutate(ratio = total_prison_pop / total_pop_15to64) %>%
  filter(ratio > 0) %>%
  summarise(ratio = mean(ratio)) %>%
  filter(ratio == max(ratio)) %>%
  summarise(ratio)

largest_prison_population_ratio <- largest_prison_population_ratio$ratio
largest_prison_population_ratio <- round(largest_prison_population_ratio, 3)

smallest_prison_population_ratio <- df %>%
  group_by(year) %>%
  mutate(ratio = total_prison_pop / total_pop_15to64) %>%
  filter(ratio > 0) %>%
  summarise(ratio = mean(ratio)) %>%
  filter(ratio == min(ratio)) %>%
  summarise(ratio)

smallest_prison_population_ratio <- smallest_prison_population_ratio$ratio
# Average total prison population of Black people over the years?
df1 <- df %>%
  group_by(year) %>%
  filter(black_prison_pop > 0) %>%
  summarise(total_black_prison = sum(black_prison_pop))
average_prison_black <- round(mean(df1$total_black_prison))

# Average total prison population of White people over the years?
df2 <- df %>%
  group_by(year) %>%
  filter(white_prison_pop > 0) %>%
  summarise(total_white_prison = sum(white_prison_pop))
average_prison_white <- round(mean(df2$total_white_prison))

# Trend over time graph
# Jail and prison population over time for each race
df[is.na(df)] <- 0
df3 <- df %>%
  select(year, aapi_jail_pop, black_jail_pop, native_jail_pop, white_jail_pop, latinx_jail_pop, aapi_prison_pop, black_prison_pop, native_prison_pop, white_prison_pop, latinx_prison_pop) %>%
  mutate(aapi = aapi_jail_pop + aapi_prison_pop, black = black_prison_pop + black_jail_pop, native = native_jail_pop + native_prison_pop, white = white_prison_pop + white_jail_pop, latinx = latinx_jail_pop + latinx_prison_pop) %>%
  select(year, aapi, black, native, white, latinx) %>%
  group_by(year) %>%
  summarise(aapi = sum(aapi), black = sum(black), native = sum(native), white = sum(white), latinx = sum(latinx))

df3[df3 == 0] <- NA

df3_black <- df3 %>%
  select(year, black) %>%
  mutate(race = "black")
colnames(df3_black) <- c("year", "incarceration population", "race")
df3_aapi <- df3 %>%
  select(year, aapi) %>%
  mutate(race = "aapi")
colnames(df3_aapi) <- c("year", "incarceration population", "race")
df3_white <- df3 %>%
  select(year, white) %>%
  mutate(race = "white")
colnames(df3_white) <- c("year", "incarceration population", "race")
df3_native <- df3 %>%
  select(year, native) %>%
  mutate(race = "native")
colnames(df3_native) <- c("year", "incarceration population", "race")
df3_latinx <- df3 %>%
  select(year, latinx) %>%
  mutate(race = "latinx")
colnames(df3_latinx) <- c("year", "incarceration population", "race")

df3_all <- bind_rows(df3_aapi, df3_black, df3_latinx, df3_white, df3_native)

Trendgraph <- df3_all %>%
  ggplot(aes(year, `incarceration population`, color = race)) +

  geom_line() +
  ggtitle("Jail and prison population over time by race") + xlab("year") + ylab("jail and prison population") + theme(element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank(), legend.title= element_blank()) +
  scale_y_continuous(labels = scales::comma) 

print(Trendgraph)

# Comparison 
# Black and White incarceration rate (sum of jail and prison population)
df4 <- df %>% # data frame of black incarceration rate, adding race column
  select(year, black_jail_pop, black_prison_pop, black_pop_15to64) %>%
  group_by(year) %>%
  mutate(ratio = (black_jail_pop + black_prison_pop) / black_pop_15to64) %>%
  filter(ratio != Inf)  %>%
  summarise(ratio = mean(ratio)) %>%
  mutate(race = "black") %>%
  mutate(ratio_per_100000 = round(ratio * 100000)) %>%
  select(year, ratio_per_100000, race)

colnames(df4) <- c("year", "incarceration rate", "race")

print(df4)

df5 <- df %>% # data frame of white incarceration rate, adding race column
  select(year, white_jail_pop, white_prison_pop, white_pop_15to64) %>%
  group_by(year) %>%
  mutate(ratio_per_100000 = (white_jail_pop + white_prison_pop) * 100000 / white_pop_15to64) %>%
  filter(ratio_per_100000 != Inf) %>%
  summarise(ratio_per_100000 = round(mean(ratio_per_100000))) %>%
  mutate(race = "white") %>%
  select(year, ratio_per_100000, race)
colnames(df5) <- c("year", "incarceration rate", "race")

print(df5)

df6 <- bind_rows(df4, df5)
print(df6)

comparison_graph <- df6 %>%
  ggplot(aes(year, `incarceration rate`, color = race)) +
  geom_line() +
  ggtitle("Black and White incarceration rate per 100,000 population over time") + 
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  xlim(1990, 2020)
print(comparison_graph)

# Map
install.packages("usmap")
install.packages("viridis")
library(lubridate)
library(usmap)
library(viridis)

# Filter out data and create a new data frame

map_df <- Incarceration %>%
  filter(is.na(total_prison_pop) == FALSE) %>%
  filter(is.na(total_pop_15to64) == FALSE) %>%
  select(state, total_prison_pop, total_pop_15to64) %>%
  group_by(state) %>%
  summarise(total_prison_pop = sum(total_prison_pop), total_pop_15to64 = sum(total_pop_15to64)) %>%
  mutate(total_prison_pop_rate = total_prison_pop * 100000 / total_pop_15to64)

us_map <- usmap::us_map(region = "states")
usmap::plot_usmap(data = map_df, values = "total_prison_pop_rate", color = "grey") +
  scale_fill_continuous(type = "viridis", label = scales::comma) + 
  labs(title = "Cummulative prison population per 100,000 people",
       subtitle = "from 1970 to 2018",
       caption = "data: Vera Institue", 
       fill = "prison population") +
  theme_classic() +
  theme(panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), legend.position = "right",axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
