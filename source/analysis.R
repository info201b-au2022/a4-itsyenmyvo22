library("tidyverse")
library("ggplot2")

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
#Your functions and variables might go here ... <todo: update comment>
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

black_jail_pop <- incarceration %>%
  filter(year == 2018) %>%
  summarise(total_black = sum(black_jail_pop, na.rm = TRUE)) #find population of black ppl in jail (2018)

white_jail_pop <- incarceration %>%
  filter(year == 2018) %>%
  summarise(total_white = sum(white_jail_pop, na.rm = TRUE)) #find population of white ppl in jail (2018)

black_pop_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarise(black_pop = sum(black_pop_15to64, na.rm = TRUE)) #find black population (2018)

white_pop_2018 <- incarceration %>%
  filter(year == 2018) %>% 
  summarise(white_pop = sum(white_pop_15to64, na.rm = TRUE)) #find white population (2018)

ratio_blackpop_jail <- black_pop_2018$black_pop / black_jail_pop$total_black 
ratio_blackpop_jail <- round(ratio_blackpop_jail, 0) #calculate ratio of black ppl going to jail

ratio_whitepop_jail <- white_pop_2018$white_pop / white_jail_pop$total_white 
ratio_whitepop_jail <- round(ratio_whitepop_jail, 0) #calculate ratio of white ppl going to jail

total_jailpop_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(total_jail_pop_2018 = sum(total_jail_pop, na.rm = TRUE)) #find total jail pop (2018)

percent_black_in_jail <- (black_jail_pop$total_black / total_jailpop_2018$total_jail_pop_2018) * 100  
percent_black_in_jail <- round(percent_black_in_jail, 0) #find % black ppl in jail

percent_white_in_jail <- (white_jail_pop$total_white / total_jailpop_2018$total_jail_pop_2018) * 100  
percent_white_in_jail <- round(percent_white_in_jail, 0) #find % whitw ppl in jail

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>

# df <- incarceration %>% #wrangle data for plot (jail population in U.S.)
#   drop_na(total_jail_pop) %>%
#   select(year, total_jail_pop)
# 
# ggplot(data = df) +  #make plot (jail population in U.S.)
#   geom_col(mapping = aes(x = year, y = total_jail_pop)) +
#   labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
#   labs(x = "Year") +
#   scale_y_continuous("Total Jail Population", labels = scales::comma)

#----------------------------------------------------------------------------#
# This function ... #wrangle data for plot (jail population in U.S.)
get_year_jail_pop <- function() { 
  df <- incarceration %>%
    drop_na(total_jail_pop) %>%
    select(year, total_jail_pop)
return(df)   
}



# This function ... #make plot (jail population in U.S.)
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
    labs(x = "Year") +
    scale_y_continuous("Total Jail Population", labels = scales::comma)
  return(plot)   
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas

states <- c("CA", "WY", "WA", "TX", "FL")

get_jail_pop_by_states <- function() { #wrangle data for plot (jail population in states)
  df1 <- incarceration %>%
    drop_na(total_jail_pop) %>%
    select(year, state, total_jail_pop) %>%
    filter(state %in% c("CA", "WY", "WA", "TX", "FL")) %>%
    group_by(year, state) %>%
    summarize(total = sum(total_jail_pop))
}

options(dplyr.summarise.inform = FALSE) #remove message from html file

plot_jail_pop_by_states <- function() { #make plot (jail population in states)
  plot1 <- ggplot(data = get_jail_pop_by_states()) +
    geom_line(mapping = aes(x = year, y = total, color = state)) +
    labs(title = "Increase of Jail Population by States (1970-2018)") +
    labs(x = "Year") +
    scale_y_continuous("Total Jail Population", labels = scales::comma)
  return(plot1)
}

plot_jail_pop_by_states()

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# get_gender_data <- function() { #wrangle data for plot (jail population by gender)
#   female <- incarceration %>%
#     group_by(year) %>%
#     summarise(total_female_jail_pop = sum(female_jail_pop, na.rm = TRUE))
#   male <- incarceration %>%
#     group_by(year) %>%
#     summarise(total_male_jail_pop = sum(male_jail_pop, na.rm = TRUE))
#   genderdf <- left_join(female, male, by = "year")
#   genderdf <- genderdf %>%
#     gather(key = gender, value = pop, -year)
#   return(genderdf)
# }
# 
# plot_jail_pop_by_gender <- function() { #make plot (jail population by gender)
#   plot2.1 <- ggplot(data = genderdf) +
#     geom_point(mapping = aes(x= year, y= pop, color = gender)) +
#     labs(title = "Increase of Jail Population by Gender (1970-2018)") +
#     labs(x = "Year") +
#     scale_y_continuous("Total Jail Population", labels = scales::comma) 
#   return(plot2.1)
# }
# 
# plot_jail_pop_by_gender()

get_race_data <- function() { #wrangle data for plot (jail population by race)
  black <- incarceration %>%
    group_by(year) %>%
    summarise(total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE))
  white <- incarceration %>%
    group_by(year) %>%
    summarise(total_white_jail_pop = sum(white_jail_pop, na.rm = TRUE))
  racesdf <- left_join(black, white, by = "year")
  racesdf <- racesdf %>%
    gather(key = race, value = pop, -year)
  return(racesdf)
}

plot_jail_pop_by_race <- function() {  #make plot (jail population by race)
  plot2 <- ggplot(data = get_race_data()) +
    geom_point(mapping = aes(x = year, y = pop, color = race)) +
    labs(title = "Increase of Jail Population by Race (1970-2018)") +
    labs(x = "Year") +
    scale_y_continuous("Total Jail Population", labels = scales::comma)
  return(plot2)
}

plot_jail_pop_by_race()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# black_pop_each_state_15to56 <- incarceration %>%
#   filter(year == 2018) %>%
#   group_by(state) %>%
#   summarise(black_pop_each_state_15to56 = sum(black_pop_15to64))
# 
# 
# jail_pop_each_state <- incarceration %>%
#   filter(year == 2018) %>%
#   group_by(state) %>%
#   summarise(jail_pop_each_state = sum(total_jail_pop, na.rm = TRUE))
# 
# 
# map_df <- left_join(jail_pop_each_state, black_pop_each_state_15to56, by = "state")
# 
# 
# coordinates_50states <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-itsyenmyvo22/main/data/coordinates_50states2.csv", stringsAsFactors = FALSE) %>%
#   select(lat, lon, state) %>%
#   add_row(lat = 43.969515, lon = -99.901813, state = "SD") %>%
#   add_row(lat = 38.597626, lon = -80.454903, state = "WV")
# names(coordinates_50states)[names(coordinates_50states) == "lon"] <- "long"
# 
# 
# map_df_with_coords <- left_join(map_df, coordinates_50states, by = "state")

# 
# black_pop <- map_df_with_coords %>%
#   select(state, black_pop_each_state_15to56)
# 
# 
# jail_pop_div <- map_df_with_coords %>%
#   summarise(jail_pop_div_by_3000 = jail_pop_each_state/3000)


get_data_for_map <- function() { #wrangle data for map
  black_pop_each_state_15to56 <- incarceration %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    summarise(black_pop_each_state_15to56 = sum(black_pop_15to64))
  jail_pop_each_state <- incarceration %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    summarise(jail_pop_each_state = sum(total_jail_pop, na.rm = TRUE)) 
  map_df <- left_join(jail_pop_each_state, black_pop_each_state_15to56, by = "state")
  coordinates_50states <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-itsyenmyvo22/main/data/coordinates_50states2.csv", stringsAsFactors = FALSE) %>%
    select(lat, lon, state) %>%
    add_row(lat = 43.969515, lon = -99.901813, state = "SD") %>%
    add_row(lat = 38.597626, lon = -80.454903, state = "WV")
  names(coordinates_50states)[names(coordinates_50states) == "lon"] <- "long"
  map_df_with_coords <- left_join(map_df, coordinates_50states, by = "state")
  new_map_df_with_coords <- map_df_with_coords %>%
    mutate(full_name = tolower(state.name[match(map_df_with_coords$state, state.abb)])) %>%
    rename(abbr = state) %>%
    rename(state = full_name)
  return(new_map_df_with_coords)
}

# View(map_df_with_coords)
# new_map_df_with_coords <- map_df_with_coords %>%
#   mutate(full_name = tolower(state.name[match(map_df_with_coords$state, state.abb)])) %>%
#   rename(abbr = state) %>%
#   rename(state = full_name)
# View(new_map_df_with_coords)

plot_map <- function() {  #plot data for map
  black_pop <- get_data_for_map() %>%
    select(state, black_pop_each_state_15to56)
  jail_pop_div <- get_data_for_map() %>%
    summarise(jail_pop_div_by_4000 = jail_pop_each_state/4000)
  state_shape <- map_data("state")
  ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group),
      color = "white",
      linewidth = .1
    ) +
    coord_map()
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(black_pop, by = "state")
  plot3 <- ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_pop_each_state_15to56),
      color = "white",
      linewidth = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Black Population") +
    geom_point(
      data = get_data_for_map(),
      mapping = aes(x = long, y = lat),
      color = "yellow",
      size = jail_pop_div$jail_pop_div_by_4000
    ) +
    labs(title = "Proportion of Jail Populations in States (2018)")
  return(plot3)
}

plot_map()

# state_shape <- map_data("state")
# 
# ggplot(state_shape) +
#   geom_polygon(
#     mapping = aes(x = long, y = lat, group = group),
#     color = "white",
#     linewidth = .1
#   ) +
#   coord_map()
# 
# state_shape <- map_data("state") %>%
#   rename(state = region) %>%
#   left_join(black_pop, by = "state")
# 
# ggplot(state_shape) +
#   geom_polygon(
#     mapping = aes(x = long, y = lat, group = group, fill = black_pop_each_state_15to56),
#     color = "white",
#     linewidth = .1
#   ) +
#   coord_map() +
#   scale_fill_continuous(low = "#132B43", high = "Red") +
#   labs(fill = "Black Population") +
#   geom_point(
#     data = map_df_with_coords,
#     mapping = aes(x = long, y = lat),
#     color = "yellow",
#     size = jail_pop_div$jail_pop_div_by_3000
#   ) +
#   labs(title = "Proportion of Jail Populations in States")

# get_data_for_map <- function() {
#   data <- incarceration %>%
#     filter(year == 2018) %>%
#     group_by(state) %>%
#     summarise(total_jail_pop_2018 = sum(total_jail_pop))
# }
# 
# 
# data <- incarceration %>%
#   filter(year == 2018) %>%
#   group_by(state) %>%
#   summarise(total_jail_pop_2018 = sum(total_jail_pop, na.rm = TRUE))


# state_shape <- map_data("state")
# 
# ggplot(state_shape) +
#   geom_polygon(
#     mapping = aes(x = long, y = lat, group = group),
#     color = "white",
#     linewidth = .1
#   ) +
#   coord_map()
# 
# state_shape <- map_data("state") %>%
#   rename(state = region) %>%
#   left_join(data, by = "state")
# 
# ggplot(state_shape) + 
#     geom_polygon(
#       mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop_2018),
#       color = "white",
#       linewidth = .1
#     ) +
#     coord_map() +
#     scale_fill_continuous(low = "#132B43", high = "Red") +
#     labs(fill = "Jail Population")
# 
# 
# 
# data <- left_join(data, coordinates_50states, by = "state")

#----------------------------------------------------------------------------#

## Load data frame ---- 

