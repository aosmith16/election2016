# 2017-02-01

# Graphics of OR election results by county

# Packages
library(ggmap)
library(dplyr)
library(scales)

# Oregon county map data ----
or_county = map_data("county", regions = "oregon")

# Read in other datasets, saved as .RData
load("or_pop_data.RData")
load("or_election_data.RData")
load("or_eligible.RData")

# Combine datasets ----
# This dataset is 3x longer than the map data due to the three types of "candidate"
or_data_all = or_county %>%
	inner_join(., or_election_data, by = c("subregion" = "county")) %>%
	inner_join(., or_pop_data, by = c("subregion" = "county")) %>%
	inner_join(., or_eligible, by = c("subregion" = "county"))

# Make dataset for centroid and area of county for labeling ----
county_poly = map("county", "oregon", plot = FALSE, fill = TRUE)
# County centers
county_centroids = maps:::apply.polygon(county_poly, maps:::centroid.polygon)
# County areas
county_areas = maps:::apply.polygon(county_poly, maps:::area.polygon)

# Combine in to dataset
county_centroid_area = data.frame(county = gsub("oregon,", "", names(county_centroids)),
		 Reduce(rbind, county_centroids),
		 county_area = unlist(county_areas))
names(county_centroid_area) = c("county", "center_long", "center_lat", "county_area")

# Join this to the datset
or_data_all = or_data_all %>%
	inner_join(., county_centroid_area, by = c("subregion" = "county"))

# Theme settings for graphics ----

# Theme settings from http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
ditch_the_axes = theme(
	panel.background = element_blank(),
	axis.text = element_blank(),
	axis.line = element_blank(),
	axis.ticks = element_blank(),
	panel.border = element_blank(),
	panel.grid = element_blank(),
	axis.title = element_blank(),
	plot.title = element_text(hjust = 0.5)
)

# Basic graphic example, with county outlines ----
ggplot(data = or_county, aes(x = long, y = lat, group = group)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	ditch_the_axes

# Graphics with eligible voters ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group, fill = tot_voters)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient(name = NULL, trans = "log10", breaks = c(1000, 10000, 100000, 500000),
					labels = scales::comma(c(1000, 10000, 100000, 500000))) +
	ggtitle("Eligible voters in 2016 election, by county")

# Add labels of eligible voters as text

# Graphics of population ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group, fill = population)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient(name = NULL, trans = "log10", breaks = c(1000, 10000, 100000, 750000),
					labels = scales::comma(c(1000, 10000, 100000, 750000))) +
	ggtitle("Population from 2010 census, by county")

# Graphics with voter turnout ----

# Summarise dataset so have total votes and total voters, percent turnout
or_data_all = or_election_data %>%
	group_by(county) %>%
	summarise(total_votes = sum(votes)) %>%
	inner_join(or_data_all, ., by = c("subregion" = "county")) %>%
	mutate(turnout = total_votes/tot_voters)

ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = turnout)) +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient(labels = scales::percent, high = "#132B43", low = "#56B1F7") +
	ggtitle("Turnout in 2016 election, by county")

# Summary dataset for labels
turnout_labels = or_data_all %>%
	filter(candidate == "Other") %>%
	group_by(group, subregion) %>%
	summarise(lat = mean(center_lat), long = mean(center_long),
			turnout = mean(turnout), county_area = mean(county_area))

ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = turnout)) +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient(labels = scales::percent, high = "#132B43", low = "#56B1F7") +
	ggtitle("Turnout in 2016 election, by county") +
	geom_label(data = turnout_labels, aes(label = percent(turnout), size = county_area),
			 show.legend = FALSE) +
	scale_size(range = c(2, 5))


# Graphics of election results ----

# Percent vote for republican presidential candidate out of all voters
ggplot(data = subset(or_data_all, candidate == "Trump"),
	  aes(x = long, y = lat, group = group, fill = percent)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient2(limits = c(0, 100), low = muted("#56B1F7"),
					 high = muted("red"), mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nRepublican presidential candidate in the 2016 election")

# Summary dataset for labels
percent_trump = or_data_all %>%
	filter(candidate == "Trump") %>%
	group_by(group, subregion) %>%
	summarise(lat = mean(center_lat), long = mean(center_long),
			percent = mean(percent), county_area = mean(county_area))

ggplot(data = subset(or_data_all, candidate == "Trump"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = percent)) +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient2(limits = c(0, 100), low = muted("#56B1F7"),
					 high = muted("red"), mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nRepublican presidential candidate in the 2016 election") +
	geom_label(data = percent_trump, aes(label = percent(percent/100), size = county_area),
			show.legend = FALSE) +
	scale_size(range = c(2, 5))

# Percent vote for democratic presidential candidate out of all voters
ggplot(data = subset(or_data_all, candidate == "Clinton"),
	  aes(x = long, y = lat, group = group, fill = percent)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "black") +
	coord_fixed(1.3) +
	ditch_the_axes +
	scale_fill_gradient2(name = "", limits = c(0, 100), high = muted("#56B1F7"),
					 mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nDemocratic presidential candidate in the 2016 election")

# Summary dataset for labels
percent_clint = or_data_all %>%
	filter(candidate == "Clinton") %>%
	group_by(group, subregion) %>%
	summarise(lat = mean(center_lat), long = mean(center_long),
			percent = mean(percent), county_area = mean(county_area))

ggplot(data = subset(or_data_all, candidate == "Clinton"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = percent)) +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient2(limits = c(0, 100), high = muted("#56B1F7"),
					 mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nDemocratic presidential candidate in the 2016 election") +
	geom_label(data = percent_clint, aes(label = percent(percent/100), size = county_area),
			 show.legend = FALSE) +
	scale_size(range = c(2, 5))

# Percent vote for third part/write-in presidential candidate out of all voters ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group, fill = percent)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	ditch_the_axes +
	scale_fill_gradient(name = "", limits = c(5, 15), high = "#132B43", low = "#56B1F7") +
	ggtitle("Percentage of total ballots cast for third-party or write-in\npresidential candidate in the 2016 election")


# end
