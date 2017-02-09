# 2017-02-01

# Graphics of OR election results by county

# Packages
library(ggmap)
library(dplyr)
library(scales)
library(rprojroot)

# File path
root = find_root(is_rstudio_project)

# Oregon county map data ----
or_county = map_data("county", regions = "oregon")

# Read in other datasets, saved as .RData in R subfolder
# Population data in "population" column
	# Data from 2010 census
load(file.path(root, "R", "or_pop_data.RData"))

# Presidential election data by county
	# By "candidate", Repub, Dem, or Other
		# "Other" encompasses third party and write-in
	# Percent of total votes for each candidate type in "percent"
	# Total votes candidate type in "votes"
load(file.path(root, "R", "or_election_data.RData"))

# Eligible voters by county
	# "tot_voters" is total eligible voters on election day
load(file.path(root, "R", "or_eligible.RData"))

# Combine map data and other datasets ----

# This dataset is 3x longer than the map data due to the three types of "candidate"
or_data_all = or_county %>%
	inner_join(., or_election_data, by = c("subregion" = "county")) %>%
	inner_join(., or_pop_data, by = c("subregion" = "county")) %>%
	inner_join(., or_eligible, by = c("subregion" = "county"))

# Calculate dataset for voter turnout
	# The total number of votes over eligible voters
	# Summary dataset has total_votes, tot_voters, turnout = percent voters voted
or_data_all = or_election_data %>%
	group_by(county) %>%
	summarise(total_votes = sum(votes)) %>%
	inner_join(or_data_all, ., by = c("subregion" = "county")) %>%
	mutate(turnout = total_votes/tot_voters)

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

# Summary datasets for label placement ----

# Turnout
turnout_labels = or_data_all %>%
	filter(candidate == "Other") %>%
	group_by(group, subregion) %>%
	summarise(lat = mean(center_lat), long = mean(center_long),
			turnout = mean(turnout), county_area = mean(county_area))

# Percent Republican candidate
percent_repub = or_data_all %>%
	filter(candidate == "Trump") %>%
	group_by(group, subregion) %>%
	summarise(lat = mean(center_lat), long = mean(center_long),
			percent = mean(percent), county_area = mean(county_area))

# Percent Democratic candidate
percent_dem = or_data_all %>%
	filter(candidate == "Clinton") %>%
	group_by(group, subregion) %>%
	summarise(lat = mean(center_lat), long = mean(center_long),
			percent = mean(percent), county_area = mean(county_area))

# Theme settings for graphics ----

# Theme settings from http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
map_theme = theme(
	panel.background = element_blank(),
	axis.text = element_blank(),
	axis.line = element_blank(),
	axis.ticks = element_blank(),
	axis.ticks.length = unit(0, "mm"),
	panel.border = element_blank(),
	panel.grid = element_blank(),
	axis.title = element_blank(),
	plot.title = element_text(hjust = 0.5),
	plot.margin = unit(c(0, 0, 0, 0), "mm"),
	legend.margin = margin(0, 0, 0, 0)
)

# Basic graphic example, with county outlines ----
ggplot(data = or_county, aes(x = long, y = lat, group = group)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	map_theme

# Graphics with eligible voters ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group, fill = tot_voters)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	map_theme +
	scale_fill_gradient(name = NULL, high = "#132B43", low = "#56B1F7",
					trans = "log10", breaks = c(1000, 10000, 100000, 500000),
					labels = scales::comma(c(1000, 10000, 100000, 500000))) +
	labs(title = "Eligible voters in 2016 election",
		caption = expression(italic("Color is displayed on log10 scale"))) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))


# Graphics of population ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group, fill = population)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	map_theme +
	scale_fill_gradient(name = NULL, high = "#132B43", low = "#56B1F7",
					trans = "log10", breaks = c(1000, 10000, 100000, 750000),
					labels = scales::comma(c(1000, 10000, 100000, 750000))) +
	labs(title = "Population from 2010 census",
		caption = expression(italic("Color is displayed on log10 scale"))) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))


# Graphics for voter turnout ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = turnout)) +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	map_theme +
	scale_fill_gradient(name = NULL, labels = scales::percent,
					high = "#132B43", low = "#56B1F7") +
	labs(title = "Voter turnout (%) in 2016 election",
		caption = expression(paste(italic("Voter turnout is the number of votes " ),
							  italic("divided by number of eligible voters")))) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))

# With labels added
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = turnout)) +
	geom_polygon(fill = NA, color = "white") +
	coord_map() +
	map_theme +
	scale_fill_gradient(name = NULL, labels = scales::percent,
					high = "#132B43", low = "#56B1F7") +
	labs(title = "Voter turnout (%) in 2016 election",
		caption = expression(paste(italic("Voter turnout is the number of votes " ),
							   italic("divided by number of eligible voters")))) +
	geom_label(data = turnout_labels, aes(label = percent(turnout), size = county_area),
			 show.legend = FALSE) +
	scale_size(range = c(2, 5)) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))


# Graphics of election results ----

# Percent vote for republican presidential candidate out of all voters
ggplot(data = subset(or_data_all, candidate == "Trump"),
	  aes(x = long, y = lat, group = group, fill = percent)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	map_theme +
	scale_fill_gradient2(limits = c(0, 100), low = muted("#56B1F7"),
					 high = muted("red"), mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nRepublican presidential candidate in the 2016 election") +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))

ggplot(data = subset(or_data_all, candidate == "Trump"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = percent)) +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	map_theme +
	scale_fill_gradient2(limits = c(0, 100), low = muted("#56B1F7"),
					 high = muted("red"), mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nRepublican presidential candidate in the 2016 election") +
	geom_label(data = percent_repub, aes(label = percent(percent/100), size = county_area),
			show.legend = FALSE) +
	scale_size(range = c(2, 5)) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))

# Percent vote for democratic presidential candidate out of all voters
ggplot(data = subset(or_data_all, candidate == "Clinton"),
	  aes(x = long, y = lat, group = group, fill = percent)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	map_theme +
	scale_fill_gradient2(name = "", limits = c(0, 100), high = muted("#56B1F7"),
					 mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nDemocratic presidential candidate in the 2016 election") +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))

ggplot(data = subset(or_data_all, candidate == "Clinton"),
	  aes(x = long, y = lat, group = group)) +
	geom_polygon(aes(fill = percent)) +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	map_theme +
	scale_fill_gradient2(limits = c(0, 100), high = muted("#56B1F7"),
					 mid = "white", midpoint = 50) +
	ggtitle("Percentage of total ballots that were cast for the\nDemocratic presidential candidate in the 2016 election") +
	geom_label(data = percent_dem, aes(label = percent(percent/100), size = county_area),
			 show.legend = FALSE) +
	scale_size(range = c(2, 5)) +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))

# Percent vote for third part/write-in presidential candidate out of all voters ----
ggplot(data = subset(or_data_all, candidate == "Other"),
	  aes(x = long, y = lat, group = group, fill = percent)) +
	geom_polygon() +
	geom_polygon(fill = NA, color = "black") +
	coord_map() +
	map_theme +
	scale_fill_gradient(name = NULL, limits = c(5, 15), high = "#132B43", low = "#56B1F7") +
	ggtitle("Percentage of total ballots cast for third-party or write-in\npresidential candidate in the 2016 election") +
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0))


# end
