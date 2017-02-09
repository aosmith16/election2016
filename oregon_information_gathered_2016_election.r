# 2017-02-07

# Pull in information at the county-level for Oregon
# 1. Population estimates (2010)
# 2. Presidential election results (2016)
# 3. Eligible OR voters (2016)

# Packages
library(rvest)
library(dplyr)


# Population data ----

or_pop_link = read_html("http://www.oregon-demographics.com/counties_by_population")

# Pull out info as a table
# This takes the three columns of data (rank, county, population)
	# and puts into a table
or_pop_dat = or_pop_link %>%
	html_table()

# Remove last row as is extra, unneeded info
or_pop_data = or_pop_dat[[1]][-37,]

# Lowecase names
names(or_pop_data) = tolower(names(or_pop_data))

# Make ranks numeric
# Make population numeric after removing commas
# Remove the word "County" from the values in the county column,
	# put counties to lowercase to match map info
or_pop_data = or_pop_data %>%
	mutate(rank = as.numeric(rank),
		  county = tolower(sub(" County", "", county)),
		  population = as.numeric(sub(",", "", population)))

# Save results for use later
# save(or_pop_data, file = "or_pop_data.RData")

# Election results for president ----

# Difficult to find and scrape the data
# OR secretary of state has the info, but couldn't figure out how to scrape it
	# This is the only data that has all parties and write-ins separated
# or_election16_link = read_html("http://results.oregonvotes.gov/resultsSW.aspx?type=FED&map=CTY")
# Politico has the data by candidate (no write-ins) but could only scrape 11 counties
# or_election16_link = read_html("http://www.politico.com/2016-election/results/map/president/oregon/")

# This website has the info in a coarse format by county that is readable
# "Other" appears to encompass all 3rd parties and write-ins
or_election16_link = read_html("http://uselectionatlas.org/RESULTS/datagraph.php?year=2016&fips=41&f=0&off=0&elect=0")


or_election_list = or_election16_link %>%
	html_table(fill = TRUE)

# Need all elements of the list except the first
# Clean prior to binding
	# 1. Name columns: county, candidate, percent, votes, other
	# 2. Remove percentage signs from "percent" and make numeric
	# 3. Remove commas from "votes" and make numeric
	# 4. Make county names lowercase to match map data
# Then bind, get rid of the 0 "McMullin" rows, and remove the "other" column

or_election_data = lapply(or_election_list[-1], function(x) {
	x = setNames(x, c("county", "candidate", "percent", "votes", "other"))
	x %>% mutate(percent = as.numeric(sub("%", "", percent)),
			   votes = as.numeric(sub(",", "", votes)),
			   county = tolower(county))
}) %>%
	bind_rows() %>%
	filter(candidate != "McMullin") %>%
	select(-other)

# Save results for use later
# save(or_election_data, file = "or_election_data.RData")

# Write-in numbers by county, typed in from http://results.oregonvotes.gov/resultsSW.aspx?type=FED&map=CTY
# Can use this to separate out third-party votes from write-ins (if needed)
writein = data.frame(county = unique(or_election_data$county),
				 candidate = "writein",
				 votes = c(217, 1828, 8498, 732, 1016, 834, 349, 328, 3265, 1409, 23, 134,
				 		115, 317, 3641, 321, 1298, 829, 76, 7278, 838, 2466, 391, 5749, 139,
				 		12757, 1774, 28, 404, 919, 417, 141, 422, 11513, 16, 2112) )


# Eligible voters by county ----
# Again, I can't figure out how to scrape OR SOS; something to do with pop-up info
# or_voters_link = read_html("http://results.oregonvotes.gov/VoterTurnoutDetails.aspx?map=TURN")

# Typed in from above link; there is monthly info on eligible voters but neither
	# Oct or Nov 2016 match the given information
or_eligible = data.frame(county = unique(or_election_data$county),
					tot_voters = c(10861, 58258, 267754, 24848, 34921, 40133, 15180, 15731, 121220,
							 70232, 1264, 5097, 4745, 13345, 142423, 12511, 59913, 39450, 4729,
							 241728, 31965, 75927, 13969, 182454, 5504, 502541, 51607, 1240, 17347,
							 38069, 16396, 5198, 15540, 334077, 981, 62045) )

# Save results for later use
# save(or_eligible, file = "or_eligible.RData")


# end
