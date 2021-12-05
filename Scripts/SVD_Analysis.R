# Loads the necessary libraries to perform the data visualizations and manipulations
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(here)

# Reads the .csv files containing the raw congressional data and initializes dataframes
house_90 <- read_csv(here("Data/house_90.csv"))
senate_90 <- read_csv(here("Data/senate_90.csv"))
house_116 <- read_csv(here("Data/house_116.csv"))
senate_116 <- read_csv(here("Data/senate_116.csv"))

# 90th House SVD Analysis -------------------------------------------

# Drops the NA values from the dataframe to avoid biasing our data points
house_90_no_missing <- drop_na(house_90)

# Creates an SVD matrix from the data
house_90_svd <- svd(house_90_no_missing[, -1:-4])

# Creates a dataframe to be used in our plot
house_90_rep <- data.frame("x" = house_90_svd$u[, 1],
                           "y" = house_90_svd$u[, 2],
                           "Party" = house_90_no_missing$party_code,
                           "state" = house_90_no_missing$state_abbrev)

# creates a dotplot showing the clustering from the SVD analysis on the Representatives in the 90th House
ggplot(house_90_rep, aes(x = x, y = y, color = Party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "SVD of Representatives During 90th House",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Replaces all NA's to 0s in the dataframe so that we can perform an accurate SVD analysis on votes
house_90_zeros <- house_90 %>%
  replace(., is.na(.), 0)

# Create an SVD Matrix
house_90_zero_svd <- svd(house_90_zeros[, -1:-4])

# Determines whether or not a vote was passed by checking if the sum over the column is positive or not
pass_fail <- colSums(house_90_zeros[, -1:-4]) > 0

# Creates a dataframe to be used for plotting the SVD of votes
house_90_votes <- data.frame("x" = house_90_zero_svd$v[, 1],
                             "y" = house_90_zero_svd$v[, 2],
                             "success" = pass_fail)

# Creates a dot plot showing the SVD of votes cast in the 90th House
ggplot(house_90_votes,
       aes(x = x, y = y, color = success)) +
  geom_point() +
  scale_color_discrete(name = "Did It Pass?",
                       labels = c("No", "Yes")) +
  labs(title = "SVD of Votes Cast in 90th House",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# A Vector of the squared singular values
sing_vals2_house_90 <- house_90_svd$d^2

# A dataframe containing the singular values and cumulative energy vectors
energy_house_90_df <- data.frame("sing_vals" = sing_vals2_house_90,
                                 "energy" = cumsum(sing_vals2_house_90/sum(sing_vals2_house_90)))

# Plots the strength of each singular value
ggplot(energy_house_90_df, aes(x = 1:nrow(energy_house_90_df), y = sing_vals)) +
  geom_point() +
  theme_bw(base_size = 20) +
  labs(y = "Singular Values", x = "k")

# Plots the cumulative energy retained with each new singular value
ggplot(energy_house_90_df, aes(x = 1:nrow(energy_house_90_df), y = energy)) +
  geom_point() +
  theme_bw(base_size = 20) +
  labs(y = "Cumulative Energy", x = "k")

# 90th Senate SVD Analysis ------------------------------------

# Drop NA's to analyze left singular vectors
senate_90_no_missing <- drop_na(senate_90)

# Create SVD Matrix
senate_90_svd <- svd(senate_90_no_missing[, -1:-4])

# Create dataframe for plotting left singular vectors
senate_90_ppl <- data.frame("x" = senate_90_svd$u[, 1],
                            "y" = senate_90_svd$u[, 2],
                            "Party" = senate_90_no_missing$party_code)

# dotplot depicting SVD analysis of left singular vectors (bipartisanship and partisanship of senators)
ggplot(senate_90_ppl, aes(x = x, y = y, color = Party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "SVD of Senators During 90th Senate",
       x = "Bipartisan Coordinate",
       y ="Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Converts NA's to 0's
senate_90_zeros <- senate_90 %>%
  replace(., is.na(.), 0)
# Generates SVD matrices
senate_90_zero_svd <- svd(senate_90_zeros[, -1:-4])
# Check if a vote passes
sen_90_pass_fail <- colSums(senate_90_zeros[, -1:-4]) > 0
# create dataframe
senate_90_votes <- data.frame("x" = senate_90_zero_svd$v[, 1],
                              "y" = senate_90_zero_svd$v[, 2],
                              "success" = sen_90_pass_fail)

# dotplot showing SVD analysis of votes
ggplot(senate_90_votes,
       aes(x = x, y = y, color = success)) +
  geom_point() +
  scale_color_discrete(name = "Did It Pass?",
                       labels = c("No", "Yes")) +
  labs(title = "SVD of Votes Cast in 90th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 116th House SVD Analysis ---------------------------------------

# Drop NA's
house_116_no_missing <- drop_na(house_116)
# Create SVD matrices
house_116_svd <- svd(house_116_no_missing[, -1:-4])
# Create dataframe for SVD analysis on left singular vectors
house_116_ppl <- data.frame("x" = house_116_svd$u[, 2],
                            "y" = house_116_svd$u[, 1],
                            "Party" = house_116_no_missing$party_code,
                            "state" = house_116_no_missing$state_abbrev)

# Dotplot showing SVD analysis of Representatives
ggplot(house_116_ppl, aes(x = x, y = y, color = Party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "SVD of Representatives in 116th House",
       x = "Bipartisan Coordinate",
       y ="Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Convert NA's to zeros
house_116_zeros <- house_116 %>%
  replace(., is.na(.), 0)
# Create SVD matrices
house_116_zero_svd <- svd(house_116_zeros[, -1:-4])
# Check if a vote passes
house_116_pass_fail <- colSums(house_116_zeros[, -1:-4]) > 0
# Create dataframe analyzing right singular vectors (votes)
house_116_votes <- data.frame("x" = house_116_zero_svd$v[, 2],
                              "y" = house_116_zero_svd$v[, 1],
                              "success" = house_116_pass_fail)

# Dotplot showing SVD analysis of votes
ggplot(house_116_votes,
       aes(x = x, y = y, color = success)) +
  geom_point() +
  scale_color_discrete(name = "Did It Pass?",
                       labels = c("No", "Yes")) +
  labs(title = "SVD of Votes Cast in 116th House",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 116th Senate SVD Analysis ------------------------------------------

# Drop NA's from dataframe
senate_116_no_missing <- drop_na(senate_116)
# Create SVD matrices
senate_116_svd <- svd(senate_116_no_missing[, -1:-4])
# Create dataframe to analyze left singular vectors (senators)
senate_116_ppl <- data.frame("x" = senate_116_svd$u[, 2],
                             "y" = senate_116_svd$u[, 1],
                             "Party" = senate_116_no_missing$party_code)

# dotplot showing SVD analysis of senators
ggplot(senate_116_ppl, aes(x = x, y = y, color = Party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "orange", "red")) +
  labs(title = "SVD of Senators in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Converts NA's to zeros
senate_116_zeros <- senate_116 %>%
  replace(., is.na(.), 0)
# Creates SVD matrices
senate_116_zero_svd <- svd(senate_116_zeros[, -1:-4])
# Checks if vote passes
senate_116_pass_fail <- colSums(senate_116_zeros[, -1:-4]) > 0
# Creates dataframe for analysis on right singular vectors (votes)
senate_116_votes <- data.frame("x" = senate_116_zero_svd$v[, 2],
                               "y" = senate_116_zero_svd$v[, 1],
                               "success" = senate_116_pass_fail)

# dotplot showing SVD analysis of votes
ggplot(senate_116_votes,
       aes(x = x, y = y, color = success)) +
  geom_point() +
  scale_color_discrete(name = "Did It Pass?",
                       labels = c("No", "Yes")) +
  labs(title = "SVD of Votes in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Partisanship of 4 States ------------------------------

# Filter the dataframes for only reps from CA, FL, TX, and NY
ca_ny_tx_fl_116 <- house_116_ppl %>%
  filter(state == c("CA", "FL", "TX", "NY"))
ca_ny_tx_fl_90 <- house_90_rep %>%
  filter(state == c("CA", "FL", "TX", "NY"))

# Dotplots showing partisanship on multiple axes with a plot for each state
ggplot(ca_ny_tx_fl_116,
       aes(x = x, y = y, color = Party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~ state) +
  labs(title = "SVD of Representatives in 116th House",
       x = "Bipartisan Coordinate",
       y ="Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(ca_ny_tx_fl_90,
       aes(x = x, y = y, color = Party)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~ state) +
  labs(title = "SVD of Representatives in 90th House",
       x = "Bipartisan Coordinate",
       y ="Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Extreme Partisans for the 4 States ---------------------------------

# Filter and sort the dataframes to only include the selected state and "Other"
ca_filter <- house_116_ppl$state %>%
  fct_other(keep = "CA") %>%
  cbind(house_116_ppl[, -4])
ca_sort <- ca_filter[order(ca_filter$., decreasing = TRUE),]
ny_filter <- house_116_ppl$state %>%
  fct_other(keep = "NY") %>%
  cbind(house_116_ppl[, -4])
ny_sort <- ny_filter[order(ny_filter$., decreasing = TRUE),]
tx_filter <- house_116_ppl$state %>%
  fct_other(keep = "TX") %>%
  cbind(house_116_ppl[, -4])
tx_sort <- tx_filter[order(tx_filter$., decreasing = TRUE),]
fl_filter <- house_116_ppl$state %>%
  fct_other(keep = "FL") %>%
  cbind(house_116_ppl[, -4])
fl_sort <- fl_filter[order(fl_filter$., decreasing = TRUE),]

# Generate 4 different dotplots showing partisans in relation to the rest of Congress (one for each state)
ca_reps <- ggplot(ca_sort,
                  aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of CA Reps in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ny_reps <- ggplot(ny_sort,
                  aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of NY Reps in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
tx_reps <- ggplot(tx_sort,
                  aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of TX Reps in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
fl_reps <- ggplot(fl_sort,
                  aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of FL Reps in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot all 4 graphs in a 2x2 fashion
grid.arrange(ca_reps, ny_reps, tx_reps, fl_reps, nrow = 2, ncol = 2)

# Age Analysis ---------------------------------

# Takes in one of the congressional dataframes (the ones that have no missing data) and calculates the median birth year and then separates the vector by grouping based off the median birth year. Returns a factor with levels "Older" and "Younger"
sort_by_age_group <- function(congress_data) {
  middle_age <- median(congress_data$born)
  year <- congress_data$born
  year[year <= middle_age] <- "Older"
  age_group <- year %>%
    fct_other(keep = "Older") %>%
    fct_recode(Younger = "Other")
  return(age_group)
}

# Initialize a transformed dataframe with an age_group column that has the values "Younger" or "Older" (relative to the median age)
house_116_age_grouped <- house_116_no_missing %>%
  sort_by_age_group() %>%
  cbind(house_116_ppl)
senate_116_age_grouped <- senate_116_no_missing %>%
  sort_by_age_group() %>%
  cbind(senate_116_ppl)
house_90_age_grouped <- house_90_no_missing %>%
  sort_by_age_group() %>%
  cbind(house_90_rep)
senate_90_age_grouped <- senate_90_no_missing %>%
  sort_by_age_group() %>%
  cbind(senate_90_ppl)

# Stores dotplots highlighting the age groups of representatives
house_116_age <- ggplot(house_116_age_grouped,
                        aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of Reps by Age in 116th House",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
senate_116_age <- ggplot(senate_116_age_grouped,
                         aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of Senators by Age in 116th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
house_90_age <- ggplot(house_90_age_grouped,
                       aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of Reps by Age in 90th House",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
senate_90_age <- ggplot(senate_90_age_grouped,
                        aes(x = x, y = y, color = .)) +
  geom_point() +
  scale_color_manual(values = c("green", "black")) +
  labs(title = "SVD of Senators by Age in 90th Senate",
       x = "Bipartisan Coordinate",
       y = "Partisan Coordinate") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plots all four graphs in a 2x2 fashion
grid.arrange(house_90_age, house_116_age, senate_90_age,
             senate_116_age, nrow = 2, ncol = 2)