# Load necessary libraries
library(maps)
library(mapproj)
library(ggplot2)
library(readr)
library(dplyr)
library(tools)

# Load US states map data
us.states <- map_data("state")
us.states <- rename(us.states, state = region)
us.states$state <- toTitleCase(us.states$state)  # Standardize state names

# Read election data
us.election <- read.csv("us_election_results_2016.csv")

# Merge state data with election data
merged.data <- merge(us.states, us.election, by = "state")

# Create the base map
base_map <- ggplot(data = merged.data, aes(x = long, y = lat, group = state)) +
  geom_polygon(fill = "white", color = "black", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void()

# Map showing the winner by state with state names
winner_map <- base_map +
  geom_polygon(aes(fill = winner), color = "black", linewidth = 0.1) +
  scale_fill_manual(values = c("blue", "red"), name = "Winner") +
  theme(legend.position = "bottom") +
  labs(title = "2016 US Presidential Election Results by State",
       subtitle = "States won by Hillary Clinton (Blue) and Donald Trump (Red)")

# Add state names
winner_map <- winner_map + 
  geom_text(data = merged.data %>% 
              group_by(state) %>% 
              summarize(long = mean(long), lat = mean(lat)), # Calculate the centroid of each state
            aes(label = state), 
            size = 3, 
            color = "black") + 
  theme(legend.position = "bottom")

# Print the map
print(winner_map)



# Map showing the percentage of votes for Trump
trump_percentage_map <- base_map +
  geom_polygon(aes(fill = pct_trump), color = "black", linewidth = 0.1) +
  scale_fill_gradient(low = "pink", high = "red4", name = "Trump Vote %") +
  labs(title = "Percentage of Votes for Donald Trump by State",
       subtitle = "Visualizing Trump's Vote Percentage across States")

# Save the Trump percentage map
ggsave("trump_percentage_map.png", plot = trump_percentage_map, width = 10, height = 6)

str(merged.data)
# Additional Analysis: Summary statistics
# Summarize election results by winner
summary_stats <- merged.data %>%
  group_by(winner) %>%
  summarise(avg_pct_trump = mean(pct_trump, na.rm = TRUE),
            total_states = n(),
            total_votes = sum(total_vote, na.rm = TRUE),  # Total votes cast
            avg_pct_clinton = mean(pct_clinton, na.rm = TRUE))  # Average percentage for Clinton

# Print the summary statistics to check results
print(summary_stats)

total_votes_by_state <- merged.data %>%
  select(state, trump_vote, clinton_vote, johnson_vote, other_vote) %>%
  gather(key = "candidate", value = "votes", -state)

p1 <- ggplot(total_votes, aes(x = reorder(state, -votes), y = votes, fill = candidate)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Total Votes by Candidate per State (2016 Election)",
       x = "State",
       y = "Number of Votes",
       fill = "Candidate") +
  theme_minimal()

# Print the plot
print(p1)

# 2. Vote Percentage Comparison as a Stacked Bar Plot
vote_percentage_comparison <- merged.data %>%
  select(state, pct_trump, pct_clinton, pct_johnson, pct_other) %>%
  gather(key = "candidate", value = "percentage", -state)

p2 <- ggplot(vote_percentage_comparison, aes(x = reorder(state, -percentage), y = percentage, fill = candidate)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked percentage bars
  coord_flip() +
  labs(title = "Vote Percentage Distribution by Candidate per State (2016 Election)",
       x = "State",
       y = "Percentage of Votes",
       fill = "Candidate") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Print the plot
print(p2)

# 3. Map Visualization of Trump Vote Percentage
map_trump_pct <- ggplot(data = merged.data, aes(x = long, y = lat, group = state, fill = pct_trump)) +
  geom_polygon(color = "black", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Trump Vote Percentage") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  theme(legend.position = "bottom")

# Print the map plot
print(map_trump_pct)

# Display all plots
print(winner_map)
print(trump_percentage_map)
print(avg_trump_plot)
print(histogram_plot)
print(scatter_plot)


# 1. Total Votes by State for Each Candidate
total_votes_by_state <- merged.data %>%
  select(state, trump_vote, clinton_vote, johnson_vote, other_vote) %>%
  gather(key = "candidate", value = "votes", -state)

p_1 <- ggplot(total_votes_by_state, aes(x = reorder(state, -votes), y = votes, fill = candidate)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Total Votes by Candidate per State (2016 Election)",
       x = "State",
       y = "Number of Votes",
       fill = "Candidate") +
  theme_minimal()

# Print the plot
print(p_1)

# 2. Vote Percentage Comparison as a Stacked Bar Plot
vote_percentage_comparison <- merged.data %>%
  select(state, pct_trump, pct_clinton, pct_johnson, pct_other) %>%
  gather(key = "candidate", value = "percentage", -state)

p2 <- ggplot(vote_percentage_comparison, aes(x = reorder(state, -percentage), y = percentage, fill = candidate)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked percentage bars
  coord_flip() +
  labs(title = "Vote Percentage Distribution by Candidate per State (2016 Election)",
       x = "State",
       y = "Percentage of Votes",
       fill = "Candidate") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Print the plot
print(p2)

# 3. Map Visualization of Trump Vote Percentage
map_trump_pct <- ggplot(data = merged.data, aes(x = long, y = lat, group = state, fill = pct_trump)) +
  geom_polygon(color = "black", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Trump Vote Percentage") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  theme(legend.position = "bottom")

# Print the map plot
print(map_trump_pct)
