# Data analysis of ARKIVO artifacts database table
# As part of PhD thesis:
# ARKIVO: An Information System for the Preservation of Networked Crypto Art
# by Daniel Filipe Farinha

# Data exported on 29 Aug 2024

library(readr)
library(ggplot2)
library(dplyr)
library(scales)


# import data from database dump
arkivo_data <- read_csv("arkivo_db_public_artifacts.csv")

# examine data structure
str(arkivo_data)

# convert into data frame
df <- as.data.frame(arkivo_data)

summary(df)

# exclude burned tokens
tokens <- df[df$is_burned == FALSE, ]
tokens$minted_at <- as.Date(tokens$minted_at)


# summarize data by date and status, then calculate the cumulative sum over time
cumulative_data <- tokens %>%
  group_by(minted_at, is_networked) %>%
  summarize(daily_size = sum(artifact_size, na.rm = TRUE)) %>%
  arrange(minted_at) %>%
  group_by(is_networked) %>%
  mutate(cumulative_size = cumsum(daily_size)) %>%
  ungroup()

# convert cumulative_size from bytes to GB
cumulative_data$cumulative_size <- cumulative_data$cumulative_size / (1024^3)

# label the status for readability
cumulative_data$is_networked <- ifelse(cumulative_data$is_networked, "Networked", "Non-Networked")

# create a stacked area plot
ggplot(cumulative_data, aes(x = minted_at, y = cumulative_size, fill = is_networked)) +
  geom_area() +
  labs(title = "IPFS Storage Over Time (Code-Based OBJKTs)",
       x = "Date",
       y = "Cumulative Size (GB)",
       fill = "Status") +
  scale_fill_manual(values = c("Networked" = "blue", "Non-Networked" = "orange")) +
  theme_minimal()


# create a data frame summarizing the counts of networked and non-networked tokens
networked_summary <- data.frame(
  Status = c("Networked", "Non-Networked"),
  Count = c(sum(tokens$is_networked == TRUE), sum(tokens$is_networked == FALSE))
)

# calculate the percentage for each category
networked_summary$Percentage <- (networked_summary$Count / sum(networked_summary$Count)) * 100

# create the pie chart with labels including both count and percentage
ggplot(networked_summary, aes(x = "", y = Count, fill = Status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "ARKIVO: Networked vs Non-Networked (Number of Tokens)") +
  geom_text(aes(label = paste0(Count, " \n(", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  scale_fill_manual(values = c("Networked" = "blue", "Non-Networked" = "orange"))



# summarize the total artifact size in bytes for networked and non-networked tokens, then convert to GB
artifact_size_summary <- data.frame(
  Status = c("Networked", "Non-Networked"),
  TotalSizeGB = c(sum(tokens$artifact_size[tokens$is_networked == TRUE]) / (1024^3), 
                  sum(tokens$artifact_size[tokens$is_networked == FALSE]) / (1024^3))
)

# calculate the percentage for each category
artifact_size_summary$Percentage <- (artifact_size_summary$TotalSizeGB / sum(artifact_size_summary$TotalSizeGB)) * 100

# create the pie chart with labels including both size in GB and percentage on separate lines
ggplot(artifact_size_summary, aes(x = "", y = TotalSizeGB, fill = Status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "ARKIVO: Networked vs Non-Networked (Total Artifact Size in GB)") +
  geom_text(aes(label = paste0(round(TotalSizeGB, 2), " GB\n(", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  scale_fill_manual(values = c("Networked" = "blue", "Non-Networked" = "orange"))

