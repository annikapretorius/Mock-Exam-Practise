# Calculate the average Healthy Life Expectancy per region
average_life_expectancy <- df %>%
    group_by(Region) %>%
    summarize(avg_life_expectancy = mean(`Healthy life expectancy`, na.rm = TRUE))