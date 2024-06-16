Barplot_Ratings <- function(dff_adj, Title, Subtitle, xaxis_size = 5, xaxis_rows = 3){

df_adj <- dff_adj %>% group_by(country) %>% summarise(num_ratings = sum(!is.na(points)),  # Count the number of non-NA points
                                                      median_score = median(points, na.rm = TRUE))  # Calculate the median score from the points column
g <- df_adj %>% ggplot(country, aes(x = reorder(country, -num_ratings), y = num_ratings)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(median_score, 1), y = num_ratings + 5), vjust = -0.5) +
    theme_minimal() +
    labs(title = "Number of Ratings per Country",
         x = "Country",
         y = "Number of Ratings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

g

}
