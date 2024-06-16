library(tidyverse)

Barplot <- function(combine_df, Title, Subtitle, xaxis_size = 5, xaxis_rows = 3) {

    # This removes the ugly summarise warning...
    options(dplyr.summarise.inform=F)

    # Calculate average ladder scores per region
    bar_df <- combine_df %>%
        group_by(`Regional indicator`) %>%
        summarise_at( vars( c(`Ladder score`, starts_with("Explained"), `Dystopia + residual`)), ~mean(.)) %>%
        gather(Score, Value, -`Regional indicator`, -`Ladder score`) %>%
        mutate(Score = gsub("Explained by: ", "", Score))


    # Extract South Africa's data
    SA_df <- combine_df %>%
        filter(`Country name` == "South Africa") %>% select(c(`Regional indicator`, `Ladder score`, starts_with("Explained"), `Dystopia + residual`)) %>% mutate(`Regional indicator` = "South Africa") %>%
        gather(Score, Value, -`Regional indicator`, -`Ladder score`) %>%
        mutate(Score = gsub("Explained by: ", "", Score))

    # See this makes your life much easier:
    barplot_df <- bind_rows(bar_df,SA_df)

    # Arrange the Score column for consistency as from highest avg to lowest:
    order1 <- c("South Africa",
                combine_df %>% group_by(`Regional indicator`) %>% summarise(Lad = mean(`Ladder score`)) %>% arrange(Lad) %>% pull(`Regional indicator`))
    order2 <- barplot_df %>% group_by(Score) %>% summarise(Avg = mean(Value)) %>% arrange(Avg) %>% pull(Score)

    # Create the barplot
    g1 <- barplot_df %>%
        arrange_data(., Column = "Regional indicator", Order = order1) %>%
        arrange_data(., Column = "Score", Order = order2) %>%
        ggplot() + geom_bar(aes(x = `Regional indicator`, y = Value, fill = Score), stat = "identity", position = "stack") +
        theme_bw() +
        scale_fill_brewer(palette="Dark2") +
        scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
        labs(title = Title, subtitle = Subtitle, caption = "Data source: World Happiness Index", x = "", y = "Breakdown of Happiness") +
        theme(legend.position = "top", legend.title = element_blank()) +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 12), axis.text.x = element_text(size = xaxis_size)) +
        guides(fill = F)

    return(g1)
}
