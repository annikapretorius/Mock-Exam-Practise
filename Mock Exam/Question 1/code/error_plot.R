Error_plot <-  function(df, xaxis_size = 5, xaxis_rows = 3){

df_plot <- df %>%
        group_by(`Regional indicator`) %>%
        summarise_at( vars(c(`Ladder score`, ends_with("whisker")) ), ~median(.))

# Life expectancy
HE <-
    df %>% group_by(`Regional indicator`) %>% summarise_at(vars(`Healthy life expectancy`), ~median(.)) %>%
    # For ease of naming:
    rename(HE = `Healthy life expectancy`) %>% mutate(HE = round(HE, 1))

# Join LE to plot, so that we have y-coordinates for LE label:
dfplot <-
    left_join(dfplot,
              HE,
              by = "Regional indicator")

# Adjust ordering as per gist:
order <- dfplot %>% arrange(HE) %>% pull(`Regional indicator`)
dfplot <- dfplot %>% arrange_data(., Column = "Regional indicator", Order = order)



}