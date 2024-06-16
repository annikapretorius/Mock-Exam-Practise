arrange_data <- function(combine_df, Column, Order){

    combine_df[,Column][[1]] <- factor(combine_df[,Column][[1]], levels = Order)

    combine_df

}
