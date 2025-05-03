#' Create a Balance of Payments Summary Bar Plot
#'
#' This function generates a bar plot summarizing the Balance of Payments data, 
#' showing debts owed to and by Armenia for each year, grouped by main category and debt type.
#'
#' @param data A data frame containing Balance of Payments data with columns `Year`, `Value`, and `Main_Category`.
#' @return A ggplot object representing the bar plot of Balance of Payments data.
#' @export
#' @examples
#' create_balance_of_payments_plot(balance_data)
create_balance_of_payments_plot <- function(data) {
  # Prepare the summarized dataset
  Balance_of_Payments_of_RA_summary <- data %>%
    filter(!is.na(Value)) %>%
    mutate(
      Debt_Type = ifelse(Value < 0, "Owed to Armenia", "Owed by Armenia"),
      Year = factor(Year, levels = 2020:2024)  # Ensure Year is ordered from 2020 to 2024
    ) %>%
    group_by(Year, Debt_Type, Main_Category) %>%
    summarize(Total_Value = sum(Value), .groups = "drop")
  
  # Check if there are valid categories
  if (nrow(Balance_of_Payments_of_RA_summary) == 0) {
    return(NULL)  # Return NULL if no data
  }
  
  # Add position adjustments for labels
  Balance_of_Payments_of_RA_summary <- Balance_of_Payments_of_RA_summary %>%
    group_by(Main_Category) %>%  # Group by facet
    mutate(
      Label_Pos = ifelse(
        Total_Value > 0, 
        Total_Value + 0.2 * max(Total_Value, na.rm = TRUE),  # Push above positive bars
        Total_Value - 0.6 * abs(min(Total_Value, na.rm = TRUE))  # Push below negative bars
      )
    ) %>%
    ungroup()
  
  # Create the ggplot
  plot <- ggplot(Balance_of_Payments_of_RA_summary, aes(
    x = Year,
    y = Total_Value,
    fill = Debt_Type,
    text = paste(
      "Type:", Debt_Type,
      "<br>Value:", round(Total_Value, 2),  # Tooltip shows rounded value
      "<br>Year:", Year
    )
  )) +
    geom_bar(stat = "identity", position = "identity", width = 0.7) +
    geom_text(
      aes(label = round(Total_Value, 2), y = Label_Pos),  # Use dynamic positions
      color = "black",
      size = 3.5
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),  # Remove axis labels
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
      axis.text.y = element_text(size = 10), 
      legend.position = "bottom", 
      legend.title = element_blank(),  # Remove legend title
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank(),
      plot.title = element_blank()  # Remove plot title
    ) +
    facet_wrap(~ Main_Category, scales = "free_y") +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.4))) +
    scale_fill_manual(values = c("Owed to Armenia" = "#018571", "Owed by Armenia" = "#cc3333"), name = "")  # Optional: Custom colors
  
  return(ggplotly(plot, tooltip = "text"))
}










