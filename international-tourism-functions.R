library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)

#' Load Cleaned Tourism Data
#'
#' @param file_path A character string specifying the path to the Excel file.
#' @return A data frame containing the cleaned tourism data with a `Date` column.
load_cleaned_tourism_data <- function(file_path) {
  tourism_data <- read_excel(file_path) %>%
    mutate(
      Date = as.Date(paste0(substr(Quarter, 1, 4), "-", 
                            as.numeric(substr(Quarter, 6, 6)) * 3 - 2, "-01")),
      Visitors = as.numeric(Visitors),
      Outbounds = as.numeric(Outbounds)
    )
  return(tourism_data)
}

#' Create a Stacked Bar Chart of Tourism Data
#'
#' @param data A data frame containing tourism data with `Date`, `Visitors`, and `Outbounds` columns.
#' @return A `plotly` object representing the interactive stacked bar chart.
create_stacked_bar_chart <- function(data) {
  plot <- ggplot(data, aes(x = Date)) +
    geom_bar(aes(y = Visitors, fill = "Inbound"), stat = "identity") +
    geom_bar(aes(y = Outbounds, fill = "Outbound"), stat = "identity") +
    labs(
      title = "Number of Inbound and Outbound Visitors",
      x = "Quarter",
      y = "Value",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.title = element_blank()
    )
  plotly::ggplotly(plot, tooltip = c("y", "x"))
}
