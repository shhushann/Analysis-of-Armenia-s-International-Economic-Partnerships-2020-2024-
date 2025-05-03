#' Load and Preprocess Turnover Data
#'
#' This function reads and preprocesses turnover data from an Excel file. It removes unnecessary rows,
#' renames columns, and converts relevant fields to appropriate data types.
#'
#' @param file_path A character string specifying the path to the Excel file containing turnover data.
#' @return A data frame containing cleaned and preprocessed turnover data with columns `Date`, `TotalBuySellTurnover`, and `RepoAgreementsTurnover`.
#' @export
#' @examples
#' turnover_data <- load_and_preprocess_turnover_data("path/to/turnover_data.xlsx")
load_and_preprocess_turnover_data <- function(file_path) {
  turnover_data <- read_excel(file_path)
  turnover_data <- turnover_data[-c(1, 2), ]
  colnames(turnover_data) <- c("Date", "TotalBuySellTurnover", "RepoAgreementsTurnover")
  
  turnover_data <- turnover_data %>%
    mutate(
      Date = as.Date(paste0(substr(Date, 1, 4), "-", substr(Date, 6, 7), "-01")),
      TotalBuySellTurnover = as.numeric(gsub(",", "", TotalBuySellTurnover)),
      RepoAgreementsTurnover = as.numeric(gsub(",", "", RepoAgreementsTurnover))
    )
  
  return(turnover_data)
}

#' Generate Statistical Summary for a Metric
#'
#' This function calculates statistical summaries (e.g., mean, median, min, max) for a specified metric in the dataset.
#'
#' @param data A data frame containing turnover data.
#' @param metric A character string specifying the name of the column to summarize.
#' @return A summary object containing statistical metrics for the specified column.
#' @export
#' @examples
#' summary_statistics(turnover_data, "TotalBuySellTurnover")
summary_statistics <- function(data, metric) {
  summary(data[[metric]])
}

#' Create a Time-Series Plot for Turnover Data
#'
#' This function generates a time-series plot for a specified turnover metric, with an optional trend line.
#'
#' @param data A data frame containing turnover data.
#' @param metric A character string specifying the column name to visualize (e.g., "TotalBuySellTurnover").
#' @param show_trend A logical value indicating whether to include a LOESS trend line (default is `TRUE`).
#' @return A ggplot object representing the time-series plot.
#' @export
#' @examples
#' create_time_series_plot(turnover_data, "TotalBuySellTurnover", show_trend = TRUE)
create_time_series_plot <- function(data, metric, show_trend = TRUE) {
  plot <- ggplot(data, aes(x = Date, y = .data[[metric]])) +
    geom_line(color = "#018571") +
    geom_point(color = "darkblue") +
    labs(
      title = paste(metric, "in millions AMD"),
      x = NULL,  # Remove x-axis label
      y = NULL   # Remove y-axis label
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),  # Ensure x-axis title is blank
      axis.title.y = element_blank(),  # Ensure y-axis title is blank
      axis.text.x = element_text(size = 10),  # Customize axis text size if needed
      axis.text.y = element_text(size = 10)   # Customize axis text size if needed
    )
  
  if (show_trend) {
    plot <- plot + geom_smooth(method = "loess", se = FALSE, color = "#cc0066")
  }
  
  return(plot)
}

