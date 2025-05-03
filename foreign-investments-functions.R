#' Create a Bar Plot of Investments by Country
#'
#' This function generates a horizontal bar plot showing the top `n` countries
#' based on their investment values for a specified year and metric.
#'
#' @param data A data frame containing investment data.
#' @param year A numeric value representing the year for which investments are visualized.
#' @param metric A character string specifying the type of investment metric (e.g., "Total", "Direct").
#' @param top_n An integer specifying the number of top countries to include in the plot.
#' 
options(scipen = 999)
#' @return A ggplot object representing the bar plot.
#' @export
#' @examples
#' create_bar_plot(data, 2023, "Total", 10)
create_bar_plot <- function(data, year, metric, top_n) {
  # Dynamically create the metric column name
  metric_col <- paste(year, metric, sep = "_")
  
  # Ensure the metric column exists in the data
  if (!metric_col %in% colnames(data)) {
    stop(paste("Column", metric_col, "not found in dataset"))
  }
  
  # Filter and process data for the bar plot
  filtered_data <- data %>%
    filter(!is.na(Country), Country != "Total investment") %>%
    arrange(desc(!!sym(metric_col))) %>%
    slice_head(n = top_n) %>%
    select(Country, !!sym(metric_col)) %>%
    rename(Investment = !!sym(metric_col))
  
  # Create the bar plot
  ggplot(filtered_data, aes(x = reorder(Country, Investment), y = Investment, fill = Investment > 0)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Countries by", metric, "Investments in", year),
      subtitle = paste("Net", metric, "Investment Contributions and Withdrawals by Country"),
      x = "",
      y = "Investment Value (Million Drams)"
    ) +
    scale_fill_manual(
      values = c("red", "blue"),
      name = "Investment Activity",
      labels = c("Negative FDI Flows", "Positive FDI Flows")) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 5, hjust = 1, angle = 0), 
      axis.text.x = element_text(size = 10),
      legend.position = "bottom",
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 10, face = "italic"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 15)
    ) +
    guides(fill = guide_legend(title = "Investment Activity"))
}

#' Create a World Map of Investment Distribution
#'
#' Visualizes investments on a global map for a given year, metric, and investment type.
#' 
#' @param data A data frame containing investment data with country-level metrics.
#' @param year A numeric value representing the year for which investments are visualized.
#' @param metric A character string specifying the type of investment metric (e.g., "Total", "Direct").
#' @param investment_type A character string specifying the type of investments to visualize ("Positive" or "Negative").
#' 
#' @return A ggplot object representing the world map visualization.
#' @export
library(plotly)
library(ggthemes)

create_world_map <- function(data, year, metric, investment_type) {
  # Dynamically create the metric column name
  metric_col <- paste(year, metric, sep = "_")
  
  if (!metric_col %in% colnames(data)) {
    stop(paste("Column", metric_col, "not found in dataset"))
  }
  
  # Load world map data
  world_map <- map_data("world")
  
  # Process investment data
  map_data <- data %>%
    filter(!is.na(!!sym(metric_col))) %>%
    filter(if (investment_type == "Positive") !!sym(metric_col) > 0 else !!sym(metric_col) < 0) %>%
    select(region, !!sym(metric_col)) %>%
    rename(Investment = !!sym(metric_col))
  
  # Merge investment data with world map data
  merged_data <- world_map %>%
    left_join(map_data, by = "region")
  
  # Create the ggplot object
  gg <- ggplot(data = merged_data, aes(x = long, y = lat, group = group, fill = Investment)) +
    geom_polygon(color = "gray95", size = 0.3) +
    scale_fill_gradient(
      low = if (investment_type == "Positive") "lightblue" else "red2",
      high = if (investment_type == "Positive") "darkblue" else "pink",
      na.value = "gray85"
    ) +
    theme_minimal() +
    labs(
      title = if (investment_type == "Positive") 
        paste("Global Contributions of", metric, "Inv.s in", year, "(millions AMD)") 
      else 
        paste("Global Withdrawals of", metric, "Inv.s in", year, "(millions AMD)"),
      subtitle = if (investment_type == "Positive") 
        "Countries with positive foreign investment values (contributions)" 
      else 
        "Countries with negative foreign investment values (withdrawals)",
      x = NULL, y = NULL
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "italic"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"  # Completely remove the legend
    )
  
  # Convert to plotly without a legend
  ggplotly(gg)
}

