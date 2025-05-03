#' @title Dollarization Level Data Analysis Functions
#' @description This script contains functions to load and preprocess the dollarization level dataset, 
#' generate summaries, and create visualizations such as line graphs and Sankey diagrams.

# Load required libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(networkD3)
library(plotly)

#' @title Load and Preprocess Data
#' @description Reads the cleaned dollarization dataset, processes the Date column, and adds a Year column.
#' @param file_path A string specifying the path to the cleaned dataset.
#' @return A data frame with the processed dataset.
load_and_preprocess_data <- function(file_path) {
  data <- read.xlsx(file_path) %>%
    select(-1) %>%
    mutate(
      Date = as.Date(Date, origin = "1899-12-30"),
      Year = format(Date, "%Y")
    )
  return(data)
}

#' @title Filter Dollarization Data by Category
#' @description Filters the dollarization dataset based on selected categories.
#' @param data A data frame containing the dollarization dataset.
#' @param categories A character vector of selected categories.
#' @return A filtered data frame containing only the selected categories.
filter_dollarization_by_category <- function(data, categories) {
  filtered_data <- data %>%
    filter(Category %in% categories)
  return(filtered_data)
}


#' @title Summarize Dollarization Levels
#' @description Aggregates the total dollarization values by year and category.
#' @param data A data frame containing the processed dollarization dataset.
#' @return A summarized data frame with columns: Year, Category, TotalValue.
summarize_dollarization <- function(data) {
  summary <- data %>%
    group_by(Year, Category) %>%
    summarize(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")
  return(summary)
}

#' @title Create Nodes for Sankey Diagram
#' @description Creates nodes for the Sankey diagram using unique years and categories.
#' @param summary A summarized data frame containing the dollarization data.
#' @return A data frame with a single column `name` for the nodes.
create_nodes <- function(summary) {
  nodes <- data.frame(
    name = c(unique(summary$Year), unique(summary$Category))
  )
  return(nodes)
}

#' @title Create Links for Sankey Diagram
#' @description Creates links for the Sankey diagram by connecting years and categories based on dollarization values.
#' @param summary A summarized data frame containing the dollarization data.
#' @return A data frame with columns: source, target, and value.
create_links <- function(summary) {
  links <- summary %>%
    mutate(
      source = as.numeric(factor(Year, levels = unique(summary$Year))) - 1,
      target = as.numeric(factor(Category, levels = unique(summary$Category))) + length(unique(summary$Year)) - 1
    ) %>%
    select(source, target, value = TotalValue)
  return(as.data.frame(links))
}

#' @title Plot Dollarization Over Time (Interactive with Plotly)
#' @description Creates an interactive line graph to visualize the dollarization level over time by category.
#' @param data_long A data frame in long format containing dollarization levels over time.
#' @return A plotly object representing the interactive line graph.
plot_dollarization_over_time <- function(data_long) {
  # Ensure Date and Value are properly formatted
  data_long <- data_long %>%
    mutate(
      Date = as.Date(Date),
      Value = as.numeric(Value)
    )
  
  # Create an interactive line plot with plotly
  plotly::plot_ly(
    data = data_long,
    x = ~Date,
    y = ~Value,
    color = ~Category,
    type = 'scatter',
    mode = 'lines+markers',
    hoverinfo = 'text',
    text = ~paste(
      "Category:", Category,
      "<br>Date:", Date,
      "<br>Value:", round(Value, 2)  # Round Value to 2 decimal points
    )
  ) %>%
    layout(
      title = "Dollarization Level of RA Over Time",
      xaxis = list(title = ""),  # Remove x-axis label
      yaxis = list(title = "Dollarization Level (%)"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
    )
}


#' @title Create Sankey Diagram
#' @description Generates a Sankey diagram to visualize connections between years and categories based on dollarization values.
#' @param nodes A data frame containing the nodes for the Sankey diagram.
#' @param links A data frame containing the links for the Sankey diagram.
#' @return A Sankey diagram object created with `networkD3`.
create_sankey_diagram <- function(nodes, links) {
  sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    fontSize = 12,
    nodeWidth = 30,
    units = "Total Value"
  )
}
