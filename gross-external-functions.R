#' @title Load and Preprocess Gross External Debt Data
#' @description Reads the cleaned gross external debt dataset, processes the Date column, and adds a Year column.
#' @param file_path A string specifying the path to the cleaned dataset.
#' @return A data frame with the processed dataset.
load_and_preprocess_gross_external_debt <- function(file_path) {
  data <- read.xlsx(file_path) %>%
    mutate(
      Date = as.Date(Date, origin = "1899-12-30"),
      Year = format(Date, "%Y")
    )
  return(data)
}

library(ggthemes)
library(plotly)

#' @title Filter Gross External Debt Data by Type
#' @description Filters the gross external debt dataset based on selected debt types.
#' @param data A data frame containing the gross external debt dataset.
#' @param types A character vector of selected debt types.
#' @return A filtered data frame containing only the selected types.
filter_gross_external_debt_by_type <- function(data, types) {
  filtered_data <- data %>%
    filter(Type %in% types)
  return(filtered_data)
}

#' @title Summarize Gross External Debt by Type and Time
#' @description Aggregates the total debt values by type and date.
#' @param data A data frame containing the processed gross external debt dataset.
#' @param types A character vector of selected debt types.
#' @return A summarized data frame with columns: Date, Type, TotalDebt.
summarize_gross_external_debt_by_type <- function(data, types) {
  summary <- data %>%
    filter(Type %in% types) %>%  # Filter for the specified types
    group_by(Date, Type) %>%
    summarize(TotalDebt = sum(Debt_Value, na.rm = TRUE), .groups = "drop")  # Summarize debt values
  return(summary)
}


#' @title Plot Gross External Debt Over Time
#' @description Creates an interactive line graph to visualize the total debt value over time by type.
#' @param data A summarized data frame containing gross external debt values over time.
#' @return A plotly object representing the interactive line graph.
plot_gross_external_debt_over_time <- function(data) {
  # Ensure Date and TotalDebt are properly formatted
  data <- data %>%
    mutate(
      Date = as.Date(Date),
      TotalDebt = as.numeric(TotalDebt)
    )
  
  # Create an interactive line plot with plotly
  plotly::plot_ly(
    data = data,
    x = ~Date,
    y = ~TotalDebt,
    color = ~Type,
    type = 'scatter',
    mode = 'lines+markers',
    hoverinfo = 'text',
    text = ~paste(
      "Type:", Type,
      "<br>Date:", Date,
      "<br>Total Debt:", round(TotalDebt, 2)
    )
  ) %>%
    layout(
      title = "Gross External Debt Over Time by Type (millions $USD)",
      xaxis = list(title = ""),  # Remove x-axis label
      yaxis = list(title = "Debt Value (in millions)"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
    )
}


#' Summarize Debt Data for a Sankey Diagram
#'
#' This function prepares the gross external debt data for visualization in a Sankey diagram,
#' grouping by type and detail.
#'
#' @param data A data frame containing gross external debt data.
#' @return A data frame with summarized debt values grouped by type and detail.
#' @export
#' @examples
#' debt_sankey <- summarize_debt_for_sankey(debt_data)
summarize_debt_for_sankey <- function(data) {
  debt_sankey <- data %>%
    filter(!is.na(Debt_Value), !is.na(Detail)) %>%
    group_by(Type, Detail) %>%
    summarise(Total_Debt = sum(as.numeric(Debt_Value), na.rm = TRUE)) %>%
    ungroup()
  return(debt_sankey)
}

#' Create a Sankey Diagram for Debt Visualization
#'
#' This function generates a Sankey diagram to visualize the flow of debt from types to details.
#'
#' @param debt_sankey A data frame containing summarized debt data prepared for the Sankey diagram.
#' @return A Sankey diagram object created using the `networkD3` package.
#' @export
#' @examples
#' create_sankey_diagram(debt_sankey)
create_sankey_diagram <- function(debt_sankey) {
  nodes <- unique(c(debt_sankey$Type, debt_sankey$Detail))
  nodes <- data.frame(name = nodes)
  
  links <- debt_sankey %>%
    mutate(
      source = match(Type, nodes$name) - 1,
      target = match(Detail, nodes$name) - 1
    ) %>%
    select(source, target, value = Total_Debt)
  
  sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    fontSize = 12,
    nodeWidth = 30,
    units = "USD"
  )
}


