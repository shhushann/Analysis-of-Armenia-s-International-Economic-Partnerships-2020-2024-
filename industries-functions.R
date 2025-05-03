library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

#' @title Data Transformation and Analysis Functions
#' @description This script contains functions for reshaping, aggregating, and analyzing the "Industries of RA" dataset.

#' @title Reshape Data
#' @description Converts the dataset into a long format with proper date formatting.
#' @param data A data frame containing the cleaned Industries of RA dataset.
#' @return A long-format data frame with columns: Category, Date, Value, and Year.
reshape_and_aggregate_data <- function(data) {
  # Updated mapping based on provided categories and industries
  category_mapping <- list(
    "Primary Industries" = c(
      "Mining and quarrying", "mining of metal ores", "other mining and quarrying", 
      "mining support service activities"
    ),
    "Food and Beverages" = c(
      "manufacture of food products", "manufacture of beverages", "manufacture of tobacco products"
    ),
    "Textiles and Apparel" = c(
      "manufacture of textiles", "manufacture of wearing apparel", 
      "manufacture of leather and related products"
    ),
    "Wood and Paper Products" = c(
      "manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials",
      "manufacture of paper and paper products", "printing and reproduction of recorded media"
    ),
    "Chemical Products" = c(
      "manufacture of coke and refined petroleum products", 
      "manufacture of chemicals and chemical products", 
      "manufacture of basic pharmaceutical products and pharmaceutical preparations"
    ),
    "Rubber, Plastics, and Non-Metallic Products" = c(
      "manufacture of rubber and plastic products", 
      "manufacture of other non-metallic mineral products"
    ),
    "Metal and Machinery Manufacturing" = c(
      "manufacture of basic metals", "manufacture of fabricated metal products, except machinery and equipment",
      "manufacture of machinery and equipment n.e.c."
    ),
    "Electronics and Electrical Equipment" = c(
      "manufacture of computer, electronic and optical products", 
      "manufacture of electrical equipment"
    ),
    "Vehicles and Accessories" = c(
      "manufacture of motor vehicles, trailers and semi-trailers",
      "manufacture of motor vehicles",
      "manufacture of bodies (coachwork) for motor vehicles; manufacture of trailers and semitrailers",
      "manufacture of other parts and accessories for motor vehicles"
    ),
    "Other Manufacturing" = c(
      "manufacture of furniture", "other manufacturing", "jewellery manufacture"
    ),
    "Other Services" = c(
      "repair and installation of machinery and equipment"
    ),
    "Energy and Utilities" = c(
      "Electricity, gas, steam and air conditioning supply", 
      "production and distribution of electricity", "trade of gas through mains"
    ),
    "Water Supply and Environmental Services" = c(
      "Water supply, sewerage, waste management and remediation activities", 
      "collection, purification and distribution of water", "sewerage",
      "waste collection, treatment and disposal activities; materials recovery"
    )
  )
  
  if (!inherits(data, "data.frame")) {
    data <- as.data.frame(data)
  }
  
  # Reshape data into long format
  data_long <- data %>%
    tidyr::pivot_longer(
      cols = starts_with("202"),  # Adjust column selection based on actual dataset
      names_to = "Date",
      values_to = "Value"
    ) %>%
    mutate(
      Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"),
      Year = lubridate::year(Date),
      Value = gsub(",", "", Value),  # Remove commas
      Value = gsub("[^0-9.-]", "", Value),  # Remove non-numeric characters
      Value = suppressWarnings(as.numeric(Value))  # Convert to numeric
    ) %>%
    filter(!is.na(Value))  # Remove rows with NA values in Value
  
  
  # Map categories
  data_long <- data_long %>%
    mutate(Category = case_when(
      Industry %in% category_mapping$`Primary Industries` ~ "Primary Industries",
      Industry %in% category_mapping$`Food and Beverages` ~ "Food and Beverages",
      Industry %in% category_mapping$`Textiles and Apparel` ~ "Textiles and Apparel",
      Industry %in% category_mapping$`Wood and Paper Products` ~ "Wood and Paper Products",
      Industry %in% category_mapping$`Chemical Products` ~ "Chemical Products",
      Industry %in% category_mapping$`Rubber, Plastics, and Non-Metallic Products` ~ "Rubber, Plastics, and Non-Metallic Products",
      Industry %in% category_mapping$`Metal and Machinery Manufacturing` ~ "Metal and Machinery Manufacturing",
      Industry %in% category_mapping$`Electronics and Electrical Equipment` ~ "Electronics and Electrical Equipment",
      Industry %in% category_mapping$`Vehicles and Accessories` ~ "Vehicles and Accessories",
      Industry %in% category_mapping$`Other Manufacturing` ~ "Other Manufacturing",
      Industry %in% category_mapping$`Other Services` ~ "Other Services",
      Industry %in% category_mapping$`Energy and Utilities` ~ "Energy and Utilities",
      Industry %in% category_mapping$`Water Supply and Environmental Services` ~ "Water Supply and Environmental Services",
      TRUE ~ "Other"
    )) %>%
    drop_na(Category)  # Remove rows with NA values in Category
  
  return(data_long)
}

#' @title Aggregate Yearly Data
#' @description Aggregates the total value by category and year.
#' @param data A long-format data frame.
#' @return A data frame with columns: Category, Year, Total_Value.
aggregate_yearly_data <- function(data) {
  data %>%
    group_by(Category, Year) %>%
    summarize(Total_Value = sum(Value, na.rm = TRUE), .groups = 'drop')
}

#' @title Get Top Categories by Year
#' @description Identifies the top 10 categories by year, excluding specified categories.
#' @param data A data frame containing yearly aggregated data.
#' @param excluded_categories A vector of categories to exclude.
#' @return A data frame with columns: Category, Year, Total_Value, Rank.
get_top_categories <- function(data, excluded_categories = NULL) {
  data <- if (!is.null(excluded_categories)) {
    data %>% filter(!Category %in% excluded_categories)
  } else {
    data
  }
  
  data %>%
    group_by(Year) %>%
    arrange(Year, desc(Total_Value)) %>%
    mutate(Rank = row_number()) %>%
    filter(Rank <= 10)
}

#' @title Plotly Visualization for Top Categories
#' @description Creates an interactive bar chart for the top categories by year.
#' @param data A data frame containing top categories with their total values.
#' @param year The year for which the plot is generated.
#' @return A Plotly object.
plot_top_categories <- function(data, year) {
  filtered_data <- data %>% filter(Year == year)
  if (nrow(filtered_data) == 0) {
    return(plot_ly() %>% layout(title = "No data available for the selected year"))
  }
  
  plot_ly(
    data = filtered_data,
    x = ~reorder(Category, -Total_Value),
    y = ~Total_Value,
    type = 'bar',
    marker = list(color = 'rgba(58, 71, 80, 0.6)', line = list(color = 'rgba(58, 71, 80, 1.0)', width = 2))
  ) %>%
    layout(
      title = paste("Top Industries in", year, "in millions AMD"),
      xaxis = list(title = "Category", tickangle = -45),
      yaxis = list(title = "Total Value"),
      margin = list(b = 100)
    )
}

#' @title Calculate Category Share
#' @description Computes the share of each category in the total industry value for each year.
#' @param top_categories A data frame of top categories.
#' @param yearly_totals A data frame with total industry values by year.
#' @return A data frame with columns: Category, Year, Total_Value, Year_Total, Share.
calculate_category_share <- function(top_categories, yearly_totals) {
  top_categories %>%
    left_join(yearly_totals, by = "Year") %>%
    mutate(Share = Total_Value / Year_Total)
}
