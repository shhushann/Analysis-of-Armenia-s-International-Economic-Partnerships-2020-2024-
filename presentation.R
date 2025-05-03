library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(networkD3)
library(readxl)

# Source functions
source("../R/functions/dollarization-functions.R")
source("../R/functions/foreign-investments-functions.R")
source("../R/functions/gross-external-functions.R")
source("../R/functions/industries-functions.R")
source("../R/functions/international-tourism-functions.R")
source("../R/functions/payments-functions.R")
source("../R/functions/turnover-volumes.R")

# Load datasets
dollarization_data <- load_and_preprocess_data("../data/processed/CLEANED-DOLLARIZATION-LEVEL-OF-RA.xlsx")
foreign_data <- readxl::read_excel("../data/processed/CLEANED-FOREIGN-INVESTMENTS.xlsx", sheet = 1)
Gross_External_Debt <- load_and_preprocess_gross_external_debt("../data/processed/CLEANED-GROSS-EXTERNAL-DEBT.xlsx")
industries_data <- readxl::read_excel("../data/processed/CLEANED-INDUSTRIES-OF-RA.xlsx")
tourism_data <- load_cleaned_tourism_data("../data/processed/CLEANED-INTERNATIONAL-TOURISM-RA.xlsx")
industries_data <- readxl::read_excel("../data/processed/CLEANED-INDUSTRIES-OF-RA.xlsx")

# Update the UI
ui <- dashboardPage(
  dashboardHeader(
    title = span("RA's Economical Partnership Analysis 2020-2024", style = "font-size: 20px;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Dollarization", tabName = "dollarization", icon = icon("dollar-sign")),
      menuItem("Foreign Investments", tabName = "investments", icon = icon("globe")),
      menuItem("Gross External Debt", tabName = "external_debt", icon = icon("chart-line")),
      menuItem("Industries", tabName = "industries", icon = icon("industry")),
      menuItem("International Tourism", tabName = "tourism", icon = icon("plane")),
      menuItem("Balance of Payments", tabName = "balance_of_payments", icon = icon("balance-scale")),
      menuItem("Turnover Volumes", tabName = "turnover_volumes", icon = icon("chart-bar")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Change the header background color */
      .main-header {
        background-color: #343a40  !important;
      }
      .main-header .logo {
        background-color: #343a40 !important;
        width: 500px;
        color: white !important;
      }
      .main-header .navbar {
        margin-left: 500px;
        background-color: #015D4F !important;
      }
      /* Sidebar color customization */
      .main-sidebar {
        background-color: #015D4F !important; /* Default dark grey */
      }
      .sidebar-menu > li.active > a {
        background-color: #343a40 !important;
        color: white !important;
      }
      /* General text and hover styles */
      .sidebar-menu > li > a {
        color: white !important;
      }
      .sidebar-menu > li > a:hover {
        background-color: #018571 !important;
        color: white !important;
      }
    "))
    ),
    tabItems(
      #Introduction
      tabItem(
        tabName = "introduction",
        fluidRow(
          box(
            title = "Introduction",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            h3("Abstract"),
            p("This paper examines the evolution of Armenia’s international economic partnerships between 2020 and 2024, analyzing the extent to which these partnerships have diversified and intensified. The study also explores the role of Armenian citizens in influencing the country’s international economic relations. Through the use of key datasets, the research identifies patterns and interactions between foreign investments, dollarization, remittance flows, external trade, and gross external debt. The findings aim to highlight Armenia’s progress in expanding its global economic footprint while assessing the contributions of individual citizens to this growth."),
            h3("Introduction"),
            p("Armenia’s economy has undergone substantial changes from 2020 to 2024, driven by shifts in international partnerships, economic diversification, and geopolitical influences. This paper provides an in-depth analysis of key economic indicators, including foreign investments, dollarization levels, remittances, external trade, and gross external debt. By examining these factors, the study aims to uncover the hidden dynamics underlying Armenia’s economic transformation."),
            h3("Motivation"),
            p("Armenia’s evolving international partnerships between 2020 and 2024 have been shaped by complex political, social, and economic dynamics. These shifts have led to a noticeable diversification and intensification in collaborations with key global partners. Understanding the underlying patterns driving these changes is essential for assessing their long-term implications on Armenia’s economic resilience and growth."),
            h3("Literature Review"),
            p("The economic growth of Armenia from 2020 to 2024 has been significantly influenced by key variables such as foreign investments, dollarization, remittance flows, external trade, and gross external debt. Reports from the Central Bank of Armenia (CBA) and the Statistical Committee of Armenia provide essential baseline data, highlighting trends in dollarization, trade balances, and remittance inflows."),
            h3("Data Overview and Description"),
            tags$ul(
              tags$li("Balance of Payments of Armenia"),
              tags$li("Dollarization Level of Armenia"),
              tags$li("Gross External Debt"),
              tags$li("International Tourism in Armenia"),
              tags$li("Turnover Volumes of Investment Companies and Banks"),
              tags$li("Foreign Investments in Armenia"),
              tags$li("Industries of RA")
            ),
            h3("Hypotheses"),
            tags$ol(
              tags$li("To what extent have Armenia’s international economic partnerships diversified and intensified between 2020 and 2024?"),
              tags$li("Do citizens of Armenia play a significant role in shaping and influencing the country’s global economic relations?")
            )
          )
        )
      ),
      # Dollarization Tab
      tabItem(
        tabName = "dollarization",
        fluidRow(
          tags$style(HTML("
    /* Add padding around elements in the Dollarization tab */
    .dollarization-padding {
      padding-left: 20px;
      padding-right: 20px;
    }
    .box.box-solid>.box-header {
      background-color: #015D4F !important;
      color: white !important;
    }
    .annotation-box {
      background-color: #f7f7f7;
      border: 1px solid #ddd;
      padding: 10px;
      border-radius: 5px;
      margin-top: 20px;
    }
  ")),  
      # Custom CSS for header and annotation styling
          fluidRow(
            class = "dollarization-padding",
            column(
              width = 6,  # Full-width row for Controls box
              box(
                title = "Controls",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = NULL,  # Allow the box to use the full column width
                selectInput(
                  "category_select",
                  "Select Category:",
                  choices = NULL,  # Dynamically updated in server
                  selected = NULL,
                  multiple = TRUE
                ),
                sliderInput(
                  "year_range",
                  "Select Year Range:",
                  min = 2020,  # Adjust this to match your dataset's year range
                  max = 2024,  # Adjust this to match your dataset's year range
                  value = c(2020, 2024),  # Default to full range
                  sep = ""
                )
              )
            ),
            box(
              title = "Annotation",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 6,
              div(
                class = "annotation-box",
                uiOutput("dollarization_annotation")  # Dynamic UI output for category details
              )
            )
          ),
          fluidRow(
            class = "dollarization-padding",
            column(
              width = 6,  # Adjust width to make plots wider
              box(
                title = "Line Plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = NULL,  # Allow the plot to use the full column width
                plotlyOutput("dollarization_plot", height = "450px")
              )
            ),
            column(
              width = 6,  # Adjust width to make plots wider
              box(
                title = "Sankey Diagram",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = NULL,  # Allow the plot to use the full column width
                sankeyNetworkOutput("dollarization_sankey", height = "450px")
              )
            )
          )
        )
      ),
      # Foreign Investments Tab
      tabItem(
        tabName = "investments",
        fluidRow(
          # First Row: Controls for both maps
          column(
            width = 6,
            box(
              title = "Controls 1",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              selectInput(
                "investment_year1",
                "Select Year for Map 1:",
                choices = NULL,  # Dynamically updated in server
                selected = NULL
              ),
              selectInput(
                "investment_metric1",
                "Select Metric for Map 1:",
                choices = c("Total", "Direct"),
                selected = "Total"
              ),
              radioButtons(
                "investment_type1",
                "Select Investment Type for Map 1:",
                choices = c("Positive", "Negative"),
                selected = "Positive"
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Controls 2",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              selectInput(
                "investment_year2",
                "Select Year for Map 2:",
                choices = NULL,  # Dynamically updated in server
                selected = NULL
              ),
              selectInput(
                "investment_metric2",
                "Select Metric for Map 2:",
                choices = c("Total", "Direct"),
                selected = "Total"
              ),
              radioButtons(
                "investment_type2",
                "Select Investment Type for Map 2:",
                choices = c("Positive", "Negative"),
                selected = "Positive"
              )
            )
          )
        ),
        fluidRow(
          # Second Row: Visualizations for both maps
          column(
            width = 6,
            box(
              title = "World Map 1",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              plotlyOutput("investment_world_map1", height = "450px")
            )
          ),
          column(
            width = 6,
            box(
              title = "World Map 2",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              plotlyOutput("investment_world_map2", height = "450px")
            )
          )
        ),
        fluidRow(
          # Third Row: Annotation Box
          box(
            title = "Investment Metrics and FDI Explanation",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            div(
              class = "annotation-box",
              h4("Investment Metrics"),
              p("The Investment Metric refers to the type of financial data being analyzed for foreign investments:"),
              tags$ul(
                tags$li("Total: Represents the cumulative value of all investments (positive and negative) for the selected year."),
                tags$li("Direct: Refers to Foreign Direct Investment (FDI), which is the investment made by a firm or individual in one country into business interests in another country.")
              ),
              h4("Investment Type"),
              p("The Investment Type distinguishes between contributions and withdrawals of foreign investments:"),
              tags$ul(
                tags$li("Positive: Represents investment inflows, where foreign entities invest in Armenia's economy."),
                tags$li("Negative: Represents investment outflows or disinvestment, where previously invested capital is withdrawn.")
              ),
              h4("What is FDI?"),
              p("Foreign Direct Investment (FDI) is an investment made by a company or individual in one country into business interests located in another country. FDI typically involves establishing business operations or acquiring business assets, including ownership or controlling interest in a foreign company. It is a key indicator of economic globalization and a significant source of funding for emerging economies.")
            )
          )
        )
      ),
      # Gross External Debt Tab
      tabItem(
        tabName = "external_debt",
        tags$style(HTML("
    /* Add padding around elements in the Gross External Debt tab */
    .external-debt-padding {
      padding-left: 20px;
      padding-right: 20px;
    }
    .box.box-solid>.box-header {
      background-color: #015D4F !important;
      color: white !important;
    }
    .annotation-box {
      background-color: #f7f7f7;
      border: 1px solid #ddd;
      padding: 10px;
      border-radius: 5px;
      margin-top: 20px;
    }
  ")),
        fluidRow(
          class = "external-debt-padding",
          column(
            width = 6,  # Line plot on the right
            box(
              title = "Time Series Plot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,  # Allow the plot to use the full column width
              plotlyOutput("debt_plot", height = "450px")
            )
          ),
          column(
            width = 6,  # Sankey diagram on the right
            box(
              title = "Sankey Diagram",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,  # Allow the diagram to use the full column width
              sankeyNetworkOutput("sankey_plot", height = "450px")
            )
          )
        ),
        fluidRow(
          class = "external-debt-padding",
          column(
            width = 6,  # Controls on the left
            box(
              title = "Controls",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,  # Allow the box to use the full column width
              selectInput(
                "debt_types",
                label = "Select Debt Types:",
                choices = unique(Gross_External_Debt$Type),  # Populate with available debt types
                selected = unique(Gross_External_Debt$Type),  # Default: Select all types
                multiple = TRUE
              )
            )
          ),
          box(
            title = "Annotation",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            div(
              class = "annotation-box",
              h4("Debt Types Explanation"),
              p("This section provides a detailed explanation of the debt types used in the analysis. Each category represents specific financial liabilities, classified based on international accounting standards:"),
              tags$ul(
                tags$li("General Government: This includes debt liabilities of the government, such as bonds and loans, used for financing budget deficits and public projects."),
                tags$li("Central Bank: This category covers debts incurred by the Central Bank, including liabilities related to foreign exchange reserves and monetary policy operations."),
                tags$li("Deposit-Taking Corporations: This includes liabilities of banks and other financial institutions that accept deposits, excluding the Central Bank."),
                tags$li("Other Sectors: This refers to debts owed by entities outside the general government and financial sector, such as corporations and non-profit organizations."),
                tags$li("Direct Investment: These are cross-border liabilities associated with direct investments, including intercompany loans and equity-related obligations.")
              ),
              p("Understanding these debt types is crucial for analyzing the composition and trends of Armenia's gross external debt and its implications for economic stability.")
            )
          )
        )
      ),
      # Industries Tab
      tabItem(
        tabName = "industries",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "year_1",
              "Select Year 1:",
              choices = NULL,  # Dynamically updated in the server
              selected = NULL  # Default will be set dynamically
            )
          ),
          column(
            width = 4,
            selectInput(
              "year_2",
              "Select Year 2:",
              choices = NULL,  # Dynamically updated in the server
              selected = NULL  # Default will be set dynamically
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            sliderInput(
              "top_n_categories",
              "Select Number of Top Categories:",
              min = 5,
              max = 20,  # Adjust based on your dataset
              value = 10,  # Default value
              step = 1
            )
          )
        ),
        fluidRow(
          box(
            title = "Industry Analysis",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("industries_plot", height = "450px")
          )
        ),
        fluidRow(
          box(
            title = "Annotation",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            div(
              class = "annotation-box",
              h4("Industry Categories Overview"),
              uiOutput("category_annotation")  # Dynamic UI output for category details
            )
          )
        )
      ),
      # Tourism Tab
      tabItem(
        tabName = "tourism",
        fluidRow(
          box(
            title = "Controls",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            sliderInput(
              "year_range",
              "Select Year Range:",
              min = as.numeric(format(min(tourism_data$Date), "%Y")),
              max = as.numeric(format(max(tourism_data$Date), "%Y")),
              value = c(
                as.numeric(format(min(tourism_data$Date), "%Y")),
                as.numeric(format(max(tourism_data$Date), "%Y"))
              ),
              sep = ""
            ),
            selectInput(
              "tourism_metric",
              "Select Metric:",
              choices = c("Inbound" = "Visitors", "Outbound" = "Outbounds"),
              selected = "Visitors"
            )
          ),
          box(
            title = "Metric Summary",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            height = 235,
            tags$style(HTML("
    #tourism_summary {
      transform: scale(1.25);
      transform-origin: top left; /* Adjust the anchor point for scaling */
    }
  ")),
            tableOutput("tourism_summary")
          )
        ),
        fluidRow(
          box(
            title = "Tourism Data Visualization",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("tourism_bar_chart", height = "400px")
          )
        )
      ),
      # Balance of Payments Tab
      tabItem(
        tabName = "balance_of_payments",
        fluidRow(
          class = "balance-payments-padding",
          column(
            width = 6,  # Selector box
            box(
              title = "Controls",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,  # Allow the box to use the full column width
              selectInput(
                "account_type",
                "Select Account Type:",
                choices = NULL,  # Dynamically updated in server
                selected = NULL,
                multiple = TRUE  # Allow multiple selections
              )
            )
          ),
          column(
            width = 6,  # Annotation box
            box(
              title = "Account Type Explanations",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              div(
                class = "annotation-box",
                p("Capital Account: Tracks changes in ownership of national assets."),
                p("Financial Account: Records investments and transactions in international financial assets."),
                p("Current Account: Includes trade in goods, services, income, and current transfers."),
                p("Reserve Assets: Central bank holdings of foreign currencies, gold, and SDRs.")
              )
            )
          )
        )
        ,
        fluidRow(
          column(
            width = 12,  # Full-width column for the plot
            box(
              title = "Balance of Payments Summary (in millions $USD)",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              plotlyOutput("balance_of_payments_plot", height = "450px")
            )
          )
        )
      ),
      # Turnover Volumes Tab
      tabItem(
        tabName = "turnover_volumes",
        fluidRow(
          column(
            width = 6,  # Controls on the left
            box(
              title = "Controls",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              selectInput(
                "turnover_metric",
                "Select Metric:",
                choices = c("TotalBuySellTurnover", "RepoAgreementsTurnover"),
                selected = "TotalBuySellTurnover"
              ),
              checkboxInput(
                "show_trend",
                "Show Trend Line",
                value = TRUE
              ),
              sliderInput(
                "turnover_year_range",
                "Select Year Range:",
                min = 2020,  # Replace with your dataset's actual min year
                max = 2024,  # Replace with your dataset's actual max year
                value = c(2020, 2024),
                sep = ""
              )
            )
          ),
          column(
            width = 6,  # Statistical summary below the plot
            box(
              title = "Statistical Summary",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              height = "270px",
              tags$style(HTML("
    #turnover_summary {
      transform: scale(1.2);
      transform-origin: top left; /* Adjust the anchor point for scaling */
    }
  ")),
              tableOutput("turnover_summary")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,  # Time-series plot on the right
            box(
              title = "Turnover Time Series",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              plotlyOutput("turnover_plot", height = "450px")
            )
          )
        )
      ),
      tabItem(
        tabName = "conclusion",
        fluidRow(
          box(
            title = "Conclusion",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            h3("Key Findings"),
            h4("Diversification of Armenia’s International Economic Partnerships (2020-2024)"),
            p("Armenia has diversified its international economic partnerships, moving beyond traditional partners like Russia and the EU. The country has strengthened ties with China, Iran, and regional partners in the Middle East and Central Asia. These changes are driven by economic needs, geopolitical shifts, and a strategic effort to reduce dependence on a limited number of trading partners."),
            p("Armenia's engagement in multilateral initiatives like the Eurasian Economic Union (EAEU) and the Comprehensive and Enhanced Partnership Agreement (CEPA) with the EU, along with growth in sectors like technology, energy, and infrastructure, highlights this diversification."),
            
            h4("Intensification of Armenia’s Economic Relations"),
            p("Armenia has intensified economic relations with existing partners through increased trade volumes, FDI inflows, and joint ventures. Significant growth has been observed in trade with Russia and the EU, particularly in high-value exports like mineral products, machinery, and agriculture."),
            p("Participation in regional and global economic initiatives, such as the EAEU and CEPA, has further integrated Armenia into global markets."),
            
            h4("Role of Citizens in Shaping Economic Relations"),
            p("Citizens have played an indirect but growing role in shaping Armenia’s global economic relations through advocacy, public opinion, and grassroots movements. Increased awareness and use of digital platforms have amplified citizen participation in discussions on trade and economic policies."),
            
            h3("Hypotheses and Final Assessment"),
            h4("Hypothesis 1: Diversification and Intensification"),
            p("Armenia's economic partnerships have diversified and intensified between 2020 and 2024. The country expanded ties beyond traditional spheres and pursued new agreements. This hypothesis is accepted based on clear evidence of diversification and growth in trade and FDI."),
            
            h4("Hypothesis 2: Role of Citizens"),
            p("Citizens play a significant but indirect role in influencing Armenia’s global economic relations. Their impact is seen through advocacy, transparency demands, and social movements, rather than direct negotiations. This hypothesis is accepted with nuances."),
            
            h3("Final Assessment"),
            p("Both hypotheses are accepted, underscoring Armenia's efforts to diversify and intensify its economic partnerships while highlighting the evolving role of its citizens in shaping these relations.")
          )
        )
      )
      
    )
  )
)

# Update the Server
server <- function(input, output, session) {
  # Load and preprocess data for Dollarization
  dollarization_data <- load_and_preprocess_data("../data/processed/CLEANED-DOLLARIZATION-LEVEL-OF-RA.xlsx")
  
  # Update the selectInput choices for category selection
  updateSelectInput(
    session,
    "category_select",
    choices = unique(dollarization_data$Category),
    selected = unique(dollarization_data$Category)
  )
  
  # Reactive expression for filtered data
  filtered_dollarization_data <- reactive({
    req(input$category_select, input$year_range)  # Ensure inputs are available
    dollarization_data %>%
      filter(
        Category %in% input$category_select,  # Filter by selected categories
        Year >= input$year_range[1],         # Filter by start year
        Year <= input$year_range[2]          # Filter by end year
      )
  })
  
  # Reactive expressions for summarized data and Sankey diagram
  summarized_dollarization <- reactive({
    summarize_dollarization(filtered_dollarization_data())
  })
  
  nodes_reactive <- reactive({
    create_nodes(summarized_dollarization())
  })
  
  links_reactive <- reactive({
    create_links(summarized_dollarization())
  })
  
  # Render the interactive line plot for Dollarization over time
  output$dollarization_plot <- renderPlotly({
    data <- filtered_dollarization_data()
    validate(
      need(nrow(data) > 0, "No data available for the selected year range or categories.")
    )
    
    plot_dollarization_over_time(data)
  })
  
  # Render the Sankey diagram for Dollarization
  output$dollarization_sankey <- renderSankeyNetwork({
    sankeyNetwork(
      Links = links_reactive(),
      Nodes = nodes_reactive(),
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 12,
      nodeWidth = 30,
      units = "Total Value"
    )
  })
  
  # Foreign Investments
  # Foreign Investments
  observe({
    years <- grep("_Total|_Direct$", colnames(foreign_data), value = TRUE) %>%
      substr(1, 4) %>%
      unique() %>%
      as.numeric()
    
    # Update both year selectors without excluding any options
    updateSelectInput(session, "investment_year1", choices = years, selected = min(years))
    updateSelectInput(session, "investment_year2", choices = years, selected = max(years))
  })
  
  output$investment_world_map1 <- renderPlotly({
    create_world_map(
      foreign_data,
      year = input$investment_year1,
      metric = input$investment_metric1,
      investment_type = input$investment_type1
    )
  })
  
  output$investment_world_map2 <- renderPlotly({
    create_world_map(
      foreign_data,
      year = input$investment_year2,
      metric = input$investment_metric2,
      investment_type = input$investment_type2
    )
  })
  
  
  # Gross External Debt Tab
  Gross_External_Debt <- read_xlsx("../data/processed/CLEANED-GROSS-EXTERNAL-DEBT.xlsx")
  
  Gross_External_Debt <- Gross_External_Debt %>%
    mutate(
      Date = as.Date(Date),          # Ensure Date is in Date format
      Debt_Value = as.numeric(Debt_Value)  # Ensure Debt_Value is numeric
    )
  
  # Populate the selectInput with unique debt types and set the default to select all
  updateSelectInput(
    session,
    "debt_types",
    choices = c("General Government", "Central Bank", 
                "Deposit-taking Corporations", "Other Sectors", 
                "Direct Investment"),
    selected = c("General Government", "Central Bank", 
                 "Deposit-taking Corporations", "Other Sectors", 
                 "Direct Investment")  # Default: all five types selected
  )
  
  # Load the dataset
  gross_external_debt <- reactive({
    load_and_preprocess_gross_external_debt("../data/processed/CLEANED-GROSS-EXTERNAL-DEBT.xlsx")
  })
  
  # Filter data based on user input
  filtered_gross_external_debt <- reactive({
    req(input$debt_types)  # Ensure input is available
    
    # Summarize data for the selected types
    summarize_gross_external_debt_by_type(
      data = Gross_External_Debt,  # Replace with your dataset
      types = input$debt_types  # User-selected types
    )
  })
  
  # Render the time-series plot
  output$debt_plot <- renderPlotly({
    req(filtered_gross_external_debt())  # Ensure summarized data is available
    
    # Pass the summarized data to the plot function
    plot_gross_external_debt_over_time(filtered_gross_external_debt())
  })
  
  
  
  # Reactive expression to filter data for the Sankey diagram
  filtered_sankey_data <- reactive({
    req(input$debt_types)
    summarize_debt_for_sankey(Gross_External_Debt) %>%
      filter(Type %in% input$debt_types)
  })
  
  # Render the Sankey diagram based on the filtered data
  output$sankey_plot <- renderSankeyNetwork({
    validate(
      need(nrow(filtered_sankey_data()) > 0, "No data available for the Sankey diagram.")
    )
    create_sankey_diagram(filtered_sankey_data())
  })
  
  # Filtered Tourism Data Based on Year Range
  filtered_tourism_data <- reactive({
    req(input$year_range)
    tourism_data %>%
      filter(
        format(Date, "%Y") >= input$year_range[1],
        format(Date, "%Y") <= input$year_range[2]
      )
  })
  
  # Render the Tourism Data Summary with Transposed Rows
  output$tourism_summary <- renderTable({
    req(input$tourism_metric)  # Ensure a metric is selected
    
    data <- filtered_tourism_data()
    metric_column <- input$tourism_metric
    
    validate(
      need(nrow(data) > 0, "No data available for the selected year range."),
      need(all(!is.na(data[[metric_column]])), "Selected metric contains NA values.")
    )
    
    # Generate the summary and transpose it
    summary_df <- summary(data[[metric_column]])
    transposed_summary <- data.frame(
      Statistic = names(summary_df),
      Value = as.numeric(summary_df)
    ) %>%
      t()  # Transpose the data frame
    
    # Add proper row names for clarity
    rownames(transposed_summary) <- c("Statistic", "Value")
    
    # Return the transposed table
    transposed_summary
  }, rownames = TRUE)  # Display row names for Statistic and Value
  
  
  # Render the Interactive Stacked Bar Chart
  output$tourism_bar_chart <- renderPlotly({
    data <- filtered_tourism_data()
    create_stacked_bar_chart(data)
  })
  
  # Load Balance of Payments data
  balance_of_payments_data <- reactive({
    read_xlsx("../data/processed/CLEANED-BALANCE-OF-PAYMENTS-OF-RA.xlsx")
  })
  
  # Update the selectInput choices for account types
  observe({
    data <- balance_of_payments_data()
    updateSelectInput(
      session,
      "account_type",
      choices = unique(data$Main_Category),  # Use the Main_Category column
      selected = unique(data$Main_Category)  # Default: Select all
    )
  })
  
  filtered_balance_of_payments_data <- reactive({
    req(input$account_type)  # Ensure input is available
    data <- balance_of_payments_data()
    data <- data %>%
      filter(Main_Category %in% input$account_type)
    
    validate(
      need(nrow(data) > 0, "No data available for the selected account type(s).")
    )
    
    data
  })
  
  # Render the Balance of Payments Plot
  output$balance_of_payments_plot <- renderPlotly({
    data <- filtered_balance_of_payments_data()
    create_balance_of_payments_plot(data)
  })
  
  # Load and preprocess turnover data
  turnover_data <- load_and_preprocess_turnover_data("../data/processed/CLEANED-TURNOVER-VOLUMES-OF-INVESTMENTCOMPANIES&BANKS.xlsx")
  
  turnover_data <- turnover_data %>%
    mutate(Year = as.numeric(format(Date, "%Y")))  # Extract Year from Date
  
  # Reactive expression for filtering turnover data based on selected metric and year range
  filtered_turnover_data <- reactive({
    req(input$turnover_metric, input$turnover_year_range)  # Ensure inputs are available
    
    turnover_data %>%
      filter(
        Year >= input$turnover_year_range[1],
        Year <= input$turnover_year_range[2]
      )
  })
  
  
  # Reactive expression for generating statistical summaries
  turnover_summary <- reactive({
    req(input$turnover_metric)
    summary_statistics(filtered_turnover_data(), input$turnover_metric)
  })
  
  # Render the time-series plot
  output$turnover_plot <- renderPlotly({
    req(input$turnover_metric)
    create_time_series_plot(
      filtered_turnover_data(),
      metric = input$turnover_metric,
      show_trend = input$show_trend
    )
  })
  
  output$turnover_summary <- renderTable({
    req(input$turnover_metric)  # Ensure a metric is selected
    
    # Generate the summary
    summary <- turnover_summary()
    
    # Convert the summary to a data frame and transpose
    transposed_summary <- data.frame(
      Statistic = names(summary),
      Value = as.numeric(summary),
      row.names = NULL
    ) %>%
      t()  # Transpose the data frame
    
    # Add proper row names for clarity
    rownames(transposed_summary) <- c("Statistic", "Value")
    
    # Return the transposed table
    transposed_summary
  }, rownames = TRUE)  # Display rownames for Statistic and Value
  
  # Reshape and aggregate data
  industries_data <- reshape_and_aggregate_data(readxl::read_excel("../data/processed/CLEANED-INDUSTRIES-OF-RA.xlsx"))
  
  # Reactive to handle first selector's exclusion from the second
  available_years_second <- reactive({
    req(input$year_1)  # Ensure the first selector has a value
    setdiff(sort(unique(industries_data$Year)), input$year_1)  # Exclude the year selected in the first selector
  })
  
  # Filter data for the selected years
  filtered_industries_data <- reactive({
    validate(
      need(input$year_1, "Select a year in Year 1."),
      need(input$year_2, "Select a year in Year 2.")
    )
    
    data <- industries_data %>%
      filter(Year %in% c(input$year_1, input$year_2)) %>%
      group_by(Category, Industry, Year) %>%
      summarize(
        Total_Value = sum(Value, na.rm = TRUE),
        .groups = "drop"
      ) 
    
    # Rank categories within each year
    ranked_data <- data %>%
      group_by(Year) %>%
      arrange(Year, desc(Total_Value)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      filter(Rank <= input$top_n_categories)  # Filter by top N categories
    
    return(ranked_data)
  })
  
  
  # Render the Comparison Bar Chart
  output$industries_plot <- renderPlotly({
    data <- filtered_industries_data()
    
    validate(
      need(nrow(data) > 0, "No data available for the selected years or top categories.")
    )
    
    plot <- ggplot(data, aes(x = Category, y = Total_Value, fill = Industry)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~Year, ncol = 2) +
      labs(
        title = paste("Top", input$top_n_categories, "Industries (millions AMD) in", input$year_1, "and", input$year_2),
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    
    ggplotly(plot)
  })
  
  output$another_plot <- renderPlotly({
    data <- filtered_industries_data()
    
    validate(
      need(nrow(data) > 0, "No data available for the selected years or top categories.")
    )
    
    # Create another visualization using the same filtered data
    plot <- ggplot(data, aes(x = Industry, y = Total_Value, fill = Category)) +
      geom_col() +
      facet_wrap(~Year, scales = "free") +
      labs(
        title = paste("Another View of Top", input$top_n_categories, "Industries"),
        x = NULL,
        y = NULL
      ) +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  
  observe({
    updateSelectInput(
      session,
      "year_1",
      choices = sort(unique(industries_data$Year)),  # Ensure choices are set
      selected = head(sort(unique(industries_data$Year)), 1)  # Default to the first year
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "year_2",
      choices = available_years_second(),  # Exclude the first selected year
      selected = head(available_years_second(), 1)  # Default to the first available year
    )
  })
  
  category_descriptions <- list(
    "Primary Industries" = "- Includes agriculture, forestry, fishing, and mining.",
    "Food and Beverages" = "- Covers industries such as food production, beverages, and tobacco products.",
    "Textiles and Apparel" = "- Includes textiles, wearing apparel, and leather products.",
    "Wood and Paper Products" = "- Covers wood products, paper manufacturing, and printing.",
    "Chemical Products" = "- Includes the production of chemicals, pharmaceuticals, and refined petroleum products.",
    "Rubber, Plastics, and Non-Metallic Products" = "- Covers manufacturing of rubber, plastics, and non-metallic materials.",
    "Metal and Machinery Manufacturing" = "- Includes basic metals, machinery, and fabricated metal products.",
    "Electronics and Electrical Equipment" = "- Covers manufacturing of computers, electronics, and electrical equipment.",
    "Vehicles and Accessories" = "- Includes motor vehicles, parts, and accessories.",
    "Other Manufacturing" = "- Covers furniture, jewelry, and other miscellaneous manufacturing.",
    "Other Services" = "- Includes repair and installation of machinery and equipment.",
    "Energy and Utilities" = "- Covers electricity, gas, steam, and water supply.",
    "Water Supply and Environmental Services" = "- Includes water supply, waste management, and remediation activities."
  )
  
  output$category_annotation <- renderUI({
    tagList(
      lapply(names(category_descriptions), function(category) {
        tags$div(
          tags$h5(category),
          tags$p(category_descriptions[[category]])
        )
      })
    )
  })
  
  # Define FX descriptions as a character vector
  fx_descriptions <- c(
    "FX Demand Deposits to Total Demand Deposits - Foreign currency demand deposits as a share of total demand.",
    "FX Deposits to Money Supply - Foreign currency deposits as a proportion of total money supply.",
    "FX Deposits to Total Deposits - Foreign currency deposits as a percentage of all deposits.",
    "FX Loans to Total Loans - Foreign currency loans as a share of all loans.",
    "FX Time Deposits to Total Time Deposits - Foreign currency time deposits relative to total time deposits."
  )
  
  # Render dynamic UI for FX annotations
  output$dollarization_annotation <- renderUI({
    tagList(
      lapply(fx_descriptions, function(description) {
        # Split the description into title and text
        parts <- strsplit(description, " - ", fixed = TRUE)[[1]]
        tags$div(
          tags$h5(parts[1]),  # Use the first part as the title
          tags$p(parts[2])    # Use the second part as the description
        )
      })
    )
  })
  
}

# Run the App
shinyApp(ui = ui, server = server)

