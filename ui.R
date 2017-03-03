library(shiny)

datafile <- read_csv("final_cdi_merged_cleaned.csv")
Vchoices <- 1:ncol(datafile)
names(Vchoices) <- names(datafile)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Select the columns that you wish to view in the data table."),
      selectInput("colChoices", label = "Select Columns for Data Table: ",
                  choices = Vchoices,
                  multiple = TRUE),
      br(),
      helpText("Select the two variables you wish to view in the scatter plot.
               First choice = X-axis; Second choice = Y-axis."),
      selectizeInput("scatChoices", label = "Select Variables for Scatter Plot: ",
                  choices = Vchoices, selected = 5,
                  options = list(maxItems = 2)),
      br(),
      helpText("Select the variable with respect to which the scatter plot colors
               will be filtered."),
      selectInput("color_var", label = "Select Variable for Plot Color Scale:",
                  choices = Vchoices, selected = 5,
                  multiple = FALSE)
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
      h2("Survey Results", align = "center"),
      DT::dataTableOutput("table"),
      plotOutput("scat", height = 500)
    )
  )
))
