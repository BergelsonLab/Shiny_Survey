#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#datafile <- read.csv("final_motor_merged.csv", fileEncoding = "iso-8859-1")
#datafile <- read.csv("final_motor_merged_cleaned.csv", fileEncoding = "iso-8859-1")
datafile <- read.csv("final_cdi_merged_cleaned.csv", fileEncoding = "iso-8859-1")
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
      helpText("Select the columns you wish to view in the scatter plot.
               First choice = X-axis; Second choice = Y-axis."),
      selectizeInput("scatChoices", label = "Select Columns for Scatter Plot: ",
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
