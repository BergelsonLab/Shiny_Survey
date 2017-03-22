library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)


datafile1 <- read.csv("final_cdi_merged_cleaned.csv")

datafile <- mutate(datafile1,
                   Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] > 0 & 
                                                datafile1[,c(17:47, 50:445)] < 2),
                   Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1))


Vchoices <- 1:ncol(datafile)
names(Vchoices) <- names(datafile)

sumChoices <- select(datafile, Understand_childname:Talk_some)

#mutate(datafile,from columns 17-445
#       Understand_Sum = ##Sum add if column name starts with "Understand"
                        ## and value = "Sometimes" or "Often" or 1 or "Yes"
                        ## or if column name starts with "Talk" and value 
                        ## = 1,
#       Talk_Sum = ##Sum add if column name starts with "Talk" and value 
                  ## = 2,
#       Gesture_Sum = ##Sum add if column name starts with "Gestures" and
                      ##Value = "Yes")

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
                  choices = Vchoices, selected = 4,
                  multiple = FALSE)
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
      h2("Survey Results", align = "center"),
      DT::dataTableOutput("table"),
      plotOutput("plot", height = 500)
    )
  )
))
