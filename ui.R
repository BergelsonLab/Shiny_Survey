library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)


datafile1 <- read.csv("final_cdi_merged_cleaned.csv")

for(n in names(datafile1[,c(17:19, 458:508)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Yes',1,0)
}

datafile <- mutate(datafile1,
                   Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                   Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                   Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)

table_choices <- 1:ncol(datafile)
names(table_choices) <- names(datafile)


datafile_understand_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                    Child_gender, Understand_childname:Understand_goforride,
                                    Fraction_Total_Understand)
Vchoices_table_understand <- 1:ncol(datafile_understand_table)
names(Vchoices_table_understand) <- gsub("Understand_","",names(datafile_understand_table))


datafile_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                              Child_gender, Talk_baabaa:Talk_some, Fraction_Total_Talk)
Vchoices_table_talk <- 1:ncol(datafile_talk_table)
names(Vchoices_table_talk) <- gsub("Talk_","",names(datafile_talk_table))


datafile_gesture_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                Child_gender, Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
Vchoices_table_gesture <- 1:ncol(datafile_gesture_table)
names(Vchoices_table_gesture) <- gsub("Gestures_","",names(datafile_gesture_table))


datafile_understand <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                              Understand_childname:Understand_goforride, 
                              Fraction_Total_Understand)
Vchoices_understand <- 1:ncol(datafile_understand)
names(Vchoices_understand) <- gsub("Understand_","",names(datafile_understand))


datafile_talk <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                        Talk_baabaa:Talk_some, Fraction_Total_Talk)
Vchoices_talk <- 1:ncol(datafile_talk)
names(Vchoices_talk) <- gsub("Talk_","",names(datafile_talk))


datafile_gesture <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                           Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
Vchoices_gesture <- 1:ncol(datafile_gesture)
names(Vchoices_gesture) <- gsub("Gestures_","",names(datafile_gesture))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Select word/phrase options to analyze:"),
      radioButtons("radioButton", label = "Select set of words/phrases: ",
                         choices = list("Understand" = 1, "Talk" = 2, "Gestures" = 3)),
      br(),
      conditionalPanel(
        condition = "input.radioButton == 1",
        helpText("Select the columns that you wish to view in the data table
                 (0 = child does not understand the word, 1 = child understands the word)."),
        selectInput("colChoices_understand", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_understand,
                    multiple = TRUE)),
      
      conditionalPanel(
        condition = "input.radioButton == 2",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child can not say the word, 1 = child can say the word)."),
        selectInput("colChoices_talk", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_talk,
                    multiple = TRUE)),

      conditionalPanel(
        condition = "input.radioButton == 3",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child does not make the gesture, 1 = child makes the gesture)."),
        selectInput("colChoices_gesture", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_gesture,
                    multiple = TRUE)),
      br(),
      conditionalPanel(
        condition = "input.radioButton == 1",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        selectizeInput("plotChoices_Understand", label = "Understand Word Choices: ",
                       choices = Vchoices_understand, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
               will be filtered."),
        selectInput("color_var_understand", label = "Select Variable for Understand Plot Color Scale:",
                    choices = table_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        condition = "input.radioButton == 2",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        selectizeInput("plotChoices_Talk", label = "Spoken Word Choices: ",
                       choices = Vchoices_talk, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
               will be filtered."),
        selectInput("color_var_talk", label = "Select Variable for Talk Plot Color Scale:",
                    choices = table_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        condition = "input.radioButton == 3",
        helpText("Select the two variables you wish to view in the plot.
                First choice = X-axis; Second choice = Y-axis."),
        selectizeInput("plotChoices", label = "Select Gestures: ",
                       choices = Vchoices_gesture, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
                 will be filtered."),
        selectInput("color_var_gestures", label = "Select Variable for Gestures Plot Color Scale:",
                    choices = table_choices, selected = 4,
                    multiple = FALSE))
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
      h2("Survey Results", align = "center"),
      DT::dataTableOutput("table"),
      plotOutput("plot", height = 500)
    )
  )
))
