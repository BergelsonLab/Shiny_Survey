library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

# Read in datafile to work with as datafile1
datafile1 <- read.csv("final_cdi_merged_cleaned.csv")

# Convert 'Yes' values in the csv file to = 1
# Convert 'No' values in the csv file to = 0
# Allows manipulation/computation of additional columns based on responses
# Fraction_total columns need integer numbers to calculate
for(n in names(datafile1[,c(17:19, 458:508)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Yes',1,0)
}

# Mutate datafile to calculate fraction of all the words udnerstood; spoken by;
# and gestures made by each subject in each month; add's columns to end of data
datafile <- mutate(datafile1,
                   Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                   Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                   Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)


# Create vector of names to be used as choices for the color scheme of the plot output
color_choices <- 1:ncol(datafile)
names(color_choices) <- names(datafile)


# Create subset of datafile for table containing only "understand" information
datafile_understand_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                    Child_gender, Understand_childname:Understand_goforride,
                                    Fraction_Total_Understand)
Vchoices_table_understand <- 1:ncol(datafile_understand_table)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_table_understand) <- gsub("Understand_","",names(datafile_understand_table))


# Create subset of datafile for table containing only "talk" information
datafile_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                              Child_gender, Talk_baabaa:Talk_some, Fraction_Total_Talk)
Vchoices_table_talk <- 1:ncol(datafile_talk_table)
# Use names from the datafile as a vector stripping the preceding "Talk_" strings
# Therefore displaying only the word to  the user
names(Vchoices_table_talk) <- gsub("Talk_","",names(datafile_talk_table))


# Create subset of datafile for table containing only "gestures" information
datafile_gesture_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                Child_gender, Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
Vchoices_table_gesture <- 1:ncol(datafile_gesture_table)
# Use names from the datafile as a vector stripping the preceding "Gestures_" strings
# Therefore displaying only the word to  the user
names(Vchoices_table_gesture) <- gsub("Gestures_","",names(datafile_gesture_table))


# Create subset of datafile for plot output containing only "understand" information
datafile_understand <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                              Understand_childname:Understand_goforride, 
                              Fraction_Total_Understand)
Vchoices_understand <- 1:ncol(datafile_understand)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_understand) <- gsub("Understand_","",names(datafile_understand))


# Create subset of datafile for plot output containing only "talk" information
datafile_talk <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                        Talk_baabaa:Talk_some, Fraction_Total_Talk)
Vchoices_talk <- 1:ncol(datafile_talk)
# Use names from the datafile as a vector stripping the preceding "Talk_" strings
# Therefore displaying only the word to  the user
names(Vchoices_talk) <- gsub("Talk_","",names(datafile_talk))


# Create subset of datafile for plot output containing only "gestures" information
datafile_gesture <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                           Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
Vchoices_gesture <- 1:ncol(datafile_gesture)
# Use names from the datafile as a vector stripping the preceding "Gestures_" strings
# Therefore displaying only the word to  the user
names(Vchoices_gesture) <- gsub("Gestures_","",names(datafile_gesture))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  # Sidebar with a widgets for user manipulation of data and display in main panel
  # 1st Download Button and Radio Button remain present regardless of user input
  sidebarLayout(
    sidebarPanel(
      # Help text describes what the following widget does and what the user should do
      helpText("Download data to a csv file containing 'understand,' 'talk,' and 'gestures,' information:"),
      # Create button to download csv file from the app to user's local machine.
      downloadButton("downloadData", "Download Full Data"),
      helpText("Select word/phrase options to analyze:"),
      # Allows user to select 1 of the choices; this input impacts the possible inputs for table and plot
      radioButtons("radioButton", label = "Select set of words/phrases: ",
                         choices = list("Understand" = 1, "Talk" = 2, "Gestures" = 3)),
      
      br(),
      
      # Conditional Panels only appear if the condition is met
      # These panels based onuser input in the radioButton widget above
      conditionalPanel(
        # If user selects "Understand" in radioButton, these widgets are displayed
        condition = "input.radioButton == 1",
        helpText("Select the columns that you wish to view in the data table
                 (0 = child does not understand the word, 1 = child understands the word)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        # Choices from "datafile_understand_table" only 
        selectInput("colChoices_understand", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_understand,
                    multiple = TRUE),
        # User can download subset of data containing only "understand" information
        # From datafile_understand_table
        helpText("Download the filtered data to csv file"),
        downloadButton("down_filter_understand", "Download Filtered Data")),
      
      conditionalPanel(
        # If user selects "Talk" in radioButton, these widgets are displayed
        condition = "input.radioButton == 2",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child can not say the word, 1 = child can say the word)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        # Choices from "datafile_talk_table" only
        selectInput("colChoices_talk", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_talk,
                    multiple = TRUE),
        # User can download subset of data containing only "talk" information
        # From datafile_talk_table"
        helpText("Download the filtered data to csv file"),
        downloadButton("down_filter_talk", "Download Filtered Data")),

      conditionalPanel(
        # If user selects "Gestures" in radioButton, these widgets are displayed
        condition = "input.radioButton == 3",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child does not make the gesture, 1 = child makes the gesture)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        # Choices from "datafile_gestures_table" only
        selectInput("colChoices_gesture", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_gesture,
                    multiple = TRUE),
        # User can download subset of data containing only "gestures" information
        # From datafile_gestures_table"
        helpText("Download the filtered data to csv file"),
        downloadButton("down_filter_gesture", "Download Filtered Data")),
      
      br(),
      conditionalPanel(
        # If user selects "Understand" in radioButton, these widgets are displayed
        condition = "input.radioButton == 1",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        # Choices from "datafile_understand" only
        selectizeInput("plotChoices_Understand", label = "Understand Word Choices: ",
                       choices = Vchoices_understand, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
               will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_understand", label = "Select Variable for Understand Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        # If user selects "Talk" in radioButton, these widgets are displayed
        condition = "input.radioButton == 2",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        # Choices from "datafile_talk" only
        selectizeInput("plotChoices_Talk", label = "Spoken Word Choices: ",
                       choices = Vchoices_talk, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
               will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_talk", label = "Select Variable for Talk Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        # If user selects "Gestures" in radioButton, these widgets are displayed
        condition = "input.radioButton == 3",
        helpText("Select the two variables you wish to view in the plot.
                First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        # Choices from "datafile_gestures" only
        selectizeInput("plotChoices", label = "Select Gestures: ",
                       choices = Vchoices_gesture, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
                 will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_gestures", label = "Select Variable for Gestures Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE))
    ),
  
    # Show a the table and plot from user input in sidebar panel widgets
    mainPanel(
      h2("Survey Results", align = "center"),
      #Data Table output
      DT::dataTableOutput("table"),
      #Plot output
      plotOutput("plot", height = 500)
    )
  )
))