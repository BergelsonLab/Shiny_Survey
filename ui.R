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

# Convert 'Often' to = 2; 'Sometimes' to = 1; 'Never' to = 0
for(n in names(datafile1[,c(446:457)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Often',2,
                           ifelse(datafile1[[n]] == 'Sometimes',1,0))
}

# Convert 'Often' to = 2; 'Sometimes' to = 1; 'Never' to = 0
for(n in names(datafile1[,c(48:49)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Often',2,
                           ifelse(datafile1[[n]] == 'Sometimes',1,0))
}

# Mutate datafile to calculate fraction of all the words/phrases udnerstood; spoken by;
# and gestures made by each subject in each month; add's columns to end of data
datafile <- mutate(datafile1,
                   Fraction_Total_First_Signs_Understand = rowSums(datafile1[,c(17:19)])/3,
                   Fraction_Total_Understand_Phrases = rowSums(datafile1[,c(20:47, 50:445)] == 1)/422,
                   Fraction_Total_Start_Talk = rowSums(datafile1[,c(48:49)]==2)/2,
                   Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                   Fraction_Total_Gestures1 = rowSums(datafile1[,446:457]>0)/12,
                   Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)


# Create vector of names to be used as choices for the color scheme of the plot output
color_choices <- 1:ncol(datafile)
names(color_choices) <- names(datafile)

#Create dataframe of gestures in first gesture section of CDI survey
datafile_gestures1_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                   Child_gender, Gestures_showobject:Gestures_allgone, Fraction_Total_Gestures1)
Vchoices_gestures1_table <- 1:ncol(datafile_gestures1_table)
# Use names from the datafile as a vector stripping the preceding "Gestures_" strings
# Therefore displaying only the word to  the user
names(Vchoices_gestures1_table) <- gsub("Gestures_","", names(datafile_gestures1_table))

#Create dataframe of gestures in second gestures section of CDI survey (Games)
datafile_gestures1_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                                  Gestures_showobject:Gestures_allgone, Fraction_Total_Gestures1)
Vchoices_gestures1_plot <- 1:ncol(datafile_gestures1_plot)
# Use names from the datafile as a vector stripping the preceding "Gestures_" strings
# Therefore displaying only the word to  the user
names(Vchoices_gestures1_plot) <- gsub("Gestures_", "", names(datafile_gestures1_plot))

# Create subset of datafile for table containing only "first signs of understanding" information
datafile_first_signs_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                     Child_gender, Understand_childname:Understand_theresmommy,
                                     Fraction_Total_First_Signs_Understand)
Vchoices_table_first_signs <- 1:ncol(datafile_first_signs_table)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_table_first_signs) <- gsub("Understand_","",names(datafile_first_signs_table))

# Create subset of datafile for table containing only "phrases understood" information
datafile_understand_phrases_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                    Child_gender, Understand_areyouhungry:Understand_goforride,
                                    Fraction_Total_Understand_Phrases)
Vchoices_table_understand_phrases <- 1:ncol(datafile_understand_phrases_table)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_table_understand_phrases) <- gsub("Understand_","",names(datafile_understand_phrases_table))


# Create subset of datafile for table containing only "starting to talk" information
datafile_starting_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                       Child_gender, Understand_parroting:Understand_labeling, Fraction_Total_Start_Talk)
Vchoices_table_starting_talk <- 1:ncol(datafile_starting_talk_table)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_table_starting_talk) <- gsub("Understand_", "", names(datafile_starting_talk_table))

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


# Create subset of datafile for plot output containing only "first signs understand" information
datafile_first_signs_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                    Understand_childname:Understand_theresmommy,
                                    Fraction_Total_First_Signs_Understand)
Vchoices_plot_first_signs <- 1:ncol(datafile_first_signs_plot)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_plot_first_signs) <- gsub("Understand_", "", names(datafile_first_signs_plot))

# Create subset of datafile for plot output containing only "phrases understood" information
datafile_understand_phrases_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                              Understand_areyouhungry:Understand_goforride, 
                              Fraction_Total_Understand_Phrases)
Vchoices_understand_phrases_plot <- 1:ncol(datafile_understand_phrases_plot)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_understand_phrases_plot) <- gsub("Understand_","",names(datafile_understand_phrases_plot))


# Create subset of datafile for plot output containing only "starting to talk" information
datafile_starting_talk_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                      Understand_parroting:Understand_labeling, Fraction_Total_Start_Talk)
Vchoices_starting_talk_plot <- 1:ncol(datafile_starting_talk_plot)
# Use names from the datafile as a vector stripping the preceding "Understand_" strings
# Therefore displaying only the word to  the user
names(Vchoices_starting_talk_plot) <- gsub("Understand_", "", names(datafile_starting_talk_plot))


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



# read in second datafile to work with
datafile2 <- read.csv("final_motor_merged_cleaned.csv")

# Create subset of datafile2 (motor survey) for table output 
datafile_motor_table <- select(datafile2, Subject.Number_Month:LocationLongitude)
Vchoices_motor_table <- 1:ncol(datafile_motor_table)
names(Vchoices_motor_table) <- names(datafile_motor_table)

# Create subset of datafile2 for plot output
datafile_motor_plot <- select(datafile2, Age_Corrected, ageweight,
                              rest_on_body:crawling_belly, crawling_hands_knees,
                              cruising, walking)
Vchoices_motor_plot <- 1:ncol(datafile_motor_plot)
names(Vchoices_motor_plot) <- names(datafile_motor_plot)

color_motor <- 1:ncol(datafile_motor_table)
names(color_motor) <- names(datafile_motor_table)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Seedling Survey Results")),
  
  # Sidebar with a widgets for user manipulation of data and display in main panel
  # 1st Download Button and Radio Button remain present regardless of user input
  sidebarLayout(
    sidebarPanel(
      # Drop down menu to allow users to select either CDI survey or Motor survey to analyze
      selectInput("dataset", "Choose Dataset", choices = list("final_cdi_merged_cleaned.csv" = 1, 
                                                              "final_motor_merged_cleaned.csv" = 2),
                  selected = 1),
      # Help text describes what the following widget does and what the user should do
      helpText("Download data to a csv file containing all rows and columns from the selected .csv file above (not filtered)"),
      # Create button to download csv file from the app to user's local machine.
      downloadButton("downloadData", "Download Full Data"),
      
      #Only display these widgets if user selected CDI dataset becasue these only apply to CDI survey
      conditionalPanel(
        condition = "input.dataset == 1",
        helpText("Select word/phrase options to analyze from CDI dataset:"),
        # Allows user to select 1 of the choices; this input impacts the possible inputs for table and plot
        radioButtons("radioButton", label = "Select set of words/phrases: ",
                     choices = list("First Signs of Understanding" = 1, "Phrases Understood" = 2,
                                    "Starting to Produce" = 3, "Vocab Checklist" = 4, "Gestures_ASN" = 5, "Games and Routines" = 6))
      ),
      br(),
      # Conditional Panels only appear if the condition is met
      # These panels based on user input in the radioButton widget above 
      conditionalPanel(
        # If user selects "First Signs of Understanding" in radioButton and CDI dataset, these widgets are displayed
        condition = "input.radioButton == 1 && input.dataset == 1",
        helpText("Select the columns that you wish to view in the data table
                 (0 = child does not understand the word, 1 = child understands the word)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        # Choices from "datafile_understand_table" only 
        selectInput("colChoices_first_signs_understand", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_first_signs,
                    multiple = TRUE),
        # User can download subset of data containing only "first signs of understanding" information
        helpText("Download the filtered data from datatable to csv file.  If no columns selected in above data field,
                 full 'Understand' dataset will be downloaded."),
        downloadButton("down_filter_first_signs", "Download Filtered Data")
        ),
      
      conditionalPanel(
        # If user selects "Phrases Understood" in radioButton and CDI dataset, these widgets are displayed
        condition = "input.radioButton == 2 && input.dataset == 1",
        helpText("Select the columns that you wish to view in the data table
                 (0 = child does not understand the word/phrase, 1 = child understands the word/phrase)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        selectInput("colChoices_phrases_understand", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_understand_phrases,
                    multiple = TRUE),
        # User can download subset of data containing only "understand phrases" information
        helpText("Download the filtered data from datatable to csv file.  If no columns selected in above data field,
                 full 'Understand' dataset will be downloaded."),
        downloadButton("down_filter_phrases_understood", "Download Filtered Data")),
      
      conditionalPanel(
        # If user selects "Starting to Produce" in radioButton and CDI dataset, these widgets are displayed
        condition = "input.radioButton == 3 && input.dataset == 1",
        helpText("Select the columns that you wish to view in the data table
                 (0 = child does not produce the word, 1 = child sometimes produces the word, 2 = child often produces the word)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        selectInput("colChoices_starting_talk", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_starting_talk,
                    multiple = TRUE),
        # User can download subset of data containing only "starting to talk" information
        helpText("Download the filtered data from datatable to csv file.  If no columns selected in above data field,
                 full 'Understand' dataset will be downloaded."),
        downloadButton("down_filter_start_talk", "Download Filtered Data")),
      
      conditionalPanel(
        # If user selects "Vocab Checklist" in radioButton and CDI dataset, these widgets are displayed
        condition = "input.radioButton == 4 && input.dataset == 1",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child can not produce the word, 1 = child can produce the word)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        selectInput("colChoices_talk", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_talk,
                    multiple = TRUE),
        # User can download subset of data containing only "Vocab Checklist" information
        helpText("Download the filtered data from datatable to csv file. If no columns selected in above data field,
                 full 'Talk' dataset will be downloaded."),
        downloadButton("down_filter_talk", "Download Filtered Data")),

      conditionalPanel(
        # If user selects "Gestures_ASN" in radioButton and CDI dataset, these widgets are displayed
        condition = "input.radioButton == 5 && input.dataset == 1",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child never makes the gesture, 1 = child sometimes makes the gesture,
                 2 = child often makes the gestures)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        selectInput("colChoices_gesture1", label = "Select Columns for Data Table: ",
                    choices = Vchoices_gestures1_table,
                    multiple = TRUE),
        # User can download subset of data containing only "gestures" information
        helpText("Download the filtered data from datatable to csv file. If no columns selected in above data field,
                 full 'Gestures' dataset will be downloaded."),
        downloadButton("down_filter_gesture1", "Download Filtered Data")),
      
      conditionalPanel(
        # If user selects "Games and Routines" in radioButton and CDI dataset, these widgets are displayed
        condition = "input.radioButton == 6 && input.dataset == 1",
        helpText("Select the columns that you wish to view in the data table 
                 (0 = child does not make the gesture, 1 = child makes the gesture)."),
        # Cretes dropdown menu of choices for user to select to be displayed in the table
        selectInput("colChoices_gesture", label = "Select Columns for Data Table: ",
                    choices = Vchoices_table_gesture,
                    multiple = TRUE),
        # User can download subset of data containing only "Games and Routines" information
        helpText("Download the filtered data from datatable to csv file. If no columns selected in above data field,
                 full 'Gestures' dataset will be downloaded."),
        downloadButton("down_filter_gesture", "Download Filtered Data")),
      
      br(),
      conditionalPanel(
        # If user selects "First Signs of Understanding" in radioButton, these widgets are displayed
        condition = "input.radioButton == 1 && input.dataset == 1",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        selectizeInput("plotChoices_First_Signs", label = "First Signs of Understanding Word Choices: ",
                       choices = Vchoices_plot_first_signs, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
               will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_first_signs", label = "Select Variable for Understand Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        # If user selects "Phrases Understood" in radioButton, these widgets are displayed
        condition = "input.radioButton == 2 && input.dataset == 1",
        helpText("Select the two variables you wish to view in the plot.
                 First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        selectizeInput("plotChoices_Phrases_Understood", label = "Phrases Understood Choices: ",
                       choices = Vchoices_understand_phrases_plot, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
                 will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_phrases_understood", label = "Select Variable for Understand Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        # If user selects "Starting to Produce" in radioButton, these widgets are displayed
        condition = "input.radioButton == 3 && input.dataset == 1",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        selectizeInput("plotChoices_Starting_Talk", label = "Starting Talk Word Choices: ",
                       choices = Vchoices_starting_talk_plot, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
               will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_start_talk", label = "Select Variable for Understand Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        # If user selects "Vocab Checklist" in radioButton, these widgets are displayed
        condition = "input.radioButton == 4 && input.dataset == 1",
        helpText("Select the two variables you wish to view in the plot.
               First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
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
        # If user selects "Gestures_ASN" in radioButton, these widgets are displayed
        condition = "input.radioButton == 5 && input.dataset == 1",
        helpText("Select the two variables you wish to view in the plot.
                 First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        selectizeInput("plotChoices_gestures1", label = "Select Gestures: ",
                       choices = Vchoices_gestures1_plot, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
                 will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_gestures1", label = "Select Variable for Gestures Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      
      conditionalPanel(
        # If user selects "Games and Routines" in radioButton, these widgets are displayed
        condition = "input.radioButton == 6 && input.dataset == 1",
        helpText("Select the two variables you wish to view in the plot.
                First choice = X-axis; Second choice = Y-axis."),
        # Cretes dropdown menu of choices for user to select to be displayed in the plot
        selectizeInput("plotChoices", label = "Select Gestures: ",
                       choices = Vchoices_gesture, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
                 will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_gestures", label = "Select Variable for Gestures Plot Color Scale:",
                    choices = color_choices, selected = 4,
                    multiple = FALSE)),
      
      conditionalPanel(
        # If user selectes motor datset in first dropdown menu 
        condition = "input.dataset == 2",
        helpText("Select columns that you wish to view in the table: "),
        # User can select columns to be viewed in table output
        selectInput("colChoices_motor", label = "Select Columns for Data Table: ",
                    choices = Vchoices_motor_table,
                    multiple = TRUE),
        # User can download filtered data from table output
        downloadButton("down_filter_motor", "Download Filtered Data")),
      conditionalPanel(
        condition = "input.dataset == 2",
        helpText("Select the two variables you wish to view in the plot.
                First choice = X-axis; Second choice = Y-axis."),
        # Choices for user to select for the plot output variables
        selectizeInput("plotChoices_motor", label = "Select Variable for Motor Plot: ",
                       choices = Vchoices_motor_plot, selected = 1,
                       options = list(maxItems = 2)),
        helpText("Select the variable with respect to which the plot colors
                 will be filtered."),
        # Cretes dropdown menu of choices for user to select color to be displayed in the plot
        selectInput("color_var_motor", label = "Select Variable for Motor Plot Color Scale:",
                    choices = color_motor, selected = 3,
                    multiple = FALSE)
      )
    ),
  
    # Show a the table and plot from user input in sidebar panel widgets
    mainPanel(
      h2("Survey Results", align = "center"),
      #Data Table output initialized
      DT::dataTableOutput("table"),
      #Plot output initialized
      plotOutput("plot", height = 500)
    )
  )
))