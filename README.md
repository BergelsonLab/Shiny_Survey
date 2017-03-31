# Shiny_Survey
These R scripts create an application (through ShinyApp) to visualize and manipulate survey results from 
CSV files. CSV files are read into the program via the code:
```
datafile <- read_csv("file_name_here")
```
Since the CSV file being analyzed in this use of the code is located within the same directory as the R scripts, 
only the file name, and not the full path to the data file, is required in order to read the CSV file.

## Usage ui.r Script
ui.r creates the template of the app with a side bar panel and various text boxes and drop down menus that enable the user to 
select which columns from the CSV file to display in the table and graph.  Furthermore, the datafile is altered at the start of the ui.r script using the "ifelse" and "mutate" functions.  The "ifelse" function converts values of "No" to "0" and values of "Yes" to "1," which is necessary in order to track the total number of words understood, spoken, and gestures made.  The "mutate" function creates 3 additional columns for the csv that track the total number of words understood by each subject, the total number of words spoken by each subject, and the total gestures made by each subject.  Furthermore, the "radioButton" widget in the sidebar panel enables the user to select which variable to analyze, whether it be understood, spoken, or gestures.  Based on what the user selects, the dropdown menus for the table and plot outputs will display the words and phrases that paired with either "understand," "talk," or "gestures."  This was accomplished using the "conditionalPanel()" function where the condition is based on the "radioButton" input. 

```
datafile <- mutate(datafile1,
                   Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                   Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                   Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)
                   
radioButtons("radioButton", label = "Select set of words/phrases: ",
                         choices = list("Understand" = 1, "Talk" = 2, "Gestures" = 3)),

conditionalPanel(
        condition = "input.radioButton == 1",
        ...)
```
The main panel code block that is at located the bottom of ui.r is where the table and graph that the app displays 
are initialized.

## Usage server.r Script
server.r uses the input from the user to create and display the reactive data table and graph in the application window.
The code "input$" followed by the id from one of the selectInput functions from ui.r, such as "colChoices", 
instructs the app to use this input in creating the table or graph.  the reactive part of the code is based on the radio button input and causes the application to recreate the output table or graph each time the input$ is altered by the user, keeping the visual display of data representative of the variables selected by the user.  "colChoices_understand/talk/gestures" impacts the variables displayed in the table, "plotChoices_understand/talk/gestures" impacts the variables represented in the graph (whether it is a scatter plot or a bar chart), "color_var_understand/talk/gestures" changes the variable off which the graph's colors are based. 
```
x <- reactive({input$radioButton})
output$plot<-renderPlot({
    if(x() == 1){...
    if(x() == 2){...
    if(x() == 3){...
```
The graph depicted will either be a scatter plot or a bar chart based on the variable selected by the user.  If the user selects either "Fraction_Total_Understand", "Fraction_Total_Talk", or "Fraction_Total_Gestures", the graph will display a scatter plot.  However, if the user wishes to visualize specific phrases and words, a bar chart will be displayed, providing a better representation of the data.

Inside each block of code, the inputs are used (as.numeric in order to be used to access columns from the CSV 
file based on column number) to actually create the table and graph using the code: 
```
output$table = DT::renderDataTable(datafile[,cols], filter = "top")

p <- ggplot(data = datafile2) + 
          geom_point(mapping = aes(x = datafile2[cols2[1]], 
                                   y = datafile2[cols2[2]],
                                   color = datafile2[colorVar])
```
"DT::renderDataTable" creates the table while "ggplot" is used to create the graphs.  Both the DT and ggplot2 libraries are
imported at the top of server.r. 

## Using the App
Before being able to run the appliction, you must make sure that all of the R libraries used in the code are installed.  
These libraries include shiny, DT, tidyverse, and ggplot2.  In order to install these libraries, simply run the following
code in your console:
```
> install.packages("shiny")
> install.packages("DT")
> install.packages("tidyverse")
> install.packages("ggplot2")
```

If you are using this code in the RStudio environment, simply clicking the "Run App" button located in the upper right hand corner
of the workspace will launch the application.  If you do not have the "Run App" button, you can launch the application by running 
"> library(shiny)" follwed by the "> runApp()" function with the argument to "runApp()" being the name of the app's directory.  For 
example, if the app is saved in a directory called my_app, executing the following code in the console will launch the application:
```
> library(shiny)
> runApp("my_app")
```
After the application is launched, the display screen will show the sidebar panel with four locations for the user to 
select inputs.  First, the user should select whether to analyze "Understand", "Talk", or "Gestures".  After selecting one of those three options, the user can input which variables he or she wishes to veiw in the data table and plot from the three drop down menus of word choices.  The user can select as many variables as he or she wishes to display in the data table by clicking the empty text box labeled "Select Columns for Data Table:" and selecting variables from the drop down menu. Furthermore, the table's columns can be sorted by using the filters located at the top of each column.  The user can select any two variables to be displayed in the scatter plot.
It is suggested that the first variable selected (x-axis) for the plot be "AgeMonthCDI_Uncorrected" in order to produce the most informative and visually useful graph.

Finally, the user can change the variable off which the color of the scatter plot points are based by selecting a different input from
the third drop down menu labeled, "Select Variable for Understand/Talk/Gestures Plot Color Scale:".  Changing this variable will alter the color of the points or bars in the scatter plot or bar chart.
