#Shiny_Survey
These R scripts create an application (through ShinyApp) to visualize and manipulate survey results from 
various CSV files. CSV files are read into the program via the code:
```
datafile <- read_csv("file_name_here")
```
Since the CSV file being analyzed in this use of the code is located within the same directory as the R scripts, 
only the file name, and not the full path to the data file, is required in order to read the CSV file.

#Usage ui.r Script
ui.r creates the template of the app with a side bar panel and various text boxes and drop down menus that enable the user to 
select which columns from the CSV file to display in the table and graph.  Vchoices is the 
variable that contains the names of the columns and is used in each "selectInput" function.  

```
Vchoices <- 1:ncol(datafile)
selectInput("colChoices", label = "Select Columns for Data Table: ",
                  choices = Vchoices,
                  multiple = TRUE)
```
The main panel code block that is at located the bottom of ui.r is where the table and graph that the app displays 
are initialized.

#Usage server.r Script
server.r uses the input from the user to create and display the reactive data table and graph in the app.
The code "input$" followed by the id from one of the selectInput functions from ui.r, such as "colChoices", 
tells the app to use this input in creating the table or graph.  the code "observeEvent" causes the application 
to recreate the output table or graph each time the input$ is altered by the user, keeping the visual display of data 
representative of the variables selected by the user.  "colChoices" impacts the variables displayed in the table, "scatChoices"
impacts the variables represented in the scatter plot, "color_var" changes the variable off which the scatter plot point colors
are based. 
```
observeEvent(input$colChoices...
observeEvent(input$scatChoices...
observeEvent(input$color_var...
```
Inside each "observeEvent" block of code, the inputs are used (as.numeric in order to be used to access columns from the CSV 
file based on column number) to actually create the table and graph using the code: 
```
output$table = DT::renderDataTable(datafile[,cols], filter = "top")

p <- ggplot(data = datafile2) + 
          geom_point(mapping = aes(x = datafile2[cols2[1]], 
                                   y = datafile2[cols2[2]],
                                   color = datafile2[colorVar])
```
"DT::renderDataTable" creates the table while "ggplot" is used to create the scatter plot.  Both the DT and ggplot2 libraries are
imported at the top of server.r. 

#Using the App
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
After the application is launched, the display screen will show the sidebar panel with three locations for the user to 
select inputs labeled "Select Columns for Data Table:", "Select Variables for Scatter Plot:", and "Select Variable for Plot Color 
Scale:".  The user can select as many variables as he or she wishes to display in the data table by clicking the empty text box and selecting
variables from the drop down menu. Furthermore, the table can be sorted by using the filters located at the top of each column.  The 
user can select any two continuous variables to be displayed in the scatter plot.  If the user selects a variable with discrete values,
such as strings and not integers, the app will display the error message below and no scatter plot will be produced:
```
Error: Discrete value supplied to continuous scale
```
Finally, the user can change the variable off which the color of the scatter plot points are based by selecting a different input from
the third drop down menu labeled, "Select Variable for Plot Color Scale:".  Changing this variable will alter the color of the points in
the scatter plot and also must be continuous. 














