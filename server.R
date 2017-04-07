library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

# Read in csv file with survey data
datafile1 <- read.csv("final_cdi_merged_cleaned.csv")

# Convert "Yes" values to 1; "No" values to 0 in csv file
for(n in names(datafile1[,c(17:19, 458:508)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Yes',1,0)
}

# Displays outputs based on inputs from user
shinyServer(function(input, output) {
  # Create new columns for datafile; fraction_total_Understand/Talk/Gesture
  datafile <- mutate(datafile1,
                     Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                     Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                     Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)
  
  # Allow user to download data in csv file before filtering
  output$downloadData <- downloadHandler(
    filename = "Seedling_Survey_AllData.csv",
    content = function(file_out){
      write.csv(datafile,file_out)
    }
  )
  
  #Reactive element based on user input on radioButton to alter which set of data is being analyzed
  x <- reactive({input$radioButton})


  output$plot<-renderPlot({
    #If user selects "Understand" in radioButton
    if(x() == 1){
      #Create new dataframes for plot and table with columns representing "understand" data only
      datafile_understand <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                                    Understand_childname:Understand_goforride, 
                                    Fraction_Total_Understand)
      datafile_understand_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                          Child_gender, Understand_childname:Understand_goforride,
                                          Fraction_Total_Understand)
      colorVar <- as.numeric(input$color_var_understand)
      cols2 <- as.numeric(input$plotChoices_Understand)
      cols <- as.numeric(input$colChoices_understand)
      
      #Display the table based on column choices from user 
      if(length(input$colChoices_understand) == 1){
        df <- data.frame(datafile_understand_table[,cols])
        names(df) <- names(datafile_understand_table)[cols]
        output$table <- DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on application table output
        output$down_filter_understand <- downloadHandler(
          filename = "Seedling_Filtered_Understand_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_understand_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      else{
        df <- data.frame(datafile_understand_table[,cols])
        names(df) <- names(datafile_understand_table)[cols]
        output$table <- DT::renderDataTable(datafile_understand_table[,cols], filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on application table output
        output$down_filter_understand <- downloadHandler(
          filename = "Seedling_Filtered_Understand_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_understand_table[s,cols , drop = FALSE], file_out)
          }
        )
      }
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_Understand) == 1){
        df2 <- data.frame(datafile_understand[,cols2])
        names(df2) <- names(datafile_understand)[cols2]
      }
      
      # If selection is > 33, plot will display a scatter plot 
      # Selection 34 is fraction_total_understand
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 33){
        #Create axis labels 
        x_lab <- colnames(datafile_understand)[cols2[1]]
        y_lab <- colnames(datafile_understand)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the plot from cols2 inputs 
        p <- ggplot(data = datafile_understand) +
          geom_point(mapping = aes(x = datafile_understand[cols2[1]],
                                   y = datafile_understand[cols2[2]],
                                   color = datafile[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          # Add trend line on top of scatter plot 
          stat_smooth(mapping = aes(x = datafile_understand[cols2[1]],
                                    y = datafile_understand[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to scatter plot axes 
        p + labs(x = x_lab , y = y_lab, color = colorV)
      }
      
      # Input is not fraction_total_understand; display a bar chart
      else{
        # Create axis labels 
        x_lab <- colnames(datafile_understand)[cols2[1]]
        y_lab <- colnames(datafile_understand)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_understand) +
          geom_bar(mapping = aes(x = datafile_understand[cols2[1]],
                                 y = datafile_understand[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add axes labels to chart
        p + labs(x = x_lab, y = y_lab, color = colorV)
      }
    }
    
    # If user selects "Talk" in radioButton input location
    else if(x() == 2){
      # Create new dataframes for plot and table with columns representing "talk" data only
      datafile_talk <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                              Talk_baabaa:Talk_some, Fraction_Total_Talk)
      datafile_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                    Child_gender, Talk_baabaa:Talk_some, Fraction_Total_Talk)
      
      colorVar <- as.numeric(input$color_var_talk)
      cols2 <- as.numeric(input$plotChoices_Talk)
      cols <- as.numeric(input$colChoices_talk)
      
      # Display the table based on column choices from user
      if(length(input$colChoices_talk) == 1){
        df <- data.frame(datafile_talk_table[,cols])
        names(df) <- names(datafile_talk_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on application table output
        output$down_filter_talk <- downloadHandler(
          filename = "Seedling_Filtered_Talk_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_talk_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      else{
        output$table = DT::renderDataTable(datafile_talk_table[,cols], filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on application table output
        output$down_filter_talk <- downloadHandler(
          filename = "Seedling_Filtered_Talk_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_talk_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_Talk) == 1){
        df2 <- data.frame(datafile_talk[,cols2])
        names(df2) <- names(datafile_talk)[cols2]
      }
      
      # If selection is > 398, plot will display a scatter plot 
      # Selection 34 is fraction_total_talk
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 398){
        #Create axis labels
        x_lab <- colnames(datafile_talk)[cols2[1]]
        y_lab <- colnames(datafile_talk)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the plot from cols2 inputs 
        p <- ggplot(data = datafile_talk) +
          geom_point(mapping = aes(x = datafile_talk[cols2[1]],
                                   y = datafile_talk[cols2[2]],
                                   color = datafile[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          # Add trend line to scatter plot
          stat_smooth(mapping = aes(x = datafile_talk[cols2[1]],
                                    y = datafile_talk[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        # Add axis labels to scatter plot
        p + labs(x = x_lab , y = y_lab, color = colorV)
      }
      
      # Input is not fraction_total_talk; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_talk)[cols2[1]]
        y_lab <- colnames(datafile_talk)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_talk) +
          geom_bar(mapping = aes(x = datafile_talk[cols2[1]],
                                 y = datafile_talk[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add axis labels to bar chart
        p + labs(x = x_lab, y = y_lab, color = colorV)
      }
    }
    
    # If user selects "Gestures" in radioButton input location
    else if(x() == 3){
      # Create new dataframes for plot and table with columns representing "gestures" data only
      datafile_gesture <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                 Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
      datafile_gesture_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                       Child_gender, Gestures_peekaboo:Gestures_wearglasses,
                                       Fraction_Total_Gesture)

      cols <- as.numeric(input$colChoices_gesture)
      
      # Display the table based on column choices from user
      if(length(input$colChoices_gesture) == 1){
        df <- data.frame(datafile_gesture_table[,cols])
        names(df) <- names(datafile_gesture_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on application table output
        output$down_filter_gesture <- downloadHandler(
          filename = "Seedling_Filtered_Gestures_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_gesture_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      else{
        output$table = DT::renderDataTable(datafile_gesture_table[,cols], filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on application table output
        output$down_filter_gesture <- downloadHandler(
          filename = "Seedling_Filtered_Gestures_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_gesture_table[s,cols,drop = FALSE], file_out)
          }
        )
      }

      colorVar <- as.numeric(input$color_var_gestures)
      cols2 <- as.numeric(input$plotChoices)
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices) == 1){
        df2 <- data.frame(datafile_gesture[,cols2])
        names(df2) <- names(datafile_gesture)[cols2]
      }
      
      # If selection is > 53, plot will display a scatter plot 
      # Selection 34 is fraction_total_gestures
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 53){
        #Create axis labels
        x_lab <- colnames(datafile_gesture)[cols2[1]]
        y_lab <- colnames(datafile_gesture)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the plot from cols2 inputs 
        p <- ggplot(data = datafile_gesture) +
          geom_point(mapping = aes(x = datafile_gesture[cols2[1]],
                                   y = datafile_gesture[cols2[2]],
                                   color = datafile[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          # Add trend line to scatter plot
          stat_smooth(mapping = aes(x = datafile_gesture[cols2[1]],
                                    y = datafile_gesture[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        # Add axis labels to scatter plot
        p + labs(x = x_lab , y = y_lab, color = colorV)
      }
      
      # Input is not fraction_total_understand; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_gesture)[cols2[1]]
        y_lab <- colnames(datafile_gesture)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_gesture) +
          geom_bar(mapping = aes(x = datafile_gesture[cols2[1]],
                                 y = datafile_gesture[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity") +
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to bar chart axes
        p + labs(x = x_lab, y = y_lab, color = colorV)
      }
    }
  })
})