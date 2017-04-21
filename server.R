library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

# Read in csv file with survey data
datafile1 <- read.csv("final_cdi_merged_cleaned.csv")
datafile2 <- read.csv("final_motor_merged_cleaned.csv")

#Convert "Yes" values to 1; "No" values to 0 in csv file
for(n in names(datafile1[,c(17:19, 458:508)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Yes',1,0)
}

# Convert "Often" values to 2; "Sometimes" values to 1; "Never" values to 0
for(n in names(datafile1[,c(446:457)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Often',2,
                           ifelse(datafile1[[n]] == 'Sometimes',1,0))
}

# Convert "Often" values to 2; "Sometimes" values to 1; "Never" values to 0
for(n in names(datafile1[,c(48:49)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Often',2,
                           ifelse(datafile1[[n]] == 'Sometimes',1,0))
}


# Displays outputs based on inputs from user
shinyServer(function(input, output) {
  #Create new columns for datafile; fraction_total... (Add new columns to end of dataframe)
  datafile <- mutate(datafile1,
                     Fraction_Total_First_Signs_Understand = rowSums(datafile1[,c(17:19)])/3,
                     Fraction_Total_Understand_Phrases = rowSums(datafile1[,c(20:47, 50:445)] == 1)/422,
                     Fraction_Total_Start_Talk = rowSums(datafile1[,c(48:49)] == 2)/2,
                     Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                     Fraction_Total_Gestures1 = rowSums(datafile1[,446:457]>0)/12,
                     Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)
  
  # Create dataframe of all columns to be used when user wants to download full survey results with downloadData button
  dFull <- select(datafile, SubjectNumber_Month:Fraction_Total_Gesture)
  
  # Reactive element based on user input on radioButton and dataset to alter which set of data is being analyzed
  x <- reactive({input$radioButton})
  d <- reactive({input$dataset})
  
  output$plot<-renderPlot({
    #If user selects "First Signs of Understanding" in radioButton
    if(d() == 1 && x() == 1){
      # Download full dataset
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(dFull,file_out)
        }
      )
      
      #Create new dataframes for table with columns representing "First Signs of Understanding" data only
      datafile_first_signs_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                           Child_gender, Understand_childname:Understand_theresmommy,
                                           Fraction_Total_First_Signs_Understand)
      
      cols <- as.numeric(input$colChoices_first_signs_understand)

      #Display the table based on column choices from user
      if(length(input$colChoices_first_signs_understand) == 0){
        # Allow user to download csv file of all the First Signs of Understanding data (if no columns selected)
        output$down_filter_first_signs <- downloadHandler(
          filename = "Seedling_Filtered_First_Signs_Understand_Data.csv",
          content = function(file_out){
            write.csv(datafile_first_signs_table, file_out)
          }
        )
      }
      if(length(input$colChoices_first_signs_understand) == 1){
        df <- data.frame(datafile_first_signs_table[,cols])
        names(df) <- names(datafile_first_signs_table)[cols]
        output$table <- DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of First Signs of Understanding data that has been filtered
        # Filters determined by user on table output
        output$down_filter_first_signs <- downloadHandler(
          filename = "Seedling_Filtered_First_Signs_Understand_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_first_signs_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      if(length(input$colChoices_first_signs_understand) > 1){
        df <- data.frame(datafile_first_signs_table[,cols])
        names(df) <- names(datafile_first_signs_table)[cols]
        output$table <- DT::renderDataTable(datafile_first_signs_table[,cols], filter = "top")
        # Allow user to download csv file of First Signs of Understanding data that has been filtered
        # Filters determined by user on table output
        output$down_filter_first_signs <- downloadHandler(
          filename = "Seedling_Filtered_First_Signs_Understand_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_first_signs_table[s,cols , drop = FALSE], file_out)
          }
        )
      }

      # Create dataframe to be used to create the plot output (Represent only First Signs of Understanding data)
      datafile_first_signs_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                          Understand_childname:Understand_theresmommy,
                                          Fraction_Total_First_Signs_Understand)
      
      colorVar <- as.numeric(input$color_var_first_signs)
      cols2 <- as.numeric(input$plotChoices_First_Signs)
      
      #If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_First_Signs) == 1){
        df2 <- data.frame(datafile_first_signs_plot[,cols2])
        names(df2) <- names(datafile_first_signs_plot)[cols2]
      }

      # If selection is > 5, plot will display a scatter plot
      # Selection 6 is Fraction_Total_First_Signs_Understand
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 5){
        #Create axis labels
        x_lab <- colnames(datafile_first_signs_plot)[cols2[1]]
        y_lab <- colnames(datafile_first_signs_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_first_signs_plot) +
          # "geom_point" specifies scatter plot output 
          geom_point(mapping = aes(x = datafile_first_signs_plot[cols2[1]],
                                   y = datafile_first_signs_plot[cols2[2]],
                                   color = datafile[[colorV]]),
                     # "position_jitter" allows similar datapoints to be seen and not lost behind other points
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line on top of scatter plot
          stat_smooth(mapping = aes(x = datafile_first_signs_plot[cols2[1]],
                                    y = datafile_first_signs_plot[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to scatter plot axes
        p + labs(x = x_lab , y = y_lab, color = colorV)
        # Display plot
        print(p)
      }

      # Input is not fraction_total_first_signs; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_first_signs_plot)[cols2[1]]
        y_lab <- colnames(datafile_first_signs_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_first_signs_plot) +
          # "geom_bar" specifies bar chart output
          geom_bar(mapping = aes(x = datafile_first_signs_plot[cols2[1]],
                                 y = datafile_first_signs_plot[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add axes labels to chart
        p + labs(x = x_lab, y = y_lab, color = colorV)
        # Display plot
        print(p)
      }
    }
    
    #If user selects "Phrases Understood" in radioButton
    if(d() == 1 && x() == 2){
      # Download full dataset
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(dFull,file_out)
        } 
      )
      
      #Create new dataframes for plot and table with columns representing "Phrases Understood" data only
      datafile_understand_phrases_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                                                 Understand_areyouhungry:Understand_goforride, 
                                                 Fraction_Total_Understand_Phrases)
      datafile_understand_phrases_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                                  Child_gender, Understand_areyouhungry:Understand_goforride,
                                                  Fraction_Total_Understand_Phrases)
      
      colorVar <- as.numeric(input$color_var_phrases_understood)
      cols2 <- as.numeric(input$plotChoices_Phrases_Understood)
      cols <- as.numeric(input$colChoices_phrases_understand)
      
      #Display the table based on column choices from user
      if(length(input$colChoices_phrases_understand) == 0){
        # Allow user to download csv file of all the "Phrases Understood" data
        output$down_filter_phrases_understood <- downloadHandler(
          filename = "Seedling_Filtered_Phrases_Understand_Data.csv",
          content = function(file_out){
            write.csv(datafile_understand_phrases_table, file_out)
          }
        )
      }
      
      else if(length(input$colChoices_phrases_understand) == 1){
        df <- data.frame(datafile_understand_phrases_table[,cols])
        names(df) <- names(datafile_understand_phrases_table)[cols]
        output$table <- DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of Phrases Understood data that has been filtered
        # Filters determined by user on table output
        output$down_filter_phrases_understood <- downloadHandler(
          filename = "Seedling_Filtered_Phrases_Understand_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_understand_phrases_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      else if(length(input$colChoices_phrases_understand) > 1){
        df <- data.frame(datafile_understand_phrases_table[,cols])
        names(df) <- names(datafile_understand_phrases_table)[cols]
        output$table <- DT::renderDataTable(datafile_understand_phrases_table[,cols], filter = "top")
        # Allow user to download csv file of Phrases Understood data that has been filtered
        # Filters determined by user on table output
        output$down_filter_phrases_understood <- downloadHandler(
          filename = "Seedling_Filtered_Phrases_Understand_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_understand_phrases_table[s,cols , drop = FALSE], file_out)
          }
        )
      }
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_Phrases_Understood) == 1){
        df2 <- data.frame(datafile_understand_phrases_plot[,cols2])
        names(df2) <- names(datafile_understand_phrases_plot)[cols2]
      }
      
      # If selection is > 33, plot will display a scatter plot
      # Selection 34 is fraction_total_understand_phrases
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 30){
        #Create axis labels
        x_lab <- colnames(datafile_understand_phrases_plot)[cols2[1]]
        y_lab <- colnames(datafile_understand_phrases_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_understand_phrases_plot) +
          # "geom_point" specifies scatter plot for output
          geom_point(mapping = aes(x = datafile_understand_phrases_plot[cols2[1]],
                                   y = datafile_understand_phrases_plot[cols2[2]],
                                   color = datafile[[colorV]]),
                     # "position_jitter" allows overlying points to be seen and not hidden
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line on top of scatter plot
          stat_smooth(mapping = aes(x = datafile_understand_phrases_plot[cols2[1]],
                                    y = datafile_understand_phrases_plot[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to scatter plot axes
        p + labs(x = x_lab , y = y_lab, color = colorV)
        # display plot
        print(p)
      }
      
      # Input is not fraction_total_understand_phrases; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_understand_phrases_plot)[cols2[1]]
        y_lab <- colnames(datafile_understand_phrases_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_understand_phrases_plot) +
          # "geom_bar" specifies bar chart for output
          geom_bar(mapping = aes(x = datafile_understand_phrases_plot[cols2[1]],
                                 y = datafile_understand_phrases_plot[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add axes labels to chart
        p + labs(x = x_lab, y = y_lab, color = colorV)
        # display plot 
        print(p)
      }
    }

    #If user selects "Starting to Produce" in radioButton
    if(d() == 1 && x() == 3){
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(dFull,file_out)
        } 
      )
      
      #Create new dataframes for plot and table with columns representing "Starting to Produce" data only
      datafile_starting_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                             Child_gender, Understand_parroting:Understand_labeling, Fraction_Total_Start_Talk)
      datafile_starting_talk_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                            Understand_parroting:Understand_labeling, Fraction_Total_Start_Talk)
      
      colorVar <- as.numeric(input$color_var_start_talk)
      cols2 <- as.numeric(input$plotChoices_Starting_Talk)
      cols <- as.numeric(input$colChoices_starting_talk)
      
      #Display the table based on column choices from user
      if(length(input$colChoices_starting_talk) == 0){
        # Allow user to download csv file of all the understand data
        output$down_filter_start_talk <- downloadHandler(
          filename = "Seedling_Filtered_Start_Produce_Data.csv",
          content = function(file_out){
            write.csv(datafile_starting_talk_table, file_out)
          }
        )
      }
      else if(length(input$colChoices_starting_talk) == 1){
        df <- data.frame(datafile_starting_talk_table[,cols])
        names(df) <- names(datafile_starting_talk_table)[cols]
        output$table <- DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on table output
        output$down_filter_start_talk <- downloadHandler(
          filename = "Seedling_Filtered_Start_Produce_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_starting_talk_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      else if(length(input$colChoices_starting_talk) > 1){
        df <- data.frame(datafile_starting_talk_table[,cols])
        names(df) <- names(datafile_starting_talk_table)[cols]
        output$table <- DT::renderDataTable(datafile_starting_talk_table[,cols], filter = "top")
        # Allow user to download csv file of understand data that has been filtered
        # Filters determined by user on table output
        output$down_filter_start_talk <- downloadHandler(
          filename = "Seedling_Filtered_Start_Talk_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_starting_talk_table[s,cols , drop = FALSE], file_out)
          }
        )
      }
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_Starting_Talk) == 1){
        df2 <- data.frame(datafile_starting_talk_plot[,cols2])
        names(df2) <- names(datafile_starting_talk_plot)[cols2]
      }
      
      # If selection is > 4, plot will display a scatter plot
      # Selection 5 is Fraction_Total_Start_Talk
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 4){
        #Create axis labels
        x_lab <- colnames(datafile_starting_talk_plot)[cols2[1]]
        y_lab <- colnames(datafile_starting_talk_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_starting_talk_plot) +
          # "geom_point" specifies scatter plot output
          geom_point(mapping = aes(x = datafile_starting_talk_plot[cols2[1]],
                                   y = datafile_starting_talk_plot[cols2[2]],
                                   color = datafile[[colorV]]),
                     # Allows points to be seen and not hidden behind other points of same value
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line on top of scatter plot
          stat_smooth(mapping = aes(x = datafile_starting_talk_plot[cols2[1]],
                                    y = datafile_starting_talk_plot[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to scatter plot axes
        p + labs(x = x_lab , y = y_lab, color = colorV)
        # Display plot
        print(p)
      }
      
      # Input is not Fraction_Total_Start_Talk; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_starting_talk_plot)[cols2[1]]
        y_lab <- colnames(datafile_starting_talk_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_starting_talk_plot) +
          geom_bar(mapping = aes(x = datafile_starting_talk_plot[cols2[1]],
                                 y = datafile_starting_talk_plot[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add axes labels to chart
        p + labs(x = x_lab, y = y_lab, color = colorV)
        print(p)
      }
    }
    
    # If user selects "Vocab Checklist" in radioButton input location
    else if(d() == 1 && x() == 4){
      # User can download full CDI dataset
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(dFull,file_out)
        } 
      )
      
      # Create new dataframes for plot and table with columns representing "Vocab Checklist" data only
      datafile_talk <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                              Talk_baabaa:Talk_some, Fraction_Total_Talk)
      datafile_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                    Child_gender, Talk_baabaa:Talk_some, Fraction_Total_Talk)

      colorVar <- as.numeric(input$color_var_talk)
      cols2 <- as.numeric(input$plotChoices_Talk)
      cols <- as.numeric(input$colChoices_talk)

      # Display the table based on column choices from user
      if(length(input$colChoices_talk) == 0){
        # Allow user to download csv file of all the "Vocab Checklist" data
        output$down_filter_talk <- downloadHandler(
          filename = "Seedling_Filtered_Talk_Data.csv",
          content = function(file_out){
            write.csv(datafile_talk_table, file_out)
          }
        )
      }

      else if(length(input$colChoices_talk) == 1){
        df <- data.frame(datafile_talk_table[,cols])
        names(df) <- names(datafile_talk_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of "Vocab Checklist" data that has been filtered
        # Filters determined by user on table output
        output$down_filter_talk <- downloadHandler(
          filename = "Seedling_Filtered_Talk_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_talk_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      else if(length(input$colChoices_talk) > 1){
        output$table = DT::renderDataTable(datafile_talk_table[,cols], filter = "top")
        # Allow user to download csv file of "Vocab Checklist" data that has been filtered
        # Filters determined by user on table output
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
      # Selection 399 is fraction_total_talk
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 398){
        #Create axis labels
        x_lab <- colnames(datafile_talk)[cols2[1]]
        y_lab <- colnames(datafile_talk)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_talk) +
          # "geom_point" specifies scatter plot output
          geom_point(mapping = aes(x = datafile_talk[cols2[1]],
                                   y = datafile_talk[cols2[2]],
                                   color = datafile[[colorV]]),
                     # "position_jitter" allows overlying points to be seen and not hidden
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line to scatter plot
          stat_smooth(mapping = aes(x = datafile_talk[cols2[1]],
                                    y = datafile_talk[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))

        # Add axis labels to scatter plot
        p + labs(x = x_lab , y = y_lab, color = colorV)
        # Display plot
        print(p)
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
          # "geom_bar" specifies a bar chart output
          geom_bar(mapping = aes(x = datafile_talk[cols2[1]],
                                 y = datafile_talk[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add axis labels to bar chart
        p + labs(x = x_lab, y = y_lab, color = colorV)
        # Display plot
        print(p)
      }
    }

    # If user selects "Gestures_ASN" in radioButton input
    else if(d() == 1 && x() == 5){
      # Allow user to download full CDI dataset 
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(dFull,file_out)
        } 
      )
      
      # Create new dataframes for plot and table with columns representing "gestures" data only
      datafile_gestures1_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                         Child_gender, Gestures_showobject:Gestures_allgone, Fraction_Total_Gestures1)
      
      datafile_gestures1_plot <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                                        Gestures_showobject:Gestures_allgone, Fraction_Total_Gestures1)
      
      cols <- as.numeric(input$colChoices_gesture1)
      
      # Display the table based on column choices from user
      if(length(input$colChoices_gesture1) == 0){
        # Allow user to download csv file of all the "Gestures_ASN" data
        output$down_filter_gesture1 <- downloadHandler(
          filename = "Seedling_Filtered_GesturesASN_Data.csv",
          content = function(file_out){
            write.csv(datafile_gestures1_table, file_out)
          }
        )
      }
      else if(length(input$colChoices_gesture1) == 1){
        df <- data.frame(datafile_gestures1_table[,cols])
        names(df) <- names(datafile_gestures1_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of "Gestures_ASN" data that has been filtered
        # Filters determined by user on table output
        output$down_filter_gesture1 <- downloadHandler(
          filename = "Seedling_Filtered_GesturesASN_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_gestures1_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      else if(length(input$colChoices_gesture1) > 1){
        output$table = DT::renderDataTable(datafile_gestures1_table[,cols], filter = "top")
        # Allow user to download csv file of "Gestures_ASN" data that has been filtered
        # Filters determined by user on table output
        output$down_filter_gesture1 <- downloadHandler(
          filename = "Seedling_Filtered_GesturesASN_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_gestures1_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      colorVar <- as.numeric(input$color_var_gestures1)
      cols2 <- as.numeric(input$plotChoices_gestures1)
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_gestures1) == 1){
        df2 <- data.frame(datafile_gestures1_plot[,cols2])
        names(df2) <- names(datafile_gestures1_plot)[cols2]
      }
      
      # If selection is > 14, plot will display a scatter plot
      # Selection 15 is Fraction_Total_Gestures1
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 14){
        #Create axis labels
        x_lab <- colnames(datafile_gestures1_plot)[cols2[1]]
        y_lab <- colnames(datafile_gestures1_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_gestures1_plot) +
          # "geom_point" specifies scatter plot output
          geom_point(mapping = aes(x = datafile_gestures1_plot[cols2[1]],
                                   y = datafile_gestures1_plot[cols2[2]],
                                   color = datafile[[colorV]]),
                     # "position_jitter" allows overlying points to be seen and not hidden
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line to scatter plot
          stat_smooth(mapping = aes(x = datafile_gestures1_plot[cols2[1]],
                                    y = datafile_gestures1_plot[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        # Add axis labels to scatter plot
        p + labs(x = x_lab , y = y_lab, color = colorV)
        # Display plot
        print(p)
      }
      
      # Input is not Fraction_Total_Gestures1; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_gestures1_plot)[cols2[1]]
        y_lab <- colnames(datafile_gestures1_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_gestures1_plot) +
          # "geom_bar" specifies bar chart output
          geom_bar(mapping = aes(x = datafile_gestures1_plot[cols2[1]],
                                 y = datafile_gestures1_plot[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity") +
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to bar chart axes
        p + labs(x = x_lab, y = y_lab, color = colorV)
        # Display plot
        print(p)
      }
    }
    
    # If user selects "Games and Routines" in radioButton input location
    else if(d() == 1 && x() == 6){
      # Allow user to download full CDI dataset
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_CDI_Data.csv",
        content = function(file_out){
          write.csv(dFull,file_out)
        } 
      )
      
      # Create new dataframes for plot and table with columns representing "Games and Routines" data only
      datafile_gesture <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                 Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
      datafile_gesture_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                       Child_gender, Gestures_peekaboo:Gestures_wearglasses,
                                       Fraction_Total_Gesture)

      cols <- as.numeric(input$colChoices_gesture)

      # Display the table based on column choices from user
      if(length(input$colChoices_gesture) == 0){
        # Allow user to download csv file of all the "Games and Routines" data
        output$down_filter_gesture <- downloadHandler(
          filename = "Seedling_Filtered_Games_Routines_Data.csv",
          content = function(file_out){
            write.csv(datafile_gesture_table, file_out)
          }
        )
      }
      else if(length(input$colChoices_gesture) == 1){
        df <- data.frame(datafile_gesture_table[,cols])
        names(df) <- names(datafile_gesture_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of "Games and Routines" data that has been filtered
        # Filters determined by user on table output
        output$down_filter_gesture <- downloadHandler(
          filename = "Seedling_Filtered_Games_Routines_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_gesture_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      else if(length(input$colChoices_gesture) > 1){
        output$table = DT::renderDataTable(datafile_gesture_table[,cols], filter = "top")
        # Allow user to download csv file of "Games and Routines" data that has been filtered
        # Filters determined by user on table output
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
      # Selection 54 is fraction_total_gestures
      # This variable is better visualized by scatter plot than bar chart
      else if(cols2[2] > 53){
        #Create axis labels
        x_lab <- colnames(datafile_gesture)[cols2[1]]
        y_lab <- colnames(datafile_gesture)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_gesture) +
          # "geom_point" specifies scatter plot for ouput 
          geom_point(mapping = aes(x = datafile_gesture[cols2[1]],
                                   y = datafile_gesture[cols2[2]],
                                   color = datafile[[colorV]]),
                     # "position_jitter" allows overlying points to be seen and not hidden
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line to scatter plot
          stat_smooth(mapping = aes(x = datafile_gesture[cols2[1]],
                                    y = datafile_gesture[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))

        # Add axis labels to scatter plot
        p + labs(x = x_lab , y = y_lab, color = colorV)
        # Display plot
        print(p)
      }

      # Input is not fraction_total_gestures; display a bar chart
      else{
        # Create axis labels
        x_lab <- colnames(datafile_gesture)[cols2[1]]
        y_lab <- colnames(datafile_gesture)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        # Construct the bar chart from cols2 inputs
        p <- ggplot(data = datafile_gesture) +
          # "geom_bar" specifies bar chart for output
          geom_bar(mapping = aes(x = datafile_gesture[cols2[1]],
                                 y = datafile_gesture[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity") +
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        # Add labels to bar chart axes
        p + labs(x = x_lab, y = y_lab, color = colorV)
        # Display plot
        print(p)
      }
    }
    
    # If user selects "Motor" dataset instead of "CDI" dataset
    else if(d() == 2){
      # Create dataframes from the motor survey csv file to be used in table and plot outputs
      datafile_motor_plot <- select(datafile2, Age_Corrected, ageweight,
                                    rest_on_body:crawling_belly, crawling_hands_knees,
                                    cruising, walking)
      datafile_motor_table <- select(datafile2, Subject.Number_Month:LocationLongitude)
      
      # Allow user to download full motor survey results 
      output$downloadData <- downloadHandler(
        filename = "Seedling_Survey_All_Motor_Data.csv",
        content = function(file_out){
          write.csv(datafile_motor_table,file_out)
        }
      )
      
      cols <- as.numeric(input$colChoices_motor)
      
      # Display the table based on column choices from user
      if(length(input$colChoices_motor) == 0){
        # Allow user to download csv file of all the motor survey data
        output$down_filter_motor <- downloadHandler(
          filename = "Seedling_Filtered_Motor_Data.csv",
          content = function(file_out){
            write.csv(datafile_motor_table, file_out)
          }
        )
      }
      
      else if(length(input$colChoices_motor) == 1){
        df <- data.frame(datafile_motor_table[,cols])
        names(df) <- names(datafile_motor_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
        # Allow user to download csv file of Motor data that has been filtered
        # Filters determined by user on table output
        output$down_filter_motor <- downloadHandler(
          filename = "Seedling_Filtered_Motor_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_motor_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      else if(length(input$colChoices_motor) > 1){
        output$table = DT::renderDataTable(datafile_motor_table[,cols], filter = "top")
        # Allow user to download csv file of Motor data that has been filtered
        # Filters determined by user on table output
        output$down_filter_motor <- downloadHandler(
          filename = "Seedling_Filtered_Motor_Data.csv",
          content = function(file_out){
            s = input$table_rows_all
            write.csv(datafile_motor_table[s,cols,drop = FALSE], file_out)
          }
        )
      }
      
      
      colorVar <- as.numeric(input$color_var_motor)
      cols2 <- as.numeric(input$plotChoices_motor)
      
      # If only one input for plot choices; no plot can be created
      if(length(input$plotChoices_motor) == 1){
        df2 <- data.frame(datafile_motor_plot[,cols2])
        names(df2) <- names(datafile_motor_plot)[cols2]
      }
      
      # Create plot output; since motor is on scale of 0-5 instead of 0-1 and 0-2 in CDI;
      # all plots are scatter plots and not bar charts
      else{
        # Set axis label names
        x_lab <- colnames(datafile_motor_plot)[cols2[1]]
        y_lab <- colnames(datafile_motor_plot)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile_motor_table)[colorVar]
        
        # Construct the plot from cols2 inputs
        p <- ggplot(data = datafile_motor_plot) +
          # "geom_point" specifies scatter plot output
          geom_point(mapping = aes(x = datafile_motor_plot[cols2[1]],
                                   y = datafile_motor_plot[cols2[2]],
                                   color = datafile_motor_table[[colorV]]),
                     # "position_jitter" allows overlaying points to be seen and not hidden
                     position = position_jitter(w = 0.2, h = 0.07),
                     alpha = .35) +
          # Add trend line to scatter plot
          stat_smooth(mapping = aes(x = datafile_motor_plot[cols2[1]],
                                    y = datafile_motor_plot[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        p + labs(x = x_lab, y = y_lab, color = colorV)
        # Display plot output
        print(p)
      }
    }
  })
})