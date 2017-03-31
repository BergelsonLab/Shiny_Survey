library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)

datafile1 <- read.csv("final_cdi_merged_cleaned.csv")

for(n in names(datafile1[,c(17:19, 458:508)])){
  datafile1[[n]] <- ifelse(datafile1[[n]] == 'Yes',1,0)
}

shinyServer(function(input, output) {
  datafile <- mutate(datafile1,
                     Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                     Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395,
                     Fraction_Total_Gesture = rowSums(datafile1[,458:508])/51)
  
  x <- reactive({input$radioButton})

  output$plot<-renderPlot({
    if(x() == 1){
      datafile_understand <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI, 
                                    Understand_childname:Understand_goforride, 
                                    Fraction_Total_Understand)
      datafile_understand_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                          Child_gender, Understand_childname:Understand_goforride,
                                          Fraction_Total_Understand)
      colorVar <- as.numeric(input$color_var_understand)
      cols2 <- as.numeric(input$plotChoices_Understand)
      cols <- as.numeric(input$colChoices_understand)

      if(length(input$colChoices_understand) == 1){
        df <- data.frame(datafile_understand_table[,cols])
        names(df) <- names(datafile_understand_table)[cols]
        output$table <- DT::renderDataTable(df, filter = "top")
      }
      else{
        output$table <- DT::renderDataTable(datafile_understand_table[,cols], filter = "top")
      }
      
      if(length(input$plotChoices_Understand) == 1){
        df2 <- data.frame(datafile_understand[,cols2])
        names(df2) <- names(datafile_understand)[cols2]
      }
      
      else if(cols2[2] > 33){
        x_lab <- colnames(datafile_understand)[cols2[1]]
        y_lab <- colnames(datafile_understand)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        p <- ggplot(data = datafile_understand) +
          geom_point(mapping = aes(x = datafile_understand[cols2[1]],
                                   y = datafile_understand[cols2[2]],
                                   color = datafile[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          stat_smooth(mapping = aes(x = datafile_understand[cols2[1]],
                                    y = datafile_understand[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        p + labs(x = x_lab , y = y_lab, color = colorV)
      }
      else{
        x_lab <- colnames(datafile_understand)[cols2[1]]
        y_lab <- colnames(datafile_understand)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        p <- ggplot(data = datafile_understand) +
          geom_bar(mapping = aes(x = datafile_understand[cols2[1]],
                                 y = datafile_understand[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        p + labs(x = x_lab, y = y_lab, color = colorV)
      }
    }
    else if(x() == 2){
      datafile_talk <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                              Talk_baabaa:Talk_some, Fraction_Total_Talk)
      datafile_talk_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                    Child_gender, Talk_baabaa:Talk_some, Fraction_Total_Talk)
      
      colorVar <- as.numeric(input$color_var_talk)
      cols2 <- as.numeric(input$plotChoices_Talk)
      cols <- as.numeric(input$colChoices_talk)
      
      if(length(input$colChoices_talk) == 1){
        df <- data.frame(datafile_talk_table[,cols])
        names(df) <- names(datafile_talk_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
      }
      else{
        output$table = DT::renderDataTable(datafile_talk_table[,cols], filter = "top")
      }
      
      if(length(input$plotChoices_Talk) == 1){
        df2 <- data.frame(datafile_talk[,cols2])
        names(df2) <- names(datafile_talk)[cols2]
      }
      
      else if(cols2[2] > 398){
        x_lab <- colnames(datafile_talk)[cols2[1]]
        y_lab <- colnames(datafile_talk)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        p <- ggplot(data = datafile_talk) +
          geom_point(mapping = aes(x = datafile_talk[cols2[1]],
                                   y = datafile_talk[cols2[2]],
                                   color = datafile[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          stat_smooth(mapping = aes(x = datafile_talk[cols2[1]],
                                    y = datafile_talk[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        p + labs(x = x_lab , y = y_lab, color = colorV)
      }
      else{
        x_lab <- colnames(datafile_talk)[cols2[1]]
        y_lab <- colnames(datafile_talk)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]
        
        p <- ggplot(data = datafile_talk) +
          geom_bar(mapping = aes(x = datafile_talk[cols2[1]],
                                 y = datafile_talk[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity")+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        p + labs(x = x_lab, y = y_lab, color = colorV)
      }
    }
    else if(x() == 3){
      datafile_gesture <- select(datafile, AgeMonthCDI_Uncorrected, AgeDaysCDI,
                                 Gestures_peekaboo:Gestures_wearglasses, Fraction_Total_Gesture)
      datafile_gesture_table <- select(datafile, SubjectNumber_Month:AgeMonthCDI_Corrected,
                                       Child_gender, Gestures_peekaboo:Gestures_wearglasses,
                                       Fraction_Total_Gesture)

      cols <- as.numeric(input$colChoices_gesture)
      if(length(input$colChoices_gesture) == 1){
        df <- data.frame(datafile_gesture_table[,cols])
        names(df) <- names(datafile_gesture_table)[cols]
        output$table = DT::renderDataTable(df, filter = "top")
      }
      else{
        output$table = DT::renderDataTable(datafile_gesture_table[,cols], filter = "top")
      }

      colorVar <- as.numeric(input$color_var_gestures)
      cols2 <- as.numeric(input$plotChoices)
      if(length(input$plotChoices) == 1){
        df2 <- data.frame(datafile_gesture[,cols2])
        names(df2) <- names(datafile_gesture)[cols2]
      }
      else if(cols2[2] > 53){
        x_lab <- colnames(datafile_gesture)[cols2[1]]
        y_lab <- colnames(datafile_gesture)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        
        p <- ggplot(data = datafile_gesture) +
          geom_point(mapping = aes(x = datafile_gesture[cols2[1]],
                                   y = datafile_gesture[cols2[2]],
                                   color = datafile[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          stat_smooth(mapping = aes(x = datafile_gesture[cols2[1]],
                                    y = datafile_gesture[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))

        p + labs(x = x_lab , y = y_lab, color = colorV)
      }
      else{
        x_lab <- colnames(datafile_gesture)[cols2[1]]
        y_lab <- colnames(datafile_gesture)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile)[colorVar]

        p <- ggplot(data = datafile_gesture) +
          geom_bar(mapping = aes(x = datafile_gesture[cols2[1]],
                                 y = datafile_gesture[cols2[2]],
                                 color = datafile[[colorV]]),
                   stat = "identity") +
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        p + labs(x = x_lab, y = y_lab, color = colorV)
      }
    }
  })
})