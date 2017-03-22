library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)


shinyServer(function(input, output) {
  datafile1 <- read.csv("final_cdi_merged_cleaned.csv")
  datafile <- mutate(datafile1,
                     #Total_Understand = rowSums(datafile1[,20:47]),
                     Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                     Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395)
  observeEvent(input$colChoices, {
    cols <- as.numeric(input$colChoices)
    if(length(input$colChoices) == 1){
      df <- data.frame(datafile[,cols])
      names(df) <- names(datafile)[cols]
      output$table = DT::renderDataTable(df, filter = "top")
    }
    else{
      output$table = DT::renderDataTable(datafile[,cols], filter = "top")
    }
  })
  
  datafile1 <- read.csv("final_cdi_merged_cleaned.csv")
  datafile2 <- mutate(datafile1,
                     #Total_Understand = rowSums(datafile1[,20:47]),
                     Fraction_Total_Understand = rowSums(datafile1[,c(17:47, 50:445)] == 1)/425,
                     Fraction_Total_Talk = rowSums(datafile1[,50:445] > 1)/395)
  observeEvent(input$scatChoices, {
    colorVar <- as.numeric(input$color_var)
    cols2 <- as.numeric(input$scatChoices)
    if(length(input$scatChoices) == 1){
      df2 <- data.frame(datafile2[,cols2])
      names(df2) <- names(datafile2)[cols2]
    }
    
    else if(cols2[2] > 510){
      output$plot = renderPlot({
        x_lab <- colnames(datafile2)[cols2[1]]
        y_lab <- colnames(datafile2)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile2)[colorVar]
        
        p <- ggplot(data = datafile2) +
          geom_point(mapping = aes(x = datafile2[cols2[1]],
                                   y = datafile2[cols2[2]],
                                   color = datafile2[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          stat_smooth(mapping = aes(x = datafile2[cols2[1]],
                                    y = datafile2[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        p + labs(x = x_lab , y = y_lab, color = colorV)
      })
    }
    
    else{
      output$plot = renderPlot({
        x_lab <- colnames(datafile2)[cols2[1]]
        y_lab <- colnames(datafile2)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile2)[colorVar]
        
        p <- ggplot(data = datafile2) +
          geom_bar(mapping = aes(x = datafile2[cols2[1]],
                                 y = datafile2[cols2[2]],
                                 color = datafile2[[colorV]]),
                                 stat = "identity")
        p + labs(x = x_lab, y = y_lab, color = colorV)
      })
    }
  })
    
  # observeEvent(input$color_var, {
  #   colorVar <- as.numeric(input$color_var)
  #   cols2 <- as.numeric(input$scatChoices)
  #   if(length(input$scatChoices) != 1){
  #     output$plot = renderPlot({
  #       x_lab <- colnames(datafile2)[cols2[1]]
  #       y_lab <- colnames(datafile2)[cols2[2]]
  #       title <- paste(x_lab, "vs.", y_lab, sep = " ")
  #       colorV <- colnames(datafile2)[colorVar]
  #       
  #       p <- ggplot(data = datafile2) +
  #         geom_point(mapping = aes(x = datafile2[cols2[1]],
  #                                  y = datafile2[cols2[2]],
  #                                  color = datafile2[[colorV]]),
  #                    position = position_jitter(w = 0.2, h = 0.1),
  #                    alpha = .6) +
  #         stat_smooth(mapping = aes(x = datafile2[cols2[1]],
  #                                   y = datafile2[cols2[2]]))+
  #         ggtitle(title)+
  #         theme(plot.title = element_text(hjust = 0.5))
  #       
  #       p + labs(x = x_lab , y = y_lab, color = colorV)
  #     })
  #   }
  # })
  
  observeEvent(input$color_var, {
    colorVar <- as.numeric(input$color_var)
    cols2 <- as.numeric(input$scatChoices)
    if(length(input$scatChoices) == 1){
      df2 <- data.frame(datafile2[,cols2])
      names(df2) <- names(datafile2)[cols2]
    }
    else if(cols2[2] > 510){
      output$plot = renderPlot({
        x_lab <- colnames(datafile2)[cols2[1]]
        y_lab <- colnames(datafile2)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile2)[colorVar]
        
        p <- ggplot(data = datafile2) +
          geom_point(mapping = aes(x = datafile2[cols2[1]],
                                   y = datafile2[cols2[2]],
                                   color = datafile2[[colorV]]),
                     position = position_jitter(w = 0.2, h = 0.1),
                     alpha = .6) +
          stat_smooth(mapping = aes(x = datafile2[cols2[1]],
                                    y = datafile2[cols2[2]]))+
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        p + labs(x = x_lab , y = y_lab, color = colorV)
      })
    }
    else{
      output$plot = renderPlot({
        x_lab <- colnames(datafile2)[cols2[1]]
        y_lab <- colnames(datafile2)[cols2[2]]
        title <- paste(x_lab, "vs.", y_lab, sep = " ")
        colorV <- colnames(datafile2)[colorVar]
        
        p <- ggplot(data = datafile2) +
          geom_bar(mapping = aes(x = datafile2[cols2[1]],
                                 y = datafile2[cols2[2]],
                                 color = datafile2[[colorV]]),
                   stat = "identity")
        p + labs(x = x_lab, y = y_lab, color = colorV)
      })
    }
  })
  })
