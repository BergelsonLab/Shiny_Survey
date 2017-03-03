library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)


shinyServer(function(input, output) {
  datafile <- read_csv("final_cdi_merged_cleaned.csv")
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
  
  
  datafile2 <- read_csv("final_cdi_merged_cleaned.csv")
  observeEvent(input$scatChoices, {
    colorVar <- as.numeric(input$color_var)
    cols2 <- as.numeric(input$scatChoices)
    if(length(input$scatChoices) == 1){
      df2 <- data.frame(datafile2[,cols2])
      names(df2) <- names(datafile2)[cols2]
    }
    else{
      output$scat = renderPlot({
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
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
          
        p + labs(x = x_lab , y = y_lab, color = colorV)
      })
    }
  })
  
  observeEvent(input$color_var, {
    colorVar <- as.numeric(input$color_var)
    cols2 <- as.numeric(input$scatChoices)
    if(length(input$scatChoices) != 1){
      output$scat = renderPlot({
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
          ggtitle(title)+
          theme(plot.title = element_text(hjust = 0.5))
        
        p + labs(x = x_lab , y = y_lab, color = colorV)
      })
    }
  })
})
