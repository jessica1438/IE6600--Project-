# server ----


server <- function(input, output, session) {
  values <- reactiveValues(
    dataset = NULL,
    plot_type = NULL
  )
  observe({
    values$df1_A <- as.data.frame(get(input$df1_A))
    values$df1_B <- as.data.frame(get(input$df1_B))
    values$Year1_A <- input$Year1_A
    values$Country1_A <- input$Country1_A
    values$Cause1_A <- input$Cause1_A
    values$Sex1_A <- input$Sex1_A
    values$Age1_A<- input$Age1_A
    
    values$df1_B <- as.data.frame(get(input$df1_B))
    values$Year1_B <- input$Year1_B
    values$Country1_B <- input$Country1_B
    values$Cause1_B <- input$Cause1_B
    values$Sex1_B <- input$Sex1_B
    values$Age1_B<- input$Age1_B
    
    values$Year2 <- input$Year2
    values$Country2 <- input$Country2
    values$Cause2 <- input$Cause2
    values$Sex2 <- input$Sex2
    values$Sort2 <- input$Sort2
    values$Age2<- input$Age2
    
    values$Year3_A <- input$Year3_A[1]:input$Year3_A[2]
    values$Country3_A <- input$Country3_A
    values$Cause3_A <- input$Cause3_A
    values$Type3_A<- input$Type3_A
    
    values$Year3_B <- input$Year3_B[1]:input$Year3_B[2]
    values$Country3_B <- input$Country3_B
    values$Cause3_B <- input$Cause3_B
    values$Sex3_B <- input$Sex3_B
    values$Age3_B<- input$Age3_B
    
    values$Year4_A <- input$Year4_A
    values$Country4_A <- input$Country4_A
    values$Cause4_A <- input$Cause4_A
    values$Sex4_A <- input$Sex4_A
    
    values$vb1_A <- vb(df=Morticd_final,country=values$Country1_A,year=values$Year1_A)
    
    values$vb1_B<- vb(df=Morticd_final,country=values$Country1_B,year=values$Year1_B)
    
  })
  
  output$bar1_A <- renderPlotly({
    histogram(df=values$df1_A,year = values$Year1_A,country = values$Country1_A,causes = values$Cause1_A,sex=values$Sex1_A,age_group=values$Age1_A)
  })
  
  output$bar1_B <- renderPlotly({
    histogram(df=values$df1_B,year = values$Year1_B,country = values$Country1_B,causes = values$Cause1_B,sex=values$Sex1_B,age_group=values$Age1_B)
  })  
  
  
  
  
  output$bar2_A <-renderPlotly({
    histogram_courses_distribution(df=Morticd_final,year=values$Year2,country=values$Country2,sex=values$Sex2 ,age_group=values$Age2,causes=values$Cause2,Sort=values$Sort2)
  })
  
  
  output$bar2_B <-renderPlot({
    tab3_bar1(df=Morticd_final,year=values$Year2,country=values$Country2,sex=values$Sex2 ,age_group=values$Age2,causes=values$Cause2,Sort=values$Sort2)
  })
  output$bar2_C <-renderPlot({
    tab3_bar2(df=Morticd_final,year=values$Year2,country=values$Country2,sex=values$Sex2 ,age_group=values$Age2,causes=values$Cause2,Sort=values$Sort2)
  })
  
  output$line3_A <- renderPlotly({
    line3_A(df=Morticd_final,year=values$Year3_A,country=values$Country3_A,causes=values$Cause3_A,type=values$Type3_A)
    
  })
  
  output$line3_B <- renderPlotly({
    line3_B(df=Morticd_final,year=values$Year3_B,country=values$Country3_B,causes=values$Cause3_B,age_group=values$Age3_B,sex=values$Sex3_B)
    
  })
  
  
  output$bar4_A <-renderPlot({
    par(mar = c(0, 0, 0, 0))
    plot4(data=Morticd_data,year=values$Year4_A ,country=values$Country4_A, sex=values$Sex4_A ,causes=values$Cause4_A)
  }
  )
  
  
  ##Tour observe
  observeEvent(input$btn,
               introjs(session))
  
  
  ##Top intro
  observeEvent("", {
    showModal(
      modalDialog(
        tags$style(type="text/css", "#img-container {display: flex; justify-content: center; align-items: center;}"),
        div(id = "img-container", img(src = "https://cdn.discordapp.com/attachments/708905184511656000/1094709964494352384/11f9af178d82700f7234613139d571f.png")),
        easyClose = TRUE
      ))
  })
  
  
  ## Pop AD
  appState <- reactiveValues(showPopup = TRUE, firstTime = TRUE)
  
  observe({
    if (appState$firstTime) {
      appState$firstTime <- FALSE
      return()
    }
    
    if (appState$showPopup) {
      appState$showPopup <- FALSE
      
      Sys.sleep(3.5)  #delay sec
      
      shinyalert(
        title = "",
        text = "This is a pop-up ads",
        imageUrl = "https://cdn.discordapp.com/attachments/708905184511656000/1094701098742325288/891294da9ff43b3dec37b5256a08104.png",
        imageWidth = 400,
        imageHeight = 200,
        type = "info",
        showCancelButton = TRUE,
        confirmButtonText = "Buy One Now",
        cancelButtonText = "Buy One Later",
        callbackR = function(value) {
          if (value) {
            removeModal(session)
          }
        }
      )
    }
  })
  
  ##valueBox
  output$VBox1 <- renderValueBox({
    valueBox(
      paste(values$vb1_A , "Deaths"), "Total Number of Country A Deaths", icon = icon("heartbeat"),
      color = "light-blue",
      width = "100%"
    )
  })
  
  output$VBox2 <- renderValueBox({
    valueBox(
      paste(values$vb1_B ,"Deaths"), "Total Number of Country B Deaths", icon = icon("ambulance"),
      color = "maroon",
      width = "100%"
    )
  })
  
}
