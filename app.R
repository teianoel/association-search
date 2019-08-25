library(shiny)
library(ggplot2)
library(acepack)
library(energy)
library(minerva)
library(data.table)

ui <- fluidPage(
  titlePanel("The Search for an Association"),
  sidebarPanel(
    fileInput("inFile", "Upload a .csv file with your x and y variables")
  ),
  mainPanel(
    tableOutput("dataTable"),
    plotOutput("scatterPlot"),
    downloadButton("downloadScatterPlot","Download"),
    tableOutput("correlationTable"),
    downloadButton("downloadCorrTable","Download"),
    plotOutput("corrBarPlot"),
    downloadButton("downloadCorrBarPlot","Download")
  )
)

server <- function(input, output) {
  readDat <- reactive({
    if(is.null(input$inFile)){
      return(NULL)
    }
    read.csv(input$inFile$datapath)
  })
  
  output$dataTable <- renderTable({
    head(readDat(),10)
  })
  
  scatterPlotter <- reactive({
    data <- readDat()
    if(is.null(data)){
      return(NULL)
    }
    data_temp <- as.data.frame(data)
    colnames(data_temp) <- c("var_1","var_2")
    ggplot(data_temp, aes(x=var_1, y=var_2)) + 
      geom_point() +
      ggtitle("") +
      xlab(colnames(data)[1])+
      ylab(colnames(data)[2])
  })
  
  output$scatterPlot <- renderPlot({
    scatterPlotter()
  })
  
  output$downloadScatterPlot <- downloadHandler(
    filename = function(){
      "ScatterPlot.pdf"
    },
    content = function(file){
      pdf(file)
      print(scatterPlotter())
      dev.off()
    }
  )
  
  correlationTableGenerator <- reactive({
    data <- readDat()
    if(is.null(data)){
      return(NULL)
    }
    correlations <- c()
    pearson_cor <- cor(data[1],data[2],method="pearson")
    correlations[1] <- pearson_cor[1]
    spearman_cor <- cor(data[1],data[2],method="spearman")
    correlations[2] <- spearman_cor[1]
    maximal_cor <- ace(as.numeric(unlist(data[1])),as.numeric(unlist(data[2])))$rsq
    correlations[3] <- maximal_cor
    distance_cor <- dcor(data[1],data[2])
    correlations[4] <- distance_cor
    maximalinfo_cor <- mine(as.numeric(unlist(data[1])),as.numeric(unlist(data[2])))$MIC
    correlations[5] <- maximalinfo_cor
    correlations <- as.data.frame(correlations)
    colnames(correlations) <- c("Correlations")
    correlations$names <- c("Pearson", "Spearman", "Maximal", "Distance", "Maximal Info")
    correlations <- as.data.frame(correlations[order(correlations$Correlations),])
    correlations_named <- correlations[,-2]
    correlations_named <- as.data.frame(correlations_named)
    colnames(correlations_named) <- c("Correlations")
    row.names(correlations_named) <- correlations[,2]
    correlations_named
  })
  
  output$correlationTable <- renderTable({
    correlationTableGenerator()
  }, rownames = TRUE)
  
  output$downloadCorrTable <- downloadHandler(
    filename = function(){
      "CoeffTable.csv"
    },
    content = function(file){
      table<-as.data.frame(correlationTableGenerator())
      write.csv(table,file)
    }
  )
  
  # label plot!
  corrBarPlotter <- reactive({
    correlations <- correlationTableGenerator()
    correlations <- as.data.frame(correlations)
    correlations$names <- row.names(correlations)
    ggplot(data=correlations, aes(x=reorder(names,Correlations),y=Correlations)) +
      geom_bar(stat="identity",alpha = 1/5,colour="red",fill="blue")+
      xlab("")+
      #geom_text(aes(label=Correlations), vjust=-0.3, size=3.5)+
      theme_minimal()
  })
  
  output$corrBarPlot <- renderPlot({
    corrBarPlotter()
  })
  
  #not working properly
  output$downloadCorrBarPlot <- downloadHandler(
    filename = function(){
      "CorrBarPlot.pdf"
    },
    content = function(file){
      pdf(file)
      print(corrBarPlotter())
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)