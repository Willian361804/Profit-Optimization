library(shiny)
library(GA)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Otimização de Transporte de Cargas"),
    hr(),
    tabsetPanel(
      tabPanel("Dados",
               fluidRow(
                 column(6,fileInput("arquivo","Selecione o arquivo",multiple = F,
                                    accept = c(".csv"))),
                 column(6,actionButton("Totais","Calcular Totais"))
               ),
               fluidRow(
                 column(3,h3(textOutput("TQuantidade"))),
                 column(3,h3(textOutput("TPesoTotal"))),
                 column(3,h3(textOutput("TVolumeTotal"))),
                 column(3,h3(textOutput("TValor")))
               ),
               fluidRow(
                 column(12,tableOutput("Dados"))
               )
      ),
      tabPanel("Processamento",
               fluidRow(
                 column(3,numericInput("sobrapeso","Informe a sobra de peso",
                                       value = 6000)),
                 column(3,numericInput("sobravolume","Informe a sobra de volume",
                                       value = 350)),
                 column(3,numericInput("iteracoes","Informe a quantidade de
                                       iterações",value = 10)),
                 column(3,actionButton("Processar","Processar"))
               ),
               fluidRow(
                 column(3,h3(textOutput("RQuantidade"))),
                 column(3,h3(textOutput("RPesoTotal"))),
                 column(3,h3(textOutput("RVolumeTotal"))),
                 column(3,h3(textOutput("RValor")))
               ),
               fluidRow(
                 column(12,tableOutput("Rfinal"))
               )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$Totais, {
    file1 = input$arquivo
    itens <<- read.csv(file1$datapath,sep=";") #Variável fica global
    z <<- nrow(itens)
    
    output$Dados = renderTable({itens})
    output$TQuantidade = renderText({ paste0("Quantidade de itens: ",z) })
    output$TPesoTotal = renderText({ paste0("Peso total: ",sum(itens$PESO)) })
    output$TVolumeTotal = renderText({ paste0("Volume total: ",sum(itens$VOLUME)) })
    output$TValor = renderText({ paste0("Valor total: ",sum(itens$VALOR)) })
  })
  
  observeEvent(input$Processar, {
    maxvolume = input$sobravolume
    maxpeso = input$sobrapeso
    
    f <-function(x)
    {
      valor = 0
      peso = 0
      volume = 0
      
      for (i in 1:z)
      {
        
        if (x[ i ] != 0)
        {
          
          valor = valor + itens[i,3]
          peso = peso +  itens[i,2]
          volume = volume +  itens[i,4]
          
        }
      }
      if ( volume > maxvolume | peso > maxpeso )
        valor = 0
      return(valor)
    }
    
    #algoritmo genetico
    resultado = ga("binary", fitness = f, nBits = z,popSize = 10, 
                   maxiter = input$iteracoes)
    result = t(as.data.frame(summary(resultado)$solution))
    result = itens[result[,1]==1,]
    output$Rfinal <- renderTable({result})
    
    output$RQuantidade = renderText({  paste0("Quantidade Final: ", 
                                              nrow(result)  )})
    output$RPesoTotal = renderText({ paste0("Peso Final: ", sum(result$PESO )) })
    output$RVolumeTotal = renderText({ paste0("Volume Final: ", 
                                              sum(result$VOLUME ))  })
    output$RValor = renderText({ paste0("Valor Total: ", sum(result$VALOR )) })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
