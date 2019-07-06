#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("Metodos.R")
library(shiny)
library(plotly)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Numerical Recipes"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
       #  sliderInput("bins",
        #             "Number of bins:",
         #            min = 1,
          #           max = 50,
           #          value = 30),
         textInput(inputId = "Formula",label="Digite"),
         numericInput('ValorInicial',label = "Valor Inicial",value=0),
         numericInput("fim",label="Valor final",value=1),
         numericInput("Tolerancia",label = "Digite a tolerancia",value = 1e-4),
         selectInput("Metodo",label = "Escolha o método de encontrar raizes",choices = c('Bicessão','Newton','Ponto Fixo','Secante')),
         actionButton("Botao",label = "Calcular")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot"),
         plotlyOutput("ObservarMetodo"),
       plotOutput("Convergencia")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  rescreverFuncao<-eventReactive(input$Botao,{
    fileConn<-file("Funcao.R")
    string=paste('return(',input$Formula,')',sep=""   )
    writeLines(c("f<-function(x){",string,'}'), fileConn)
    close(fileConn)
  })
  
  GerarGrafico<-function(a,b){
    a=as.numeric(a)
    b<-as.numeric(b)
    print("teste")
    print(c(a,b))
    sequencia= seq(from=a,to=b,by=(b-a)/100.0)
    df=data.frame(sequencia,f(sequencia))
    names(df)=c('x','fx')
    return(df)
    
  }
  
  AnaliseMetodo<-eventReactive(input$Botao,{
    a<-input$ValorInicial
    b<-input$fim
    source("Funcao.R")
    Grafico<-GerarGrafico(a,b)
    return(Grafico)
  })
  
   output$distPlot <- renderPlot({
     rescreverFuncao()
      # generate bins based on input$bins from ui.R
     Frame<-AnaliseMetodo()
     require(ggplot2)
     ggplot(data=Frame) + geom_line(aes(x=x,y=fx) )
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
   })
   
   #output$ObservarMetodo<-renderPlot({
    # rescreverFuncao()
    # Frame<-AnaliseMetodo()
    # require(ggplot2)
    # Tentativas<-bisection(a=input$inicio,b=input$fim,tol=1e-3)
    # Valoresx=Tentativas[[1]]
    # Valoresy=Tentativas[[2]]
    # print(Valoresy)
    # for(i in 1:length(Valoresx)){
    #   print(ggplot(data=Frame) + geom_line(aes(x=x,y=fx) ) + geom_point(aes(x=Valoresx[i],y=Valoresy[i],col="red")))
     #  Sys.sleep(5)
    #   }
     
   #})
   
   output$ObservarMetodo<-renderPlotly({
     rescreverFuncao()
     require(plotly)
     Frame<-AnaliseMetodo()
     require(ggplot2)
     if(input$Metodo=='Bicessão')
      Tentativas<-bisection(a=input$ValorInicial,b=input$fim,tol=input$Tolerancia)
     else if(input$Metodo=='Newton')
      Tentativas<-newton.raphson(a=input$ValorInicial,b=input$fim,tol=input$Tolerancia) 
     else if(input$Metodo == 'Ponto Fixo')
       Tentativas<-fixedpoint(x0=input$ValorInicial,tol=input$Tolerancia)
     else if(input$Metodo == 'Secante')
       Tentativas<-secant(x0=input$ValorInicial,x1=input$fim,tol=input$Tolerancia)
       
     Valoresx=Tentativas[[1]]
     Valoresy=Tentativas[[2]]
     l=list()
     for(i in 1:length(Valoresx)){
      aux=data.frame(Frame,i)
      names(aux)[ncol(aux)]='f'
      l[[i]]=aux
     }
     Frame=Reduce(rbind,l)
     print(dim(Frame))
     Evolucao=data.frame(Valoresx,Valoresy,1:length(Valoresx))
     print(dim(Evolucao) )
     names(Evolucao)=c('x','y','t')
     print(Valoresy)
     print(Evolucao)
    p1=ggplot() + geom_line(aes(x=Frame$x,y=Frame$fx,frame=Frame$f) ) + geom_point(aes(x=Evolucao$x,y=Evolucao$y,col="red",frame=Evolucao$t)) +labs(x='x',y='f(x)')
     ggplotly(p1)
     
   })
   
   output$Convergencia<-renderPlot({
     require(plotly)
    Frame<-AnaliseMetodo()
     require(ggplot2)
     if(input$Metodo=='Bicessão')
       Tentativas<-bisection(a=input$ValorInicial,b=input$fim,tol=input$Tolerancia)
     else if(input$Metodo=='Newton')
       Tentativas<-newton.raphson(a=input$ValorInicial,b=input$fim,tol=input$Tolerancia) 
     else if(input$Metodo == 'Ponto Fixo')
       Tentativas<-fixedpoint(x0=input$ValorInicial,tol=input$Tolerancia)
     else if(input$Metodo == 'Secante')
       Tentativas<-secant(x0=input$ValorInicial,x1=input$fim,tol=input$Tolerancia)
     
     Valoresx=Tentativas[[1]]
     Valoresy=Tentativas[[2]]
     Evolucao<-data.frame(1:length(Valoresy),Valoresy)
     ggplot() + geom_line(aes(x=Evolucao[,1],y=Evolucao[,2]) ) + labs(x="passo",y="Valor")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

