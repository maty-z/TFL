library(shiny)
library(bslib)
library(latex2exp)

#### Importo librerías y funciones auxiliares
#Importo funciones para generar muestras: 
#Uniforme-exponencial o Normal-exponencial
source("./f_generar_muestras.R")
#Importo funciones para estimaciones
source("./f_estimaciones.R")

#### Inicia código app
withMathJax()

##### Layout #####
ui <- page_sidebar(
  title = "Uniforme o Normal-Exponencial",
  sidebar = sidebar(
    # Tarjeta con parámetros de búsqueda del umbral
    card(
      sliderInput("c",
                  "Factor \\(c\\) de penalizacion:",
                  min = 0,
                  max = 15,
                  value = 0.8,
                  step = 0.001),
      sliderInput("qinf",
                  "Cuantil inferior estimación",
                  min = 0.5,
                  max = 0.99,
                  value = 0.5),
    ),
    # Tarjeta con parámetros de la muestra
    card(
      sliderInput("size",
                  "Tamaño de la muestra:",
                  min = 500,
                  max = 10000,
                  value = 1000,
      ),
      radioButtons("modelo",
                   "Elegir modelo",
                   c("Unif-Exp","Nor-Exp"),
                   inline = TRUE,
                   selected = "Unif-Exp"
      ),
      conditionalPanel(
        condition = "input.modelo=='Nor-Exp'",
        sliderInput(
          "mu",
          withMathJax("Media \\(\\mu\\)"),
          min = -10,
          max = 1,
          value = 0.5,
          step = 0.01
        ),
        sliderInput(
          "sigma2",
          withMathJax("Varianza \\(\\sigma^2\\)"),
          min = 0,
          max = 10,
          value = 1,
          step = 0.01
        )
      ),
      sliderInput("elam",
                  "\\(\\lambda\\):",
                  min = 0,
                  max = 50,
                  value = 0.5,
                  step = 0.01),
      sliderInput("p_umbral",
                  "\\(p_{umbral}\\):",
                  min = 0.5,
                  max = 1,
                  value = 0.5),
      actionButton("regenerar", 
                   "Generar muestra"),
    ),
  ),
  
  #Gráfico principal
  navset_card_underline(
    nav_panel("Datos y modelo",
              layout_columns(
                radioButtons("density",
                             "Incluir densidad no paramétrica",
                             c("Si","No"),
                             inline = TRUE,
                             selected = "Si"
                ),
                radioButtons("puntos",
                             "Incluir puntos",
                             c("Si","No"),
                             inline = TRUE,
                             selected = "Si")
              ),
                plotOutput("distPlot")
              ),
    nav_panel("Función de pérdida", plotOutput("perdida")),
  ),
  
  #Destaco valores estimados: El umbral y el lambda
  layout_columns(
    "\\(\\widehat{u}\\)",textOutput("u_est_text"),
    "\\(\\widehat{\\lambda}\\)",textOutput("lamb_est_text"),
    "\\(\\widehat{p}_{umbral}\\)",textOutput("p_umbral_est_text")
  )
)

##### Server #####
server <- function(input, output) {
  
  # Se genera una muestra nueva al cambiar algún parámetro
  #o al tocar el botón
  muestra <- reactive({
    input$regenerar
    size <- input$size
    lambda <- input$elam
    p_umbral <- input$p_umbral
    
    if (input$modelo == "Unif-Exp") {
      generar_muestra(size = size,
                      modelo = "unif_exp",
                      p_umbral = p_umbral, 
                      u_min = 0,
                      u_max = 1,
                      exp_lambda = lambda)
    } else {
      generar_muestra(size = size,
                      modelo = "normal_exp",
                      p_umbral = p_umbral, 
                      norm_max = 1,
                      mu = input$mu,
                      sigma2 = input$sigma2,
                      exp_lambda = input$elam)
      }
  })
  
  # Calculo las estimaciones correspondientes considerando
  #la muestra y los parámetros de búsqueda del umbral
  est.u <- reactive({
    x <- muestra()
    u_est(x, n_pen = 1, alpha_pen = 0.4, c = input$c, qinf = input$qinf)
  })

  est.lamb <- reactive({
    x <- muestra()
    u_estimado <- est.u()
    lambda_emv(u_estimado,x)
  })

  est.p_umbral <- reactive({
    x <- muestra()
    u_estimado <- est.u()
    size <- input$size
    length(x[x<=u_estimado])/size
  })
  
  # Muestra el valor del umbral estimado
  output$u_est_text <- renderText({
    est.u()
  })
  
  # Muestra el valor del lambda estimado
  output$lamb_est_text <- renderText({
    est.lamb()
  })
  
  # Muestra el valor del p_umbral estimado
  output$p_umbral_est_text <- renderText({
    est.p_umbral()
  }) 
  # Muestra el gráfico principal
  # Compara el histograma con la densidad real,
  #con la parte exponencial estimada y la densidad empírica.
  # Además se muestra el umbral real y el umbral estimado 
  #con líneas verticales
  output$distPlot <- renderPlot({
    x <- muestra()
    u_estimado <- est.u()
    lambda_estimado <- est.lamb()
    p_umbral_estimado <- est.p_umbral()
    
    hist(x, breaks = 51, col = 'darkgray', border = 'white',
         main = 'Histograma',
         probability = TRUE,
         ylab = "Densidad"
    )
    if (input$modelo == "Unif-Exp") {
      curve((1-input$p_umbral)*dexp(t-1,rate=input$elam),from = 1,xname = "t",add = TRUE)
      curve(input$p_umbral*dunif(t, min = 0, max = 1),to = 1,xname = "t",add = TRUE)
    } else {
      curve((1-input$p_umbral/pnorm(1,mean=input$mu,sd = sqrt(input$sigma2)))*dexp(t-1,rate=input$elam),from = 1,xname = "t",add = TRUE)
      curve(input$p_umbral/pnorm(1,mean=input$mu,sd = sqrt(input$sigma2))*dnorm(t, mean = input$mu, sd = sqrt(input$sigma2)),to = 1,xname = "t",add = TRUE)
    }
    curve((1-p_umbral_estimado)*dexp(t-u_estimado,rate=lambda_estimado),from = u_estimado,xname = "t",add = TRUE,col = "red")
    if (input$puntos == "Si") rug(x,side = 1, ticksize = 0.05)
    abline(v = 1)
    abline(v = u_estimado,col = "red")
    
    legend_lty <- c(1,1)
    legend_col <- c("black","red")
    legend_leg <- c("Población","Estimación")
    
    if (input$density == "Si") {
      lines(density(x),lty = 2)
      legend_lty <- c(legend_lty,2)
      legend_col <- c(legend_col,"black")
      legend_leg <- c(legend_leg,"Densidad no paramétrica")
    }
    
    legend(
      x = "topright",
      lty = legend_lty,
      col = legend_col,
      legend = legend_leg#c("Realidad","Estimaciones","Densidad empírica"))
    )
  })
  
  output$perdida <- renderPlot({
    x <- muestra()
    u_estimado <- est.u()
    u_interval <- c(quantile(x, probs = input$qinf),
                    quantile(x, probs = 0.99))
    u_grilla <- seq(u_interval[1],u_interval[2],length.out = 1000)
    perdidas <- sapply(u_grilla,f_obj,muestra = x,alpha_pen = 0.4,n_pen=1,c=input$c)
    
    plot(x = u_grilla,y = perdidas, ylab = "Pérdida")
    abline(v = 1)
    abline(v = u_estimado,col = "red")
  })
  
}

shinyApp(ui, server)