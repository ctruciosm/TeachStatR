#########################################################
######       Shiny App to illustrate the CLT      #######
#########################################################
### Author: Carlos Trucios
### website: https://ctruciosm.github.io
#########################################################

library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Teorema Central do Limite: Vizualise como o TCL funciona"),
    theme = shinythemes::shinytheme('superhero'),
 
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Vizualise como o TCL (caso iid) funciona. Escolha uma distribuição e defina o valor dos paràmetros."),
            helpText(" - Uniforme[par1, par2]"),
            helpText(" - Bernoulli(par1)"),
            helpText(" - Poisson(par1)"),
            helpText(" - Exponencial(par1)"),
            selectInput('distri', 'Distribution', c('Uniforme', 'Bernoulli', 'Poisson', 'Exponencial'), 'Uniforme'),
            numericInput('par1', 'Parametro 1: ', 0),
            numericInput('par2', 'Parametro 2: ', 1),
            numericInput('n_sample', 'Tamanho n: ', 10)
 
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        M = 5000
        tcl = rep(NA,M)
        for (i in 1:M){
            if (input$distri == 'Uniforme'){
                x = runif(input$n_sample, input$par1, input$par2)
                tcl[i] = sqrt(input$n_sample)*(mean(x)-(input$par2+input$par1)/2)/(sqrt((input$par2-input$par1)^2/12))
            }
            if (input$distri == 'Bernoulli'){
                x = rbinom(input$n_sample, 1, prob = input$par1)
                tcl[i] = sqrt(input$n_sample)*(mean(x)-input$par1)/(sqrt(input$par1*(1-input$par1)))
            }
            if (input$distri == 'Poisson'){
                x = rpois(input$n_sample, input$par1)
                tcl[i] = sqrt(input$n_sample)*(mean(x)-input$par1)/(sqrt(input$par1))
            }
            if (input$distri == 'Exponencial'){
                x = rexp(input$n_sample, input$par1)
                tcl[i] = sqrt(input$n_sample)*(mean(x)-1/input$par1)/(sqrt(1/(input$par1^2)))
            }
        }
        
        eixo_x = seq(-4.5,4.5, length.out = M)
        eixo_y = dnorm(eixo_x)
        dados = data.frame(tcl, eixo_x, normal = eixo_y)
        
        ggplot(dados) + geom_histogram(aes(tcl,y=..density..), alpha = 0.80, bins = 40) + 
            geom_line(aes(eixo_x,normal), colour="red4") +
            xlab(" ") + ylab(" ")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
