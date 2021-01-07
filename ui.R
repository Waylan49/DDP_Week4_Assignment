library(shiny)
library(ggplot2)

data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
data<-PimaIndiansDiabetes2

shinyUI(navbarPage(
    title="Data Analysis - Diabetes",

    navbarMenu(title="Exploratory Analysis",
               tabPanel(
                   title="Boxplot",
                   sidebarLayout(
                       sidebarPanel(
                           selectInput(inputId = "predictor", label = "Choose the predictor",
                                       choices = colnames(data)[1:8]),
                           tags$hr(),
                           p("Click below link for more predictors information"),
                           tags$a(href="https://rdrr.io/github/quantide/qdata/man/pimaindiansdiabetes2.html", "RDocumention_Link")
                       ),
                       mainPanel(
                           plotOutput("boxplot"),
                           h5("Predicotr Explanation: "),
                           textOutput("text1")
                       )
                   )

               ),
               tabPanel(
                   title="dot plot"
               )
    ),


    tabPanel(
        "World"
    ),


    tabPanel(
        "!"
    )
    ))
