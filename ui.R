library(shiny)
library(ggplot2)
library(corrplot)
library(plotly)
library(PerformanceAnalytics)
library(DT)
library(caret)


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
                           h3(textOutput("text0")),
                           plotlyOutput("boxplot", width = 800, height = 700),
                           h5("Predicotr Explanation: "),
                           textOutput("text1")
                       )
                   )

               ),

               tabPanel(
                   title="Correlation Plot",
                   sidebarLayout(
                       sidebarPanel(
                   selectInput(inputId = "corrtype", label = "Select the correlation plot:",
                               choices = c("Option1", "Option2")),
                       ),
                   mainPanel(
                   title="Correlation Plot",
                   verbatimTextOutput("text2"),
                   plotOutput("corrplot", width=1000, height=700),
                   ),

               )
               )
    ),


    navbarMenu(
        title="Prediction Method",
        tabPanel(
            title="Logistic Regression",
            column(width = 4,
                   wellPanel(
                       h3("Using all variables to construct the logistic regression model"),
                       tags$code("model_logistic<-glm( diabetes ~., data = training, family = binomial)"),
                       br(),
                       tags$code("summary(model_logistic)"),
                       br(),
                       br(),
                       verbatimTextOutput("logistic")
                   )
                   ),
            column(width = 8,
                 checkboxGroupInput(inputId = "variable", label = "Predictors you want to include in your model",
                                    choices = c("pregnant", "glucose", "pressure", "triceps", "insulin",
                                                "mass", "pedigree", "age")),
                 verbatimTextOutput("test")

                   )

        ),
        tabPanel(
            title="Random Forest",

        )
    ),


    navbarMenu(
            title="Data",
            tabPanel(
                title="Complete Raw Data",
                dataTableOutput("dt")
            ),
            tabPanel(
                title="Training Data (80%)",
                dataTableOutput("training")
            ),
            tabPanel(
                title="Testing Data (20%)",
                dataTableOutput("testing")
            )

        )

    ))









