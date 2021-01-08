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
    
    tabPanel(
      title="Introduction",

      h3("In this shiny application, you can perform some simple exploratory data analysis, and make your own logistic regression 
             model with dataset PimaIndiansDiabetes2.", font.size=4),
      tags$hr(),
      h4("In  section", tags$strong("Data Exploratory Analysis"), ", you can play around with boxplot, dotplot 
         and correlation plot to explore the data in order to understand how the data looks like."),
      br(),
      h4("In section", tags$strong("Logistic Regression"), "you can select the predictors to include in your construction of logistic regression model"),
      h4("It also feedbacks you the summary result of the model you construct, residual plot analysis, and confusion matrix to get accuracy of your model"),
      br(),
      h4("In section", tags$strong("Data"), "you can review the data table we used in this shiny application"),
      h4("It has three sub-sections, one is the whole data, the second is the training data we used to build our logistic regression model, and the third is the testing data we use to test the accuracy of our prediction model."),
      tags$hr(),
      h5("Plot Examples you'll see in section", tags$strong("Data Exploratory Analysis")),
      fluidRow(
          column(width=4, tags$image(src="p1.png", height=500, width=500)),
          column(width=4, tags$image(src="p2.png", height=500, width=500)),
          column(width=4, tags$image(src="p3.png", height=500, width=500))
      )
      
    ),

    navbarMenu(title="Data Exploratory Analysis",
               tabPanel(
                   title="Boxplot",
                   sidebarLayout(
                       sidebarPanel(
                           tags$p("The data set PimaIndiansDiabetes2 used in this analysis contains contain information about diabetes of indian people."),
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
                           tags$p("The data set PimaIndiansDiabetes2 used in this analysis contains contain information about diabetes of indian people."),
                   selectInput(inputId = "corrtype", label = "Select the correlation plot:",
                               choices = c("Option1", "Option2")),
                       ),
                   mainPanel(
                   title="Correlation Plot",
                   verbatimTextOutput("text2"),
                   plotOutput("corrplot", width=1000, height=700),
                   ),

               )
               ),
               
               tabPanel(
                   title="Dot Plot",
                   sidebarLayout(
                       sidebarPanel(
                           selectInput(inputId = "xvar", label = "Choose X-axis Variable",
                                       choices = colnames(data)[1:8], selected = "pregnant"),
                           selectInput(inputId = "yvar", label = "Choose y-axis Variable",
                                       choices = colnames(data)[1:8], selected = "glucose")
                       ),
                       mainPanel(
                           plotlyOutput("dotplot", width = 800, height = 700)
                       )
                   )
               )
    ),
    navbarMenu(
        title="Prediction",
        tabPanel(
            title="Logistic Regression",
            column(width = 4,
                   wellPanel(
                       h3("Result of using all variables to construct the logistic regression model"),
                       tags$code("model_logistic<-glm( diabetes ~., data = training, family = binomial)"),
                       br(),
                       tags$code("summary(model_logistic)"),
                       br(),
                       br(),
                       verbatimTextOutput("logistic")
                   )
                   ),
            column(width = 8,
                 wellPanel(
                     h3("Based on the summary from left, you can determine which predictors you want to include"),
                     checkboxGroupInput(inputId = "variable", label = "Predictors to include: ",
                                        choices = c("pregnant", "glucose", "pressure", "triceps", "insulin",
                                                    "mass", "pedigree", "age"), selected = c("pregnant"), inline=TRUE),
                     tags$code(textOutput("test")),
                     h4("Corresponding Summary Result of the ", tags$strong("Logistic Regression Model")),
                     verbatimTextOutput("logistic1"),
                     h4("Model Summary Plot"),
                     plotOutput("resid", width = 800, height = 800),
                     h4(tags$strong("Prediction Result with testing dataset")),
                     verbatimTextOutput("pred")
                 ),
                   )

        ),
        tabPanel(
         title="Predict",
         sidebarLayout(
             sidebarPanel(
                 checkboxInput(inputId = "var1", label = "pregnant", value = TRUE),
                 checkboxInput(inputId = "var2", label = "glucose", value = TRUE),
                 checkboxInput(inputId = "var3", label = "pressure", value = TRUE),
                 checkboxInput(inputId = "var4", label = "triceps", value = TRUE),
                 checkboxInput(inputId = "var5", label = "insulin", value = TRUE),
                 checkboxInput(inputId = "var6", label = "mass", value = TRUE),
                 checkboxInput(inputId = "var7", label = "pedigree", value = TRUE),
                 checkboxInput(inputId = "var8", label = "age", value = TRUE),
                 hr(),
                 uiOutput("numeric1"),
                 uiOutput("slider2"),
                 uiOutput("slider3"),
                 uiOutput("slider4"),
                 uiOutput("slider5"),
                 uiOutput("slider6"),
                 uiOutput("slider7"),
                 uiOutput("slider8"),
             ),
             mainPanel(
                 h4("In this section, you can make your own prediction by the logistic regression model."),
                 h4("By selecting the predictors you want to include in your model, you can also input the numeric value for that predictors."),
                 h4("The constructed logistic regression model will make the prediction based on the predictors you select and the numeric input you give to it"),
                 verbatimTextOutput("text3"),
                 verbatimTextOutput("pred_final")
             )
         )
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

