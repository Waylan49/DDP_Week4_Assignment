library(shiny)
library(ggplot2)
library(corrplot)
library(plotly)
library(PerformanceAnalytics)
library(DT)
library(caret)



shinyServer(function(input, output) {

  data("PimaIndiansDiabetes2", package = "mlbench")
  PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
  data<-PimaIndiansDiabetes2
  inTrain<-createDataPartition(data$diabetes, p=0.8, list=FALSE)
  training<-data[inTrain,]
  testing<-data[-inTrain,]


  output$boxplot<-renderPlotly({
      predictor<-input$predictor
      fill<-c("#00FFA8", "#E30671")
      formula<-as.formula(paste("~ ", predictor, sep=""))
      title<-paste("Boxplot of ", predictor, " ~ diabetes", sep="")
      p<-plot_ly(data, y=formula, color=~diabetes, type="box", colors=fill)%>%
        layout(xaxis = list(title = 'Diagnosis Result'), yaxis = list(title = predictor)) %>%
        layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
              yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)))
  })

  output$text0<-renderText({
    predictor<-input$predictor
    paste("Boxplot of ", predictor, "~ diabetes", sep="")
  })

  output$text1<-renderPrint({
      predictor<-input$predictor
      if(predictor == "pregnant"){
         " Number of times pregnant (numeric);"
      }else if(predictor == "glucose"){
          "Plasma glucose concentration a 2 hours in an oral glucose tolerance test (numeric);"
      }else if(predictor =="pressure"){
          "Diastolic blood pressure (mm Hg) (numeric);"
      }else if(predictor =="triceps"){
          "Triceps skin fold thickness (mm) (numeric);"
      }else if(predictor =="insulin"){
          "2-Hour serum insulin (mu U/ml)(numeric);"
      }else if(predictor =="mass"){
          "Body mass index (weight in kg/(height in m)^2) (numeric);"
      }else if(predictor =="pedigree "){
          "Diabetes pedigree function (numeric);"
      }else if(predictor =="age"){
          "Age (years) (numeric);"
      }else{
          "class variable: diabetic or not (factor with 2 levels: neg and pos);"
      }
  })

  output$corrplot<-renderPlot({
    data1<-data
    data1$diabetes<-as.numeric(data1$diabetes)
    if(input$corrtype == "Option1"){
    res<-cor(data1)
    corrplot(res, type="upper", order="hclust", tl.col = "black", tl.srt = 45,
             tl.cex=1.5)
    }else {
    chart.Correlation(data1, histogram = TRUE, pch=19)
  }
  })

  output$text2<-renderPrint({
    if(input$corrtype == "Option1"){
      "The correlation plot is created by function corrplot()"
    }else{
      "The correlation plot is created by function chart.Correlation() with argument histogram=TRUE"
    }
  })

  output$dt<-renderDataTable({
    datatable(data, options = list(pageLength=20))
  })

  output$training<-renderDataTable({
    datatable(training, options = list(pageLength=20))
  })

  output$testing<-renderDataTable({
    datatable(testing, options = list(pageLength=20))
  })


  model_logistic<-glm( diabetes ~., data = training, family = binomial)

  output$logistic<-renderPrint({
    summary(model_logistic)
  })

  output$pred1<-renderPrint({
    pred<-predict(model_logistic, testing)
    ifelse(pred>0.5, "pos", "neg")
  })

  output$test<-renderText({
    input$variable
  })



})

