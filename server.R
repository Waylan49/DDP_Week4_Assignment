library(shiny)
library(ggplot2)

data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
data<-PimaIndiansDiabetes2

shinyServer(function(input, output) {

  output$boxplot<-renderPlot({
      predictor<-input$predictor
      fill<-c("#03F2F0", "#FF08F3")
      title<-paste("Boxplot of ", predictor, " ~ diabetes", sep="")
      ggplot(data, aes_string(x="diabetes", y=predictor))+
          geom_boxplot(aes(fill=diabetes))+scale_fill_manual(values=fill)+ggtitle(title)
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





})

