library(shiny)
library(ggplot2)
library(corrplot)
library(plotly)
library(PerformanceAnalytics)
library(DT)
library(caret)
library(mlbench)



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
    var1<-paste(input$variable, collapse = "+")
    paste("daibetes~", var1, sep="")
  })
  
  model_logistic1<-reactive({
    var<-input$variable
    var1<-paste(var, collapse = "+")
    formula1<-as.formula(paste("diabetes~", var1, sep=""))
    glm(formula1, data = training, family = "binomial")
  })
  
  output$logistic1<-renderPrint({
    summary(model_logistic1())
  })
  
  output$pred<-renderPrint({
    pred_logistic1<-predict(model_logistic1(), testing, type="response")
    pred_logistic1_class<-as.factor(ifelse(pred_logistic1>0.5, "pos", "neg"))
    confusionMatrix(testing$diabetes, pred_logistic1_class)
  })
  
  output$resid<-renderPlot({
    par(mfrow=c(2,2))
    plot(model_logistic1())
  })
  
  output$dotplot<-renderPlotly({
    xvar<-input$xvar
    yvar<-input$yvar
    formu1<-as.formula(paste("~", xvar, sep=""))
    formu2<-as.formula(paste("~", yvar, sep=""))
    fill<-c("#00FFA8", "#E30671")
    plot_ly(data=data, x=formu1, y=formu2, type="scatter", mode="markers", color=~diabetes,
            colors=fill) %>% layout(xaxis = list(title = xvar), yaxis = list(title = yvar)) %>%
      layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
                                    yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)))
  })
  
 output$numeric1<-renderUI({
   if(input$var1 == TRUE){
     minv<-min(data[, "pregnant"])
     maxv<-max(data[, "pregnant"])
     numericInput(inputId = "numeric_1", label = "pregnant", value = 1, min = minv, max=maxv)
   }
 })
 
 
 output$slider2<-renderUI({
   if(input$var2 == TRUE){
     minv<-min(data[, "glucose"])
     maxv<-max(data[, "glucose"])
     sliderInput(inputId = "slider_2", label = "glucose", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 output$slider3<-renderUI({
   if(input$var3 == TRUE){
     minv<-min(data[, "pressure"])
     maxv<-max(data[, "pressure"])
     sliderInput(inputId = "slider_3", label = "pressure", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 output$slider4<-renderUI({
   if(input$var4 == TRUE){
     minv<-min(data[, "triceps"])
     maxv<-max(data[, "triceps"])
     sliderInput(inputId = "slider_4", label = "triceps", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 output$slider5<-renderUI({
   if(input$var5 == TRUE){
     minv<-min(data[, "insulin"])
     maxv<-max(data[, "insulin"])
     sliderInput(inputId = "slider_5", label = "insulin", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 output$slider6<-renderUI({
   if(input$var6 == TRUE){
     minv<-min(data[, "mass"])
     maxv<-max(data[, "mass"])
     sliderInput(inputId = "slider_6", label = "mass", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 output$slider7<-renderUI({
   if(input$var7 == TRUE){
     minv<-min(data[, "pedigree"])
     maxv<-max(data[, "pedigree"])
     sliderInput(inputId = "slider_7", label = "pedigree", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 output$slider8<-renderUI({
   if(input$var8 == TRUE){
     minv<-min(data[, "age"])
     maxv<-max(data[, "age"])
     sliderInput(inputId = "slider_8", label = "age", value = (minv+maxv)/2, min = minv, max=maxv)
   }
 })
 
 final_model<-reactive({
   v1<-input$var1
   v2<-input$var2
   v3<-input$var3
   v4<-input$var4
   v5<-input$var5
   v6<-input$var6
   v7<-input$var7
   v8<-input$var8
   
   sub_data<-data[, c(v1,v2,v3,v4,v5,v6,v7,v8, TRUE)]
   glm(data=sub_data, diabetes~., family = "binomial")
 })
 
 output$text3<-renderPrint({
   summary(final_model())
 })
 
 output$pred_final<-renderPrint({
   v1<-input$var1
   v2<-input$var2
   v3<-input$var3
   v4<-input$var4
   v5<-input$var5
   v6<-input$var6
   v7<-input$var7
   v8<-input$var8
   col_name<-colnames(data)[c(v1,v2,v3,v4,v5,v6,v7,v8, FALSE)]
   len<-length(col_name)
   mat<-matrix(0, nrow=1, ncol=len)
   new_data<-as.data.frame(mat)
   colnames(new_data)<-col_name
   
   if(v1==FALSE){}else{new_data[,"pregnant"]=input$numeric_1}
   if(v2==FALSE){}else{new_data[,"glucose"]=input$slider_2}
   if(v3==FALSE){}else{new_data[,"pressure"]=input$slider_3}
   if(v4==FALSE){}else{new_data[,"triceps"]=input$slider_4}
   if(v5==FALSE){}else{new_data[,"inslulin"]=input$slider_5}
   if(v6==FALSE){}else{new_data[,"mass"]=input$slider_6}
   if(v7==FALSE){}else{new_data[,"pedigree"]=input$slider_7}
   if(v8==FALSE){}else{new_data[,"age"]=input$slider_8}
   
   
   prediction_final<-predict(final_model(), newdata=new_data, type="response")
   Result<-ifelse(prediction_final>0.5, "pos", "neg")
   
   list(input_data=new_data, Probability=prediction_final , Result=Result)
   })
 

})

