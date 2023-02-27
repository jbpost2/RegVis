library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
#library(plot3D)
library(plotly)
library(car)
library(rgl)
library(shinyRGL)

data<-as_tibble(read.csv("data.csv",header=TRUE))
data<-data[,c(-3:-1)]

#function to get RHS of formula for two pred part
addFormula<-function(formula, condition){
  switch(formula,
         "Main effect for x1 only" = as.formula("y~x1"),
         "Main effect for x2 only" = as.formula("y~x2"),
         "Quadratic in x1 only" = as.formula("y~x1+I(x1^2)"),
         "Quadratic in x2 only" = as.formula("y~x2+I(x2^2)"),
         "Both main effects only" = as.formula("y~x1+x2"),
         "Both main effects and interaction" = as.formula("y~x1+x2+x1:x2"),
         "Both main effects and interaction with quadratic for x1 only" = as.formula("y~x1+x2+x1:x2+I(x1^2)"),
         "Both main effects and interaction with quadratic for x2 only" = as.formula("y~x1+x2+x1:x2+I(x2^2)"),
         "Both main effects and interaction with quadratic for x1 and x2" = as.formula("y~x1+x2+x1:x2+I(x1^2)+I(x2^2)")
         )
}


# Define server logic required to draw the plots etc
shinyServer(function(input, output,session) {
  
  #populate/update ui
  observe({
    
    updateSelectInput(session,"xOneVar","Explanatory Variable (x)",choices=names(data)[sapply(data,is.numeric)],selected=names(data)[sapply(data,is.numeric)][1])
    
    updateSelectInput(session,"yOneVar","Response Variable (y)",choices=names(data)[sapply(data,is.numeric)],selected=names(data)[sapply(data,is.numeric)][2])
    
    updateSelectInput(session,"xTwoVar1","Explanatory Variable (x1)",choices=names(data)[sapply(data,is.numeric)],selected=names(data)[sapply(data,is.numeric)][1])
    
    updateSelectInput(session,"xTwoVar2","Explanatory Variable (x2)",choices=names(data)[sapply(data,is.numeric)],selected=names(data)[sapply(data,is.numeric)][2])
    
    updateSelectInput(session,"yTwoVar","Response Variable (y)",choices=names(data)[sapply(data,is.numeric)],selected=names(data)[sapply(data,is.numeric)][3])
    
  })
  
  
  ###################################################
  #One var section
  
  # #Object to contain the fitted model
  # model_fit <- reactiveValues(fit = NULL, ys = NA, xs = NA, datax = NA, datay = NA)
  # 
  # #fit the model appropriate
  # observe({
  #   # fit <- ys <- xs <- datax <- datay <- NA
  #   # #get inputs
  #   # x<-input$xOneVar
  #   # y<-input$yOneVar
  #   # #remove missing values
  #   # dataTemp<-na.omit(data[,c(x,y)])
  #   # #get just the y and x vectors
  #   # datax<-pull(dataTemp[,x])
  #   # datay<-pull(dataTemp[,y])
  #   # xs<-seq(from=min(datax),to=max(datax),length=1000)
  #   # 
  #   # if(input$oneVarReg){
  #   #   if(input$oneVarPreds == "None"){
  #   #     fit<-lm(datay~1)
  #   #     ys<-predict(fit, newdata = data.frame(datax = xs))
  #   #   } else if(input$oneVarPreds == "Linear"){
  #   #     fit<-lm(datay~datax)
  #   #     ys<-predict(fit, newdata = data.frame(datax = xs))
  #   #   } else if(input$oneVarPreds == "Quadratic"){
  #   #     fit<-lm(datay ~ datax + I(datax^2))
  #   #     ys<-predict(fit, newdata = data.frame(datax = xs))
  #   #   } else if(input$oneVarPreds == "Cubic"){
  #   #     fit<-lm(datay ~ datax + I(datax^2) + I(datax^3))
  #   #     ys<-predict(fit, newdata = data.frame(datax = xs))
  #   #   } else if(input$oneVarPreds == "Quartic"){
  #   #     fit<-lm(datay ~ datax + I(datax^2) + I(datax^3) + I(datax^4))
  #   #     ys<-predict(fit, newdata = data.frame(datax = xs))
  #   #   } 
  #   # }
  #   input$oneVarReg
  #   model_fit$ys <- 10
  #   print(model_fit$ys)
  #   # model_fit$fit <- fit
  #   # model_fit$ys <- ys
  #   # model_fit$xs <- xs
  #   # model_fit$datax <- datax
  #   # model_fit$datay <- datay
  # })
  
  #create plot
  output$oneVarPlot<-renderPlotly({
    req(input$xOneVar, input$yOneVar)
    #get inputs
    x<-input$xOneVar
    y<-input$yOneVar
    #remove missing values
    dataTemp<-na.omit(data[,c(x,y)])
    #get just the y and x vectors
    datax<-pull(dataTemp[,x])
    datay<-pull(dataTemp[,y])
    
    if(input$oneVarReg){
      if(input$oneVarPreds == "None"){
        #plot
        dataTemp %>% 
          plot_ly(x = ~datax) %>% 
          add_markers(y = ~datay, name = "Data Points") %>%
          add_lines(x = ~datax,y = ~mean(datay), name = "0 degree polynomial \n (mean of response)") %>%
          layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
      } else if(input$oneVarPreds == "Linear"){
        fit<-lm(datay~datax)
        xs<-seq(from=min(datax),to=max(datax),length=2000)
        ys<-rep(coef(fit)[1],length(xs))+coef(fit)[2]*xs
        #plot
        dataTemp %>% 
          plot_ly(x = ~datax) %>% 
          add_markers(y = ~datay, name = "Data Points") %>%
          add_lines(x = ~xs,y = ~ys, name = "Line of Best fit") %>%
          layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        
      } else if(input$oneVarPreds == "Quadratic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 2))
          xs<-seq(from=min(datax),to=max(datax),length=2000)
          ys<-predict(fit, newdata = data.frame(datax = xs))
          #plot
          dataTemp %>% 
            plot_ly(x = ~datax) %>%
            add_markers(y = ~datay, name = "Data Points") %>%
            add_lines(x = ~xs,y = ~ys, data=data.frame(xs,ys), name = "Quadratic Fit") %>%
            layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          xs<-seq(from=min(datax),to=max(datax),length=2000)
          fit<-lm(datay ~ datax + I(datax^2))
          ys<-predict(fit, newdata = data.frame(datax = xs))
        
          #plot
          dataTemp %>% 
            plot_ly(x = ~datax) %>%
            add_markers(y = ~datay, name = "Data Points") %>%
            add_lines(x = ~xs,y = ~ys, data=data.frame(xs,ys), name = "Quadratic Fit") %>%
            layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        }
        
      } else if(input$oneVarPreds == "Cubic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 3))
          xs<-seq(from=min(datax),to=max(datax),length=2000)
          ys<-predict(fit, newdata = data.frame(datax = xs))
          #plot
          dataTemp %>% 
            plot_ly(x = ~datax) %>%
            add_markers(y = ~datay, name = "Data Points") %>%
            add_lines(x = ~xs,y = ~ys, data=data.frame(xs,ys), name = "Cubic Fit") %>%
            layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          xs<-seq(from=min(datax),to=max(datax),length=2000)
          fit<-lm(datay ~ datax + I(datax^2) + I(datax^3))
          ys<-predict(fit, newdata = data.frame(datax = xs))
          
          #plot
          dataTemp %>% 
            plot_ly(x = ~datax) %>%
            add_markers(y = ~datay, name = "Data Points") %>%
            add_lines(x = ~xs,y = ~ys, data=data.frame(xs,ys), name = "Cubic Fit") %>%
            layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        }
      } else if(input$oneVarPreds == "Quartic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 4))
          xs<-seq(from=min(datax),to=max(datax),length=2000)
          ys<-predict(fit, newdata = data.frame(datax = xs))
          #plot
          dataTemp %>% 
            plot_ly(x = ~datax) %>%
            add_markers(y = ~datay, name = "Data Points") %>%
            add_lines(x = ~xs,y = ~ys, data=data.frame(xs,ys), name = "Quartic Fit") %>%
            layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          xs<-seq(from=min(datax),to=max(datax),length=2000)
          fit<-lm(datay ~ datax + I(datax^2) + I(datax^3) + I(datax^4))
          ys<-predict(fit, newdata = data.frame(datax = xs))
          
          #plot
          dataTemp %>% 
            plot_ly(x = ~datax) %>%
            add_markers(y = ~datay, name = "Data Points") %>%
            add_lines(x = ~xs,y = ~ys, data=data.frame(xs,ys), name = "Quartic Fit") %>%
            layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
        } 
      }
    } else{
      dataTemp %>% 
        plot_ly(x = datax) %>% 
        add_markers(y = datay, name = "Data Points")%>%
        layout(xaxis=list(title=paste(x)),yaxis=list(title=paste(y)))
    }
  })
  
  
  #create regression equation or return string. Not ideal but small dataset so I'm just going to repeat code for fitting.
  output$oneVarEq<-renderUI({
    req(input$xOneVar, input$yOneVar)
    #get inputs
    x<-input$xOneVar
    y<-input$yOneVar
    #remove missing values
    dataTemp<-na.omit(data[,c(x,y)])
    #get just the y and x vectors
    datax<-pull(dataTemp[,x])
    datay<-pull(dataTemp[,y])

    if(input$oneVarReg){
      xs<-seq(from=min(datax),to=max(datax),length=2000)
      centered_xs <- seq(from = -4, to = 4, length = 2000)
      if(input$oneVarPreds == "None"){
        fit<-lm(datay~1)
        withMathJax(HTML(paste0('$$\\hat{y} = ',paste(signif(fit$coef,6),collapse=" + "),'$$'))) 
      } else if(input$oneVarPreds == "Linear"){
        fit<-lm(datay ~ datax)
        fit$coef<-signif(fit$coef,6)
        withMathJax(HTML(paste0('$$\\hat{y} = ',paste(fit$coef[1],paste(fit$coef[2],'x',sep="*"),sep=" + "),'$$'))) 
      } else if(input$oneVarPreds == "Quadratic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 2))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          fit<-lm(datay ~ datax + I(datax^2))
        }
        fit$coef<-signif(fit$coef,6)
        #return equation
        withMathJax(HTML(paste0('$$\\hat{y} = ',paste(fit$coef[1],paste(fit$coef[2],'x',sep="*"),paste(fit$coef[3],'x^2',sep="*"),sep=" + "),'$$'))) 
      } else if(input$oneVarPreds == "Cubic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 3))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          fit<-lm(datay ~ datax + I(datax^2) + I(datax^3))
        }
        fit$coef<-signif(fit$coef,6)
        #return equation
        withMathJax(HTML(paste0('$$\\hat{y} = ',paste(fit$coef[1],paste(fit$coef[2],'x',sep="*"),paste(fit$coef[3],'x^2',sep="*"),paste(fit$coef[4],'x^3',sep="*"),sep=" + "),'$$'))) 
      }else if(input$oneVarPreds == "Quartic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 4))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          fit<-lm(datay ~ datax + I(datax^2) + I(datax^3) + I(datax^4))
        }
        fit$coef<-signif(fit$coef,6)
        withMathJax(HTML(paste0('$$\\hat{y} = ',paste(fit$coef[1],paste(fit$coef[2],'x',sep="*"),paste(fit$coef[3],'x^2',sep="*"),paste(fit$coef[4],'x^3',sep="*"),paste(fit$coef[5],'x^4',sep="*"),collapse=" + "),'$$')))
      }
    } else {
      paste("No regression fit selected.")
    }
  })
  
  #get coef table
  output$oneVarCoef<-renderTable(include.rownames=TRUE,{
    req(input$xOneVar, input$yOneVar)
    #get inputs
    x<-input$xOneVar
    y<-input$yOneVar
    #remove missing values
    dataTemp<-na.omit(data[,c(x,y)])
    #get just the y and x vectors
    datax<-pull(dataTemp[,x])
    datay<-pull(dataTemp[,y])

    if(input$oneVarReg){
      if(input$oneVarPreds == "None"){
        fit<-lm(datay~1)
        info<-as.data.frame(summary(fit)$coefficients)
        row.names(info)<-c("Intercept")
      } else if(input$oneVarPreds == "Linear"){
        fit<-lm(datay ~ datax)
        info<-as.data.frame(summary(fit)$coefficients)
        row.names(info)<-c("Intercept","Linear")
      } else if(input$oneVarPreds == "Quadratic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 2))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          fit<-lm(datay ~ datax + I(datax^2))
        }
        info<-as.data.frame(summary(fit)$coefficients)
        row.names(info)<-c("Intercept","Linear","Quadratic")
      } else if(input$oneVarPreds == "Cubic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 3))
        } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          fit<-lm(datay ~ datax + I(datax^2) + I(datax^3))
        }
        info<-as.data.frame(summary(fit)$coefficients)
        row.names(info)<-c("Intercept","Linear","Quadratic", "Cubic")
      }else if(input$oneVarPreds == "Quartic"){
        if(input$oneVarStd == "Orthogonal Polynomials"){
          fit<-lm(datay ~ poly(datax, 4))
          } else {
          if(input$oneVarStd == "Center & Scale"){
            datax <- (datax-mean(datax))/sd(datax)
          }
          fit<-lm(datay ~ datax + I(datax^2) + I(datax^3) + I(datax^4))
        }
        info<-as.data.frame(summary(fit)$coefficients)
        row.names(info)<-c("Intercept","Linear","Quadratic", "Cubic", "Quartic")
      }
      info
    } else {
      data.frame(Message="No regression fit selected.")
    }
  })

  # #get vif table (this currently isn't working for theorthogonal poly models... need to investigate)
  # output$oneVarVifs<-renderTable(include.rownames=TRUE,{
  #   #get inputs
  #   x<-input$xOneVar
  #   y<-input$yOneVar
  #   #remove missing values
  #   dataTemp<-na.omit(data[,c(x,y)])
  #   #get just the y and x vectors
  #   datax<-pull(dataTemp[,x])
  #   datay<-pull(dataTemp[,y])
  #   if(input$oneVarReg){
  #     if(input$oneVarPreds == "None"){
  #       data.frame(Message="VIF only applicable for models with two predictors or more")
  #     } else if(input$oneVarPreds == "Linear"){
  #       data.frame(Message="VIF only applicable for models with two predictors or more")
  #     } else if(input$oneVarPreds == "Quadratic"){
  #       if(input$oneVarStd == "Orthogonal Polynomials"){
  #         fit<-lm(datay ~ poly(datax, 2))
  #       } else {
  #         if(input$oneVarStd == "Center & Scale"){
  #           datax <- (datax-mean(datax))/sd(datax)
  #         }
  #         fit<-lm(datay ~ datax + I(datax^2))
  #       }
  #       vif(fit)
  #     } else if(input$oneVarPreds == "Cubic"){
  #       if(input$oneVarStd == "Orthogonal Polynomials"){
  #         fit<-lm(datay ~ poly(datax, 3))
  #       } else {
  #         if(input$oneVarStd == "Center & Scale"){
  #           datax <- (datax-mean(datax))/sd(datax)
  #         }
  #         fit<-lm(datay ~ datax + I(datax^2) + I(datax^3))
  #       }
  #       vif(fit)
  #     }else if(input$oneVarPreds == "Quartic"){
  #       if(input$oneVarStd == "Orthogonal Polynomials"){
  #         fit<-lm(datay ~ poly(datax, 4))
  #       } else {
  #         if(input$oneVarStd == "Center & Scale"){
  #           datax <- (datax-mean(datax))/sd(datax)
  #         }
  #         fit<-lm(datay ~ datax + I(datax^2) + I(datax^3) + I(datax^4))
  #       }
  #       vif(fit)
  #     }
  #   } else {
  #     data.frame(Message="No regression fit selected.")
  #   }
  # })  
  
  ###################################################
  #Two Var part
  
  fitter <- reactive({
    if(input$twoVarReg){    
      #get inputs
      x1<-input$xTwoVar1
      x2<-input$xTwoVar2
      y<-input$yTwoVar
      dataTemp<-na.omit(data[,c(x1,x2,y)])
      names(dataTemp)<-c("x1","x2","y")
      fit<-lm(addFormula(input$twoVarPreds), data = dataTemp)
      return(fit)
    }
  })
  
  # #create plot (using interactive one from below now)
  # output$twoVarPlot<-renderPlot({
  #   #get inputs
  #   x1<-input$xTwoVar1
  #   x2<-input$xTwoVar2
  #   y<-input$yTwoVar
  #   dataTemp<-na.omit(data[,c(x1,x2,y)])
  #   datax1<-pull(dataTemp[,x1])
  #   datax2<-pull(dataTemp[,x2])
  #   datay<-pull(dataTemp[,y])
  #   
  #   if(input$twoVarReg){
  #     theta<-input$theta
  #     phi<-input$phi
  #     
  #     fit<-fitter()
  #     grid.lines = 40
  #     x1.pred <- seq(min(datax1), max(datax1), length.out = grid.lines)
  #     x2.pred <- seq(min(datax2), max(datax2), length.out = grid.lines)
  #     x1x2 <- expand.grid(x1.pred, x2.pred)
  #     names(x1x2)<-c("x1","x2")
  #     y.pred <- matrix(predict(fit, newdata = x1x2), 
  #                      nrow = grid.lines, ncol = grid.lines)
  #     
  #     scatter3D(x=datax1, y=datax2, z=datay, pch = 16, cex = 1, theta = theta, 
  #               phi = phi, ticktype = "detailed",
  #               xlab = input$xTwoVar1, ylab = input$xTwoVar2, 
  #               zlab = input$yTwoVar,
  #               surf = list(x = x1.pred, y = x2.pred, z = y.pred))
  #     
  #     
  #     #plot_ly(x = ~datax1, y = ~datax2, z= ~datay) #%>%
  #     #add_surface(z=plane)
  #     
  #   } else {
  #     scatter3D(x=datax1, y=datax2, z=datay, pch = 16, cex = 1, 
  #               theta = input$theta, phi = input$phi, ticktype = "detailed",
  #               xlab = input$xTwoVar1, ylab = input$xTwoVar2, zlab = input$yTwoVar)
  #     # data %>% plot_ly(x = ~datax1, y = ~datax2, z= ~datay)# %>%
  #     # layout(title = "3-D Scatterplot",
  #     #      scene = list(
  #     #        xaxis = list(title = "x1"), 
  #     #        yaxis = list(title = "x2"), 
  #     #        zaxis = list(title = "y")))
  #   }
  #   
  # })
  
  output$threeD <- renderRglwidget({
    #get inputs
    x1<-input$xTwoVar1
    x2<-input$xTwoVar2
    y<-input$yTwoVar
    dataTemp<-na.omit(data[,c(x1,x2,y)])
    datax1<-pull(dataTemp[,x1])
    datax2<-pull(dataTemp[,x2])
    datay<-pull(dataTemp[,y])
    
    
    if(input$twoVarReg){
      
      #get inputs
      names(dataTemp)<-c("x1","x2","y")
      dataTemp <- dataTemp[,c(3,1,2)]
      open3d()
      plot3d(lm(addFormula(input$twoVarPreds), data = dataTemp), vars = dataTemp)
      scene2 <- scene3d()
      close3d()
      rglwidget(scene2)
      
    } else {
      open3d()
      plot3d(x=datax1, y=datax2, z=datay, pch = 16, cex = 1,
             xlab = input$xTwoVar1, ylab = input$xTwoVar2, zlab = input$yTwoVar)
      scene1 <- scene3d()
      close3d()
      
      rglwidget(scene1)
    }
  })
  
  #get coefficientstable
  output$twoVarCoef<-renderTable(rownames=TRUE, digits = 4, {
    fit<-fitter()
    if(is.null(fit)){
      "No regression fit selected."
    } else {
      signif(fit$coef,6)
    }
  })
  
  #get ANOVA table
  output$twoVarANOVA<-renderTable(rownames=TRUE,{
    fit<-fitter()
    if(is.null(fit)){
      "No regression fit selected."
    } else {
      anova(fit)
    }
  })
  
})  