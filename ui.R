###########################################################################
##Reg Vis
##Justin Post - 2017
###########################################################################
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
#library(plot3D)
library(rgl)
library(plotly)
library(shinyRGL)
  # Define UI for application that displays an about page and the app itself
  
  dashboardPage(skin="red",
                #add title
                dashboardHeader(title="Visualizing Regression",titleWidth=750),
                
                #define sidebar items
                dashboardSidebar(sidebarMenu(
                  menuItem("One Variable Regression", tabName = "appOneVar", icon = icon("laptop")),
                  menuItem("Two Variable Regression", tabName = "appTwoVar", icon = icon("laptop"))
                )),
                
                #define the body of the app
                dashboardBody(
                  tabItems(
                    #actual app layout for One Variable part   
                    tabItem(tabName = "appOneVar",
                            fluidRow(
                              column(3,br(),
                                     selectInput("xOneVar","Explanatory Variable (x)",choices=NULL,selected=NULL,multiple=FALSE),
                                     selectInput("yOneVar","Response Variable (y)",choices=NULL,selected=NULL,multiple=FALSE),
                                     checkboxInput("oneVarReg",'Fit Regression Equation?',value=FALSE),
                                     conditionalPanel(condition= "input.oneVarReg",
                                                      radioButtons("oneVarPreds","Degree Polynomial",choices = c("None", "Linear", "Quadratic", "Cubic", "Quartic"), selected = "None"), 
                                                      radioButtons("oneVarStd", "Standardize X variable when fitting Quadratic, Cubic, and Quartic model?", choices = c("None", "Center & Scale", "Orthogonal Polynomials"),
                                                                   selected = "None"))
                              ),
                              
                              #Show the scatter plot  
                              column(9,
                                     fluidRow(
                                       box(width=12,plotlyOutput("oneVarPlot"))
                                     ), 
                                     fluidRow(
                                       box(width=12,title="Fitted Regression Equation",uiOutput("oneVarEq"))),
                                     fluidRow(
                                       box(width=6,title="Coefficient Information for Model",tableOutput("oneVarCoef"))
                                       #box(width=6,title="Variance Inflation Factors",tableOutput("oneVarVifs"))
                                     )
                              )
                            )
                    ),
                    #actual app layout for Two Variable part   
                    tabItem(tabName = "appTwoVar",
                            fluidRow(
                              column(3,br(),
                                     selectInput("xTwoVar1","Explanatory Variable (x1)",choices=NULL,selected=NULL,multiple=FALSE),
                                     selectInput("xTwoVar2","Explanatory Variable (x2)",choices=NULL,selected=NULL,multiple=FALSE),
                                     selectInput("yTwoVar","Response Variable (y)",choices=NULL,selected=NULL,multiple=FALSE),
                                     # sliderInput("theta",label="Viewing Angle",min=0,max=360,value=20,animate=TRUE),
                                     # sliderInput("phi",label="Viewing Angle",min=0,max=360,value=20,animate=TRUE),
                                     checkboxInput("twoVarReg",'Fit Regression Equation?',value=FALSE),
                                     conditionalPanel(condition = "input.twoVarReg",
                                                      selectizeInput("twoVarPreds","Model to Fit",
                                                                     choices=c("Main effect for x1 only", 
                                                                               "Main effect for x2 only", 
                                                                               "Quadratic in x1 only", 
                                                                               "Quadratic in x2 only", 
                                                                               "Both main effects only",
                                                                               "Both main effects and interaction",
                                                                               "Both main effects and interaction with quadratic for x1 only",
                                                                               "Both main effects and interaction with quadratic for x2 only",
                                                                               "Both main effects and interaction with quadratic for x1 and x2"
                                                                               ),
                                                                     selected="Both main effects only")
                                     )
                              ),
                              
                              #Show the scatter plot  
                              column(9,
                                     fluidRow(
                                       #box(width=12,plotOutput("twoVarPlot"))
                                       box(width = 12, rglwidgetOutput("threeD"))
                                     ), 
                                     fluidRow(
                                       box(width=12,title="Fitted Regression Equation",uiOutput("twoVarCoef"))),
                                     fluidRow(
                                       box(width=12,title="ANOVA Table for Model",tableOutput("twoVarANOVA"))
                                     ),
                                     fluidRow()
                              )
                            )
                    )
                  )
                )
  )
  
  

