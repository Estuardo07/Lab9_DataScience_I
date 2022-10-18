#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(openxlsx)
library(tidyverse)
library(ggplot2)


db<-read.xlsx('menoresDeEdad.xlsx')


# Define UI for application that draws a histogram
library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Matrimonios con menores de edad en Guatemala del 2010 al 2020"),
  # Select Year
  # sliderInput('style',"Style",
  #                         label = "Year",
  #                         min = 2010,
  #                         max = 2020,
  #                         value = 2017),
  selectInput("style", "Style", c("2010",
                                  "2011",
                                  "2012",
                                  "2013",
                                  "2014",
                                  "2015")),

  
  plotOutput("brandBar")
  
  
)

# Define server logic
server <- function(input, output) {
  
  output$brandBar <-renderPlot({
    
    #2010
    db10<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2010)
    #2011
    db11<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2011)
    nrow(db11)
    #2012
    db12<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2012)
    nrow(db12)
    #2013
    db13<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2013)
    nrow(db13)
    #2014
    db14<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2014)
    nrow(db14)
    #2015
    db15<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2015)
    nrow(db15)
    #2016
    db16<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2016)
    nrow(db16)
    #2017
    db17<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2017)
    nrow(db17)
    #2018
    db18<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2018)
    nrow(db18)
    #2019
    db19<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2019)
    nrow(db19)
    #2020
    db20<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Año.de.registro'),db$Año.de.registro==2020)
    nrow(db20)
    
    Año_Registrado<-c(2010	,2011	,2012	,2013	,2014	,2015	,2016	,2017	,2018,	2019,	2020)
    Cantidad_Matrimonios<-c(nrow(db10),nrow(db11),nrow(db12),nrow(db13),nrow(db14),nrow(db15),nrow(db16),nrow(db17),nrow(db18),nrow(db19),nrow(db20))
    
    
    
    bodasMenores<-data.frame(Año_Registrado,Cantidad_Matrimonios)
  
    
    ggplot(data=bodasMenores, aes(x=Año_Registrado, y=Cantidad_Matrimonios, fill=Año_Registrado)) +
      
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=Cantidad_Matrimonios), vjust=1.6, color="black",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Cantidad de matrimonios con menores de edad por año",x='Año de registro' ,y="Cantidad de matrimonios ")+
      theme(legend.position="none")+scale_x_continuous(breaks = c(2010	,2011	,2012	,2013	,2014	,2015	,2016	,2017	,2018,	2019,	2020))
    
    # Filter data based on selected Style
    
    
    
  })
  # Create bar chart of brands
  output$brandBar <- renderPlot({
    
    # Filter data based on selected Style
    if (input$style != "2015") {
      bodasMenores <- filter(bodasMenores, Style == input$style)
    }})
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
