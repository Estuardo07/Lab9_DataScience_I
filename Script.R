## ui.R ## library(openxlsx)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "yellow",

  dashboardHeader(title = "Matrimonios con menores de edad en Guatemala del 2010 al 2020",  titleWidth = 700),
  dashboardSidebar(disable = TRUE),

  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Histogram", background = "navy", solidHeader = TRUE,
      
        plotOutput("plot1")
      ),
      
      box(

        title = "Year", status = "primary",


        sliderInput("slider", "Number of observations:", 2010, 2020, 2017)
      )
    ),
    #cantidad de matrimonios infantiles en ese ano
    fluidRow(
      # A static infoBox
      
      # Dynamic infoBoxes
      infoBoxOutput("progressBox"),
      infoBoxOutput("cantHombre"),
      infoBoxOutput("cantMujer")
     
    ),
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(status = "danger", plotOutput("chartHombre")),
      box(status = "danger", plotOutput("piechartHombre"))
      
    ),
    fluidRow(
      box(status = "warning", plotOutput("chartMujer")),
      box(status = "warning", plotOutput("piechartMujer"))
      
    ),
   
  )
)

server <- function(input, output) {
  set.seed(122)

  
  output$plot1<-renderPlot({
   
    db<-read.xlsx('menoresDeEdad.xlsx')
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
    bodasMenores<-filter(bodasMenores,bodasMenores$Año_Registrado<=input$slider)
    ggplot(data=bodasMenores, aes(x=Año_Registrado, y=Cantidad_Matrimonios, fill=Año_Registrado)) +
      
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=Cantidad_Matrimonios), vjust=1.6, color="black",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Cantidad de matrimonios con menores de edad por año",x='Año de registro' ,y="Cantidad de matrimonios ")+
      theme(legend.position="none")+scale_x_continuous(breaks = c(2010	,2011	,2012	,2013	,2014	,2015	,2016	,2017	,2018,	2019,	2020))
    
  })
  output$progressBox <- renderInfoBox({
    #Matrimonio donde almenos uno es menor de edad
    
    edadParejaAlMenosUnMenor<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Escolaridad.del.hombre','Escolaridad.de.la.mujer','Año.de.registro'),db$Edad.de.la.mujer<18 | db$Edad.del.hombre<18)
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    
    
    infoBox(
      "Matrimonios infantiles", paste0(nrow(edadParejaAlMenosUnMenor)), icon = icon("alert", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
    
  })
  output$cantHombre<-renderInfoBox({
    edadParejaAlMenosUnMenor<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Escolaridad.del.hombre','Escolaridad.de.la.mujer','Año.de.registro'),db$Edad.de.la.mujer<18 | db$Edad.del.hombre<18)
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    
    
    
    infoBox(
      "Matrimonios de niños menores de 18 años", paste0(sum(edadParejaAlMenosUnMenor$Edad.del.hombre<18)), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "navy", fill = TRUE
    )
    
  })
  output$cantMujer<-renderInfoBox({
    edadParejaAlMenosUnMenor<-subset(db, select=c('Edad.del.hombre','Edad.de.la.mujer','Escolaridad.del.hombre','Escolaridad.de.la.mujer','Año.de.registro'),db$Edad.de.la.mujer<18 | db$Edad.del.hombre<18)
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    
    
    infoBox(
      "Matrimonios de niñas menores de 18 años", paste0(sum(edadParejaAlMenosUnMenor$Edad.de.la.mujer<18)), icon = icon("exclamation-sign", lib = "glyphicon"),
      color = "maroon", fill = TRUE
    )
    
  })
  output$chartHombre<-renderPlot({
    
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    EscoH<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.del.hombre`) %>%
      tally()
    
    
    EscoM<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.de.la.mujer`) %>%
      tally()
    
    
    EscolaridadHombre<-c(EscoH$`Escolaridad.del.hombre`)
    CantidadEscolaridadHombre<-c(EscoH$n)
    
    EscoH<-data.frame(EscolaridadHombre,CantidadEscolaridadHombre)
    # EscoH$EscolaridadHombre<-as.factor(EscoH$EscolaridadHombre)
    
    EscolaridadMujer<-c(EscoM$`Escolaridad.de.la.mujer`)
    CantidadEscolaridadMujer<-c(EscoM$n)
    
    EscoM<-data.frame(EscolaridadMujer,CantidadEscolaridadMujer)
    # EscoM$EscolaridadMujer<-as.factor(EscoM$EscolaridadMujer)
    
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='1']<-'Ninguno'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='2']<-'Primaria'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='3']<-'Basico'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='4']<-'Diversificado'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='5']<-'Universitario'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='6']<-'Postgrado'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='9']<-'Ignorado'
  
    ggplot(data=EscoH, aes(x=EscolaridadHombre, y=CantidadEscolaridadHombre,fill=EscolaridadHombre)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=CantidadEscolaridadHombre), vjust=1.6, color="black",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Escolaridad de los hombres con matrimonio", y="Cantidad de matrimonios ")+
      theme(legend.position="none")
 
  })
  output$piechartHombre<-renderPlot({

    
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    EscoH<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.del.hombre`) %>%
      tally()
    
    
    EscoM<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.de.la.mujer`) %>%
      tally()
    
    
    EscolaridadHombre<-c(EscoH$`Escolaridad.del.hombre`)
    CantidadEscolaridadHombre<-c(EscoH$n)
    
    EscoH<-data.frame(EscolaridadHombre,CantidadEscolaridadHombre)
    # EscoH$EscolaridadHombre<-as.factor(EscoH$EscolaridadHombre)
    
    EscolaridadMujer<-c(EscoM$`Escolaridad.de.la.mujer`)
    CantidadEscolaridadMujer<-c(EscoM$n)
    
    EscoM<-data.frame(EscolaridadMujer,CantidadEscolaridadMujer)
    # EscoM$EscolaridadMujer<-as.factor(EscoM$EscolaridadMujer)
    
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='1']<-'Ninguno'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='2']<-'Primaria'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='3']<-'Basico'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='4']<-'Diversificado'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='5']<-'Universitario'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='6']<-'Postgrado'
    EscoH$EscolaridadHombre[EscoH$EscolaridadHombre=='9']<-'Ignorado'

    pd<-round(((CantidadEscolaridadHombre*100)/sum(EscoH$CantidadEscolaridadHombre)),digits = 2)
    
    
    ggplot(EscoH, aes(x = "", y = pd, fill = EscolaridadHombre)) +
      geom_col(color = "black") +
      geom_label(aes(label = pd),
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      coord_polar(theta = "y")+theme(legend.position = "bottom")+
      labs(title="Porcentaje de escolaridad de los hombres")
    
  })
  #Mujer
  output$chartMujer<-renderPlot({
    
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    EscoH<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.del.hombre`) %>%
      tally()
    
    
    EscoM<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.de.la.mujer`) %>%
      tally()
    
    
    EscolaridadHombre<-c(EscoH$`Escolaridad.del.hombre`)
    CantidadEscolaridadHombre<-c(EscoH$n)
    
    EscoH<-data.frame(EscolaridadHombre,CantidadEscolaridadHombre)
    # EscoH$EscolaridadHombre<-as.factor(EscoH$EscolaridadHombre)
    
    EscolaridadMujer<-c(EscoM$`Escolaridad.de.la.mujer`)
    CantidadEscolaridadMujer<-c(EscoM$n)
    
    EscoM<-data.frame(EscolaridadMujer,CantidadEscolaridadMujer)
    # EscoM$EscolaridadMujer<-as.factor(EscoM$EscolaridadMujer)
    
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='1']<-'Ninguno'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='2']<-'Primaria'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='3']<-'Basico'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='4']<-'Diversificado'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='5']<-'Universitario'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='6']<-'Postgrado'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='9']<-'Ignorado'
    
    ggplot(data=EscoM, aes(x=EscolaridadMujer, y=CantidadEscolaridadMujer,fill=EscolaridadMujer)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=CantidadEscolaridadMujer), vjust=1.6, color="black",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Escolaridad de los hombres con matrimonio", y="Cantidad de matrimonios ")+
      theme(legend.position="none")
    
  })
  output$piechartMujer<-renderPlot({
    
    edadParejaAlMenosUnMenor<-filter(edadParejaAlMenosUnMenor,edadParejaAlMenosUnMenor$Año.de.registro<=input$slider)
    EscoH<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.del.hombre`) %>%
      tally()
    
    
    EscoM<-edadParejaAlMenosUnMenor %>%
      group_by(`Escolaridad.de.la.mujer`) %>%
      tally()
    
    
    EscolaridadHombre<-c(EscoH$`Escolaridad.del.hombre`)
    CantidadEscolaridadHombre<-c(EscoH$n)
    
    EscoH<-data.frame(EscolaridadHombre,CantidadEscolaridadHombre)
    # EscoH$EscolaridadHombre<-as.factor(EscoH$EscolaridadHombre)
    
    EscolaridadMujer<-c(EscoM$`Escolaridad.de.la.mujer`)
    CantidadEscolaridadMujer<-c(EscoM$n)
    
    EscoM<-data.frame(EscolaridadMujer,CantidadEscolaridadMujer)
    # EscoM$EscolaridadMujer<-as.factor(EscoM$EscolaridadMujer)
    
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='1']<-'Ninguno'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='2']<-'Primaria'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='3']<-'Basico'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='4']<-'Diversificado'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='5']<-'Universitario'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='6']<-'Postgrado'
    EscoM$EscolaridadMujer[EscoM$EscolaridadMujer=='9']<-'Ignorado'
    
    ggplot(data=EscoM, aes(x=EscolaridadMujer, y=CantidadEscolaridadMujer,fill=EscolaridadMujer)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=CantidadEscolaridadMujer), vjust=1.6, color="black",
                position = position_dodge(0.9), size=3.5)+
      labs(title="Escolaridad de los hombres con matrimonio", y="Cantidad de matrimonios ")+
      theme(legend.position="none")
    
    pd<-round(((CantidadEscolaridadMujer*100)/sum(EscoM$CantidadEscolaridadMujer)),digits = 2)
    
    
    ggplot(EscoM, aes(x = "", y = pd, fill = EscolaridadMujer)) +
      geom_col(color = "black") +
      geom_label(aes(label = pd),
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      coord_polar(theta = "y")+theme(legend.position = "bottom")+
      labs(title="Porcentaje de escolaridad de las Mujeres")
    
  })
  #
  # Filter data based on selected Style


  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
}

shinyApp(ui, server)
