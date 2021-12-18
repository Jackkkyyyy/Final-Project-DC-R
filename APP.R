library(dplyr)
library(shiny)
library(ggplot2)
library(caret)
library(shinyWidgets)
library(shinythemes)
library(scales)
library(fmsb)
library(RColorBrewer)
library(scales)
library(readxl)
library(h2o)
getwd()

source("prediction.R")
pre_data <- as.data.frame(read_xlsx("power_matrix.xlsx"))

mydata <- as.data.frame(read.csv("superheroes_nlp_dataset_1.csv")) %>% 
  filter(creator %in% "DC Comics") %>%
  select(1,4:11,18,23,31:80) %>%
  filter(is.na(history_text) ==F) %>%
  filter(is.na(powers_text) ==F) %>%
  filter(is.na(alignment) ==F) %>%
  filter(is.na(gender) ==F)

mydata$intelligence_score <-as.numeric(mydata$intelligence_score)
mydata$strength_score<-as.numeric(mydata$strength_score)
mydata$speed_score<- as.numeric(mydata$speed_score)
mydata$durability_score <- as.numeric(mydata$durability_score)
mydata$power_score<-as.numeric(mydata$power_score)
mydata$combat_score<- as.numeric(mydata$combat_score)

ui <- fluidPage(
  
  
  
  theme = shinytheme("superhero"),
  titlePanel(title=div(img(src="https://cdnb.artstation.com/p/assets/images/images/029/700/321/large/semrram-gonzalez-dc-logo.jpg?1598376808", height = 70), 
                       "DC Superheroes")),
  setBackgroundImage(
    src = "https://wallpapercave.com/wp/wp2412746.jpg"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId="Alignment", 
                         label="Alignment of the hero", 
                         choices=unique(mydata$alignment),
                         selected = NULL),
      uiOutput("alignment_choice"),
      radioButtons(inputId="Alignment_foe", 
                   label="Alignment of the foe", 
                   choices=unique(mydata$alignment),
                   selected = NULL),
      uiOutput("alignment_choice_foe"),
      actionButton(inputId = "Fight",label = "Fight", icon("fantasy-flight-games"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4" )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Basic Info",
                 wellPanel(
                   fluidRow(column(3,h3("Name"),textOutput("Name_hero")),
                            column(9,h3("Info"),textOutput("Info_alignment"),
                                    textOutput("Info_gender"))

                   ),
                   fluidRow(column(12,
                        h3("History"),
                        textOutput("Info_history"),
                        tags$head(tags$style(type="text/css","#Info_history{font-size:12px; 
                        font-style:italic; overflow-x:scroll;white-space: pre-wrap; 
                                             max-height: 100px;}"))
                                   )),
                   fluidRow(column(12,
                                   h3("Power Description"),
                                   textOutput("Info_power"),
                                   tags$head(tags$style(type="text/css","#Info_power{font-size:12px; 
                        font-style:italic; overflow-x:scroll;white-space: pre-wrap; 
                                             max-height: 100px;}"))
                   ))
                 )),
        tabPanel("Fight Info", 
                 wellPanel(
                   fluidRow(column(12,
                                   h3("Fight Info - Graph"),
                                   imageOutput("fightgraph"))),
                  fluidRow(column(12,
                                  h4("Hero's Data",
                                     tableOutput("fightdata")),
                                  h4("Foe's Data",
                                     tableOutput("fightdata_foe")))))),
      tabPanel("Prediction",
               wellPanel(
                 fluidRow(column(12,h4("Data Prediction",
                                       tableOutput("prediction_table")))),
                 fluidRow(column(6, h4("Prediction Result",
                                    div(textOutput("prediction_result")),
                                    tags$head(tags$style(type="text/css","#prediction_result{
                                 font-size:18px;font-style:italic;text-shadow: 2px 2px 5px blue;}")))),
                          column(6,div(img(src="https://preview.pixlr.com/images/800wm/100/1/1001410253.jpg", height=200))))
                                 ))
               )
               )
    )
  )

server <- function(input, output) {
  
  
  
  output$alignment_choice <- renderUI({
    selectInput(inputId = "Name", 
                label = "Choose a hero",
                choices = mydata %>% 
                  filter(alignment == input$Alignment) %>%
                  select(name),
                selected = 1)
  })
  
  output$alignment_choice_foe <- renderUI({
    selectInput(inputId = "Foe", label = "Choose a foe", 
                choices = mydata %>% 
                  filter(alignment == input$Alignment_foe) %>%
                  filter(name != input$Name) %>%
                  select(name),
                selected = 1)
  })
  
  newdata <- reactive({
    prediction(pre_data)
  })
  
  output$Name_hero <- renderText({
    input$Name
  })
  
  output$Info_alignment <- renderText({
    paste0("Alignment: ", mydata %>%
      filter(name %in% input$Name) %>%
      select(alignment))
  })
  
  output$Info_gender <- renderText({
    paste0("Gender: ", mydata%>%
             filter(name %in% input$Name)%>%
             select(gender))
  })
  
  output$Info_history <- renderText({
   paste0("History : ", mydata%>%
            filter(name %in% input$Name)%>%
            select(history_text)) 
  })
  
  output$Info_power <- renderText({
    paste0("Power : ", mydata%>%
             filter(name %in% input$Name)%>%
             select(powers_text)) 
  })
  
  output$fightdata <- renderTable({
    a <- mydata %>% filter(name %in% input$Name)%>% select(4:9)
    colnames(a) <- c("Intelligence", "Strength","Speed", "Durability","Power","Combat")
    a
  })
  
  output$fightdata_foe <- renderTable({
    a <- mydata %>% filter(name %in% input$Foe)%>% select(4:9)
    colnames(a) <- c("Intelligence", "Strength","Speed", "Durability","Power","Combat")
    a
  })
  
  output$fightgraph <- renderPlot({
    plotdata <- mydata %>%
      filter(name %in% c(input$Name, input$Foe)) %>%
      select(4:9)
    
    rownames(plotdata)<-c(input$Name, input$Foe)
    
    max_min <- data.frame(
      intelligence_score = c(100, 0), strength_score = c(100, 0), speed_score = c(100, 0),
      durability_score = c(100, 0), power_score = c(100, 0), combat_score = c(100, 0)
    )
    rownames(max_min) <- c("Max", "Min")
    
    df <- rbind(max_min, plotdata)
    
    coul <- brewer.pal(3, "BuPu")
    colors_border <- coul
    colors_in <- alpha(coul,0.4)
    
    radarchart(df,
               axistype=0 , 
               #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
               #custom labels
               vlcex=0.8 ,caxislabels=seq(0,100,20))
    
    legend(x=0.5,y=-0.7,legend = rownames(-df[3:4,]),
           bty = "n", pch = 20 , 
           col = c("#E0ECF4","#9EBCDA","#8856A7"),
           text.col = "black", cex = 1, pt.cex = 1.5)

  })
  
  observeEvent(input$Fight,{
    output$prediction_table <- renderTable({
      df <- newdata()
      d <- data.frame(df%>%filter(name %in% c(input$Name, input$Foe)))
    })
    
    output$prediction_result <- renderText({
        df <- newdata()
        df1 <- df %>% filter(name %in% c(input$Name, input$Foe))%>%
          arrange(predicted_score, desc=T)
        paste0(df1[2,1], " will win!!!")
    })
  }
    )
  

  
}

shinyApp(ui = ui, server = server)

