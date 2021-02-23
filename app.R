library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(forecast)
library(xts)
library(corrplot)
library(reshape2)


CountryVaccinations=read.csv("C:\\Users\\INDIA\\Documents\\data science\\evaluation task\\dashboard\\CountryVaccinations.csv",header=TRUE)
vaccination_1.3 <- read.csv("C:\\Users\\INDIA\\Documents\\data science\\evaluation task\\COVID VACCINE\\country_vaccinations.csv",header = TRUE)

protein_diet=read.csv("C:\\Users\\INDIA\\Documents\\data science\\evaluation task\\COVID DIET\\Protein_Supply_Quantity_Data.csv",header=TRUE)
fat_diet=read.csv("C:\\Users\\INDIA\\Documents\\data science\\evaluation task\\COVID DIET\\Fat_Supply_Quantity_Data.csv",header=TRUE)
energy_diet=read.csv("C:\\Users\\INDIA\\Documents\\data science\\evaluation task\\COVID DIET\\Food_Supply_kcal_Data.csv",header=TRUE)
ui <- shinyUI(
    dashboardPage(skin="black",
                  dashboardHeader(title="Analysis on COVID-19",titleWidth=320),
                  dashboardSidebar(width=320,
                                   sidebarMenu(
                                       menuItem(strong("1) Study on Country Vaccinations"),tabName = "one"),
                                         menuSubItem("Geographical Distribution of COVID-19 Vaccines",tabName="first",icon = icon("project-diagram")),
                                         menuSubItem("Plot of Daily Vaccination Amount",tabName="second",icon = icon("chart-line")),
                                         menuSubItem("Prediction on Daily Vaccination",tabName="third",icon = icon("hourglass-start")),
                                       menuItem(strong("2) Study on COVID-19 Healthy Diet"),tabName = "two"),
                                         menuSubItem("Correlation Analysis",tabName="fourth",icon = icon("paperclip")),
                                         menuSubItem("Relationship of Foods with Consequences",tabName="fifth",icon = icon("bacon"))
                                   )
                  ),
                  
                  
                  
                  
                  dashboardBody(
                      tabItems(
                          tabItem(
                              tabName = "one",tags$h2("PROGRESS OF COVID 19 VACCINATION",align = "center") , img(src ="image1.jpg",height = 500,width = 1000,align = "center")),
                          tabItem(
                              tabName="first",h3("Map View for Number of People Vaccinated & Name of the Vaccines Distributed by the Countries around the Globe "),
                              fluidRow(
                                  box(solidHeader = TRUE,width= 3 ,background = "navy",tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations/locations.csv", "COVID-19 Data Repository", target="_blank"),
                                      h5("All data are aggregated by people vaccinated and people fully vaccinated.The repository contains information about vaccine update from the official sites of different country.Likewise, it also reflects name of vaccines distributed by the different countries along with their ISO Code"),
                                      selectInput("name",
                                                  "Select the Name of Country:", 
                                                  choices= unique(CountryVaccinations$country))
                                        ),
                                  box(solidHeader = TRUE,background = "olive" , tableOutput("data"),width=9),
                                  
                                  box(background = "orange" ,width = 12,tabsetPanel(  
                                      tabPanel("GEOGRAPHICAL DISTRIBUTION OF VACCINE", leafletOutput("people_vaccinated")))),
                                  
                                   )),
                          tabItem(tabName="second",h3("Line Chart showing Daily Vaccination done on different Dates in Various Countries")),
                          tabItem(
                              tabName="third",h3("Prediction of Total Amount of Vaccination within March 30,2021 with current rate of Daily Vaccination"),
                              fluidRow(
                                 box(background = "navy", solidHeader = TRUE,
                                  selectInput("choice",
                                              "Select the Name of Country:", 
                                              choices= unique(vaccination_1.3$country)),
                                  
                                  dateInput("date", "prediction date", value = "2021-02-14")
                                  ),
                             box(background = "blue" ,width = 12,tabsetPanel(
                                 tabPanel(" According to the current rate of daily vaccination predicting how many ill be total vaccinated  with March 30, 2021", plotOutput("arimaplot")))),
                             box(background = "aqua" , width=8 , tabsetPanel(
                                 tabPanel("toal vaccinated till March30 2021 is")))   
                             
                              )),
                          tabItem(
                              tabName = "two",tags$h2("COVID 19 HEALTHY DIET",align = "center") , img(src ="image2.jpg",height = 500,width = 900, align = "center")),
                          tabItem(tabName="fourth",h3("Finding Features that are Correlated with Confirm Cases")),
                          tabItem(
                              tabName="fifth",h3("Searching which type of Foods are Responsible for what kind of Consequences")),
                              fluidRow(
                              box(solidHeader = TRUE,width= 3 ,background = "green",
                                  selectInput("choices","select the effect of food intake",
                                              colnames(diet[,25:30]))),
                              box(background = "light-blue" ,width = 12,paste(" following type of foods(multiple foods) are responsible for the consequence along with the correlation coeeficient",br())),
                              box(background = "green" , width=8 , tabsetPanel(
                                tabPanel("correlation table",tableOutput("cor_table")))) 
                              ),
                      ))))






server <- function(input,output){
    
   
    output$data <- renderTable({
        countryFilter <- subset(CountryVaccinations,CountryVaccinations$country==input$name)
         })
        output$people_vaccinated <- renderLeaflet({
        CountryVaccinations<-CountryVaccinations%>%mutate(popup_info=paste("Name of Country:",country,"<br/>","ISO_Code:",iso_code,"<br/>","Name of the Vaccines Distributed:",vaccines,"<br/>","Amount of People Vaccinated:",people_vaccinated,"<br/>","Amount of People Fully Vaccinated:",people_fully_vaccinated))
        CountryVaccinations$popup_info
        colors<-c("red","navy")
        pal<-colorBin(palette = colors,CountryVaccinations$people_vaccinated)
        leaflet()%>%addProviderTiles(provider = "Esri.NatGeoWorldMap")%>%addCircleMarkers(data=CountryVaccinations,lat = ~lat,lng = ~long,radius = ~6,popup = ~popup_info,color = ~pal(people_vaccinated),stroke = FALSE,fillOpacity = 1.5)%>% addLegend(position = "bottomright",pal = pal,values = CountryVaccinations$people_vaccinated, title = "PEOPLE VACCINATED" , opacity = 1)
        })
        
       
        output$arimaplot<-renderPlot({
            vaccination_1.3<- subset(vaccination_1.3,vaccination_1.3$country==input$choice)
            vaccination_1.3 <- na.omit(vaccination_1.3[c("country","date", "daily_vaccinations")])
            vaccination_1.3$date <- as.Date (vaccination_1.3$date,format="%Y-%m-%d")
            vaccination_1.3<-vaccination_1.3[order(vaccination_1.3$date),]
            #vaccination_1.3=aggregate(vaccination_1.3$daily_vaccinations,by=list(vaccination_1.3$date),sum)
            
            ds_ts<-ts(vaccination_1.3)
            fit<-auto.arima(ds_ts[,3])
            dat=(as.Date(input$date) - vaccination_1.3[nrow(vaccination_1.3),2])
            forecast1=forecast(fit,h=dat)
            autoplot(forecast1,xlab = "date",ylab = "daily vaccinations")
             #accuracy(fit)#mape=8.77,92%accurate
    
    
            diet=rbind(protein_diet,fat_diet)
            diet=rbind(diet,energy_diet)
            diet[,25:30]=sapply(diet[,25:30],FUN=as.numeric)
            diet=na.omit(diet)
            
            output$cor_table<- renderTable({
              effect=diet[,which(input$choices==colnames(diet))]
              cr=cor(diet[,2:24],effect)
              cr=subset(melt(cr),value>0.3)
              cr=cr[,-2]
              cr
              
            })
            
                
            
      
       })
    
    }
    
        
        
        
# Run the application 
shinyApp(ui = ui, server = server)



        
        

