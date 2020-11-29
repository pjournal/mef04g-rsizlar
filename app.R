pti <- c("digest","glue","jsonlite","shiny","dplyr","ggplot2","rio","sjmisc","shinyWidgets",
         "leaflet","leaflet.extras","extrafont")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

library(digest)
library(glue)
library(jsonlite)
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rio)
library(sjmisc)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(extrafont)


#Sets locale to Turkish
#Sys.setlocale(category = "LC_ALL", locale = "Turkish")

#Import Data and Clean
ilce_data<-import("https://raw.githubusercontent.com/pjournal/mef04g-rsizlar/gh-pages/Data/ilce_lat_lon_istanbul.csv",quote="")

ilce_data<-ilce_data%>%
    mutate(southwest_lon=str_remove(`southwest_lon"`,"\""))

ilce_data[,c(1,9)]<-NULL 

#setwd("C:/Users/erenm/Downloads/ISBIKE_Project")
json<-readRDS("isbike_20201118.rds")
data<-as.data.frame(fromJSON(json))

#Convert Data Types

data$dataList.lat<-as.numeric(data$dataList.lat)
data$dataList.lon<-as.numeric(data$dataList.lon)
data$dataList.dolu<-as.numeric(data$dataList.dolu)
data$dataList.bos<-as.numeric(data$dataList.bos)
data$dataList.istasyon_no<-as.numeric(data$dataList.istasyon_no)

#Calculate Density Columns and filter unnecessary rows

data<-data%>%filter(dataList.bos!=0 & dataList.dolu !=0)%>%
    mutate(yogunluk=dataList.bos/(dataList.dolu+dataList.bos),
           yogunluk_cat=case_when(yogunluk<0.25 ~ "0-0.25",
                                 yogunluk>=0.25 & yogunluk<0.5 ~ "0.25-0.5",
                                 yogunluk>=0.5 & yogunluk<0.75 ~ "0.5-0.75",
                                 yogunluk>=0.75 ~ "0.75-1"))

#Match County data with our Isbike data
repeated_data<-data[rep(seq_len(nrow(data)), each = nrow(ilce_data)), ]

repeated_ilce_data<-do.call("rbind", replicate(188, ilce_data, simplify = FALSE))

matched_data<-cbind(repeated_data%>%
                        select(dataList.istasyon_no,dataList.adi,dataList.lat,dataList.lon),
                    repeated_ilce_data[,c(2,5:8)])

matched_data<-matched_data%>%
    mutate(ilce=case_when(northeast_lat>dataList.lat & southwest_lat<dataList.lat &
                              northeast_lon>dataList.lon & southwest_lon<dataList.lon ~ ilce_adi))

check_data<-matched_data%>%
    filter(is.na(ilce)==FALSE & ilce!="ADALAR")

check_data<-check_data %>% 
    group_by(dataList.istasyon_no) %>%
    mutate(row_number = row_number())%>%
    select(dataList.istasyon_no,dataList.adi,ilce,row_number)

#Clean County Data
join_data<-left_join(data,check_data,"dataList.istasyon_no")%>%
    select(dataList.istasyon_no,dataList.adi.x,ilce)


istasyon_ilce<-join_data%>%
    mutate(ilce=case_when(str_detect(dataList.adi.x,"Beykoz")==TRUE~"BEYKOZ",
                          str_detect(dataList.adi.x,"Çubuklu")==TRUE~"BEYKOZ",
                          str_detect(dataList.adi.x,"Ortaçeşme")==TRUE~"BEYKOZ",
                          str_detect(dataList.adi.x,"Mihrabat")==TRUE~"BEYKOZ",
                          str_detect(dataList.adi.x,"Türk-Alman")==TRUE~"BEYKOZ",
                          str_detect(dataList.adi.x,"Mesire")==TRUE~"BEYKOZ",
                          str_detect(dataList.adi.x,"Tarabya")==TRUE~"SARIYER",
                          str_detect(dataList.adi.x,"Emirgan")==TRUE~"SARIYER",
                          str_detect(dataList.adi.x,"İstinye")==TRUE~"SARIYER",
                          str_detect(dataList.adi.x,"Pendik")==TRUE~"PENDIK",
                          dataList.istasyon_no %in% c(5924,5923,5920,5921,1414,1402,1408)~"BEYKOZ",
                          dataList.istasyon_no %in% c(1407,1206)~"USKUDAR",
                          dataList.istasyon_no %in% c(1601)~"SILE",
                          dataList.istasyon_no %in% c(1900,5914)~"SARIYER",
                          dataList.istasyon_no %in% c(1229,1232)~"SULTANBEYLI",
                          dataList.istasyon_no %in% c(1301,1302)~"KARTAL",
                          dataList.istasyon_no %in% c(5001,5002,5003,5006,5007:5011,5014:5018)~"GAZIOSMANPASA",
                          dataList.istasyon_no %in% c(5203:5204)~"BAKIRKOY",
                          dataList.istasyon_no %in% c(5501,5603,5729,5803)~"ZEYTINBURNU",
                          dataList.istasyon_no %in% c(5605,5715:5716,5719,5721)~"FATIH",
                          dataList.istasyon_no %in% c(5726,5904:5907,5909:5911)~"BESIKTAS",
                          dataList.istasyon_no %in% c(5731)~"ESENLER",
                          dataList.istasyon_no %in% c(5901)~"BEYOGLU",
                          TRUE ~ ilce))%>%
                          distinct()

istasyon_ilce$dataList.adi<-istasyon_ilce$dataList.adi.x

#Join data with County Names
final_data<-inner_join(data,istasyon_ilce,by="dataList.adi")%>%
    select(dataList.istasyon_no.x,dataList.adi,dataList.bos,dataList.dolu,dataList.lat,
           dataList.lon,yogunluk,yogunluk_cat,ilce)

valid_ilce_data<-final_data%>%
    group_by(ilce)%>%
    summarise(count = n(),
              yogunluk = round(sum(dataList.bos)/(sum(dataList.bos)+sum(dataList.dolu)),2))



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Isbike Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "yogunluk",
                        "Density Ratio (This filter works only with Istanbul Map tab)",
                        min = 0,
                        max = 1,
                        value = c(0,1),
                        sep="",
                        step=0.1),
            
            em("This dashboard's aim is to show usage of several Isbike stations and their distribution based on counties of Istanbul.")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type = "tab", 
                        tabPanel("Istanbul Map", leafletOutput(outputId = "IstanbulMap")),
                        tabPanel("County Based Plot", plotOutput("IlcePlot"))
                        )
        )
    )
)

# Define server logic required to create ISBIKE Dashboard
server <- function(input, output) {
    
    
    output$IstanbulMap <- renderLeaflet({
        
        print(input$yogunluk)
        
        df<-final_data
        
        qpal <- colorQuantile("Dark2", unique(df$yogunluk), n = 4)
        
        mydf<-final_data
        
        mydf <- mydf %>% filter(yogunluk >= input$yogunluk[1] & yogunluk <= input$yogunluk[2])
        
        
        leaflet(mydf) %>% 
            addProviderTiles(provider = "CartoDB")%>%
            setView(lng = 29.05, lat = 41.05, zoom = 10.35)  %>% 
            addCircles(data = mydf, lat = ~ dataList.lat, lng = ~ dataList.lon, weight = 5, radius = ~(yogunluk)*500, 
                       color = ~qpal(df$yogunluk),fillOpacity = 0.2,
                       label = ~dataList.adi, labelOptions = labelOptions(noHide = F))%>%
            addLegend(pal = qpal, values = ~df$yogunluk, opacity = 1,title = "Density")
        
        
    })
    
    output$IlcePlot <- renderPlot({
        
        valid_ilce_data%>%
            ggplot()+
            geom_col(aes(x=reorder(ilce,-count),y=count),fill="steelblue",color="white")+
            geom_line(aes(x=reorder(ilce,-count),y=30*yogunluk),group = 1,size=1.3,color="coral1")+
            scale_y_continuous(sec.axis = sec_axis(~./30, name = "Density"))+
            theme_minimal()+
            theme(legend.position = 'none',axis.text.x = element_text(angle=45,hjust=1,vjust=1))+
            labs(x="County", y="Station Count", title = "County Based Station Count and Density Ratio")
       
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
