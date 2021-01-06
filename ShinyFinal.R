pti <- c("devtools","tidyverse","rio","shiny","dplyr","ggplot2","rio","xts","shinydashboard",
         "leaflet","leaflet.extras","extrafont","ggmap","sf","DT","dygraphs")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

library(devtools)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rio)
library(leaflet)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggmap)
library(leaflet.extras)
library(sf)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)

options(encoding="UTF-8")
Sys.setlocale(category="LC_ALL", locale="Turkish")

githubURL <- "https://github.com/pjournal/mef04g-rsizlar/blob/gh-pages/Data/data.RData?raw=true"
load(url(githubURL))


#Clean Month and City variables
data$Month<-str_sub(data$Month,start= 4)

data<-data %>% 
    separate(City, c("CityCode", "City"),sep = "( - )")

data<-data%>%
    mutate(Month=case_when(data$Month=="Ocak"~"01",
                           data$Month=="Subat"~"02",
                           data$Month=="Mart"~"03",
                           data$Month=="Nisan"~"04",
                           data$Month=="Mayis"~"05",
                           data$Month=="Haziran"~"06",
                           data$Month=="Temmuz"~"07",
                           data$Month=="Agustos"~"08",
                           data$Month=="Eylul"~"09",
                           data$Month=="Ekim"~"10",
                           data$Month=="Kasim"~"11",
                           data$Month=="Aralik"~"12"))

data<-data%>%
    mutate(TradingType=case_when(data$TradingType=="Ithalat"~"Import",
                                 data$TradingType=="Ihracat"~"Export"))

#Create Date Variable from Year and Month
data$Date<-paste0(data$Year,as.character("-"),data$Month,as.character("-01"))
data$Date<-as.Date(data$Date, format="%Y-%m-%d")

data<-left_join(data,reg)

data.frame(Total_Missing=sapply(data,function(x){sum(is.na(x))}))

turkceden_ingilizceye <- function(dataset){
    turkce_harfler<- c("Ç","Þ","Ð","Ý","Ü","Ö","ç","þ","ð","ý","ü","ö")
    ingilizce_harfler<- c("C","S","G","I","U","O","c","s","g","i","u","o")
    dataset=mgsub(turkce_harfler,ingilizce_harfler,dataset)
    return(dataset)
}


mgsub <- function(pattern, replacement, x, ...) {
    n = length(pattern)
    if (n != length(replacement)) {
        stop("pattern and replacement do not have the same length.")
    }
    result = x
    for (i in 1:n) {
        result <- gsub(pattern[i],replacement[i],result)
    }
    return(result)
}

tur@data$NAME_1 <- turkceden_ingilizceye(tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("K. Maras", "Kahramanmaras",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Kinkkale","Kirikkale",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Zinguldak", "Zonguldak", tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Afyon","Afyonkarahisar", tur@data$NAME_1 )

tur@data$NAME_1 <- toupper(tur@data$NAME_1) 

tur@data$NAME_1 <- gsub("Ý","I", tur@data$NAME_1 )

data <-data %>% left_join(koordinat,by=c("City")) 

#data  %>% 
#    filter(Year>=2018)%>%
#    select(City,TradingType,Year,AmountUSD,LONGITUDE,LATITUDE)%>%
#    group_by(City,TradingType,Year,LONGITUDE,LATITUDE) %>% 
#    summarise(AmountUSD=sum(AmountUSD))%>%
#    pivot_wider(names_from = TradingType, values_from = AmountUSD)%>%
#    mutate(Balance=round((Export-Import)/1000000,2))%>%
#    select(City,LONGITUDE,LATITUDE,Balance)

map <- tur
data_mean <- data %>%
    select(City,Year,TradingType,AmountUSD,LONGITUDE,LATITUDE)%>%
    group_by(City,TradingType,LONGITUDE,LATITUDE,Year) %>% 
    summarise(AmountUSD=sum(AmountUSD))%>%
    pivot_wider(names_from = TradingType, values_from = AmountUSD)%>%
    mutate(Balance=round((Export-Import)/1000000,2))%>%
    select(City,Year,LONGITUDE,LATITUDE,Balance,Export,Import)
head(data_mean)


data_mean <-data_mean%>%ungroup() %>%select(-LONGITUDE,-LATITUDE)
head(data_mean)
colnames(data_mean)[1] <- "county"
colnames(data_mean)[2] <- "year"
df<-data_mean
df<-na.omit(df)
head(map@data)


plotly_data<-data%>%
    select(Date,TradingType,AmountUSD)%>%
    group_by(Date,TradingType)%>%
    summarise(Total=round(sum(AmountUSD)/1000000000,2))%>%
    pivot_wider(names_from = TradingType, values_from = Total)%>%
    mutate(Balance = Export-Import)

# header board
header <- dashboardHeader(
    title = 'Turkey Trading Analysis'
    # task list for status of data processing
    , dropdownMenuOutput('task_menu'))


sidebar <- dashboardSidebar(
    sidebarMenu(
        id = 'menu_tabs'
        , menuItem('Export/Import Balance Analysis', tabName = 'Balance')
        , menuItem('Heat Map', tabName = 'HeatMap')
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = 'Balance',
            plotlyOutput('plotly'),
            dateRangeInput("Date", "Date range:",
                           start = min(plotly_data$Date),
                           end   = max(plotly_data$Date))),
        tabItem(tabName = 'HeatMap',
                leafletOutput('HeatMap'),
                selectInput(
                    inputId = "variableselected",
                    label = "Select variable",
                    choices = c("Balance")
                ),
                selectInput(
                    inputId = "yearselected",
                    label = "Select year",
                    choices = 2013:2019
                ),
                dygraphOutput(outputId = "timetrend"),
                DTOutput(outputId = "table"))
        
    )
)



# Define UI for application that draws a histogram
ui <- dashboardPage(
    title = 'Trading Analysis',
    dashboardHeader(),
    sidebar,
    body
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    output$plotly = renderPlotly({
        #Export, Import, Balance Plot
        
        cols<- c("Export"="#F8766D","Import"="#00BFC4","Balance"="#FFDB6D")
        
        plotly_data<-plotly_data %>% filter(Date >= input$Date[1] & Date <= input$Date[2])
        
        p<-plotly_data%>%
            ggplot() +
            geom_col(aes(x=Date, y=Balance, fill = "Balance"),size = 0.5, color = "#C4961A")+
            geom_line(aes(x=Date, y=Import,color="Export"),size=1) +
            geom_line(aes(x=Date, y=Export,color="Import"),size=1) +
            scale_colour_manual(name="Lines",values=cols) + scale_fill_manual(name="Bar",values=cols)+
            theme_test() + 
            labs(x="Date", y="Total Amount (billion dollars)", title = "Yearly Total Export/Import Comparison")
        
        ggplotly(p,tooltip = c("Balance","Import","Export","Date"))%>%
            layout(legend = list(orientation = "h", x = 0.25, y = -0.2))
    })
    
    output$table <- renderDT(df)
    
    output$timetrend <- renderDygraph({
        dataxts <- NULL
        counties <- unique(df$county)
        for (l in 1:length(counties)) {
            datacounty <- df [df$county == counties[l], ]
            dd <- xts::xts(datacounty[, input$variableselected],
                           as.Date(paste0(datacounty$year, "-01-01"))
            )
            dataxts <- cbind(dataxts, dd)
        }
        colnames(dataxts) <- counties
        
        dygraph(dataxts) %>%
            dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
        
        d1$x$css <- "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "
        d1
    })
    
    output$HeatMap <- renderLeaflet({
        
        # Add data to map
        # CHANGE 1980 by input$yearselected
        datafiltered <- df[which(df$year == input$yearselected), ]
        ordercounties <- match(map@data$NAME_1, datafiltered$county)
        map@data <- datafiltered[ordercounties, ]
        
        # Create variableplot
        # ADD this to create variableplot
        map$variableplot <- as.numeric(map@data[[input$variableselected]],stringasFactors=TRUE)
        
        # Create leaflet
        # CHANGE map$cases by map$variableplot
        bins <- c(-Inf,-15000, -5000, 0, 500, 1000, 3000, 5000, Inf)
        pal <- colorBin("YlOrRd", df$Balance, bins=bins, na.color = "#bdbdbd")
        
        # CHANGE map$cases by map$variableplot
        labels <- sprintf("<strong>%s</strong><br/>%s $ ",map@data$county,prettyNum(map@data$variableplot, format = "f", big.mark = ".",decimal.mark ="," )
        ) %>% lapply(htmltools::HTML)
        
        # CHANGE cases by variableplot
        l <- leaflet(map) %>%
            addProviderTiles("Stamen.TonerBackground", group = "Terrain")%>%
            addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB"  ) %>% 
            addProviderTiles("Esri.OceanBasemap", group = "Esri") %>% 
            addPolygons(smoothFactor = 0.3, fillOpacity = 10,weight = 2,
                        opacity = 1,color = "white",
                        dashArray = "3",
                        highlight = highlightOptions(stroke = 2,
                                                     weight = 4,
                                                     color = "#666",
                                                     dashArray = NULL,
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),fillColor = ~pal(variableplot),label=labels,labelOptions = labelOptions(
                                                         style = list("font-weight" = "normal", padding = "3px 8px"),
                                                         textsize = "15px",
                                                         direction = "auto")) %>%
            leaflet::addLegend(pal = pal, values = ~variableplot, opacity = 1.0) %>%
            
            addLayersControl(baseGroups = c( "CartoDB", "Esri","Terrain")) %>% 
            setView(lng = 33.243322, lat =	41.963745, zoom =5.4)%>% addSearchOSM()%>% addReverseSearchOSM()%>% addResetMapButton()
        
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
