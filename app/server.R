library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
#install.packages("shinyWidgets")
library(shinyWidgets)
library(leaflet.extras)
library(shiny.semantic)
library(ggplot2)
library(shinyjs)
library(dplyr)
#install.packages("GGally")
library(GGally)



rsconnect::setAccountInfo(name='morgan-yang-0710', token='527B9FC0D4ED12F5BB41CA8893DD106C', secret='yPbVAfJeItYeMmjN7MLl90ye7z1q/g+DqH8PxkhO')

#rsconnect::setAccountInfo(name='ads2020',
#                          token='495DCB1F043DE3D88193B8F311F05191',
#                         secret='e9D7Bgm1gT0KZGEPeUaUIfZ9JDpZS37ZNDnLm0MN')

data <- read.csv('data/DroughtRxMort.csv')
data=data%>%
    mutate(Status=as.factor(Status))


#define the color pallate for the magnitidue of the earthquake
pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$CrownScorchPercent)

#define the color of for the type of 
pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = data$Species
)

##################################################################################################################
#######################################----------Server----------#######################################################
##################################################################################################################



server=function(input,output){
    #------------table (Reports Tab)----------------------------------------------------------------------------------------------------------------
    
    output$distTable=DT::renderDataTable(
        DT::datatable({
            Distdata=data[,-c(2,3,4,6,7,8)]
            if(input$Park!="All"){
                Distdata=Distdata[Distdata$ParkID==input$Park,]
            }
            if(input$Season!="All"){
                Distdata=Distdata[Distdata$Season==input$Season,]
                
            }
            if(input$Species!="All"){
                Distdata=Distdata[Distdata$Species==input$Species,]
                
            }
            if(input$Status!="All"){
                Distdata=Distdata[Distdata$Status==input$Status,]
            }
            Distdata
        }),
        options=list(
            autoWidth=TRUE,
            lengthMenu=c(25),
            scrollX=TRUE
            #columnDefs=list(list(width='300px',targets="_all"))
        )
    )
    
    #--------------------plot (Reports tab charts)------------------------------------------------------------------------------------------------------------
    
    candidate=c("MaxCharHt","gro.10","BA_comp","aVpd.pre.10","trnd.25","ad.25")
    candidateID=c(13,14,15,17,18,19)
    output$plot1 <- renderPlot({
        
        if(input$type=="Plot") {
            #plot(input$xcol5,input$ycol5,xlim=c(0,10),ylim=c(0,10))
            data%>%ggplot()+
                geom_point(aes_string(x=input$xcol5,y=input$ycol5,color=input$group2))+
                theme_light()
        }
        #elseif (input$type=="bar") barplot()
        else if(input$type=="Pie") {
            #pie(table(data[as.character(input$xcol1)]),col=rainbow(8))
            #legend("topright", unique(data[input$xcol1]), cex = 0.8,
            #       fill = rainbow(8))
            data%>%ggplot()+
                geom_bar(aes_string(x=input$xcol1,fill=input$xcol1),show.legend = T)+
                coord_polar("y", start=0)+
                theme_light()
            
        }
        else if(input$type=="Bar") {
            #barplot(table(data[as.character(input$xcol2)]))
            data%>%ggplot()+
                geom_bar(aes_string(x=input$xcol2,fill=input$xcol2),show.legend = T)+
                theme_light()
        }
        else if(input$type=="Boxplot") {
            data%>%ggplot()+
                geom_boxplot(aes_string(x=input$xcol3,y=input$ycol3,fill=input$xcol3))+
                theme_light()
            
        }
        else if(input$type=="Violin") {
            data%>%ggplot()+
                geom_violin(aes_string(x=input$xcol4,y=input$ycol4,fill=input$xcol4))+
                theme_light()
            
        }
        else if(input$type=="Pairplot") {
            ggpairs(data,columns=candidateID[as.numeric(input$checkGroup)],aes_string(color=input$group))+
                theme_light()
            
        }
    })
    
    #--------------------map-------------------------------------------------------------------------------------------------------------- 
    
    #   #create the map
    #   output$mymap <- renderLeaflet({
    #     leaflet(data) %>% 
    #       setView(lng = -113, lat = 37.45, zoom = 5)  %>% #setting the view over ~ center of North America
    #       addTiles() %>% 
    #       addCircles(data = data, lat = ~ Lat, lng = ~ Long, weight = 1, radius = ~sqrt(CrownScorchPercent), 
    #                  popup = ~as.character(MaxCharHt), label = ~as.character(paste0("MaxCharHit: ", sep = " ", CrownScorchPercent)), 
    #                  color = ~pal(CrownScorchPercent), fillOpacity = 10)
    #   })
    #   
    #   #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, 
    #   #when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server
    #   #to update the map when the checkboxes are unchecked.
    #   observe({
    #     proxy <- leafletProxy("mymap", data = data)
    #     proxy %>% clearMarkers()
    #     if (input$markers) {
    #       proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(Species), fillOpacity = .2, 
    #                                  label = ~as.character(paste0("Scorch Percentage: ", sep = " ", CrownScorchPercent))) %>%
    #         addLegend("bottomright", pal = pal2, values = data$Species,
    #                   title = "Species",
    #                   opacity = .5)}
    #     else {
    #       proxy %>% clearMarkers() %>% clearControls()
    #     }}
    #   )
    #   observe({
    #     proxy <- leafletProxy("mymap", data = data)
    #     proxy %>% clearMarkers()
    #     if (input$heat) {
    #       proxy %>%  addHeatmap(lng=~Long, lat=~Lat, intensity = ~CrownScorchPercent, blur =  10, max = 10, radius = 15) 
    #     }
    #     else{
    #       proxy %>% clearHeatmap()}
    #   })
    #   
    # }
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng =  -113, lat = 37.45, zoom = 5)
        
    })
    
    # This is to maintain the circle and legend of the variables that are inputted.
    observe({
        scorch <- input$CrownScorchPercent
        dbh <- input$DBH
        # maxchar <- input$MaxCharHt
        # spec <- input$species
        # seas <- input$Season
        # stat <- input$Status
        
        
        if (scorch >= median(data$CrownScorchPercent)) {
            colorData <- ifelse(data$CrownScorchPercent >= 
                                    (input$CrownScorchPercent), "Scorched More than %", "Scorched less than %")
            pal <- colorFactor("viridis", colorData)
            # pal <- colorNumeric(
            #     palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
            #     domain = colorData)
            
        } else {
            colorData <- data[[scorch]]
            pal <- colorBin("viridis", colorData, min(data$CrownScorchPercent), pretty = FALSE)
            # pal <- colorNumeric(
            #     palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
            #     domain = colorData)
        }
        
        if (dbh >= median(data$DBH)) {
            radius <- ifelse(input$DBH >= data$DBH,input$DBH * 150,
                             100)
        } else {
            radius <- data[[dbh]]/max(data[[dbh]]) * 150
            
        }
        
        leafletProxy("mymap", data=data) %>%
            addCircles(data = data, lng = ~Long, lat = ~Lat, radius = radius, layerId = ~ParkID,
                       stroke = FALSE, fillOpacity = .7, fillColor = pal(colorData)) %>%    
            addLegend("bottomleft", pal = pal, values = colorData, title = scorch,
                      layerId = "colorLegend")
        
    })
    
    #show a popup at a given location- IN PROGRESS (NOT SHOWING UP ON MAP CURRENTLY)
    
    ShowParkIDPopup <- function(dat, Long, Lat) {
        selectedPark <- dat[dat$ParkID == ParkID,]
        content <- as.character(tagList(
            tags$h4("Scorch %:", as.integer(selectedPark$CrownScorchPercent)),
            tags$strong(HTML(sprintf(selectedPark$ParkID, 
                                     selectedPark$PlotID
            ))), tags$br(),
            sprintf("Species", list(levels(selectedPark$Species))), tags$br(),
            sprintf("Height of Burn:", selectedPark$MaxCharHt)
        ))
        leafletProxy("mymap") %>% addPopups(Long, Lat, content, layerId = ParkID)
    }
    
    # When map is clicked, show a popup with park info
    observe({
        leafletProxy("mymap") %>% clearPopups()
        event <- input$map_shape_click
        if (is.null(event$id))
            return()
        
        isolate({
            ShowParkIDPopup(event$id, event$Lat, event$Long)
        })
    })
    
    #######INTERACTIVE MAP- SPECIES
    
    # Create the map
    output$mymap2 <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng =  -113, lat = 37.45, zoom = 5)})
    
    observe({
        spec <- req(input$species)
        # maxchar <- input$MaxCharH
        # seas <- input$Season
        # stat <- input$Status
        
        
        if (spec == "All") {
            radius2 <- ifelse(spec == "All", 100000000, ifelse(spec == "ABCO", 1000000, ifelse(spec == "PIPO", 10000)))
        } else {
            radius2 <- ifelse(spec == "PSME", 1000)
            
        }
        
        leafletProxy("mymap2", data=data) %>%
            addCircles(data = data, lng = ~Long, lat = ~Lat, radius = radius2, layerId = ~ParkID,
                       stroke = FALSE, fillOpacity = .7, fillColor = pal(data$MaxBurnYear)) %>%    
            addLegend("bottomleft", pal = pal, values = data$MaxBurnYear, title = "Max Burn Year",
                      layerId = "colorLegend")
        
    })
    
    
}