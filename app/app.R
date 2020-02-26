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


##################################################################################################################
#######################################----------UI----------#######################################################
##################################################################################################################

#--------------------Header------------------------------------------------------------------------------------------
ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                        title = "Fire in West America National Park",
                        titleWidth = 300,
                        tags$li(
                            a(
                                strong("ABOUT US"),
                                height = 40,
                                href = "https://github.com/TZstatsADS/Spring2020-Project2-group-4/blob/master/README.md",
                                title = "",
                                target = "_blank"
                            ),
                            class = "dropdown"
                        )
                    ),
                    
                    
                    
                    #-------------------Sidebar-------------------------------------------------------------------------------------------                    
                    
                    dashboardSidebar(
                        width=300,
                        sidebarMenu(
                            sidebarSearchForm(textId="searchtext",buttonId = "searchbutton",label="Search..."),
                            menuItem("Home", tabName = "Home", icon = icon("home")),
                            menuItem("Map", tabName = "Map", icon = icon("map")),
                            menuItem("Report", tabName = "Report", icon = icon("industry")),
                            menuItem("Reference", tabName = "Reference", icon = icon("th"))
                        )),
                    #--------------------Body---------------------------------------------------------------------------------------------
                    
                    dashboardBody(
                        tabItems(
                            
                            #--------------------Body--Home---------------------------------------------------------------------------------------                        
                            tabItem(tabName="Home",
                                    h2("Welcome!",style = "color:white"),
                                    br(),
                                    
                                    #--------------------Home--Gallery----------------------------------------------------------------------------
                                    semanticPage(style="width:1450px;height=200px",theme = "Sketchy",
                                        
                                        includeHTML("flip.HTML"),
                                        includeCSS("flip.css")
                                        
                                    ),
                                    br(),
                                    br(),
                                    
                                    #----------------Home--Welcome--------------------------------------------------------------------------------
                                    box(title="Overview",background ="red",width=12,height=110,
                                        
                                        p("This data consists of observations of individual trees that were subjected to prescribed fire in western US national parks. Information on individual trees include measurements of
                                    tree size, competition, and fire-caused damage. The data also includes estimates of plot-level vapor pressure deficit anomaly before fire.")
                                    ),
                                    br(),
                                    box(title="Guide", background ="red",width=6,height=280,
                                        p("This app aims to help prevent tree fires by examing past data."),
                                        p("- Map"),
                                        p("\t - Map-Damage: The red dots represent the scorch percentage of each spot. The dark red means ones that are above the selected scorch percentage while the light red means under the percentage."),
                                        p("- Report: The report part offers a highly personalized experience for users to explore the whole dataset at their wish. It includes an interactive table and an interactive plot section that can do several type of plots.")    
                                    ),
                                    box(title="Data", background ="red",width=6,height=280,
                                        p("- NPS park unit identifier. Acronyms refer to the following:"), 
                                        p("   BAND = Bandelier NM"),
                                        p("   BRCA = Bryce Canyon NP"), 
                                        p("   CRLA = Crater Lake NP"), 
                                        p("   ELMA = El Malpias NM"), 
                                        p("   GRCA = Grand Canyon NP"), 
                                        p("   LABE = Lava Beds NM"), 
                                        # p("   LAVO = Lassen Volcanic NP"), 
                                        # p("   SEKI = Sequoia and Kings Canyon NP"),  
                                        # p("   YOSE = Yosemite NP"), 
                                        # p("   ZION = Zion NP"),
                                        # p("- DBH: Tree stem diameter at breast height (1.37 meters")
                                    )
                                    
                                    
                                    
                                    # box(background ="red",width=4,height=200,
                                    #         includeCSS("flip.css"),
                                    #         includeHTML("flip.html")
                                    #         
                                    # )
                                   
                                    
                                    #img(src='pic3.jpg',align="middle",width=1000)
                                    
                            ),
                            
                            #-------------------Body--Map------------------------------------------------------------------------------------------                        
                            # tabItem(tabName = "Map",
                            #         h2("Interactive Map",style = "color:white"),
                            #         fluidRow(
                            #           box(width = 10,heights=500,
                            #               #this will create a space for us to display our map
                            #               leafletOutput(outputId = "mymap"), 
                            #               #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                            #               absolutePanel(top = 5, right = 50, 
                            #                             checkboxInput("markers", "Species", FALSE),
                            #                             checkboxInput("heat", "Scorched %", FALSE)
                            #               ))
                            #         )
                            # ),
                            tabItem(tabName = "Map",
                                    navbarPage( "Forest Fires in the Northwest", id="Project2-ADS",
                                                
                                                tabPanel("Map- Damage",
                                                         div(class="outer",
                                                             
                                                             tags$head(
                                                                 # Include our custom CSS
                                                                 includeCSS("styles.css"),
                                                                 includeScript("gomap.js")
                                                             ),
                                                             
                                                             leafletOutput(outputId = "mymap", width = "100%", height = "100%"),
                                                             
                                                             absolutePanel(id = "controls", class = "panel panel-defaul", fixed = TRUE,
                                                                           draggable = TRUE, top = 60, left = "auto", right = 20, 
                                                                           bottom = "auto", width = 330, height = "auto",
                                                                           
                                                                           h2("Tree Damage Data"),
                                                                           
                                                                           #numericInput("CrownScorchPercent", "Scorch Percentage", vars),
                                                                           
                                                                           sliderInput("CrownScorchPercent",
                                                                                       "Scorch Percentage",
                                                                                       min = min(data$CrownScorchPercent),
                                                                                       max = max(data$CrownScorchPercent),
                                                                                       step = .1,
                                                                                       value = 50),
                                                                           
                                                                           
                                                                           numericInput("DBH",
                                                                                        "Tree Size Minimum",
                                                                                        min = min(data$DBH),
                                                                                        max = max(data$DBH),
                                                                                        value = max(data$DBH))
                                                             )
                                                             
                                                         )),
                                                
                                                tabPanel("Map- Species",
                                                         div(class="outer",
                                                             
                                                             tags$head(
                                                                 # Include our custom CSS
                                                                 includeCSS("styles.css"),
                                                                 includeScript("gomap.js")
                                                             ),
                                                             
                                                             leafletOutput(outputId = "mymap2", width = "100%", height = "100%"),
                                                             
                                                             absolutePanel(id = "controls", class = "panel panel-defaul", fixed = TRUE,
                                                                           draggable = TRUE, top = 60, left = "auto", right = 20, 
                                                                           bottom = "auto", width = 330, height = "auto",
                                                                           
                                                                           h2("Species of Tree Damaged"),
                                                                           
                                                                           #numericInput("CrownScorchPercent", "Scorch Percentage", vars),
                                                                         
                                                                           selectInput("Species",
                                                                                       "Species",
                                                                                       choices = c('ABCO', 'PIPO', 'PSME', 'All'),
                                                                                       multiple = FALSE,
                                                                                       selected = 'All')
                                                                          
                                                             )
                                                             
                                                         ))
                                                # , If needed new map- add 'tabpanel' and start here tabPanel()
                                                
                                    )),
                            #----------------------Body--Report---------------------------------------------------------------------------------
                            
                            tabItem(tabName="Report",
                                    h2("Personalized Report",style = "color:white"),
                                    br(),
                                    fluidRow(
                                        box(
                                            title="Table",status="warning",solidHeader = TRUE,width=12,height=600,
                                            column(3,selectInput("Park","Park:",c("All",unique(as.character(data$ParkID))))),
                                            column(3,selectInput("Season","Season:",c("All",unique(as.character(data$Season))))),
                                            column(3,selectInput("Species","Species:",c("All",unique(as.character(data$Species))))),
                                            column(3,selectInput("Status","Status:",c("All",unique(as.character(data$Status))))),
                                            dataTableOutput(outputId="distTable",width="100%")
                                        ),
                                        
                            #----------------------Body-Plot-------------------------------------------------------------------    
                                        box(
                                            title="Plot",background = "red",solidHeader = TRUE,width=6,height=500,
                                            plotOutput("plot1")
                                        ),
                                        
                            #------------------------Plot--Input panel--------------------------------------------------------------------------------------------
                                        box(
                                            title="Inputs",background = "olive",solidHeader = TRUE,width=6,height=500,
                                            "Choose what you are interested in",
                                            # selectInput('type','Which kind of chart do you like',c("Pie",'Bar',"Plot"),selected='Pie'),
                                            # conditionalPanel("input$type=='pie'",
                                            #                  selectInput('xcol', 'X Variable', c("ParkID","PlotID","Season","Species","Status"))
                                            #                  ),
                                            # conditionalPanel("input$type=='bar'",
                                            #                  selectInput('xcol', 'X Variable', c("ParkID","PlotID","Season","Species","Status")),
                                            #                  selectInput('ycol', 'Y Variable', names(data))
                                            # ),
                                            # conditionalPanel("input$type=='pie'",
                                            #                  selectInput('xcol', 'X Variable', c("ParkID","PlotID","Season","Species","Status"))
                                            # )
                                            #selectInput('xcol', 'X Variable', names(plotdata)),
                                            #selectInput('ycol', 'Y Variable', names(plotdata))
                                            
                                            
                                            selectInput(
                                                'type','Which kind of chart do you like',c("Pie",'Bar',"Boxplot","Violin","Plot","Pairplot"),selected='Pie'
                                            ),
                                            
                                            # Only show this panel if the plot type is a pie
                                            conditionalPanel(
                                                condition = "input.type == 'Pie'",
                                                selectInput(
                                                    "xcol1", "X variable",
                                                    c("ParkID","Season","Species","Status"))
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.type == 'Bar'",
                                                selectInput(
                                                    "xcol2", "X variable",
                                                    c("ParkID","Season","Species","Status"))
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.type == 'Boxplot'",
                                                selectInput(
                                                    "xcol3", "X variable",
                                                    c("ParkID","Season","Species","Status")),
                                                selectInput(
                                                    "ycol3", "Y variable",
                                                    c("MaxCharHt","gro.10","BA_comp","DBH","aVpd.pre.10","trnd","ad.25"))
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.type == 'Violin'",
                                                selectInput(
                                                    "xcol4", "X variable",
                                                    c("ParkID","Season","Species","Status")),
                                                selectInput(
                                                    "ycol4", "Y variable",
                                                    c("MaxCharHt","gro.10","BA_comp","DBH","aVpd.pre.10","trnd","ad.25"))
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.type == 'Plot'",
                                                selectInput(
                                                    "xcol5", "X variable",
                                                    c("MaxCharHt","gro.10","BA_comp","aVpd.pre.10","trnd.25","ad.25")),
                                                selectInput(
                                                    "ycol5", "Y variable",
                                                    c("MaxCharHt","gro.10","BA_comp","aVpd.pre.10","trnd.25","ad.25")),
                                                selectInput(
                                                    "group2", "Color",
                                                    c("ParkID","Season","Species","Status"))
                                            ),
                                            
                                            conditionalPanel(
                                                condition = "input.type == 'Pairplot'",
                                                # selectInput(
                                                #     "xcol5", "X variable",
                                                #     c("ParkID","Season","Species","Status")),
                                                # selectInput(
                                                #     "ycol5", "Y variable",
                                                #     c("ParkID","PlotID","Season","Species","Status"))
                                                checkboxGroupInput("checkGroup", label = h3("Pair"), 
                                                                   choices = list("MaxCharHt"=1 , "gro.10"=2 , "BA_comp"=3 ,"aVpd.pre.10"=4,"trnd.25"=5,"ad.25"=6),
                                                                   selected = 1),
                                                selectInput(
                                                    "group", "Color",
                                                    c("ParkID","
                                                      Season","Species","Status"))
                                                # selectInput(
                                                #     "Dfilter", "Data filter",
                                                #     c("ParkID","
                                                #       Season","Species","Status"))
                                                
                                            )
                                            
                                            
                                        )
                                    )
                            ),
                            
                            #-------------------Body--Source---------------------------------------------------------------------------------
                            
                            tabItem(tabName="Reference",
                                    h2("Data source and other resources",style = "color:white"),
                                    setBackgroundImage(src = "pic2.jpg",shinydashboard = T),
                                    box(width=12,background = "red",
                                        h2("Data source"),
                                        p("The DroughtRxMort dataset consists of observations of individual trees that were subjected to prescribed fire in western US national parks. Information on individual trees include measurements of tree live/dead status, growth, size, competition, and fire-caused damage. The data also includes estimates of plot-level vapor pressure deficit anomaly before fire."),
                                        p("Link: https://www.sciencebase.gov/catalog/item/get/5e2a02f1e4b0a79317cf80b2"),
                                        h2("Image source"),
                                        p("https://www.cnn.com/2019/10/24/weather/kincade-fire-sonoma-county/index.html"),
                                        p("https://www.accuweather.com/en/business/the-2019-california-wildfires-caused-less-damage-than-the-last-two-devastating-seasons/643455"),
                                        p("https://www.cnn.com/2019/10/24/weather/gallery/kincade-fire/index.html"),
                                        p("https://www.nytimes.com/2020/01/10/world/australia/australia-wildfires-photos.html"),
                                        p("https://news.sky.com/story/australia-wildfires-set-to-get-worse-as-another-extreme-heatwave-looms-11895113")
                                    ),
                                    
                                    p(strong("For more infomation on this app itself and code, go check our github page!",style="color:white;text-align:center")),
                                    actionButton(
                                        inputId = "bttn2",
                                        label = "Go!",
                                        color = "danger",
                                        style = "pill",
                                        icon = icon("sliders"),
                                        block = TRUE,
                                        onclick = "window.open('https://github.com/TZstatsADS/Spring2020-Project2-group-4/blob/master/README.md')"
                                        
                                    )
                                    
                            )
                        )
                    ))

shinyApp(ui, server)


