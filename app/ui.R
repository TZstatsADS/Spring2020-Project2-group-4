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




