# UI ----



ui <- dashboardPage(
  
  dashboardHeader(title = "Global Mortality Report",
                  titleWidth = 300,
                  tags$li(class = "dropdown", 
                          tags$div(
                            class = "btn-group",
                            actionButton( # Replace tags$button with actionButton
                              inputId = "btn",
                              label = "Get a Tour!",
                              class = "btn btn-primary btn-lg",
                              style = "background-color: primary; border-color: primary;"
                            )
                          )
                  )
                  
  ),
  
  
  dashboardSidebar(
    width = 250,
    HTML('<center><img src="https://cdn.discordapp.com/attachments/708905184511656000/1094308362201612369/Snake.png" width ="250"></center>'),
    introjsUI(),
    introBox(
      sidebarMenu(
        menuItem("Histogram by Age", tabName = "tab1", icon = icon("bar-chart")),
        menuItem("Histogram by Causes", tabName = "tab2", icon = icon("align-right")),
        menuItem("Line by Year", tabName = "tab3", icon = icon("line-chart")),
        menuItem("Radar by Age", tabName = "tab4", icon = icon("pie-chart"))
      ),
      data.step = 1,
      data.intro = "Data menu"
    ),
    
    fixedPanel(
      helpText("Data Source: "),
      helpText("https://www.who.int/data/data-collection-", style = "margin-top: -12px;"),
      helpText("tools/who-mortality-database", style = "margin-top: -12px;"),
      bottom = 0
    )
    
  ),
  
  ############################### Main Body #############################
  ########################################################################
  dashboardBody(
    useShinyalert(force=TRUE),
    ############################### First Panel #############################
    tabItems(
      tabItem(tabName = "tab1",
              span("Deaths by sex and age for a selected country and year", style = "color:Black; font-size: 30px;"),
              br(),
              br(),
              ## Control bar of 1st panel
              introBox(
                fluidRow(
                  column(2,
                         selectInput(
                           inputId = "df1_A",
                           label = "Data Type:",
                           width = "150px",
                           choices = c("Morticd_final","Mortality_rate"),
                           selected = "Morticd_final"
                         )
                         
                         
                  ),
                  column(1,
                         selectInput(
                           inputId = "Year1_A",
                           label = "Year(A):",
                           width = "100px",
                           choices = Year_List,
                           selected = 2020
                         )
                         
                         
                  ),
                  
                  
                  column(2,
                         pickerInput(
                           inputId = "Cause1_A",
                           label = "Cause(A):",
                           multiple = TRUE,
                           choices = Cause_List,
                           selected = "Neoplasms",
                           options = pickerOptions(
                             actionsBox = TRUE,
                             `selected-text-format` = "count",
                             liveSearch = TRUE,
                             liveSearchPlaceholder = "Search country"
                           ))),
                  
                  column(2,
                         pickerInput(
                           inputId = "Country1_A",
                           label = "Country(A):",
                           multiple = FALSE,
                           choices = Country_List,
                           selected = "United States of America",
                           options = pickerOptions(
                             actionsBox = TRUE,
                             `selected-text-format` = "count",
                             liveSearch = TRUE,
                             liveSearchPlaceholder = "Search country"
                             
                           ))),
                  
                  column(1,
                         pickerInput(
                           inputId = "Age1_A",
                           label = "Age(A):", 
                           multiple = TRUE,
                           choices = Age_List,
                           selected =Age_List ,
                           options = pickerOptions(
                             actionsBox = TRUE,
                             `selected-text-format` = "count"
                           ))),
                  
                  column(2,
                         checkboxGroupButtons(
                           inputId = "Sex1_A",
                           width = "500px",
                           label = "Sex(A):",
                           choices = Sex_List,
                           selected=c("All","Male","Female"),
                           status = "primary",
                           checkIcon = list(
                             yes = icon("ok", 
                                        lib = "glyphicon"),
                             no = icon("remove",
                                       lib = "glyphicon"))
                         )),
                  
                  
                  
                ),
                
                
                fluidRow(
                  column(2,
                         selectInput(
                           inputId = "df1_B",
                           label = "Data Type:",
                           width = "150px",
                           choices = c("Morticd_final","Mortality_rate"),
                           selected = "Morticd_final"
                         )
                         
                         
                  ),
                  column(1,
                         selectInput(
                           inputId = "Year1_B",
                           label = "Year(B):",
                           width = "100px",
                           choices = Year_List,
                           selected = 2020
                         )
                         
                         
                  ),
                  
                  
                  column(2,
                         pickerInput(
                           inputId = "Cause1_B",
                           label = "Cause(B):",
                           multiple = TRUE,
                           choices = Cause_List,
                           selected = "Neoplasms",
                           options = pickerOptions(
                             actionsBox = TRUE,
                             `selected-text-format` = "count",
                             liveSearch = TRUE,
                             liveSearchPlaceholder = "Search country"
                           ))),
                  
                  column(2,
                         pickerInput(
                           inputId = "Country1_B",
                           label = "Country(B):",
                           multiple = FALSE,
                           choices = Country_List,
                           selected = "Japan",
                           options = pickerOptions(
                             actionsBox = TRUE,
                             `selected-text-format` = "count",
                             liveSearch = TRUE,
                             liveSearchPlaceholder = "Search country"
                             
                           ))),
                  
                  column(1,
                         pickerInput(
                           inputId = "Age1_B",
                           label = "Age(B):", 
                           multiple = TRUE,
                           choices = Age_List,
                           selected =Age_List ,
                           options = pickerOptions(
                             actionsBox = TRUE,
                             `selected-text-format` = "count"
                           ))),
                  
                  column(2,
                         checkboxGroupButtons(
                           inputId = "Sex1_B",
                           width = "500px",
                           label = "Sex(B):",
                           choices = Sex_List,
                           selected=c("All","Male","Female"),
                           status = "primary",
                           checkIcon = list(
                             yes = icon("ok", 
                                        lib = "glyphicon"),
                             no = icon("remove",
                                       lib = "glyphicon"))
                         )),
                  
                  
                  
                ),
                data.step = 2,
                data.intro = "Control strip"
              ),
              
              introBox(
                valueBoxOutput("VBox1"),
                
                valueBoxOutput("VBox2"),
                br(),br(),br(),br(),br(),br(),br(),
                data.step = 3,
                data.intro = "Value box"
              ),
              
              introBox(
                fluidRow(
                  column(6,
                         box(
                           width = 12,
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           title = "Country A :",
                           fluidRow(
                             column(12, plotlyOutput("bar1_A"))
                           )
                         )
                         
                  ),
                  column(6,
                         box(
                           width = 12,
                           status = "danger",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           title = "Country B :",
                           fluidRow(
                             column(12, plotlyOutput("bar1_B"))
                           )
                         )
                         
                  )
                  
                ),
                data.step = 4,
                data.intro = "Data Visualization"
              )
              
              
      ),
      
      ############################### Second Panel #############################
      tabItem(tabName = "tab2",
              span("Deaths by causes for the selected Country, Year, Age, and Sex.", style = "color:Black; font-size: 30px;"),
              br(),
              br(),
              fluidRow(
                column(1,
                       selectInput(
                         inputId = "Year2",
                         label = "Year:",
                         multiple = FALSE,
                         width = "100px",
                         choices = Year_List,
                         selected = 2020
                       )
                       
                       
                ),
                
                
                column(2,
                       pickerInput(
                         inputId = "Cause2",
                         label = "Cause:",
                         multiple = TRUE,
                         choices = Cause_List,
                         selected = Cause_List,
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                         ))),
                
                column(2,
                       pickerInput(
                         inputId = "Country2",
                         label = "Country:",
                         multiple = FALSE,
                         choices = Country_List,
                         selected = "United States of America",
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                           
                         ))),
                
                column(2,
                       pickerInput(
                         inputId = "Age2",
                         label = "Age:", 
                         multiple = TRUE,
                         choices = Age_List,
                         selected = Age_List,
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count"
                         ))),
                
                column(2,
                       pickerInput(
                         inputId = "Sort2",
                         label = "Sort:", 
                         multiple = FALSE,
                         choices = c("From High to Low","From Low to High"),
                         selected = c("From High to Low"),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count"
                         ))),
                
                
                column(2,
                       radioGroupButtons(
                         inputId = "Sex2",
                         width = "500px",
                         label = "Sex:",
                         choices = Sex_List,
                         selected=c("All"),
                         status = "primary",
                       )),
                
                
                
                
                
              ),
              
              fluidRow(
                column(12,
                       box(
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         title = "Deaths by causes",
                         fluidRow(
                           column(12, plotlyOutput("bar2_A"))
                         )
                       )
                       
                ),
                
                
              ),
              fluidRow(
                column(12,
                       box(
                         width = 12,
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         title = "Deaths by causes with age distribution",
                         fluidRow(
                           column(8, plotOutput("bar2_B")),column(4,plotOutput("bar2_C"))
                         )
                       )
                       
                ),
                
                
              ),
              
              
      ),
      
      ############################### Third Panel #############################
      tabItem(tabName = "tab3",
              span("Trends in cause-specific mortality with Linear Regression Prediction", style = "color:Black; font-size: 30px;"),
              br(),
              br(),
              fluidRow(
                column(3,
                       sliderInput(
                         label = "Year:",
                         inputId = "Year3_A",
                         width = "400px",
                         min =2008,
                         max = 2021,
                         value = c(2008, 2021)
                       )
                       
                       
                ),
                column(2,
                       pickerInput(
                         inputId = "Country3_A",
                         label = "Country:",
                         multiple = FALSE,
                         choices = Country_List,
                         selected = "United States of America",
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count"
                           
                         ))
                       
                       
                ),
                
                
                column(2,
                       pickerInput(
                         inputId = "Cause3_A",
                         label = "Cause:",
                         multiple = TRUE,
                         choices = Cause_List,
                         selected = "Neoplasms",
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                         ))),
                
                
                
                column(2,
                       radioGroupButtons(
                         inputId = "Type3_A",
                         label = "Type",
                         choices = c("Sex","Age"),
                         selected=c("Sex"),
                         status = "primary"
                       ) ),   
              ),
              
              fluidRow(
                column(12,
                       box(
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         title = "Trends in cause-specific mortality by all age or sex for a selected country",
                         fluidRow(
                           column(12, plotlyOutput("line3_A")))
                       )
                )
                
              ),
              
              fluidRow(
                column(3,
                       sliderInput(
                         label = "Year:",
                         inputId = "Year3_B",
                         width = "400px",
                         min =2008,
                         max = 2021,
                         value = c(2008, 2021)
                       )
                       
                       
                ),
                column(2,
                       pickerInput(
                         inputId = "Country3_B",
                         label = "Country:",
                         multiple = TRUE,
                         choices = Country_List,
                         selected = c("United States of America","Japan"),
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                         ))
                       
                       
                ),
                
                
                column(2,
                       pickerInput(
                         inputId = "Cause3_B",
                         label = "Cause:",
                         multiple = TRUE,
                         choices = Cause_List,
                         selected = "Neoplasms",
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                         ))),
                
                column(1,
                       pickerInput(
                         inputId = "Age3_B",
                         label = "Age:", 
                         multiple = TRUE,
                         choices = Age_List,
                         selected =Age_List ,
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count"
                         ))),
                
                
                
                column(2,
                       radioGroupButtons(
                         inputId = "Sex3_B",
                         label = "Sex:",
                         choices = c("Male","Female","All"),
                         selected=c("Male"),
                         status = "primary"
                       ) ),
                
                
                
              ),
              
              fluidRow(
                column(12,
                       box(
                         width = 12,
                         status = "warning",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         title = "Trends in cause-specific mortality by country(s) for a selected age group and sex",
                         fluidRow(
                           column(12, plotlyOutput("line3_B")))
                       )
                )
                
              ),
              
              
              
              
              
      ),
      
      ############################### Fourth Panel #############################
      tabItem(tabName = "tab4",
              span("Mortality Radar by Age and Cause", style = "color:Black; font-size: 30px;"),
              br(),
              br(),
              fluidRow(
                
                column(1,
                       selectInput(
                         inputId = "Year4_A",
                         label = "Year:",
                         width = "100px",
                         choices = Year_List,
                         selected = 2020
                       )
                       
                       
                ),
                
                
                column(2,
                       pickerInput(
                         inputId = "Cause4_A",
                         label = "Cause:",
                         multiple = TRUE,
                         choices = Cause_List,
                         selected = Cause_List,
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                         ))),
                
                column(2,
                       pickerInput(
                         inputId = "Country4_A",
                         label = "Country:",
                         multiple = FALSE,
                         choices = Country_List,
                         selected = "United States of America",
                         options = pickerOptions(
                           actionsBox = TRUE,
                           `selected-text-format` = "count",
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Search country"
                           
                         ))),
                
                column(2,
                       radioGroupButtons(
                         inputId = "Sex4_A",
                         width = "500px",
                         label = "Sex:",
                         choices = Sex_List,
                         selected=c("All"),
                         status = "primary",
                       )),
                fluidRow(
                  column(12,
                         box(
                           width = 12,
                           status = "info",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           title = "Mortality by Age, for selected causes ",
                           fluidRow(
                             column(12, plotOutput("bar4_A"))
                           )
                         )
                         
                  ),
                  
                  
                ),
                
                
                
              ),
              
      )
    )
  )
)
