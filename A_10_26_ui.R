library(shiny)
library(shinydashboard)
library(DT)
library(party)

png(file="tree.png")

png(file="tree_id3.png")

ui<-dashboardPage(skin = "purple",
  dashboardHeader(title = "UCI Machine Learning Repository",dropdownMenu(type = "messages",
                                                                         messageItem(
                                                                           from = "New Patient",
                                                                           message = "How do I register?",
                                                                           icon = icon("question"),
                                                                           time = "13:45"
                                                                         ),
                                                                         messageItem(
                                                                           from = "Support",
                                                                           message = "The new server is ready.",
                                                                           icon = icon("life-ring"),
                                                                           time = "2014-12-01"
                                                                         )
  )),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("RandomForest ", tabName = "RandomForest", icon = icon("random")),
      
      menuItem("DecisionTree c45", tabName = "DecisionTree_c45", icon = icon("heartbeat")),
      
      menuItem("DecisionTree ID3 ", tabName = "DecisionTree_ID3", icon = icon("tree"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      # First tab content
      
      tabItem(tabName = "RandomForest",
              h2("Random Forest Algorithm"),
              
              fluidRow(
                
                box("This page will show the working of Random Forest algorithm", br(),br(), "By using processed.cleveland data",
                  background = "maroon", solidHeader = TRUE,
                  plotOutput("plot1", height = 100,width = 400)),
                
                dataTableOutput("tableA"),
                
                imageOutput("image", height = 5,width = 5)
                #fluidRow(
                  
                 #imageOutput("image")
                  
                #)
                
              )
              
      ),
      
      # 2 tab content
      tabItem(tabName = "DecisionTree_c45",
              
              h2("DecisionTree_c45"),
              
              mainPanel(plotOutput(outputId = "p1"),height = 100,width = 100),
              
              fluidRow(tableOutput("tableC"))
              
              
              
      ),
      
      # 3 tab content
      
      tabItem(tabName = "DecisionTree_ID3",
              
              h2("DecisionTree_ID3"),
              
              
              ##fluidRow( dataTableOutput("tableB")),
              
              mainPanel(plotOutput(outputId="p2"),height = 100,width = 100),
              fluidRow( dataTableOutput("tableB")),
              
              imageOutput("image2",height = 1,width = 1)
      )
    )
  )
)