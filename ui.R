library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title="SIAC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Iowa Assessment/ISASP", tabName = "stateAssessment", icon = icon("dashboard")),
    menuItem("MAP Results", tabName = "mapResults", icon = icon("map") ),
    menuItem("Math Facts", tabName = "mathFacts", icon = icon("calculator")),
    menuItem("Honors Science", tabName = "cutoffs", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "stateAssessment", 
        column( width = 12, 
                box( width = 12, 
                     "Controls", 
                     uiOutput("cohort_select_SA")
                )
        ),

        column( width = 6,
                plotOutput("assessmentCohortsELA"), 
                dataTableOutput("reading_table_SA")
                ), 
        column( width = 6,
                plotOutput("assessmentCohortsMath"), 
                dataTableOutput("math_table_SA")
                
        ), 
       
       
        
     
        
        )
    )
  ),
  title = "SIAC Dashboard"
  
)
