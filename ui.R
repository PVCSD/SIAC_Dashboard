library(shinydashboard)


ui <- dashboardPage(skin = 'black',
  dashboardHeader(title="SIAC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("State Assessment", icon = icon("dashboard"),
             menuSubItem("Cohort Trends", 
                         tabName = "cohort_SA", 
                         icon=icon("line-chart"),
                         ),
             menuSubItem("Grade Trends",
                         tabName = "grade_SA",
                         icon=icon("bar-chart"),
                         )
             ),
    menuItem("MAP Results",  icon = icon("map"),
             menuSubItem("Projected vs. Actual", 
                         tabName = "cohort_projections_MAP", 
                         icon=icon("line-chart"),
             ),
             menuSubItem("Grade Growth", 
                         tabName = "grade_MAP",
                         icon=icon("seedling"),
                         )
             ),
    menuItem("Math Facts", tabName = "mathFacts", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      
      
      ##### State Assessment COHORT Tab #####
      tabItem(
        tabName = "cohort_SA", 
        column( width = 12, 
                box( width = 12, 
                     "Controls", 
                     uiOutput("cohort_select_SA")
                )
        ),

        column( 
          width = 6,
          plotOutput("assessmentCohortsELA"), 
          dataTableOutput("reading_table_SA")
          ), 
        column( 
          width = 6,
          plotOutput("assessmentCohortsMath"),
          dataTableOutput("math_table_SA")
          )
        ),
      ##### State Assessment Grade Tab #####
      tabItem(
        tabName = "grade_SA", 
        column(width=6, 
        box( width = 12,
          column( width  = 6,
            selectInput( 
              "subject_select_SA", 
              "Select Subject", 
              choices = c("Reading", "Math")
            ), 
            uiOutput("grade_select_SA")
          ) ,
          column(width = 6,
                 
                 ), 
          )
        
        ), 
        column(width = 6,
        
        plotOutput("grade_plot_district_SA"), 
        plotOutput("grade_plot_building_SA")
        
        )
        
      ),
      #### MAP Tabs #####
      #### MAP COHORT #####
      tabItem(
        tabName = "cohort_projections_MAP",
        
        plotOutput("projected_growth_MAP")
        
      ),
      #### Map Grade ####
      tabItem(
        tabName = "grade_MAP", 
        plotOutput("growth_MAP"),
        box(
          width=12,
          uiOutput("start_year_select_MAP_growth"),
          uiOutput("timing_select_MAP_growth"),
          uiOutput("subject_select_MAP_growth")
          
        )
        
      )
    )
  ),
  title = "SIAC Dashboard"
  
)
