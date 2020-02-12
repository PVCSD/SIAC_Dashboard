library(tidyverse)
#devtools::install_github("clauswilke/ggtext")
library(ggtext)
library(Cairo)
library(DT)
options(shiny.usecairo=T)

proficencyGroups <- readr::read_csv("Data/state_assessment_agg.csv")



server <- function(input, output) {
  
  cohortsAvailabeSA <- reactive({
    print("getting cohorts")
    cohorts <- proficencyGroups %>%
      select(Cohort)%>%
      distinct() %>%
      arrange(Cohort)
    cohorts <- cohorts$Cohort
    
    return(cohorts)
  })
  
  filteredStateAssessments <- reactive({
    print("filtering data")
    proficencyGroups %>%
      filter(Cohort==cohortToPlot)-> filteredDF
    
    return(filteredDF)
  })
  
  
  data_for_math_SA <- reactive({
    
    cohortToPlot <- input$SAClassOf
    proficencyGroups %>%
      filter(Cohort==cohortToPlot) %>%
      filter(Subject=="Math") %>%
      group_by(Year, Level) %>%
      summarise(Percent = mean(Percent)*100) %>%
      pivot_wider(names_from = Year, values_from = Percent)->df
    
    return(df)
  })
  
  
  
  data_for_reading_SA <- reactive({
    
    cohortToPlot <- input$SAClassOf
    proficencyGroups %>%
      filter(Cohort==cohortToPlot) %>%
      filter(Subject=="Reading") %>%
      group_by(Year, Level) %>%
      summarise(Percent = mean(Percent)*100) %>%
      pivot_wider(names_from = Year, values_from = Percent)->df
    
    return(df)
  })
  
  output$reading_table_SA <- renderDataTable({
    
    datatable(data =  data_for_reading_SA(), 
              options = list(autoWidth = FALSE, dom = 't'), rownames=FALSE)
    
    
    
  })
  
  output$math_table_SA <- 
    output$reading_table_SA <- renderDataTable({
      
      datatable(data =  data_for_reading_SA(), 
                options = list(autoWidth = FALSE, dom = 't'), rownames=FALSE)
      
      
      
    })
  
  
  
  ## UI functions
  output$cohort_select_SA <- renderUI({
    list <-  cohortsAvailabeSA()
    selectInput("SAClassOf", "Cohort", choices = list)
  })
  
  
  ## Plots
  
  # STATE ASSESSMENT ELA/Reading Scores
  output$assessmentCohortsELA <- renderPlot({
    print("ELA plot")
    cohortToPlot <- input$SAClassOf
    print("ELA CLASS OF")
    subjectToPlot <- "Reading"
    print("ELA SUBJECT")

    
    proficencyGroups %>%
      filter(Cohort==cohortToPlot) %>%
      filter(Subject==subjectToPlot) %>%
      group_by(Year, Level) %>%
      summarise(Percent = mean(Percent)*100, TestName = TestName[1])-> filteredDF
    print("filtering")
    
    filteredDF %>% 
      select(Year) %>%
      distinct() ->numYears
    print("number of years")
    
    if(length(numYears$Year)<=1){
      print("bar chart")
      filteredDF %>%
      ggplot(aes(x=Year, y=Percent, fill=Level, group= Level, color=TestName, label=paste0(Percent, "%")))+
        geom_bar(stat='identity', alpha = .8, col="#000000", width = .5)+
        # geom_text(color="#000000", position = position_stack(vjust = .5))+
        theme_minimal()+
        scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39"))+
        scale_color_manual(values = c( "#FFFFFF", "#000000"))+
        theme(legend.position = "none")+
        labs(x="", y="", 
             title =  paste0(cohortToPlot ," ", subjectToPlot, " Proficiencies By:"),
             subtitle ="<b style='color:#3D9970'>Advanced</b>, 
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>")+
        theme(
          plot.title = element_markdown(lineheight = 3.5, size = 25),
          plot.subtitle = element_markdown(lineheight = 3,size = 20),
          panel.background = element_blank(), 
          panel.grid.major = element_line(color="#A6A6A6"))+
        theme(plot.title.position = "plot")
    }  
    else{
      print("area plot")

    filteredDF %>%
      ggplot(aes(x=Year, y=Percent, fill=Level, group= Level,  label=paste0(Percent, "%")))+
      geom_area(alpha = .8, col="#000000")+
      theme_minimal()+
      scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39"))+
      scale_color_manual(values = c( "#FFFFFF", "#000000"))+
      theme(legend.position = "none")+
      labs(x="", y="",
           title =  paste0(cohortToPlot ," ", subjectToPlot, " Proficiencies By:"),
           subtitle ="<b style='color:#3D9970'>Advanced</b>,
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>")+
      theme(
        plot.title = element_markdown(lineheight = 3.5, size = 25),
        plot.subtitle = element_markdown(lineheight = 3,size = 20),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color="#A6A6A6"))+
      theme(plot.title.position = "plot")
    # 
    }
    
  }, bg="transparent", execOnResize = TRUE)
  
  # STATE ASSESSMENT Math Scores
  output$assessmentCohortsMath <- renderPlot({
    cohortToPlot <- input$SAClassOf
    subjectToPlot <- "Math"
    
    proficencyGroups %>%
      filter(Cohort==cohortToPlot) %>%
      filter(Subject==subjectToPlot) %>%
      group_by(Year, Level) %>%
      summarise(Percent = mean(Percent)*100, TestName = TestName[1])-> filteredDF
    
    filteredDF %>% 
      select(Year) %>%
      distinct() ->numYears
    
    if(length(numYears$Year)<=1){
      filteredDF %>%
        ggplot(aes(x=Year, y=Percent, fill=Level, group= Level, color=TestName, label=paste0(Percent, "%")))+
        geom_bar(stat='identity', alpha = .8, col="#000000", width = .5)+
        # geom_text(color="#000000", position = position_stack(vjust = .5))+
        theme_minimal()+
        scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39"))+
        scale_color_manual(values = c( "#FFFFFF", "#000000"))+
        theme(legend.position = "none")+
        labs(x="", y="", 
             title =  paste0(cohortToPlot ," ", subjectToPlot, " Proficiencies By:"),
             subtitle ="<b style='color:#3D9970'>Advanced</b>, 
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>")+
        theme(
          plot.title = element_markdown(lineheight = 3.5, size = 25),
          plot.subtitle = element_markdown(lineheight = 3,size = 20),
          panel.background = element_blank(), 
          panel.grid.major = element_line(color="#A6A6A6"))+
        theme(plot.title.position = "plot")
    }  
    else{
      
      filteredDF %>%
        ggplot(aes(x=Year, y=Percent, fill=Level, group= Level,  label=paste0(Percent, "%")))+
        geom_area(alpha = .8, col="#000000")+
        theme_minimal()+
        scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39"))+
        scale_color_manual(values = c( "#FFFFFF", "#000000"))+
        theme(legend.position = "none")+
        labs(x="", y="",
             title =  paste0(cohortToPlot ," ", subjectToPlot, " Proficiencies By:"),
             subtitle ="<b style='color:#3D9970'>Advanced</b>,
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>")+
        theme(
          plot.title = element_markdown(lineheight = 3.5, size = 25),
          plot.subtitle = element_markdown(lineheight = 3,size = 20),
          panel.background = element_blank(), 
          panel.grid.major = element_line(color="#A6A6A6"))+
        theme(plot.title.position = "plot")
      # 
    }
    
  }, bg="transparent", execOnResize = TRUE)
  
  
  # ## Value Boxes
  # 
  # output$vb_reading_high<- renderUI({
  #   
  #   print("valueboxfilter")
  #   
  #   filteredStateAssessments() %>%
  #     filter(Level=="High", 
  #            Subject=="Reading") %>%
  #     summarise(Percent = mean(Percent))->data
  #   
  #   print(data)
  #   paste0(round(data$Percent[1]*100, 2), '%')->test
  #   print(test)
  #   
  #   valueBox(width= 4,value=test, subtitle= "High", color = "olive")
  #   
  #   
  # })
}