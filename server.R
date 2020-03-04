# Load Libraries
library(tidyverse)
# devtools::install_github("clauswilke/ggtext")
library(ggtext)
library(Cairo)
library(DT)
library(patchwork)
library(janitor)
library(shinydashboard)
#options(shiny.usecairo = T)



## read in data. Update this data to update the plots. 
proficencyGroups <- readr::read_csv("Data/state_assessment_agg.csv")
proficencyGrades <- readr::read_csv("Data/state_assessment_buildings.csv")
map_growth <- read_csv("Data/map_gowth_by_grade.csv")
map_growth <- clean_names(map_growth)





server <- function(input, output) {


  ## Create lists dynamically for UI
  cohortsAvailabeSA <- reactive({
    print("getting cohorts")
    cohorts <- proficencyGroups %>%
      select(Cohort) %>%
      distinct() %>%
      arrange(Cohort)
    cohorts <- cohorts$Cohort

    return(cohorts)
  })

  grades_available_SA <- reactive({
    print("getting grades")
    grades <- proficencyGroups %>%
      select(Grade) %>%
      distinct() %>%
      arrange(Grade)
    grade <- grades$Grade

    return(grade)
  })


  # get the years available for growth
  years_available_MAP_growth <- reactive({
    years <- map_growth %>%
      select(start_year) %>%
      distinct() %>%
      arrange(start_year)

    years <- years$start_year
    yearNames <- paste0("20", substr(years, 1, 2))
    print(yearNames)
    names(years) <- yearNames


    return(years)
  })

  test_timing_map_growth <- reactive({
    timing <- map_growth %>%
      select(testing_time_frame) %>%
      distinct() %>%
      arrange()

    timing <- timing$testing_time_frame
    return(timing)
  })
  
  test_subject_map_growth <- reactive({
    sub <- map_growth %>%
      select(subject) %>%
      distinct() %>%
      arrange()
    
    sub <- sub$subject
    return(sub)
  })


  #state assessment math data
  data_for_math_SA <- reactive({
    req(input$SAClassOf)

    cohortToPlot <- input$SAClassOf
    proficencyGroups %>%
      filter(Cohort == cohortToPlot) %>%
      filter(Subject == "Math") %>%
      group_by(Year, Level) %>%
      summarise(Percent = mean(Percent) * 100) %>%
      pivot_wider(names_from = Year, values_from = Percent) -> df

    return(df)
  })



  data_for_reading_SA <- reactive({
    req(input$SAClassOf)


    cohortToPlot <- input$SAClassOf
    proficencyGroups %>%
      filter(Cohort == cohortToPlot) %>%
      filter(Subject == "Reading") %>%
      group_by(Year, Level) %>%
      summarise(Percent = mean(Percent) * 100) %>%
      pivot_wider(names_from = Year, values_from = Percent) -> df

    return(df)
  })

  output$reading_table_SA <- renderDataTable({
    req(input$SAClassOf)


    datatable(
      data = data_for_reading_SA(),
      options = list(autoWidth = TRUE, dom = "t"), rownames = FALSE
    )
  })

  output$math_table_SA <-
    output$reading_table_SA <- renderDataTable({
      req(input$SAClassOf)


      datatable(
        data = data_for_reading_SA(),
        options = list(autoWidth = FALSE, dom = "t"), rownames = FALSE
      )
    })



  #### UI functions ####
  
  ##### STATE ASSEMENT CONTROLS #####

  # State Assessment cohort selection
  output$cohort_select_SA <- renderUI({
    list <- cohortsAvailabeSA()
    selectInput("SAClassOf", "Cohort", choices = list)
  })

  # State Assessment grade selection
  output$grade_select_SA <- renderUI({
    list <- grades_available_SA()
    selectInput("grade_SA", "Grade", choices = list)
  })

  ##### MAP GROWTH CONTROLS #####
  
  # MAP Starting Year Selection
  output$start_year_select_MAP_growth <- renderUI({
    list <- years_available_MAP_growth()
    selectInput("start_year_MAP", "Start Year", choices = list)
  })

  # MAP Test Timing 
  output$timing_select_MAP_growth <- renderUI({
    list <- test_timing_map_growth()
    selectInput("timing_of_test_map", "Test Timing", choices = list)
  })
  
  # MAP Test subject
  output$subject_select_MAP_growth <- renderUI({
    list <- test_subject_map_growth()
    selectInput("subject_of_test_map", "Test subject", choices = list)
  })

  ##### / #####

  #### Plots ####

  # STATE ASSESSMENT ELA/Reading Scores
  output$assessmentCohortsELA <- renderPlot(
    {
      req(input$SAClassOf)



      print("ELA plot")
      cohortToPlot <- input$SAClassOf
      print("ELA CLASS OF")
      subjectToPlot <- "Reading"
      print("ELA SUBJECT")


      proficencyGroups %>%
        filter(Cohort == cohortToPlot) %>%
        filter(Subject == subjectToPlot) %>%
        group_by(Year, Level) %>%
        summarise(Percent = mean(Percent) * 100, TestName = TestName[1]) -> filteredDF
      print("filtering")

      filteredDF %>%
        select(Year) %>%
        distinct() -> numYears
      print("number of years")

      if (length(numYears$Year) <= 1) {
        print("bar chart")
        filteredDF %>%
          ggplot(aes(x = Year, y = Percent, fill = Level, group = Level, color = TestName, label = paste0(Percent, "%"))) +
          geom_bar(stat = "identity", alpha = .8, col = "#000000", width = .5) +
          # geom_text(color="#000000", position = position_stack(vjust = .5))+
          theme_minimal() +
          scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39")) +
          scale_color_manual(values = c("#FFFFFF", "#000000")) +
          theme(legend.position = "none") +
          labs(
            x = "", y = "",
            title = paste0(cohortToPlot, " ", subjectToPlot, " Proficiencies By:"),
            subtitle = "<b style='color:#3D9970'>Advanced</b>, 
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>"
          ) +
          theme(
            plot.title = element_markdown(lineheight = 3.5, size = 25),
            plot.subtitle = element_markdown(lineheight = 3, size = 20),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = "#A6A6A6")
          ) +
          theme(plot.title.position = "plot")
      }
      else {
        print("area plot")

        filteredDF %>%
          ggplot(aes(x = Year, y = Percent, fill = Level, group = Level, label = paste0(Percent, "%"))) +
          geom_area(alpha = .8, col = "#000000") +
          theme_minimal() +
          scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39")) +
          scale_color_manual(values = c("#FFFFFF", "#000000")) +
          theme(legend.position = "none") +
          labs(
            x = "", y = "",
            title = paste0(cohortToPlot, " ", subjectToPlot, " Proficiencies By:"),
            subtitle = "<b style='color:#3D9970'>Advanced</b>,
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>"
          ) +
          theme(
            plot.title = element_markdown(lineheight = 3.5, size = 25),
            plot.subtitle = element_markdown(lineheight = 3, size = 20),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = "#A6A6A6")
          ) +
          theme(plot.title.position = "plot")
        #
      }
    },
    bg = "transparent",
    execOnResize = TRUE
  )

  # STATE ASSESSMENT Math Scores
  output$assessmentCohortsMath <- renderPlot({
      req(input$SAClassOf)

      cohortToPlot <- input$SAClassOf
      subjectToPlot <- "Math"

      proficencyGroups %>%
        filter(Cohort == cohortToPlot) %>%
        filter(Subject == subjectToPlot) %>%
        group_by(Year, Level) %>%
        summarise(Percent = mean(Percent) * 100, TestName = TestName[1]) -> filteredDF

      filteredDF %>%
        select(Year) %>%
        distinct() -> numYears

      if (length(numYears$Year) <= 1) {
        filteredDF %>%
          ggplot(aes(x = Year, y = Percent, fill = Level, group = Level, color = TestName, label = paste0(Percent, "%"))) +
          geom_bar(stat = "identity", alpha = .8, col = "#000000", width = .5) +
          # geom_text(color="#000000", position = position_stack(vjust = .5))+
          theme_minimal() +
          scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39")) +
          scale_color_manual(values = c("#FFFFFF", "#000000")) +
          theme(legend.position = "none") +
          labs(
            x = "", y = "",
            title = paste0(cohortToPlot, " ", subjectToPlot, " Proficiencies By:"),
            subtitle = "<b style='color:#3D9970'>Advanced</b>, 
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>"
          ) +
          theme(
            plot.title = element_markdown(lineheight = 3.5, size = 25),
            plot.subtitle = element_markdown(lineheight = 3, size = 20),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = "#A6A6A6")
          ) +
          theme(plot.title.position = "plot")
      }
      else {
        filteredDF %>%
          ggplot(aes(x = Year, y = Percent, fill = Level, group = Level, label = paste0(Percent, "%"))) +
          geom_area(alpha = .8, col = "#000000") +
          theme_minimal() +
          scale_fill_manual(values = c("#3D9970", "#F39C12", "#DD4B39")) +
          scale_color_manual(values = c("#FFFFFF", "#000000")) +
          theme(legend.position = "none") +
          labs(
            x = "", y = "",
            title = paste0(cohortToPlot, " ", subjectToPlot, " Proficiencies By:"),
            subtitle = "<b style='color:#3D9970'>Advanced</b>,
         <b style='color:#F39C12'>Proficient</b>, and
       <b style='color:#DD4B39'>Not Proficient</b>"
          ) +
          theme(
            plot.title = element_markdown(lineheight = 3.5, size = 25),
            plot.subtitle = element_markdown(lineheight = 3, size = 20),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = "#A6A6A6")
          ) +
          theme(plot.title.position = "plot")
        #
      }
    },
    bg = "transparent",
    execOnResize = TRUE
  )


  # STATE ASSESSMENT PLOT DISTRICT
  output$grade_plot_district_SA <- renderPlot({
    req(input$grade_SA)
    req(input$subject_select_SA)

    p1 <- proficencyGrades %>%
      filter(
        Grade == input$grade_SA,
        Subject == input$subject_select_SA
      ) %>%
      filter(Year != "13-14") %>%
      group_by(Year) %>%
      mutate(Score = str_remove(Score, pattern = "%")) %>%
      summarise(
        Score2 = mean(as.numeric(Score), na.rm = T),
        testName = `Test Name`[1]
      ) %>%
      ggplot(aes(x = Year, y = Score2, fill = testName, color = testName)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(Score2, 1), "%")),
        color = "#D6D6D6",
        position = position_dodge(width = 1),
        vjust = 1.2
      ) +
      ylim(0, 100) +
      labs(
        x = "", y = "",
        title = "District",
        fill = "Test", color = "Test"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("#26547C", "#011638")) +
      scale_color_manual(values = c("#000000", "#F6F6F6")) +
      theme(legend.position = "none") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.background = element_rect(
        fill = "#f9f9f9",
        color = "#f9f9f9"
      ))


    p2 <- proficencyGrades %>%
      filter(
        Grade == input$grade_SA,
        Subject == input$subject_select_SA
      ) %>%
      filter(Year != "13-14") %>%
      group_by(Year, School) %>%
      mutate(Score = str_remove(Score, pattern = "%")) %>%
      summarise(
        Score2 = mean(as.numeric(Score), na.rm = T),
        testName = `Test Name`[1]
      ) %>%
      ggplot(aes(x = Year, y = Score2, fill = testName, color = testName)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(Score2, 1), "%")),
        color = "#D6D6D6",
        position = position_dodge(width = 1),
        vjust = 1.2
      ) +
      ylim(0, 100) +
      labs(
        x = "", y = "",
        title = "",
        fill = "Test", color = "Test"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("#26547C", "#011638")) +
      scale_color_manual(values = c("#000000", "#F6F6F6")) +
      theme(legend.position = "none") +
      facet_wrap(vars(School), nrow = 3) +
      theme(plot.background = element_rect(
        fill = "#f9f9f9",
        color = "#f9f9f9"
      ))


    p1 / p2 + plot_layout(ncol = 1) -> p3

    return(p1)
  })


  output$grade_plot_building_SA <- renderPlot({
    req(input$grade_SA)
    req(input$subject_select_SA)

    p2 <- proficencyGrades %>%
      filter(
        Grade == input$grade_SA,
        Subject == input$subject_select_SA
      ) %>%
      filter(Year != "13-14") %>%
      group_by(Year, School) %>%
      mutate(Score = str_remove(Score, pattern = "%")) %>%
      summarise(
        Score2 = mean(as.numeric(Score), na.rm = T),
        testName = `Test Name`[1]
      ) %>%
      ggplot(aes(x = Year, y = Score2, fill = testName, color = testName)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(Score2, 1), "%")),
        color = "#D6D6D6",
        position = position_dodge(width = 1),
        vjust = 1.2
      ) +
      ylim(0, 100) +
      labs(
        x = "", y = "",
        title = "",
        fill = "Test", color = "Test"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("#26547C", "#011638")) +
      scale_color_manual(values = c("#000000", "#F6F6F6")) +
      theme(legend.position = "none") +
      facet_wrap(vars(School), nrow = 3)

    return(p2)
  })

  #growth plot from fall to fall by chohorts
  output$growth_MAP <- renderPlot({
    subjectToPlot <- input$subject_of_test_map
    previous_year <- paste0("20", substr(input$start_year_MAP, 1, 2))
    end_year <- paste0("20", substr(input$start_year_MAP, 4, 5))
    sub <- paste0(
      "Growth by <b>Grade</b> in <b style='color:#4B6C8C'>Fall ", end_year, "</b> from ",
      "<b style='color:#011638'>Fall ", previous_year, " RIT </b>",
      "vs. <b style='color:#F39C12'>Projected Growth </b>"
    )

    year_comparision <- paste0(previous_year, " to ", end_year)

    map_growth %>%
      filter(testing_time_frame == input$timing_of_test_map) %>%
      filter(start_year == input$start_year_MAP) %>%
      filter(subject == subjectToPlot) %>%
      filter(end_year_grade <= 9) %>%
      distinct() %>%
      ggplot() +
      geom_bar(aes(x = end_year_rit, y = as.factor(end_year_grade)), stat = "identity", fill = "#4B6C8C" ) +
      geom_bar(aes(x = start_year_rit, y = as.factor(end_year_grade)), stat = "identity", fill = "#011638", width = .75) +
      geom_point(aes(x = start_year_rit + projected_growth, y = as.factor(end_year_grade)), fill = "#F39C12", size = 5, pch = 21, ) +
      scale_y_discrete(limits = rev(levels(as.factor(c(4, 5, 6, 7, 8, 9))))) +
      theme_minimal() +
      labs(
        x = "", y = "",
        title = paste0(subjectToPlot, " Mean RIT Growth"),
        subtitle = sub
      ) +
      # coord_flip()+
      theme(
        plot.title = element_markdown(lineheight = 2),
        plot.subtitle = element_markdown(lineheight = 1)
      ) +
      theme(plot.title.position = "plot")
  })
  
  #### / ####
}
