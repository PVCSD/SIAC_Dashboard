library(tidyverse)
library(janitor)
library(ggtext)


map_growth <- read_csv("C:/Users/delabruerejosh/Downloads/Mean Map by Grade - Growth Data.csv")
map_growth <- clean_names(map_growth)

trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

p1 <- map_growth %>%
  filter(subject == "Reading") %>%
  filter( end_year_grade > 3, end_year_grade <= 9) %>%
  filter(cohort %in% c("CO2020", "CO2021","CO2022", "CO2023", "CO2024","CO2025")) %>%
  ggplot(aes(x=end_year_grade))+
  geom_line(aes(y=projected_growth), col="#B8860B", linetype = "dashed")+
  geom_point(aes(y=projected_growth), col="#B8860B")+
  geom_point(aes(y=observed_growth), col="#013220" , alpha=.8)+
    geom_line(aes(y=observed_growth), col="#013220")+
  ylim(0,15)+
  facet_wrap(~cohort,nrow = 3)+
  theme_minimal()+
  labs(title="MAP Reading Growth Trends By Cohort ",
       subtitle = "<b style = 'color:#B8860B'> Projected Growth</b> 
        & <b style='color:#013220'>Actual Growth</b> Per Grade",
       x="", y="")+
  theme(plot.background = element_rect(fill="#f9f9f9", 
                                       color="#f9f9f9"))+
  theme(
    plot.title = element_markdown(lineheight = 2), 
    plot.subtitle = element_markdown(lineheight = 1))+
  theme(panel.border = element_rect(color = "#c6c6c6", fill = NA, size = .1), 
        panel.grid.major = element_line(colour = "#D6D6D6"))+
  theme(plot.title.position = "plot")+
  theme(panel.spacing = unit(2, "lines"))


ggsave("cohortsReading.png", p1, type="cairo", height = 10, width = 7.5)  


p2 <- map_growth %>%
  filter(subject == "Math") %>%
  filter( end_year_grade > 3, end_year_grade <= 9) %>%
  filter(cohort %in% c("CO2020", "CO2021","CO2022", "CO2023", "CO2024","CO2025")) %>%
  ggplot(aes(x=end_year_grade))+
  geom_line(aes(y=projected_growth), col="#B8860B", linetype = "dashed")+
  geom_point(aes(y=projected_growth), col="#B8860B")+
  geom_point(aes(y=observed_growth), col="#013220" , alpha=.8)+
  geom_line(aes(y=observed_growth), col="#013220")+
  ylim(0,15)+
  facet_wrap(~cohort,nrow = 3)+
  theme_minimal()+
  labs(title="MAP Math Growth Trends By Cohort ",
       subtitle = "<b style = 'color:#B8860B'> Projected Growth</b> 
        & <b style='color:#013220'>Actual Growth</b> Per Grade",
       x="", y="")+
  theme(plot.background = element_rect(fill="#f9f9f9", 
                                       color="#f9f9f9"))+
  theme(
    plot.title = element_markdown(lineheight = 2), 
    plot.subtitle = element_markdown(lineheight = 1))+
  theme(panel.border = element_rect(color = "#c6c6c6", fill = NA, size = .1), 
        panel.grid.major = element_line(colour = "#D6D6D6"))+
  theme(plot.title.position = "plot")+
  theme(panel.spacing = unit(2, "lines"))


ggsave("cohortsMath.png", p2, type="cairo", height = 10, width = 7.5)  




test <- map_growth %>%
  filter(subject =="Reading") %>%
  filter( end_year_grade == 4 ) %>%
  filter(cohort %in% c("CO2021"))
