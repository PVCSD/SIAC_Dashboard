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

subjectToPlot <- "Reading"
school_start_year <- "15-16"
previous_year <- paste0("20",substr(school_start_year, 1,2))
end_year <- paste0("20",substr(school_start_year, 4,5))
sub <- paste0( "Growth in <b style='color:#4B6C8C'>Fall ",end_year, "</b> from ",
               "<b style='color:#011638'>Fall ",previous_year," RIT </b>",
               "vs. <b style='color:#F39C12'>Projected Growth </b>")

year_comparision <- paste0(previous_year, " to ", end_year)

map_growth %>%
  filter(start_year==school_start_year) %>%
  filter(subject==subjectToPlot) %>%
  filter(end_year_grade <= 9) %>%
  ggplot()+
  geom_bar(aes(x=end_year_rit, y=as.factor(end_year_grade)), stat = "identity", fill="#4B6C8C")+
  geom_bar(aes(x=start_year_rit, y=as.factor(end_year_grade)), stat = "identity", fill="#011638")+
  geom_point(aes(x=start_year_rit + projected_growth, y=as.factor(end_year_grade)),  fill ="#F39C12",size = 5, pch=21,)+
  #scale_y_discrete(limits = rev(levels(as.factor(c(4,5,6,7,8,9)))))+
  theme_minimal()+
  labs(x="", y="", 
       title =  paste0(subjectToPlot, " Mean RIT Growth"),
       subtitle =sub)+
  coord_flip()+
  theme(
    plot.title = element_markdown(lineheight = 2), 
    plot.subtitle = element_markdown(lineheight = 1))+
  theme(plot.title.position = "plot")
