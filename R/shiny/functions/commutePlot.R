# set up some colours to use
DarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
Purple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
lightGrey <- rgb(red = 186, green = 187, blue = 188, maxColorValue = 255)

# create function for plotting commute
CommutePlot <- function (data, start, end, interval) {
  # take the data and perform some manipulations
  data %>%
    filter(weekend != "Weekend") %>%
    mutate(start.time.clean = start.time.clean %>% as.character %>%
             str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]") %>%
             strptime(format = "%T") %>%
             as.POSIXct %>%
             CeilingTime(interval, "minute")) %>%
    filter(start.time.clean %>% between(start, end)) %>%
    group_by(start.time.clean) %>%
    summarise(journeys = n(),
              journey.time = journey.time %>% as.numeric %>% mean) %>% # View
    mutate(start.time.clean = start.time.clean %>% as.character %>%
             str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) %>%
    # make the plot itself
    ggplot(aes(x = start.time.clean, y = journey.time, group = 1)) +
    #  geom_line(colour = DarkBlue) +
    geom_point(aes(size = journeys), colour = DarkBlue, alpha = 0.8) +
    scale_size(name = "Number of\nJourneys", range = c(0, 10)) +
    xlab("Departure Time") +
    ylab("Average Journey Time / minutes") +
    #  geom_smooth(method = "lm", alpha = 0.075) +
    #   geom_smooth(method = "loess", size = 0.5, colour = Purple, alpha = 0.25) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(size = 12),
          #axis.title.y = element_blank(),
          #axis.ticks.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          #           element_line(colour = "lightgrey",s
          #                                           linetype = "dotted"),
          panel.grid.major.y = element_line(colour = "lightgrey",
                                            linetype = "dotted"),
          panel.grid.minor.y = element_blank(),
          panel.margin.y = unit(0.1, units = "in"),
          panel.background = element_rect(fill = "white",colour = "lightgrey"),
          legend.background = element_rect(fill = "white"))
}
