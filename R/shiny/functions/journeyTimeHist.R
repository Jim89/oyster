# set up some colours to use
DarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
Purple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
lightGrey <- rgb(red = 186, green = 187, blue = 188, maxColorValue = 255)

# create function for plotting the histogram
journeyTimeHist.f <- function (data, days = c("Weekend", "Weekday")) {
  data %>%
    filter(weekend %in% days) %>%
    ggplot(aes(x = journey.time %>% as.numeric)) +
    geom_histogram(binwidth = 5, aes(fill = weekend), alpha = 0.8,
                   colour = "white") +
    facet_grid(weekend ~ ., scales = "fixed") +
    scale_x_continuous(breaks = seq(from = 0,
                                    to = data$journey.time %>%
                                      as.numeric() %>%
                                      max(na.rm = T) + 5,
                                    by = 5)) +
    scale_fill_manual(values = rep(DarkBlue, 2), guide = F) +
    xlab("Journey time / minutes") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          #axis.text.y = element_blank(),
          text = element_text(size = 14),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          #         element_line(colour = "lightgrey",
          #                                         linetype = "dotted"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.margin.y = unit(0.1, units = "in"),
          panel.background = element_rect(fill = "white", colour = "lightgrey"))
}