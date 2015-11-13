# set up colours
bakerloo <- rgb(red = 137, green = 78, blue = 36, maxColorValue = 255)
central <- rgb(red = 220, green = 36, blue = 31, maxColorValue = 255)
circle <- rgb(red = 255, green = 206, blue = 0, maxColorValue = 255)
district <- rgb(red = 0, green = 114, blue = 41, maxColorValue = 255)
hc <- rgb(red = 215, green = 153, blue = 175, maxColorValue = 255)
jubilee <- rgb(red = 134, green = 143, blue = 152, maxColorValue = 255)
metropolitan <- rgb(red = 117, green = 16, blue = 86, maxColorValue = 255)
northern <- rgb(red = 0, green = 0, blue = 0, maxColorValue = 255)
picadilly <- rgb(red = 0, green = 25, blue = 168, maxColorValue = 255)
victoria <- rgb(red = 0, green = 160, blue = 226, maxColorValue = 255)
wc <- rgb(red = 118, green = 208, blue = 189, maxColorValue = 255)
dlr <- rgb(red = 0, green = 175, blue = 173, maxColorValue = 255)
overground <- rgb(red = 232, green = 106, blue = 16, maxColorValue = 255)

createChordPlot <- function(oyster_data, top_journeys){
  journeys <- oyster_data %>%
    group_by(from, to) %>%
    summarise(journeys = n()) %>%
    ungroup() %>%
    arrange(desc(journeys)) %>%
    mutate(rank = row_number(-journeys))
  
  dat <- oyster_data %>%
    inner_join(journeys %>% filter(rank <= top_journeys),
               by = c("from" = "from",
                      "to" = "to"))
  
  mat <- table(dat$from, dat$to) %>%
    as.data.frame.matrix() %>%
    as.matrix()
  colnames(mat) %<>% str_wrap(width = 10)
  rownames(mat) %<>% str_wrap(width = 10)
  
  
  grid.col = NULL # just create the variable
  grid.col[rownames(mat)] = "steelblue"
  grid.col[colnames(mat)] = "steelblue"
  
  
  # clear the plot
  circos.clear()
  
  
  # set the basic basic circos graphic parameters
  circos.par(cell.padding=c(0,0,0,0),
             track.margin=c(0,0),
             start.degree = 0,
             gap.degree = 2.5,
             points.overflow.warning=FALSE)
  
  # make the basic plot
  chordDiagram(mat,
               annotationTrack = NA,
               preAllocateTracks = list(track.height = 0.1),
               grid.col = picadilly,
               link.border = "lightgrey")
  
  # add the labels
  circos.trackPlotRegion(track.index = 1,
                         panel.fun = function(x, y) {
                           xlim = get.cell.meta.data("xlim")
                           xplot = get.cell.meta.data("xplot")
                           ylim = get.cell.meta.data("ylim")
                           sector.name = get.cell.meta.data("sector.index")
                           if ( abs(xplot[2] - xplot[1]) < 20) {
                             circos.text(mean(xlim),
                                         ylim[1],
                                         sector.name,
                                         facing = "clockwise",
                                         niceFacing = TRUE,
                                         adj = c(0, 0.5)
                             )
                           } else {
                             circos.text(mean(xlim),
                                         ylim[1],
                                         sector.name,
                                         facing = "inside",
                                         niceFacing = TRUE,
                                         adj = c(0.5, 0)
                             )
                           }
                         },
                         bg.border = central,
                         bg.col = central,
                         track.height = 5)
  
}