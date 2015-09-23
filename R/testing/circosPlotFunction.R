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
  colnames(mat) %<>% str_wrap(width = 15)
  rownames(mat) %<>% str_wrap(width = 15)


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
               annotationTrack = "",
               annotationTrackHeight = 0.05,
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
                                           adj = c(0, 0.5),
                                          )
                          } else {
                          circos.text(mean(xlim),
                                      ylim[1],
                                      sector.name,
                                      facing = "inside",
                                      niceFacing = TRUE,
                                      adj = c(0.5, 0),
                                      )
                            }
                          },
                         bg.border = NA,
                         bg.col = central)

}


createChordPlot(combined, 10)