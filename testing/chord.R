library(circlize)
mat <- table(oyster$from, oyster$to) %>%
       as.data.frame.matrix() %>%
       as.matrix()
colnames(mat) %<>% str_wrap(width = 40)
rownames(mat) %<>% str_wrap(width = 40)



circos.clear()
circos.par(gap.degree = 2)
chordDiagram(mat,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.1))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
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
                                         adj = c(0, 0.5))
                        } else {
                        circos.text(mean(xlim),
                                    ylim[1],
                                    sector.name,
                                    facing = "inside",
                                    niceFacing = TRUE,
                                    adj = c(0.5, 0))
                        }
                        }
                       , bg.border = NA)






