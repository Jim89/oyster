library(rvest)
library(dplyr)
library(magrittr)
library(tidyr)
library(igraph)
library(networkD3)
library(ggplot2)

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

# Colour df
# Set up colours
colour_df <- data_frame(line = c("Bakerloo", "Central", "Circle", "District",
                                 "Hammersmit & City", "Jubilee", "Metropolitan",
                                 "Northern", "Piccadilly", "Victoria",
                                 "Waterloo & City", "DLR", "Overground"),
                        colour = c(bakerloo, central, circle, district,
                                   hc, jubilee, metropolitan, northern, picadilly,
                                   victoria, wc, dlr, overground))

url <- "https://en.wikipedia.org/wiki/List_of_London_Underground_stations#cite_note-usage-7"

stations <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table() %>% 
    tbl_df()

names(stations) %<>% tolower()

station_to_lines <- stations %>% 
    select(1, 3) %>% 
    setNames(c("station", "lines")) %>% 
    separate(lines, into = paste("line", 1:6), sep = "\n") %>% 
    gather(temp, line, -station) %>% 
    select(-temp) %>% 
    mutate(line = gsub("\\[[aA-zZ]\\]", "", line)) %>% 
    na.omit()

edgelist <- station_to_lines %>% 
            left_join(station_to_lines, by = c("line" = "line")) %>% 
            rename(station1 = station.x,
                   station2 = station.y) %>% 
            filter(station1 != station2) %>% 
            select(-line)

graph <- graph_from_data_frame(edgelist, directed = FALSE)

# Centrality
centralities <- eigen_centrality(graph, directed = FALSE)
V(graph)$eig <- centralities$vector

# Betweenness
betweens <- betweenness(graph, directed = FALSE)
V(graph)$bet <- betweens

# Closeness
closes <- closeness(graph)
V(graph)$close <- closes


networks <- igraph_to_networkD3(graph, group = rep(1, length(V(graph))))
networks$links$value <- 1

networks$nodes <- networks$nodes %>% 
    left_join(station_to_lines %>% 
                  group_by(station) %>% 
                  mutate(rn = row_number(line)) %>% 
                  filter(rn == 1), by = c("name" = "station")) %>% 
    select(name, line) %>% 
    rename(group = line)

# control colours with a JS ordinal scale
ColourScale <- 'd3.scale.ordinal().domain(["Bakerloo", "Central", "Circle", "District", "Hammersmith & City", "Jubilee", "Metropolitan", "Northern", "Piccadilly", "Victoria", "Waterloo & City", "DLR", "Overground"]).range(["#894E24", "#DC241F", #FFCE00, "#007229", "#D799AF", "#868F98", "#751056", "#000000", "#0019A8", "#00A0E2", "#76D0BD", "#00AFAD", "#E86A10"]);'

# Plot the network
# forceNetwork(Links = networks$links,
#              Nodes = networks$nodes,
#              Source = "source",
#              Target = "target",
#              Value = "value",
#              NodeID = "name",
#              Group = "group",
#              charge = -10,
#              linkColour = "grey",
#              colourScale = JS(ColourScale),
#              fontSize = 16,
#              opacity = 1,
#              legend = F,
#              bounded = F,
#              zoom = TRUE)

# Convert graph to data frame
station_stats <- data_frame(station = names(V(graph)),
                                          eig = V(graph)$eig,
                                          bet = V(graph)$bet,
                                          close = V(graph)$close) %>% 
                    left_join(station_to_lines)

# Set up theme object for prettier plots
theme_jim <-  theme(legend.position = "bottom",
                    axis.text.y = element_text(size = 16, colour = "black"),
                    axis.text.x = element_text(size = 16, colour = "black"),
                    legend.text = element_text(size = 16),
                    legend.title = element_text(size = 16),
                    title = element_text(size = 16),
                    strip.text = element_text(size = 16, colour = "black"),
                    strip.background = element_rect(fill = "white"),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
                    panel.grid.minor.y = element_line(colour = "lightgrey", linetype = "dotted"),
                    panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
                    panel.margin.y = unit(0.1, units = "in"),
                    panel.background = element_rect(fill = "white", colour = "lightgrey"),
                    panel.border = element_rect(colour = "black", fill = NA))

imp_plot <- function(measure) {
station_stats %>%
    ggplot(aes_string(x = "line", y = measure, fill = "line")) +
    geom_boxplot(outlier.colour = NULL, colour = "black") +
    scale_fill_manual(values = c(bakerloo, central, circle, district, hc, jubilee,
                                 metropolitan, northern, picadilly, victoria,
                                 wc)) +
    scale_colour_manual(values = c(bakerloo, central, circle, district, hc, jubilee,
                                   metropolitan, northern, picadilly, victoria,
                                   wc)) +
    theme_jim +
    theme(legend.position = "none")
}

imp_plot("eig") + coord_flip()
imp_plot("bet") + coord_flip()
imp_plot("close") + coord_flip()

# Get wiki csvs -----------------------------------------------------------
stations_geo <- readr::read_csv("./data/geo/stations_geo.csv")
line_lk <- readr::read_csv("./data/geo/station_lk.csv")
adjacency <- readr::read_csv("./data/geo/adjacency.csv")

edgelist_adj <- adjacency %>% 
                left_join(stations_geo, by = c("station1" = "id")) %>% 
                select(name, station2, line) %>% 
                rename(station1 = name) %>% 
                left_join(stations_geo, by = c("station2" = "id")) %>% 
                select(station1, name, line) %>% 
                rename(station2 = name)


graph_adj <- graph_from_data_frame(edgelist_adj %>% select(1, 2), directed = F)


# Centrality
centralities <- eigen_centrality(graph_adj, directed = FALSE)
V(graph_adj)$eig <- centralities$vector

# Betweenness
betweens <- betweenness(graph_adj, directed = FALSE)
V(graph_adj)$bet <- betweens

# Closeness
closes <- closeness(graph_adj)
V(graph_adj)$close <- closes


networks <- igraph_to_networkD3(graph_adj, group = rep(1, length(V(graph_adj))))
networks$links$value <- 1

networks$nodes <- networks$nodes %>% 
  left_join(station_to_lines %>% 
              group_by(station) %>% 
              mutate(rn = row_number(line)) %>% 
              filter(rn == 1), by = c("name" = "station")) %>% 
  select(name, line) %>% 
  rename(group = line)

# control colours with a JS ordinal scale
ColourScale <- 'd3.scale.ordinal()
                .domain(["Bakerloo", "Central", "Circle", "District", "Hammersmith & City", "Jubilee", "Metropolitan", "Northern", "Piccadilly", "Victoria", "Waterloo & City", "DLR", "Overground"])
                .range(["#894E24", "#DC241F", #FFCE00, "#007229", "#D799AF", "#868F98", "#751056", "#000000", "#0019A8", "#00A0E2", "#76D0BD", "#00AFAD", "#E86A10"]);'

# Plot the network
forceNetwork(Links = networks$links,
             Nodes = networks$nodes,
             Source = "source",
             Target = "target",
             Value = "value",
             NodeID = "name",
             Group = "group",
             charge = -100,
             linkColour = "grey",
             colourScale = 'd3.scale.ordinal().range(["#894E24", "#DC241F", #FFCE00, "#007229", "#D799AF", "#868F98", "#751056", "#000000", "#0019A8", "#00A0E2", "#76D0BD", "#00AFAD", "#E86A10"]);',
             fontSize = 16,
             opacity = 1,
             legend = F,
             bounded = F,
             zoom = TRUE)


# Convert graph to data frame
station_stats <- data_frame(station = names(V(graph_adj)),
                            eig = V(graph_adj)$eig,
                            bet = V(graph_adj)$bet,
                            close = V(graph_adj)$close) %>% 
  left_join(station_to_lines)

# Set up theme object for prettier plots
theme_jim <-  theme(legend.position = "bottom",
                    axis.text.y = element_text(size = 16, colour = "black"),
                    axis.text.x = element_text(size = 16, colour = "black"),
                    legend.text = element_text(size = 16),
                    legend.title = element_text(size = 16),
                    title = element_text(size = 16),
                    strip.text = element_text(size = 16, colour = "black"),
                    strip.background = element_rect(fill = "white"),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
                    panel.grid.minor.y = element_line(colour = "lightgrey", linetype = "dotted"),
                    panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
                    panel.margin.y = unit(0.1, units = "in"),
                    panel.background = element_rect(fill = "white", colour = "lightgrey"),
                    panel.border = element_rect(colour = "black", fill = NA))

imp_plot <- function(measure) {
  station_stats %>%
    ggplot(aes_string(x = "line", y = measure, fill = "line")) +
    geom_boxplot(outlier.colour = NULL, colour = "black") +
    scale_fill_manual(values = c(bakerloo, central, circle, district, hc, jubilee,
                                 metropolitan, northern, picadilly, victoria,
                                 wc)) +
    scale_colour_manual(values = c(bakerloo, central, circle, district, hc, jubilee,
                                   metropolitan, northern, picadilly, victoria,
                                   wc)) +
    theme_jim +
    theme(legend.position = "none")
}

imp_plot("eig") + coord_flip()
imp_plot("bet") + coord_flip()
imp_plot("close") + coord_flip()


station_stats %<>% 
  group_by(station) %>% 
  mutate(lines = n()) %>% 
  ungroup() %>% 
  mutate(eig = eig/lines,
         bet = bet/lines,
         close = close/lines)


temp <- adjacency %>% 
  left_join(stations_geo, by = c("station1" = "id")) %>% 
  select(station1, station2, line, latitude, longitude) %>% 
  rename(lat1 = latitude,
         long1 = longitude) %>% 
  left_join(stations_geo, by = c("station2" = "id")) %>% 
  select(station1, station2, line, lat1, long1, latitude, longitude) %>% 
  rename(lat2 = latitude,
         long2 = longitude) 
  
ggplot(temp) +
  geom_point(aes(x = lat1, y = long1)) +
  geom_point(aes(x = lat2, y = long2))

stations_geo %>% 
  ggplot(aes(y = latitude, x = longitude)) +
  geom_point()
