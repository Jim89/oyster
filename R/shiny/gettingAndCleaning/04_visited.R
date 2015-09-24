# set up data for the map widget -----------------------------------------------
# reshape the data
vistited <- combined %>%
  select(from, from.long, from.lat) %>%
  setNames(c("station", "longitude", "latitude")) %>%
  rbind(combined %>%
          select(to, to.long, to.lat) %>%
          setNames(c("station", "longitude", "latitude"))) %>%
  group_by(station, longitude, latitude) %>%
  summarise(visits = n()) %>%
  filter(!is.na((longitude)))

# create pop-up text
popup <- paste0("<strong> Station: </strong>",
                vistited$station,
                "<br><strong>Visits: </strong>",
                vistited$visits)