############### Combine and perform further cleaning on the data ###############

combined <- oyster %>%
            left_join(stations, by = c("from" = "station")) %>%
            rename(from.long = long,
                   from.lat = lat) %>%
            left_join(stations, by = c("to" = "station")) %>%
            rename(to.long = long,
                   to.lat = lat)

# clean up
  rm(list = c("oyster", "stations"))