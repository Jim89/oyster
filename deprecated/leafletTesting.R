
visited <- 
combined %>% 
  select(from, from.long, from.lat)  %>% 
  setNames(c("station", "long", "lat")) %>%
  rbind(combined %>% 
          select(to, to.long, to.lat)  %>% 
          setNames(c("station", "long", "lat"))) %>%
  group_by(station, long, lat) %>%
  summarise(visits = n()) 

visited %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -0.1275, lat = 51.507222, zoom = 11) %>%
  addCircles(radius = ~ visits)

map <- visited %>%
  leaflet() %>%
  addTiles(
    'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
    attribution = 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community') %>%
  setView(lng = -0.1275, lat = 51.507222, zoom = 11) %>%
  addCircles(radius = ~ visits)

map





// https: also suppported.
var HERE_normalDayTransit = L.tileLayer('http://{s}.{base}.maps.cit.api.here.com/maptile/2.1/maptile/{mapID}/normal.day.transit/{z}/{x}/{y}/256/png8?app_id={app_id}&app_code={app_code}', {
  attribution: 'Map &copy; 1987-2014 <a href="http://developer.here.com">HERE</a>',
  subdomains: '1234',
  mapID: 'newest',
  app_id: 'Y8m9dK2brESDPGJPdrvs',
  app_code: 'dq2MYIvjAotR8tHvY8Q_Dg',
  base: 'base',
  minZoom: 0,
  maxZoom: 20
});




leaflet() %>%
  addTiles(
    'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
    attribution = 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community') %>%
  setView(-93.65, 42.0285, zoom = 17)