
da_net_rel <- read.csv("Dumbledores Army_Relational.csv", stringsAsFactors = FALSE)

da_m <- as.matrix(da_net_rel)
g_da <- graph_from_edgelist(da_m[,1:2], directed = FALSE)
vis_da <- toVisNetworkData(g_da)
da_nodes <- vis_da$nodes

da_self_loop_m <- da_nodes
da_self_loop_m$Actor.1 <- da_self_loop_m$id
da_self_loop_m$Actor.2 <- da_self_loop_m$id
da_self_loop_m$Strength.of.the.Relationship <- 10
da_self_loop_m <- da_self_loop_m[,c("Actor.1", "Actor.2", "Strength.of.the.Relationship")]
da_self_loop_m <- as.matrix(da_self_loop_m)

death_m <- as.matrix(deatheaters_rel)
g_death <- graph_from_edgelist(death_m[,1:2], directed = FALSE)
vis_death <- toVisNetworkData(g_death)
death_nodes <- vis_death$nodes

death_self_loop_m <- death_nodes
death_self_loop_m$Actor.1 <- death_self_loop_m$id
death_self_loop_m$Actor.2 <- death_self_loop_m$id
death_self_loop_m$Strength.of.the.Relationship <- 10
death_self_loop_m <- death_self_loop_m[,c("Actor.1", "Actor.2", "Strength.of.the.Relationship")]
death_self_loop_m <- as.matrix(death_self_loop_m)
death_m_comb <- rbind(death_m, death_self_loop_m)



test_list <- list("Spell1" = TRUE, "Spell2" = FALSE)
test_list$Spell1
test_list$Spell1
temp2 <- subset(test_list, test_list == TRUE)

is.element("Spell1", temp2)

getColor <- function(geo_df) {
  sapply(geo_df$marker_color, function(marker_color) {
    marker_color })
}
getIcon <- function(geo_df) {
  sapply(geo_df$icon_type, function(icon_type) {
    icon_type })
}

icons2 <- awesomeIcons(
  icon = "magic",
  iconColor = 'white',
  library = 'fa',
  markerColor = "grey"
)

leaflet() %>%
  setView(lng = 0, lat = 54, zoom = 5) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addAwesomeMarkers(data = combined_geo, lng = ~Long, lat = ~Lat,
                    icon = icons2,
                    popup = ~new_name,
                    clusterOptions = markerClusterOptions())

leafletProxy("da_geo", data = filtered_geo) %>%
  clearMarkers() %>%
  clearMarkerClusters() %>%
  # Popup is created from new_name, color is a column as well
  addAwesomeMarkers(data = geo_points(), lng = ~Long, lat = ~Lat,
                    icon = icons,
                    popup = ~new_name,
                    clusterOptions = markerClusterOptions())


