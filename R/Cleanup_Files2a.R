### Discover NPS Day NPS HTML Files ###

library(visNetwork)
library(igraph)
library(leaflet)
library(sp)



setwd("~/Documents/DiscoverNPS")
deatheaters_rel <- read.csv("Deatheaters_Relational.csv", stringsAsFactors = FALSE)
deatheaters_geo <- read.csv("Deatheaters_Geospatial.csv", stringsAsFactors = FALSE)

deatheaters_geo$Latitude <- stringr::str_trim(deatheaters_geo$Latitude)
deatheaters_geo$Longitute <- stringr::str_trim(deatheaters_geo$Longitute)
deatheaters_geo$Latitude = gsub('°', 'd', deatheaters_geo$Latitude)
deatheaters_geo$Longitute = gsub('°', 'd', deatheaters_geo$Longitute)

#deatheaters_geo$Latitude = measurements::conv_unit(deatheaters_geo$Latitude, from = 'deg_dec_min', to = 'dec_deg')
temp_lat2 <- char2dms(deatheaters_geo$Latitude)
temp_long2 <- char2dms(deatheaters_geo$Longitute)
deatheaters_geo$Lat <- as.numeric(temp_lat2)
deatheaters_geo$Long <- as.numeric(temp_long2)


dumbledores_army_geo <- read.csv("Dumbledores Army_Geospatial.csv", stringsAsFactors = FALSE)
dumbledores_army_geo$Longitude <- stringr::str_trim(dumbledores_army_geo$Longitude)
dumbledores_army_geo$Longitude <- gsub(" ", "", dumbledores_army_geo$Longitude)
dumbledores_army_geo$Longitude <- as.numeric(dumbledores_army_geo$Longitude)
dumbledores_army_geo$Longitude <- ifelse(dumbledores_army_geo$Longitude > 0, dumbledores_army_geo$Longitude * -1, dumbledores_army_geo$Longitude)
dumbledores_army_geo$new_name <- paste("<b>", dumbledores_army_geo$Actor, "</b><br>",
                                       "<b>Location: </b>", dumbledores_army_geo$Location, "<br>",
                                       "<b>Primary Spell: </b>", dumbledores_army_geo$Primary.Spell, "<br>",
                                       "<b>Secondary Spell: </b>", dumbledores_army_geo$Secondary.Spell, sep = "")


da_army_map <- leaflet(dumbledores_army_geo) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addMarkers(lng = ~Longitude, lat = ~Latitute, popup = ~new_name,
             clusterOptions = markerClusterOptions())


mapview::mapshot(da_army_map, url = "da_army_map.html")

### Dumbledore's Army Network ###

da_net_rel <- read.csv("Dumbledores Army_Relational.csv", stringsAsFactors = FALSE)

da_m <- as.matrix(da_net_rel)

g1 <- graph_from_edgelist(da_m[,1:2], directed = FALSE)
V(g1)$Degree <- degree(g1, v=V(g1), mode = ("total"), normalized = FALSE)
V(g1)$Betweenness <- betweenness(g1, v=V(g1), normalized = TRUE)
V(g1)$Closeness <- closeness(g1, v=V(g1), normalized = TRUE)

temp_net3 <- toVisNetworkData(g1)
temp_net3$temp_cent <- centralization.betweenness(g1)$centralization
temp_net3$avg_pl <- average.path.length(g1, directed = FALSE)
temp_net3$avg_pl
temp4 <- temp_net3$avg_pl
temp_net3$deg_cent <- centralization.degree(g1, mode = c("total"))$centralization
temp4 <- temp_net3$deg_cent

data <- toVisNetworkData(g1)


temp_net <- toVisNetworkData(g1)
temp_net$v_count <- vcount(g1)
temp_net$e_count <- ecount(g1)
temp_net$avg_deg <- mean(V(g1)$Degree)
temp_net$avg_pl <- average.path.length(g1, directed = FALSE)
temp_net$deg_cent <- centralization.degree(g1, mode = c("total"), loops = FALSE)$centralization
temp_net$betw_cent <- centralization.betweenness(g1, directed = FALSE)$centralization
temp_net$clos_cent <- centralization.closeness(g1, mode = c("total"))$centralization
temp_net$eigen_cent <- centralization.evcent(g1)$centralization

da_nodes <- data$nodes
da_edges <- data$edges

deg_cent <- centralization.degree(g1, mode = c("total"), loops = FALSE)$centralization

avg_pl <- average.path.length(g1, directed = FALSE)
betw_cent <- centralization.betweenness(g1, directed = FALSE)$centralization

deatheaters_geo2 <- deatheaters_geo[,c("Actor", "Location", "Primary.Spell", "Secondary.Spell", "Strength.of.the.Wizard", "Lat", "Long")]
dumbledores_army_geo$Lat <- dumbledores_army_geo$Latitute
dumbledores_army_geo$Long <- dumbledores_army_geo$Longitude
dumbledores_army_geo2 <- dumbledores_army_geo[,c("Actor", "Location", "Primary.Spell", "Secondary.Spell", "Strength.of.the.Wizard", "Lat", "Long")]

deatheaters_geo2$Group <- "Deatheaters"
dumbledores_army_geo2$Group <- "DA"
combined_geo <- rbind(deatheaters_geo2, dumbledores_army_geo2)
combined_geo$marker_color <- ifelse(combined_geo$Group == "DA", "red", "gray")
#combined_geo$icon_type <- ifelse(combined_geo$Group == "DA", "hat-wizard", "skull-crossbones")
combined_geo$new_name <- paste("<b><em>", combined_geo$Actor, "</em></b><br>",
                                       "<b>Location: </b>", combined_geo$Location, "<br>",
                                       "<b>Primary Spell: </b>", combined_geo$Primary.Spell, "<br>",
                                       "<b>Secondary Spell: </b>", combined_geo$Secondary.Spell, sep = "")



spells1 <- unique(combined_geo$Primary.Spell, combined_geo$Secondary.Spell)
spells2 <- unique(spells1)

spells2 <- sort(spells2)


spells_comb <- rbind(spells1, spells2)


wizard_strength <- combined_geo[,c("Actor", "Strength.of.the.Wizard")]

wizard_strength$id <- wizard_strength$Actor
wizard_strength$Strength <- wizard_strength$Strength.of.the.Wizard
wizard_strength$Strength.of.the.Wizard <- NULL
wizard_strength$Actor <- NULL

saveRDS(wizard_strength, "wizard_strength.rds")
saveRDS(spells_comb, "spells_comb.rds")
saveRDS(combined_geo, "combined_geo.rds")
saveRDS(da_net_rel, "da_net_rel.rds")
saveRDS(da_self_loop_m, "da_self_loop_m.rds")
saveRDS(death_self_loop_m, "death_self_loop_m.rds")
saveRDS(deatheaters_rel, "deatheaters_rel.rds")
