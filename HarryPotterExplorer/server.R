#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(visNetwork)
library(igraph)
library(shinyBS)

combined_geo <- readRDS("combined_geo2.rds")
da_self_loop_m <- readRDS("da_self_loop_m_v2.rds")
da_net_rel <- readRDS("da_net_rel_v2.rds")
death_self_loop_m <- readRDS("death_self_loop_m.rds")
deatheaters_rel <- readRDS("deatheaters_rel.rds")
wizard_strength <- readRDS("wizard_strength_v2.rds")
wizard_info <- readRDS("wizard_info.rds")

combined_geo_da <- subset(combined_geo, combined_geo$Group != "DA")
combined_geo_death <- subset(combined_geo, combined_geo$Group != "Deatheaters")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  toggleModal(session, "id1", toggle = "open")
  
  observeEvent(input$no_good, {
    toggleModal(session, "id1", toggle = "close")
  })
  
  ### Image for Modal ###
  output$IntroImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./images/Marauder.png'))
    
    
  }, deleteFile = FALSE)
  
  output$da_geo <- renderLeaflet({
    leaflet() %>%
      setView(lng = 0, lat = 54, zoom = 5) %>%
      # Use Watercolor provider tiles
      addProviderTiles(providers$Stamen.Watercolor,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = combined_geo_da, lng = ~Long, lat = ~Lat,
                 popup = ~new_name, group = "Dumbledore's Army",
                 clusterOptions = markerClusterOptions()) %>%
      addMarkers(data = combined_geo_death, lng = ~Long, lat = ~Lat,
                 popup = ~new_name, group = "Deatheaters",
                 clusterOptions = markerClusterOptions()) %>%
      addLayersControl(
        overlayGroups = c("Dumbledore's Army", "Deatheaters"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  #  DA Network
  create_da_network <- reactive({
    cutoff <- input$network_strength
    size_nodes_by <- input$node_sizer
    temp_net <- list()
      temp_da_rel <- subset(da_net_rel, da_net_rel$Strength.of.the.Relationship >= cutoff)
      da_m <- as.matrix(temp_da_rel)
      # Create self-loop so that isolates are included
      da_m_comb <- rbind(da_m, da_self_loop_m)
      g1 <- graph_from_edgelist(da_m_comb[,1:2], directed = FALSE)
      # Remove multiple relationships and self-loops
      g1 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE)
      # Measures
      V(g1)$Degree <- degree(g1, v=V(g1), mode = ("total"), normalized = FALSE)
      V(g1)$Betweenness <- betweenness(g1, v=V(g1), normalized = TRUE)
      V(g1)$Closeness <- closeness(g1, v=V(g1), normalized = TRUE)
      V(g1)$Eigenvector <- eigen_centrality(g1)$vector
      
      
      temp_net <- toVisNetworkData(g1)
      
      # Merge Strength of Wizard to nodes
      temp_net$nodes <- merge(temp_net$nodes, wizard_strength, by = "id", all.x = TRUE, all.y = FALSE)
      temp_net$nodes <- merge(temp_net$nodes, wizard_info, by = "id", all.x = TRUE, all.y = FALSE)
      row.names(temp_net$nodes) <- temp_net$nodes$id
      
      # Save Network Topography measures
      temp_net$v_count <- vcount(g1)
      temp_net$e_count <- ecount(g1)
      temp_net$avg_deg <- mean(V(g1)$Degree)
      temp_net$avg_pl <- average.path.length(g1, directed = FALSE)
      temp_net$deg_cent <- centralization.degree(g1, mode = c("total"), loops = FALSE)$centralization
      temp_net$betw_cent <- centralization.betweenness(g1, directed = FALSE)$centralization
      temp_net$clos_cent <- centralization.closeness(g1, mode = c("total"))$centralization
      temp_net$eigen_cent <- centralization.evcent(g1)$centralization
      # Round centrality metrics
      temp_net$nodes$Betweenness_r <- round(temp_net$nodes$Betweenness, 4)
      temp_net$nodes$Closeness_r <- round(temp_net$nodes$Closeness, 4)
      temp_net$nodes$Eigenvector_r <- round(temp_net$nodes$Eigenvector, 4)
      # Size nodes
      if(size_nodes_by == "Degree"){
        temp_net$nodes$size <- scales::rescale(temp_net$nodes$Degree, to = c(5,20))
      }
      if(size_nodes_by == "Strength"){
        temp_net$nodes$size <- scales::rescale(temp_net$nodes$Strength, to = c(5,20))
      }
      if(size_nodes_by == "Closeness"){
        temp_net$nodes$size <- scales::rescale(temp_net$nodes$Closeness, to = c(5,20))
      }
      if(size_nodes_by == "Betweenness"){
        temp_net$nodes$size <- scales::rescale(temp_net$nodes$Betweenness, to = c(5,20))
      }
      if(size_nodes_by == "Eigenvector"){
        temp_net$nodes$size <- scales::rescale(temp_net$nodes$Eigenvector, to = c(5,20))
      }
      # Node styling
      temp_net$nodes$color.border <- "black"
      temp_net$nodes$color.background <- "tomato"
      # Node title
      temp_net$nodes$title <- paste0("<b><em>", temp_net$nodes$id, "</em></b><br>",
                                     "<b>Wizard Strength: </b>", temp_net$nodes$Strength, "<br>",
                                     "<b>Primary Spell: </b>", temp_net$nodes$Primary.Spell, "<br>",
                                     "Counter Spell: ", temp_net$nodes$Counter.Spell, "<br>",
                                     "<b>Secondary Spell: </b>", temp_net$nodes$Secondary.Spell, "<br>",
                                     "Counter Spell: ", temp_net$nodes$Counter.Spell.1, "<br>",
                                     "<b>Degree: </b>", temp_net$nodes$Degree, "<br>",
                                     "<b>Betweenness: </b>", temp_net$nodes$Betweenness_r, "<br>",
                                     "<b>Closeness: </b>", temp_net$nodes$Closeness_r, "<br>",
                                     "<b>Eigenvector: </b>", temp_net$nodes$Eigenvector_r)
      return(temp_net)
    
  })
  
  output$da_net <- renderVisNetwork({
    # Create network
    temp_net2 <- create_da_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else{
      temp_nodes <- temp_net2$nodes
      temp_edges <- temp_net2$edges
      # If network is 2 or less use Igraph layout
      if(input$network_strength <= 2){
        visNetwork(temp_nodes, temp_edges) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
          visIgraphLayout()
      } else {
        visNetwork(temp_nodes, temp_edges) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      }
    }
    
  })
  #### Network Measures Output ####
  
  output$network_def_da <- renderText({
    node_sizer2 <- input$node_sizer
    if(node_sizer2 == "None" | node_sizer2 == "Strength") {
      return(NULL)
    } else
      if(node_sizer2 == "Degree"){
        out_text <- "Network members who have a high number of degree centrality will be the ones with all the information and the resources needed to carry out the mission."
      }
    if(node_sizer2 == "Betweenness"){
      out_text <- "Another name for betweenness could be “bridge”, betweenness tells us who connects the separate groups within each network. Individuals with a high degree of betweenness are more important than those with less to supporting the mission because they connect everybody together."
    }
    if(node_sizer2 == "Closeness"){
      
      out_text <- "Closeness is how connected an individual is to the other members of the network. The individuals with the higher levels of closeness are considered more trustworthy."
    }
    if(node_sizer2 == "Eigenvector"){
      out_text <- "Members with a high degree of Eigenvector centrality are the key members of the network. Without these members the network will be weakened, and the mission will be in jeopardy."
    }
    
    return(out_text)
    
  })
  output$node_count <- renderText({
    temp_net2 <- create_da_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else{
    n_count2 <- temp_net2$v_count
    paste("Nodes: ", n_count2, sep = "")
    }
  })
  output$edge_count <- renderText({
    temp_net2 <- create_da_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else{
    e_count2 <- temp_net2$e_count
    paste("Edges: ", e_count2, sep = "")
    }
  })
  output$avg_deg <- renderText({
    temp_net2 <- create_da_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else {
      avg_deg2 <- temp_net2$avg_deg
      avg_deg2 <- round(avg_deg2, 4)
      paste("Average Degree: ", avg_deg2, sep = "")
    }
  })
  output$avg_path_length <- renderText({
    temp_net2 <- create_da_network()
    if(length(temp_net2) < 1){
      return(NULL)
      } else {
      avg_pl2 <- temp_net2$avg_pl
      avg_pl2 <- round(avg_pl2, 4)
      paste("Average Path Length: ", avg_pl2, sep = "")
    }
  })
  
  # Show centralization score only for how it is sized
  output$node_centz <- renderText({
    temp_net2 <- create_da_network()
    node_sizer2 <- input$node_sizer
    if(node_sizer2 == "None" | length(temp_net2) < 1 | node_sizer2 == "Strength") {
      return(NULL)
    } else
    if(node_sizer2 == "Degree"){
      deg_cent2 <- temp_net2$deg_cent
      deg_cent2 <- round(deg_cent2, 4)
      out_text <- paste("Degree Centralization: ", deg_cent2, sep = "")
    }
    if(node_sizer2 == "Betweenness"){
      betw_cent2 <- temp_net2$betw_cent
      betw_cent2 <- round(betw_cent2, 4)
      out_text <- paste("Betweenness Centralization: ", betw_cent2, sep = "")
    }
    if(node_sizer2 == "Closeness"){
      clos_cent2 <- temp_net2$clos_cent
      clos_cent2 <- round(clos_cent2, 4)
      out_text <- paste("Closeness Centralization: ", clos_cent2, sep = "")
    }
    if(node_sizer2 == "Eigenvector"){
      eigen_cent2 <- temp_net2$eigen_cent
      eigen_cent2 <- round(eigen_cent2, 4)
      out_text <- paste("Eigenvector Centralization: ", eigen_cent2, sep = "")
    }
    
    return(out_text)
  })
  ### DA Table ###
  output$da_table <- renderDataTable({
    temp_net2 <- create_da_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else {
      for_dt <- temp_net2$nodes[,c("Strength","Degree", "Betweenness", "Closeness", "Eigenvector")]
      datatable(for_dt) %>%
        formatRound('Betweenness', 3) %>%
        formatRound('Closeness', 3) %>%
        formatRound('Eigenvector', 3)
    }
  })
  
  #### Deatheaters' Network ####
  
  create_death_network <- reactive({
    cutoff <- input$network_strength_death
    size_nodes_by_death <- input$node_sizer_death
    temp_net <- list()
    temp_death_rel <- subset(deatheaters_rel, deatheaters_rel$Strength.of.the.Relationship >= cutoff)
    death_m <- as.matrix(temp_death_rel)
    death_m_comb <- rbind(death_m, death_self_loop_m)
    g1 <- graph_from_edgelist(death_m_comb[,1:2], directed = FALSE)
    g1 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE)
    V(g1)$Degree <- degree(g1, v=V(g1), mode = ("total"), normalized = FALSE)
    V(g1)$Betweenness <- betweenness(g1, v=V(g1), normalized = TRUE)
    V(g1)$Closeness <- closeness(g1, v=V(g1), normalized = TRUE)
    V(g1)$Eigenvector <- eigen_centrality(g1)$vector
    
    
    temp_net <- toVisNetworkData(g1)
    
    # Merge Strength of Wizard to nodes
    temp_net$nodes <- merge(temp_net$nodes, wizard_strength, by = "id", all.x = TRUE, all.y = FALSE)
    temp_net$nodes <- merge(temp_net$nodes, wizard_info, by = "id", all.x = TRUE, all.y = FALSE)
    row.names(temp_net$nodes) <- temp_net$nodes$id
    
    temp_net$v_count <- vcount(g1)
    temp_net$e_count <- ecount(g1)
    temp_net$avg_deg <- mean(V(g1)$Degree)
    temp_net$avg_pl <- average.path.length(g1, directed = FALSE)
    temp_net$deg_cent <- centralization.degree(g1, mode = c("total"), loops = FALSE)$centralization
    temp_net$betw_cent <- centralization.betweenness(g1, directed = FALSE)$centralization
    temp_net$clos_cent <- centralization.closeness(g1, mode = c("total"))$centralization
    temp_net$eigen_cent <- centralization.evcent(g1)$centralization
    temp_net$nodes$Betweenness_r <- round(temp_net$nodes$Betweenness, 4)
    temp_net$nodes$Closeness_r <- round(temp_net$nodes$Closeness, 4)
    temp_net$nodes$Eigenvector_r <- round(temp_net$nodes$Eigenvector, 4)
    if(size_nodes_by_death == "Degree"){
      temp_net$nodes$size <- scales::rescale(temp_net$nodes$Degree, to = c(5,20))
    }
    if(size_nodes_by_death == "Strength"){
      temp_net$nodes$size <- scales::rescale(temp_net$nodes$Strength, to = c(5,20))
    }
    if(size_nodes_by_death == "Closeness"){
      temp_net$nodes$size <- scales::rescale(temp_net$nodes$Closeness, to = c(5,20))
    }
    if(size_nodes_by_death == "Betweenness"){
      temp_net$nodes$size <- scales::rescale(temp_net$nodes$Betweenness, to = c(5,20))
    }
    if(size_nodes_by_death == "Eigenvector"){
      temp_net$nodes$size <- scales::rescale(temp_net$nodes$Eigenvector, to = c(5,20))
    }
    temp_net$nodes$color.border <- "black"
    temp_net$nodes$color.background <- "grey"
    temp_net$nodes$title <- paste0("<b><em>", temp_net$nodes$id, "</em></b><br>",
                                   "<b>Wizard Strength: </b>", temp_net$nodes$Strength, "<br>",
                                   "<b>Primary Spell: </b>", temp_net$nodes$Primary.Spell, "<br>",
                                   "Counter Spell: ", temp_net$nodes$Counter.Spell, "<br>",
                                   "<b>Secondary Spell: </b>", temp_net$nodes$Secondary.Spell, "<br>",
                                   "Counter Spell: ", temp_net$nodes$Counter.Spell.1, "<br>",
                                   "<b>Degree: </b>", temp_net$nodes$Degree, "<br>",
                                   "<b>Betweenness: </b>", temp_net$nodes$Betweenness_r, "<br>",
                                   "<b>Closeness: </b>", temp_net$nodes$Closeness_r, "<br>",
                                   "<b>Eigenvector: </b>", temp_net$nodes$Eigenvector_r)
    return(temp_net)
    
  })
  
  output$death_net <- renderVisNetwork({
    temp_net2 <- create_death_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else{
      temp_nodes <- temp_net2$nodes
      temp_edges <- temp_net2$edges
      if(input$network_strength_death <= 2){
        visNetwork(temp_nodes, temp_edges) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
          visIgraphLayout()
      } else {
        visNetwork(temp_nodes, temp_edges) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      }
    }
    
  })
  
  #### Network Measures Output ####
  output$network_def_death <- renderText({
    node_sizer2 <- input$node_sizer_death
    if(node_sizer2 == "None" | node_sizer2 == "Strength") {
      return(NULL)
    } else
      if(node_sizer2 == "Degree"){
        out_text <- "Network members who have a high number of degree centrality will be the ones with all the information and the resources needed to carry out the mission."
      }
    if(node_sizer2 == "Betweenness"){
      out_text <- "Another name for betweenness could be “bridge”, betweenness tells us who connects the separate groups within each network. Individuals with a high degree of betweenness are more important than those with less to supporting the mission because they connect everybody together."
    }
    if(node_sizer2 == "Closeness"){
      
      out_text <- "Closeness is how connected an individual is to the other members of the network. The individuals with the higher levels of closeness are considered more trustworthy."
    }
    if(node_sizer2 == "Eigenvector"){
      out_text <- "Members with a high degree of Eigenvector centrality are the key members of the network. Without these members the network will be weakened, and the mission will be in jeopardy."
    }
    
    return(out_text)
    
  })
  
  #### Network Measures Output ####
  output$node_count_death <- renderText({
    temp_net2 <- create_death_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else{
      n_count2 <- temp_net2$v_count
      paste("Nodes: ", n_count2, sep = "")
    }
  })
  output$edge_count_death <- renderText({
    temp_net2 <- create_death_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else{
      e_count2 <- temp_net2$e_count
      paste("Edges: ", e_count2, sep = "")
    }
  })
  output$avg_deg_death <- renderText({
    temp_net2 <- create_death_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else {
      avg_deg2 <- temp_net2$avg_deg
      avg_deg2 <- round(avg_deg2, 4)
      paste("Average Degree: ", avg_deg2, sep = "")
    }
  })
  output$avg_path_length_death <- renderText({
    temp_net2 <- create_death_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else {
      avg_pl2 <- temp_net2$avg_pl
      avg_pl2 <- round(avg_pl2, 4)
      paste("Average Path Length: ", avg_pl2, sep = "")
    }
  })
  
  output$node_centz_death <- renderText({
    temp_net2 <- create_death_network()
    node_sizer2 <- input$node_sizer_death
    if(node_sizer2 == "None" | length(temp_net2) < 1 | node_sizer2 == "Strength") {
      return(NULL)
    } else
      if(node_sizer2 == "Degree"){
        deg_cent2 <- temp_net2$deg_cent
        deg_cent2 <- round(deg_cent2, 4)
        out_text <- paste("Degree Centralization: ", deg_cent2, sep = "")
      }
    if(node_sizer2 == "Betweenness"){
      betw_cent2 <- temp_net2$betw_cent
      betw_cent2 <- round(betw_cent2, 4)
      out_text <- paste("Betweenness Centralization: ", betw_cent2, sep = "")
    }
    if(node_sizer2 == "Closeness"){
      clos_cent2 <- temp_net2$clos_cent
      clos_cent2 <- round(clos_cent2, 4)
      out_text <- paste("Closeness Centralization: ", clos_cent2, sep = "")
    }
    if(node_sizer2 == "Eigenvector"){
      eigen_cent2 <- temp_net2$eigen_cent
      eigen_cent2 <- round(eigen_cent2, 4)
      out_text <- paste("Eigenvector Centralization: ", eigen_cent2, sep = "")
    }
    
    return(out_text)
  })
  ### Deatheaters' Table ###
  output$death_table <- renderDataTable({
    temp_net2 <- create_death_network()
    if(length(temp_net2) < 1){
      return(NULL)
    } else {
      for_dt <- temp_net2$nodes[,c("Strength","Degree", "Betweenness", "Closeness", "Eigenvector")]
      datatable(for_dt) %>%
        formatRound('Betweenness', 3) %>%
        formatRound('Closeness', 3) %>%
        formatRound('Eigenvector', 3)
    }
  })
  
})
