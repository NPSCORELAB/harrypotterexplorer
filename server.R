
shinyServer(function(input, output, session) {
  # Start - Modal --------------------------------------------------------------
  toggleModal(session,
              modalId = "id1",
              toggle  = "open")
  observeEvent(input$no_good, {
    toggleModal(session,
                modalId = "id1",
                toggle  = "close")
  })
  
  # Start - Geospatial Events Output -------------------------------------------
  output$da_geo <- renderLeaflet({
    leaflet() %>%
      setView(lng  = 0,
              lat  = 54,
              zoom = 5) %>%
      # Use Watercolor provider tiles
      addProviderTiles(provider = providers$Stamen.Watercolor,
                       options  = providerTileOptions(
                         noWrap = TRUE)
                       ) %>%
      addMarkers(data           = combined_geo_da,
                 lng            = ~ Long,
                 lat            = ~ Lat,
                 popup          = ~ new_name,
                 group          = "Dumbledore's Army",
                 clusterOptions = markerClusterOptions()
                 ) %>%
      addMarkers(data           = combined_geo_death,
                 lng            = ~ Long,
                 lat            = ~ Lat,
                 popup          = ~ new_name,
                 group          = "Death Eaters",
                 clusterOptions = markerClusterOptions()
                 ) %>%
      addLayersControl(overlayGroups = c("Dumbledore's Army",
                                         "Death Eaters"),
                       options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #  Start - Dumbledore's Army Network -----------------------------------------
  # Init reactive values
  da_reac_vals <- reactiveValues(counter = 1)
  da_reac_vals$da_net <- da_net
  da_reac_vals$cur_weight <- 4
  
  # Init DA Network
  output$da_net <- renderVisNetwork({
    # Create network
    temp_nodes <- da_net$nodes
    temp_edges <- da_net$edges
    # Render network
    visNetwork(temp_nodes, temp_edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  
  # Network Proxies Create and Draw
  observeEvent(input$network_strength, ignoreInit = TRUE, {
    # Get initial reactive values and inputs
    orig_edges <- da_reac_vals$da_net$edges
    cur_weight <- da_reac_vals$cur_weight
    new_weight <- input$network_strength
    # Save new weight to reactive value
    da_reac_vals$cur_weight <- new_weight
    
    # Subset the edges
    temp_da_rel <- subset(da_net_rel, 
                          da_net_rel$Strength.of.the.Relationship >= new_weight)
    da_m <- as.matrix(temp_da_rel)
    # Create self-loop so that isolates are included
    da_m_comb <- rbind(da_m, 
                       da_self_loop_m)
    
    ## Igraph
    g1 <- graph_from_edgelist(da_m_comb[,1:2], 
                              directed = FALSE)
    # Remove multiple relationships and self-loops
    g1 <- simplify(g1, 
                   remove.multiple = TRUE, 
                   remove.loops = TRUE)
    # Measures
    V(g1)$Degree <- degree(g1, v=V(g1), 
                           mode = ("total"), 
                           normalized = FALSE)
    V(g1)$Betweenness <- betweenness(g1, 
                                     v=V(g1), 
                                     normalized = TRUE)
    V(g1)$Closeness <- closeness(g1, 
                                 v=V(g1), 
                                 normalized = TRUE)
    V(g1)$Eigenvector <- eigen_centrality(g1)$vector
    
    ## Send to visNetwork to create list of lists
    da_net <- toVisNetworkData(g1)
    
    # Merge Strength and Info of Wizard to Nodes
    da_net$nodes <- merge(da_net$nodes, 
                          wizard_strength, by = "id", 
                          all.x = TRUE, 
                          all.y = FALSE)
    da_net$nodes <- merge(da_net$nodes, 
                          wizard_info, 
                          by = "id", 
                          all.x = TRUE, 
                          all.y = FALSE)
    # Save Node Id's as rownames for nodes
    row.names(da_net$nodes) <- da_net$nodes$id
    
    # Save Network Topography measures from igraph
    da_net$v_count <- vcount(g1)
    da_net$e_count <- ecount(g1)
    da_net$avg_deg <- mean(V(g1)$Degree)
    da_net$avg_pl <- average.path.length(g1, 
                                         directed = FALSE)
    da_net$deg_cent <- centralization.degree(g1, 
                                             mode = c("total"), 
                                             loops = FALSE)$centralization
    da_net$betw_cent <- centralization.betweenness(g1, 
                                                   directed = FALSE)$centralization
    da_net$clos_cent <- centralization.closeness(g1, 
                                                 mode = c("total"))$centralization
    da_net$eigen_cent <- centralization.evcent(g1)$centralization
    # Round centrality metrics
    da_net$nodes$Betweenness_r <- round(da_net$nodes$Betweenness, 4)
    da_net$nodes$Closeness_r <- round(da_net$nodes$Closeness, 4)
    da_net$nodes$Eigenvector_r <- round(da_net$nodes$Eigenvector, 4)
    
    # Node styling
    da_net$nodes$color.border <- "black"
    da_net$nodes$color.background <- "tomato"
    da_net$nodes$shadow <- TRUE
    
    # Node sizing by sizer
    size_nodes_by <- input$node_sizer
    
    if(size_nodes_by == "Degree"){
      da_net$nodes$size <- scales::rescale(da_net$nodes$Degree, to = c(5,20))
    }
    if(size_nodes_by == "Wizard Strength"){
      da_net$nodes$size <- scales::rescale(da_net$nodes$Strength, to = c(5,20))
    }
    if(size_nodes_by == "Closeness"){
      da_net$nodes$size <- scales::rescale(da_net$nodes$Closeness, to = c(5,20))
    }
    if(size_nodes_by == "Betweenness"){
      da_net$nodes$size <- scales::rescale(da_net$nodes$Betweenness, to = c(5,20))
    }
    if(size_nodes_by == "Eigenvector"){
      da_net$nodes$size <- scales::rescale(da_net$nodes$Eigenvector, to = c(5,20))
    }
    if(size_nodes_by == "None"){
      da_net$nodes$size <- 20
    }
    
    # Node title
    da_net$nodes$title <- paste0("<b><em>", da_net$nodes$id, "</em></b><br>",
                                 "<b>Wizard Strength: </b>", da_net$nodes$Strength, "<br>",
                                 "<b>Primary Spell: </b>", da_net$nodes$Primary.Spell, "<br>",
                                 "Counter Spell: ", da_net$nodes$Counter.Spell, "<br>",
                                 "<b>Secondary Spell: </b>", da_net$nodes$Secondary.Spell, "<br>",
                                 "Counter Spell: ", da_net$nodes$Counter.Spell.1, "<br>",
                                 "<b>Degree: </b>", da_net$nodes$Degree, "<br>",
                                 "<b>Betweenness: </b>", da_net$nodes$Betweenness_r, "<br>",
                                 "<b>Closeness: </b>", da_net$nodes$Closeness_r, "<br>",
                                 "<b>Eigenvector: </b>", da_net$nodes$Eigenvector_r)
    
    # Edges info
    da_net$edges$id <- paste(da_net$edges$from, 
                             da_net$edges$to, 
                             sep = "_")
    da_net$edges$opp_id <- paste(da_net$edges$to, 
                                 da_net$edges$from, 
                                 sep = "_")
    da_net$edges$shadow <- TRUE
    da_net$edges$color <- "tomato"
    
    ## Either add or remove edges and update nodes
    if(cur_weight > new_weight){
      # Add edges if new weight is less than old weight
      edges_add <- subset(da_net$edges, 
                          !is.element(da_net$edges$id, 
                                      orig_edges$id) & 
                            !is.element(da_net$edges$opp_id, orig_edges$id))
      # Update Network
      visNetworkProxy("da_net") %>%
        visUpdateEdges(edges_add) %>%
        visUpdateNodes(da_net$nodes, 
                       updateOptions = FALSE)
      
    } else if(cur_weight < new_weight){
      # Remove edges if new weight is greater than old weight
      edges_remove <- subset(orig_edges, 
                             !is.element(orig_edges$id, 
                                         da_net$edges$id) &
                               !is.element(orig_edges$id, da_net$edges$opp_id))
      # Update Network
      visNetworkProxy("da_net") %>%
        visRemoveEdges(edges_remove$id) %>%
        visUpdateNodes(da_net$nodes, 
                       updateOptions = FALSE)
    }
    
    ## Switch between normal visualization and igraph layout
    if(new_weight <= 2){
      # Happens if weight is 2 or less
      # Manually do igraph layout
      temp_nodes <- da_net$nodes
      temp_edges <- da_net$edges
      
      coord <- igraph::layout_nicely(g1)
      da_net$nodes$x <- coord[,1]
      da_net$nodes$y <- coord[,2]
      
      # Hard coding to normalize to area of
      to <- c(-500, 500)
      from <- range(da_net$nodes$x, 
                    na.rm = TRUE, 
                    finite = TRUE)
      da_net$nodes$x  <- (da_net$nodes$x - from[1])/diff(from) * diff(to) + to[1]
      from <- range(da_net$nodes$y, 
                    na.rm = TRUE, 
                    finite = TRUE)
      da_net$nodes$y <- (da_net$nodes$y - from[1])/diff(from) * diff(to) + to[1]
      
      # Set node and edges options
      da_net$nodes$physics <- FALSE
      da_net$edges$smooth = FALSE
      da_net$edges$physics <- FALSE
      
      # Update network
      visNetworkProxy("da_net") %>%
        visPhysics(stabilization = FALSE) %>%
        visUpdateNodes(da_net$nodes, 
                       updateOptions = FALSE) %>%
        visEdges(da_net$edges, physics = FALSE, smooth = FALSE)
    } else if(cur_weight == 2 & new_weight == 3){
      # Happens if moving from weight 2 to 3
      
      # Set node and edges options back
      da_net$nodes$physics <- TRUE
      da_net$edges$smooth <- TRUE
      da_net$edges$physics <- TRUE
      
      visNetworkProxy("da_net") %>%
        visPhysics(stabilization = TRUE) %>%
        visUpdateNodes(da_net$nodes, 
                       updateOptions =FALSE) %>%
        visEdges(da_net$edges, physics = TRUE, smooth = TRUE)
    }
    
    ## Save results to reactive values
    da_reac_vals$da_net <- da_net
  })
  
  # Node Sizing
  observeEvent(input$node_sizer, ignoreInit = TRUE, {
    size_nodes_by <- input$node_sizer
    temp_nodes <- da_reac_vals$da_net$nodes
    
    if(size_nodes_by == "Degree"){
      temp_nodes$size <- scales::rescale(temp_nodes$Degree, to = c(5,20))
    }
    if(size_nodes_by == "Wizard Strength"){
      temp_nodes$size <- scales::rescale(temp_nodes$Strength, to = c(5,20))
    }
    if(size_nodes_by == "Closeness"){
      temp_nodes$size <- scales::rescale(temp_nodes$Closeness, to = c(5,20))
    }
    if(size_nodes_by == "Betweenness"){
      temp_nodes$size <- scales::rescale(temp_nodes$Betweenness, to = c(5,20))
    }
    if(size_nodes_by == "Eigenvector"){
      temp_nodes$size <- scales::rescale(temp_nodes$Eigenvector, to = c(5,20))
    }
    if(size_nodes_by == "None"){
      temp_nodes$size <- 20
    }
    
    visNetworkProxy("da_net") %>%
      visUpdateNodes(temp_nodes, 
                     updateOptions = FALSE)
    
    da_reac_vals$da_net$nodes <- temp_nodes
    
  })
  
  
  ### Side Panel ###
  # Network metrics using reactive network:
  output$network_def_da <- renderText({
    node_sizer2 <- input$node_sizer
    if (node_sizer2 == "None" | node_sizer2 == "Wizard Strength") {
      return(NULL)
    } else
      if (node_sizer2 == "Degree") {
        out_text <- "Network members who have a high degree centrality will may have direct influence and have the information and resources needed to carry out the mission."
      }
    if (node_sizer2 == "Betweenness") {
      out_text <- "Another name for betweenness could be “bridge”, betweenness tells us who connects the separate groups within each network. Individuals with a high degree of betweenness are more important than those with less to supporting the mission because they connect everybody together."
    }
    if (node_sizer2 == "Closeness") {
      out_text <- "Closeness is how connected an individual is to the other members of the network. The individuals with higher levels of closeness are considered more trustworthy."
    }
    if (node_sizer2 == "Eigenvector") {
      out_text <- "Members with a high degree of Eigenvector centrality are key members of the network and likely have high indirect influence. Without these members the network will be weakened, and the mission will be in jeopardy."
    }
    return(out_text)
  })
  # Network Topography Measures
  output$node_count <- renderText({
    temp_v_count <- da_reac_vals$da_net$v_count
    if (is.null(temp_v_count)) {
      return(NULL)
    } else {
    paste("Nodes: ",
          temp_v_count,
          sep = "")
    }
  })
  output$edge_count <- renderText({
    temp_e_count <- da_reac_vals$da_net$e_count
    if (is.null(temp_e_count)) {
      return(NULL)
    } else {
    paste("Edges: ",
          temp_e_count,
          sep = "")
    }
  })
  output$avg_deg <- renderText({
    temp_avg_deg <- da_reac_vals$da_net$avg_deg
    if (is.null(temp_avg_deg)) {
      return(NULL)
    } else {
      # Round Avg Deg
      avg_deg2 <- round(temp_avg_deg,
                        digits = 4)
      paste("Average Degree: ",
            avg_deg2,
            sep = "")
    }
  })
  output$avg_path_length <- renderText({
    temp_avg_pl<- da_reac_vals$da_net$avg_pl
    if (is.null(temp_avg_pl)) {
      return(NULL)
    } else {
      # Round Path Length
      avg_pl2 <- round(temp_avg_pl,
                       digits = 4)
      paste("Average Path Length: ",
            avg_pl2,
            sep = "")
    }
  })
  
  # Get centralization with reactive network:
  output$node_centz <- renderText({
    temp_net2 <- da_reac_vals$da_net
    node_sizer2 <- input$node_sizer
    if (node_sizer2 == "None" | length(temp_net2) < 1 | node_sizer2 == "Wizard Strength") {
      return(NULL)
    } else
    if (node_sizer2 == "Degree") {
      deg_cent2 <- temp_net2$deg_cent
      deg_cent2 <- round(deg_cent2,
                         digits = 4)
      out_text <- paste("Degree Centralization: ",
                        deg_cent2,
                        sep = "")
    }
    if (node_sizer2 == "Betweenness") {
      betw_cent2 <- temp_net2$betw_cent
      betw_cent2 <- round(betw_cent2,
                          digits = 4)
      out_text <- paste("Betweenness Centralization: ",
                        betw_cent2,
                        sep = "")
    }
    if (node_sizer2 == "Closeness") {
      clos_cent2 <- temp_net2$clos_cent
      clos_cent2 <- round(clos_cent2,
                          digits = 4)
      out_text <- paste("Closeness Centralization: ",
                        clos_cent2,
                        sep = "")
    }
    if (node_sizer2 == "Eigenvector") {
      eigen_cent2 <- temp_net2$eigen_cent
      eigen_cent2 <- round(eigen_cent2, 
                           digits = 4)
      out_text <- paste("Eigenvector Centralization: ",
                        eigen_cent2,
                        sep = "")
    }
    return(out_text)
  })
  
  # Scores table for reactive table:
  output$da_table <- renderDataTable({
    temp_net2 <- da_reac_vals$da_net
    if (length(temp_net2) < 1) {
      return(NULL)
    } else {
      for_dt <- temp_net2$nodes[,c("Strength",
                                   "Degree",
                                   "Betweenness",
                                   "Closeness",
                                   "Eigenvector")]  %>%
        dplyr::rename(`Wizard Strength` = "Strength")
      
      datatable(for_dt) %>%
        formatRound('Betweenness',
                    digits = 3) %>%
        formatRound('Closeness',
                    digits = 3) %>%
        formatRound('Eigenvector',
                    digits = 3)
    }
  })
  
  # Start - Death Eater's Output ------------------------------------------------
  # Init reactive values
  death_reac_vals <- reactiveValues(counter = 1)
  death_reac_vals$death_net <- death_net
  death_reac_vals$cur_weight <- 4
  death_reac_vals$inc_snape <- TRUE
  
  
  # Init Death Network
  output$death_net <- renderVisNetwork({
    # Create network
    temp_nodes <- death_net$nodes
    temp_edges <- death_net$edges
    # Render network
    visNetwork(temp_nodes, temp_edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  
  # Network Proxies Create and Draw
  observeEvent(c(input$network_strength_death, input$inc_snape), ignoreInit = TRUE, {
    # Get initial reactive values and inputs
    orig_edges <- death_reac_vals$death_net$edges
    cur_weight <- death_reac_vals$cur_weight
    new_weight <- input$network_strength_death
    old_snape <- death_reac_vals$inc_snape
    include_snape <- ifelse(input$inc_snape == "y", TRUE,FALSE)
    # Save new weight to reactive value
    death_reac_vals$cur_weight <- new_weight
    death_reac_vals$inc_snape <- include_snape
    
    # Subset the edges
    temp_death_rel <- subset(deatheaters_rel, 
                             deatheaters_rel$Strength.of.the.Relationship >= new_weight)
    
    temp_self_loop <- death_self_loop_m
    
    if(include_snape == FALSE){
      temp_death_rel <- subset(temp_death_rel, 
                               temp_death_rel$Actor.1 != "Severus Snape" &
                                 temp_death_rel$Actor.2 != "Severus Snape")
      
      temp_self_loop <- subset(temp_self_loop, 
                               temp_self_loop[,1] != "Severus Snape")
    }
    death_m <- as.matrix(temp_death_rel)
    # Create self-loop so that isolates are included
    death_m_comb <- rbind(death_m, 
                       temp_self_loop)
    
    ## Igraph
    g1 <- graph_from_edgelist(death_m_comb[,1:2], 
                              directed = FALSE)
    # Remove multiple relationships and self-loops
    g1 <- simplify(g1, 
                   remove.multiple = TRUE, 
                   remove.loops = TRUE)
    # Measures
    V(g1)$Degree <- degree(g1, v=V(g1), 
                           mode = ("total"), 
                           normalized = FALSE)
    V(g1)$Betweenness <- betweenness(g1, 
                                     v=V(g1), 
                                     normalized = TRUE)
    V(g1)$Closeness <- closeness(g1, 
                                 v=V(g1), 
                                 normalized = TRUE)
    V(g1)$Eigenvector <- eigen_centrality(g1)$vector
    
    ## Send to visNetwork to create list of lists
    death_net <- toVisNetworkData(g1)
    
    # Merge Strength and Info of Wizard to Nodes
    death_net$nodes <- merge(death_net$nodes, 
                          wizard_strength, by = "id", 
                          all.x = TRUE, 
                          all.y = FALSE)
    death_net$nodes <- merge(death_net$nodes, 
                          wizard_info, 
                          by = "id", 
                          all.x = TRUE, 
                          all.y = FALSE)
    # Save Node Id's as rownames for nodes
    row.names(death_net$nodes) <- death_net$nodes$id
    
    # Save Network Topography measures from igraph
    death_net$v_count <- vcount(g1)
    death_net$e_count <- ecount(g1)
    death_netavg_deg <- mean(V(g1)$Degree)
    death_net$avg_pl <- average.path.length(g1, 
                                         directed = FALSE)
    death_net$deg_cent <- centralization.degree(g1, 
                                             mode = c("total"), 
                                             loops = FALSE)$centralization
    death_net$betw_cent <- centralization.betweenness(g1, 
                                                   directed = FALSE)$centralization
    death_net$clos_cent <- centralization.closeness(g1, 
                                                 mode = c("total"))$centralization
    death_net$eigen_cent <- centralization.evcent(g1)$centralization
    # Round centrality metrics
    death_net$nodes$Betweenness_r <- round(death_net$nodes$Betweenness, 4)
    death_net$nodes$Closeness_r <- round(death_net$nodes$Closeness, 4)
    death_net$nodes$Eigenvector_r <- round(death_net$nodes$Eigenvector, 4)
    
    # Node styling
    death_net$nodes$color.border <- "black"
    death_net$nodes$color.background <- "grey"
    death_net$nodes$shadow <- TRUE
    
    # Node sizing by sizer
    size_nodes_by <- input$node_sizer_death
    
    if(size_nodes_by == "Degree"){
      death_net$nodes$size <- scales::rescale(death_net$nodes$Degree, to = c(5,20))
    }
    if(size_nodes_by == "Wizard Strength"){
      death_net$nodes$size <- scales::rescale(death_net$nodes$Strength, to = c(5,20))
    }
    if(size_nodes_by == "Closeness"){
      death_net$nodes$size <- scales::rescale(death_net$nodes$Closeness, to = c(5,20))
    }
    if(size_nodes_by == "Betweenness"){
      death_net$nodes$size <- scales::rescale(death_net$nodes$Betweenness, to = c(5,20))
    }
    if(size_nodes_by == "Eigenvector"){
      death_net$nodes$size <- scales::rescale(death_net$nodes$Eigenvector, to = c(5,20))
    }
    if(size_nodes_by == "None"){
      death_net$nodes$size <- 20
    }
    
    # Node title
    death_net$nodes$title <- paste0("<b><em>", death_net$nodes$id, "</em></b><br>",
                                 "<b>Wizard Strength: </b>", death_net$nodes$Strength, "<br>",
                                 "<b>Primary Spell: </b>", death_net$nodes$Primary.Spell, "<br>",
                                 "Counter Spell: ", death_net$nodes$Counter.Spell, "<br>",
                                 "<b>Secondary Spell: </b>", death_net$nodes$Secondary.Spell, "<br>",
                                 "Counter Spell: ", death_net$nodes$Counter.Spell.1, "<br>",
                                 "<b>Degree: </b>", death_net$nodes$Degree, "<br>",
                                 "<b>Betweenness: </b>", death_net$nodes$Betweenness_r, "<br>",
                                 "<b>Closeness: </b>", death_net$nodes$Closeness_r, "<br>",
                                 "<b>Eigenvector: </b>", death_net$nodes$Eigenvector_r)
    
    # Edges info
    death_net$edges$id <- paste(death_net$edges$from, 
                                death_net$edges$to, 
                             sep = "_")
    death_net$edges$opp_id <- paste(death_net$edges$to, 
                                    death_net$edges$from, 
                                 sep = "_")
    death_net$edges$shadow <- TRUE
    death_net$edges$color <- "grey"
    
    # If Snape is added or Removed
    if(old_snape != include_snape){
      if(include_snape == TRUE){
        edges_add <- subset(death_net$edges, 
                            !is.element(death_net$edges$id, 
                                        orig_edges$id) & 
                              !is.element(death_net$edges$opp_id, orig_edges$id))
        # Update Network
        visNetworkProxy("death_net") %>%
          visUpdateEdges(edges_add) %>%
          visUpdateNodes(death_net$nodes, 
                         updateOptions = FALSE)
        
      } else {
        # Remove edges if new weight is greater than old weight
        edges_remove <- subset(orig_edges, 
                               !is.element(orig_edges$id, 
                                           death_net$edges$id) &
                                 !is.element(orig_edges$id,
                                             death_net$edges$opp_id))
        # Update Network
        visNetworkProxy("death_net") %>%
          visRemoveEdges(edges_remove$id) %>%
          visRemoveNodes(id = "Severus Snape",
                         updateOptions = FALSE) %>%
          visUpdateNodes(death_net$nodes, 
                         updateOptions = FALSE)
        
      }
    }
    
    ## Either add or remove edges and update nodes
    if(cur_weight > new_weight){
      # Add edges if new weight is less than old weight
      edges_add <- subset(death_net$edges, 
                          !is.element(death_net$edges$id, 
                                      orig_edges$id) & 
                            !is.element(death_net$edges$opp_id, 
                                        orig_edges$id))
      # Update Network
      visNetworkProxy("death_net") %>%
        visUpdateEdges(edges_add) %>%
        visUpdateNodes(death_net$nodes, 
                       updateOptions = FALSE)
      
    } else if(cur_weight < new_weight){
      # Remove edges if new weight is greater than old weight
      edges_remove <- subset(orig_edges, 
                             !is.element(orig_edges$id, 
                                         death_net$edges$id) &
                               !is.element(orig_edges$id,
                                           death_net$edges$opp_id))
      # Update Network
      visNetworkProxy("death_net") %>%
        visRemoveEdges(edges_remove$id) %>%
        visUpdateNodes(death_net$nodes, 
                       updateOptions = FALSE)
    }
    
    ## Switch between normal visualization and igraph layout
    if(new_weight <= 2){
      # Happens if weight is 2 or less
      # Manually do igraph layout
      temp_nodes <- death_net$nodes
      temp_edges <- death_net$edges
      
      coord <- igraph::layout_nicely(g1)
      death_net$nodes$x <- coord[,1]
      death_net$nodes$y <- coord[,2]
      
      # Hard coding to normalize to area of
      to <- c(-500, 500)
      from <- range(death_net$nodes$x, 
                    na.rm = TRUE, 
                    finite = TRUE)
      death_net$nodes$x  <- (death_net$nodes$x - from[1])/diff(from) * diff(to) + to[1]
      from <- range(death_net$nodes$y, 
                    na.rm = TRUE, 
                    finite = TRUE)
      death_net$nodes$y <- (death_net$nodes$y - from[1])/diff(from) * diff(to) + to[1]
      
      # Set node and edges options
      death_net$nodes$physics <- FALSE
      death_net$edges$smooth = FALSE
      death_net$edges$physics <- FALSE
      
      # Update network
      visNetworkProxy("death_net") %>%
        visPhysics(stabilization = FALSE) %>%
        visUpdateNodes(death_net$nodes, 
                       updateOptions = FALSE) %>%
        visEdges(death_net$edges, physics = FALSE, smooth = FALSE)
    } else if(cur_weight == 2 & new_weight == 3){
      # Happens if moving from weight 2 to 3
      
      # Set node and edges options back
      death_net$nodes$physics <- TRUE
      death_net$edges$smooth <- TRUE
      death_net$edges$physics <- TRUE
      
      visNetworkProxy("death_net") %>%
        visPhysics(stabilization = TRUE) %>%
        visUpdateNodes(death_net$nodes, 
                       updateOptions =FALSE) %>%
        visEdges(death_net$edges, physics = TRUE, smooth = TRUE)
    }
    
    ## Save results to reactive values
    death_reac_vals$death_net <- death_net
  })
  
  
  # Network measures using reactive network:
  output$network_def_death <- renderText({
    node_sizer2 <- input$node_sizer_death
    if (node_sizer2 == "None" | node_sizer2 == "Wizard Strength") {
      return(NULL)
    } else
      if (node_sizer2 == "Degree") {
        out_text <- "Network members who have a high number of degree centrality will be the ones with all the information and the resources needed to carry out the mission."
      }
    if (node_sizer2 == "Betweenness") {
      out_text <- "Another name for betweenness could be “bridge”, betweenness tells us who connects the separate groups within each network. Individuals with a high degree of betweenness are more important than those with less to supporting the mission because they connect everybody together."
    }
    if (node_sizer2 == "Closeness") {
      out_text <- "Closeness is how connected an individual is to the other members of the network. The individuals with higher levels of closeness are considered more trustworthy."
    }
    if (node_sizer2 == "Eigenvector") {
      out_text <- "Members with a high degree of Eigenvector centrality are key members of the network and likely have high indirect influence. Without these members the network will be weakened, and the mission will be in jeopardy."
    }
    return(out_text)
  })
  
  
  # Network measures output from reactive network:
  
  output$node_count_death <- renderText({
    temp_v_count <- death_reac_vals$death_net$v_count
    if (is.null(temp_v_count)) {
      return(NULL)
    } else{
      paste("Nodes: ",
            temp_v_count,
            sep = "")
    }
  })
  output$edge_count_death <- renderText({
    temp_e_count <- death_reac_vals$death_net$e_count
    if (is.null(temp_e_count)) {
      return(NULL)
    } else{
      paste("Edges: ",
            temp_e_count,
            sep = "")
    }
  })
  output$avg_deg_death <- renderText({
    temp_avg_deg <- death_reac_vals$death_net$avg_deg
    if (is.null(temp_avg_deg)) {
      return(NULL)
    } else {
      avg_deg2 <- round(temp_avg_deg,
                        digits = 4)
      paste("Average Degree: ",
            avg_deg2,
            sep = "")
    }
  })
  output$avg_path_length_death <- renderText({
    temp_avg_pl <- death_reac_vals$death_net$avg_pl
    if (is.null(temp_avg_pl)) {
      return(NULL)
    } else {
      avg_pl2 <- round(temp_avg_pl,
                       digits = 4)
      paste("Average Path Length: ",
            avg_pl2,
            sep = "")
    }
  })
  
  output$node_centz_death <- renderText({
    temp_net2 <- death_reac_vals$death_net
    node_sizer2 <- input$node_sizer_death
    if (node_sizer2 == "None" | length(temp_net2) < 1 | node_sizer2 == "Wizard Strength") {
      return(NULL)
    } else
      if (node_sizer2 == "Degree") {
        deg_cent2 <- temp_net2$deg_cent
        deg_cent2 <- round(deg_cent2,
                           digits = 4)
        out_text <- paste("Degree Centralization: ",
                          deg_cent2,
                          sep = "")
      }
    if (node_sizer2 == "Betweenness") {
      betw_cent2 <- temp_net2$betw_cent
      betw_cent2 <- round(betw_cent2,
                          digits = 4)
      out_text <- paste("Betweenness Centralization: ",
                        betw_cent2,
                        sep = "")
    }
    if (node_sizer2 == "Closeness") {
      clos_cent2 <- temp_net2$clos_cent
      clos_cent2 <- round(clos_cent2, 
                          digits = 4)
      out_text <- paste("Closeness Centralization: ",
                        clos_cent2,
                        sep = "")
    }
    if (node_sizer2 == "Eigenvector") {
      eigen_cent2 <- temp_net2$eigen_cent
      eigen_cent2 <- round(eigen_cent2,
                           digits = 4)
      out_text <- paste("Eigenvector Centralization: ", 
                        eigen_cent2,
                        sep = "")
    }
    
    return(out_text)
  })
  
  # Scores table for reactive table:
  output$death_table <- renderDataTable({
    temp_net2 <- death_reac_vals$death_net
    if (length(temp_net2) < 1) {
      return(NULL)
    } else {
      for_dt <- temp_net2$nodes[,c("Strength",
                                    "Degree",
                                    "Betweenness",
                                    "Closeness",
                                    "Eigenvector")] %>%
        dplyr::rename(`Wizard Strength` = "Strength")
      
      datatable(for_dt) %>%
        formatRound('Betweenness',
                    digits = 3) %>%
        formatRound('Closeness',
                    digits = 3) %>%
        formatRound('Eigenvector',
                    digits = 3)
    }
  })
})
