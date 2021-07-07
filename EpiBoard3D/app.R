library(shiny)
library(shinydashboard)
library(igraph)
library(ggraph)
library(dplyr)
library(visNetwork)
library(stringi)
library(tidygraph)
library(threejs)

# There's only 2 layers currently, but added support for more.
layer_count <- 2

# Processing the list of different countries into a list
sub.folders <- list.dirs("data", recursive=FALSE)

# Creating a list of options for the select menu, all you need to do is drag
# and drop the different country data and it will add the country.
countries.options <- list()
for (country in sub.folders) {
    country.name <- substring(country, 6)
    countries.options[country.name] <- country
}


ui <- dashboardPage(
    dashboardHeader(title = "TICEpiBoard"),
    dashboardSidebar(
        sidebarMenu(
            selectInput("country", "Select Country",
                        choices = countries.options, selected = 1),
            selectInput("layer.selected", "Select Layer",
                        choices = 2:layer_count, selected = 2),
            checkboxInput("animation", strong("Hovering Animation"),
                          value = FALSE)
        )
    ),
    dashboardBody(
        div(visNetworkOutput("top_layer"),style="display:none;"), # static (I assume the first file is the genetic layer)
        div(visNetworkOutput("bottom_layer"),style="display:none;"), # changable via menu
        scatterplotThreeOutput("otherplot")
    )
)

# Returns a the list of layers based on the country selected.
get_country_networks <- function(which.option) {
    return(list.files(which.option, recursive=FALSE, full.names=TRUE))
}


generate_layout <- function(url) {
    
    covid.data <- read.csv(url, header=T)
    
    # gsub removes the .L0 or .L1 from the suffix, makes it easier for displaying
    colnames(covid.data) <- gsub('.{3}$', '', colnames(covid.data)) 
    rownames(covid.data) <- gsub('.{3}$', '', covid.data[,1])
    
    covid.data <- select(covid.data, -1)
    covid.network <- graph_from_adjacency_matrix(data.matrix(covid.data))
    return(layout_with_fr(covid.network))
}


generate_network <- function(url, layout) {
    
    covid.data <- read.csv(url, header=T)
    
    # Sets the matrix row names, then removes the row
    colnames(covid.data) <- gsub('.{3}$', '', colnames(covid.data)) 
    rownames(covid.data) <- gsub('.{3}$', '', covid.data[,1]) 
    covid.data <- select(covid.data, -1)
    covid.network <- graph_from_adjacency_matrix(data.matrix(covid.data))
    
    # Node and edge size attributes
    # Due to the sheer amount of edge weight, using a log() function
    # Even though label is set to NA, if you zoom in you can see the labels
    E(covid.network)$width <- log(E(covid.network)) / 2
    V(covid.network)$size <- 20
    V(covid.network)$label <- NA
    E(covid.network)$arrow.mode <- 0
    
    # Configures the graph layout
    # When selecting a node it updates current_node_id so both graphs can simultaneously select the same node.
    # Scale changes when zooming in and out, only for buggy hovering animation option.
    disp.graph <- visIgraph(covid.network) %>%
        visEdges(smooth = FALSE,
                 color = list(color = "black", highlight = "red")) %>%
        visNodes(shape = "square",
                 shadow = list(enabled = TRUE, size = 20),
                 color = list(highlight = list(background = "#F5E1FD", border = "#B399D4"))) %>%
        visIgraphLayout(layout = "layout.norm", layoutMatrix = layout) %>%
        visInteraction(dragNodes = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1))  %>%
        visEvents(select="function(node){
                      if(1 == 1){Shiny.onInputChange('current_node_id', node.nodes);
                      } if(node.nodes==''){Shiny.onInputChange('current_node_id', 'deselect_node')}}") %>%
        visEvents(zoom="function(zoomed){
                      Shiny.onInputChange('scale', zoomed.scale);
                      }") 
    
    return(disp.graph)
}



generate_3D_network <- function (country) {
    layer_1 <- read.csv(country[1], header=T)
    layer_2 <- read.csv(country[2], header=T)
    
    rownames(layer_1) <- layer_1[,1]
    layer_1 <- select(layer_1, -1)
    layer_1.net <- graph_from_adjacency_matrix(data.matrix(layer_1))
    
    rownames(layer_2) <- layer_2[,1]
    layer_2 <- select(layer_2, -1)
    layer_2.net <- graph_from_adjacency_matrix(data.matrix(layer_2))
    
    layer <- layout_with_fr(layer_1.net)
    coords <- rbind(cbind(layer, 0), cbind(layer, 1))
    node_count <- length(V(layer_1.net))
    colors <- c(rep("blue",node_count),rep(c("orange"), node_count))
    final.net <- igraph::union(layer_1.net, layer_2.net)
    V(final.net)$color <- colors
    graphjs(final.net, layout=coords, vertex.size=0.25)
    
}




# So layout can be initialized
disp <- reactiveValues(layout=0)

server <- function(input, output) {
    
    # Initializes layout
    observeEvent(input$country, {
        disp$layout <- generate_layout(get_country_networks(input$country)[1])
        
    })
    
    # Handles selection, deselection, and zooming events.
    observeEvent(input$current_node_id, {
        
        # Changes node being viewed
        visNetworkProxy("top_layer") %>%
            visSelectNodes(id=input$current_node_id)
        visNetworkProxy("bottom_layer") %>%
            visSelectNodes(id=input$current_node_id)
        
        if(input$current_node_id=="deselect_node") {
            visNetworkProxy("top_layer") %>%
                visSetSelection()
            visNetworkProxy("bottom_layer") %>%
                visSetSelection()
        }
        
        # Node being viewed animation, still a bit buggy
        # Optional through the tick box
        if (!is.null(input$current_node_id) && !is.null(input$scale) && input$animation) {
            visNetworkProxy("top_layer") %>%
                visFocus(id=input$current_node_id,
                         input$scale,
                         animation=list(duration=500, easingFunction="linear"))
            visNetworkProxy("bottom_layer") %>%
                visFocus(id=input$current_node_id,
                         input$scale,
                         animation=list(duration=500, easingFunction="linear"))
        }
        
        
    })
    
    output$top_layer <- renderVisNetwork({
        layer_file_name <- get_country_networks(input$country)[1]
        generate_network(layer_file_name,disp$layout)
    })
    
    output$bottom_layer <- renderVisNetwork({
        layer_file_name <- get_country_networks(input$country)[as.integer(input$layer.selected)]
        generate_network(layer_file_name,disp$layout)
    })
    
    output$otherplot <- renderScatterplotThree({
        
        layer_1 <- read.csv(get_country_networks(input$country)[1], header=T)
        layer_2 <- read.csv(get_country_networks(input$country)[2], header=T)
        
        rownames(layer_1) <- layer_1[,1]
        layer_1 <- select(layer_1, -1)
        layer_1.net <- graph_from_adjacency_matrix(data.matrix(layer_1))
        
        rownames(layer_2) <- layer_2[,1]
        layer_2 <- select(layer_2, -1)
        layer_2.net <- graph_from_adjacency_matrix(data.matrix(layer_2))
        
        layer <- layout_with_fr(layer_1.net)
        coords <- rbind(cbind(layer, 0), cbind(layer, 1))
        node_count <- length(V(layer_1.net))
        colors <- c(rep("blue",node_count),rep(c("orange"), node_count))
        final.net <- igraph::union(layer_1.net, layer_2.net)
        V(final.net)$color <- colors
        graphjs(final.net, layout=coords, vertex.size=0.25)
    })
    
}

shinyApp(ui = ui, server = server)
