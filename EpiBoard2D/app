library(shiny)
library(shinydashboard)
library(igraph)
library(ggraph)
library(dplyr)
library(visNetwork)
library(stringi)

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
        visNetworkOutput("top_layer"), # static (I assume the first file is the genetic layer)
        visNetworkOutput("bottom_layer") # changable via menu
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
        print(input$layer.selected)
        layer_file_name <- get_country_networks(input$country)[as.integer(input$layer.selected)]
        generate_network(layer_file_name,disp$layout)
    })
    
}

shinyApp(ui = ui, server = server)
