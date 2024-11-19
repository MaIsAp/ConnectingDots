install.packages(c("shiny","googlesheets4", "dplyr","stringr", "visNetwork","DT"),repos = "http://cran.us.r-project.org")

library(shiny)
library(googlesheets4)
library(dplyr)
library(stringr)
library(visNetwork)
library(DT)

# Conect to Google Sheets (need to authorize in Google account)
# sheets_deauth() to disconnect and reconnect
gs4_auth(cache = ".secrets", email="mi.arceplata@gmail.com")  # will ask for authentication the first time

# Google Sheet ID (within URL of sheet)
sheet_id <- "1btm5PkkWzy8pDbg1_-lNuMge8cZN6AHKboU0Wg1ztSo"

# Load data from Google Sheets
load_data <- function() {
  edges <- read_sheet(sheet_id,sheet="edges_new")#read.csv("input_matrix - Hoja 4.csv")#
  nodes <- read_sheet(sheet_id,sheet="nodes_new")#read.csv("input_matrix - Hoja 3.csv")#
  # edges <- read_sheet(sheet_id,sheet=4)#read.csv("input_matrix - Hoja 4.csv")#
  # nodes <- read_sheet(sheet_id,sheet=3)#read.csv("input_matrix - Hoja 3.csv")#
  return(list(edges,nodes))
}

# UI of app
ui <- fluidPage(
  titlePanel("Connecting the dots"),
  
  sidebarPanel(
    checkboxGroupInput("filter_elements", 
                       "Choose elements:",
                       choices = c("goal","target","category","headline","binary","component","complementary"),
                       selected = c("goal","target","category","headline","binary"))
  ),
  
  fluidRow(
    column(12,
           visNetworkOutput("mynetworkid", height = "800px"))
  ),
  
  fluidRow(
    column(12,
           actionButton("refresh", "Update data"))
  ),
  
  fluidRow(
    column(12,
           dataTableOutput('table')
    )
  )
)

# Server of app
server <- function(input, output, session) {
  
  # load data when pressing button
  data <- reactive({
    input$refresh
    isolate({
      load_data()
    })
  })
  
  # create network graph
  output$mynetworkid <- renderVisNetwork({
    nodes <- data()[[2]]
    edges <- data()[[1]]
    
    req(input$filter_elements)
    filter_indicator <- input$filter_elements
    
    vis.nodes <- nodes |> filter(type %in% filter_indicator)
    vis.links <- edges |> filter(from %in% vis.nodes$id &
                                   to %in% vis.nodes$id)
    vis.links$color <- factor(vis.links$source,labels=c("#d4cf9a",
                                                        "#9cc0ed",
                                                        "#ebaea7",
                                                        "#70d3e3",
                                                        "#d8b3e1",
                                                        "#9fdbbb"))
    vis.links$source <- str_wrap(vis.links$source,width=50)
    
    vis.nodes$shadow <- TRUE # Nodes will drop shadow
    vis.nodes$borderWidth <- 2 # Node border width
    
    vis.nodes$color <- str_replace_all(vis.nodes$type,c("category"= "#56ae6c", "goal"= "#b54f90","target"= "#7066bc", 
                                                        "headline"="#ba6437", "binary"="#ba6437", "component"="#af953c", "complementary"="#af953c"))
    vis.nodes$shape <- str_replace_all(vis.nodes$type,c("category"= "square", "goal"= "diamond", "target"= "diamond",
                                                        "headline"="dot", "binary"="dot", "component"="dot", "complementary"="dot"))
    
    count_goals_targets <- vis.links |> count(from) |> rename(id=from,value=n) |> mutate(id=as.character(id))
    count_indic_categories <- vis.links |> count(to) |> rename(id=to,value=n) |> mutate(id=as.character(id))

    count_nodes <- count_goals_targets |> bind_rows(count_indic_categories)
    count_nodes2 <- count_nodes |> group_by(id) |> summarise(n=sum(value))
    
    vis.nodes2 <- vis.nodes |> mutate(id=as.character(id)) |> 
      left_join(count_nodes2 , by="id" ) |> mutate(value=case_when(type=="category" ~ n/2,
                                                                   TRUE ~n))
    
    visNetwork(vis.nodes2, vis.links) |>
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |> #, manipulation = TRUE) |> 
      visLegend(useGroups = FALSE, addNodes = data.frame(label = c("Category","Goal","Target","Headline/Binary","Component/Complementary"), 
                                                         shape = c("square","diamond","diamond","dot","dot"),
                                                         color=c("#56ae6c","#b54f90","#7066bc","#ba6437","#af953c")),
                addEdges = data.frame(color=unique(vis.links$color),
                                      label=unique(vis.links$source),
                                      font.align = "bottom"))# |> 
      #visEdges(color=list(unique(vis.links$color)))
  
  })
  
  # insert table for targets and goals
  output$table <- renderDataTable({
    descriptions <- data()[[2]]
    # descriptions
    descriptions |>
      dplyr::filter(type!="category") |>
    #   # dplyr::filter(type == "goal" | type == "target") |> #!!!!!!!!!!!!!!!!!!!!!!
      dplyr::select(-id)
  })
}

# deploy app
shinyApp(ui, server)
