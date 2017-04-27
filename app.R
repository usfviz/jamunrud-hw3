dependencies <- c('shiny', 'ggplot2', 'dplyr', 'ggvis', 'tidyr', 'pairsD3', 'reshape2')
new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages, repos="http://cran.rstudio.com/")}

fb_raw <- read.table('dataset_Facebook.csv', header = TRUE, sep = ";")
fb_raw$id <- 1:nrow(fb_raw)

fb_subset <- fb_raw[which(fb_raw$Lifetime.Post.Total.Impressions < 300000), ]

fb_long <- gather(fb_subset, Lifetime, Amount, c(8,9,13), factor_key=TRUE)
levels(fb_long$Lifetime) <- c('Reach', 'Impressions', 'Impressions (Liked Your Page)')

colnames(fb_subset) <- c("Page.total.likes", "Type", "Category", "Month", "Weekday", "Hour", "Paid",
                         "Reach", "Impressions", "Engaged Users", "Consumers", "Impressions (Liked Your Page)",
                         "Reach (Liked Your Page)", "Liked Your Page and Engaged", "Comment",
                         "Like", "Share", "Total.Interactions", "id")



ui <- fluidPage(
  navbarPage("",
    tabPanel("Parallel Coordinates",
             fluidRow(
                 h3(style= "text-align: center;", 'Lifetime Reach and Impressions by Post Type'),
                 div(style = "width:100%",
                     ggvisOutput("plot1")
                 )
               )
    ),
    tabPanel("ScatterPlot Matrix",
             fluidRow(
                 h3(style= "text-align: center;", 'Facebook Post Metrics'),
                 div(style = "margin-left:2em; width:120%; height:800px",
                     uiOutput("pairsplot")
                 )
               )
    ),
    tabPanel("Heatmap",
             fluidRow(
                 h3(style= "text-align: center;", 'Facebook Metrics Correlation'),
                 div(style = "margin-left:2em; width:100%;",
                     uiOutput("heatmap")
                 )
               )
    )
  )
)

server <- function(input, output) {
  # Parallel Coordinates Plot
  fb_long %>%
    ggvis(~Lifetime, ~Amount) %>%
    group_by(id) %>%
    layer_paths(stroke =~Type, opacity := .5, opacity.hover := 1, strokeWidth.hover := 8, strokeWidth := 2) %>%
    layer_points(fill =~Type, size := 50, size.hover := 200) %>%
    add_tooltip(function(data){
      reach = fb_long[which(fb_long$id == data$id & fb_long$Lifetime == 'Reach'), 'Amount']
      imps = fb_long[which(fb_long$id == data$id & fb_long$Lifetime == 'Impressions'), 'Amount']
      imps_like = fb_long[which(fb_long$id == data$id & fb_long$Lifetime == 'Impressions (Liked Your Page)'), 'Amount']
      paste0("<b>Reach:</b> ", prettyNum(reach, big.mark = ','), "<br>",
             "<b>Impressions:</b> ", prettyNum(imps, big.mark = ','), "<br>",
             "<b>Impressions (Liked):</b> ", prettyNum(imps_like, big.mark = ',')
      )
    }, "hover") %>%
    scale_ordinal("fill", range=c('#e41a1c','#377eb8','#4daf4a','#984ea3')) %>%
    scale_ordinal("stroke", range=c('#e41a1c','#377eb8','#4daf4a','#984ea3')) %>%
    add_axis("x", title = "Category", title_offset = 40) %>%
    add_axis("y", title = "Amount", title_offset = 60) %>%
    set_options(width = "auto", height = "auto", resizable=FALSE) %>%
    bind_shiny('plot1')

  # ScatterPlot Matrix
  output$pairsplot <- renderUI({
    pairsD3Output("pD3", width='100%', height='850px')
  })

  output$pD3 <- renderPairsD3({
    pairsD3(fb_subset[, c("Reach", "Impressions", "Engaged Users",
                          "Impressions (Liked Your Page)", "Reach (Liked Your Page)")],
            labels = c("Reach", "Impressions", "Engaged Users", "Impressions(Liked)", "Reach(Liked)"),
            group=fb_subset$Type, opacity = 0.9, cex = 3,
            col = c('#e41a1c','#377eb8','#4daf4a','#984ea3'),
            tooltip = paste0('<b>Post Type: </b>', fb_subset$Type))
  })
  
  # Heatmap
  fb_heat <- fb_subset[, c("Reach", "Impressions", "Engaged Users",
                           "Impressions (Liked Your Page)", "Reach (Liked Your Page)")]
  colnames(fb_heat) <- c("Reach", "Impressions", "Engaged Users", "Impressions(Liked)", "Reach(Liked)")
  
  fb_heat <- melt(cor(fb_heat, use="p"), value.name="Correlation")
  fb_heat$Labels <- ifelse(fb_heat$Var1 == fb_heat$Var2, as.character(fb_heat$Var1), "")
  
  fb_heat%>%
    ggvis(~Var1, ~Var2, fill= ~Correlation) %>%
    layer_rects(width = band(), height = band()) %>%
    layer_text(
      x = prop("x", ~Var1, scale = "xcenter"),
      y = prop("y", ~Var2, scale = "ycenter"),
      text := ~Labels, fontSize := 16, fill:="white", baseline:="middle", align:="center") %>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    scale_nominal("y", padding = 0, points = FALSE) %>% 
    scale_nominal("x", name = "xcenter", padding = 1, points = TRUE) %>%
    scale_nominal("y", name = "ycenter", padding = 1, points = TRUE) %>%
    scale_numeric("fill", range=c('lightblue', 'darkblue')) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    add_tooltip(function(data){
      paste0('<b>', round(data$Correlation,2), "</b>")
    }, "hover") %>%
    set_options(width = "auto", height = "600px", resizable=FALSE) %>%
    bind_shiny('heatmap')
}

# Run the application 
shinyApp(ui = ui, server = server)
