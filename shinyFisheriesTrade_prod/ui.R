shinyUI(
  
  fluidPage(
    ##-- Favicon ----
    tags$head(
      #-- biblio js ----
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://use.fontawesome.com/releases/v5.8.2/css/all.css"),
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
      
    ),
    # tags$head(tags$style(
    #   HTML("input[type='search']:disabled {visibility:hidden}")
    # )),    
    ##-- Logo ----
    list(tags$head(HTML('<link rel="icon", href="img/logo.png",
                        type="image/png" />'))),
    
    ##-- Header ----
    navbarPage(title = div(img(src="img/logo.png",
                               height = "75px"), style = "padding-left:100px;"),
               windowTitle = "Fisheries Trade",
               id = "navbar",
               selected = "build_data",
               theme = "styles.css", 
               fluid = T,
               ##-- Tabs ----
               build_data,
               mapping,
               imputation,
               outlier_detection,
               mirroring,
               raw_data,
               about
    ),
    
    ##-- Footer ----
    div(class = "footer", style = "z-index: 200;",
        includeHTML("html/footer.html")
    )
    )
  )
