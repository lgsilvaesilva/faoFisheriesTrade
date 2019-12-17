about <- tabPanel(title = "Help", 
                  value = "about", 
                  br(), hr(),
                  # actionBttn(inputId = "methods", 
                  #            label = "Methodology", 
                  #            style = "fill", 
                  #            color = "danger", 
                  #            icon = icon("book"), size = "sm")
                  # withMathJax(includeHTML(rmarkdown::render('tabs/server/about/about.Rmd')))
                  withMathJax(includeHTML('tabs/server/about/about.html'))
                  
                  
)
