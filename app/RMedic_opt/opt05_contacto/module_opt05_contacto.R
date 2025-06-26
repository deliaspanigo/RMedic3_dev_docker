module_opt05_contacto_UI <- function(id) {
  ns <- NS(id)
  
  
  div(
    tagList(
      tags$head(
        tags$script(type="text/javascript", src = "busy.js"),
        tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
        tags$script(type="text/javascript", "var switchTo5x=true"),
        tags$script(type="text/javascript",'stLight.options({publisher: "675b3562-a081-470a-9fc4-3dd6a712209d", doNotHash: true, doNotCopy: true, hashAddressBar: false})')
      )
    ),
    div(id = ns("home"),
        br(),
        fluidRow(
          column(3, img(src = "rmediclogo.jpg", width = 300, height = 300)),
          column(9,  # Pie de página
                 div(
                   class = "sidebar-footer",
                   # tags$a(fa_i("github"), href = "https://github.com/deliaspanigo/Rscience2", class = "social-icon"),
                   tags$a(fa_i("linkedin"), href = "https://www.linkedin.com/company/r-medic/", class = "social-icon", style="font-size: 20em;"),
                   # tags$a(fa_i("coffee"), href = "https://github.com/deliaspanigo/Rscience2", class = "social-icon"),
                   p("© 2025", style = "text-align: center; font-size: 0.8rem; margin-top: 10px; opacity: 0.7;")
                 )
          )
        ),
        
        br(), br(), br(), br(), br()
    ), br(), br(), br(), br(), br()
  )
  
}



module_opt05_contacto_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    


    
  })
}

