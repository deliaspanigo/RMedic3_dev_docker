module_opt99_who_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("aver"))
}



module_opt99_who_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica del servidor para la pestaña "Inicio"
    
    output$aver <- renderUI({
      
      
      
      fluidRow(
        # Columna para la Persona 1 (50% del ancho)
        column(width = 6,
               h3("Dr. Arnaldo Mangeuad"),
               img(src = "ARN.png", height = 200, width = 200),  # Imagen de la persona 1
               p("LinkedIn:", a("Perfil de LinkedIn", href = "https://www.linkedin.com/in/arnaldo-mangeaud-565877108//")),
               p("Correo: amangeaud@yahoo.com.ar"),
               p("- Asesor estadístico con más de 25 años de experiencia."),
               p("- Gran trayectoria en docencia universitaria en grado y posgrado en ciencias de la salud.")#,
               
               # h4("Habilidades"),
               # p("- Habilidad 1"),
               # p("- Habilidad 2"),
               # p("- Habilidad 3")
        ),
        
        # Columna para la Persona 2 (50% del ancho)
        column(width = 6,
               h3("Mgter. David Elías Panigo"),
               img(src = "DAVID.png", height = 200, width = 200),  # Imagen de la persona 1
               p("LinkedIn:", a("Perfil de LinkedIn", href = "https://www.linkedin.com/in/deliaspanigo/")),
               p("Correo: d.eliaspanigo@gmail.com"),
               p("- Desarrollador R/Shiny y Python"),
               p("- Asesor estadístico con 15 años de experiencia.")
        )
      )
      
      
    })
    
   
  })
}

