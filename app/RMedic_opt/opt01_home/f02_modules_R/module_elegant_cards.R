# UI del módulo
module_elegant_cards_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .card-custom {
          transition: transform 0.3s ease, box-shadow 0.3s ease;
          margin-bottom: 20px;
        }
        .card-custom:hover {
          transform: translateY(-8px);
          box-shadow: 0 12px 24px rgba(0,0,0,0.25) !important;
        }
        .card-header-custom {
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 1.3rem;
          letter-spacing: 0.5px;
          text-transform: uppercase;
        }
        .card-icon {
          margin-right: 10px;
          font-size: 1.5rem;
        }
        .content-paragraph {
          border-left: 3px solid rgba(0,0,0,0.1);
          padding-left: 15px;
          margin: 15px 0;
        }
      "))
    ),
    h1(
      "Análisis estadístico para Ciencias de la Salud",
      style = "text-align: center; margin: 30px 0; padding-bottom: 15px; border-bottom: 2px solid #ddd;"
    ),
    layout_columns(
      col_width = 4,
      
      # Tarjeta 1
      card(
        class = "card-custom",
        full_screen = FALSE,
        height = "100%",
        card_header(
          div(
            class = "card-header-custom",
            fa_i("gem", fill = "currentColor", class = "card-icon"),
            "RMedic"
          ),
          style = "background: linear-gradient(135deg, #6e8efb, #4287f5);
                  color: white;
                  border: none;
                  border-radius: 20px 20px 0 0;
                  font-weight: 500;
                  padding: 20px;
                  text-shadow: 1px 1px 2px rgba(0,0,0,0.2);"
        ),
        style = "background: linear-gradient(to bottom, #f0f7ff, white);
                border: 2px solid #4287f5; /* Más ancho y color */
                border-radius: 20px;
                box-shadow: 0 8px 16px rgba(66, 135, 245, 0.3);",
        p(class = "content-paragraph", "Asesor estadístico automatizado para el análisis de datos médicos."),
        p(class = "content-paragraph", "Estandarización en el proceso de análsis estadísticos e interpretación.")
      ),
      
      # Tarjeta 2
      card(
        class = "card-custom",
        full_screen = FALSE,
        height = "100%",
        card_header(
          div(
            class = "card-header-custom",
            fa_i("leaf", fill = "currentColor", class = "card-icon"),
            "Data Análisis"
          ),
          style = "background: linear-gradient(135deg, #56ab2f, #42f565);
                  color: white;
                  border: none;
                  border-radius: 20px 20px 0 0;
                  font-weight: 500;
                  padding: 20px;
                  text-shadow: 1px 1px 2px rgba(0,0,0,0.2);"
        ),
        style = "background: linear-gradient(to bottom, #f0fff0, white);
                border: 2px solid #42f565; /* Más ancho y color */
                border-radius: 20px;
                box-shadow: 0 8px 16px rgba(66, 245, 101, 0.3);",
        p(class = "content-paragraph", "Estandarización en el proceso de análsis."),
        p(class = "content-paragraph", "Facilidades para la obtención de tablas, gráficos, análisis estadísticos e interpretación.")
      ),
      
      # Tarjeta 3
      card(
        class = "card-custom",
        full_screen = FALSE,
        height = "100%",
        card_header(
          div(
            class = "card-header-custom",
            fa_i("bolt", fill = "currentColor", class = "card-icon"),
            "Servicios"
          ),
          style = "background: linear-gradient(135deg, #ffb347, #f5d742);
                  color: white;
                  border: none;
                  border-radius: 20px 20px 0 0;
                  font-weight: 500;
                  padding: 20px;
                  text-shadow: 1px 1px 2px rgba(0,0,0,0.2);"
        ),
        style = "background: linear-gradient(to bottom, #fffcf0, white);
                border: 2px solid #f5d742; /* Más ancho y color */
                border-radius: 20px;
                box-shadow: 0 8px 16px rgba(245, 215, 66, 0.3);",
        p(class = "content-paragraph", ""),
        p(class = "content-paragraph", "Monitoreo proactivo del rendimiento y ajuste automático de recursos.")
      )
    )
  )
}


# Server del módulo
module_elegant_cards_SERVER <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # No se requiere lógica del servidor para esta aplicación
    }
  )
}