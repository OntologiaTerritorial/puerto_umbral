# modules/ergo_drawer.R
# Module for Collapsible Field Ergonomics Drawer (Light slate theme, high-contrast, escaped unicode)

ergo_drawer_ui <- function() {
  uiOutput("right_ergo_drawer_ui")
}

ergo_server <- function(input, output, session, ergo_open, lang) {
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  output$right_ergo_drawer_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    
    climates <- if (is_en) {
      list("Temperate / Urban" = "temperate", "Subpolar / Cold" = "subpolar", "Tropical / Humid" = "tropical", "Arid / Desert" = "arid")
    } else {
      list("Templado / Urbano" = "temperate", "Subpolar / Fr\u00edo" = "subpolar", "Tropical / H\u00famedo" = "tropical", "\u00c1rido / Desierto" = "arid")
    }
    
    selected_climate <- if (!is.null(input$ergo_climate)) input$ergo_climate else "temperate"
    
    # Estilo din\u00e1mico adaptado al fondo claro con acento en borde lateral izquierdo seg\u00fan el clima de campa\u00f1a
    bg_style <- switch(selected_climate,
      "subpolar" = "background: rgba(255, 255, 255, 0.98); border-left: 5px solid #0284c7;",
      "tropical" = "background: rgba(255, 255, 255, 0.98); border-left: 5px solid #10b981;",
      "arid"      = "background: rgba(255, 255, 255, 0.98); border-left: 5px solid #b45309;",
      "temperate" = "background: rgba(255, 255, 255, 0.98); border-left: 5px solid #6366f1;"
    )
    
    tactics <- if (is_en) {
      switch(selected_climate,
        "subpolar" = tags$ul(
          tags$li(tags$b("Thermal Isolation: "), "Keep smartphones in inner pockets next to body heat. Li-ion batteries drop capacity by 40% under 0\u00b0C."),
          tags$li(tags$b("Experimental Blindness: "), "The pedestrian observer must NOT see the computed active geodesics during campaign collection."),
          tags$li(tags$b("Stochastic Frost: "), "Ice increases step friction. Apply safety factor kappa = +0.25 to BVP parameters.")
        ),
        "tropical" = tags$ul(
          tags$li(tags$b("Condensation Shield: "), "Keep hardware inside hermetic dry-bags. Lens fogging degrades camera calibration."),
          tags$li(tags$b("Battery Overheating: "), "Avoid direct tropical sun. Thermal sensors block telemetry ingestion or cause battery explosions if overheated."),
          tags$li(tags$b("Path Obstruction: "), "Mud deforms the metric tensor. Increment spatial stiffness parameter lambda.")
        ),
        "arid" = tags$ul(
          tags$li(tags$b("Thermal Overheating: "), "Limit continuous screen brightness. CPUs throttle or shut down under direct solar glare. Keep backup devices in shade."),
          tags$li(tags$b("Dust Protection: "), "Cover device ports. Fine sand particles cause touch screen lag."),
          tags$li(tags$b("Hydration Vector: "), "Pedestrians deviate towards shade attractors. Manually insert high magnitude 'shade' distortions.")
        ),
        "temperate" = tags$ul(
          tags$li(tags$b("Signal Multipath: "), "Tall concrete buildings distort GPS. Run double loops for IEO validation."),
          tags$li(tags$b("Urban Autonomy: "), "Standard batteries perform in typical ranges. Keep backlight at 70%."),
          tags$li(tags$b("Solo Mode: "), "If surveying alone, use 'Solo Mode' to block active layers from display.")
        )
      )
    } else {
      switch(selected_climate,
        "subpolar" = tags$ul(
          tags$li(tags$b("Aislamiento T\u00e9rmico: "), "Mantenga los celulares en bolsillos internos. Las bater\u00edas de litio pierden 40% de carga bajo 0\u00b0C."),
          tags$li(tags$b("Ceguera Experimental: "), "El observador peatonal del IEO no debe ver las geod\u00e9sicas te\u00f3ricas durante el levantamiento."),
          tags$li(tags$b("Escarcha Estoc\u00e1stica: "), "El hielo incrementa la fricci\u00f3n. Aplique factor de seguridad kappa = +0.25 en los par\u00e1metros BVP.")
        ),
        "tropical" = tags$ul(
          tags$li(tags$b("Escudo de Condensaci\u00f3n: "), "Guarde los equipos en bolsas herm\u00e9ticas secas. El empa\u00f1amiento de lentes da\u00f1a la calibraci\u00f3n."),
          tags$li(tags$b("Sobrecalentamiento de Bater\u00edas: "), "Evite sol tropical directo. Temperaturas extremas causan fallas o explosiones por fatiga t\u00e9rmica."),
          tags$li(tags$b("Obstrucci\u00f3n de V\u00eda: "), "El barro deforma el tensor m\u00e9trico. Incremente la rigidez del manifold (par\u00e1metro lambda).")
        ),
        "arid" = tags$ul(
          tags$li(tags$b("Sobrecalentamiento T\u00e9rmico: "), "Limite el brillo continuo de pantalla. Los procesadores fallan bajo radiaci\u00f3n directa. Resguarde equipos en la sombra."),
          tags$li(tags$b("Protecci\u00f3n de Polvo: "), "Cubra los puertos de carga. La arena fina genera retardos en pantallas capacitivas."),
          tags$li(tags$b("Vector de Hidrataci\u00f3n: "), "Los peatones se desv\u00edan a atractores de sombra. Inserte distorsiones de sombra de gran magnitud.")
        ),
        "temperate" = tags$ul(
          tags$li(tags$b("Multitrayectoria de Se\u00f1al: "), "Edificios altos distorsionan el GPS. Realice doble pasada para validar el IEO."),
          tags$li(tags$b("Autonom\u00eda Urbana: "), "Bater\u00edas funcionan en rangos normales. Mantenga el brillo de pantalla en 70%."),
          tags$li(tags$b("Modo Solo: "), "Si levanta datos en solitario, active 'Modo Solo' para bloquear la visualizaci\u00f3n de capas activas.")
        )
      )
    }
    
    div(id = "right_ergo_drawer", class = "sidebar-right", style = bg_style,
      div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:20px;",
        h3(style = "margin:0; font-size:1.3rem !important; color:#0284c7; font-weight:700;", if (is_en) "Field Ergonomics" else "Ergonom\u00eda de Terreno"),
        actionButton("close_ergo", "X", class = "btn-danger btn-xs", style = "padding:4px 10px; font-weight:bold; font-size:1rem;")
      ),
      
      div(style = "margin-bottom: 20px;",
        selectInput("ergo_climate", if (is_en) "Campaign Climate:" else "Clima de Campa\u00f1a:",
                    choices = climates, selected = selected_climate, width = "100%")
      ),
      
      div(style = "margin-bottom:20px; display:flex; flex-direction:column; gap:10px;",
        checkboxInput("mode_field", if (is_en) "Activate Field Mode (High Contrast)" else "Activar Modo Campo (Alto Contraste)", value = FALSE),
        checkboxInput("mode_solo_blind", if (is_en) "Activate Solo Mode (Auto-Blindness)" else "Activar Modo Solo (Auto-Ceguera)", value = FALSE)
      ),
      
      tags$hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1); margin: 15px 0;"),
      
      h4(style = "color:#b45309; margin-top:0; font-size:1.1rem !important; font-weight:600;", if (is_en) "Tactical Field Rubric" else "R\u00fabrica T\u00e1ctica de Terreno"),
      div(style = "font-size:0.95rem; line-height:1.5; color:#334155; margin-bottom: 15px;", tactics),
      
      tags$hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1); margin: 15px 0;"),
      
      h4(style = "margin-top:0; font-size:1.1rem !important; font-weight:600; color:#1e293b;", if (is_en) "Campaign Downloads" else "Descargas de Campa\u00f1a"),
      div(style = "display:flex; flex-direction:column; gap:10px; margin-top:10px;",
        downloadButton("download_ieo_pdf", if (is_en) "Offline IEO Sheet (TXT)" else "Ficha IEO (TXT) Offline", class = "btn-block btn-info btn-lg", style="width:100%; font-weight:bold; display:block;"),
        downloadButton("download_csv_template", if (is_en) "Data Template (CSV)" else "Plantilla Datos (CSV)", class = "btn-block btn-info btn-lg", style="width:100%; font-weight:bold; display:block;")
      )
    )
  })
  
  observe({
    # Alternar din\u00e1micamente la clase "open" con shinyjs para no destruir checkboxes
    shinyjs::toggleClass(id = "right_ergo_drawer", class = "open", condition = ergo_open())
    shinyjs::toggleClass(id = "toggle_ergo", class = "drawer-open", condition = ergo_open())
  })
  
  observeEvent(input$toggle_ergo, { ergo_open(!ergo_open()) })
  observeEvent(input$close_ergo, { ergo_open(FALSE) })
}
