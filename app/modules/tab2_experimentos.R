# modules/tab2_experimentos.R
# Module for Experiments Portal Tab

tab2_ui <- function() {
  tabPanel("L\u00edneas de Trabajo",
           uiOutput("tab2_portal_ui")
  )
}

tab2_server <- function(input, output, session, active_exp_line, lang, run_sim_trigger) {
  
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  # Reactive database query for reference simulated data of the selected experiment
  active_pixels <- reactive({
    sel_line <- active_exp_line() # 1 to 7
    db_p <- "geotensor_experimentos.db"
    if (!file.exists(db_p)) db_p <- "app/geotensor_experimentos.db"
    
    if (file.exists(db_p)) {
      conn <- dbConnect(SQLite(), dbname = db_p)
      # Ingest coordinates and default attributes
      df <- dbGetQuery(conn, sprintf("SELECT id, x, y, altitud, ndvi, red_cuidado FROM pixeles WHERE experimento_id = %s", sel_line))
      dbDisconnect(conn)
      return(df)
    }
    return(NULL)
  })
  
  # Render the data preview table
  output$exp_data_preview <- renderTable({
    df <- active_pixels()
    if (!is.null(df) && nrow(df) > 0) {
      head(df, 8)
    } else {
      data.frame(Mensaje = "No hay datos de referencia disponibles.")
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  output$tab2_portal_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    sel_line <- as.character(active_exp_line())
    
    # 7 Experiments Configuration
    titles <- list(
      "1" = trans("1. Refracci\u00f3n de Borde (Snell)", "1. Edge Refraction (Snell)"),
      "2" = trans("2. Desviaci\u00f3n Geod\u00e9sica (Exclusi\u00f3n)", "2. Geodesic Deviation (Exclusion)"),
      "3" = trans("3. Autopoiesis Colectiva (Atractores)", "3. Collective Autopoiesis (Attractors)"),
      "4" = trans("4. Memoria del Trauma (Caputo)", "4. Trauma Memory (Caputo)"),
      "5" = trans("5. Regularizaci\u00f3n de Moran (Ledoit-Wolf)", "5. Moran Regularization (Ledoit-Wolf)"),
      "6" = trans("6. Fronteras Ecol\u00f3gicas (Robin)", "6. Ecological Boundaries (Robin)"),
      "7" = trans("7. Refracci\u00f3n de Capital (Harvey)", "7. Capital Refraction (Harvey)")
    )
    
    # Create the horizontal selector cards
    selectors <- lapply(names(titles), function(num) {
      is_active <- (num == sel_line)
      card_class <- if (is_active) "experiment-card active" else "experiment-card"
      column(4, style = "margin-bottom:10px;",
        div(class = card_class,
            onclick = sprintf("Shiny.setInputValue('select_line', '%s');", num),
            tags$span(style = "color:#0284c7; font-weight:600; font-size:0.85rem; display:block;", 
                      trans(paste("L\u00ednea / Line", num), paste("Line", num))),
            tags$span(style = "font-weight:500; font-size:0.9rem; color:#0f172a;", titles[[num]])
        )
      )
    })
    
    # Active experiment details
    detail_content <- switch(sel_line,
      "1" = list(
        title = titles[1],
        desc = trans("Mide c\u00f3mo el salto abrupto de m\u00e9tricas de seguridad e infraestructura entre dos zonas (borde urbano/rural) desv\u00eda el \u00e1ngulo de la trayectoria peatonal al cruzar la frontera, siguiendo la ley de refracci\u00f3n.",
                     "Measures how the abrupt jump in safety and infrastructure metrics between two zones (peri-urban borders) deflects the pedestrian path angle upon crossing, following the refraction law."),
        formula = "$$\\frac{\\sin \\theta_1}{\\sin \\theta_2} = \\frac{v_1}{v_2} = \\frac{\\eta_2}{\\eta_1}$$",
        img = "images/refraction_didactic.png",
        exp_mode_val = "exp1"
      ),
      "2" = list(
        title = titles[2],
        desc = trans("Modela la segregaci\u00f3n urbana. Las geod\u00e9sicas te\u00f3ricas que pasan cerca de repulsores de exclusi\u00f3n (delitos, barreras) se desv\u00edan de forma acelerada, creando brechas geom\u00e9tricas de aislamiento.",
                     "Models spatial segregation. Theoretical geodesics passing near exclusion repulsors (crime zones, barriers) deviate, creating geometric isolation gaps."),
        formula = "$$\\frac{D^2 J^i}{ds^2} + R^i_{\\;jkl} T^j T^k J^l = 0$$",
        img = "images/refraction_didactic.png", # fallback
        exp_mode_val = "exp2"
      ),
      "3" = list(
        title = titles[3],
        desc = trans("Analiza la autopoiesis comunitaria. La organizaci\u00f3n social (P) act\u00faa como una presi\u00f3n local que, al cruzar un umbral (P_crit), invierte la matriz Hessiana local, convirtiendo un repulsor de exclusi\u00f3n en un atractor de cuidados.",
                     "Analyzes community autopoiesis. Social organization (P) acts as a local pressure that, when crossing a threshold (P_crit), inverts the local Hessian matrix, turning a repulsive zone of exclusion into a caring attractor."),
        formula = "$$H_{ij}(V) < 0 \\quad (P \\ge P_{crit})$$",
        img = "images/hessian_didactic.png",
        exp_mode_val = "exp3"
      ),
      "4" = list(
        title = titles[4],
        desc = trans("Estudia la memoria colectiva. Los hitos traum\u00e1ticos en el suelo generan deformaciones locales. La velocidad del peat\u00f3n decae de acuerdo con una ventana de memoria no markoviana modelada por la derivada fraccionaria de Caputo.",
                     "Studies collective memory. Traumatic events on the ground create local deformations. Pedestrian velocity decays according to a non-Markovian memory window modeled by a Caputo fractional derivative."),
        formula = "$${}^C D^\\alpha V(t) = -\\omega V(t)$$",
        img = "images/memory_didactic.png",
        exp_mode_val = "exp4"
      ),
      "5" = list(
        title = titles[5],
        desc = trans("Modela el muestreo limitado. El muestreo geoestad\u00edstico en campa\u00f1as entrega datos escasos (N=50). La regularizaci\u00f3n de Ledoit-Wolf suaviza la covarianza m\u00e9trica, evitando singularidades por divisi\u00f3n por cero.",
                     "Models limited sampling. Geostatistical sampling in campaigns yields sparse data (N=50). Ledoit-Wolf shrinkage regularizes the metric covariance matrix, avoiding division-by-zero singularities."),
        formula = "$$\\Sigma_{LW} = \\rho \\mu I + (1-\\rho) S$$",
        img = "images/ledoit_didactic.png",
        exp_mode_val = "exp5"
      ),
      "6" = list(
        title = titles[6],
        desc = trans("Modela la protecci\u00f3n del corredor ecol\u00f3gico. Los l\u00edmites asim\u00e9tricos y la frontera de Robin impiden la propagaci\u00f3n de la fricci\u00f3n urbana hacia el santuario natural de la Quebrada de Macul, resguardando flujos de conservaci\u00f3n.",
                     "Models ecological corridor protection. Asymmetric Robin boundaries prevent the sprawl of urban friction into the Quebrada de Macul sanctuary, protecting conservation flows."),
        formula = "$$\\alpha V + \\beta \\frac{\\partial V}{\\partial n} = \\gamma$$",
        img = "images/hessian_didactic.png", # fallback
        exp_mode_val = "exp6"
      ),
      "7" = list(
        title = titles[7],
        desc = trans("Modela flujos financieros e inmobiliarios. Los flujos de capital y el valor del suelo act\u00faan como una deformaci\u00f3n de la m\u00e9trica que succiona la plusval\u00eda de la periferia campesina periurbana de Pe\u00f1alol\u00e9n.",
                     "Models real estate financial flows. Capital flows and land prices act as a metric warp that sucks financial surplus value from the peri-urban peasant periphery of Pe\u00f1alol\u00e9n."),
        formula = "$$g_{ij} \\rightarrow R_{ratio} \\cdot g_{ij}$$",
        img = "images/refraction_didactic.png", # fallback
        exp_mode_val = "exp7"
      )
    )
    
    fluidRow(
      column(12,
        div(class = "panel-glass", style = "margin-bottom: 20px;",
          h4(style = "color:#fbbf24; margin-top:0;", trans("Seleccione Experimento y L\u00ednea de Trabajo:", "Select Experiment & Research Line:")),
          fluidRow(style = "margin-bottom:10px;", tagList(selectors))
        )
      ),
      
      # Left Column: Experiment Details & Caching Data
      column(7,
        div(class = "panel-glass", style = "min-height:550px; padding:20px; display: flex; flex-direction: column; justify-content: space-between;",
          div(
            h3(style = "color:#38bdf8; margin-top:0;", detail_content$title),
            tags$p(style = "font-size:1.0rem; line-height:1.6; color:#334155;", detail_content$desc),
            
            div(style = "background:rgba(0,0,0,0.3); border:1px solid rgba(255,255,255,0.05); border-radius:8px; padding:15px; text-align:center; margin:15px 0;",
              withMathJax(detail_content$formula)
            ),
            
            # Interactive Tabset Panel to toggle between Illustration and Data Preview
            tabsetPanel(id = "exp_content_tabs",
              tabPanel(trans("Ilustraci\u00f3n Te\u00f3rica", "Theoretical Illustration"),
                div(style = "text-align:center; padding: 15px 0;",
                  tags$img(src = detail_content$img, 
                           style = "width: 100%; max-width: 540px; height: auto; border:1px solid rgba(255,255,255,0.1); border-radius:8px; box-shadow:0 6px 15px rgba(0,0,0,0.45);")
                )
              ),
              tabPanel(trans("Datos de Referencia (Simulados)", "Reference Data (Simulated)"),
                div(style = "padding: 15px 0; font-size: 0.8rem; color: #334155;",
                  tags$p(style = "color: #475569; font-style: italic; font-size: 0.8rem; margin-bottom: 10px;",
                         trans("Estructura de variables en la base de datos local para este experimento:",
                               "Variable structure inside the local database for this experiment:")),
                  tableOutput("exp_data_preview")
                )
              )
            )
          ),
          
          div(style = "margin-top: 15px;",
            tags$hr(style = "border-top:1px solid rgba(255,255,255,0.08); margin: 10px 0;"),
            actionButton("go_to_sim_from_exp", trans("Cargar e Iniciar en Simulador ->", "Load & Start in Simulator ->"), class = "btn-success w-100", style = "font-weight:600; padding:10px; font-size: 0.95rem;")
          )
        )
      ),
      
      # Right Column: Analytical Validation
      column(5,
        div(class = "panel-glass", style = "min-height:550px; padding:20px;",
          h3(style = "color:#10b981; margin-top:0; margin-bottom:10px;",
             trans("Validaci\u00f3n Anal\u00edtica Post-Terreno", "Analytical Post-Field Validation")),
          tags$p(style = "font-size:0.9rem; line-height:1.45; color:#475569;",
                 trans("Motor de falsaci\u00f3n y verificaci\u00f3n: Cargue los datos CSV recolectados en terreno para contrastar la fricci\u00f3n observada con el modelo te\u00f3rico y calcular el ajuste de regresaci\u00f3n.",
                       "Falsification and verification engine: Upload CSV campaign data collected in the field to contrast observed friction against the theoretical model and compute the regression fit.")),
          
          div(style = "background: rgba(16, 185, 129, 0.05); border: 1px solid rgba(16, 185, 129, 0.2); border-radius: 8px; padding: 12px; margin-bottom:15px; margin-top:15px;",
            fileInput("ieo_file", trans("Cargar Campa\u00f1a de Terreno (CSV):", "Upload Field Campaign Data (CSV):"), accept = c(".csv"))
          ),
          
          plotlyOutput("ieo_contrast_plot", height = "280px"),
          uiOutput("ieo_contrast_metrics"),
          
          tags$hr(style = "border-top:1px solid rgba(15, 23, 42, 0.08); margin: 15px 0;"),
          h4(style = "color:#b45309; margin-top:0; margin-bottom:8px;",
             trans("Agente Auditor Vecinal (Ap\u00e9ndice D)", "Neighborhood Audit Agent (Appendix D)")),
          tags$p(style = "font-size:0.8rem; line-height:1.35; color:#64748b;",
                 trans("Audita la calidad estad\u00edstica y de privacidad (CARE/FAIR) de la base de datos de esta campa\u00f1a.",
                       "Audits the statistical and privacy (CARE/FAIR) quality of the active database campaign.")),
          
          div(style = "background: rgba(180, 83, 9, 0.03); border: 1px solid rgba(180, 83, 9, 0.1); border-radius: 8px; padding: 12px; margin-bottom: 12px; font-size:0.85rem; color:#334155;",
            htmlOutput("auditor_report_output")
          ),
          
          div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton("run_auditoria", trans("Ejecutar Auditor\u00eda", "Run Audit"), class = "btn-info btn-xs", style = "font-size:0.75rem; padding:4px 10px;"),
            actionButton("auditor_ignore", trans("Ignorar Ruido", "Ignore Noise"), class = "btn-secondary btn-xs", style = "font-size:0.75rem; padding:4px 10px;"),
            actionButton("auditor_reset", trans("Restablecer", "Reset"), class = "btn-danger btn-xs", style = "font-size:0.75rem; padding:4px 10px;")
          ),
          downloadButton("download_care_guide_direct", trans("Descargar Gu\u00eda de Gobernanza CARE/FAIR", "Download CARE/FAIR Governance Guide"), class = "btn-warning btn-xs w-100", style = "font-size:0.8rem; font-weight:600; padding:6px; margin-top:12px; display:block; text-align:center;")
        )
      )
    )
  })
  
  # ---- SERVIDOR DEL AGENTE AUDITOR ----
  auditor_threshold <- reactiveVal(1.96) # Umbral inicial (nivel profesional avanzado)
  auditor_report <- reactiveVal(NULL)
  
  # Cargar umbral persistente al inicio de la sesion
  observe({
    cfg_path <- "www/data/auditor_config.json"
    if (!file.exists(cfg_path)) cfg_path <- "app/www/data/auditor_config.json"
    if (file.exists(cfg_path)) {
      tryCatch({
        cfg <- jsonlite::fromJSON(cfg_path)
        if (!is.null(cfg$threshold)) {
          auditor_threshold(cfg$threshold)
        }
      }, error = function(e) NULL)
    }
  })
  
  save_auditor_config <- function(thresh) {
    cfg_path <- "www/data/auditor_config.json"
    if (!file.exists(cfg_path)) cfg_path <- "app/www/data/auditor_config.json"
    tryCatch({
      jsonlite::write_json(list(threshold = thresh), cfg_path)
    }, error = function(e) NULL)
  }
  
  run_audit <- function() {
    is_en <- identical(lang(), "EN")
    df <- active_pixels()
    
    if (is.null(df) || nrow(df) == 0) {
      msg <- if (is_en) "No data loaded to audit." else "No hay datos cargados para auditar."
      auditor_report(msg)
      return()
    }
    
    # 1. Auditoria de Privacidad (CARE/FAIR)
    has_meta <- "metadata" %in% names(df) || "user_id" %in% names(df) || ("experimento_id" %in% names(df) && any(df$experimento_id == 4 && nrow(df) < 50))
    
    # 2. Auditoria Estadistica (Anomalias de Altitud/Cuidado)
    alt_num <- suppressWarnings(as.numeric(df$altitud))
    
    # Convertir red_cuidado a numérico silenciosamente
    care_num <- suppressWarnings(as.numeric(df$red_cuidado))
    if (all(is.na(care_num))) {
      care_num <- ifelse(df$red_cuidado == "Ninguno", 0, 1)
    }
    
    z_alt <- (alt_num - mean(alt_num, na.rm = TRUE)) / (sd(alt_num, na.rm = TRUE) + 1e-5)
    z_care <- (care_num - mean(care_num, na.rm = TRUE)) / (sd(care_num, na.rm = TRUE) + 1e-5)
    
    thresh <- auditor_threshold()
    outliers_alt <- sum(abs(z_alt) > thresh, na.rm = TRUE)
    outliers_care <- sum(abs(z_care) > thresh, na.rm = TRUE)
    
    if (outliers_alt == 0 && outliers_care == 0 && !has_meta) {
      verdict <- if (is_en) {
        paste0("<b>Status: <span style='color:#0f766e;'>APPROVED</span></b><br/>",
               "The Neighborhood Audit Agent has inspected the active campaign dataset.<br/>",
               "\u2022 No privacy risk metadata found (FAIR/CARE compliant).<br/>",
               "\u2022 No statistical anomalies detected at current sensitivity (Z-threshold = ", round(thresh, 2), ").")
      } else {
        paste0("<b>Estado: <span style='color:#0f766e;'>APROBADO</span></b><br/>",
               "El Agente Auditor Vecinal ha inspeccionado la campa\u00f1a activa.<br/>",
               "\u2022 No se detectaron metadatos de riesgo (Cumple CARE/FAIR).<br/>",
               "\u2022 No hay anomal\u00edas estad\u00edsticas al nivel de sensibilidad actual (Z = ", round(thresh, 2), ").")
      }
    } else {
      verdict <- if (is_en) {
        paste0("<b>Status: <span style='color:#b45309;'>WARNING</span></b><br/>",
               "The Neighborhood Audit Agent flagged potential issues:<br/>",
               if (has_meta) "\u2022 <span style='color:#dc2626;'>CRITICAL:</span> Raw metadata identified. Please apply SDE masking.<br/>" else "",
               if (outliers_alt > 0) paste0("\u2022 Found ", outliers_alt, " spatial altitude outliers (Z > ", round(thresh, 2), ").<br/>") else "",
               if (outliers_care > 0) paste0("\u2022 Found ", outliers_care, " care density outliers (Z > ", round(thresh, 2), ").<br/>") else "",
               "<i style='font-size:0.8rem;'>Do you want the agent to learn and adapt to this local variance?</i>")
      } else {
        paste0("<b>Estado: <span style='color:#b45309;'>ADVERTENCIA</span></b><br/>",
               "El Agente Auditor Vecinal ha detectado posibles problemas:<br/>",
               if (has_meta) "\u2022 <span style='color:#dc2626;'>CR\u00cdTICO:</span> Metadatos crudos expuestos. Aplique enmascaramiento SDE.<br/>" else "",
               if (outliers_alt > 0) paste0("\u2022 Se detectaron ", outliers_alt, " anomal\u00edas de altitud espacial (Z > ", round(thresh, 2), ").<br/>") else "",
               if (outliers_care > 0) paste0("\u2022 Se detectaron ", outliers_care, " anomal\u00edas de densidad de cuidado (Z > ", round(thresh, 2), ").<br/>") else "",
               "<i style='font-size:0.8rem;'>\u00bfDesea que el agente aprenda y se adapte a esta varianza local?</i>")
      }
    }
    auditor_report(verdict)
  }
  
  # Inicializar reporte
  observe({
    req(active_pixels())
    run_audit()
  })
  
  output$auditor_report_output <- renderUI({
    HTML(if (is.null(auditor_report())) "" else auditor_report())
  })
  
  observeEvent(input$run_auditoria, {
    run_audit()
    showNotification(trans("Auditor\u00eda ejecutada.", "Audit executed."), type = "message")
  })
  
  observeEvent(input$auditor_ignore, {
    # Aumentar umbral para ignorar ruido (Z = 2.58 -> 99% confianza, menos sensible a outliers)
    auditor_threshold(2.58)
    save_auditor_config(2.58)
    run_audit()
    showNotification(trans("El Agente Auditor ha aprendido el ruido y ampliado su umbral a Z = 2.58.", 
                           "The Audit Agent learned the noise and expanded its threshold to Z = 2.58."), type = "message")
  })
  
  observeEvent(input$auditor_reset, {
    # Regresar a umbral por defecto (Z = 1.96)
    auditor_threshold(1.96)
    save_auditor_config(1.96)
    run_audit()
    showNotification(trans("Criterios de auditor\u00eda restablecidos a Z = 1.96.", 
                           "Audit thresholds reset back to Z = 1.96."), type = "message")
  })
  
  observeEvent(input$select_line, {
    active_exp_line(input$select_line)
  })
  
  observeEvent(input$go_to_sim_from_exp, {
    sel_line <- active_exp_line()
    exp_mode_val <- switch(sel_line,
      "1" = "exp1", "2" = "exp2", "3" = "exp3", "4" = "exp4",
      "5" = "exp5", "6" = "exp6", "7" = "exp7"
    )
    updateSelectInput(session, "exp_mode", selected = exp_mode_val)
    run_sim_trigger(run_sim_trigger() + 1)
    updateTabsetPanel(session, "nav_active", selected = "Centro de Simulaci\u00f3n")
  })
  
  output$download_care_guide_direct <- downloadHandler(
    filename = function() {
      "Guia_Gobernanza_CARE_FAIR_Puerto_Umbral.txt"
    },
    content = function(file) {
      writeLines(c(
        "==========================================================================",
        "GU\u00cdA DE GOBERNANZA DE DATOS TERRITORIALES (PRINCIPIOS CARE / FAIR)",
        "==========================================================================",
        "Plataforma Puerto Umbral - Ontolog\u00eda Territorial, Tomo II",
        "John Treimun R\u00edos",
        "",
        "Esta gu\u00eda t\u00e9cnica e institucional establece las salvaguardas \u00e9ticas bajo las",
        "cuales se administran y procesan los datos e indicadores espaciales en la",
        "f\u00edsica intr\u00ednseca de los geotensores.",
        "",
        "1. PRINCIPIOS CARE (Gobernanza de Datos Ind\u00edgenas y Comunitarios)",
        "--------------------------------------------------------------------------",
        "   * Collective Benefit (Beneficio Colectivo): Las simulaciones y datos territoriales",
        "     deben servir al desarrollo de la habitabilidad local y el cuidado com\u00fan, nunca",
        "     para la planificaci\u00f3n ex\u00f3gena de vigilancia, control pol\u00edtico o despojo corporativo.",
        "   * Authority to Control (Autoridad de Control): Las comunidades retienen los",
        "     derechos intelectuales y la soberan\u00eda de los datos de campo. Tienen derecho a veto",
        "     sobre la publicaci\u00f3n de mapas que expongan trazas vulnerables.",
        "   * Responsibility (Responsabilidad): Quien procesa los datos debe rendir cuentas",
        "     del impacto social de la representaci\u00f3n. La simplificaci\u00f3n del espacio de calidades",
        "     no debe invisibilizar a los sectores vulnerables.",
        "   * Ethics (Etica): Minimizar el riesgo de estigmatizaci\u00f3n espacial e individual.",
        "",
        "2. PRINCIPIOS FAIR (Gobernanza Cient\u00edfica y Reutilizaci\u00f3n)",
        "--------------------------------------------------------------------------",
        "   * Findable (Localizable): Los datos de p\u00edxeles ontol\u00f3gicos poseen URIs e identificadores",
        "     un\u00edvocos basados en mallas geoespaciales de manzanas chilenas.",
        "   * Accessible (Accesible): Disponibles a trav\u00e9s de protocolos abiertos y transparentes,",
        "     protegiendo el anonimato.",
        "   * Interoperable (Interoperable): Estructurados en bases SQLite est\u00e1ndar y formatos CSV",
        "     de f\u00e1cil ingesta por software SIG comunitario.",
        "   * Reusable (Reutilizable): Licenciados bajo atribuci\u00f3n libre, permitiendo la soberan\u00eda.",
        "",
        "3. EL BLOQUEO DE PRIVACIDAD EN PUERTO UMBRAL",
        "--------------------------------------------------------------------------",
        "   Para resguardar la privacidad de los caminantes, la plataforma Puerto Umbral",
        "   impide por dise\u00f1o la visualizaci\u00f3n de geod\u00e9sicas peatonales sin el enmascaramiento",
        "   estoc\u00e1stico de Langevin (Ruido T\u00e1ctico). Al activar el enmascaramiento, se inyecta",
        "   una fluctuaci\u00f3n t\u00e9rmica (m\u00ednimo 50m) que anonimiza las trazas individuales de forma",
        "   irreversible, manteniendo la consistencia de la NTI global pero protegiendo al habitante.",
        "",
        "Garantizar la soberan\u00eda del dato es el primer paso para habitar con dignidad."
      ), file)
    }
  )
}
