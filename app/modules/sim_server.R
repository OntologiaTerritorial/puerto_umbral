# modules/sim_server.R
# Módulo de Servidor de Cálculos y Simulación Principal (Funciones puras y motores Leaflet/Plotly)
# Autor / Contacto: john.treimun.r@uai.cl

sim_server <- function(input, output, session, lang, run_sim_trigger) {
  # Función de traducción auxiliar local al servidor
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  # ---- BILINGUAL TRANSLATION MECHANISM ----
  lang <- reactive({
    if (is.null(input$lang_toggle)) "ES" else input$lang_toggle
  })
  
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  # Renderizar la interfaz dinámica de la pestaña de simulación
  output$tab3_simulator_ui <- renderUI({
    withMathJax(get_tab3_ui(input, output, session, lang))
  })

  output$control_territorio_title_ui <- renderUI({ trans("Control del Territorio", "Territorial Control") })
  output$sec1_title_ui <- renderUI({ trans("1. Configuraci\u00f3n de Escenario", "1. Scenario Configuration") })
  output$sec2_title_ui <- renderUI({ trans("2. Eventos y Distorsiones", "2. Events & Distortions") })
  output$sec3_title_ui <- renderUI({ trans("3. Campos de Fuerza Globales", "3. Global Force Fields") })
  output$sec4_title_ui <- renderUI({ trans("4. Geometr\u00eda y Solucionador", "4. Geometry & Solver") })
  output$sec5_title_ui <- renderUI({ trans("5. Cargar Datos Externos (Zenodo)", "5. Load External Data (Zenodo)") })
  
  # ---- 2D-3D INTRINSIC VISION SYNCHRONIZATION ----
  observeEvent(input$visual_heatmap, {
    val <- input$visual_heatmap
    if (!is.null(val)) {
      if (val == "none") {
        updateSelectInput(session, "dim_3d", selected = "friccion")
      } else if (val == "nti") {
        updateSelectInput(session, "dim_3d", selected = "nti")
      } else if (val == "ricci") {
        updateSelectInput(session, "dim_3d", selected = "ricci")
      } else if (val == "trauma") {
        updateSelectInput(session, "dim_3d", selected = "altitud")
      }
    }
  })
  
  observeEvent(input$visual_vectorfield, {
    val <- input$visual_vectorfield
    if (!is.null(val)) {
      if (val == "none") {
        updateCheckboxInput(session, "show_vectors", value = FALSE)
      } else {
        updateCheckboxInput(session, "show_vectors", value = TRUE)
      }
    }
  })
  
  output$csv_label_ui <- renderUI({ tags$span(trans("Atributos Territoriales (CSV):", "Territorial Attributes (CSV):")) })
  output$geojson_label_ui <- renderUI({ tags$span(trans("Malla Territorial (GeoJSON):", "Territorial Grid (GeoJSON):")) })
  
  output$banner_active_title_ui <- renderUI({ trans("Capa Sem\u00e1ntica Activa: ", "Active Semantic Layer: ") })
  output$map2d_title_ui <- renderUI({ trans("Mapa 2D: Geod\u00e9sicas e Interacci\u00f3n", "2D Map: Geodesics & Interaction") })
  output$map3d_title_ui <- renderUI({ trans("Malla 3D: Variedad Deformada (Mapas Conectados)", "3D Mesh: Deformed Manifold (Connected Maps)") })
  output$saving_mode_placeholder_ui <- renderUI({ trans("Plotly 3D Desactivado (Modo Ahorro)", "Plotly 3D Disabled (Power Saving Mode)") })
  output$snell_diagnostic_title_ui <- renderUI({ trans("Diagn\u00f3stico de Refracci\u00f3n de Capital (Ley de Snell)", "Capital Refraction Diagnosis (Snell's Law)") })

  # (Moved to tab1_onboarding.R module)

  # (Moved to tab4_matematica.R module)

  # Local Caputo parameter memory store
  local_pixel_params <- reactiveVal(list())
  
  # ---- LIVE FEED TERMINAL LOGS ----
  live_feed_log <- reactiveVal(character(0))
  
  add_log_event <- function(msg) {
    curr <- live_feed_log()
    t_str <- format(Sys.time(), "%H:%M:%S")
    new_event <- paste0("[", t_str, "] ", msg)
    if (length(curr) > 40) curr <- curr[1:40]
    live_feed_log(c(new_event, curr))
  }
  
  output$live_feed_ui <- renderUI({
    logs <- live_feed_log()
    if (length(logs) == 0) {
      return(HTML("<span style='color: #64748b;'>Consola ociosa. Dispare una geod\u00e9sica para iniciar registro...</span>"))
    }
    HTML(paste(logs, collapse = "<br>"))
  })

  observe({
    req(input$local_alpha, input$local_memory_L, selected_pixel())
    id_sel <- selected_pixel()
    params <- local_pixel_params()
    prev <- params[[id_sel]]
    if (is.null(prev) || prev$alpha != input$local_alpha || prev$L != input$local_memory_L) {
      params[[id_sel]] <- list(alpha = input$local_alpha, L = input$local_memory_L)
      local_pixel_params(params)
    }
  })

  observeEvent(selected_pixel(), {
    id_sel <- selected_pixel()
    current_params <- local_pixel_params()[[id_sel]]
    if (!is.null(current_params)) {
      updateSliderInput(session, "alpha_caputo", value = current_params$alpha)
    }
  })

  # Reactivo para cargar el experimento activo desde la base de datos SQLite
  active_experiment_data <- reactive({
    req(input$exp_mode)
    
    exp_mode_val <- input$exp_mode
    exp_id <- switch(exp_mode_val,
                     "exp1" = 1,
                     "exp2" = 2,
                     "exp3" = 3,
                     "exp4" = 4,
                     "exp5" = 5,
                     "exp6" = 6,
                     "exp7" = 7,
                     6) # Default to 6 (Pe\u00f1alol\u00e9n / base)
    
    conn_act <- dbConnect(SQLite(), dbname = db_path)
    on.exit(dbDisconnect(conn_act))
    
    pix_query <- sprintf("SELECT id, x, y, altitud, ndvi, red_cuidado FROM pixeles WHERE experimento_id = %d", exp_id)
    pix_res <- dbGetQuery(conn_act, pix_query)
    
    if (nrow(pix_res) == 0) {
      return(list(manzanas = manzanas_df, graph_data = graph_data))
    }
    
    m_df <- data.frame(
      id = as.character(pix_res$id),
      x = as.numeric(pix_res$x),
      y = as.numeric(pix_res$y),
      ndvi = as.numeric(pix_res$ndvi),
      altitud = as.numeric(pix_res$altitud),
      red_cuidado = as.character(pix_res$red_cuidado),
      stringsAsFactors = FALSE
    )
    
    m_sf <- st_as_sf(m_df, coords = c("x", "y"), crs = UTM_CRS) %>%
      st_transform(4326)
    coords_wgs84 <- st_coordinates(m_sf)
    m_df$lng <- coords_wgs84[, 1]
    m_df$lat <- coords_wgs84[, 2]
    
    pix_ids_str <- paste(sprintf("'%s'", m_df$id), collapse = ",")
    mem_query <- sprintf("SELECT pixel_id, timestamp_simulado, atraccion_H_i FROM pixel_memorias WHERE pixel_id IN (%s)", pix_ids_str)
    lat_query <- sprintf("SELECT pixel_id, timestamp_simulado, magnitud_friccion FROM pixel_latencias WHERE pixel_id IN (%s)", pix_ids_str)
    
    mem_res <- dbGetQuery(conn_act, mem_query)
    lat_res <- dbGetQuery(conn_act, lat_query)
    
    m_list <- split(mem_res, mem_res$pixel_id)
    l_list <- split(lat_res, lat_res$pixel_id)
    
    tensor_memoria_list <- lapply(m_df$id, function(p_id) {
      m_sub <- m_list[[p_id]]
      l_sub <- l_list[[p_id]]
      
      if (is.null(m_sub) || is.null(l_sub) || nrow(m_sub) == 0 || nrow(l_sub) == 0) {
        return(data.frame(
          pasoTiempo = 1:6,
          tensionGentrificacion = rep(0.1, 6),
          cohesionSocial = rep(0.5, 6),
          stringsAsFactors = FALSE
        ))
      }
      
      m_sub <- m_sub[order(m_sub$timestamp_simulado), ]
      l_sub <- l_sub[order(l_sub$timestamp_simulado), ]
      
      tension <- l_sub$magnitud_friccion
      tension_norm <- pmax(0, pmin(1, tension / 2.0))
      
      cohesion <- m_sub$atraccion_H_i
      cohesion_norm <- pmax(0, pmin(1, (cohesion - 1.0) / 4.0))
      
      data.frame(
        pasoTiempo = seq_along(tension),
        tensionGentrificacion = tension_norm,
        cohesionSocial = cohesion_norm,
        stringsAsFactors = FALSE
      )
    })
    names(tensor_memoria_list) <- m_df$id
    
    g_data <- list(
      manzanaId = m_df$id,
      tensorMemoria = tensor_memoria_list
    )
    
    list(manzanas = m_df, graph_data = g_data)
  })
  
  # Reactive choice updater for Origin and Destination selectInputs
  observe({
    m_df <- current_manzanas_df()
    req(m_df)
    ids <- m_df$id
    
    exp_mode_val <- input$exp_mode
    if (grepl("exp", exp_mode_val) && exp_mode_val != "exp6") {
      # Synthetic default
      default_orig <- ids[1]
      default_dest <- ids[length(ids)]
    } else {
      # Pe\u00f1alol\u00e9n real grid default
      default_orig <- "13122111004016"
      default_dest <- "13122041002007"
    }
    
    if (!(default_orig %in% ids)) default_orig <- ids[1]
    if (!(default_dest %in% ids)) default_dest <- ids[length(ids)]
    
    updateSelectizeInput(session, "origen_id", choices = ids, selected = default_orig, server = TRUE)
    updateSelectizeInput(session, "destino_id", choices = ids, selected = default_dest, server = TRUE)
  })

  # Observers for Onboarding Experiment cards buttons
  observeEvent(input$load_exp_1, { updateSelectInput(session, "exp_mode", selected = "exp1"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })
  observeEvent(input$load_exp_2, { updateSelectInput(session, "exp_mode", selected = "exp2"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })
  observeEvent(input$load_exp_3, { updateSelectInput(session, "exp_mode", selected = "exp3"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })
  observeEvent(input$load_exp_4, { updateSelectInput(session, "exp_mode", selected = "exp4"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })
  observeEvent(input$load_exp_5, { updateSelectInput(session, "exp_mode", selected = "exp5"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })
  observeEvent(input$load_exp_6, { updateSelectInput(session, "exp_mode", selected = "exp6"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })
  observeEvent(input$load_exp_7, { updateSelectInput(session, "exp_mode", selected = "exp7"); updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n") })

  # Reactive value for custom uploaded data (Zenodo)
  # (Note: custom_data was L1322, we replace the definition below)
  
  # Reactive value for custom uploaded data (Zenodo)
  custom_data <- reactiveVal(NULL)
  
  # Reactive for the current manzanas data frame
  current_manzanas_df <- reactive({
    if (!is.null(custom_data())) {
      return(custom_data()$manzanas)
    } else {
      return(active_experiment_data()$manzanas)
    }
  })
  
  # Reactive for the care links
  current_care_links <- reactive({
    compute_care_links(current_manzanas_df())
  })
  
  # Reactive for the graph data (Tension/Cohesion history)
  current_graph_data <- reactive({
    m_df <- current_manzanas_df()
    if (!is.null(custom_data())) {
      tensor_memoria_list <- lapply(m_df$id, function(p_id) {
        gent_val <- if ("gentrificacion" %in% colnames(m_df)) m_df[m_df$id == p_id, "gentrificacion"][1] else 0.1
        cohe_val <- if ("cohesion" %in% colnames(m_df)) m_df[m_df$id == p_id, "cohesion"][1] else 0.5
        if (is.na(gent_val)) gent_val <- 0.1
        if (is.na(cohe_val)) cohe_val <- 0.5
        
        data.frame(
          pasoTiempo = 1:6,
          tensionGentrificacion = rep(gent_val, 6),
          cohesionSocial = rep(cohe_val, 6),
          stringsAsFactors = FALSE
        )
      })
    } else {
      tensor_memoria_list <- active_experiment_data()$graph_data$tensorMemoria
    }
    names(tensor_memoria_list) <- m_df$id
    list(
      manzanaId = m_df$id,
      tensorMemoria = tensor_memoria_list
    )
  })
  
  # Función de interpolación espacial robusta con fallback IDW nativo en R puro para Shinylive (WebAssembly)
  safe_interpolate_grid <- function(x_points, y_points, values, xo, yo, linear_mode = TRUE) {
    # Sanitizar NAs en los valores
    values[is.na(values)] <- mean(values, na.rm = TRUE)
    if (all(is.na(values))) values <- rep(0.0, length(values))
    
    # Remover registros con coordenadas nulas
    valid_indices <- !is.na(x_points) & !is.na(y_points)
    x_p <- x_points[valid_indices]
    y_p <- y_points[valid_indices]
    val_p <- values[valid_indices]
    
    if (length(x_p) == 0) {
      return(matrix(0, nrow = length(yo), ncol = length(xo)))
    }
    
    # Agregar pequeño jitter para evitar puntos colineales y duplicados exactos en akima
    jx <- x_p + runif(length(x_p), -0.2, 0.2)
    jy <- y_p + runif(length(y_p), -0.2, 0.2)
    
    ir <- tryCatch({
      akima::interp(jx, jy, val_p, xo = xo, yo = yo, linear = linear_mode, duplicate = "mean")
    }, error = function(e) {
      tryCatch({
        akima::interp(jx, jy, val_p, xo = xo, yo = yo, linear = FALSE, duplicate = "mean")
      }, error = function(e2) {
        NULL
      })
    })
    
    if (!is.null(ir) && !is.null(ir$z)) {
      Z <- t(ir$z)
    } else {
      # Fallback IDW robusto en R puro (garantizado para WebAssembly / Shinylive)
      nx <- length(xo)
      ny <- length(yo)
      Z <- matrix(0, ny, nx)
      for (i in 1:ny) {
        for (j in 1:nx) {
          dists <- sqrt((x_p - xo[j])^2 + (y_p - yo[i])^2)
          weights <- 1 / (dists^2 + 1e-5)
          Z[i, j] <- sum(val_p * weights) / sum(weights)
        }
      }
    }
    
    Z[is.na(Z)] <- mean(Z, na.rm = TRUE)
    return(Z)
  }

  # Auxiliar de interpolación para escenarios personalizados (grilla 30x30)
  interpolate_column_30x30 <- function(df, col_name, xo, yo) {
    if (col_name %in% colnames(df)) {
      val <- df[[col_name]]
    } else {
      val <- rep(100.0, nrow(df))
    }
    safe_interpolate_grid(df$x, df$y, val, xo = xo, yo = yo, linear_mode = TRUE)
  }
  
  # Reactive for demographic calibration (IPF)
  current_ipf_calib_data <- reactive({
    m_df <- current_manzanas_df()
    if (!is.null(custom_data())) {
      if ("persons_orig" %in% colnames(m_df)) {
        n_mz <- nrow(m_df)
        seed_matrix <- matrix(1.0, nrow = 2, ncol = n_mz)
        total_e4a18 <- sum(m_df$e4a18_orig, na.rm = TRUE)
        total_e15a24 <- sum(m_df$e15a24_orig, na.rm = TRUE)
        if (total_e4a18 == 0) total_e4a18 <- 10.0 * n_mz
        if (total_e15a24 == 0) total_e15a24 <- 15.0 * n_mz
        target_rows <- c(total_e4a18, total_e15a24)
        target_cols <- m_df$persons_orig
        target_cols[is.na(target_cols)] <- 100.0
        
        calib_matrix <- ipf_2d_r(seed_matrix, target_rows, target_cols)
        data.frame(
          id = m_df$id,
          persons_orig   = target_cols,
          e4a18_orig     = if ("e4a18_orig" %in% colnames(m_df)) m_df$e4a18_orig else target_cols * 0.25,
          e15a24_orig    = if ("e15a24_orig" %in% colnames(m_df)) m_df$e15a24_orig else target_cols * 0.35,
          persons_calib  = colSums(calib_matrix),
          e4a18_calib    = calib_matrix[1, ],
          e15a24_calib   = calib_matrix[2, ],
          stringsAsFactors = FALSE
        )
      } else {
        set.seed(42)
        n_mz <- nrow(m_df)
        persons_orig <- if ("persons" %in% colnames(m_df)) m_df$persons else sample(80:320, n_mz, replace = TRUE)
        persons_orig[is.na(persons_orig)] <- 100.0
        data.frame(
          id = m_df$id,
          persons_orig   = persons_orig,
          e4a18_orig     = round(persons_orig * 0.22),
          e15a24_orig    = round(persons_orig * 0.28),
          persons_calib  = round(persons_orig * 1.05),
          e4a18_calib    = round(persons_orig * 1.05 * 0.22),
          e15a24_calib   = round(persons_orig * 1.05 * 0.28),
          stringsAsFactors = FALSE
        )
      }
    } else {
      ipf_calib_data
    }
  })
  
  # Data Ingestion Observer
  observeEvent(input$ingest_data, {
    req(input$uploaded_csv, input$uploaded_geojson)
    csv_file <- input$uploaded_csv$datapath
    geojson_file <- input$uploaded_geojson$datapath
    
    tryCatch({
      sf_data <- sf::st_read(geojson_file, quiet = TRUE)
      sf_wgs <- sf::st_transform(sf_data, 4326)
      coords_wgs <- sf::st_coordinates(sf::st_centroid(sf_wgs))
      
      sf_metric <- sf::st_transform(sf_data, 3857)
      coords_metric <- sf::st_coordinates(sf::st_centroid(sf_metric))
      
      csv_data <- read.csv(csv_file, stringsAsFactors = FALSE)
      
      id_col_csv <- intersect(c("id", "ID", "Id", "key"), colnames(csv_data))[1]
      id_col_geo <- intersect(c("id", "ID", "Id", "key"), colnames(sf_data))[1]
      
      if (is.na(id_col_csv)) id_col_csv <- colnames(csv_data)[1]
      if (is.na(id_col_geo)) id_col_geo <- colnames(sf_data)[1]
      
      csv_data$id <- as.character(csv_data[[id_col_csv]])
      sf_data$id <- as.character(sf_data[[id_col_geo]])
      
      m_df <- merge(csv_data, as.data.frame(sf_data)[, c("id", id_col_geo), drop=FALSE], by="id")
      
      match_idx <- match(m_df$id, sf_data$id)
      m_df$x <- coords_metric[match_idx, 1]
      m_df$y <- coords_metric[match_idx, 2]
      m_df$lng <- coords_wgs[match_idx, 1]
      m_df$lat <- coords_wgs[match_idx, 2]
      
      if (!"altitud" %in% colnames(m_df)) {
        if ("altitude" %in% colnames(m_df)) {
          m_df$altitud <- m_df$altitude
        } else {
          m_df$altitud <- 100.0
        }
      }
      
      custom_data(list(
        manzanas = m_df,
        sf_wgs = sf_wgs
      ))
      
      updateSelectizeInput(session, "origen_id", choices = m_df$id, selected = m_df$id[1], server = TRUE)
      updateSelectizeInput(session, "destino_id", choices = m_df$id, selected = m_df$id[min(length(m_df$id), 2)], server = TRUE)
      
      showNotification("Datos territoriales ingestados con \u00e9xito y proyectados a EPSG:3857 (Web Mercator).", type = "message")
      recalc_trigger(recalc_trigger() + 1)
    }, error = function(e) {
      showNotification(paste("Error en ingesta de datos:", e$message), type = "error")
    })
  })
  
  # Reactive para cargar los datos base
  base_manifold_data <- reactive({
    precompute_eqt_base(current_manzanas_df(), t_idx = 0)
  })
  
  # Reactive para unificar el cálculo de variables intrínsecas en las manzanas
  manzanas_con_tensores <- reactive({
    g_data <- current_graph_data()
    map_df <- current_manzanas_df()
    if (is.null(g_data) || is.null(map_df) || nrow(map_df) == 0) return(NULL)
    
    t_idx <- 0
    gentrificacion <- sapply(g_data$tensorMemoria, function(df) {
      df$tensionGentrificacion[df$pasoTiempo == t_idx + 1]
    })
    cohesion <- sapply(g_data$tensorMemoria, function(df) {
      df$cohesionSocial[df$pasoTiempo == t_idx + 1]
    })
    
    map_df$gentrificacion <- gentrificacion
    map_df$cohesion <- cohesion
    
    lambda_sys <- if (!is.null(input$lambda_val)) input$lambda_val else 0.8
    lyap_sys <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.4
    
    map_df$nti_val <- abs(map_df$gentrificacion - lambda_sys * map_df$cohesion) * (1.0 + map_df$altitud / 150.0)
    map_df$ricci_val <- map_df$gentrificacion - map_df$cohesion
    map_df$lie_val <- abs(map_df$gentrificacion * map_df$cohesion * lyap_sys)
    
    m_trauma <- matriz_memoria_trauma()
    base_data <- base_manifold_data()
    map_df$trauma_val <- 0
    if (!is.null(m_trauma) && !is.null(base_data)) {
      xg <- base_data$Xg
      yg <- base_data$Yg
      for (idx in seq_len(nrow(map_df))) {
        idx_x <- which.min(abs(xg - map_df$x[idx]))
        idx_y <- which.min(abs(yg - map_df$y[idx]))
        map_df$trauma_val[idx] <- m_trauma[idx_y, idx_x]
      }
    }
    
    return(map_df)
  })
  
  # Reactive para precomputar la variedad deformada activa y sus Christoffels
  active_surface <- reactive({
    esc <- escenario_actual()
    base_data <- base_manifold_data()
    dim_sel <- input$dim_3d
    
    # Seleccionar la base de la superficie
    Z_base <- switch(dim_sel,
                     "bienestar" = {
                       Z_b <- as.matrix(esc$superficie)
                       max(Z_b) - Z_b + 50
                     },
                     "altitud" = {
                       as.matrix(base_data$Z_alt) / 2.0
                     },
                     "lie" = {
                       seq_sel <- if (!is.null(input$lie_seq)) input$lie_seq else "X_Y"
                       if (seq_sel == "X_Y") {
                         as.matrix(sim_avanzada$lie_algebra$surface_X_Y)
                       } else if (seq_sel == "Y_X") {
                         as.matrix(sim_avanzada$lie_algebra$surface_Y_X)
                       } else {
                         as.matrix(sim_avanzada$lie_algebra$surface_diff)
                       }
                     },
                     "nti" = {
                        m_tens <- manzanas_con_tensores()
                        xg_vec <- base_data$Xg
                        yg_vec <- base_data$Yg
                        if (!is.null(m_tens) && nrow(m_tens) > 0) {
                          safe_interpolate_grid(m_tens$x, m_tens$y, m_tens$nti_val, xo = xg_vec, yo = yg_vec, linear_mode = TRUE) * 450.0
                        } else {
                          matrix(0, nrow = length(yg_vec), ncol = length(xg_vec))
                        }
                      },
                      "ricci" = {
                        m_tens <- manzanas_con_tensores()
                        xg_vec <- base_data$Xg
                        yg_vec <- base_data$Yg
                        if (!is.null(m_tens) && nrow(m_tens) > 0) {
                          safe_interpolate_grid(m_tens$x, m_tens$y, m_tens$ricci_val, xo = xg_vec, yo = yg_vec, linear_mode = TRUE) * 450.0
                        } else {
                          matrix(0, nrow = length(yg_vec), ncol = length(xg_vec))
                        }
                      },
                     as.matrix(esc$superficie)) # friccion (defecto)
    
    Z_base[is.na(Z_base)] <- mean(Z_base, na.rm = TRUE)
    
    # Aplicar distorsiones locales directamente a la malla geom\u00e9trica
    dist_df <- distorsiones_df()
    xg_vec <- base_data$Xg
    yg_vec <- base_data$Yg
    Z_active <- Z_base
    
    # Sumar memoria colectiva si esta habilitada
    if (isTRUE(input$use_collective_memory) && !is.null(matriz_memoria_trauma())) {
      if (dim_sel == "bienestar") {
        Z_active <- Z_active - matriz_memoria_trauma()
      } else {
        Z_active <- Z_active + matriz_memoria_trauma()
      }
    }
    
    if (nrow(dist_df) > 0) {
      for (k in seq_len(nrow(dist_df))) {
        d_x <- dist_df$x[k]
        d_y <- dist_df$y[k]
        d_type <- dist_df$type[k]
        d_mag <- dist_df$mag[k]
        
        is_repulsive <- d_type %in% c("obstacle", "delito", "accidente")
        
        if (dim_sel == "bienestar") {
          sign_val <- if (is_repulsive) -1.0 else 1.0
        } else {
          sign_val <- if (is_repulsive) 1.0 else -1.0
        }
        
        mag_params <- switch(d_mag,
                             "leve"     = list(r = 100, amp = 80),
                             "moderada" = list(r = 200, amp = 150),
                             "severa"   = list(r = 350, amp = 250),
                             "critica"  = list(r = 500, amp = 400),
                             list(r = 200, amp = 150))
        r_dist <- mag_params$r
        amp_3d <- sign_val * mag_params$amp
        
        for (i in seq_along(yg_vec)) {
          for (j in seq_along(xg_vec)) {
            dist_sq <- (xg_vec[j] - d_x)^2 + (yg_vec[i] - d_y)^2
            Z_active[i, j] <- Z_active[i, j] + amp_3d * exp(-dist_sq / (2 * r_dist^2))
          }
        }
      }
    }
    
    lambda_val <- if (!is.null(input$lambda_val)) input$lambda_val else 0.8
    exp_m <- if (!is.null(input$exp_mode)) input$exp_mode else "base"
    g_ratio <- if (!is.null(input$g_urban_ratio)) input$g_urban_ratio else 8.0
    use_lw_val <- if (!is.null(input$use_lw)) input$use_lw else TRUE
    compute_manifold_geometry(base_data$Xg, base_data$Yg, Z_active, base_data$Z_alt, base_data$dx, base_data$dy, lambda_val = lambda_val, exp_mode_val = exp_m, g_urban_ratio_val = g_ratio, use_lw = use_lw_val)
  })
  
  # Inicializar distorsiones territoriales realistas por defecto
  pts_f_init <- utm_to_wgs84(353700, 6294800)
  pts_d_init <- utm_to_wgs84(352800, 6294100)
  pts_s_init <- utm_to_wgs84(354200, 6295200)
  
  distorsiones_df <- reactiveVal(data.frame(
    id = c("dist_feria_init", "dist_delito_init", "dist_seguridad_init"),
    x = c(353700, 352800, 354200),
    y = c(6294800, 6294100, 6295200),
    lng = c(pts_f_init[1], pts_d_init[1], pts_s_init[1]),
    lat = c(pts_f_init[2], pts_d_init[2], pts_s_init[2]),
    type = c("compras", "delito", "seguridad"),
    mag = c("moderada", "severa", "severa"),
    stringsAsFactors = FALSE
  ))
  
  # Inicializar matriz de memoria colectiva y cargar pre-entrenamiento
  matriz_memoria_trauma <- reactiveVal(NULL)
  
  observe({
    mem_path <- "www/data/pre_trained_memory.json"
    if (!file.exists(mem_path)) mem_path <- "app/www/data/pre_trained_memory.json"
    
    initial_mem <- if (file.exists(mem_path)) {
      tryCatch({
        as.matrix(jsonlite::fromJSON(mem_path))
      }, error = function(e) {
        matrix(0, 30, 30)
      })
    } else {
      # Fallback a hotspot pre-calibrado si no existe el archivo JSON
      m <- matrix(0, 30, 30)
      m[10:18, 11:15] <- 75 # Canal Las Perdices
      m[6:9, 20:24] <- 110 # Ladera Quebrada Macul
      m
    }
    matriz_memoria_trauma(initial_mem)
  })
  
  selected_pixel <- reactiveVal("13122111004016")
  
  # ---- REDIRECCIONADORES DE CASOS DESDE EL INICIO ----
  go_trigger <- reactiveVal(0)
  recalc_trigger <- reactiveVal(0)
  
  observeEvent(input$go_case_a, {
    updateSelectInput(session, "narrative_case", selected = "caso_a")
    updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n")
    go_trigger(go_trigger() + 1)
  })
  observeEvent(input$go_case_b, {
    updateSelectInput(session, "narrative_case", selected = "caso_b")
    updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n")
    go_trigger(go_trigger() + 1)
  })
  observeEvent(input$go_case_c, {
    updateSelectInput(session, "narrative_case", selected = "caso_c")
    updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n")
    go_trigger(go_trigger() + 1)
  })
  observeEvent(input$go_case_d, {
    updateSelectInput(session, "narrative_case", selected = "caso_d")
    updateNavbarPage(session, "nav_active", selected = "Centro de Simulaci\u00f3n")
    go_trigger(go_trigger() + 1)
  })
  
  # Esperar a que la UI del cliente se sincronice antes de recalcular
  observeEvent(go_trigger(), {
    if (go_trigger() > 0) {
      invalidateLater(450)
      recalc_trigger(recalc_trigger() + 1)
    }
  }, ignoreInit = TRUE)
  
  # Auto-run ODE solver when triggered from other modules
  observeEvent(run_sim_trigger(), {
    if (run_sim_trigger() > 0) {
      invalidateLater(450)
      recalc_trigger(recalc_trigger() + 1)
    }
  }, ignoreInit = TRUE)
  
  # ---- CONFIGURADOR DE CASOS NARRATIVOS ----
  observeEvent(input$narrative_case, {
    case <- input$narrative_case
    if (case == "custom") return()
    
    # Limpiar distorsiones previas
    distorsiones_df(data.frame(
      id = character(), x = numeric(), y = numeric(), lng = numeric(), lat = numeric(),
      type = character(), mag = character(), stringsAsFactors = FALSE
    ))
    
    if (case == "caso_a") {
      updateSelectInput(session, "ped_profile", selected = "cuidado")
      updateSelectInput(session, "scen_time", selected = "Dia")
      updateSelectInput(session, "scen_season", selected = "Verano")
      updateSelectInput(session, "dim_3d", selected = "friccion")
      updateSelectInput(session, "origen_id", selected = "13122111004016")
      updateSelectInput(session, "destino_id", selected = "13122041002007")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 12000)
      updateSliderInput(session, "pot_dest_amp", value = 15000)
      updateSliderInput(session, "v0_vital", value = 12.0)
      updateSliderInput(session, "lyap_vol", value = 0.2)
      updateSliderInput(session, "lambda_val", value = 0.8)
    } else if (case == "caso_b") {
      updateSelectInput(session, "ped_profile", selected = "nocturna")
      updateSelectInput(session, "scen_time", selected = "Noche")
      updateSelectInput(session, "scen_season", selected = "Invierno")
      updateSelectInput(session, "dim_3d", selected = "friccion")
      updateSelectInput(session, "origen_id", selected = "13122061004026")
      updateSelectInput(session, "destino_id", selected = "13122041003021")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 15000)
      updateSliderInput(session, "pot_dest_amp", value = 20000)
      updateSliderInput(session, "v0_vital", value = 14.0)
      updateSliderInput(session, "lyap_vol", value = 0.45)
      updateSliderInput(session, "lambda_val", value = 0.8)
      
      # Agregar distorsi\u00f3n de Delito
      new_row <- data.frame(
        id = "dist_delito_init",
        x = 353100, y = 6294300,
        lng = -70.535, lat = -33.475,
        type = "delito", mag = "severa",
        stringsAsFactors = FALSE
      )
      distorsiones_df(new_row)
    } else if (case == "caso_c") {
      updateSelectInput(session, "ped_profile", selected = "mayor")
      updateSelectInput(session, "scen_time", selected = "Dia")
      updateSelectInput(session, "scen_season", selected = "Invierno")
      updateSelectInput(session, "dim_3d", selected = "altitud")
      updateSelectInput(session, "origen_id", selected = "13122061004026")
      updateSelectInput(session, "destino_id", selected = "13122011003004")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 18000)
      updateSliderInput(session, "pot_dest_amp", value = 12000)
      updateSliderInput(session, "pot_slope_amp", value = 22000)
      updateSliderInput(session, "v0_vital", value = 6.0)
      updateSliderInput(session, "lyap_vol", value = 0.70)
      updateSliderInput(session, "lambda_val", value = 0.5)
      
      # Agregar un obst\u00e1culo en el trayecto de ladera
      pts_obs <- utm_to_wgs84(354550, 6295450)
      new_row <- data.frame(
        id = "dist_obs_init",
        x = 354550, y = 6295450,
        lng = pts_obs[1], lat = pts_obs[2],
        type = "obstacle", mag = "severa",
        stringsAsFactors = FALSE
      )
      distorsiones_df(new_row)
    } else if (case == "caso_d") {
      updateSelectInput(session, "ped_profile", selected = "cuidado")
      updateSelectInput(session, "scen_time", selected = "Dia")
      updateSelectInput(session, "scen_season", selected = "Verano")
      updateSelectInput(session, "dim_3d", selected = "lie")
      updateSelectInput(session, "lie_seq", selected = "diff")
      updateSelectInput(session, "origen_id", selected = "13122061003009")
      updateSelectInput(session, "destino_id", selected = "13122061001056")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 10000)
      updateSliderInput(session, "pot_dest_amp", value = 25000)
      updateSliderInput(session, "v0_vital", value = 12.0)
      updateSliderInput(session, "lyap_vol", value = 0.3)
      updateSliderInput(session, "lambda_val", value = 0.8)
      
      # Agregar corredor comunitario y obst\u00e1culo inmobiliario en el trayecto
      pts_c <- utm_to_wgs84(354600, 6295000)
      pts_g <- utm_to_wgs84(354500, 6294800)
      new_rows <- data.frame(
        id = c("dist_corredor_init", "dist_gent_init"),
        x = c(354600, 354500),
        y = c(6295000, 6294800),
        lng = c(pts_c[1], pts_g[1]),
        lat = c(pts_c[2], pts_g[2]),
        type = c("seguridad", "obstacle"),
        mag = c("severa", "critica"),
        stringsAsFactors = FALSE
      )
      distorsiones_df(new_rows)
    } else if (case == "caso_e") {
      updateSelectInput(session, "ped_profile", selected = "cuidado")
      updateSelectInput(session, "scen_time", selected = "Dia")
      updateSelectInput(session, "scen_season", selected = "Verano")
      updateSelectInput(session, "dim_3d", selected = "friccion")
      updateSelectInput(session, "origen_id", selected = "13122111004016")
      updateSelectInput(session, "destino_id", selected = "13122041002007")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 15000)
      updateSliderInput(session, "pot_dest_amp", value = 10000)
      updateSliderInput(session, "v0_vital", value = 10.0)
      updateSliderInput(session, "lyap_vol", value = 0.5)
      updateSliderInput(session, "lambda_val", value = 0.9)
      
      new_row <- data.frame(
        id = "dist_gent_e", x = 353500, y = 6294500,
        lng = utm_to_wgs84(353500, 6294500)[1], lat = utm_to_wgs84(353500, 6294500)[2],
        type = "obstacle", mag = "critica", stringsAsFactors = FALSE
      )
      distorsiones_df(new_row)
    } else if (case == "caso_f") {
      updateSelectInput(session, "ped_profile", selected = "joven")
      updateSelectInput(session, "scen_time", selected = "Dia")
      updateSelectInput(session, "scen_season", selected = "Verano")
      updateSelectInput(session, "dim_3d", selected = "friccion")
      updateSelectInput(session, "origen_id", selected = "13122061003009")
      updateSelectInput(session, "destino_id", selected = "13122061001056")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 5000)
      updateSliderInput(session, "pot_dest_amp", value = 30000)
      updateSliderInput(session, "v0_vital", value = 18.0)
      updateSliderInput(session, "lyap_vol", value = 0.1)
      updateSliderInput(session, "lambda_val", value = 0.4)
      
      new_row <- data.frame(
        id = "dist_metro_f", x = 354000, y = 6294800,
        lng = utm_to_wgs84(354000, 6294800)[1], lat = utm_to_wgs84(354000, 6294800)[2],
        type = "seguridad", mag = "critica", stringsAsFactors = FALSE
      )
      distorsiones_df(new_row)
    } else if (case == "caso_g") {
      updateSelectInput(session, "ped_profile", selected = "mayor")
      updateSelectInput(session, "scen_time", selected = "Noche")
      updateSelectInput(session, "scen_season", selected = "Invierno")
      updateSelectInput(session, "dim_3d", selected = "friccion")
      updateSelectInput(session, "origen_id", selected = "13122061004026")
      updateSelectInput(session, "destino_id", selected = "13122011003004")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 25000)
      updateSliderInput(session, "pot_dest_amp", value = 8000)
      updateSliderInput(session, "v0_vital", value = 5.0)
      updateSliderInput(session, "lyap_vol", value = 0.8)
      updateSliderInput(session, "lambda_val", value = 1.0)
      
      pts1 <- utm_to_wgs84(353200, 6294500)
      pts2 <- utm_to_wgs84(353800, 6294200)
      new_rows <- data.frame(
        id = c("dist_des_1", "dist_des_2"),
        x = c(353200, 353800), y = c(6294500, 6294200),
        lng = c(pts1[1], pts2[1]), lat = c(pts1[2], pts2[2]),
        type = c("accidente", "accidente"),
        mag = c("critica", "severa"), stringsAsFactors = FALSE
      )
      distorsiones_df(new_rows)
    } else if (case == "caso_h") {
      updateSelectInput(session, "ped_profile", selected = "nocturna")
      updateSelectInput(session, "scen_time", selected = "Dia")
      updateSelectInput(session, "scen_season", selected = "Verano")
      updateSelectInput(session, "dim_3d", selected = "lie")
      updateSelectInput(session, "lie_seq", selected = "X_Y")
      updateSelectInput(session, "origen_id", selected = "13122111004016")
      updateSelectInput(session, "destino_id", selected = "13122041003021")
      updateCheckboxInput(session, "use_potentials", value = TRUE)
      updateSliderInput(session, "pot_wall_amp", value = 20000)
      updateSliderInput(session, "pot_dest_amp", value = 15000)
      updateSliderInput(session, "v0_vital", value = 12.0)
      updateSliderInput(session, "lyap_vol", value = 0.4)
      updateSliderInput(session, "lambda_val", value = 0.9)
      
      new_row <- data.frame(
        id = "dist_sub_h", x = 353600, y = 6294900,
        lng = utm_to_wgs84(353600, 6294900)[1], lat = utm_to_wgs84(353600, 6294900)[2],
        type = "obstacle", mag = "critica", stringsAsFactors = FALSE
      )
      distorsiones_df(new_row)
    }
    
    # ---- 1. EXPLICACI\u00d3N INFORMATIVA POR POPUPS Y VOZ SINTETIZADA ----
    # Cada vez que el operador activa un caso preestablecido en el Centro de Simulaci\u00f3n,
    # la plataforma lanza un modal explicativo con el marco conceptual/matem\u00e1tico
    # y reproduce una s\u00edntesis de voz (TTS) para facilitar que la informaci\u00f3n sea
    # incorporada de forma multimodal por los sentidos.
    if (case != "custom") {
      is_en <- identical(lang(), "EN")
      m_title <- ""
      m_body <- ""
      audio_text <- ""
      
      if (case == "caso_a") {
        m_title <- if (is_en) "Case A: Base Control Model (Symmetric Conditions)" else "Caso A: Modelo de Control Base (Condiciones Sim\u00e9tricas)"
        m_body <- if (is_en) {
          "This case simulates a homogeneous territory without exclusion barriers or sharp real estate surplus value deformations. The geodesic paths of walkers (caregivers) are virtually linear, minimizing physical and intrinsic friction. It serves as the baseline for comparison."
        } else {
          "Este caso simula un territorio homog\u00e9neo sin barreras de exclusi\u00f3n ni deformaciones agudas por plusval\u00eda. Las trayectorias geod\u00e9sicas de los caminantes (cuidadores) son pr\u00e1cticamente lineales, minimizando la fricci\u00f3n f\u00edsica e intr\u00ednseca. Es el escenario de referencia contra el cual se miden las desviaciones de poder."
        }
        audio_text <- if (is_en) "Case A: Base Control Model. Symmetric conditions simulated." else "Caso A: Modelo de Control Base. Se simulan condiciones de tr\u00e1nsito sim\u00e9tricas sin fricci\u00f3n."
      } else if (case == "caso_b") {
        m_title <- if (is_en) "Case B: Real Estate Pressure & Gentrification" else "Caso B: Presi\u00f3n Inmobiliaria y Gentrificaci\u00f3n"
        m_body <- if (is_en) {
          "A high-magnitude Power tensor P_ij is introduced in the residential center, simulating speculative land pressure. This generates a 'friction hill' that aggressively deflects care geodesics toward the periphery, increasing travel times and forcing pedestrians onto hostile edges."
        } else {
          "Se introduce un tensor de Poder P_ij de alta magnitud en el centro residencial, simulando especulaci\u00f3n de suelo. Esto genera una 'colina de fricci\u00f3n' que desv\u00eda agresivamente las geod\u00e9sicas de cuidado hacia la periferia, aumentando los tiempos de viaje y forzando a los peatones a transitar por bordes hostiles."
        }
        audio_text <- if (is_en) "Case B: Real Estate Pressure. A friction hill deflects care trajectories." else "Caso B: Presi\u00f3n Inmobiliaria. Un repulsor de plusval\u00eda expulsa las trayectorias de cuidado."
      } else if (case == "caso_c") {
        m_title <- if (is_en) "Case C: Urban Barrier & Absolute Segregation" else "Caso C: Barrera Urbana y Segregaci\u00f3n Absoluta"
        m_body <- if (is_en) {
          "Simulates the intrusion of private infrastructure or a highway that acts as a wall (infinite potential barrier). Care agents cannot cross this zone and must take extreme detours, representing the fragmentation of the community social fabric."
        } else {
          "Simula la irrupci\u00f3n de una infraestructura privada o autopista que act\u00faa como un muro (barrera de potencial infinito). Los agentes de cuidado no pueden cruzar esta zona y se ven obligados a dar rodeos extremos, lo que representa la fragmentaci\u00f3n del tejido social comunitario."
        }
        audio_text <- if (is_en) "Case C: Urban Barrier. A physical wall blocks care walkers." else "Caso C: Barrera Urbana. Un muro f\u00edsico de segregaci\u00f3n bloquea el paso de los caminantes."
      } else if (case == "caso_d") {
        m_title <- if (is_en) "Case D: Community Cohesion Shield" else "Caso D: Escudo de Cohesi\u00f3n Comunitaria"
        m_body <- if (is_en) {
          "Demonstrates Proposition 2 (Metric Stabilization). Neighborhood organization and localized care networks create a 'metric shield' of social cohesion (local attractors) that dampens real estate deformation, allowing pedestrians to walk safely within a local shelter radius."
        } else {
          "Aqu\u00ed se demuestra la Proposici\u00f3n 2 (Estabilizaci\u00f3n M\u00e9trica). La organizaci\u00f3n vecinal y las redes de cuidado localizadas crean un 'escudo m\u00e9trico' de cohesi\u00f3n social (atractores locales) que amortiguan la deformaci\u00f3n inmobiliaria, permitiendo que los peatones transiten de forma segura en un radio de amparo local."
        }
        audio_text <- if (is_en) "Case D: Cohesion Shield. Social organization buffers capital pressure." else "Caso D: Escudo de Cohesi\u00f3n. La organizaci\u00f3n social amortigua la fricci\u00f3n y desv\u00eda la presi\u00f3n del capital."
      } else if (case == "caso_e") {
        m_title <- if (is_en) "Case E: Autopoiesis & Attractors of Solidarity" else "Caso E: Autopoiesis y Atractores de Solidaridad"
        m_body <- if (is_en) {
          "Under critical pressure (P > P_crit), the Hessian curvature of the local potential is locally inverted. Spaces previously condemned as exclusion zones are autopoiethically transformed into soup kitchens and shelters, acting as community attractors."
        } else {
          "Bajo presi\u00f3n cr\u00edtica (P > P_crit), la curvatura Hessiana del potencial local se invierte localmente. Espacios que antes eran focos de exclusi\u00f3n se transmutan autopoieticamente en ollas comunes y refugios, actuando como sumideros de amparo que atraen las geod\u00e9sicas locales."
        }
        audio_text <- if (is_en) "Case E: Community Autopoiesis. Exclusion areas invert into attractors of solidarity." else "Caso E: Autopoiesis Comunitaria. Un obst\u00e1culo repulsor se invierte en un atractor de solidaridad en tiempos de crisis."
      } else if (case == "caso_f") {
        m_title <- if (is_en) "Case F: Conservation Sanctuary (Robin Boundary)" else "Caso F: Santuario de Conservaci\u00f3n (Condiciones Robin)"
        m_body <- if (is_en) {
          "Represents the Quebrada de Macul case. Mixed Robin boundary conditions act as a 'semi-permeable skin': they allow regulated pedestrian access (commons access) but repel the encroachment of real estate speculation at the cordillera borders."
        } else {
          "Representa el caso de la Quebrada de Macul. Condiciones mixtas de contorno de Robin act\u00faan como una 'piel semipermeable': permiten el paso regulado de peatones (acceso al commons ecol\u00f3gico) pero repelen el avance de la especulaci\u00f3n inmobiliaria en el borde cordillerano."
        }
        audio_text <- if (is_en) "Case F: Conservation Sanctuary. Semipermeable boundaries regulate capital flows." else "Caso F: Santuario de Conservaci\u00f3n. Fronteras semipermeables regulan el flujo de biodiversidad y capital."
      } else if (case == "caso_g") {
        m_title <- if (is_en) "Case G: Edge Refraction (Territorial Snell)" else "Caso G: Refracci\u00f3n de Borde (Snell Territorial)"
        m_body <- if (is_en) {
          "Simulates crossing segregated borders. Abrupt changes in sidewalk and lighting quality between municipalities act as a change in refractive index, physically deflecting the angle of pedestrian geodesics at the boundary."
        } else {
          "Simula el cruce de fronteras segregadas. La variaci\u00f3n abrupta en la calidad de veredas y luminarias entre municipios act\u00faa como un cambio de \u00edndice de refracci\u00f3n, desviando f\u00edsicamente el \u00e1ngulo de las geod\u00e9sicas peatonal en el borde."
        }
        audio_text <- if (is_en) "Case G: Edge Refraction. Infrastructure jumps deflect the walk." else "Caso G: Refracci\u00f3n de Borde. La diferencia de infraestructura entre comunas desv\u00eda el \u00e1ngulo del caminar peatonal."
      } else if (case == "caso_h") {
        m_title <- if (is_en) "Case H: Location Subsidy & Social Housing" else "Caso H: Subsidio de Localizaci\u00f3n y Vivienda Social"
        m_body <- if (is_en) {
          "Modeling of public housing policies in central integrated locations. The spatial subsidy locally reduces ground friction, neutralizing the Intrinsic Tensive Norm (NTI) gradient and allowing low-income families to live and move without segregation."
        } else {
          "Modelamiento del impacto de pol\u00edticas habitacionales con localizaci\u00f3n integrada. El subsidio espacial reduce localmente la fricci\u00f3n del suelo, neutralizando el gradiente de la Norma Tensiva Intr\u00ednseca (NTI) y permitiendo que familias de menores recursos habiten y se desplacen sin segregaci\u00f3n."
        }
        audio_text <- if (is_en) "Case H: Location Subsidy. Public subsidies reduce NTI and facilitate transit." else "Caso H: Subsidio de Localizaci\u00f3n. Subsidios p\u00fablicos en zonas densas reducen la fricci\u00f3n y facilitan el transitar."
      }
      
      # Desplegar modal emergente interactivo
      showModal(modalDialog(
        title = HTML(paste0("<h4 style='color:#0284c7; font-weight:700; margin:0;'>", m_title, "</h4>")),
        tags$p(style = "font-size:1.05rem; line-height:1.5; color:#334155; margin:10px 0 0 0;", m_body),
        easyClose = TRUE,
        footer = modalButton(if (is_en) "Close / Cerrar" else "Cerrar")
      ))
      
      # Disparar reproducci\u00f3n de audio (TTS) por mensaje personalizado
      session$sendCustomMessage("speak_response", audio_text)
    }
  })
  
  # ---- CLICS EN MAPA LEAFLET ----
  observeEvent(input$leaflet_map_marker_click, {
    click <- input$leaflet_map_marker_click
    if (!is.null(click$id)) {
      if (input$click_mode == "fiche") {
        selected_pixel(click$id)
      } else if (input$click_mode == "origen") {
        updateSelectInput(session, "origen_id", selected = click$id)
      } else if (input$click_mode == "destino") {
        updateSelectInput(session, "destino_id", selected = click$id)
      } else if (input$click_mode == "distorsion") {
        m_df <- current_manzanas_df()
        m_data <- m_df[m_df$id == click$id, ]
        new_id <- paste0("dist_", format(Sys.time(), "%H%M%S"), "_", sample(100:999, 1))
        
        new_row <- data.frame(
          id = new_id, x = m_data$x, y = m_data$y, lng = m_data$lng, lat = m_data$lat,
          type = input$dist_type, mag = input$dist_mag, stringsAsFactors = FALSE
        )
        current_df <- distorsiones_df()
        distorsiones_df(rbind(current_df, new_row))
      }
    }
  })
  
  observeEvent(input$leaflet_map_click, {
    click <- input$leaflet_map_click
    if (!is.null(click) && input$click_mode == "distorsion") {
      target_crs <- if (!is.null(custom_data())) 3857 else UTM_CRS
      pts <- st_as_sf(data.frame(lng = click$lng, lat = click$lat), coords = c("lng", "lat"), crs = 4326) %>%
        st_transform(target_crs)
      utm_coords <- st_coordinates(pts)
      
      new_id <- paste0("dist_", format(Sys.time(), "%H%M%S"), "_", sample(100:999, 1))
      new_row <- data.frame(
        id = new_id, x = utm_coords[1, 1], y = utm_coords[1, 2], lng = click$lng, lat = click$lat,
        type = input$dist_type, mag = input$dist_mag, stringsAsFactors = FALSE
      )
      current_df <- distorsiones_df()
      distorsiones_df(rbind(current_df, new_row))
    }
  })
  
  observeEvent(input$clear_distorsiones, {
    distorsiones_df(data.frame(
      id = character(), x = numeric(), y = numeric(), lng = numeric(), lat = numeric(),
      type = character(), mag = character(), stringsAsFactors = FALSE
    ))
  })
  
  # Cargar datos del escenario actual del JSON (con salvaguarda de inicio para RStudio)
  escenario_actual <- reactive({
    s_time <- if (!is.null(input$scen_time)) input$scen_time else "Dia"
    s_season <- if (!is.null(input$scen_season)) input$scen_season else "Verano"
    key <- paste0(s_time, "_", s_season)
    esc <- sim_avanzada$escenarios[[key]]
    if (is.null(esc)) {
      esc <- sim_avanzada$escenarios[["Dia_Verano"]]
    }
    esc
  })
  
  # ---- RESOLVER GEODÉSICA CON DISPARO BVP E HISTORIA ----
  geodesic_traj <- eventReactive({
    input$btn_geodesica
    input$rebuild_trajectory
    recalc_trigger()
  }, {
    req(input$origen_id, input$destino_id)
    
    # --- BLOQUEO ETICO ACTIVO CARE/FAIR ---
    exp_m <- if (!is.null(input$exp_mode)) input$exp_mode else "base"
    if (identical(exp_m, "exp1") && !isTRUE(input$ruido_tactico)) {
      showModal(modalDialog(
        title = HTML("<span style='color:#dc2626; font-weight:bold;'><i class='fa fa-shield-alt'></i> ADVERTENCIA CR\u00cdTICO: Bloqueo de Privacidad CARE/FAIR</span>"),
        HTML(paste0(
          "<p>La simulaci\u00f3n ha sido <b>detenida</b> para proteger la integridad del territorio vivido.</p>",
          "<p><b>Raz\u00f3n:</b> Est\u00e1 intentando simular y visualizar trayectorias individuales de GPS peatonal (L\u00ednea de Trabajo 1) ",
          "con el <i>Ruido T\u00e1ctico</i> deshabilitado. Esto expone las trazas geogr\u00e1ficas crudas y vulnera el principio CARE de ",
          "privacidad y soberan\u00eda de datos comunitarios.</p>",
          "<p>Para reanudar la simulaci\u00f3n, debe <b>Activar el Ruido T\u00e1ctico</b> en el panel izquierdo de controles ",
          "para inyectar el enmascaramiento estoc\u00e1stico por difusi\u00f3n de Langevin (m\u00ednimo 50m) que anonimiza las trayectorias de forma irreversible.</p>",
          "<div style='background:rgba(180, 83, 9, 0.05); border:1px solid rgba(180, 83, 9, 0.2); border-radius:8px; padding:12px; font-size:0.85rem; margin-top:15px;'>",
          "<b>\u00bfQu\u00e9 son los principios CARE/FAIR?</b><br/>",
          "Los principios CARE regulan el beneficio colectivo y la autoridad de control local sobre los datos urbanos. ",
          "La planificaci\u00f3n cartesiana ex\u00f3gena no debe convertir la movilidad pedestre en un insumo de vigilancia.",
          "</div>"
        )),
        footer = tagList(
          downloadButton("download_care_guide", "Descargar Gu\u00eda de Gobernanza CARE/FAIR", class = "btn-info"),
          modalButton("Cerrar")
        ),
        easyClose = FALSE,
        size = "m"
      ))
      return(NULL)
    }
    
    m_df <- current_manzanas_df()
    if (is.null(m_df) || nrow(m_df) == 0) return(NULL)
    orig <- m_df[m_df$id == input$origen_id, ]
    dest <- m_df[m_df$id == input$destino_id, ]
    if (is.null(orig) || nrow(orig) == 0 || is.null(dest) || nrow(dest) == 0) return(NULL)
    pre <- active_surface()
    dist_df <- distorsiones_df()
    
    # Calcular distancia real para la escala
    dx_dist <- dest$x - orig$x
    dy_dist <- dest$y - orig$y
    d_total <- sqrt(dx_dist^2 + dy_dist^2)
    
    tmax_val <- max(5, min(500, 1.5 * d_total / input$v0_vital))
    dt_val <- tmax_val / 100
    
    slope_amp_val <- if (!is.null(input$pot_slope_amp)) input$pot_slope_amp else 15000
    lyap_damp <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
    
    exp_m <- if (!is.null(input$exp_mode)) input$exp_mode else "base"
    k_cons <- if (!is.null(input$k_conservation)) input$k_conservation else 2.0
    amp_sant <- if (!is.null(input$amp_santuario)) input$amp_santuario else 20000
    sig_cap <- if (!is.null(input$sigma_capital)) input$sigma_capital else 1.0
    cap_dir <- if (!is.null(input$capital_flow_direction)) input$capital_flow_direction else "rural_to_urban"
    
    if (input$solve_bvp) {
      opt_theta_local <- function(th) {
        traj <- solve_geodesic(
          pre = pre, x0 = orig$x, y0 = orig$y, theta = th, v0 = input$v0_vital,
          dest_x = dest$x, dest_y = dest$y, use_V = input$use_potentials,
          A_wall = input$pot_wall_amp, A_dest = input$pot_dest_amp,
          distorsiones = dist_df, profile = input$ped_profile, A_slope = slope_amp_val,
          omega_damping = lyap_damp, Tmax = tmax_val, dt = dt_val,
          exp_mode = exp_m, k_conservation = k_cons, amp_santuario = amp_sant,
          sigma_capital = sig_cap, capital_flow_direction = cap_dir
        )
        last_pt <- traj[nrow(traj), ]
        sqrt((last_pt$x - dest$x)^2 + (last_pt$y - dest$y)^2)
      }
      
      theta_grid <- seq(-pi, pi, length.out = 24)
      dists <- sapply(theta_grid, opt_theta_local)
      best_th <- theta_grid[which.min(dists)]
      
      opt_res <- tryCatch({
        optimize(opt_theta_local, interval = c(best_th - 0.3, best_th + 0.3))
      }, error = function(e) list(minimum = best_th))
      
      best_theta <- opt_res$minimum
    } else {
      best_theta <- atan2(dy_dist, dx_dist)
    }
    
    traj <- solve_geodesic(
      pre = pre, x0 = orig$x, y0 = orig$y, theta = best_theta, v0 = input$v0_vital,
      dest_x = dest$x, dest_y = dest$y, use_V = input$use_potentials,
      A_wall = input$pot_wall_amp, A_dest = input$pot_dest_amp,
      distorsiones = dist_df, profile = input$ped_profile, A_slope = slope_amp_val,
      omega_damping = lyap_damp, Tmax = tmax_val, dt = dt_val,
      exp_mode = exp_m, k_conservation = k_cons, amp_santuario = amp_sant,
      sigma_capital = sig_cap, capital_flow_direction = cap_dir
    )
    if (isTRUE(input$ruido_tactico)) {
      # Aplicar Donut Jittering de Langevin a la geod\u00e9sica din\u00e1mica
      jittered <- donut_jitter_langevin_vec(traj$x, traj$y, r_min = 10, r_max = 35, n_steps = 15, dt = 0.04, sigma = 8)
      traj$x <- jittered$x
      traj$y <- jittered$y
    }
    traj
  }, ignoreNULL = FALSE)
  
  # ---- APRENDIZAJE DE MEMORIA COLECTIVA PEATONAL ----
  observeEvent(geodesic_traj(), {
    traj <- geodesic_traj()
    req(traj)
    
    # LOG EVENT: Trayectoria resuelta
    is_bvp <- isTRUE(input$solve_bvp)
    mode_str <- if (is_bvp) "BVP (Dos puntos)" else "Disparo inicial (IVP)"
    add_log_event(paste0("Geod\u00e9sica Wu Wei resuelta exitosamente usando modo: ", mode_str, "."))
    
    if (isTRUE(input$ruido_tactico)) {
      add_log_event("Ruido T\u00e1ctico: difusi\u00f3n estoc\u00e1stica de Langevin aplicada de forma no-euclidiana (Donut Jittering).")
    }
    
    dest_manzanas <- current_manzanas_df()
    req(dest_manzanas)
    dest <- dest_manzanas[dest_manzanas$id == input$destino_id, ]
    req(nrow(dest) > 0)
    
    last_pt <- traj[nrow(traj), ]
    dist_final <- sqrt((last_pt$x - dest$x)^2 + (last_pt$y - dest$y)^2)
    
    # Se considera trauma si termina a mas de 150m de distancia (no llego) o vital_energy < 5
    if (dist_final > 150 || (is.numeric(last_pt$vital_energy) && last_pt$vital_energy < 5)) {
      # Fallo
      add_log_event(paste0("Alerta de Fricci\u00f3n: El peat\u00f3n no logr\u00f3 alcanzar el destino B (distancia residual: ", round(dist_final, 1), "m). Acumulando trauma de Caputo."))
      
      if (!isTRUE(input$use_collective_memory)) return()
      
      m <- matriz_memoria_trauma()
      if (is.null(m)) m <- matrix(0, 30, 30)
      
      base_data <- base_manifold_data()
      xg <- base_data$Xg
      yg <- base_data$Yg
      
      # Evaporar memoria antigua (Decaimiento Fraccionario / Olvido Natural)
      m <- m * 0.94
      
      # Sumar la huella del trauma en la grilla local
      for (k in seq_len(nrow(traj))) {
        tx <- traj$x[k]
        ty <- traj$y[k]
        
        idx_x <- which.min(abs(xg - tx))
        idx_y <- which.min(abs(yg - ty))
        
        for (i in max(1, idx_y-2):min(30, idx_y+2)) {
          for (j in max(1, idx_x-2):min(30, idx_x+2)) {
            dist_sq <- (xg[j] - tx)^2 + (yg[i] - ty)^2
            m[i, j] <- m[i, j] + 45 * exp(-dist_sq / (2 * 200^2))
          }
        }
      }
      
      m[m > 600] <- 600 # Limitar valor maximo para estabilidad
      matriz_memoria_trauma(m)
      
      # Guardar de forma eficiente y ligera en disco (JSON de ~14 KB)
      mem_path <- "www/data/pre_trained_memory.json"
      if (!file.exists(mem_path)) mem_path <- "app/www/data/pre_trained_memory.json"
      tryCatch({
        jsonlite::write_json(m, mem_path, pretty = TRUE)
      }, error = function(e) NULL)
      # Éxito
      vital_val <- if ("vital_energy" %in% colnames(last_pt) && is.numeric(last_pt$vital_energy)) {
        round(last_pt$vital_energy, 1)
      } else {
        round(sqrt(last_pt$vx^2 + last_pt$vy^2), 1) # Fallback: velocidad residual
      }
      val_label <- if ("vital_energy" %in% colnames(last_pt) && is.numeric(last_pt$vital_energy)) {
        "unidades de energ\u00eda vital"
      } else {
        "m/s de velocidad"
      }
      add_log_event(paste0("Tr\u00e1nsito Completado: El peat\u00f3n alcanz\u00f3 el destino B con ", vital_val, " ", val_label, " residual."))
    }
  })
  
  observeEvent(input$clear_collective_memory, {
    m_clean <- matrix(0, 30, 30)
    matriz_memoria_trauma(m_clean)
    
    mem_path <- "www/data/pre_trained_memory.json"
    if (!file.exists(mem_path)) mem_path <- "app/www/data/pre_trained_memory.json"
    tryCatch({
      jsonlite::write_json(m_clean, mem_path, pretty = TRUE)
    }, error = function(e) NULL)
    
    showNotification(trans("Memoria colectiva reseteada a cero y limpiada en disco.", 
                           "Collective memory reset to zero and cleared on disk."), type = "message")
  })
  
  output$collective_memory_status_ui <- renderUI({
    m <- matriz_memoria_trauma()
    total_trauma <- if (!is.null(m)) sum(m > 10) else 0
    is_en <- identical(lang(), "EN")
    
    tags$div(style = "margin-top: 5px; font-size: 0.8rem; color: #334155;",
      if (is_en) {
        HTML(paste("Learned trauma cells: <b style='color:#be123c;'>", total_trauma, "/ 900</b>"))
      } else {
        HTML(paste("Celdas de trauma aprendidas: <b style='color:#be123c;'>", total_trauma, "/ 900</b>"))
      }
    )
  })
  
  # ---- DESCRIPCIONES SEM\u00c1NTICAS NARRATIVAS ----
  output$active_case_details_ui <- renderUI({
    case <- input$narrative_case
    is_en <- identical(lang(), "EN")
    switch(case,
      "caso_a" = span(style = "color:#10b981; font-weight:600;", 
                      trans("Caso A (Mam\u00e1 de Cuidado - D\u00eda): Tr\u00e1nsito fluido por el territorio. Las geod\u00e9sicas se adaptan levemente a la infraestructura f\u00edsica y son fuertemente atra\u00eddas a ferias/salud.",
                            "Case A (Caregiving Mother - Day): Fluid transit through the territory. Geodesics adapt slightly to physical infrastructure and are strongly attracted to markets/health sites.")),
      "caso_b" = span(style = "color:#f43f5e; font-weight:600;", 
                      trans("Caso B (Trabajadora Nocturna - Invierno): Se deforma el EQT en el centro debido al fr\u00edo y peligro de delincuencia. La trayectoria realiza un amplio desv\u00edo preventivo buscando iluminaci\u00f3n.",
                            "Case B (Night Shift Worker - Winter): The EQT center deforms due to cold and risk of crime. The trajectory makes a wide preventive detour seeking street lighting.")),
      "caso_c" = span(style = "color:#e11d48; font-weight:600;", 
                      trans("Caso C (Adulto Mayor - Ladera): La geod\u00e9sica se curva siguiendo las curvas de nivel topogr\u00e1fico, evitando ascender en l\u00ednea recta por las laderas empinadas del Este.",
                            "Case C (Elderly - Slope): The geodesic curves following topographic contour lines, avoiding ascending in a straight line up the steep eastern slopes.")),
      "caso_d" = span(style = "color:#f59e0b; font-weight:600;", 
                      trans("Caso D (Escudo de Cohesi\u00f3n): Un parque autogestionado act\u00faa como atractor y sumidero de tensi\u00f3n, bloqueando y absorbiendo la distorsi\u00f3n impuesta por el capital inmobiliario.",
                            "Case D (Cohesion Shield): A self-managed park acts as an attractor and tension sink, blocking and absorbing the distortion imposed by real estate capital.")),
      "caso_e" = span(style = "color:#fbbf24; font-weight:600;", 
                      trans("Caso E (Gentrificaci\u00f3n Agresiva): Un nodo inmobiliario cr\u00edtico fractura el territorio, elevando fuertemente la fricci\u00f3n local y expulsando a la poblaci\u00f3n vulnerable.",
                            "Case E (Aggressive Gentrification): A critical real estate node fractures the territory, greatly increasing local friction and displacing vulnerable populations.")),
      "caso_f" = span(style = "color:#14b8a6; font-weight:600;", 
                      trans("Caso F (Nodo de Transporte Masivo): El metro genera un pozo gravitacional de atracci\u00f3n fuerte, facilitando el r\u00e1pido desplazamiento de perfiles j\u00f3venes.",
                            "Case F (Mass Transit Node): The metro station generates a strong gravitational well of attraction, facilitating rapid transit for younger profiles.")),
      "caso_g" = span(style = "color:#ef4444; font-weight:600;", 
                      trans("Caso G (Desastre Natural): Varios obst\u00e1culos cr\u00edticos (inundaci\u00f3n/derrumbe) bloquean los corredores en invierno nocturno, atrapando al adulto mayor.",
                            "Case G (Natural Disaster): Several critical obstacles (flooding/landslide) block the corridors on a winter night, trapping the elderly pedestrian.")),
      "caso_h" = span(style = "color:#f59e0b; font-weight:600;", 
                      trans("Caso H (Subsidio Inmobiliario Masivo): Simulaci\u00f3n de asimetr\u00eda de Lie donde el capital (X) interviene sin cohesi\u00f3n (Y) previa, deformando severamente el EQT.",
                            "Case H (Massive Real Estate Subsidy): Simulations of Lie asymmetries where capital (X) intervenes without prior cohesion (Y), severely deforming the EQT.")),
      span(trans("Modo Libre activo. Ajusta los par\u00e1metros del EQT y pincha el mapa para crear distorsiones locales (delitos, accidentes, etc.)",
                 "Free Mode active. Adjust EQT parameters and click the map to create local distortions (crimes, accidents, etc.)"))
    )
  })
  
  # ---- RETROALIMENTACI\u00d3N SINT\u00c9TICA DEL IMPACTO TOPOL\u00d3GICO ----
  output$distortion_impact_feedback <- renderUI({
    df <- distorsiones_df()
    dim_sel <- input$dim_3d
    
    if (nrow(df) == 0) {
      return(div(class = "distortion-banner-neutral",
                 tags$b("Topolog\u00eda del Manifold: "), "El territorio se encuentra en su estado de tensi\u00f3n base. Las geod\u00e9sicas siguen las deformaciones hist\u00f3ricas y f\u00edsicas de Pe\u00f1alol\u00e9n sin perturbaciones locales artificiales."))
    }
    
    total_obs <- sum(df$type %in% c("obstacle", "delito", "accidente"))
    total_attr <- sum(df$type %in% c("compras", "seguridad"))
    
    impact_texts <- c()
    for (i in seq_len(nrow(df))) {
      t <- df$type[i]
      mag <- df$mag[i]
      
      mag_h <- switch(mag, "leve" = "80m", "moderada" = "150m", "severa" = "250m", "critica" = "400m", "150m")
      mag_w <- switch(mag, "leve" = "120m", "moderada" = "220m", "severa" = "350m", "critica" = "500m", "250m")
      
      type_desc <- switch(t,
        "delito" = paste0("Un evento de Delito (", mag, ") genera un pico repulsivo de ", mag_h, " de altura. El territorio se deforma hacia arriba, creando una colina de fricci\u00f3n social."),
        "accidente" = paste0("Un Accidente vial (", mag, ") bloquea el espacio geom\u00e9trico local con un pico de fricci\u00f3n de ", mag_h, "."),
        "obstacle" = paste0("Un Obst\u00e1culo de infraestructura (", mag, ") eleva la resistencia local del territorio."),
        "compras" = paste0("Una Feria libre / Comercio (", mag, ") deforma el EQT hacia abajo, creando un pozo de bienestar atractor con un radio de influencia de ", mag_w, "."),
        "seguridad" = paste0("Un Corredor Seguro (", mag, ") reduce localmente la fricci\u00f3n en un radio de ", mag_w, ", atrayendo la geod\u00e9sica y aliviando la fatiga peatonal."),
        paste0("Distorsi\u00f3n tipo '", t, "' (", mag, ") perturba el EQT.")
      )
      impact_texts <- c(impact_texts, type_desc)
    }
    
    physical_effect <- ""
    if (total_obs > 0 && total_attr > 0) {
      physical_effect <- "Las geod\u00e9sicas resultantes se curvan preventivamente rodeando las colinas de peligro y desliz\u00e1ndose a trav\u00e9s de los valles atractores de cohesi\u00f3n."
    } else if (total_obs > 0) {
      physical_effect <- "El caminante se ve obligado a gastar m\u00e1s energ\u00eda ('Voluntad') o a curvar su trayectoria para evadir los picos de repulsi\u00f3n de la ladera."
    } else if (total_attr > 0) {
      physical_effect <- "El territorio act\u00faa como un embudo gravitacional territorial, facilitando el tr\u00e1nsito de la trayectoria y atrayendo la geod\u00e9sica."
    }
    
    div(class = "distortion-banner-active",
        tags$b("Impacto Territorial Din\u00e1mico: "),
        tags$ul(style = "margin: 5px 0; padding-left: 15px; font-size:0.95rem;", 
                lapply(impact_texts, function(txt) tags$li(txt))),
        tags$p(style = "margin-top: 5px; font-size:0.95rem; font-style: italic; color: #f43f5e; font-weight:600;", physical_effect)
    )
  })
  
  # ---- RENDER DE KPIS ----
  output$kpi_fric <- renderUI({
    esc <- escenario_actual()
    div(class = "kpi-val", round(esc$avg_friccion_global, 1))
  })
  output$kpi_tors <- renderUI({
    esc <- escenario_actual()
    div(class = "kpi-val", round(esc$avg_torsion_global, 3))
  })
  output$kpi_eff <- renderUI({
    esc <- escenario_actual()
    traj_reached <- sum(sapply(esc$trayectorias$reached, isTRUE))
    ratio <- traj_reached / length(esc$trayectorias$reached) * 100
    div(class = "kpi-val", paste0(round(ratio, 0), "%"))
  })
  
  # ---- FICHA DEL SOLUCIONADOR BVP ----
  output$bvp_metrics_card <- renderUI({
    traj <- geodesic_traj()
    if (is.null(traj) || nrow(traj) <= 1) return(NULL)
    
    last_pt <- traj[nrow(traj), ]
    m_df <- current_manzanas_df()
    dest <- m_df[m_df$id == input$destino_id, ]
    dist_error <- sqrt((last_pt$x - dest$x)^2 + (last_pt$y - dest$y)^2)
    
    reached_status <- if (dist_error < 400) {
      span("CONVERGENCIA (Wu Wei)", style = "color: #10b981; font-weight: bold;")
    } else {
      span("FATIGA PEATONAL (No alcanz\u00f3 B)", style = "color: #ef4444; font-weight: bold;")
    }
    
    div(style = "background: rgba(56, 189, 248, 0.04); border: 1px solid rgba(56, 189, 248, 0.2); border-radius: 8px; padding: 12px; margin-top: 10px;",
      fluidRow(
        column(4, tags$p(style = "font-size: 0.95rem; margin:0;", tags$b("Estado: "), reached_status)),
        column(4, tags$p(style = "font-size: 0.95rem; margin:0;", tags$b("Error: "), round(dist_error, 1), " metros")),
        column(4, tags$p(style = "font-size: 0.95rem; margin:0;", tags$b("Pasos ODE: "), nrow(traj)))
      )
    )
  })
  
  # ---- DIAGN\u00d3STICO DE REFRACCI\u00d3N DE CAPITAL (SNELL) ----
  output$snell_diagnostic_panel <- renderUI({
    traj <- geodesic_traj()
    if (is.null(traj) || nrow(traj) < 4) {
      return(p(style = "color:#475569;", "No hay trayectoria activa suficiente para el an\u00e1lisis."))
    }
    
    # Encontrar si cruza x = 352800
    x_vals <- traj$x
    crossings <- which((x_vals[-1] > 352800 & x_vals[-length(x_vals)] <= 352800) |
                       (x_vals[-1] < 352800 & x_vals[-length(x_vals)] >= 352800))
    
    if (length(crossings) == 0) {
      return(div(class = "alert alert-warning", style = "background: rgba(239,68,68,0.05); border: 1px solid rgba(239,68,68,0.2); color:#334155; padding: 12px; border-radius: 8px;",
        p(style = "margin: 0;", "\u26a0\ufe0f La geod\u00e9sica no cruza el L\u00edmite Urbano de Capital en x = 352800. Ubique el Origen A y Destino B a lados opuestos de este l\u00edmite para observar la refracci\u00f3n de inversi\u00f3n.")
      ))
    }
    
    k <- crossings[1] + 1
    
    if (x_vals[k] > x_vals[k-1]) {
      idx_rural_1 <- max(1, k-3)
      idx_rural_2 <- k-1
      idx_urban_1 <- k
      idx_urban_2 <- min(nrow(traj), k+2)
    } else {
      idx_urban_1 <- max(1, k-3)
      idx_urban_2 <- k-1
      idx_rural_1 <- k
      idx_rural_2 <- min(nrow(traj), k+2)
    }
    
    dx_rural <- traj$x[idx_rural_2] - traj$x[idx_rural_1]
    dy_rural <- traj$y[idx_rural_2] - traj$y[idx_rural_1]
    dx_urban <- traj$x[idx_urban_2] - traj$x[idx_urban_1]
    dy_urban <- traj$y[idx_urban_2] - traj$y[idx_urban_1]
    
    theta_rural <- atan2(abs(dy_rural), abs(dx_rural))
    theta_urban <- atan2(abs(dy_urban), abs(dx_urban))
    
    sin_rural <- sin(theta_rural)
    sin_urban <- sin(theta_urban)
    
    ratio_obs <- if (sin_urban > 1e-5) sin_rural / sin_urban else NA
    
    R_ratio <- if (!is.null(input$g_urban_ratio)) input$g_urban_ratio else 8.0
    ratio_teor <- sqrt(R_ratio)
    
    err_pct <- if (!is.na(ratio_obs)) abs(ratio_obs - ratio_teor) / ratio_teor * 100 else NA
    
    explicacion <- paste(
      "Al cruzar el l\u00edmite urbano, la geod\u00e9sica sufre un quiebre de refracci\u00f3n an\u00e1logo a la ley de Snell en \u00f3ptica.",
      "Dado que la m\u00e9trica urbana es mayor (g =", R_ratio, "vs g = 1.0), el espacio en el lado urbano se percibe como 'm\u00e1s denso' y con menor velocidad de avance.",
      "Por lo tanto, la trayectoria se desv\u00eda buscando aproximarse a la normal del l\u00edmite para reducir la distancia en el medio denso."
    )
    
    div(
      p(style = "color:#1e293b; font-size:1.1rem; margin-bottom: 12px;", explicacion),
      fluidRow(
        column(4,
          div(style = "background: rgba(255,255,255,0.03); border: 1px solid rgba(255,255,255,0.08); padding: 10px; border-radius: 6px; text-align: center;",
            span(style = "display:block; color:#475569; font-size:0.9rem;", "\u00c1ngulo Rural (\u03b8_rural)"),
            span(style = "font-size:1.4rem; font-weight:bold; color:#10b981;", paste0(round(theta_rural * 180 / pi, 1), "\u00b0"))
          )
        ),
        column(4,
          div(style = "background: rgba(255,255,255,0.03); border: 1px solid rgba(255,255,255,0.08); padding: 10px; border-radius: 6px; text-align: center;",
            span(style = "display:block; color:#475569; font-size:0.9rem;", "\u00c1ngulo Urbano (\u03b8_urbano)"),
            span(style = "font-size:1.4rem; font-weight:bold; color:#0ea5e9;", paste0(round(theta_urban * 180 / pi, 1), "\u00b0"))
          )
        ),
        column(4,
          div(style = "background: rgba(255,255,255,0.03); border: 1px solid rgba(255,255,255,0.08); padding: 10px; border-radius: 6px; text-align: center;",
            span(style = "display:block; color:#475569; font-size:0.9rem;", "Desviaci\u00f3n angular (\u0394\u03b8)"),
            span(style = "font-size:1.4rem; font-weight:bold; color:#c084fc;", paste0(round(abs(theta_rural - theta_urban) * 180 / pi, 1), "\u00b0"))
          )
        )
      ),
      br(),
      fluidRow(
        column(6,
          div(style = "background: rgba(16,185,129,0.05); border: 1px solid rgba(16,185,129,0.2); padding: 12px; border-radius: 8px;",
            h4(style = "margin-top:0; color:#10b981; font-size:1.05rem;", "Ley de Snell Observada"),
            p(style = "font-size:1.25rem; font-weight:bold; margin-bottom: 2px; color:#ffffff;", 
              if (is.na(ratio_obs)) "N/A" else paste0("sin(\u03b8_r) / sin(\u03b8_u) = ", round(ratio_obs, 3))),
            span(style = "color:#475569; font-size:0.85rem;", "Relaci\u00f3n emp\u00edrica calculada localmente")
          )
        ),
        column(6,
          div(style = "background: rgba(56,189,248,0.05); border: 1px solid rgba(56,189,248,0.2); padding: 12px; border-radius: 8px;",
            h4(style = "margin-top:0; color:#38bdf8; font-size:1.05rem;", "Relaci\u00f3n Te\u00f3rica Esperada"),
            p(style = "font-size:1.25rem; font-weight:bold; margin-bottom: 2px; color:#ffffff;", 
              paste0("\u221aR_ratio = \u221a", R_ratio, " = ", round(ratio_teor, 3))),
            span(style = "color:#475569; font-size:0.85rem;", "Previsi\u00f3n anal\u00edtica de la geometr\u00eda")
          )
        )
      ),
      if (!is.na(err_pct)) {
        div(style = "margin-top: 12px; text-align: right; font-size: 0.9rem; color: #a1a1aa;",
          paste0("Ajuste del resolvedor num\u00e9rico: ", round(100 - err_pct, 1), "% (Error relativo: ", round(err_pct, 2), "%)")
        )
      } else NULL
    )
  })
  
  # ---- TABLA DE DISTORSIONES EN SIDEBAR ----
  output$distorsiones_table <- renderTable({
    df <- distorsiones_df()
    if (nrow(df) == 0) return(data.frame(Mensaje = "Sin distorsiones activas."))
    
    tipo_trad <- sapply(df$type, function(t) {
      switch(t,
             "delito" = "Delito (-)",
             "accidente" = "Accidente (-)",
             "compras" = "Feria (+)",
             "seguridad" = "Corredor (+)",
             "obstacle" = "Obst\u00e1culo (-)",
             t)
    })
    data.frame(
      N = seq_len(nrow(df)),
      Tipo = tipo_trad,
      Mag = tools::toTitleCase(df$mag)
    )
  }, spacing = "s", align = "c", class = "table table-dark table-striped")
  
  # ---- LEAFLET 2D MAP ----
  output$leaflet_map <- renderLeaflet({
    m_df <- current_manzanas_df()
    all_gent <- unlist(lapply(current_graph_data()$tensorMemoria, function(df) df$tensionGentrificacion))
    pal_domain <- range(all_gent, na.rm = TRUE)
    pal <- colorNumeric(palette = "YlOrRd", domain = pal_domain)
    
    m <- leaflet(m_df) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro (Dark)") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Calles (Streets)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Sat\u00e9lite (Satellite)") %>%
      setView(lng = mean(m_df$lng), lat = mean(m_df$lat), zoom = 14) %>%
      addLayersControl(
        baseGroups = c("Oscuro (Dark)", "Calles (Streets)", "Sat\u00e9lite (Satellite)"),
        overlayGroups = c(
          "Red de Cuidado (Care Network)",
          "Barreras/Bordes (Experiments)",
          "Flujos de Fondo (Background Flows)",
          "Trayectorias/Geodesicas (Trajectories)",
          "Distorsiones (Distortion Markers)"
        ),
        options = layersControlOptions(collapsed = TRUE)
      )
    
    # Agregar polil\u00edneas de la Red de Cuidado Comunitaria de forma segura
    links <- current_care_links()
    if (length(links$lng) > 0) {
      m <- m %>% addPolylines(
        lng = links$lng,
        lat = links$lat,
        color = "#10b981",
        weight = 2,
        opacity = 0.35,
        group = "Red de Cuidado (Care Network)",
        label = "Enlaces de Cuidado Comunitario (Manzanas Unidas)"
      )
    }
    
    # Agregar la barrera urbana del Canal y Av. Tobalaba
    x_range <- range(m_df$x)
    if (x_range[1] <= 352500 && x_range[2] >= 352500) {
      m <- m %>% addPolylines(
        lng = c(wall_p1[1], wall_p2[1]),
        lat = c(wall_p1[2], wall_p2[2]),
        color = "#ef4444",
        weight = 3.5,
        opacity = 0.65,
        label = "Barrera Canal / Av. Tobalaba"
      ) %>%
      # Agregar cruces peatonales habilitados (puentes)
      addCircleMarkers(
        lng = c(bridge1[1], bridge2[1]),
        lat = c(bridge1[2], bridge2[2]),
        radius = 8,
        color = "#14b8a6",
        fillColor = "#14b8a6",
        fillOpacity = 1.0,
        weight = 2,
        group = "bridges",
        label = c("Cruce Habilitado Grecia", "Cruce Habilitado Arrieta")
      )
    }
    
    # Agregar l\u00edmite del Santuario (l\u00ednea verde en x = 354000)
    if (x_range[1] <= 354000 && x_range[2] >= 354000) {
      y_seq <- seq(min(m_df$y) - 500, max(m_df$y) + 500, length.out = 10)
      x_santuario <- rep(354000, length(y_seq))
      wgs_santuario <- utm_to_wgs84_vector(x_santuario, y_seq)
      m <- m %>% addPolylines(
        lng = wgs_santuario$lng, lat = wgs_santuario$lat,
        color = "#10b981", weight = 3, opacity = 0.8,
        dashArray = "5, 5",
        label = "L\u00edmite Santuario Natural (x = 354000)",
        group = "Barreras/Bordes (Experiments)"
      )
    }
    
    # Agregar l\u00edmite urbano (l\u00ednea roja en x = 352800)
    if (x_range[1] <= 352800 && x_range[2] >= 352800) {
      y_seq <- seq(min(m_df$y) - 500, max(m_df$y) + 500, length.out = 10)
      x_limite <- rep(352800, length(y_seq))
      wgs_limite <- utm_to_wgs84_vector(x_limite, y_seq)
      m <- m %>% addPolylines(
        lng = wgs_limite$lng, lat = wgs_limite$lat,
        color = "#f43f5e", weight = 3, opacity = 0.8,
        dashArray = "5, 5",
        label = "L\u00edmite Urbano de Capital (x = 352800)",
        group = "Barreras/Bordes (Experiments)"
      )
    }
    
    m <- m %>%
      leaflet::addLegend(
        pal = pal,
        values = pal_domain,
        title = "Tensi\u00f3n Gentrificaci\u00f3n",
        position = "bottomright",
        layerId = "map_legend"
      ) %>%
      leaflet::addLegend(
        position = "bottomleft",
        colors = c("#f59e0b", "#ec4899", "#94a3b8"),
        labels = c("Trayectoria Wu Wei (Geod\u00e9sica)", "Distorsiones (Eventos)", "L\u00ednea Euclidiana"),
        title = "Elementos Espaciales"
      )
    
    m
  })
  
  # Dibujar c\u00edrculos de manzanas en 2D con visualizaci\u00f3n de Red de Cuidado
  observe({
    map_df <- manzanas_con_tensores()
    if (is.null(map_df) || nrow(map_df) == 0) return()
    
    # Aplicar Donut Jittering de Langevin en modo Noche para privacidad
    is_noche <- isTRUE(input$scen_time == "Noche")
    if (is_noche) {
      jit_coords <- donut_jitter_langevin_vec(map_df$x, map_df$y, r_min = 15, r_max = 45, n_steps = 20, dt = 0.05, sigma = 10)
      if (!is.null(custom_data())) {
        pts_metric <- sf::st_as_sf(data.frame(x = jit_coords$x, y = jit_coords$y), coords = c("x", "y"), crs = 3857)
        pts_wgs <- sf::st_transform(pts_metric, 4326)
        coords_wgs <- sf::st_coordinates(pts_wgs)
        map_df$lng <- coords_wgs[, 1]
        map_df$lat <- coords_wgs[, 2]
      } else {
        jit_wgs <- utm_to_wgs84_vector(jit_coords$x, jit_coords$y)
        map_df$lng <- jit_wgs$lng
        map_df$lat <- jit_wgs$lat
      }
    }
    
    # Destacar visualmente manzanas de cuidado (Red de Cuidado)
    if (!"red_cuidado" %in% colnames(map_df)) {
      map_df$red_cuidado <- "Ninguno"
    }
    map_df$color_border <- ifelse(map_df$red_cuidado != "Ninguno", "#10b981", "#14b8a6")
    map_df$size_marker <- ifelse(map_df$red_cuidado != "Ninguno", 6.0, 3.5)
    map_df$weight_border <- ifelse(map_df$red_cuidado != "Ninguno", 1.5, 0.5)
    
    # Calcular encogimiento de Ledoit-Wolf local para cada manzana
    pre_geom <- active_surface()
    shrinkage_vals <- sapply(seq_len(nrow(map_df)), function(idx) {
      get_z_height_scalar(map_df$x[idx], map_df$y[idx], pre_geom$shrinkage_map, pre_geom$Xg, pre_geom$Yg)
    })
    map_df$shrinkage <- shrinkage_vals
    
    # Unir datos IPF para popups
    map_df <- merge(map_df, current_ipf_calib_data(), by = "id", all.x = TRUE)
    
    # Lógica de coloreo (Heatmap)
    heatmap_sel <- if (!is.null(input$visual_heatmap)) input$visual_heatmap else "none"
    is_blind <- isTRUE(input$mode_solo_blind)
    
    # 5. Campos vectoriales interactivos de Sentido y NTI
    pre_geom_v <- active_surface()
    dest_v <- map_df[map_df$id == input$destino_id, ]
    dist_df_v <- distorsiones_df()
    
    map_df$fx <- 0
    map_df$fy <- 0
    map_df$f_mag <- 0
    map_df$f_angle <- 0
    map_df$vx_end <- map_df$x
    map_df$vy_end <- map_df$y
    
    if (!is.null(pre_geom_v) && !is.null(input$visual_vectorfield)) {
      # Calcular fuerzas para todas las manzanas
      for (i in seq_len(nrow(map_df))) {
        cx <- map_df$x[i]
        cy <- map_df$y[i]
        
        # Gradiente de pared (tobalaba)
        x_wall <- 352500
        w_width <- 70
        dist_to_bridge1 <- abs(cy - 6294400)
        dist_to_bridge2 <- abs(cy - 6295300)
        bridge_mult <- min(1.0, min(dist_to_bridge1, dist_to_bridge2) / 200)
        V_w <- input$pot_wall_amp * bridge_mult * exp(- (cx - x_wall)^2 / (2 * w_width^2))
        dV_w_dx <- - ((cx - x_wall) / (w_width^2)) * V_w
        closest_bridge_y <- if (dist_to_bridge1 < dist_to_bridge2) 6294400 else 6295300
        dV_w_dy <- if (min(dist_to_bridge1, dist_to_bridge2) < 200) {
          input$pot_wall_amp * exp(- (cx - x_wall)^2 / (2 * w_width^2)) * sign(cy - closest_bridge_y) / 200
        } else {
          0
        }
        
        # Gradiente de destino
        dV_d_dx <- 0; dV_d_dy <- 0
        if (!is.null(dest_v$x) && !is.null(dest_v$y)) {
          r_dest <- 400
          dist_sq <- (cx - dest_v$x)^2 + (cy - dest_v$y)^2
          V_d <- - input$pot_dest_amp * exp(- dist_sq / (2 * r_dest^2))
          dV_d_dx <- - ((cx - dest_v$x) / (r_dest^2)) * V_d
          dV_d_dy <- - ((cy - dest_v$y) / (r_dest^2)) * V_d
        }
        
        # Gradiente de pendiente física
        dV_slope_dx <- 0; dV_slope_dy <- 0
        if (input$ped_profile == "mayor") {
          loc_base <- findInterval(cx, pre_geom_v$Xg); if (loc_base < 1) loc_base <- 1
          loc_base_y <- findInterval(cy, pre_geom_v$Yg); if (loc_base_y < 1) loc_base_y <- 1
          dAlt_dx_loc <- pre_geom_v$GammaPacked[loc_base, loc_base_y, 10]
          dAlt_dy_loc <- pre_geom_v$GammaPacked[loc_base, loc_base_y, 11]
          slope_amp_val <- if (!is.null(input$pot_slope_amp)) input$pot_slope_amp else 5000
          dV_slope_dx <- slope_amp_val * dAlt_dx_loc
          dV_slope_dy <- slope_amp_val * dAlt_dy_loc
        }
        
        # Gradiente de distorsiones locales
        dV_dist_dx <- 0; dV_dist_dy <- 0
        if (nrow(dist_df_v) > 0) {
          for (k in seq_len(nrow(dist_df_v))) {
            d_x <- dist_df_v$x[k]
            d_y <- dist_df_v$y[k]
            d_type <- dist_df_v$type[k]
            d_mag <- dist_df_v$mag[k]
            is_repulsive <- d_type %in% c("obstacle", "delito", "accidente")
            sign_val <- if (is_repulsive) 1.0 else -1.0
            sens_mult <- 1.0
            if (input$ped_profile == "cuidado") {
              if (d_type == "delito") sens_mult <- 1.8
              if (d_type == "compras") sens_mult <- 1.6
            } else if (input$ped_profile == "nocturna") {
              if (d_type == "delito") sens_mult <- 2.8
              if (d_type == "seguridad") sens_mult <- 2.2
            } else if (input$ped_profile == "mayor") {
              if (d_type %in% c("delito", "muro", "barrera", "obstaculo")) sens_mult <- 3.5
            }
            mag_params <- switch(d_mag,
                                 "leve"     = list(r = 100, amp = 4000),
                                 "moderada" = list(r = 200, amp = 12000),
                                 "severa"   = list(r = 350, amp = 30000),
                                 "critica"  = list(r = 500, amp = 70000),
                                 list(r = 200, amp = 12000))
            r_dist <- mag_params$r
            dist_amp_val <- mag_params$amp * sens_mult
            dist_sq_pt <- (cx - d_x)^2 + (cy - d_y)^2
            V_dist <- sign_val * dist_amp_val * exp(- dist_sq_pt / (2 * r_dist^2))
            dV_dist_dx <- dV_dist_dx - ((cx - d_x) / (r_dist^2)) * V_dist
            dV_dist_dy <- dV_dist_dy - ((cy - d_y) / (r_dist^2)) * V_dist
          }
        }
        
        # Seleccionar componentes vectoriales según control
        if (input$visual_vectorfield == "sentido") {
          fx <- - (dV_w_dx + dV_d_dx + dV_slope_dx + dV_dist_dx)
          fy <- - (dV_w_dy + dV_d_dy + dV_slope_dy + dV_dist_dy)
        } else if (input$visual_vectorfield == "grad_nti") {
          if (nrow(dist_df_v) > 0) {
            idx_close <- which.min((cx - dist_df_v$x)^2 + (cy - dist_df_v$y)^2)
            dx_dist <- dist_df_v$x[idx_close] - cx
            dy_dist <- dist_df_v$y[idx_close] - cy
            dist_len <- sqrt(dx_dist^2 + dy_dist^2)
            if (dist_len > 0) {
              fx <- (dx_dist / dist_len) * map_df$nti_val[i] * 3500
              fy <- (dy_dist / dist_len) * map_df$nti_val[i] * 3500
            } else {
              fx <- 0; fy <- 0
            }
          } else {
            fx <- 0; fy <- 0
          }
        } else {
          fx <- 0; fy <- 0
        }
        
        f_mag <- sqrt(fx^2 + fy^2)
        map_df$fx[i] <- fx
        map_df$fy[i] <- fy
        map_df$f_mag[i] <- f_mag
        map_df$f_angle[i] <- if (f_mag > 1e-3) atan2(fy, fx) * 180 / pi else 0
        
        if (f_mag > 1e-3) {
          scale_fac <- (20 + 35 * log1p(f_mag)) / f_mag
          map_df$vx_end[i] <- cx + fx * scale_fac
          map_df$vy_end[i] <- cy + fy * scale_fac
        }
      }
    }
    
    # Lógica de coloreo (Heatmap)
    heatmap_sel <- if (!is.null(input$visual_heatmap)) input$visual_heatmap else "none"
    is_blind <- isTRUE(input$mode_solo_blind)
    
    if (is_blind) {
      map_df$marker_fill <- "#475569"
    } else if (heatmap_sel == "nti") {
      log_nti <- log10(map_df$nti_val + 1)
      pal_nti <- colorNumeric(palette = "magma", domain = range(log_nti, na.rm = TRUE))
      map_df$marker_fill <- pal_nti(log_nti)
    } else if (heatmap_sel == "ricci") {
      r_range <- max(abs(map_df$ricci_val), na.rm = TRUE)
      if (r_range == 0) r_range <- 1
      pal_ricci <- colorNumeric(palette = "RdBu", domain = c(-r_range, r_range), reverse = TRUE)
      map_df$marker_fill <- pal_ricci(map_df$ricci_val)
    } else if (heatmap_sel == "trauma") {
      pal_trauma <- colorNumeric(palette = "Oranges", domain = range(map_df$trauma_val, na.rm = TRUE))
      map_df$marker_fill <- pal_trauma(map_df$trauma_val)
    } else {
      g_data <- current_graph_data()
      if (!is.null(g_data) && !is.null(g_data$tensorMemoria)) {
        all_gent <- unlist(lapply(g_data$tensorMemoria, function(df) df$tensionGentrificacion))
      } else {
        all_gent <- map_df$gentrificacion
      }
      if (length(all_gent) == 0 || all(is.na(all_gent))) {
        all_gent <- c(0, 1)
      }
      pal_default <- colorNumeric(palette = "YlOrRd", domain = range(all_gent, na.rm = TRUE))
      map_df$marker_fill <- pal_default(map_df$gentrificacion)
    }
    
    # Alerta visual de Lie conmutador (borde rojo grueso si show_lie_conflicts esta activo)
    if (isTRUE(input$show_lie_conflicts)) {
      map_df$color_border <- ifelse(map_df$lie_val > 0.06, "#ef4444", map_df$color_border)
      map_df$weight_border <- ifelse(map_df$lie_val > 0.06, 2.5, map_df$weight_border)
    }
    
    map_df$marker_popup <- sapply(seq_len(nrow(map_df)), function(i) {
      if (is_blind) {
        if (identical(lang(), "EN")) {
          paste0("<div style='font-family: Inter, sans-serif; font-size:12px; color:#1e293b; padding:5px;'>",
                 "<b>Blind Field Mode Active</b><br>",
                 "Theoretical overlays are hidden to prevent confirmation bias. ",
                 "Please complete the IEO sidewalk forms in situ.",
                 "</div>")
        } else {
          paste0("<div style='font-family: Inter, sans-serif; font-size:12px; color:#1e293b; padding:5px;'>",
                 "<b>Modo Solo (Auto-Ceguera) Activo</b><br>",
                 "Las capas te\u00f3ricas est\u00e1n ocultas para evitar sesgos de confirmaci\u00f3n durante su levantamiento. ",
                 "Por favor, complete las fichas peatonales IEO in situ.",
                 "</div>")
        }
      } else {
        # Determine status badge
        gent_val <- map_df$gentrificacion[i]
        coh_val <- map_df$cohesion[i]
        
        status_badge <- if (gent_val > coh_val + 0.15) {
          if (identical(lang(), "EN")) {
            "<span style='background-color:#fee2e2; color:#ef4444; border:1px solid #fecaca; border-radius:3px; padding:1px 4px; font-size:9px; font-weight:700; letter-spacing:0.5px;'>GENTRIFICATION RISK</span>"
          } else {
            "<span style='background-color:#fee2e2; color:#ef4444; border:1px solid #fecaca; border-radius:3px; padding:1px 4px; font-size:9px; font-weight:700; letter-spacing:0.5px;'>RIESGO GENTRIFICACI\u00d3N</span>"
          }
        } else if (coh_val > gent_val + 0.15) {
          if (identical(lang(), "EN")) {
            "<span style='background-color:#d1fae5; color:#10b981; border:1px solid #a7f3d0; border-radius:3px; padding:1px 4px; font-size:9px; font-weight:700; letter-spacing:0.5px;'>COHESIVE SHELTER</span>"
          } else {
            "<span style='background-color:#d1fae5; color:#10b981; border:1px solid #a7f3d0; border-radius:3px; padding:1px 4px; font-size:9px; font-weight:700; letter-spacing:0.5px;'>AMPARO COHESIVO</span>"
          }
        } else {
          if (identical(lang(), "EN")) {
            "<span style='background-color:#e0f2fe; color:#0369a1; border:1px solid #bae6fd; border-radius:3px; padding:1px 4px; font-size:9px; font-weight:700; letter-spacing:0.5px;'>STABLE INTERFACE</span>"
          } else {
            "<span style='background-color:#e0f2fe; color:#0369a1; border:1px solid #bae6fd; border-radius:3px; padding:1px 4px; font-size:9px; font-weight:700; letter-spacing:0.5px;'>INTERFAZ ESTABLE</span>"
          }
        }
        
        # Calcular métrica de costo vital disipado
        vital_cost_est <- round(gent_val * 42.5 + map_df$altitud[i] * 0.05, 1)
        
        # Build translated popup with intrinsic operators
        if (identical(lang(), "EN")) {
          paste0(
            "<div style='font-family: Inter, system-ui, sans-serif; font-size:12px; color:#1e293b; padding:4px; min-width:210px; line-height:1.4;'>",
            "<div style='display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;'>",
            "<h5 style='font-weight:700; margin:0; color:#0ea5e9; font-size:13px;'>Ontological Pixel ", map_df$id[i], "</h5>",
            status_badge,
            "</div>",
            "<b>Physical Relief:</b> Altitude: ", round(map_df$altitud[i], 1), " m | NDVI: ", round(map_df$ndvi[i], 3), "<br>",
            "<b>Care Infrastructure:</b> Kitchens: ", map_df$red_cuidado[i], "<br>",
            "<b>Tension Vectors:</b> Speculation (P): ", round(gent_val, 3), " | Cohesion (\u03a9): ", round(coh_val, 3), "<br>",
            "<b>Ledoit-Wolf &alpha;:</b> ", round(map_df$shrinkage[i], 4), " | <b>Vital Energy Cost:</b> ", vital_cost_est, " kcal/m<br>",
            "<b>Vector Force:</b> ", round(map_df$f_mag[i], 1), " N at ", round(map_df$f_angle[i]), "\u00b0<br>",
            "<hr style='margin:4px 0; border-color:#cbd5e1;'>",
            "<b>NTI ||T||:</b> ", round(map_df$nti_val[i], 2), " | <b>Ricci R:</b> ", round(map_df$ricci_val[i], 2), "<br>",
            "<b>Lie [P, \u03a9]:</b> ", round(map_df$lie_val[i], 3), " | <b>Trauma (Caputo):</b> ", round(map_df$trauma_val[i], 1), "<br>",
            "<p style='font-size:10px; font-style:italic; color:#0f766e; margin:4px 0 0 0; line-height:1.2; font-weight: 500;'>",
            if (heatmap_sel == "nti") {
              "NTI: Visualizing dynamic territorial stress density."
            } else if (heatmap_sel == "ricci") {
              if (map_df$ricci_val[i] > 0.1) "Ricci Peak: Coercive exclusion ripping the metric fabric."
              else "Ricci Valley: Cohesive shelter damping daily travel curvature."
            } else if (heatmap_sel == "trauma") {
              "Caputo Memory: Accumulated local collective trauma trace."
            } else if (isTRUE(input$show_lie_conflicts) && map_df$lie_val[i] > 0.06) {
              "Lie Dispute: Non-commutative sequential actions conflict!"
            } else {
              if (gent_val > 0.6) "Compression Space: Speculative forces deform the local metric."
              else "Shelter Space: Mutual aid networks damp the curvature of daily transport."
            },
            "</p></div>"
          )
        } else {
          paste0(
            "<div style='font-family: Inter, system-ui, sans-serif; font-size:12px; color:#1e293b; padding:4px; min-width:210px; line-height:1.4;'>",
            "<div style='display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;'>",
            "<h5 style='font-weight:700; margin:0; color:#0ea5e9; font-size:13px;'>P\u00edxel Ontol\u00f3gico ", map_df$id[i], "</h5>",
            status_badge,
            "</div>",
            "<b>Nivel F\u00edsico:</b> Altitud: ", round(map_df$altitud[i], 1), " m | NDVI: ", round(map_df$ndvi[i], 3), "<br>",
            "<b>Redes de Cuidado:</b> Comedores: ", map_df$red_cuidado[i], "<br>",
            "<b>Vectores de Tensi\u00f3n:</b> Fricci\u00f3n (P): ", round(gent_val, 3), " | Cohesi\u00f3n (\u03a9): ", round(coh_val, 3), "<br>",
            "<b>Ledoit-Wolf &alpha;:</b> ", round(map_df$shrinkage[i], 4), " | <b>Costo Energ\u00eda Vital:</b> ", vital_cost_est, " kcal/m<br>",
            "<b>Fuerza Vectorial:</b> ", round(map_df$f_mag[i], 1), " N a ", round(map_df$f_angle[i]), "\u00b0<br>",
            "<hr style='margin:4px 0; border-color:#cbd5e1;'>",
            "<b>NTI ||T||:</b> ", round(map_df$nti_val[i], 2), " | <b>Ricci R:</b> ", round(map_df$ricci_val[i], 2), "<br>",
            "<b>Lie [P, \u03a9]:</b> ", round(map_df$lie_val[i], 3), " | <b>Trauma (Caputo):</b> ", round(map_df$trauma_val[i], 1), "<br>",
            "<p style='font-size:10px; font-style:italic; color:#0f766e; margin:4px 0 0 0; line-height:1.2; font-weight: 500;'>",
            if (heatmap_sel == "nti") {
              "Modo NTI: Tensi\u00f3n acumulada por desequilibrio de tensores locales."
            } else if (heatmap_sel == "ricci") {
              if (map_df$ricci_val[i] > 0.1) "Pico de Ricci: desgarro metrol\u00f3gico coercitivo de exclusi\u00f3n."
              else "Valle de Ricci: amparo cohesivo que amortigua la curvatura m\u00e9trica."
            } else if (heatmap_sel == "trauma") {
              "Memoria de Caputo: Hotspot de trauma colectivo residual activo."
            } else if (isTRUE(input$show_lie_conflicts) && map_df$lie_val[i] > 0.06) {
              "Alerta Lie: \u00a1Las acciones no conmutan! Disputa secuencial activa."
            } else {
              if (gent_val > 0.6) "Espacio de Compresi\u00f3n: Las fuerzas de especulaci\u00f3n gentrificadora deforman el relieve."
              else "Espacio de Amparo: Las redes vecinales amortiguan la curvatura del tr\u00e1nsito."
            },
            "</p></div>"
          )
        }
      }
    })

    if (!is.null(map_df) && nrow(map_df) > 0) {
      leaflet_proxy_manzanas <- leafletProxy("leaflet_map", data = map_df) %>%
        clearMarkers() %>%
        clearGroup("vectores_IPF") %>%
        addCircleMarkers(
          ~lng, ~lat,
          layerId = ~id,
          radius = ~size_marker,
          color = ~color_border,
          stroke = TRUE,
          weight = ~weight_border,
          fillColor = ~marker_fill,
          fillOpacity = 0.8,
          popup = ~marker_popup,
          popupOptions = popupOptions(maxWidth = 450, minWidth = 260)
        )
      
      # Dibujar quiver interactivo 2D de corrientes si está activo
      if (!is_blind && !is.null(input$visual_vectorfield) && input$visual_vectorfield != "none") {
        # Convertir puntos de origen y final a WGS84
        orig_wgs <- utm_to_wgs84_vector(map_df$x, map_df$y)
        dest_wgs <- utm_to_wgs84_vector(map_df$vx_end, map_df$vy_end)
        
        # Estructurar coordenadas con NAs interpolados para optimización vectorial
        n_pts <- nrow(map_df)
        lng_vec <- rep(NA, n_pts * 3)
        lat_vec <- rep(NA, n_pts * 3)
        
        idx_lng <- seq(1, n_pts * 3, by = 3)
        idx_dest <- seq(2, n_pts * 3, by = 3)
        
        lng_vec[idx_lng] <- orig_wgs$lng
        lng_vec[idx_dest] <- dest_wgs$lng
        lat_vec[idx_lng] <- orig_wgs$lat
        lat_vec[idx_dest] <- dest_wgs$lat
        
        color_vec <- if (input$visual_vectorfield == "sentido") "#06b6d4" else "#e11d48"
        
        leaflet_proxy_manzanas <- leaflet_proxy_manzanas %>%
          addPolylines(
            lng = lng_vec, lat = lat_vec,
            color = color_vec, weight = 1.6, opacity = 0.65,
            group = "vectores_IPF"
          )
        
        # Dibujar micro-cabezas de flechas (circulitos al extremo) para manzanas con fuerza
        manzanas_con_fuerza <- map_df[map_df$f_mag > 1e-3, ]
        if (nrow(manzanas_con_fuerza) > 0) {
          dest_wgs_f <- utm_to_wgs84_vector(manzanas_con_fuerza$vx_end, manzanas_con_fuerza$vy_end)
          leaflet_proxy_manzanas <- leaflet_proxy_manzanas %>%
            addCircleMarkers(
              lng = dest_wgs_f$lng, lat = dest_wgs_f$lat,
              radius = 1.5,
              color = color_vec,
              fillColor = color_vec,
              fillOpacity = 0.8,
              stroke = FALSE,
              group = "vectores_IPF"
            )
        }
      }
    }
  })
  
  # Dibujar geod\u00e9sicas y distorsiones en 2D
  observe({
    esc <- escenario_actual()
    if (is.null(esc) || is.null(esc$flujos_fondo) || is.null(esc$trayectorias)) return()
    leaflet_proxy <- leafletProxy("leaflet_map") %>%
      clearGroup("Trayectorias/Geodesicas (Trajectories)") %>%
      clearGroup("Flujos de Fondo (Background Flows)") %>%
      clearGroup("Distorsiones (Distortion Markers)")
    
    # 1. Dibujar flujos de fondo (corredores)
    for (i in seq_along(esc$flujos_fondo$x)) {
      f_x <- esc$flujos_fondo$x[[i]]
      f_y <- esc$flujos_fondo$y[[i]]
      coords <- utm_to_wgs84_vector(f_x, f_y)
      if (length(coords$lng) > 0) {
        leaflet_proxy <- leaflet_proxy %>%
          addPolylines(
            lng = coords$lng, lat = coords$lat,
            color = "#14b8a6", weight = 3, opacity = 0.35, dashArray = "4, 10",
            group = "Flujos de Fondo (Background Flows)",
            label = esc$flujos_fondo$nombre[i]
          )
      }
    }
    
    # 2. Dibujar trayectorias peatonales base (con Langevin nocturno)
    is_noche_traj <- isTRUE(input$scen_time == "Noche")
    sample_indices <- c(1, 4, 7, 10, 13)
    for (idx in sample_indices) {
      t_data <- esc$trayectorias[idx, ]
      tx <- t_data$x[[1]]
      ty <- t_data$y[[1]]
      # Aplicar perturbaci\u00f3n Langevin a trayectorias de fondo en horario nocturno
      if (is_noche_traj && length(tx) > 0) {
        jit_bg <- donut_jitter_langevin_vec(tx, ty, r_min = 10, r_max = 30, n_steps = 12, dt = 0.04, sigma = 6)
        tx <- jit_bg$x
        ty <- jit_bg$y
      }
      coords <- utm_to_wgs84_vector(tx, ty)
      
      fric_prom <- t_data$avg_friccion
      line_color <- if (fric_prom > 3000) "#ef4444" else if (fric_prom > 1500) "#f59e0b" else "#10b981"
      
      if (length(coords$lng) > 0) {
        leaflet_proxy <- leaflet_proxy %>%
          addPolylines(
            lng = coords$lng, lat = coords$lat,
            color = line_color, weight = 4, opacity = 0.6,
            group = "Trayectorias/Geodesicas (Trajectories)",
            label = paste("Trayect. Cuidado ->", t_data$destino)
          )
      }
    }
    
    # 3. Dibujar geod\u00e9sica interactiva actual
    traj <- geodesic_traj()
    if (!is.null(traj) && nrow(traj) > 1) {
      coords <- utm_to_wgs84_vector(traj$x, traj$y)
      if (length(coords$lng) > 0) {
        leaflet_proxy <- leaflet_proxy %>%
          addPolylines(
            lng = coords$lng, lat = coords$lat,
            color = "#f59e0b", weight = 6, opacity = 0.9,
            group = "Trayectorias/Geodesicas (Trajectories)",
            label = "Geod\u00e9sica Wu Wei Activa"
          ) %>%
          addCircleMarkers(
            lng = coords$lng[1], lat = coords$lat[1],
            radius = 14, color = "#10b981", fillColor = "#10b981", fillOpacity = 0.9,
            weight = 4, group = "Trayectorias/Geodesicas (Trajectories)", label = "Origen A"
          ) %>%
          addCircleMarkers(
            lng = coords$lng[length(coords$lng)], lat = coords$lat[length(coords$lat)],
            radius = 14, color = "#0ea5e9", fillColor = "#0ea5e9", fillOpacity = 0.9,
            weight = 4, group = "Trayectorias/Geodesicas (Trajectories)", label = "Destino B"
          )
      }
    }
    
    # 4. Dibujar marcadores de distorsi\u00f3n local D
    dist_df <- distorsiones_df()
    if (nrow(dist_df) > 0) {
      for (i in seq_len(nrow(dist_df))) {
        is_repulsive <- dist_df$type[i] %in% c("obstacle", "delito", "accidente")
        color_marker <- if (is_repulsive) "#ef4444" else "#10b981"
        
        tipo_lbl <- switch(dist_df$type[i],
                           "delito" = "Delito",
                           "accidente" = "Accidente",
                           "compras" = "Feria",
                           "seguridad" = "Corredor Seguro",
                           "obstacle" = "Obst\u00e1culo",
                           dist_df$type[i])
        
        label_marker <- paste0(tipo_lbl, " (D - ", tools::toTitleCase(dist_df$mag[i]), ")")
        
        r_influence <- switch(dist_df$mag[i],
                              "leve" = 120,
                              "moderada" = 220,
                              "severa" = 350,
                              "critica" = 500,
                              250)
        
        leaflet_proxy %>%
          addCircles(
            lng = dist_df$lng[i], lat = dist_df$lat[i],
            radius = r_influence,
            color = color_marker,
            fillColor = color_marker,
            fillOpacity = 0.12,
            weight = 1,
            group = "Distorsiones (Distortion Markers)"
          ) %>%
          addCircleMarkers(
            lng = dist_df$lng[i], lat = dist_df$lat[i],
            radius = 12,
            color = color_marker,
            fillColor = color_marker,
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 3,
            group = "Distorsiones (Distortion Markers)",
            label = label_marker
          )
      }
    }
  })
  
  # ---- PLOTLY CONNECTED 3D SURFACE ----
  output$plotly_mesh <- renderPlotly({
    req(input$dim_3d)
    esc <- escenario_actual()
    dist_df <- distorsiones_df()
    
    # Recuperación segura de la trayectoria interactiva para evitar la propagación de caídas en el arranque
    traj_inter <- NULL
    if (!is.null(input$origen_id) && !is.null(input$destino_id) && input$origen_id != "" && input$destino_id != "") {
      traj_inter <- tryCatch({
        geodesic_traj()
      }, error = function(e) NULL)
    }
    
    # Obtener malla de la superficie activa y Christoffels reactivos
    pre <- active_surface()
    xg_vec <- pre$Xg
    yg_vec <- pre$Yg
    Z_mesh <- pre$Z
    
    # Funci\u00f3n para interpolar z exacto en la superficie 3D deformada
    get_deformed_z_vector <- function(x_vec, y_vec) {
      sapply(seq_along(x_vec), function(idx) {
        get_z_height_scalar(x_vec[idx], y_vec[idx], Z_mesh, xg_vec, yg_vec)
      })
    }
    
    # Graficar la superficie reactiva deformada
    fig <- plot_ly(x = ~xg_vec, y = ~yg_vec, z = ~Z_mesh) %>%
      add_surface(
        colorscale = switch(input$dim_3d,
                            "bienestar" = "Plasma",
                            "nti" = "Plasma", # magma/plasma
                            "ricci" = "RdBu",
                            "lie" = "RdBu",
                            "Viridis"),
        opacity = 0.82,
        colorbar = list(title = "EQT Altitud", titlefont = list(color = "#334155", size = 10), tickfont = list(color = "#475569", size = 8)),
        lighting = list(ambient = 0.4, diffuse = 0.8, roughness = 0.2, specular = 1.5, fresnel = 0.5),
        name = "Superficie EQT"
      )
    
    # 3. Graficar flujos de fondo sobre el relieve
    for (i in seq_along(esc$flujos_fondo$x)) {
      f_x <- esc$flujos_fondo$x[[i]]
      f_y <- esc$flujos_fondo$y[[i]]
      f_z <- get_deformed_z_vector(f_x, f_y)
      
      fig <- fig %>%
        add_trace(
          x = f_x, y = f_y, z = f_z + 0.1,
          type = "scatter3d", mode = "lines",
          line = list(color = "rgba(56, 189, 248, 0.4)", width = 3, dash = "dash"),
          name = paste("Corredor:", esc$flujos_fondo$nombre[i])
        )
    }
    
    # 4. Graficar trayectorias base sobre el relieve
    sample_indices <- c(1, 4, 7, 10, 13)
    for (idx in sample_indices) {
      t_data <- esc$trayectorias[idx, ]
      t_x <- t_data$x[[1]]
      t_y <- t_data$y[[1]]
      t_z <- get_deformed_z_vector(t_x, t_y)
      
      fric_prom <- t_data$avg_friccion
      line_color <- if (fric_prom > 3000) "#ef4444" else if (fric_prom > 1500) "#f59e0b" else "#10b981"
      
      fig <- fig %>%
        add_trace(
          x = t_x, y = t_y, z = t_z + 0.2,
          type = "scatter3d", mode = "lines",
          line = list(color = line_color, width = 5),
          name = paste("Trayect. Cuidado ->", t_data$destino)
        )
    }
    
    # 5. Graficar geod\u00e9sica interactiva (asentada perfectamente sobre el relieve activo)
    if (!is.null(traj_inter) && nrow(traj_inter) > 1) {
      t_z_inter <- get_deformed_z_vector(traj_inter$x, traj_inter$y)
      fig <- fig %>%
        add_trace(
          x = traj_inter$x, y = traj_inter$y, z = t_z_inter + 0.3,
          type = "scatter3d", mode = "lines+markers",
          line = list(color = "#c084fc", width = 8),
          marker = list(color = "#f59e0b", size = 2),
          name = "Geod\u00e9sica Wu Wei"
        )
    }
    
    # 6. Graficar Origen A y Destino B
    m_df <- current_manzanas_df()
    if (!is.null(input$origen_id) && !is.null(input$destino_id) && input$origen_id != "" && input$destino_id != "") {
      orig <- m_df[m_df$id == input$origen_id, ]
      dest <- m_df[m_df$id == input$destino_id, ]
      if (nrow(orig) > 0 && nrow(dest) > 0) {
        orig_z <- get_z_height_scalar(orig$x, orig$y, Z_mesh, xg_vec, yg_vec)
        dest_z <- get_z_height_scalar(dest$x, dest$y, Z_mesh, xg_vec, yg_vec)
        
        fig <- fig %>%
          add_trace(
            x = orig$x, y = orig$y, z = orig_z + 0.5,
            type = "scatter3d", mode = "markers",
            marker = list(color = "#0ea5e9", size = 8, symbol = "circle"),
            name = "Origen A"
          ) %>%
          add_trace(
            x = dest$x, y = dest$y, z = dest_z + 0.5,
            type = "scatter3d", mode = "markers",
            marker = list(color = "#f43f5e", size = 8, symbol = "circle"),
            name = "Destino B"
          )
      }
    }
    
    # Dibujar marcadores de distorsi\u00f3n 3D
    if (nrow(dist_df) > 0) {
      for (k in seq_len(nrow(dist_df))) {
        dist_z <- get_z_height_scalar(dist_df$x[k], dist_df$y[k], Z_mesh, xg_vec, yg_vec)
        is_repulsive <- dist_df$type[k] %in% c("obstacle", "delito", "accidente")
        color_marker <- if (is_repulsive) "#ef4444" else "#10b981"
        tipo_lbl <- switch(dist_df$type[k],
                           "delito" = "Delito",
                           "accidente" = "Accidente",
                           "compras" = "Compras",
                           "seguridad" = "Corredor Seguro",
                           "obstacle" = "Obst\u00e1culo",
                           dist_df$type[k])
        
        fig <- fig %>%
          add_trace(
            x = dist_df$x[k], y = dist_df$y[k], z = dist_z + 0.8,
            type = "scatter3d", mode = "markers+text",
            text = paste0(tipo_lbl, " (D)"),
            textfont = list(color = "#ffffff", size = 11),
            marker = list(color = color_marker, size = 9, symbol = "circle"),
            name = paste0(tipo_lbl, " D")
          )
      }
    }
    
    # 7. Graficar vectores de fuerza 3D (Quiver) si está activado (Sincronizado con visión intrínseca)
    show_q3d <- isTRUE(input$show_vectors) || (!is.null(input$visual_vectorfield) && input$visual_vectorfield != "none" && !is_blind)
    if (show_q3d) {
      x_sparse <- seq(min(xg_vec), max(xg_vec), length.out = 8)
      y_sparse <- seq(min(yg_vec), max(yg_vec), length.out = 8)
      
      forces_df <- data.frame(x = numeric(), y = numeric(), z = numeric(), u = numeric(), v = numeric(), w = numeric())
      
      for (cy in y_sparse) {
        for (cx in x_sparse) {
          cz <- get_z_height_scalar(cx, cy, Z_mesh, xg_vec, yg_vec)
          
          # Determinar tipo de vector y calcular
          v_field_mode <- if (!is.null(input$visual_vectorfield) && input$visual_vectorfield != "none") input$visual_vectorfield else "sentido"
          
          if (v_field_mode == "grad_nti") {
            # Gradiente de NTI hacia el hotspot de disturbio más cercano
            if (nrow(dist_df) > 0) {
              idx_close <- which.min((cx - dist_df$x)^2 + (cy - dist_df$y)^2)
              dx_dist <- dist_df$x[idx_close] - cx
              dy_dist <- dist_df$y[idx_close] - cy
              dist_len <- sqrt(dx_dist^2 + dy_dist^2)
              if (dist_len > 0) {
                # Estimar NTI local en (cx, cy)
                nti_val_loc <- if (input$dim_3d == "nti") {
                  cz / 450.0
                } else {
                  exp(- dist_len / 350) * 1.5
                }
                fx <- (dx_dist / dist_len) * nti_val_loc * 3500
                fy <- (dy_dist / dist_len) * nti_val_loc * 3500
              } else {
                fx <- 0; fy <- 0
              }
            } else {
              fx <- 0; fy <- 0
            }
            # Evitar cálculos redundantes de potencial
            dV_w_dx <- 0; dV_w_dy <- 0; dV_d_dx <- 0; dV_d_dy <- 0; dV_slope_dx <- 0; dV_slope_dy <- 0; dV_dist_dx <- 0; dV_dist_dy <- 0
          } else {
            # Calcular gradientes de fuerza de Sentido
            x_wall <- 352500
            w_width <- 70
          
          # Factor de cruces habilitados (puentes) en la barrera
          dist_to_bridge1 <- abs(cy - 6294400)
          dist_to_bridge2 <- abs(cy - 6295300)
          bridge_mult <- min(1.0, min(dist_to_bridge1, dist_to_bridge2) / 200)
          
          V_w <- input$pot_wall_amp * bridge_mult * exp(- (cx - x_wall)^2 / (2 * w_width^2))
          dV_w_dx <- - ((cx - x_wall) / (w_width^2)) * V_w
          
          closest_bridge_y <- if (dist_to_bridge1 < dist_to_bridge2) 6294400 else 6295300
          dV_w_dy <- if (min(dist_to_bridge1, dist_to_bridge2) < 200) {
            input$pot_wall_amp * exp(- (cx - x_wall)^2 / (2 * w_width^2)) * sign(cy - closest_bridge_y) / 200
          } else {
            0
          }
          
          dV_d_dx <- 0; dV_d_dy <- 0
          if (!is.null(dest$x) && !is.null(dest$y)) {
            r_dest <- 400
            dist_sq <- (cx - dest$x)^2 + (cy - dest$y)^2
            V_d <- - input$pot_dest_amp * exp(- dist_sq / (2 * r_dest^2))
            dV_d_dx <- - ((cx - dest$x) / (r_dest^2)) * V_d
            dV_d_dy <- - ((cy - dest$y) / (r_dest^2)) * V_d
          }
          
          # Gradiente de pendiente f\u00edsica
          dV_slope_dx <- 0; dV_slope_dy <- 0
          if (input$ped_profile == "mayor") {
            loc_base <- findInterval(cx, pre$Xg); if (loc_base < 1) loc_base <- 1
            loc_base_y <- findInterval(cy, pre$Yg); if (loc_base_y < 1) loc_base_y <- 1
            dAlt_dx_loc <- pre$GammaPacked[loc_base, loc_base_y, 10]
            dAlt_dy_loc <- pre$GammaPacked[loc_base, loc_base_y, 11]
            slope_amp_val <- if (!is.null(input$pot_slope_amp)) input$pot_slope_amp else 15000
            dV_slope_dx <- slope_amp_val * dAlt_dx_loc
            dV_slope_dy <- slope_amp_val * dAlt_dy_loc
          }
          
          # Distorsiones
          dV_dist_dx <- 0; dV_dist_dy <- 0
          if (nrow(dist_df) > 0) {
            for (k in seq_len(nrow(dist_df))) {
              d_x <- dist_df$x[k]
              d_y <- dist_df$y[k]
              d_type <- dist_df$type[k]
              d_mag <- dist_df$mag[k]
              
              is_repulsive <- d_type %in% c("obstacle", "delito", "accidente")
              sign_val <- if (is_repulsive) 1.0 else -1.0
              
              sens_mult <- 1.0
              if (input$ped_profile == "cuidado") {
                if (d_type == "delito") sens_mult <- 1.8
                if (d_type == "compras") sens_mult <- 1.6
} else if (input$ped_profile == "nocturna") {
                if (d_type == "delito") sens_mult <- 2.8
                if (d_type == "seguridad") sens_mult <- 2.2
              } else if (input$ped_profile == "mayor") {
                if (d_type %in% c("delito", "muro", "barrera", "obstaculo")) sens_mult <- 3.5
              }
              
              mag_params <- switch(d_mag,
                                   "leve"     = list(r = 100, amp = 4000),
                                   "moderada" = list(r = 200, amp = 12000),
                                   "severa"   = list(r = 350, amp = 30000),
                                   "critica"  = list(r = 500, amp = 70000),
                                   list(r = 200, amp = 12000))
              r_dist <- mag_params$r
              dist_amp_val <- mag_params$amp * sens_mult
              
              dist_sq_pt <- (cx - d_x)^2 + (cy - d_y)^2
              V_dist <- sign_val * dist_amp_val * exp(- dist_sq_pt / (2 * r_dist^2))
              dV_dist_dx <- dV_dist_dx - ((cx - d_x) / (r_dist^2)) * V_dist
            }
          }
          } # Cierre de la rama else (fuerzas de Sentido)
          
          # Fuerza es -gradiente (común para ambos)
          fx <- - (dV_w_dx + dV_d_dx + dV_slope_dx + dV_dist_dx)
          fy <- - (dV_w_dy + dV_d_dy + dV_slope_dy + dV_dist_dy)
          
          f_mag <- sqrt(fx^2 + fy^2)
          if (f_mag > 1e-3) {
            # Normalizaci\u00f3n y escala logar\u00edtmica uniforme para evitar flechas gigantescas (Quiver)
            scale_fac <- (15 + 25 * log1p(f_mag)) / f_mag
            u_val <- fx * scale_fac
            v_val <- fy * scale_fac
          } else {
            u_val <- 0; v_val <- 0
          }
          
          forces_df <- rbind(forces_df, data.frame(
            x = cx, y = cy, z = cz + 6, u = u_val, v = v_val, w = 0
          ))
        }
      }
      
      fig <- fig %>%
        add_trace(
          type = "cone",
          x = forces_df$x, y = forces_df$y, z = forces_df$z,
          u = forces_df$u, v = forces_df$v, w = forces_df$w,
          sizemode = "scaled",
          sizeref = 0.25, # Cones de tama\u00f1o refinado y discreto
          showscale = FALSE,
          colorscale = if (!is.null(input$visual_vectorfield) && input$visual_vectorfield == "grad_nti") "Reds" else "Viridis",
          name = "Vectores de Fuerza"
        )
    }
    
    # Generate scene annotations dynamically to explain landmarks
    scene_ann <- list()
    if (!is.null(input$origen_id) && !is.null(input$destino_id) && input$origen_id != "" && input$destino_id != "") {
      orig <- m_df[m_df$id == input$origen_id, ]
      dest <- m_df[m_df$id == input$destino_id, ]
      if (nrow(orig) > 0 && nrow(dest) > 0) {
        orig_z <- get_z_height_scalar(orig$x, orig$y, Z_mesh, xg_vec, yg_vec)
        dest_z <- get_z_height_scalar(dest$x, dest$y, Z_mesh, xg_vec, yg_vec)
        
        lbl_a <- if (identical(lang(), "EN")) "Start A" else "Origen A"
        lbl_b <- if (identical(lang(), "EN")) "Destination B" else "Destino B"
        
        scene_ann <- list(
          list(
            x = orig$x, y = orig$y, z = orig_z + 6,
            text = lbl_a,
            showarrow = TRUE, arrowhead = 2, arrowcolor = "#0ea5e9", arrowsize = 1,
            font = list(color = "#1e293b", size = 10),
            bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "#0ea5e9", borderwidth = 1
          ),
          list(
            x = dest$x, y = dest$y, z = dest_z + 6,
            text = lbl_b,
            showarrow = TRUE, arrowhead = 2, arrowcolor = "#f43f5e", arrowsize = 1,
            font = list(color = "#1e293b", size = 10),
            bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "#f43f5e", borderwidth = 1
          )
        )
      }
    }
    
    # Add landmark for the cadastral speculative boundary (Case B/C/G)
    if (isTRUE(input$pot_wall_amp > 10000)) {
      lbl_wall <- if (identical(lang(), "EN")) "Speculative Boundary (x=352,500)" else "L\u00edmite Especulativo (x=352,500)"
      wall_z <- get_z_height_scalar(352500, 6294800, Z_mesh, xg_vec, yg_vec)
      scene_ann[[length(scene_ann) + 1]] <- list(
        x = 352500, y = 6294800, z = wall_z + 8,
        text = lbl_wall,
        showarrow = TRUE, arrowhead = 1, arrowcolor = "#cbd5e1",
        font = list(color = "#334155", size = 9),
        bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "#cbd5e1", borderwidth = 1
      )
    }
    
    # Add landmark for Care & Cohesion Valleys (Case D/E)
    if (identical(esc$nombre, "Caso D: Escudo de Cohesi\u00f3n (Cohesion Shield)") ||
        identical(esc$nombre, "Caso E: Autopoiesis Territorial (Territorial Autopoiesis)")) {
      lbl_care <- if (identical(lang(), "EN")) "Care Center (Cohesion Valley)" else "Centro de Cuidados (Valle Cohesivo)"
      care_z <- get_z_height_scalar(353500, 6294000, Z_mesh, xg_vec, yg_vec)
      scene_ann[[length(scene_ann) + 1]] <- list(
        x = 353500, y = 6294000, z = care_z + 4,
        text = lbl_care,
        showarrow = TRUE, arrowhead = 2, arrowcolor = "#10b981",
        font = list(color = "#1e293b", size = 10),
        bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "#10b981", borderwidth = 1
      )
    }

    # Formateo amigable libre de fondos opacos
    fig %>%
      layout(
        font = list(size = 11, color = "#1e293b"),
        scene = list(
          aspectmode = "manual",
          aspectratio = list(x = 1.0, y = 1.0, z = 0.35),
          xaxis = list(title = "X UTM (E)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8), showbackground = FALSE),
          yaxis = list(title = "Y UTM (N)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8), showbackground = FALSE),
          zaxis = list(title = "Z Variedad", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8), showbackground = FALSE),
          camera = list(eye = list(x = 1.4, y = -1.4, z = 1.2)),
          annotations = scene_ann
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
  })
  
  # ---- TAB 3: MESA DE ALGEBRA DE LIE ----
  output$plotly_lie_mesh <- renderPlotly({
    req(input$lie_seq)
    xg_vec <- sim_avanzada$info$xg
    yg_vec <- sim_avanzada$info$yg
    
    seq_sel <- if (!is.null(input$lie_seq)) input$lie_seq else "diff"
    Z_mesh <- if (seq_sel == "X_Y") {
      as.matrix(sim_avanzada$lie_algebra$surface_X_Y)
    } else if (seq_sel == "Y_X") {
      as.matrix(sim_avanzada$lie_algebra$surface_Y_X)
    } else {
      as.matrix(sim_avanzada$lie_algebra$surface_diff)
    }
    
    # A\u00f1adir distorsiones interactivas (hundir/elevar) en la malla del conmutador de Lie
    dist_df <- distorsiones_df()
    if (nrow(dist_df) > 0) {
      for (k in seq_len(nrow(dist_df))) {
        d_x <- dist_df$x[k]
        d_y <- dist_df$y[k]
        d_type <- dist_df$type[k]
        d_mag <- dist_df$mag[k]
        
        is_repulsive <- d_type %in% c("obstacle", "delito", "accidente")
        sign_val <- if (is_repulsive) 1.0 else -1.0
        
        mag_params <- switch(d_mag,
                             "leve"     = list(r = 100, amp = 0.5),
                             "moderada" = list(r = 200, amp = 1.0),
                             "severa"   = list(r = 350, amp = 1.8),
                             "critica"  = list(r = 500, amp = 2.8),
                             list(r = 200, amp = 1.0))
        r_dist <- mag_params$r
        amp_3d <- sign_val * mag_params$amp
        
        for (i in seq_along(yg_vec)) {
          for (j in seq_along(xg_vec)) {
            dist_sq <- (xg_vec[j] - d_x)^2 + (yg_vec[i] - d_y)^2
            Z_mesh[i, j] <- Z_mesh[i, j] + amp_3d * exp(-dist_sq / (2 * r_dist^2))
          }
        }
      }
    }
    
    fig <- plot_ly(x = ~xg_vec, y = ~yg_vec, z = ~Z_mesh) %>%
      add_surface(
        colorscale = if (seq_sel == "diff") "RdBu" else "Viridis",
        colorbar = list(title = "Lie Z", titlefont = list(color = "#334155", size = 10), tickfont = list(color = "#475569", size = 8)),
        lighting = list(ambient = 0.7, diffuse = 0.75, roughness = 0.8),
        name = "Superficie de Lie"
      )
    
    fig %>%
      layout(
        font = list(size = 11, color = "#1e293b"),
        scene = list(
          aspectmode = "manual",
          aspectratio = list(x = 1.0, y = 1.0, z = 0.35),
          xaxis = list(title = "X UTM (E)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8), showbackground = FALSE),
          yaxis = list(title = "Y UTM (N)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8), showbackground = FALSE),
          zaxis = list(title = "Variaci\u00f3n Lie", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8), showbackground = FALSE)
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
  })
  
  # ---- TAB 3: TEXTOS DE INTERPRETACI\u00d3N DIN\u00c1MICA DE LA MATH ----
  output$lie_interpretation_text <- renderUI({
    is_en <- identical(lang(), "EN")
    seq_sel <- if (!is.null(input$lie_seq)) input$lie_seq else "diff"
    case_name <- input$narrative_case
    
    desc <- if (is_en) {
      switch(seq_sel,
        "X_Y" = "Real Estate Sequence (X) before Park (Y): Developer gentrification capital is deployed first, increasing local territorial friction and blocking social cohesion. The subsequent park has a very limited restorative impact.",
        "Y_X" = "Social Sequence (Y) before Real Estate (X): Community parks and social infrastructure are built first, creating a low-friction 'social shield' that successfully resists subsequent developer pressure.",
        "diff" = "Net Asymmetry (Commutator [X,Y]): Shows the net power difference between sequences. Red areas show where starting with developers generates disproportionately higher friction, while blue areas show protection achieved by starting with parks."
      )
    } else {
      switch(seq_sel,
        "X_Y" = "Secuencia Inmobiliaria (X) antes que Parque (Y): El capital privado gentrificador se asienta en primer lugar, elevando la fricci\u00f3n territorial y bloqueando la cohesi\u00f3n comunitaria. El parque posterior tiene un impacto reparador muy limitado.",
        "Y_X" = "Secuencia Social (Y) antes que Inmobiliaria (X): El equipamiento comunitario y los parques se construyen primero, creando un 'escudo social' de baja fricci\u00f3n que resiste con \u00e9xito la pr\u00e9sion del capital inmobiliario posterior.",
        "diff" = "Asimetr\u00eda Neta (Conmutador [X,Y]): Muestra la diferencia neta de poder entre ambas secuencias. Las \u00e1reas rojas indican d\u00f3nde comenzar con la inmobiliaria genera una fricci\u00f3n territorial desproporcionadamente mayor, y las \u00e1reas azules muestran la protecci\u00f3n y descompresi\u00f3n lograda al comenzar con el parque."
      )
    }
    
    case_desc <- if (is_en) {
      if (identical(case_name, "caso_d")) {
        "In Case D (Cohesion Shield), the commutator reveals how the self-managed community park in the Grecia/Tobalaba zone acts as a tension-dampening sink, absorbing and neutralizing the geodesic of gentrifying capital."
      } else {
        "This asymmetry geometrically demonstrates that in the Pe\u00f1alol\u00e9n territory, the temporal order of urban interventions is non-commutative, revealing the underlying power structure."
      }
    } else {
      if (identical(case_name, "caso_d")) {
        "En el Caso D (Escudo de Cohesi\u00f3n), el conmutador revela c\u00f3mo el parque comunitario autogestionado en la zona Grecia/Tobalaba act\u00faa como un pozo amortiguador de tensiones, absorbiendo e inactivando la geod\u00e9sica del capital gentrificador."
      } else {
        "Esta asimetr\u00eda demuestra geom\u00e9tricamente que en el territorio de Pe\u00f1alol\u00e9n, el orden temporal de las intervenciones urbanas no es conmutativo, revelando la estructura de poder."
      }
    }
    
    div(style = "background: rgba(236, 72, 153, 0.05); border: 1px solid rgba(236, 72, 153, 0.25); border-radius: 8px; padding: 14px; margin-top: 10px;",
        tags$b(if (is_en) "Commutator Interpretation: " else "Interpretaci\u00f3n del Conmutador: "), tags$span(desc),
        tags$p(style = "margin-top: 6px; font-style: italic; color: #f43f5e; font-weight:600;", case_desc)
    )
  })
  
  # Stub for old lie translation
  output$old_lie_translation_unused <- renderUI({
    seq_sel <- if (!is.null(input$lie_seq)) input$lie_seq else "diff"
    case_name <- input$narrative_case
    
    desc <- switch(seq_sel,
      "X_Y" = "Secuencia Inmobiliaria (X) antes que Parque (Y): El capital privado gentrificador se asienta en primer lugar, elevando la fricci\u00f3n territorial y bloqueando la cohesi\u00f3n comunitaria. El parque posterior tiene un impacto reparador muy limitado.",
      "Y_X" = "Secuencia Social (Y) antes que Inmobiliaria (X): El equipamiento comunitario y los parques se construyen primero, creando un 'escudo social' de baja fricci\u00f3n que resiste con \u00e9xito la presi\u00f3n del capital inmobiliario posterior.",
      "diff" = "Asimetr\u00eda Neta (Conmutador [X,Y]): Muestra la diferencia neta de poder entre ambas secuencias. Las \u00e1reas rojas indican d\u00f3nde comenzar con la inmobiliaria genera una fricci\u00f3n territorial desproporcionadamente mayor, y las \u00e1reas azules muestran la protecci\u00f3n y descompresi\u00f3n lograda al comenzar con el parque."
    )
    
    case_desc <- if (case_name == "caso_d") {
      "En el Caso D (Escudo de Cohesi\u00f3n), el conmutador revela c\u00f3mo el parque comunitario autogestionado en la zona Grecia/Tobalaba act\u00faa como un pozo amortiguador de tensiones, absorbiendo e inactivando la geod\u00e9sica del capital gentrificador."
    } else {
      "Esta asimetr\u00eda demuestra geom\u00e9tricamente que en el territorio de Pe\u00f1alol\u00e9n, el orden temporal de las intervenciones urbanas no es conmutativo, revelando la estructura de poder."
    }
    
    div(style = "background: rgba(236, 72, 153, 0.05); border: 1px solid rgba(236, 72, 153, 0.25); border-radius: 8px; padding: 14px; margin-top: 10px;",
        tags$b("Interpretaci\u00f3n del Conmutador: "), tags$span(desc),
        tags$p(style = "margin-top: 6px; font-style: italic; color: #f43f5e; font-weight:600;", case_desc)
    )
  })
  
  output$jacobian_interpretation_text <- renderUI({
    is_en <- identical(lang(), "EN")
    id_sel <- selected_pixel()
    req(id_sel)
    idx <- which(graph_data$manzanaId == id_sel)
    if (length(idx) == 0) idx <- 12
    memoria_df <- graph_data$tensorMemoria[[idx]]
    cohesion <- memoria_df$cohesionSocial[1]
    gent <- memoria_df$tensionGentrificacion[1]
    
    re_val <- -0.6 + gent * 1.5
    
    stability_status <- if (re_val < 0) {
      if (is_en) {
        span("STABLE (Attractor Focus of the Territory)", style = "color: #10b981; font-weight: bold;")
      } else {
        span("ESTABLE (Foco Atractor del Territorio)", style = "color: #10b981; font-weight: bold;")
      }
    } else {
      if (is_en) {
        span("UNSTABLE (Saddle / Repelling Focus)", style = "color: #ef4444; font-weight: bold;")
      } else {
        span("INESTABLE (Silla / Foco Repulsor)", style = "color: #ef4444; font-weight: bold;")
      }
    }
    
    desc <- if (is_en) {
      if (re_val < 0) {
        paste0("Selected pixel '", id_sel, "' displays eigenvalues with negative real part (", round(re_val, 2), "). This represents local stability. Walkers experience a harmonic, controlled flow, feeling gently guided and protected by the social cohesion level of ", round(cohesion, 2), ".")
      } else {
        paste0("Selected pixel '", id_sel, "' displays eigenvalues with positive real part (", round(re_val, 2), "). The zone is locally unstable due to the high gentrification tension of ", round(gent, 2), ". Nearby pedestrian paths are repelled or deflected chaotically, disrupting their destination.")
      }
    } else {
      if (re_val < 0) {
        paste0("El pixel seleccionado '", id_sel, "' presenta autovalores con parte real negativa (", round(re_val, 2), "). Esto representa estabilidad local. El peat\u00f3n experimenta un flujo arm\u00f3nico controlado, sinti\u00e9ndose guiado y protegido suavemente por la cohesi\u00f3n comunitaria de ", round(cohesion, 2), ".")
      } else {
        paste0("El pixel seleccionado '", id_sel, "' tiene autovalores con parte real positiva (", round(re_val, 2), "). La zona es localmente inestable debido a la alta tensi\u00f3n de gentrificaci\u00f3n de ", round(gent, 2), ". Las trayectorias peatonales que pasen cerca ser\u00e1n repelidas o desviadas de forma ca\u00f3tica, rompiendo su rumbo.")
      }
    }
    
    div(style = "background: rgba(16, 185, 129, 0.05); border: 1px solid rgba(16, 185, 129, 0.25); border-radius: 8px; padding: 14px; margin-top: 10px;",
        tags$b(if (is_en) "Dynamic Stability Analysis: " else "An\u00e1lisis Din\u00e1mico de Estabilidad: "), tags$span(stability_status),
        tags$p(style = "margin-top: 6px; color: #10b981; font-weight:600;", desc)
    )
  })
  
  # Stub for old jacobian translation
  output$old_jac_translation_unused <- renderUI({
    id_sel <- selected_pixel()
    req(id_sel)
    idx <- which(graph_data$manzanaId == id_sel)
    if (length(idx) == 0) idx <- 12
    memoria_df <- graph_data$tensorMemoria[[idx]]
    cohesion <- memoria_df$cohesionSocial[1]
    gent <- memoria_df$tensionGentrificacion[1]
    
    re_val <- -0.6 + gent * 1.5
    
    stability_status <- if (re_val < 0) {
      span("ESTABLE (Foco Atractor del Territorio)", style = "color: #10b981; font-weight: bold;")
    } else {
      span("INESTABLE (Silla / Foco Repulsor)", style = "color: #ef4444; font-weight: bold;")
    }
    
    desc <- if (re_val < 0) {
      paste0("El pixel seleccionado '", id_sel, "' presenta autovalores con parte real negativa (", round(re_val, 2), "). Esto representa estabilidad local. El peat\u00f3n experimenta un flujo arm\u00f3nico controlado, sinti\u00e9ndose guiado y protegido suavemente por la cohesi\u00f3n comunitaria de ", round(cohesion, 2), ".")
    } else {
      paste0("El pixel seleccionado '", id_sel, "' tiene autovalores con parte real positiva (", round(re_val, 2), "). La zona es localmente inestable debido a la alta tensi\u00f3n de gentrificaci\u00f3n de ", round(gent, 2), ". Las trayectorias peatonales que pasen cerca ser\u00e1n repelidas o desviadas de forma ca\u00f3tica, rompiendo su rumbo.")
    }
    
    div(style = "background: rgba(16, 185, 129, 0.05); border: 1px solid rgba(16, 185, 129, 0.25); border-radius: 8px; padding: 14px; margin-top: 10px;",
        tags$b("An\u00e1lisis Din\u00e1mico de Estabilidad: "), tags$span(stability_status),
        tags$p(style = "margin-top: 6px; color: #10b981; font-weight:600;", desc)
    )
  })
  
  output$lyapunov_interpretation_text <- renderUI({
    is_en <- identical(lang(), "EN")
    omega <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
    
    desc <- if (is_en) {
      paste0("The Lyapunov function V_L(t) decays at a damping rate of Omega = ", omega, ". ")
    } else {
      paste0("La funci\u00f3n de Lyapunov V_L(t) decae a una tasa de amortiguaci\u00f3n de \u03a9 = ", omega, ". ")
    }
    
    consequence <- if (is_en) {
      if (omega > 0.6) {
        "Under critical damping, the walker suffers severe dissipation of vital energy at every step. If they enter a conflict zone (crime or physical barriers), their velocity drops drastically until they remain 'trapped' in a local minimum of social friction (pedestrian fatigue)."
      } else if (omega > 0.3) {
        "Moderate damping simulates normal walker fatigue under land metric deformations, forcing detour paths to optimize residual energy and continue the geodesic path."
      } else {
        "Low energy dissipation allows the walker to conserve almost all kinetic energy, enabling them to bypass barriers and persistently continue to their goal."
      }
    } else {
      if (omega > 0.6) {
        "Con esta amortiguaci\u00f3n cr\u00edtica, el peat\u00f3n sufre una disipaci\u00f3n severa de su Voluntad en cada paso. Si entra en una zona de conflicto (delitos u obst\u00e1culos), su velocidad se reducir\u00e1 dr\u00e1sticamente hasta quedar 'atrapado' en un m\u00ednimo local de fricci\u00f3n social (fatiga peatonal)."
      } else if (omega > 0.3) {
        "La amortiguaci\u00f3n moderada simula el cansancio normal del caminante ante las distorsiones del territorio, oblig\u00e1ndole a dar rodeos para optimizar su energ\u00eda residual y seguir la geod\u00e9sica."
      } else {
        "La baja disipaci\u00f3n permite al peat\u00f3n conservar casi toda su energ\u00eda, lo que le permite superar barreras y continuar de forma persistente hacia su meta."
      }
    }
    
    div(style = "background: rgba(168, 85, 247, 0.05); border: 1px solid rgba(168, 85, 247, 0.25); border-radius: 8px; padding: 14px; margin-top: 10px;",
        tags$b(if (is_en) "Lyapunov Dynamics: " else "Din\u00e1mica de Lyapunov: "), tags$span(desc),
        tags$p(style = "margin-top: 6px; color: #c084fc; font-weight:600;", consequence)
    )
  })
  
  # ---- REPORTE ONTOL\u00d3GICO PO\u00c9TICO DIN\u00c1MICO ----
  output$poetic_ontological_report <- renderUI({
    is_en <- identical(lang(), "EN")
    
    omega <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
    alpha_base <- if (!is.null(input$alpha_caputo)) input$alpha_caputo else 0.6
    
    # Memoria fraccionaria din\u00e1mica acoplada a la tensi\u00f3n del p\u00edxel activo
    id_sel <- selected_pixel()
    g_data <- current_graph_data()
    idx_graph <- which(g_data$manzanaId == id_sel)
    alpha <- alpha_base
    if (length(idx_graph) > 0) {
      gent_tension <- g_data$tensorMemoria[[idx_graph]]$tensionGentrificacion[1]
      alpha <- max(0.2, alpha_base - gent_tension * 0.3)
    }
    
    exp_mode <- if (!is.null(input$exp_mode)) input$exp_mode else "base"
    
    mem_poet <- if (alpha < 0.45) {
      if (is_en) {
        "The fractional memory order (alpha) is low, representing a high-memory regime. Past trauma clings heavily to the soil; walking paths are curved by the non-local pull of memory, refusing to fade into absolute Euclidean space."
      } else {
        "El orden fraccionario de memoria (alpha) es bajo, representando un r\u00e9gimen de alta persistencia. El trauma pasado se aferra con fuerza al suelo; las trayectorias de caminata se curvan por la atracci\u00f3n no local del recuerdo, neg\u00e1ndose a disolverse en el espacio euclidiano absoluto."
      }
    } else {
      if (is_en) {
        "The fractional memory order is high, showing rapid localized decay. The pedestrian walks with minimal historical drag, adapting directly to local friction gradients."
      } else {
        "El orden fraccionario de memoria es alto, mostrando un decaimiento localizado r\u00e1pido. El peat\u00f3n se desplaza con un arrastre hist\u00f3rico m\u00ednimo, adapt\u00e1ndose directamente a las gradientes inmediatas de fricci\u00f3n local."
      }
    }
    
    diss_poet <- if (omega > 0.5) {
      if (is_en) {
        "Lyapunov dissipation is high. The city drains vital energy quickly, reflecting exhaustion under real estate enclosure or spatial isolation. The walker is forced into local minima, representing structural fatigue."
      } else {
        "La disipaci\u00f3n de Lyapunov es elevada. La ciudad drena la energ\u00eda vital de forma acelerada, reflejando el agotamiento pedestre bajo cercamientos inmobiliarios o aislamiento espacial. El caminante queda atrapado en pozos locales, representando fatiga estructural."
      }
    } else {
      if (is_en) {
        "Lyapunov energy dissipation is low. The pedestrian retains their territorial will, enabling them to cross boundaries and complete long drifts through the relational topology."
      } else {
        "La disipaci\u00f3n de Lyapunov es baja. El peat\u00f3n conserva su voluntad territorial, lo que le permite sortear bordes de fricci\u00f3n y completar largas derivas a trav\u00e9s de la topolog\u00eda relacional."
      }
    }
    
    exp_poet <- switch(exp_mode,
      "base" = if (is_en) {
        "Base Simulation: Revealing the intrinsic topology of Pe\u00f1alol\u00e9n's soil prior to developer intervention, displaying organic care networks as geodesic basins."
      } else {
        "Simulaci\u00f3n Base: Revela la topolog\u00eda intr\u00ednseca del suelo de Pe\u00f1alol\u00e9n antes de la intervenci\u00f3n del capital inmobiliario, mostrando cuencas de cuidado org\u00e1nico."
      },
      "exp6" = if (is_en) {
        "Experiment 6 (Natural Sanctuary): The Robin boundary condition deforms the metric at the park edge, establishing a semi-permeable conservation field that resists urban expansion."
      } else {
        "Experimento 6 (Santuario Natural): La condici\u00f3n de borde de Robin deforma la m\u00e9trica en el l\u00edmite del parque, estableciendo un campo de conservaci\u00f3n semipermeable que resiste el avance de la urbanizaci\u00f3n."
      },
      "exp7" = if (is_en) {
        "Experiment 7 (Capital Refraction): Real estate speculation distorts the Riemannian metric, creating a steep slope of capital plusval\u00eda that refracts low-income flows, pushing them to peripheral trajectories."
      } else {
        "Experimento 7 (Refracci\u00f3n de Capital): La especulaci\u00f3n inmobiliaria deforma la m\u00e9trica riemanniana, creando una pendiente pronunciada de plusval\u00eda que refracta los flujos peatonales, expuls\u00e1ndolos hacia trayectorias perif\u00e9ricas."
      }
    )
    
    div(class = "panel-glass", style = "padding: 20px; border-left: 4px solid #fbbf24; background: rgba(251, 191, 36, 0.05); margin-top: 15px; border-radius: 8px;",
      h4(style = "color: #fbbf24; font-weight: 700; margin-top: 0; margin-bottom: 12px;",
         trans("Reporte Ontol\u00f3gico Po\u00e9tico", "Poetic Ontological Report")),
      tags$p(style = "color: #334155; font-size: 0.95rem; line-height: 1.5; margin-bottom: 8px;",
             tags$b(trans("Memoria Colectiva: ", "Collective Memory: ")), tags$span(mem_poet)),
      tags$p(style = "color: #334155; font-size: 0.95rem; line-height: 1.5; margin-bottom: 8px;",
             tags$b(trans("Energ\u00eda y Voluntad: ", "Energy & Will: ")), tags$span(diss_poet)),
      tags$p(style = "color: #334155; font-size: 0.95rem; line-height: 1.5; margin-bottom: 0; font-style: italic; color: #fbbf24;",
             tags$b(trans("Falsaci\u00f3n Po\u00e9tica: ", "Poetic Falsification: ")), tags$span(exp_poet))
    )
  })
  
  # Stub for old lyapunov translation
  output$old_lyap_translation_unused <- renderUI({
    omega <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
    
    desc <- paste0("La funci\u00f3n de Lyapunov V_L(t) decae a una tasa de amortiguaci\u00f3n de \u03a9 = ", omega, ". ")
    
    consequence <- if (omega > 0.6) {
      "Con esta amortiguaci\u00f3n cr\u00edtica, el peat\u00f3n sufre una disipaci\u00f3n severa de su Voluntad en cada paso. Si entra en una zona de conflicto (delitos u obst\u00e1culos), su velocidad se reducir\u00e1 dr\u00e1sticamente hasta quedar 'atrapado' en un m\u00ednimo local de fricci\u00f3n social (fatiga peatonal)."
    } else if (omega > 0.3) {
      "La amortiguaci\u00f3n moderada simula el cansancio normal del caminante ante las distorsiones del territorio, oblig\u00e1ndole a dar rodeos para optimizar su energ\u00eda residual y seguir la geod\u00e9sica."
    } else {
      "La baja disipaci\u00f3n permite al peat\u00f3n conservar casi toda su energ\u00eda, lo que le permite superar barreras y continuar de forma persistente hacia su meta."
    }
    
    div(style = "background: rgba(168, 85, 247, 0.05); border: 1px solid rgba(168, 85, 247, 0.25); border-radius: 8px; padding: 14px; margin-top: 10px;",
        tags$b("Din\u00e1mica de Lyapunov: "), tags$span(desc),
        tags$p(style = "margin-top: 6px; color: #c084fc; font-weight:600;", consequence)
    )
  })
  
  # ---- TAB 3: ESTABILIDAD JACOBIANA (CORREGIDO ERROR COLOR base R) ----
  output$jacobian_stability_plot <- renderPlotly({
    id_sel <- selected_pixel()
    m_df <- current_manzanas_df()
    m_data <- m_df[m_df$id == id_sel, ]
    g_data <- current_graph_data()
    idx_graph <- which(g_data$manzanaId == id_sel)
    if (length(idx_graph) == 0) idx_graph <- 12
    tension_gent <- g_data$tensorMemoria[[idx_graph]]$tensionGentrificacion[1]
    
    re1 <- -0.6 + tension_gent * 1.5
    im1 <- 0.8
    re2 <- -0.6 + tension_gent * 1.5
    im2 <- -0.8
    
    status_str <- if (re1 >= 0) {
      if (identical(lang(), "EN")) "Unstable (Gentrifying)" else "Inestable (Gentrificador)"
    } else {
      if (identical(lang(), "EN")) "Stable (Caring)" else "Estable (Amparo)"
    }
    
    df_poles <- data.frame(
      Real = c(re1, re2),
      Imag = c(im1, im2),
      Pole = c("Polo 1 (\u03bb_1)", "Polo 2 (\u03bb_2)"),
      Status = c(status_str, status_str),
      stringsAsFactors = FALSE
    )
    
    plot_ly() %>%
      add_trace(x = c(-2, 0, 0, -2), y = c(-2, -2, 2, 2), type = "scatter", mode = "none", fill = "toself", fillcolor = "rgba(16, 185, 129, 0.08)", name = if (identical(lang(), "EN")) "Stable Region" else "Regi\u00f3n Estable", showlegend = FALSE) %>%
      add_trace(x = c(0, 2, 2, 0), y = c(-2, -2, 2, 2), type = "scatter", mode = "none", fill = "toself", fillcolor = "rgba(239, 68, 68, 0.08)", name = if (identical(lang(), "EN")) "Unstable Region" else "Regi\u00f3n Inestable", showlegend = FALSE) %>%
      add_trace(data = df_poles, x = ~Real, y = ~Imag, type = "scatter", mode = "markers+text",
                marker = list(color = "#fbbf24", size = 12, symbol = "x"),
                text = ~Pole, textposition = "top center",
                hovertemplate = paste0("<b>%{text}</b><br>",
                                      "Real: %{x:.3f}<br>",
                                      "Imag: %{y:.3f}<br>",
                                      "Estado: ", df_poles$Status, "<extra></extra>")) %>%
      layout(
        font = list(size = 11, color = "#1e293b"),
        xaxis = list(title = "Real (Re\u03bb)", range = c(-2, 2), gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155"), tickfont = list(color = "#475569")),
        yaxis = list(title = "Imaginario (Im\u03bb)", range = c(-2, 2), gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155"), tickfont = list(color = "#475569")),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = FALSE,
        margin = list(l = 40, r = 20, b = 40, t = 40)
      )
  })
  
  # ---- TAB 3: DECAIMIENTO DE LYAPUNOV ----
  output$lyapunov_decay_plot <- renderPlotly({
    t_steps <- 0:15
    omega <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
    alpha_base <- if (!is.null(input$alpha_caputo)) input$alpha_caputo else 0.6
    
    # Memoria fraccionaria din\u00e1mica acoplada a la tensi\u00f3n del p\u00edxel activo
    id_sel <- selected_pixel()
    g_data <- current_graph_data()
    idx_graph <- which(g_data$manzanaId == id_sel)
    alpha <- alpha_base
    if (length(idx_graph) > 0) {
      gent_tension <- g_data$tensorMemoria[[idx_graph]]$tensionGentrificacion[1]
      alpha <- max(0.2, alpha_base - gent_tension * 0.3)
    }
    
    # Resolutor num\u00e9rico Gr\u00fcnwald-Letnikov para la Ecuaci\u00f3n Caputo: D^alpha y = -omega * y
    dt_val <- 1.0
    n <- length(t_steps)
    y_energy <- numeric(n)
    y_energy[1] <- 10.0  # Energ\u00eda inicial V_L(0)
    
    # Precomputar coeficientes binomiales fraccionarios
    c_coeff <- numeric(n)
    c_coeff[1] <- 1
    if (n > 1) {
      for (j in 2:n) {
        c_coeff[j] <- c_coeff[j-1] * (1 - (alpha + 1) / (j - 1))
      }
    }
    
    for (k in 2:n) {
      val_sum <- sum(c_coeff[2:k] * y_energy[(k-1):1])
      y_energy[k] <- -val_sum / (1 + omega * dt_val^alpha)
    }
    
    df_lyap <- data.frame(Tiempo = t_steps, Energia = y_energy)
    
    plot_ly(df_lyap, x = ~Tiempo, y = ~Energia, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#f59e0b', width = 3), marker = list(color = '#c084fc', size = 5)) %>%
      layout(
        font = list(size = 11, color = "#1e293b"),
        xaxis = list(title = "Tiempo de Integraci\u00f3n (t)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8)),
        yaxis = list(title = "Energ\u00eda del Sistema V_L(t)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155", size = 11), tickfont = list(color = "#475569", size = 8)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 40, r = 20, b = 40, t = 40)
      )
  })
  
  # ---- TAB 3: HUD DE CAPAS DEL P\u00cdXEL ONTOL\u00d3GICO ----
  output$pixel_fiche_ui <- renderUI({
    id_sel <- selected_pixel()
    if (is.null(id_sel) || id_sel == "") {
      return(div(
        style = "padding:20px; text-align:center; color:#475569; font-size:1.15rem; font-weight:500; font-style:italic;",
        trans(
          "Seleccione una manzana catastral (p\u00edxel) en el mapa 2D para ver su Inventario Ergon\u00f3mico Peatonal (IEO) y Geotensor local.",
          "Select a cadastral block (pixel) on the 2D map to inspect its Pedestrian Ergonomic Inventory (IEO) and local Geotensor."
        )
      ))
    }
    
    is_en <- identical(lang(), "EN")
    m_df <- current_manzanas_df()
    m_data <- m_df[m_df$id == id_sel, ]
    g_data <- current_graph_data()
    idx <- which(g_data$manzanaId == id_sel)
    if (length(idx) == 0) idx <- 12
    memoria_df <- g_data$tensorMemoria[[idx]]
    
    cohesion_pct <- round(memoria_df$cohesionSocial[1] * 100)
    gent_pct <- round(memoria_df$tensionGentrificacion[1] * 100)
    lat_pct <- round((memoria_df$tensionGentrificacion[1] * 1.15) * 100)
    
    cohesion_pct <- max(0, min(100, cohesion_pct))
    gent_pct <- max(0, min(100, gent_pct))
    lat_pct <- max(0, min(100, lat_pct))
    
    current_params <- local_pixel_params()[[id_sel]]
    if (is.null(current_params)) {
      current_params <- list(alpha = 0.6, L = 100)
    }
    
    div(
      div(class = "hud-layer", span(class = "hud-label", trans("P\u00edxel Activo: ", "Active Pixel: ")), id_sel),
      div(class = "hud-layer", span(class = "hud-label", trans("Capa 1: Soporte Geom\u00e9trico -> ", "Layer 1: Geometric Support -> ")), "UTM E:", round(m_data$x), " N:", round(m_data$y)),
      div(class = "hud-layer", span(class = "hud-label", trans("Capa 2: Atributos F\u00edsicos -> ", "Layer 2: Physical Attributes -> ")), "NDVI: ", round(m_data$ndvi, 3), " | Altitud: ", round(m_data$altitud, 1), " m"),
      div(class = "hud-layer", span(class = "hud-label", trans("Capa 3: Relaciones Territoriales -> ", "Layer 3: Territorial Relations -> ")), trans("Conexi\u00f3n Red de Cuidado: ", "Care Network Connection: "), m_data$red_cuidado),
      
      div(class = "hud-layer", 
        span(class = "hud-label", trans("Capa 4: Memoria (Tensi\u00f3n Gentrificaci\u00f3n): ", "Layer 4: Memory (Gentrification Tension): ")), round(memoria_df$tensionGentrificacion[1], 3),
        div(class = "hud-meter", div(class = "hud-progress", style = paste0("width: ", gent_pct, "%; background:#f43f5e;")))
      ),
      div(class = "hud-layer", 
        span(class = "hud-label", trans("Capa 5: Latencia (Conflicto Potencial): ", "Layer 5: Latency (Potential Conflict): ")), round(memoria_df$tensionGentrificacion[1]*1.15, 3),
        div(class = "hud-meter", div(class = "hud-progress", style = paste0("width: ", lat_pct, "%; background:#f59e0b;")))
      ),
      div(class = "hud-layer", 
        span(class = "hud-label", trans("Cohesi\u00f3n Comunitaria (Calidad de Vida): ", "Social Cohesion (Quality of Life): ")), round(memoria_df$cohesionSocial[1], 3),
        div(class = "hud-meter", div(class = "hud-progress", style = paste0("width: ", cohesion_pct, "%; background:#10b981;")))
      ),
      tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.08); margin: 12px 0;"),
      h4(trans("Par\u00e1metros de Memoria Local (Caputo)", "Local Memory Parameters (Caputo)"), style = "color: #f59e0b; font-weight:600; margin-top:0; margin-bottom: 8px;"),
      sliderInput("local_alpha", trans("Alfa de Caputo Local (\u03b1):", "Local Caputo Alpha (\u03b1):"), min = 0.1, max = 1.0, value = current_params$alpha, step = 0.05),
      sliderInput("local_memory_L", trans("Longitud de Memoria L (m):", "Memory Window L (m):"), min = 10, max = 500, value = current_params$L, step = 10)
    )
  })
  
  # ==================== TOMO II NEW SERVER COMPONENTS ====================
  
  # ---- 1. HUD: ESTADO VITAL DE LA GEOD\u00c9SICA ----
  output$costo_vital_box <- renderUI({
    traj <- geodesic_traj()
    if (is.null(traj) || nrow(traj) <= 1) return(NULL)
    
    # Calcular distancia recorrida real
    dx <- diff(traj$x); dy <- diff(traj$y)
    ds <- sqrt(dx^2 + dy^2)
    path_len <- sum(ds)
    
    # Costo vital estimado
    omega_damp <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
    cost_val <- round(path_len * (1.2 + omega_damp * 2.0) / 10)
    
    p_color <- if (cost_val < 500) "#10b981" else if (cost_val < 1500) "#f59e0b" else "#ef4444"
    p_width <- min(100, max(5, cost_val / 20))
    
    div(style = "background: rgba(20, 184, 166, 0.04); border: 1px solid rgba(20, 184, 166, 0.2); border-radius: 8px; padding: 12px; margin-top: 10px;",
      h4("HUD: Costo Vital del Peat\u00f3n", style = "color: #14b8a6 !important; font-weight:600; margin-top:0; margin-bottom:5px;"),
      div(style = "display:flex; justify-content:space-between; font-size:0.9rem; color:#334155;",
        span(tags$b("Costo Vital: "), cost_val, " u"),
        span(tags$b("Distancia: "), round(path_len, 1), " m")
      ),
      div(class = "hud-meter", style = "background: rgba(255,255,255,0.08); height:8px; border-radius:4px; margin-top:5px; overflow:hidden;",
        div(style = sprintf("width: %d%%; height:100%%; background:%s;", round(p_width), p_color))
      )
    )
  })
  
  # ---- 2. BIT\u00c1CORA ONTOL\u00d3GICA DE LA VARIEDAD ----
  output$ontological_narrator <- renderUI({
    req(input$exp_mode)
    exp_mode_val <- input$exp_mode
    is_en <- identical(lang(), "EN")
    
    narrative_text <- switch(exp_mode_val,
      "exp1" = if (is_en) "The voluntary particle enters the refraction zone. The horizontal/vertical metric transition emulates pedestrian Snell's law. Transit is altered by the asymmetry of the periurban border." else "La part\u00edcula voluntaria ingresa a la zona de refracci\u00f3n. La transici\u00f3n de m\u00e9trica horizontal/vertical emula la ley de Snell peatonal. El transitar es modificado por la asimetr\u00eda del borde periurbano.",
      "exp2" = if (is_en) "An exclusion hill is detected in the center of the space. Wu Wei geodesics curve preventively away from the repeller of power. Jacobi's geodesic deviation formalizes segregation." else "Se detecta una colina de exclusi\u00f3n en el centro del espacio. Las geod\u00e9sicas Wu Wei se curvan preventivamente alej\u00e1ndose del repulsor del poder. El desv\u00edo geod\u00e9sico de Jacobi formaliza la segregaci\u00f3n.",
      "exp3" = {
        p_val <- if (!is.null(input$v0_vital)) input$v0_vital else 12.0
        if (p_val >= 15.0) {
          if (is_en) "ALERT: Critical threshold exceeded (P >= P_crit). The Hessian curvature has locally inverted. The exclusion repeller turns into a cohesive attractor. Trajectories merge in an autopoietic care core." else "ALERTA: Umbral cr\u00edtico superado (P >= P_crit). La curvatura de la Hessiana se ha invertido localmente. El repulsor de exclusi\u00f3n se torna en un atractor solidario. Las trayectorias confluyen en un n\u00facleo autopoietico de cuidados."
        } else {
          if (is_en) "The territorial space experiences moderate social pressure. The central attractor remains inactive; the walker's inertia does not yet trigger the common autopoiesis commutator." else "El espacio territorial experimenta presi\u00f3n social moderada. El atractor central permanece inactivo; la inercia del peat\u00f3n a\u00fan no gatilla el conmutador de autopoiesis com\u00fan."
        }
      },
      "exp4" = if (is_en) "Fractional memory mapping active. Geodesics cross nodes of historical trauma. Memory has non-Markovian persistence (Caputo, alpha order); the scar on the ground remains, altering present transit." else "Mapeo de memoria fraccionaria activo. Las geod\u00e9sicas atraviesan nodos de trauma hist\u00f3rico. La memoria tiene persistencia no markoviana (Caputo, orden alfa); la cicatriz en el suelo no se borra, alterando el transitar presente.",
      "exp5" = if (is_en) "Moran spatial autocorrelation and covariance validation active. In data-sparse field setups (N = 50), Ledoit-Wolf shrinkage is applied to stabilize the metric tensor g_ij against field noise." else "Validaci\u00f3n de covarianza y autocorrelaci\u00f3n de Moran activa. Ante la escasez de muestras en terreno (N = 50), se aplica la regularizaci\u00f3n de Ledoit-Wolf para estabilizar el tensor m\u00e9trico g_ij frente al ruido de campo.",
      "exp6" = if (is_en) "Pe\u00f1alol\u00e9n sanctuary active. The asymmetric boundary and Robin conditions channel the ecological walker's geodesic, protecting the natural corridor from slope erosion." else "Santuario de Pe\u00f1alol\u00e9n activo. La frontera asim\u00e9trica y las condiciones de Robin canalizan la geod\u00e9sica del peat\u00f3n ecol\u00f3gico, protegiendo el corredor natural del desgaste de la loma.",
      "exp7" = if (is_en) "Urban-rural limit and capital refraction active. The gradient of land rent and real estate speculation attracts or repels valuation paths, forcing phase transitions of financial flows." else "L\u00edmite urbano-rural y refracci\u00f3n de capital activa. El gradiente de plusval\u00eda y especulaci\u00f3n inmobiliaria atrae o repele las trayectorias de valorizaci\u00f3n, forzando transiciones de fase de flujo financiero.",
      if (is_en) "Stable municipal communes and territorial manifolds. Geodesics track altitude contours and baseline friction in equilibrium." else "Comunas y variedades territoriales estables. Las geod\u00e9sicas siguen el contorno de la altitud y fricci\u00f3n base en equilibrio."
    )
    
    # Active user-added interactive distortions analysis (Dynamic "What is happening" explanation)
    dist_df <- distorsiones_df()
    num_dist <- nrow(dist_df)
    
    deform_status <- ""
    wu_wei_status <- ""
    
    if (num_dist == 0) {
      deform_status <- if (is_en) {
        "The manifold is currently in its clean, baseline state. No local deformations are active."
      } else {
        "La variedad se encuentra en su estado base limpio. No hay deformaciones locales activas."
      }
      wu_wei_status <- if (is_en) {
        "The Wu Wei trajectories flow naturally across the topographic slopes, representing unconstrained pedestrian transit."
      } else {
        "Las trayectorias Wu Wei fluyen de manera natural siguiendo las pendientes topogr\u00e1ficas, representando un tr\u00e1nsito peatonal libre de coacciones."
      }
    } else {
      repulsors <- sum(dist_df$type %in% c("obstacle", "delito", "accidente"))
      attractors <- sum(dist_df$type %in% c("compras", "seguridad"))
      
      deform_status <- if (is_en) {
        sprintf("Active Deformations: %d local distortions detected (%d obstacles/friction points, %d safety/care centers). The metric g_ij is warped.", 
                num_dist, repulsors, attractors)
      } else {
        sprintf("Deformaci\u00f3n Activa: Se detectan %d distorsiones locales (%d focos de fricci\u00f3n/obst\u00e1culos, %d centros de cuidado/amparo). La m\u00e9trica g_ij se deforma en estas coordenadas.", 
                num_dist, repulsors, attractors)
      }
      
      if (repulsors >= 3) {
        wu_wei_status <- if (is_en) {
          "ALERT: The density of physical barriers and incidents has fractured the Wu Wei. Pedestrian connectivity is broken, forcing extreme detour angles or blocking safe paths."
        } else {
          "ALERTA CR\u00cdTICA: La densidad de barreras f\u00edsicas y delitos ha quebrado el Wu Wei. La conectividad peatonal se ha fracturado por completo, forzando desv\u00edos extremos o bloqueando el tr\u00e1nsito seguro."
        }
      } else if (repulsors > 0) {
        wu_wei_status <- if (is_en) {
          "The Wu Wei geodesics deform preventively to bypass the active friction peaks. The system successfully recalculates alternative least-friction paths."
        } else {
          "Las geod\u00e9sicas Wu Wei se curvan de manera preventiva para rodear las crestas de fricci\u00f3n activas. El sistema recalcula con \u00e9xito senderos alternativos de menor resistencia."
        }
      } else {
        wu_wei_status <- if (is_en) {
          "The care attractors successfully smooth the potential manifold, creating local transport valleys that pull and protect the geodesics."
        } else {
          "Los atractores de cuidado suavizan el potencial de la variedad, creando valles locales de tr\u00e1nsito que canalizan y protegen las geod\u00e9sicas."
        }
      }
    }
    
        div(style = "background: rgba(244, 63, 94, 0.04); border: 1px solid rgba(244, 63, 94, 0.2); border-radius: 8px; padding: 12px; margin-top: 10px;",
      h4(if (is_en) "Ontological Bitacora & Deformations" else "Bit\u00a1cora Ontol\u00f3gica de Deformaciones", style = "color: #f43f5e !important; font-weight:600; margin-top:0; margin-bottom:5px;"),
      tags$p(style = "font-size:0.95rem; font-weight:bold; margin-bottom:4px; color:#f87171;", deform_status),
      tags$p(style = "font-size:0.9rem; font-style: italic; margin-bottom:8px; line-height: 1.35; color:#334155;", narrative_text),
      tags$hr(style = "margin:6px 0; border-color: rgba(244, 63, 94, 0.1);"),
      tags$p(style = "font-size:0.9rem; font-weight:600; color:#38bdf8; margin:0; line-height:1.3;", wu_wei_status)
    )
  })
  
  # ---- 3. C\u00cdRCULO DE CONSEJEROS TERRITORIALES (LOCAL AI) ----
  output$ai_counselors <- renderUI({
    req(input$exp_mode)
    exp_mode_val <- input$exp_mode
    
    advice_list <- switch(exp_mode_val,
      "exp1" = list(
        name = "Consejo \u00d3ptico Territorial (Tarantola & Tuan)",
        diag = "Salto de m\u00e9trica de 1.0 (rural) a 8.0 (urbano) en el borde horizontal.",
        poet = "El peat\u00f3n refracta su andar al cruzar el l\u00edmite; la fluidez del suelo rural se congela en la grilla del asfalto.",
        policy = "Alinear transectas perpendiculares en el Canal San Carlos. Dise\u00f1ar un boulevard verde de transici\u00f3n para amortiguar el salto de fricci\u00f3n urbana."
      ),
      "exp2" = list(
        name = "Consejero de Desviaci\u00f3n M\u00e9trica (Weyl & Massey)",
        diag = "Fuerza repulsiva central activa. Desviaci\u00f3n geod\u00e9sica acumulada de Jacobi alta en el centro.",
        poet = "La colina de exclusi\u00f3n empuja al peat\u00f3n hacia los bordes; el poder segregador vac\u00eda el centro del habitar com\u00fan.",
        policy = "Realizar muestreos conc\u00e9ntricos. Se recomienda al municipio peatonalizar el centro segregado e instalar centros de salud familiar (CESFAM)."
      ),
      "exp3" = list(
        name = "Consejero Autopoi\u00e9tico (Maturana & Luhmann)",
        diag = "Inversi\u00f3n Hessiana local dependiente del umbral cr\u00edtico de presi\u00f3n social.",
        poet = "Bajo presi\u00f3n, el repulsor se quiebra y da paso al atractor solidario, replegando las voluntades en un c\u00edrculo de amparo.",
        policy = "Muestrear mediante talleres participativos. Crear una cooperativa comunitaria de abastecimiento local."
      ),
      "exp4" = list(
        name = "Consejero del Eje del Tiempo (Tuan & Caputo)",
        diag = "Decaimiento de memoria de orden fraccionario (alfa) en nodos de trauma hist\u00f3rico.",
        poet = "El dolor no es markoviano; el suelo conserva la cicatriz del trauma ralentizando el paso peatonal en el presente.",
        policy = "Declarar los sitios de memoria como monumentos nacionales e integrarlos en rutas de educaci\u00f3n c\u00edvica."
      ),
      "exp5" = list(
        name = "Consejero Geoestad\u00edstico (Moran & Ledoit-Wolf)",
        diag = "Ruido espacial elevado con matrices inestables por muestras escasas (N = 50).",
        poet = "El encogimiento de Ledoit-Wolf act\u00faa como escudo matem\u00e1tico, ordenando el caos del muestreo ruidoso.",
        policy = "Aplicar el factor de encogimiento rho = 0.45. Capacitar a los equipos de campo en grillas aleatorias estratificadas."
      ),
      "exp6" = list(
        name = "Consejero de Conservaci\u00f3n (Robin Boundary & Peat\u00f3n)",
        diag = "L\u00edmite asim\u00e9trico Quebrada de Macul y frontera de Robin activa.",
        poet = "El potencial asim\u00e9trico ampara el transitar ecol\u00f3gico, protegiendo las lomas de la llanura devastada.",
        policy = "Muestrear con GPS m\u00f3vil en senderos autorizados. Establecer un cintur\u00f3n verde y restringir el avance de proyectos inmobiliarios."
      ),
      "exp7" = list(
        name = "Consejero de Inversi\u00f3n (David Harvey & Smith)",
        diag = "Salto de m\u00e9trica y plusval\u00eda especulativa en el l\u00edmite urbano-rural.",
        poet = "La refracci\u00f3n del capital deforma las geod\u00e9sicas inmobiliarias, succionando la plusval\u00eda de la periferia campesina.",
        policy = "Muestrear precios de suelo en el l\u00edmite. Aplicar un impuesto al mayor valor de suelo para financiar viviendas sociales locales."
      ),
      list(
        name = "C\u00edrculo de Consejeros Territoriales (IA Local)",
        diag = "Variedad y m\u00e9tricas en equilibrio geod\u00e9sico base.",
        poet = "El andar del peat\u00f3n discurre arm\u00f3nicamente a lo largo de las curvas de nivel.",
        policy = "Optimizar los corredores de transporte local en concordancia con el plan regulador comunal."
      )
    )
    
    div(style = "background: rgba(245, 158, 11, 0.04); border: 1px solid rgba(245, 158, 11, 0.2); border-radius: 8px; padding: 12px; margin-top: 10px;",
      h4(sprintf("Consejero Activo: %s", advice_list$name), style = "color: #f59e0b !important; font-weight:600; margin-top:0; margin-bottom:5px;"),
      div(style = "font-size:0.85rem; line-height: 1.35; color:#334155;",
        tags$p(style = "margin-bottom:3px;", tags$b("1. Diagn\u00f3stico F\u00edsico: "), advice_list$diag),
        tags$p(style = "margin-bottom:3px; font-style: italic; color:#1e293b;", tags$b("2. Met\u00e1fora Po\u00e9tica: "), advice_list$poet),
        tags$p(style = "margin-bottom:0; color: #10b981;", tags$b("3. Acci\u00f3n de Pol\u00edtica Territorial: "), advice_list$policy)
      )
    )
  })

  # ---- 4. DESCARGAS DE PLANTILLAS Y MANUALES ----
  output$download_csv_template <- downloadHandler(
    filename = function() {
      paste0("IEO_plantilla_campo_exp_", input$exp_mode, ".csv")
    },
    content = function(file) {
      m_df <- current_manzanas_df()
      template <- data.frame(
        pixel_id = m_df$id,
        friccion_observada = round(m_df$altitud * (1.0 - m_df$ndvi) * 0.8 + rnorm(nrow(m_df), 0, 10), 1),
        cohesion_observada = round((1.0 - m_df$ndvi) * 100 + rnorm(nrow(m_df), 0, 5), 1),
        stringsAsFactors = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )

  output$download_ieo_pdf <- downloadHandler(
    filename = function() {
      "Manual_Tactico_Campana_Magallanes_IEO.txt"
    },
    content = function(file) {
      text_content <- c(
        "==========================================================================",
        "INFORME T\u00c9CNICO, METODOL\u00d3GICO Y LOG\u00cdSTICO DE TERRENO (R\u00daBRICA DE CAMPO IEO)",
        "==========================================================================",
        "DOCUMENTO DE APOYO LOG\u00cdSTICO Y PROTOCOLO DE CAMPO (MAGALLANES SPIRIT)",
        "",
        "1. DISE\u00d1O EXPERIMENTAL (DOBLE CIEGO)",
        "- Observador de terreno (ciego al modelo): Aplica la r\u00fabrica IEO in situ.",
        "- Operador de datos y dron (no ciego): Pilota el dron y registra metadata.",
        "- Conductor log\u00edstico (no ciego): Controla tiempos de traslado y rutas.",
        "",
        "2. R\u00daBRICA DEL \u00cdNDICE DE ESTADO OBSERVADO (IEO)",
        "Para cada p\u00edxel ontol\u00f3gico, eval\u00fae en escala de 1 a 10:",
        "- FRICCI\u00d3N OBSERVADA (Obst\u00e1culos f\u00edsicos, pendiente, deterioro, delincuencia).",
        "- COHESI\u00d3N OBSERVADA (Redes de apoyo mutuo, vecindarios solidarios, huertos).",
        "",
        "3. PROTOCOLO DE CONTINGENCIA",
        "- En fr\u00edo extremo (sensaci\u00f3n < 0\u00b0C), guarde bater\u00edas en bolsillos interiores.",
        "- Realice muestreos adaptativos intensificando en zonas de borde o transici\u00f3n.",
        "=========================================================================="
      )
      writeLines(text_content, file)
    }
  )
  
  output$download_care_guide <- downloadHandler(
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
  
  # ---- 5. CONTRASTE Y VALIDACI\u00d3N ESTAD\u00cdSTICA (IEO VS MODELO) ----
  ieo_uploaded_data <- reactive({
    req(input$ieo_file)
    tryCatch({
      df <- read.csv(input$ieo_file$datapath, stringsAsFactors = FALSE)
      if (!all(c("pixel_id", "friccion_observada", "cohesion_observada") %in% colnames(df))) {
        showNotification("Error: CSV debe tener columnas 'pixel_id', 'friccion_observada' y 'cohesion_observada'", type = "error")
        return(NULL)
      }
      df
    }, error = function(e) {
      showNotification(paste("Error leyendo CSV:", e$message), type = "error")
      return(NULL)
    })
  })
  
  ieo_contrast_stats <- reactive({
    df_field <- ieo_uploaded_data()
    req(df_field)
    
    df_sim <- current_manzanas_df()
    req(df_sim)
    
    merged <- merge(df_sim, df_field, by.x = "id", by.y = "pixel_id")
    if (nrow(merged) < 3) return(NULL)
    
    merged$friccion_simulada <- merged$altitud * (1.0 - merged$ndvi)
    
    fit <- lm(friccion_observada ~ friccion_simulada, data = merged)
    sum_fit <- summary(fit)
    
    r_val <- cor(merged$friccion_simulada, merged$friccion_observada)
    r2_val <- sum_fit$r.squared
    p_val <- sum_fit$coefficients[2, 4]
    
    list(data = merged, r = r_val, r2 = r2_val, p = p_val, fit = fit)
  })
  
  output$ieo_contrast_plot <- renderPlotly({
    stats <- ieo_contrast_stats()
    if (is.null(stats)) {
      return(plot_ly(type = 'scatter', mode = 'markers') %>%
               layout(title = "Cargue datos CSV para contrastar",
                      paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)'))
    }
    
    df <- stats$data
    fit <- stats$fit
    df$pred <- predict(fit)
    
    plot_ly(df, x = ~friccion_simulada, y = ~friccion_observada, type = 'scatter', mode = 'markers',
            name = 'P\u00edxeles de Campo', marker = list(color = '#0284c7', size = 8)) %>%
      add_lines(x = ~friccion_simulada, y = ~pred, name = 'Ajuste Lineal', line = list(color = '#10b981', width = 2)) %>%
      layout(
        xaxis = list(title = "Fricci\u00f3n Met\u00e1lica Simulada", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155")),
        yaxis = list(title = "Fricci\u00f3n Peatonal Observada (IEO)", gridcolor = "rgba(15,23,42,0.06)", titlefont = list(color = "#334155")),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = TRUE
      )
  })
  
  output$ieo_contrast_metrics <- renderUI({
    stats <- ieo_contrast_stats()
    if (is.null(stats)) {
      return(div(style = "color: #475569; font-style: italic; margin-top:10px;", "Esperando archivo CSV para contrastar..."))
    }
    
    significant <- stats$p < 0.05
    banner_color <- if (significant) "rgba(16, 185, 129, 0.08)" else "rgba(239, 68, 68, 0.08)"
    border_color <- if (significant) "rgba(16, 185, 129, 0.2)" else "rgba(239, 68, 68, 0.2)"
    text_color <- if (significant) "#10b981" else "#ef4444"
    status_text <- if (significant) "VALIDACI\u00d3N EXITOSA: Consistencia Estad\u00edstica Cr\u00edtica (p < 0.05)" else "RUIDO ELEVADO: Sin significaci\u00f3n estad\u00edstica (p >= 0.05)"
    
    div(style = sprintf("background: %s; border: 1px solid %s; border-radius: 8px; padding: 12px; margin-top: 10px; color:#334155;", banner_color, border_color),
      fluidRow(
        column(4, tags$p(style = "margin:0;", tags$b("Pearson r: "), round(stats$r, 4))),
        column(4, tags$p(style = "margin:0;", tags$b("Determinaci\u00f3n R\u00b2: "), round(stats$r2, 4))),
        column(4, tags$p(style = "margin:0;", tags$b("p-valor: "), round(stats$p, 5)))
      ),
      tags$p(style = sprintf("margin-top: 5px; font-weight:600; color: %s; margin-bottom:0;", text_color), status_text)
    )
  })
  
  # ---- DESCARGA DEL REPORTE MATEM\u00c1TICO ----
  output$download_math_report <- downloadHandler(
    filename = function() {
      paste0("reporte_matematico_geotensores_", Sys.Date(), ".txt")
    },
    content = function(file) {
      is_en <- identical(lang(), "EN")
      omega <- if (!is.null(input$lyap_vol)) input$lyap_vol else 0.2
      alpha_base <- if (!is.null(input$alpha_caputo)) input$alpha_caputo else 0.6
      
      # Memoria fraccionaria din\u00e1mica acoplada a la tensi\u00f3n del p\u00edxel activo
      id_sel <- selected_pixel()
      g_data <- current_graph_data()
      idx_graph <- which(g_data$manzanaId == id_sel)
      alpha <- alpha_base
      if (length(idx_graph) > 0) {
        gent_tension <- g_data$tensorMemoria[[idx_graph]]$tensionGentrificacion[1]
        alpha <- max(0.2, alpha_base - gent_tension * 0.3)
      }
      exp_mode <- if (!is.null(input$exp_mode)) input$exp_mode else "base"
      
      lines <- c(
        "===========================================================",
        if (is_en) "GEOTENSORS: MATHEMATICAL ANALYSIS REPORT" else "GEOTENSORES: REPORTE DE AN\u00c1LISIS MATEM\u00c1TICO",
        "===========================================================",
        paste(if (is_en) "Date:" else "Fecha:", Sys.Date()),
        paste(if (is_en) "Active Experiment:" else "Experimento Activo:", exp_mode),
        paste(if (is_en) "Caputo Fractional Memory Order (alpha):" else "Orden de Memoria Fraccionaria de Caputo (alpha):", alpha),
        paste(if (is_en) "Lyapunov Damping Coefficient (omega):" else "Coeficiente de Amortiguamiento de Lyapunov (omega):", omega),
        "-----------------------------------------------------------",
        if (is_en) "ONTOLOGICAL REFLECTION / POETIC INTERPRETATION:" else "REFLEXI\u00d3N ONTOL\u00d3GICA / INTERPRETACI\u00d3N PO\u00c9TICA:",
        "-----------------------------------------------------------"
      )
      
      # Memory
      mem_poet <- if (alpha < 0.45) {
        if (is_en) {
          "Collective Memory (alpha low): The fractional memory order (alpha) is low, representing a high-memory regime. Past trauma clings heavily to the soil; walking paths are curved by the non-local pull of memory, refusing to fade into absolute Euclidean space."
        } else {
          "Memoria Colectiva (alpha bajo): El orden fraccionario de memoria (alpha) es bajo, representando un r\u00e9gimen de alta persistencia. El trauma pasado se aferra con fuerza al suelo; las trayectorias de caminata se curvan por la atracci\u00f3n no local del recuerdo, neg\u00e1ndose a disolverse en el espacio euclidiano absoluto."
        }
      } else {
        if (is_en) {
          "Collective Memory (alpha high): The fractional memory order is high, showing rapid localized decay. The pedestrian walks with minimal historical drag, adapting directly to local friction gradients."
        } else {
          "Memoria Colectiva (alpha alto): El orden fraccionario de memoria es alto, mostrando un decaimiento localizado r\u00e1pido. El peat\u00f3n se desplaza con un arrastre hist\u00f3rico m\u00ednimo, adapt\u00e1ndose directamente a las gradientes inmediatas de fricci\u00f3n local."
        }
      }
      
      # Dissipation
      diss_poet <- if (omega > 0.5) {
        if (is_en) {
          "Energy and Will (omega high): Lyapunov dissipation is high. The city drains vital energy quickly, reflecting exhaustion under real estate enclosure or spatial isolation. The walker is forced into local minima, representing structural fatigue."
        } else {
          "Energ\u00eda y Voluntad (omega alto): La disipaci\u00f3n de Lyapunov es elevada. La ciudad drena la energ\u00eda vital de forma acelerada, reflejando el agotamiento pedestre bajo cercamientos inmobiliarios o aislamiento espacial. El caminante queda atrapado en pozos locales, representando fatiga estructural."
        }
      } else {
        if (is_en) {
          "Energy and Will (omega low): Lyapunov energy dissipation is low. The pedestrian retains their territorial will, enabling them to cross boundaries and complete long drifts through the relational topology."
        } else {
          "Energ\u00eda y Voluntad (omega bajo): La disipaci\u00f3n de Lyapunov es baja. El peat\u00f3n conserva su voluntad territorial, lo que le permite sortear bordes de fricci\u00f3n y completar largas derivas a trav\u00e9s de la topolog\u00eda relacional."
        }
      }
      
      lines <- c(lines, mem_poet, "", diss_poet)
      writeLines(lines, file, useBytes = TRUE)
    }
  )

  # ---- DESCARGA DE LA MALLA GEOMETRICA ACTIVA EN GEOJSON ----
  output$download_geojson <- downloadHandler(
    filename = function() {
      paste0("geotensores_malla_activa_", Sys.Date(), ".geojson")
    },
    content = function(file) {
      m_df <- current_manzanas_df()
      m_sf <- st_as_sf(m_df, coords = c("x", "y"), crs = UTM_CRS) %>%
        st_transform(4326)
      sf::st_write(m_sf, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  # ---- TAB 3: CONTENEDOR DE KPIS DEL SOLUCIONADOR ----
  output$solver_kpis_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    fluidRow(
      column(4, div(class = "card-kpi", div(class = "kpi-title", if (is_en) "Care Friction" else "Fricci\u00f3n Cuidado"), uiOutput("kpi_fric"), div(class = "kpi-unit", "||\u2207V||_g prom"))),
      column(4, div(class = "card-kpi", div(class = "kpi-title", if (is_en) "Pedestrian Torsion" else "Torsi\u00f3n Peatonal"), uiOutput("kpi_tors"), div(class = "kpi-unit", "Declinativa rad"))),
      column(4, div(class = "card-kpi", div(class = "kpi-title", if (is_en) "Geodesic Efficiency" else "Eficiencia Geod\u00e9sica"), uiOutput("kpi_eff"), div(class = "kpi-unit", "Wu Wei / L_eucl")))
    )
  })
}