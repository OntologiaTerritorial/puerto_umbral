# =====================================================================
# Puerto Umbral V5.4 - Centro de Control de Física Intrínseca (Tomo II)
# Visualización e Interacción Avanzada: Mapas Conectados y Perfiles Peatonales
# Autor / Contacto: john.treimun.r@uai.cl
# =====================================================================

# ---- AJUSTE AUTOM\u00c1TICO DE DIRECTORIO DE TRABAJO (RStudio / Windows) ----
if (dir.exists("puerto_umbral_zenodo_bundle/app")) {
  setwd("puerto_umbral_zenodo_bundle/app")
} else if (basename(getwd()) != "app" && dir.exists("app")) {
  setwd("app")
}

# Workaround para problemas de descarga en Chromium bajo Shinylive
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
downloadLink <- function(...) {
  tag <- shiny::downloadLink(...)
  tag$attribs$download <- NULL
  tag
}
suppressPackageStartupMessages({
  library(shiny)
  
  # Registrar ruta de recursos para la carpeta media (PDFs y Videos)
  media_dir <- "../media"
  if (!dir.exists(media_dir)) media_dir <- "media"
  if (!dir.exists(media_dir)) media_dir <- "puerto_umbral_zenodo_bundle/media"
  if (dir.exists(media_dir)) {
    addResourcePath("media", media_dir)
  }
  
UTM_CRS <- 32719 # Proyecci\u00f3n UTM por defecto (Santiago UTM 19S)
  library(leaflet)
  library(plotly)
  library(jsonlite)
  library(dplyr)
  library(RColorBrewer)
  library(scales)
  library(sf)
  library(RSQLite)

  library(shinyjs)
  library(DBI)
  library(shinythemes)
})

# Opciones de depuraci\u00f3n (Sanitizar en produccion, mostrar trazas si PUERTO_DEBUG=TRUE)
options(shiny.sanitize.errors = !(Sys.getenv("PUERTO_DEBUG") == "TRUE"))

# ==================== HISTORIAL Y MEMORIA CAPUTO ====================
# NOTA DE DESARROLLO Y ESCALABILIDAD COMPUTACIONAL:
# La evaluación del historial no markoviano de Caputo requiere un bucle anidado sobre los pasos temporales.
# En aplicaciones metropolitanas multi-agente a gran escala fuera de la academia, este bucle R
# debe ser migrado a C++ usando Rcpp para evitar el overhead del intérprete.
# Ejemplo de firma C++ sugerida:
# // [[Rcpp::export]]
# // NumericVector compute_caputo_history_cpp(double base_val, double alpha, int L, int steps) { ... }
# La vectorización directa mediante Rcpp reduce los tiempos de cómputo en un factor de ~100x.
compute_caputo_history <- function(base_val, alpha, L, steps = 6) {
  y <- numeric(steps)
  y[1] <- base_val
  omega <- 0.3
  dt_val <- 1.0
  
  c_coeff <- numeric(steps)
  c_coeff[1] <- 1
  if (steps > 1) {
    for (j in 2:steps) {
      c_coeff[j] <- c_coeff[j-1] * (1 - (alpha + 1) / (j - 1))
    }
  }
  
  for (k in 2:steps) {
    val_sum <- 0
    for (j in 2:k) {
      if ((j - 1) <= L) {
        val_sum <- val_sum + c_coeff[j] * y[k - j + 1]
      }
    }
    y[k] <- -val_sum / (1 + omega * dt_val^alpha)
  }
  pmax(0, pmin(1, y))
}

tr <- list(
  ES = list(
    title = "Puerto Umbral V5.4 - Centro de Control de F\u00edsica Intr\u00ednseca (Tomo II)",
    tab1 = "Inicio / Pedagog\u00eda",
    tab2 = "Centro de Simulaci\u00f3n",
    tab3 = "An\u00e1lisis Matem\u00e1tico",
    h1_title = "Puerto Umbral: El Manifold Territorial (Tomo II)",
    p1_desc = "Esta aplicaci\u00f3n act\u00faa como el resolvedor interactivo de f\u00edsica intr\u00ednseca territorial de la obra. Explora los 7 experimentos te\u00f3ricos y de campo en R-Shiny interactivo, contrastando el modelo con datos de terreno.",
    h3_mosaico = "Mosaico de Experimentos Cient\u00edficos",
    exp1_title = "1. Refracci\u00f3n de Borde (Snell)",
    exp1_desc = "Mide c\u00f3mo el salto abrupto de m\u00e9trica entre dos \u00e1reas (urbano/rural) desv\u00eda la trayectoria del peat\u00f3n en el borde.",
    exp2_title = "2. Desviaci\u00f3n Geod\u00e9sica (Segregaci\u00f3n)",
    exp2_desc = "Modelamiento de la desviaci\u00f3n de trayectorias peatonales generada por colinas de exclusi\u00f3n y fricci\u00f3n social.",
    exp3_title = "3. Autopoiesis (Atractor)",
    exp3_desc = "Inversi\u00f3n de la curvatura Hessiana local bajo presi\u00f3n social cr\u00edtica, tornando repulsores en atractores solidarios.",
    exp4_title = "4. Memoria del Trauma (Caputo)",
    exp4_desc = "Persistencia temporal del trauma en las trayectorias de memoria con decaimiento de orden fraccionario no markoviano.",
    exp5_title = "5. M\u00e9trica de Moran (Ledoit-Wolf)",
    exp5_desc = "Compensaci\u00f3n del ruido geoestad\u00edstico espacial con regularizaci\u00f3n local Ledoit-Wolf para muestras limitadas.",
    exp6_title = "6. Santuario Ecol\u00f3gico (Robin)",
    exp6_desc = "Caso real en Pe\u00f1alol\u00e9n: l\u00edmites asim\u00e9tricos y condiciones de Robin que canalizan los flujos de conservaci\u00f3n.",
    exp7_title = "7. Refracci\u00f3n de Capital",
    exp7_desc = "Desviaci\u00f3n de flujos financieros y plusval\u00eda inmobiliaria acumulada en el l\u00edmite periurbano.",
    btn_load_exp = "Cargar Experimento",
    ergonomics_title = "Ergonom\u00eda de Terreno",
    mode_field = "Activar Modo Campo (Alto Contraste)",
    mode_solo_blind = "Activar Modo Solo (Auto-Ceguera)",
    tactics_title = "T\u00e1cticas de Terreno (Magallanes)",
    tactics_desc = "Siga estos protocolos log\u00edsticos en campa\u00f1as extremas:",
    tactic1 = "Ceguera Experimental: El observador peatonal del IEO debe aplicar la r\u00fabrica sin ver las geod\u00e9sicas en el mapa para evitar sesgos.",
    tactic2 = "Modo Solo: Si levanta solo, active 'Modo Solo: Auto-Ceguera' en la simulaci\u00f3n para bloquear las capas te\u00f3ricas durante la recolecci\u00f3n.",
    tactic3 = "Autonom\u00eda T\u00e9rmica: Mantenga los dispositivos en bolsillos interiores para evitar la degradaci\u00f3n r\u00e1pida de bater\u00edas por fr\u00edo.",
    downloads_title = "Descargas del Bundle",
    download_ieo_btn = "Ficha IEO (TXT) Offline",
    download_csv_btn = "Plantilla Datos (CSV)",
    control_title = "Control del Territorio",
    sec1_title = "1. Configuraci\u00f3n de Escenario",
    sec2_title = "2. Eventos y Distorsiones",
    sec3_title = "3. Campos de Fuerza Globales",
    sec4_title = "4. Geometr\u00eda y Solucionador",
    sec5_title = "5. Cargar Datos Externos (Zenodo)",
    narrative_label = "Caso de la Obra (Auto-Configura):",
    ped_profile_label = "Perfil Peatonal (Wu Wei):",
    dim_3d_label = "Dimensi\u00f3n Superficie 3D:",
    lie_seq_label = "Secuencia de Acciones (Lie):",
    exp_mode_label = "Modo Experimento (Tomo II):",
    click_mode_label = "Acci\u00f3n de Clic en Mapa:",
    dist_type_label = "Tipo de Impacto:",
    dist_mag_label = "Magnitud del Evento:",
    clear_dist_btn = "Limpiar Todo",
    origen_id_label = "Manzana Origen (A):",
    destino_id_label = "Manzana Destino (B):",
    use_potentials_label = "Activar Campos de Fuerza (V)",
    pot_wall_amp_label = "Zonas de Desgaste (V_canal):",
    pot_dest_amp_label = "N\u00facleos de Contenci\u00f3n (V_sentido):",
    pot_slope_amp_label = "Fricci\u00f3n de Pendiente (V_loma):",
    show_vectors_label = "Mostrar Vectores de Fuerza 3D",
    solve_bvp_label = "Resolver Disparo BVP",
    v0_label = "Nivel de Energ\u00eda Vital / Carga (||v0||):",
    lyap_vol_label = "Damping de Lyapunov (\u03a9):",
    lambda_val_label = "Rigidez del Manifold (\u03bb):",
    ruido_tactico_label = "Activar Ruido Estoc\u00e1stico (T\u00e1ctica Urbana)",
    csv_file_label = "Atributos Territoriales (CSV):",
    geojson_file_label = "Malla Territorial (GeoJSON):",
    ingest_btn = "Ingestar e Iniciar Resolvedor",
    geodesic_btn = "Disparar Geod\u00e9sica Wu Wei",
    map2d_title = "2D Map: Geodesics and Interaction",
    map3d_title = "3D Mesh: Deformed Manifold (Connected Maps)",
    hud_title = "HUD de P\u00edxel Ontol\u00f3gico",
    pixel_note = "Nota: Haz clic en una manzana del Mapa 2D con el modo de clic 'Ver Ficha' para cargar sus capas ontol\u00f3gicas aqu\u00ed.",
    solver_kpis_title = "Estado del Solucionador y KPIs",
    kpi_fric_title = "Fricci\u00f3n Cuidado",
    kpi_tors_title = "Torsi\u00f3n Peatonal",
    kpi_eff_title = "Eficiencia Geod\u00e9sica",
    tab_lie = "Conmutador de Lie [X, Y]",
    tab_stability = "Estabilidad y Autovalores",
    tab_lyapunov = "Decaimiento de Lyapunov",
    alpha_caputo_label = "Orden Caputo Fraccionario (\u03b1) - Memoria del Trauma:",
    saving_mode_label = "Modo Ahorro de Energ\u00eda (Congelar 3D)"
  ),
  EN = list(
    title = "Puerto Umbral V5.4 - Intrinsic Physics Control Center (Volume II)",
    tab1 = "Home / Onboarding",
    tab2 = "Simulation Center",
    tab3 = "Mathematical Analysis",
    h1_title = "Puerto Umbral: The Territorial Manifold (Volume II)",
    p1_desc = "This application acts as the interactive solver for the intrinsic territorial physics of the work. It explores the seven theoretical and field experiments in R/Shiny, contrasting the model with territorial observations.",
    h3_mosaico = "Scientific Experiment Mosaic",
    exp1_title = "1. Edge Refraction (Snell)",
    exp1_desc = "Measures how the abrupt jump in metrics between two areas (urban/rural) deflects the pedestrian trajectory at the edge.",
    exp2_title = "2. Geodesic Deviation (Segregation)",
    exp2_desc = "Modeling of pedestrian trajectory deviation generated by exclusion hills and social friction.",
    exp3_title = "3. Autopoiesis (Attractor)",
    exp3_desc = "Inversion of local Hessian curvature under critical social pressure, turning repellers into solidarity attractors.",
    exp4_title = "4. Trauma Memory (Caputo)",
    exp4_desc = "Temporal persistence of trauma in memory trajectories with fractional-order non-Markovian decay.",
    exp5_title = "5. Moran Metric (Ledoit-Wolf)",
    exp5_desc = "Compensation of spatial geo-statistical noise with Ledoit-Wolf local regularization for limited samples.",
    exp6_title = "6. Ecological Sanctuary (Robin)",
    exp6_desc = "Real case in Pe\u00f1alol\u00e9n: asymmetric boundaries and Robin conditions channeling ecological conservation flows.",
    exp7_title = "7. Capital Refraction",
    exp7_desc = "Deflection of financial flows and accumulated real estate surplus value at the peri-urban boundary.",
    btn_load_exp = "Load Experiment",
    ergonomics_title = "Field Ergonomics",
    mode_field = "Activate Field Mode (High Contrast)",
    mode_solo_blind = "Activate Solo Mode (Self-Blindness)",
    tactics_title = "Field Tactics (Magallanes)",
    tactics_desc = "Follow these logistical protocols in extreme campaigns:",
    tactic1 = "Experimental Blindness: The IEO pedestrian observer must apply the rubric in situ without seeing the geodesics on the map to avoid bias.",
    tactic2 = "Solo Mode: If surveying alone, activate 'Solo Mode: Self-Blindness' in the simulation to block theoretical layers during collection.",
    tactic3 = "Thermal Autonomy: Keep devices in inner pockets to prevent rapid battery degradation due to cold.",
    downloads_title = "Bundle Downloads",
    download_ieo_btn = "IEO Sheet (TXT) Offline",
    download_csv_btn = "Data Template (CSV)",
    control_title = "Territory Control",
    sec1_title = "1. Scenario Configuration",
    sec2_title = "2. Events and Distortions",
    sec3_title = "3. Global Force Fields",
    sec4_title = "4. Geometry and Solver",
    sec5_title = "5. Upload External Data (Zenodo)",
    narrative_label = "Scenario of the Book (Auto-Configures):",
    ped_profile_label = "Pedestrian Profile (Wu Wei):",
    dim_3d_label = "3D Surface Dimension:",
    lie_seq_label = "Lie Action Sequence:",
    exp_mode_label = "Experiment Mode (Volume II):",
    click_mode_label = "Map Click Action:",
    dist_type_label = "Impact Type:",
    dist_mag_label = "Event Magnitude:",
    clear_dist_btn = "Clear All",
    origen_id_label = "Origin Block (A):",
    destino_id_label = "Destination Block (B):",
    use_potentials_label = "Activate Force Fields (V)",
    pot_wall_amp_label = "Friction Zones (V_canal):",
    pot_dest_amp_label = "Containment Cores (V_sense):",
    pot_slope_amp_label = "Slope Friction (V_hill):",
    show_vectors_label = "Show 3D Force Vectors",
    solve_bvp_label = "Solve BVP Shooting",
    v0_label = "Vital Energy Level / Charge (||v0||):",
    lyap_vol_label = "Lyapunov Damping (\u03a9):",
    lambda_val_label = "Manifold Stiffness (\u03bb):",
    ruido_tactico_label = "Activate Stochastic Noise (Urban Tactics)",
    csv_file_label = "Territorial Attributes (CSV):",
    geojson_file_label = "Territorial Mesh (GeoJSON):",
    ingest_btn = "Ingest & Start Solver",
    geodesic_btn = "Trigger Wu Wei Geodesic",
    map2d_title = "2D Map: Geodesics and Interaction",
    map3d_title = "3D Mesh: Deformed Manifold (Connected Maps)",
    hud_title = "Ontological Pixel HUD",
    pixel_note = "Note: Click on a block in the 2D Map with the click mode 'View Fiche' to load its ontological layers here.",
    solver_kpis_title = "Solver Status and KPIs",
    kpi_fric_title = "Care Friction",
    kpi_tors_title = "Pedestrian Torsion",
    kpi_eff_title = "Geodesic Efficiency",
    tab_lie = "Lie Commutator [X, Y]",
    tab_stability = "Stability and Eigenvalues",
    tab_lyapunov = "Lyapunov Decay",
    alpha_caputo_label = "Fractional Caputo Order (\u03b1) - Trauma Memory:",
    saving_mode_label = "Energy Saving Mode (Freeze 3D)"
  )
)

# ==================== CARGA DE DATOS DESDE SQLITE ====================
db_path <- "../geotensor_experimentos.db"
if (!file.exists(db_path)) db_path <- "geotensor_experimentos.db"
if (!file.exists(db_path)) db_path <- "app/geotensor_experimentos.db"

if (!file.exists(db_path)) {
  stop("No se encuentra geotensor_experimentos.db. Coloque el archivo en la raiz de 'app' o regenere la base con 'python scripts/generar_experimentos_db.py'.")
}

conn <- dbConnect(SQLite(), dbname = db_path)

# Cargar p\u00edxeles reales (Capa 1 y Capa 2) para el experimento inicial
pixeles_db <- dbGetQuery(conn, "SELECT id, x, y, altitud, ndvi, red_cuidado FROM pixeles WHERE experimento_id = 6")
if (nrow(pixeles_db) == 0) {
  pixeles_db <- dbGetQuery(conn, "SELECT id, x, y, altitud, ndvi, red_cuidado FROM pixeles WHERE experimento_id = 1")
}

# Cargar memorias y latencias
memorias_db <- dbGetQuery(conn, "SELECT pixel_id, timestamp_simulado, atraccion_H_i FROM pixel_memorias")
latencias_db <- dbGetQuery(conn, "SELECT pixel_id, timestamp_simulado, magnitud_friccion FROM pixel_latencias")

dbDisconnect(conn)

manzanas_df <- data.frame(
  id = as.character(pixeles_db$id),
  x = as.numeric(pixeles_db$x),
  y = as.numeric(pixeles_db$y),
  ndvi = as.numeric(pixeles_db$ndvi),
  altitud = as.numeric(pixeles_db$altitud),
  red_cuidado = as.character(pixeles_db$red_cuidado),
  stringsAsFactors = FALSE
)

# Construir estructura de graph_data compatible
m_list <- split(memorias_db, memorias_db$pixel_id)
l_list <- split(latencias_db, latencias_db$pixel_id)

tensor_memoria_list <- lapply(manzanas_df$id, function(p_id) {
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
    pasoTiempo = 1:length(tension),
    tensionGentrificacion = tension_norm,
    cohesionSocial = cohesion_norm,
    stringsAsFactors = FALSE
  )
})
names(tensor_memoria_list) <- manzanas_df$id

graph_data <- list(
  manzanaId = manzanas_df$id,
  tensorMemoria = tensor_memoria_list
)

# Proyectar UTM a Lat/Long para Leaflet
manzanas_sf <- st_as_sf(manzanas_df, coords = c("x", "y"), crs = UTM_CRS) %>%
  st_transform(4326)
coords_wgs84 <- st_coordinates(manzanas_sf)
manzanas_df$lng <- coords_wgs84[, 1]
manzanas_df$lat <- coords_wgs84[, 2]

# Cargar simulaci\u00f3n avanzada del motor de Python con FALLBACK seguro en R
sim_path <- "../simulacion_avanzada.json"
if (!file.exists(sim_path)) sim_path <- "simulacion_avanzada.json"
if (!file.exists(sim_path)) sim_path <- "app/simulacion_avanzada.json"
if (!file.exists(sim_path)) sim_path <- "../../simulacion_avanzada.json"

if (file.exists(sim_path)) {
  sim_avanzada <- jsonlite::fromJSON(sim_path, simplifyVector = TRUE)
} else {
  warning("simulacion_avanzada.json no encontrada. Cargando fallback R nativo para Zenodo.")
  xg <- seq(350000, 360000, length.out = 30)
  yg <- seq(6290000, 6300000, length.out = 30)
  grid_mat_X_Y <- t(outer(xg, yg, function(x, y) { sin(x/1000) * cos(y/1000) * 1000 }))
  grid_mat_Y_X <- t(outer(xg, yg, function(x, y) { sin(x/1000) * cos(y/1000) * 980 }))
  grid_mat_diff <- grid_mat_X_Y - grid_mat_Y_X
  
  # Generar 15 trayectorias sint\u00e9ticas peatonales de fondo
  n_traj <- 15
  reached_list <- rep(TRUE, n_traj)
  x_list <- list()
  y_list <- list()
  destino_list <- rep("Equipamiento", n_traj)
  avg_fric_list <- rep(1200, n_traj)
  
  for (i in 1:n_traj) {
    idx_start <- ((i * 7) %% nrow(manzanas_df)) + 1
    idx_end <- ((i * 13) %% nrow(manzanas_df)) + 1
    
    start_pt <- manzanas_df[idx_start, ]
    end_pt <- manzanas_df[idx_end, ]
    
    steps <- 15
    tx <- seq(start_pt$x, end_pt$x, length.out = steps)
    ty <- seq(start_pt$y, end_pt$y, length.out = steps)
    tx <- tx + sin(seq(0, pi, length.out = steps)) * 100
    
    x_list[[i]] <- tx
    y_list[[i]] <- ty
    reached_list[i] <- (i %% 5 != 0)
    avg_fric_list[i] <- 800 + (i * 150) %% 2000
  }
  
  trayectorias_df <- data.frame(
    reached = reached_list,
    avg_friccion = avg_fric_list,
    destino = destino_list,
    stringsAsFactors = FALSE
  )
  trayectorias_df$x <- x_list
  trayectorias_df$y <- y_list
  
  # Generar 5 flujos de fondo (corredores)
  flujos_x <- list()
  flujos_y <- list()
  flujos_nombre <- c("Av. Grecia", "Av. Tobalaba", "Diag. Las Torres", "Av. Consistorial", "Av. Las Perdices")
  for (i in 1:5) {
    idx_start <- ((i * 9) %% nrow(manzanas_df)) + 1
    idx_end <- ((i * 17) %% nrow(manzanas_df)) + 1
    start_pt <- manzanas_df[idx_start, ]
    end_pt <- manzanas_df[idx_end, ]
    
    flujos_x[[i]] <- seq(start_pt$x, end_pt$x, length.out = 10)
    flujos_y[[i]] <- seq(start_pt$y, end_pt$y, length.out = 10)
  }
  
  flujos_fondo_df <- list(
    x = flujos_x,
    y = flujos_y,
    nombre = flujos_nombre
  )
  
  default_scen <- list(
    nombre = "Caso A: Modelo Base",
    superficie = grid_mat_X_Y,
    avg_friccion_global = 12.5,
    avg_torsion_global = 0.05,
    trayectorias = trayectorias_df,
    flujos_fondo = flujos_fondo_df
  )
  
  scenarios_list <- list()
  for (t in c("Dia", "Noche")) {
    for (s in c("Verano", "Invierno")) {
      key <- paste0(t, "_", s)
      scenarios_list[[key]] <- default_scen
    }
  }
  
  sim_avanzada <- list(
    info = list(
      xg = xg,
      yg = yg,
      manzanas_calib = NULL
    ),
    lie_algebra = list(
      surface_X_Y = grid_mat_X_Y,
      surface_Y_X = grid_mat_Y_X,
      surface_diff = grid_mat_diff
    ),
    escenarios = scenarios_list
  )
}

# === Datos IPF (Iterative Proportional Fitting) para calibraci\u00f3n demogr\u00e1fica ===
# Intentar cargar manzanas_calib desde el JSON; si no existe, generar datos sint\u00e9ticos
ipf_calib_data <- tryCatch({
  if (!is.null(sim_avanzada$info$manzanas_calib)) {
    df <- sim_avanzada$info$manzanas_calib
    data.frame(
      id = as.character(df$id),
      persons_orig   = as.numeric(df$original$persons),
      e4a18_orig     = as.numeric(df$original$e4a18),
      e15a24_orig    = as.numeric(df$original$e15a24),
      persons_calib  = as.numeric(df$calibrado$persons),
      e4a18_calib    = as.numeric(df$calibrado$e4a18),
      e15a24_calib   = as.numeric(df$calibrado$e15a24),
      stringsAsFactors = FALSE
    )
  } else {
    # Generar tabla IPF sint\u00e9tica a partir de las manzanas existentes
    set.seed(42)
    n_mz <- nrow(manzanas_df)
    data.frame(
      id = manzanas_df$id,
      persons_orig   = sample(80:320, n_mz, replace = TRUE),
      e4a18_orig     = sample(10:60,  n_mz, replace = TRUE),
      e15a24_orig    = sample(15:70,  n_mz, replace = TRUE),
      persons_calib  = sample(85:310, n_mz, replace = TRUE),
      e4a18_calib    = sample(12:55,  n_mz, replace = TRUE),
      e15a24_calib   = sample(18:65,  n_mz, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }
}, error = function(e) {
  set.seed(42)
  n_mz <- nrow(manzanas_df)
  data.frame(
    id = manzanas_df$id,
    persons_orig   = sample(80:320, n_mz, replace = TRUE),
    e4a18_orig     = sample(10:60,  n_mz, replace = TRUE),
    e15a24_orig    = sample(15:70,  n_mz, replace = TRUE),
    persons_calib  = sample(85:310, n_mz, replace = TRUE),
    e4a18_calib    = sample(12:55,  n_mz, replace = TRUE),
    e15a24_calib   = sample(18:65,  n_mz, replace = TRUE),
    stringsAsFactors = FALSE
  )
})

# Conversor de UTM a WGS84
utm_to_wgs84 <- function(x, y) {
  pts <- st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = UTM_CRS) %>%
    st_transform(4326)
  st_coordinates(pts)
}

utm_to_wgs84_vector <- function(x_vec, y_vec) {
  df <- data.frame(x = x_vec, y = y_vec)
  pts <- st_as_sf(df, coords = c("x", "y"), crs = UTM_CRS) %>%
    st_transform(4326)
  coords <- st_coordinates(pts)
  list(lng = coords[, 1], lat = coords[, 2])
}

wall_p1 <- utm_to_wgs84(352500, 6300000)
wall_p2 <- utm_to_wgs84(352500, 6305000)

# Cruces habilitados (puentes) en la barrera urbana
bridge_p1_utm <- c(352500, 6294400)
bridge_p2_utm <- c(352500, 6295300)
bridge1 <- utm_to_wgs84(bridge_p1_utm[1], bridge_p1_utm[2])
bridge2 <- utm_to_wgs84(bridge_p2_utm[1], bridge_p2_utm[2])

# Helper para calcular la Red de Cuidado Comunitaria como un enlace de polil\u00edneas
compute_care_links <- function(df) {
  c_df <- df[df$red_cuidado != "Ninguno", ]
  lngs <- c()
  lats <- c()
  if (nrow(c_df) >= 2) {
    for (i in 1:(nrow(c_df)-1)) {
      for (j in (i+1):nrow(c_df)) {
        dist <- sqrt((c_df$x[i] - c_df$x[j])^2 + (c_df$y[i] - c_df$y[j])^2)
        if (dist < 600) { # Conectar manzanas de cuidado si est\u00e1n a menos de 600 metros
          lngs <- c(lngs, c_df$lng[i], c_df$lng[j], NA)
          lats <- c(lats, c_df$lat[i], c_df$lat[j], NA)
        }
      }
    }
  }
  list(lng = lngs, lat = lats)
}
care_links <- compute_care_links(manzanas_df)

# === Enmascaramiento Donut Jittering (Ecuaci\u00f3n de Langevin) ===
# Implementaci\u00f3n fiel del Ap\u00e9ndice B.1 (segundo orden, potencial c\u00fabico)
donut_jitter_langevin <- function(x, y, r_min = 15, r_max = 45, steps = 10, dt = 0.1, gamma = 0.5, temp = 0.1, n_steps = NULL, sigma = NULL) {
  if (!is.null(n_steps)) steps <- n_steps
  if (!is.null(sigma)) temp <- sigma^2
  
  # Radial potential function V(r) creating a barrier outside [r_min, r_max]
  get_potential_force <- function(r) {
    if (r < r_min) {
      return(-1.0 * (r - r_min)^3) # push outward
    } else if (r > r_max) {
      return(-1.0 * (r - r_max)^3) # pull inward
    } else {
      return(0.0) # flat potential inside the donut
    }
  }
  
  # Initialize coordinates inside the donut
  theta_init <- runif(1, 0, 2*pi)
  r_init <- runif(1, r_min, r_max)
  px <- x + r_init * cos(theta_init)
  py <- y + r_init * sin(theta_init)
  
  # Velocity initialization
  vx <- 0.0
  vy <- 0.0
  
  for (i in 1:steps) {
    dx <- px - x
    dy <- py - y
    r <- sqrt(dx^2 + dy^2)
    
    ux <- if (r > 0) dx / r else 0.0
    uy <- if (r > 0) dy / r else 0.0
    
    force_mag <- get_potential_force(r)
    fx <- force_mag * ux
    fy <- force_mag * uy
    
    # Langevin stochastic integration
    wx <- rnorm(1, 0, 1)
    wy <- rnorm(1, 0, 1)
    
    vx <- vx + (-gamma * vx + fx) * dt + sqrt(2 * gamma * temp * dt) * wx
    vy <- vy + (-gamma * vy + fy) * dt + sqrt(2 * gamma * temp * dt) * wy
    
    px <- px + vx * dt
    py <- py + vy * dt
  }
  
  return(c(x_new = px, y_new = py))
}

donut_jitter_langevin_vec <- function(x_vec, y_vec, r_min = 15, r_max = 45, steps = 10, dt = 0.1, gamma = 0.5, temp = 0.1, n_steps = NULL, sigma = NULL) {
  if (!is.null(n_steps)) steps <- n_steps
  if (!is.null(sigma)) temp <- sigma^2
  
  n <- length(x_vec)
  result <- matrix(0, nrow = n, ncol = 2)
  for (i in seq_len(n)) {
    res <- donut_jitter_langevin(x_vec[i], y_vec[i], r_min, r_max, steps, dt, gamma, temp)
    result[i, 1] <- res["x_new"]
    result[i, 2] <- res["y_new"]
  }
  data.frame(x = result[, 1], y = result[, 2])
}

# === Regularizaci\u00f3n de Ledoit-Wolf ===
# Calcula la contracci\u00f3n \u00f3ptima anal\u00edtica de la matriz de covarianza
# === Regularizaci\u00f3n de Ledoit-Wolf ===
# Implementaci\u00f3n fiel del Ap\u00e9ndice B.3 (estimador anal\u00edtico insesgado y factor rho)
ledoit_wolf_shrinkage <- function(X, shrinkage_factor = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (n < 2) {
    if (is.null(shrinkage_factor)) {
      return(list(shrunk_cov = diag(p), shrinkage = 1.0))
    } else {
      return(list(shrunk_cov = diag(p), shrinkage = shrinkage_factor))
    }
  }
  
  X_mean <- colMeans(X)
  X_centered <- sweep(X, 2, X_mean, "-")
  
  # Sample covariance matrix S (unbiased, divided by n - 1)
  S <- tcrossprod(t(X_centered)) / (n - 1)
  
  # Prior target T: diagonal matrix with average sample variance
  mean_var <- sum(diag(S)) / p
  T_mat <- mean_var * diag(p)
  
  if (!is.null(shrinkage_factor)) {
    shrinkage <- shrinkage_factor
  } else {
    # Calcular el factor de encogimiento óptimo (fórmula analítica como en el libro)
    a <- mean((S - T_mat)^2)
    b <- 0.0
    for (i in 1:n) {
      dev <- matrix(X_centered[i, ], ncol = 1)
      sample_cov_i <- dev %*% t(dev)
      b <- b + mean((sample_cov_i - S)^2)
    }
    b <- b / (n * n)
    
    if (a < 1e-15) {
      shrinkage <- 1.0
    } else {
      shrinkage <- max(0.0, min(1.0, b / a))
    }
  }
  
  shrunk_cov <- shrinkage * T_mat + (1.0 - shrinkage) * S
  return(list(shrunk_cov = shrunk_cov, shrinkage = shrinkage))
}

# === Calibraci\u00f3n por IPF en R Shiny ===
ipf_2d_r <- function(seed_matrix, target_rows, target_cols, max_iters = 100, tolerance = 1e-5) {
  M_cal <- as.matrix(seed_matrix)
  r_target <- as.numeric(target_rows)
  c_target <- as.numeric(target_cols)
  
  if (abs(sum(r_target) - sum(c_target)) > 1e-5) {
    c_target <- c_target * (sum(r_target) / sum(c_target))
  }
  
  for (iteration in 1:max_iters) {
    row_sums <- rowSums(M_cal)
    row_sums[row_sums == 0] <- 1.0
    factors_r <- r_target / row_sums
    M_cal <- M_cal * factors_r
    
    col_sums <- colSums(M_cal)
    col_sums[col_sums == 0] <- 1.0
    factors_c <- c_target / col_sums
    M_cal <- sweep(M_cal, 2, factors_c, "*")
    
    current_r <- rowSums(M_cal)
    current_c <- colSums(M_cal)
    diff_r <- max(abs(current_r - r_target))
    diff_c <- max(abs(current_c - c_target))
    
    if (diff_r < tolerance && diff_c < tolerance) {
      break
    }
  }
  return(M_cal)
}

# Helper para obtener altura escalar en una malla 2D
get_z_height_scalar <- function(cx, cy, Z_mat, xg_vec, yg_vec) {
  if (length(cx) == 0 || length(cy) == 0) return(0)
  ix <- findInterval(cx, xg_vec); if (ix < 1) ix <- 1; if (ix >= length(xg_vec)) ix <- length(xg_vec)-1
  iy <- findInterval(cy, yg_vec); if (iy < 1) iy <- 1; if (iy >= length(yg_vec)) iy <- length(yg_vec)-1
  x1 <- xg_vec[ix]; x2 <- xg_vec[ix+1]; y1 <- yg_vec[iy]; y2 <- yg_vec[iy+1]
  wx <- (cx - x1) / (x2 - x1); wy <- (cy - y1) / (y2 - y1)
  w11 <- (1-wx)*(1-wy); w21 <- wx*(1-wy); w12 <- (1-wx)*wy; w22 <- wx*wy
  Z_mat[iy, ix]*w11 + Z_mat[iy, ix+1]*w21 + Z_mat[iy+1, ix]*w12 + Z_mat[iy+1, ix+1]*w22
}

# Precomputaci\u00f3n base de la topograf\u00eda y dimensiones
precompute_eqt_base <- function(manzanas, t_idx = 0) {
  xo <- seq(min(manzanas$x) - 100, max(manzanas$x) + 100, length.out = 30)
  yo <- seq(min(manzanas$y) - 100, max(manzanas$y) + 100, length.out = 30)
  
  # Agregar peque\u00f1o jitter para evitar colinealidad en akima::interp
  set.seed(42) # Semilla fija para reproducibilidad del jitter
  jx <- manzanas$x + runif(length(manzanas$x), -0.1, 0.1)
  jy <- manzanas$y + runif(length(manzanas$y), -0.1, 0.1)
  
  # Interpolar Altitud Física (Topografía) mediante IDW (distancia inversa ponderada) robusto en R puro
  nx <- length(xo)
  ny <- length(yo)
  Z_alt <- matrix(0, ny, nx)
  for (i in 1:ny) {
    for (j in 1:nx) {
      dists <- sqrt((manzanas$x - xo[j])^2 + (manzanas$y - yo[i])^2)
      weights <- 1 / (dists^2 + 1e-5)
      Z_alt[i, j] <- sum(manzanas$altitud * weights) / sum(weights)
    }
  }
  
  Z_alt[is.na(Z_alt)] <- mean(Z_alt, na.rm = TRUE)
  
  dx <- mean(diff(xo))
  dy <- mean(diff(yo))
  
  list(Xg = xo, Yg = yo, Z_alt = Z_alt, dx = dx, dy = dy)
}

# resolvedor geom\u00e9trico del tensor y Christoffels a partir de una superficie activa deformada
compute_manifold_geometry <- function(Xg, Yg, Z_active, Z_alt, dx, dy, lambda_val = 0.8, exp_mode_val = "base", g_urban_ratio_val = 8.0, use_lw = TRUE) {
  nx <- length(Xg)
  ny <- length(Yg)
  
  # Suavizado de la superficie activa
  gaussian_kernel <- function(sigma, radius = 2) {
    x <- -radius:radius
    k <- exp(-x^2/(2*sigma^2)); k/sum(k)
  }
  k_gauss <- gaussian_kernel(0.8)
  convolve_1d <- function(v, k) {
    res <- as.numeric(stats::filter(v, k, sides=2))
    res[is.na(res)] <- v[is.na(res)]
    res
  }
  Zs <- Z_active
  for (j in 1:ncol(Zs)) { Zs[,j] <- convolve_1d(Zs[,j], k_gauss) }
  for (i in 1:nrow(Zs)) { Zs[i,] <- convolve_1d(Zs[i,], k_gauss) }
  
  # Derivada a lo largo de columnas (dimensi\u00f3n x)
  centered_diff_x <- function(M, dx) {
    ny <- nrow(M); nx <- ncol(M); D <- matrix(0, ny, nx)
    if (nx >= 2) { D[,1] <- (M[,2]-M[,1])/dx; D[,nx] <- (M[,nx]-M[,nx-1])/dx }
    if (nx >= 3) D[,2:(nx-1)] <- (M[,3:nx]-M[,1:(nx-2)])/(2*dx)
    D
  }
  # Derivada a lo largo de filas (dimensi\u00f3n y)
  centered_diff_y <- function(M, dy) {
    ny <- nrow(M); nx <- ncol(M); D <- matrix(0, ny, nx)
    if (ny >= 2) { D[1,] <- (M[2,]-M[1,])/dy; D[ny,] <- (M[ny,]-M[ny-1,])/dy }
    if (ny >= 3) D[2:(ny-1),] <- (M[3:ny,]-M[1:(ny-2),])/(2*dy)
    D
  }
  
  # Escalar la altura de la variedad para prop\u00f3sitos de la m\u00e9trica intr\u00ednseca (estabilidad)
  z_min <- min(Zs)
  z_max <- max(Zs)
  z_diff <- z_max - z_min
  if (z_diff < 1.0) z_diff <- 1.0
  Z_geom <- (Zs - z_min) / z_diff * 300
  
  fx <- centered_diff_x(Z_geom, dx); fy <- centered_diff_y(Z_geom, dy)
  
  # Ledoit-Wolf shrinkage: regularizar g_ij con vecindario 3x3
  g11 <- matrix(0, ny, nx); g12 <- matrix(0, ny, nx); g22 <- matrix(0, ny, nx)
  shrinkage_map <- matrix(0, ny, nx)
  
  is_exp7 <- identical(exp_mode_val, "exp7")
  R_ratio <- if (!is.null(g_urban_ratio_val)) g_urban_ratio_val else 8.0
  
  for (ii in 1:ny) {
    for (jj in 1:nx) {
      if (is_exp7) {
        cx_val <- Xg[jj]
        w_urban <- 1 / (1 + exp(- (cx_val - 352800) / 20)) # 20m transition
        g11[ii, jj] <- 1.0 * (1 - w_urban) + R_ratio * w_urban
        g22[ii, jj] <- 1.0 * (1 - w_urban) + R_ratio * w_urban
        g12[ii, jj] <- 0.0
        shrinkage_map[ii, jj] <- 0.0
      } else {
        i_rng <- max(1, ii-1):min(ny, ii+1)
        j_rng <- max(1, jj-1):min(nx, jj+1)
        fx_nb <- as.vector(fx[i_rng, j_rng])
        fy_nb <- as.vector(fy[i_rng, j_rng])
        if (length(fx_nb) >= 2 && isTRUE(use_lw)) {
          X_nb <- cbind(fx_nb, fy_nb)
          lw_res <- ledoit_wolf_shrinkage(X_nb)
          S_lw <- lw_res$shrunk_cov
          g11[ii, jj] <- 1 + lambda_val * S_lw[1, 1]
          g12[ii, jj] <- lambda_val * S_lw[1, 2]
          g22[ii, jj] <- 1 + lambda_val * S_lw[2, 2]
          shrinkage_map[ii, jj] <- lw_res$shrinkage
        } else {
          g11[ii, jj] <- 1 + lambda_val * fx[ii, jj]^2
          g12[ii, jj] <- lambda_val * fx[ii, jj] * fy[ii, jj]
          g22[ii, jj] <- 1 + lambda_val * fy[ii, jj]^2
          shrinkage_map[ii, jj] <- 0.0
        }
      }
    }
  }
  
  dg11_dx <- centered_diff_x(g11, dx); dg11_dy <- centered_diff_y(g11, dy)
  dg12_dx <- centered_diff_x(g12, dx); dg12_dy <- centered_diff_y(g12, dy)
  dg22_dx <- centered_diff_x(g22, dx); dg22_dy <- centered_diff_y(g22, dy)
  
  detg <- g11*g22 - g12*g12
  detg_stab <- ifelse(detg > 1e-8, detg, 1e-8)
  ginv11 <-  g22 / detg_stab; ginv12 <- -g12 / detg_stab; ginv22 <-  g11 / detg_stab
  
  get_dg <- function(j, k, l) {
    if (j == 1 && k == 1) return(if (l == 1) dg11_dx else dg11_dy)
    if ((j == 1 && k == 2) || (j == 2 && k == 1)) return(if (l == 1) dg12_dx else dg12_dy)
    if (j == 2 && k == 2) return(if (l == 1) dg22_dx else dg22_dy)
    stop("get_dg")
  }
  gamma_comp <- function(i, j, k) {
    T1 <- get_dg(j, k, 1) + get_dg(k, j, 1) - get_dg(1, j, k)
    T2 <- get_dg(j, k, 2) + get_dg(k, j, 2) - get_dg(2, j, k)
    if (i == 1) 0.5 * (ginv11 * T1 + ginv12 * T2) else 0.5 * (ginv12 * T1 + ginv22 * T2)
  }
  Gamma1_11 <- gamma_comp(1,1,1); Gamma1_12 <- gamma_comp(1,1,2); Gamma1_22 <- gamma_comp(1,2,2)
  Gamma2_11 <- gamma_comp(2,1,1); Gamma2_12 <- gamma_comp(2,1,2); Gamma2_22 <- gamma_comp(2,2,2)
  
  # Gradientes de Altitud f\u00edsica para el perfil de Adulto Mayor
  dAlt_dx <- centered_diff_x(Z_alt, dx)
  dAlt_dy <- centered_diff_y(Z_alt, dy)
  
  # Empacar en 11 canales transponiendo a formato [nx, ny]
  pack11 <- array(0, dim = c(nx, ny, 11))
  pack11[,,1] <- t(Gamma1_11); pack11[,,2] <- t(Gamma1_12); pack11[,,3] <- t(Gamma1_22)
  pack11[,,4] <- t(Gamma2_11); pack11[,,5] <- t(Gamma2_12); pack11[,,6] <- t(Gamma2_22)
  pack11[,,7] <- t(g11);       pack11[,,8] <- t(g22);       pack11[,,9] <- t(g12)
  pack11[,,10] <- t(dAlt_dx);  pack11[,,11] <- t(dAlt_dy)
  
  list(Xg = Xg, Yg = Yg, Z = Zs, Z_geom = Z_geom, GammaPacked = pack11, dx = dx, dy = dy, fx = fx, fy = fy, shrinkage_map = shrinkage_map)
}

# ==================== RESOLVEDOR GEOD\u00c9SICO CON ACOPLAMIENTO ODE ====================
# NOTA DE DESARROLLO Y OPTIMIZACIÓN MULTI-AGENTE:
# La integración numérica RK4 sobre el manifold territorial Riemanniano consume tiempo en R interactivo.
# Para escalabilidad de simulación metropolitana (ej. miles de agentes en paralelo):
# 1. Migrar esta función de integración Runge-Kutta 4 a C++ usando Rcpp y paralelizar los agentes
#    con hilos de ejecución concurrentes usando OpenMP (#pragma omp parallel for).
# 2. Utilizar métodos de disparo BVP vectorizados o aproximaciones por Hamilton-Jacobi-Bellman (HJB)
#    mediante métodos de barrido rápido (Fast Marching Methods) para resolver múltiples destinos de forma simultánea.
# Custom Runge-Kutta 4th order solver to avoid deSolve package dependency in WebAssembly
rk4_solver <- function(y, times, func, parms = NULL) {
  n_steps <- length(times)
  n_vars <- length(y)
  
  out <- matrix(0, nrow = n_steps, ncol = n_vars + 1)
  colnames(out) <- c("time", names(y))
  out[1, 1] <- times[1]
  out[1, 2:(n_vars + 1)] <- y
  
  curr_y <- y
  for (i in 2:n_steps) {
    t_prev <- times[i - 1]
    t_curr <- times[i]
    dt <- t_curr - t_prev
    
    res1 <- func(t_prev, curr_y, parms)
    k1 <- res1[[1]]
    if (all(k1 == 0)) {
      out[i:n_steps, 1] <- times[i:n_steps]
      for (j in i:n_steps) {
        out[j, 2:(n_vars + 1)] <- curr_y
      }
      break
    }
    
    y2 <- curr_y + 0.5 * dt * k1
    res2 <- func(t_prev + 0.5 * dt, y2, parms)
    k2 <- res2[[1]]
    
    y3 <- curr_y + 0.5 * dt * k2
    res3 <- func(t_prev + 0.5 * dt, y3, parms)
    k3 <- res3[[1]]
    
    y4 <- curr_y + dt * k3
    res4 <- func(t_curr, y4, parms)
    k4 <- res4[[1]]
    
    curr_y <- curr_y + (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    
    out[i, 1] <- t_curr
    out[i, 2:(n_vars + 1)] <- curr_y
  }
  
  as.data.frame(out)
}

solve_geodesic <- function(pre, x0, y0, theta, v0, dest_x = NULL, dest_y = NULL, 
                           use_V = TRUE, A_wall = 12000, A_dest = 15000, 
                           distorsiones = NULL, profile = "cuidado", A_slope = 15000,
                           omega_damping = 0.0, Tmax = 12, dt = 0.1,
                           exp_mode = "base", k_conservation = 2.0, amp_santuario = 20000,
                           sigma_capital = 1.0, capital_flow_direction = "rural_to_urban") {
  if (is.null(x0) || length(x0) == 0 || any(is.na(x0)) ||
      is.null(y0) || length(y0) == 0 || any(is.na(y0)) ||
      is.null(v0) || length(v0) == 0 || any(is.na(v0)) ||
      is.null(theta) || length(theta) == 0 || any(is.na(theta)) ||
      is.null(pre) || length(pre) == 0) {
    x_val <- if (length(x0) > 0) x0[1] else 352800
    y_val <- if (length(y0) > 0) y0[1] else 6294000
    return(data.frame(time = 0, x = x_val, y = y_val, vx = 0, vy = 0))
  }
  Xg <- pre$Xg; Yg <- pre$Yg; GammaPacked <- pre$GammaPacked
  locate_point <- function(x0, y0) {
    nx <- length(Xg); ny <- length(Yg)
    if (x0 < Xg[1] || x0 > Xg[nx] || y0 < Yg[1] || y0 > Yg[ny]) return(NULL)
    i <- findInterval(x0, Xg, rightmost.closed = TRUE); if (i >= nx) i <- nx-1
    j <- findInterval(y0, Yg, rightmost.closed = TRUE); if (j >= ny) j <- ny-1
    x1 <- Xg[i]; x2 <- Xg[i+1]; y1 <- Yg[j]; y2 <- Yg[j+1]
    wx <- (x0 - x1) / (x2 - x1); wy <- (y0 - y1) / (y2 - y1)
    list(i=i, j=j, wx=wx, wy=wy)
  }
  gamma_at <- function(loc) {
    if (is.null(loc)) return(rep(0, 11))
    i <- loc$i; j <- loc$j; wx <- loc$wx; wy <- loc$wy
    w11 <- (1-wx)*(1-wy); w21 <- wx*(1-wy); w12 <- (1-wx)*wy; w22 <- wx*wy
    v11 <- GammaPacked[i,  j,  ]
    v21 <- GammaPacked[i+1,j,  ]
    v12 <- GammaPacked[i,  j+1,]
    v22 <- GammaPacked[i+1,j+1,]
    as.numeric(w11*v11 + w21*v21 + w12*v12 + w22*v22)
  }
  
  get_potential_grad <- function(x, y, dAlt_dx_loc = 0, dAlt_dy_loc = 0, vx = 0, vy = 0) {
    if (!use_V) return(c(0, 0))
    
    # 1. Delimitaci\u00f3n F\u00edsica (Barrera Canal San Carlos / Av. Tobalaba)
    x_wall <- 352500
    w_width <- 70
    
    # Factor de cruces habilitados (puentes): Grecia (6294400) y Arrieta (6295300)
    dist_to_bridge1 <- abs(y - 6294400)
    dist_to_bridge2 <- abs(y - 6295300)
    bridge_mult <- min(1.0, min(dist_to_bridge1, dist_to_bridge2) / 200) # Cero en el puente, sube a 1 en 200m
    
    wall_mult <- if (profile == "mayor") 2.0 else if (profile == "joven") 0.4 else 1.0
    V_w <- (A_wall * wall_mult * bridge_mult) * exp(- (x - x_wall)^2 / (2 * w_width^2))
    dV_w_dx <- - ((x - x_wall) / (w_width^2)) * V_w
    
    # Fuerza lateral que empuja f\u00edsicamente al peat\u00f3n hacia el puente m\u00e1s cercano si est\u00e1 junto a la barrera
    closest_bridge_y <- if (dist_to_bridge1 < dist_to_bridge2) 6294400 else 6295300
    dV_w_dy <- if (min(dist_to_bridge1, dist_to_bridge2) < 200) {
      (A_wall * wall_mult) * exp(- (x - x_wall)^2 / (2 * w_width^2)) * sign(y - closest_bridge_y) / 200
    } else {
      0
    }
    
    # 2. Atracci\u00f3n de Destino B (Sentido)
    dV_d_dx <- 0; dV_d_dy <- 0
    if (!is.null(dest_x) && !is.null(dest_y)) {
      r_dest <- 400
      dest_mult <- if (profile == "mayor") 0.8 else if (profile == "nocturna") 1.2 else 1.0
      dist_sq <- (x - dest_x)^2 + (y - dest_y)^2
      V_d <- - (A_dest * dest_mult) * exp(- dist_sq / (2 * r_dest^2))
      dV_d_dx <- - ((x - dest_x) / (r_dest^2)) * V_d
      dV_d_dy <- - ((y - dest_y) / (r_dest^2)) * V_d
    }
    
    # 3. Fricci\u00f3n por Pendiente de Altitud (Solo Adulto Mayor)
    dV_slope_dx <- 0
    dV_slope_dy <- 0
    if (profile == "mayor") {
      v_mag <- sqrt(vx^2 + vy^2)
      grad_mag <- sqrt(dAlt_dx_loc^2 + dAlt_dy_loc^2)
      cardio_mult <- 1.0
      if (v_mag > 1e-5 && grad_mag > 1e-5) {
        cos_angle <- (vx * dAlt_dx_loc + vy * dAlt_dy_loc) / (v_mag * grad_mag)
        # Penaliza trayectorias paralelas al gradiente (cos_angle^2 close to 1)
        cardio_mult <- 1.0 + 2.5 * (cos_angle^2)
      }
      dV_slope_dx <- A_slope * dAlt_dx_loc * cardio_mult
      dV_slope_dy <- A_slope * dAlt_dy_loc * cardio_mult
    }
    
    # 4. Distorsiones Territoriales Activas
    dV_dist_dx <- 0; dV_dist_dy <- 0
    if (!is.null(distorsiones) && nrow(distorsiones) > 0) {
      for (k in 1:nrow(distorsiones)) {
        d_x <- distorsiones$x[k]
        d_y <- distorsiones$y[k]
        d_type <- distorsiones$type[k]
        d_mag <- distorsiones$mag[k]
        
        is_repulsive <- d_type %in% c("obstacle", "delito", "accidente")
        sign_val <- if (is_repulsive) 1.0 else -1.0
        
        sens_mult <- 1.0
        if (profile == "cuidado") {
          if (d_type == "delito") sens_mult <- 1.8
          if (d_type == "compras") sens_mult <- 1.6
        } else if (profile == "nocturna") {
          if (d_type == "delito") sens_mult <- 2.8
          if (d_type == "seguridad") sens_mult <- 2.2
          if (d_type == "compras") sens_mult <- 0.4
        } else if (profile == "joven") {
          if (d_type == "delito") sens_mult <- 0.4
          if (d_type == "obstacle") sens_mult <- 0.5
        } else if (profile == "mayor") {
          if (is_repulsive) sens_mult <- 1.3
        }
        
        mag_params <- switch(d_mag,
                             "leve"     = list(r = 100, amp = 4000),
                             "moderada" = list(r = 200, amp = 12000),
                             "severa"   = list(r = 350, amp = 30000),
                             "critica"  = list(r = 500, amp = 70000),
                             list(r = 200, amp = 12000))
        r_dist <- mag_params$r
        dist_amp_val <- mag_params$amp * sens_mult
        
        dist_sq_pt <- (x - d_x)^2 + (y - d_y)^2
        V_dist <- sign_val * dist_amp_val * exp(- dist_sq_pt / (2 * r_dist^2))
        dV_dist_dx <- dV_dist_dx - ((x - d_x) / (r_dist^2)) * V_dist
        dV_dist_dy <- dV_dist_dy - ((y - d_y) / (r_dist^2)) * V_dist
      }
    }
    
    grad_x <- dV_w_dx + dV_d_dx + dV_slope_dx + dV_dist_dx
    grad_y <- dV_w_dy + dV_d_dy + dV_slope_dy + dV_dist_dy
    
    # Clipping de fuerzas para evitar divergencias en la ODE
    grad_x <- max(-1500, min(1500, grad_x))
    grad_y <- max(-1500, min(1500, grad_y))
    
    c(grad_x, grad_y)
  }
  
  rhs <- function(t, state, parms) {
    x <- state[1]; y <- state[2]; vx <- state[3]; vy <- state[4]
    
    if (!is.null(dest_x) && length(dest_x) > 0 && !any(is.na(dest_x)) &&
        !is.null(dest_y) && length(dest_y) > 0 && !any(is.na(dest_y))) {
      dist_to_dest <- sqrt((x - dest_x)^2 + (y - dest_y)^2)
      if (length(dist_to_dest) > 0 && !any(is.na(dist_to_dest)) && any(dist_to_dest < 40.0)) {
        return(list(c(0, 0, 0, 0))) # Detener integraci\u00f3n al llegar
      }
    }
    
    loc <- locate_point(x, y)
    if (is.null(loc)) return(list(c(0, 0, 0, 0)))
    G <- gamma_at(loc)
    
    g11_val <- G[7]; g22_val <- G[8]; g12_val <- G[9]
    det_g <- g11_val*g22_val - g12_val*g12_val
    det_g_stab <- if (det_g > 1e-8) det_g else 1e-8
    ginv11 <-  g22_val / det_g_stab; ginv12 <- -g12_val / det_g_stab; ginv22 <-  g11_val / det_g_stab
    
    dAlt_dx_loc <- G[10]
    dAlt_dy_loc <- G[11]
    
    dV <- get_potential_grad(x, y, dAlt_dx_loc, dAlt_dy_loc, vx, vy)
    
    if (identical(exp_mode, "exp6")) {
      v_norm_sq <- G[7]*vx^2 + 2*G[9]*vx*vy + G[8]*vy^2
      v_norm <- sqrt(max(1e-9, v_norm_sq))
      n_norm <- sqrt(max(1e-9, G[7]))
      cos_phi <- if (v_norm > 1e-5) (G[7]*vx + G[9]*vy) / (n_norm * v_norm) else 0.0
      
      w_santuario <- 1 / (1 + exp(- (x - 354000) / 20))
      dw_dx <- (1/20) * w_santuario * (1 - w_santuario)
      asym_factor <- 1 - tanh(k_conservation * cos_phi)
      dV[1] <- dV[1] + amp_santuario * dw_dx * asym_factor
    }
    
    grad_mag <- sqrt(dV[1]^2 + dV[2]^2)
    if (grad_mag > 22000) {
      return(list(c(0, 0, 0, 0))) # Ruptura Topologica: la trayectoria se quiebra
    }
    
    if (identical(exp_mode, "exp7")) {
      sign_dir <- if (identical(capital_flow_direction, "rural_to_urban")) -1.0 else 1.0
      F_x <- -sigma_capital * sign_dir * 50.0
      F_y <- 0.0
    } else {
      F_x <- - (ginv11 * dV[1] + ginv12 * dV[2])
      F_y <- - (ginv12 * dV[1] + ginv22 * dV[2])
    }
    
    # Ecuaci\u00f3n Geod\u00e9sica con el factor de amortiguamiento de Lyapunov
    ddx <- -(G[1]*vx*vx + 2*G[2]*vx*vy + G[3]*vy*vy) - omega_damping * vx + F_x
    ddy <- -(G[4]*vx*vx + 2*G[5]*vx*vy + G[6]*vy*vy) - omega_damping * vy + F_y
    
    list(c(vx, vy, ddx, ddy))
  }
  
  vx0 <- v0 * cos(theta); vy0 <- v0 * sin(theta)
  state0 <- c(x = x0, y = y0, vx = vx0, vy = vy0)
  times <- seq(0, Tmax, by = dt)
  out <- rk4_solver(y = state0, times = times, func = rhs, parms = NULL)
  
  out
}


# ---- SOURCING DE M\u00d3DULOS DE UI Y SERVIDOR ----
source("modules/translations.R", encoding = "UTF-8")
source("modules/tab0_home.R", encoding = "UTF-8")
source("modules/tab1_onboarding.R", encoding = "UTF-8")
source("modules/tab2_experimentos.R", encoding = "UTF-8")
source("modules/tab3_simulacion.R", encoding = "UTF-8")
source("modules/tab3_ui_definition.R", encoding = "UTF-8")
source("modules/tab4_matematica.R", encoding = "UTF-8")
source("modules/tab5_biblioteca.R", encoding = "UTF-8")
source("modules/ergo_drawer.R", encoding = "UTF-8")
source("modules/sim_server.R", encoding = "UTF-8")
