# modules/tab3_ui_definition.R
# Definición Funcional Pura de la Interfaz para la Pestaña de Simulación (Separada bilingüemente y con secuencias de escape Unicode)

get_tab3_ui <- function(input, output, session, lang) {
  is_en <- identical(lang(), "EN")
  
  trans <- function(es_txt, en_txt) {
    if (is_en) en_txt else es_txt
  }
  
  sidebarLayout(
    sidebarPanel(
      class = "panel-glass sidebar-glass",
      width = 3,
      h3(style = "color:#0284c7; font-weight:600; margin-top:0;", 
         trans("Control del Territorio", "Territory Control")),
      tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.08);"),
      
      # SECTION 1: SCENARIO CONFIGURATION
      tags$details(open = "open", style = "margin-bottom: 10px; border-bottom: 1px solid rgba(255,255,255,0.08); padding-bottom: 5px;",
        tags$summary(style = "font-weight: bold; cursor: pointer; color: #b45309; margin-bottom: 8px; outline: none; user-select: none; font-size: 1.1rem;",
          title = trans("Selecciona casos preconfigurados de la obra, perfiles peatonales (Wu Wei) y dimensiones visuales para el mapa.", "Select preconfigured cases of the book, pedestrian profiles (Wu Wei), and visual dimensions for the map."),
          trans("1. Configuraci\u00f3n de Escenario", "1. Scenario Configuration")
        ),
        
        selectInput("narrative_case", trans("Caso de la Obra (Auto-Configura):", "Case of the Work (Auto-Configures):"),
                    choices = list(
                      "Personalizado (Libre)" = "custom",
                      "Caso A: Desplazamiento Cuidado Diurno" = "caso_a",
                      "Caso B: Retorno en Penumbra Nocturna" = "caso_b",
                      "Caso C: Adulto Mayor en la Ladera" = "caso_c",
                      "Caso D: Escudo de Cohesi\u00f3n de Lie" = "caso_d",
                      "Caso E: Gentrificaci\u00f3n Agresiva" = "caso_e",
                      "Caso F: Nodo de Transporte Masivo" = "caso_f",
                      "Caso G: Desastre Natural (Obst\u00e1culos)" = "caso_g",
                      "Caso H: Subsidio Inmobiliario Masivo" = "caso_h"
                    ), selected = "caso_a"),
        
        selectInput("ped_profile", trans("Perfil Peatonal (Wu Wei):", "Pedestrian Profile (Wu Wei):"),
                    choices = list(
                      "Mam\u00e1 de Cuidado (Compras)" = "cuidado",
                      "Adulto Mayor (Movilidad Reducida)" = "mayor",
                      "Trabajadora Nocturna (Seguridad)" = "nocturna",
                      "Estudiante / Joven (Directo)" = "joven"
                    ), selected = "cuidado"),
        
        selectInput("dim_3d", trans("Dimensi\u00f3n Superficie 3D:", "3D Surface Dimension:"),
                    choices = list(
                      "Fricci\u00f3n EQT (Resistencia)" = "friccion",
                      "Bienestar y Calidad de Vida" = "bienestar",
                      "Altitud F\u00edsica del Terreno" = "altitud",
                      "Asimetr\u00eda de Poder [X,Y] (Lie)" = "lie",
                      "Tensi\u00f3n Intr\u00ednseca (NTI)" = "nti",
                      "Curvatura de Ricci" = "ricci"
                    ), selected = "friccion"),
        
        conditionalPanel("input.dim_3d == 'lie'",
          selectInput("lie_seq", trans("Secuencia de Acciones (Lie):", "Sequence of Actions (Lie):"),
                      choices = list("Parque (Y) -> Inmobiliaria (X)" = "Y_X", 
                                     "Inmobiliaria (X) -> Parque (Y)" = "X_Y", 
                                     "Asimetr\u00eda Neta (Conmutador)" = "diff"),
                      selected = "X_Y")
        ),
        
        selectInput("exp_mode", trans("Modo Experimento (Tomo II):", "Experiment Mode (Volume II):"),
                    choices = list(
                      "Ninguno (Simulaci\u00f3n Base)" = "base",
                      "Experimento 6: Santuario Natural" = "exp6",
                      "Experimento 7: Refracci\u00f3n de Capital" = "exp7"
                    ), selected = "base"),
        
        conditionalPanel("input.exp_mode == 'exp6'",
          sliderInput("k_conservation", trans("Resistencia de Conservaci\u00f3n (\u03ba):", "Conservation Stiffness (\u03ba):"), min = 0.5, max = 5.0, value = 2.0, step = 0.1),
          sliderInput("amp_santuario", trans("Amplitud del Santuario (V_santuario):", "Sanctuary Amplitude (V_sanctuary):"), min = 5000, max = 40000, value = 20000, step = 1000)
        ),
        
        conditionalPanel("input.exp_mode == 'exp7'",
          sliderInput("g_urban_ratio", trans("Raz\u00f3n de M\u00e9trica (g_urbano/g_rural):", "Metric Ratio (g_urban/g_rural):"), min = 1.0, max = 15.0, value = 8.0, step = 0.5),
          sliderInput("sigma_capital", trans("Conductividad de Inversi\u00f3n (\u03c3):", "Investment Conductivity (\u03c3):"), min = 0.1, max = 3.0, value = 1.0, step = 0.1),
          selectInput("capital_flow_direction", trans("Direcci\u00f3n de Inversi\u00f3n:", "Investment Direction:"), 
                      choices = list("De Rural a Urbano" = "rural_to_urban", "De Urbano a Rural" = "urban_to_rural"), 
                      selected = "rural_to_urban")
        ),
        
        fluidRow(
          column(6, selectInput("scen_time", trans("Horario:", "Time:"), choices = list("D\u00eda" = "Dia", "Noche" = "Noche"), selected = "Dia")),
          column(6, selectInput("scen_season", trans("Estaci\u00f3n:", "Season:"), choices = list("Verano" = "Verano", "Invierno" = "Invierno"), selected = "Verano"))
        )
      ),
      
      # SECTION 2: EVENTS & DISTORTIONS
      tags$details(open = "open", style = "margin-bottom: 10px; border-bottom: 1px solid rgba(255,255,255,0.08); padding-bottom: 5px;",
        tags$summary(style = "font-weight: bold; cursor: pointer; color: #be123c; margin-bottom: 8px; outline: none; user-select: none; font-size: 1.1rem;",
          title = trans("Establece los puntos de Origen y Destino de las trayectorias peatonales, o agrega distorsiones y obst\u00e1culos en el mapa.", "Set the Origin and Destination points of pedestrian paths, or add local distortions and obstacles on the map."),
          trans("2. Eventos y Distorsiones", "2. Events & Distortions")
        ),
        
        radioButtons("click_mode", trans("Acci\u00f3n de Clic en Mapa:", "Map Click Action:"),
                     choices = list("Ver Ficha" = "fiche", "Origen A" = "origen", "Destino B" = "destino", "Distorsi\u00f3n D" = "distorsion"),
                     selected = "origen", inline = TRUE),
        
        selectInput("origen_id", trans("Manzana Origen (A):", "Origin Block (A):"), choices = NULL, selected = NULL),
        selectInput("destino_id", trans("Manzana Destino (B):", "Destination Block (B):"), choices = NULL, selected = NULL),
        
        conditionalPanel("input.click_mode == 'distorsion'",
          div(style = "background: rgba(236, 72, 153, 0.08); border: 1px solid rgba(236, 72, 153, 0.2); border-radius: 10px; padding: 12px; margin-bottom: 10px;",
            h4(trans("Configurar Evento D:", "Configure Event D:"), style = "color: #be123c !important; font-weight: 600; margin-top: 0;"),
            selectInput("dist_type", trans("Tipo de Impacto:", "Impact Type:"), 
                        choices = list("Delito (Repele)" = "delito",
                                       "Accidente (Repele)" = "accidente",
                                       "Compras / Feria (Atrae)" = "compras",
                                       "Corredor Seguro (Atrae)" = "seguridad",
                                       "Obst\u00e1culo (Repele)" = "obstacle"), 
                        selected = "delito"),
            selectInput("dist_mag", trans("Magnitud del Evento:", "Event Magnitude:"), 
                        choices = list("Leve" = "leve", 
                                       "Moderada" = "moderada", 
                                       "Severa" = "severa", 
                                       "Cr\u00edtica" = "critica"), 
                        selected = "moderada"),
            actionButton("clear_distorsiones", trans("Limpiar Distorsiones", "Clear All Events"), class = "btn-danger btn-xs w-100")
          )
        ),
        
        # Origin/Destination HUD Labels
        div(style = "font-size: 0.85rem; color: #475569; margin-top: 5px;",
          uiOutput("orig_dest_hud_ui")
        )
      ),
      
      # SECTION 3: GLOBAL FORCE FIELDS
      tags$details(style = "margin-bottom: 10px; border-bottom: 1px solid rgba(255,255,255,0.08); padding-bottom: 5px;",
        tags$summary(style = "font-weight: bold; cursor: pointer; color: #0f766e; margin-bottom: 8px; outline: none; user-select: none; font-size: 1.1rem;",
          title = trans("Activa y configura fuerzas de atracci\u00f3n de destinos, canalizaci\u00f3n por barreras y fricci\u00f3n por pendientes en el relieve 3D.", "Activate and configure attraction forces to destinations, channeling by barriers, and slope friction in the 3D relief."),
          trans("3. Campos de Fuerza Globales", "3. Global Force Fields")
        ),
        
        checkboxInput("use_potentials", trans("Activar Campos de Fuerza (V)", "Activate Force Fields (V)"), value = TRUE),
        conditionalPanel("input.use_potentials == true",
          sliderInput("pot_wall_amp", trans("Canalizaci\u00f3n de Flujo (V_canal):", "Flow Channeling (V_canal):"), min = 0, max = 50000, value = 15000, step = 1000),
          sliderInput("pot_dest_amp", trans("Atracci\u00f3n de Destino (V_atrae):", "Destination Attraction (V_attract):"), min = 0, max = 50000, value = 10000, step = 1000),
          sliderInput("pot_slope_amp", trans("Fricci\u00f3n por Pendiente (V_loma):", "Slope Friction (V_slope):"), min = 0, max = 20000, value = 5000, step = 500),
          checkboxInput("show_vectors", trans("Mostrar Vectores de Fuerza 3D", "Show 3D Force Vectors"), value = FALSE)
        )
      ),
      
      # SECTION 4: GEOMETRY & SOLVER
      tags$details(style = "margin-bottom: 10px; border-bottom: 1px solid rgba(255,255,255,0.08); padding-bottom: 5px;",
        tags$summary(style = "font-weight: bold; cursor: pointer; color: #0284c7; margin-bottom: 8px; outline: none; user-select: none; font-size: 1.1rem;",
          title = trans("Ajusta par\u00e1metros de f\u00edsica territorial: memoria de Caputo (trauma), damping de Lyapunov, rigidez m\u00e9trica y solucionador BVP.", "Adjust territorial physics parameters: Caputo memory (trauma), Lyapunov damping, metric stiffness, and the BVP solver."),
          trans("4. Geometr\u00eda y Solucionador", "4. Geometry & Solver")
        ),
        
        sliderInput("alpha_caputo", trans("Orden Caputo (\u03b1) - Memoria Trauma:", "Caputo Order (\u03b1) - Trauma Memory:"), min = 0.1, max = 1.0, value = 0.8, step = 0.05),
        sliderInput("v0_vital", trans("Energ\u00eda Vital / Carga (||v0||):", "Vital Energy / Charge (||v0||):"), min = 5, max = 35, value = 18, step = 1),
        sliderInput("lyap_vol", trans("Amortiguaci\u00f3n Lyapunov (\u03a9):", "Lyapunov Damping (\u03a9):"), min = 0.0, max = 2.0, value = 0.4, step = 0.05),
        sliderInput("lambda_val", trans("Rigidez de Manifold (\u03bb):", "Manifold Stiffness (\u03bb):"), min = 0.1, max = 2.0, value = 0.8, step = 0.05),
        checkboxInput("ruido_tactico", trans("Activar Ruido T\u00e1ctico", "Activate Tactical Noise"), value = FALSE),
        checkboxInput("saving_mode", trans("Modo Ahorro Energ\u00eda (Congelar 3D)", "Energy Saving Mode (Freeze 3D)"), value = FALSE),
        checkboxInput("solve_bvp", trans("Activar Solucionador BVP", "Activate BVP Solver"), value = TRUE),
        
        tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.05); margin: 8px 0;"),
        checkboxInput("use_collective_memory", trans("Activar Memoria Colectiva (Hotspots)", "Activate Collective Memory (Hotspots)"), value = TRUE),
        uiOutput("collective_memory_status_ui"),
        actionButton("clear_collective_memory", trans("Olvidar Memoria Colectiva", "Forget Collective Memory"), class = "btn-warning btn-xs w-100", style = "margin-top: 5px; font-weight: 500;")
      ),
      
      # SECTION 5: INTRINSIC VISION FILTERS
      tags$details(open = "open", style = "margin-bottom: 10px; border-bottom: 1px solid rgba(255,255,255,0.08); padding-bottom: 5px;",
        tags$summary(style = "font-weight: bold; cursor: pointer; color: #0f766e; margin-bottom: 8px; outline: none; user-select: none; font-size: 1.1rem;",
          title = trans("Activa mapas de calor de NTI o Ricci, visualiza campos vectoriales de Sentido y enciende las alertas de disputas de Lie.", "Activate NTI or Ricci heatmaps, visualize Sentido vector fields, and toggle Lie dispute alerts."),
          trans("5. Filtros de Visi\u00f3n Intr\u00ednseca", "5. Intrinsic Vision Filters")
        ),
        selectInput("visual_heatmap", trans("Mapa de Calor Intr\u00ednseco:", "Intrinsic Heatmap:"), 
                    choices = list("Ninguno" = "none", 
                                   "Tensi\u00f3n Intr\u00ednseca (NTI)" = "nti", 
                                   "Curvatura de Ricci" = "ricci", 
                                   "Trauma Acumulado (Caputo)" = "trauma"), 
                    selected = "none"),
        selectInput("visual_vectorfield", trans("Flujos y Corrientes Vectoriales:", "Vector Flows & Currents:"), 
                    choices = list("Ninguno" = "none", 
                                   "Corrientes de Sentido (S^i)" = "sentido", 
                                   "Gradiente de Tensi\u00f3n (NTI)" = "grad_nti"), 
                    selected = "none"),
        checkboxInput("show_lie_conflicts", trans("Mostrar Zonas de Conflicto (Lie)", "Show Conflict Zones (Lie)"), value = FALSE)
      ),
      
      # SECTION 6: INGEST EXTERNAL DATA
      tags$details(style = "margin-bottom: 15px;",
        tags$summary(style = "font-weight: bold; cursor: pointer; color: #6d28d9; margin-bottom: 8px; outline: none; user-select: none; font-size: 1.1rem;",
          title = trans("Sube tus propios archivos GeoJSON de mallas espaciales o atributos CSV de terreno para calibrar y exportar la simulaci\u00f3n.", "Upload your own spatial GeoJSON meshes or CSV field attributes to calibrate and export the simulation."),
          trans("6. Ingesta de Datos (Zenodo)", "6. Ingest External Data (Zenodo)")
        ),
        
        fileInput("custom_csv", trans("Cargar Atributos (CSV):", "Upload Attributes (CSV):"), accept = c(".csv")),
        tags$p(style = "font-size:0.75rem; color:#f43f5e; font-weight:600; margin-top:-5px; margin-bottom:10px;",
               trans("ADVERTENCIA DE SEGURIDAD: Remueva metadatos e identidades de las trazas GPS antes de subir archivos para proteger la seguridad comunitaria en terreno.",
                     "SECURITY WARNING: Remove metadata and personal identities from GPS tracks before uploading to protect local community safety in the field.")),
        fileInput("custom_geojson", trans("Cargar Malla (GeoJSON):", "Upload Mesh (GeoJSON):"), accept = c(".geojson")),
        actionButton("ingest_data", trans("Ingestar e Iniciar Resolvedor", "Ingest & Start Solver"), class = "btn-primary w-100", style = "font-weight:600;"),
        downloadButton("download_geojson", trans("Exportar Malla Activa (GeoJSON)", "Export Active Mesh (GeoJSON)"), class = "btn-info w-100", style = "margin-top: 10px; font-weight:600;"),

  # JavaScript for Fullscreen Map toggling
  tags$script(HTML(r"(
    $(document).on('click', '#toggle_2d_fullscreen', function() {
      var parent = $(this).closest('.panel-glass');
      parent.toggleClass('fullscreen-map');
      var isFull = parent.hasClass('fullscreen-map');
      
      var mapEl = $('#leaflet_map');
      if (isFull) {
        mapEl.css('height', 'calc(100vh - 100px)');
      } else {
        mapEl.css('height', '430px');
      }
      
      var text_es = isFull ? ' Salir Pantalla Completa' : ' Pantalla Completa';
      var text_en = isFull ? ' Exit Fullscreen' : ' Fullscreen';
      var isEn = $('#chat_query').attr('placeholder') && $('#chat_query').attr('placeholder').indexOf('Ask') !== -1;
      var text = isEn ? text_en : text_es;
      var icon = isFull ? '<i class="fa fa-compress"></i>' : '<i class="fa fa-expand"></i>';
      $(this).html(icon + text);
      
      var map = HTMLWidgets.find('#leaflet_map').getMap();
      setTimeout(function() { map.invalidateSize(); }, 300);
    });

    $(document).on('click', '#toggle_3d_fullscreen', function() {
      var parent = $(this).closest('.panel-glass');
      parent.toggleClass('fullscreen-map');
      var isFull = parent.hasClass('fullscreen-map');
      
      var plotEl = $('#plotly_mesh');
      if (isFull) {
        plotEl.css('height', 'calc(100vh - 100px)');
      } else {
        plotEl.css('height', '430px');
      }
      
      var text_es = isFull ? ' Salir Pantalla Completa' : ' Pantalla Completa';
      var text_en = isFull ? ' Exit Fullscreen' : ' Fullscreen';
      var isEn = $('#chat_query').attr('placeholder') && $('#chat_query').attr('placeholder').indexOf('Ask') !== -1;
      var text = isEn ? text_en : text_es;
      var icon = isFull ? '<i class="fa fa-compress"></i>' : '<i class="fa fa-expand"></i>';
      $(this).html(icon + text);
      
      setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 300);
    });
  )"))

      ),
      
      # ACTION BUTTON TRIGGER
      tags$hr(style = "border-top: 1px solid rgba(255,255,255,0.08);"),
      actionButton("btn_geodesica", trans("Disparar Geod\u00e9sica Wu Wei", "Trigger Wu Wei Geodesic"), class = "btn-success w-100", style = "font-weight:700; padding:12px; font-size:1.0rem;")
    ),
    
    # Main Dashboard Visualizers
    mainPanel(
      width = 9,
      
      # Semantic layer descriptions
      div(class = "panel-glass", style = "padding:15px; margin-bottom:20px; border-left:4px solid #0f766e;",
        tags$span(style = "font-weight: 700; color: #0f766e; font-size:0.95rem; display:block; margin-bottom:4px;", 
                  trans("Capa Sem\u00e1ntica Activa:", "Active Semantic Layer:")),
        uiOutput("active_case_details_ui")
      ),
      
      # Maps layout
      fluidRow(
        column(6,
          div(class = "panel-glass", style = "padding:20px; min-height:500px;",
            div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
              h4(style = "color:#1e293b; margin:0; font-weight:600;", 
                 trans("Mapa 2D: Geod\u00e9sicas e Interacci\u00f3n", "2D Map: Geodesics & Interaction")),
              actionButton("toggle_2d_fullscreen", trans("Pantalla Completa", "Fullscreen"), class = "btn-info btn-xs", icon = icon("expand"), style = "padding:6px 12px; font-size:0.95rem; font-weight:600;")
            ),
            leafletOutput("leaflet_map", height = "430px")
          )
        ),
        column(6,
          div(class = "panel-glass", style = "padding:20px; min-height:500px;",
            div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
              h4(style = "color:#1e293b; margin:0; font-weight:600;", 
                 trans("Malla 3D: Variedad Deformada (Manifold)", "3D Mesh: Deformed Manifold")),
              actionButton("toggle_3d_fullscreen", trans("Pantalla Completa", "Fullscreen"), class = "btn-info btn-xs", icon = icon("expand"), style = "padding:6px 12px; font-size:0.95rem; font-weight:600;")
            ),
            plotlyOutput("plotly_mesh", height = "430px")
          )
        )
      ),
      
      # HUD and KPIs Panels
      fluidRow(style = "margin-top:20px;",
        column(5,
          div(class = "panel-glass", style = "padding:15px; min-height:220px;",
            h4(style = "color:#0284c7; margin-top:0; font-weight:600;", 
               trans("HUD de P\u00edxel Ontol\u00f3gico", "Ontological Pixel HUD")),
            uiOutput("pixel_fiche_ui")
          )
        ),
        column(7,
          div(class = "panel-glass", style = "padding:15px; min-height:220px;",
            h4(style = "color:#0f766e; margin-top:0; font-weight:600;", 
               trans("Estado del Solucionador y KPIs", "Solver Status & KPIs")),
            uiOutput("solver_kpis_ui")
          )
        )
      ),
      
      # Live Feed Console Panel
      fluidRow(style = "margin-top:20px;",
        column(12,
          div(class = "panel-glass", style = "padding:15px; min-height:140px; background: #0f172a; border: 1px solid #334155; border-radius: 8px;",
            h4(style = "color:#10b981; margin-top:0; font-family: monospace; font-weight:600;", 
               trans("Consola de Eventos Tensivos en Vivo (Live Feed)", "Live Tensive Event Console (Live Feed)")),
            div(style = "font-family: monospace; color: #34d399; font-size: 0.85rem; max-height: 90px; overflow-y: auto; padding: 5px; line-height: 1.4;",
                htmlOutput("live_feed_ui"))
          )
        )
      )
    )
  )
}