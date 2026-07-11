# ---- AJUSTE AUTOM\u00c1TICO DE DIRECTORIO DE TRABAJO (RStudio / Windows) ----
if (dir.exists("puerto_umbral_zenodo_bundle/app")) {
  setwd("puerto_umbral_zenodo_bundle/app")
} else if (basename(getwd()) != "app" && dir.exists("app")) {
  setwd("app")
}

# ---- GARANTIZAR LA CARGA DEL ENTORNO GLOBAL (global.R) ----
source("global.R", encoding = "UTF-8")

# =====================================================================
# Puerto Umbral: Plataforma de Ontología Territorial y Geotensores
# Orquestador Principal - Arquitectura Modular y Programación Funcional
# Autor / Contacto: john.treimun.r@uai.cl
# =====================================================================

# (Toda la carga de librer\u00edas, resolvedores de f\u00edsica y datos de manzanas
# se inicializan autom\u00e1ticamente desde global.R al cargar la aplicaci\u00f3n)

# ---- CARGA DE BASES DE DATOS EXTRA EN INICIO (Memoria Compartida) ----
tomo1_db <- tryCatch({
  jsonlite::fromJSON("www/data/tomo_i_text.json")
}, error = function(e) {
  data.frame(volume = integer(), chapter = character(), text = character(), page = integer(), stringsAsFactors = FALSE)
})

tomo2_db <- tryCatch({
  jsonlite::fromJSON("www/data/tomo_ii_text.json")
}, error = function(e) {
  data.frame(volume = 2, chapter = "General", text = character(), page = 0, stringsAsFactors = FALSE)
})

# ---- DISE\u00d1O DE LA INTERFAZ DE USUARIO (UI) ----
ui <- tagList(
  useShinyjs(),
  tags$head(
    # Service Worker Version Purge & Update Enforcement
    tags$script(HTML("
      (function() {
        var CURRENT_VERSION = 'v12';
        if (localStorage.getItem('app_sw_version') !== CURRENT_VERSION) {
          if ('serviceWorker' in navigator) {
            navigator.serviceWorker.getRegistrations().then(function(registrations) {
              var promises = registrations.map(function(r) { return r.unregister(); });
              Promise.all(promises).then(function() {
                localStorage.setItem('app_sw_version', CURRENT_VERSION);
                window.location.reload();
              });
            }).catch(function() {
              localStorage.setItem('app_sw_version', CURRENT_VERSION);
            });
          } else {
            localStorage.setItem('app_sw_version', CURRENT_VERSION);
          }
        }
      })();
    ")),
    uiOutput("field_mode_css"),
    # Bing Webmaster Site Verification Meta Tag
    tags$meta(name = "msvalidate.01", content = "0DD7C9BCF5A3FF8BBD5D64A773EA8D71"),
    # Google Search Console Verification Meta Tag
    tags$meta(name = "google-site-verification", content = "2n2kCsOuT24KBkiYLQUUDWmx8JTAdssHeRcJNmswQNE"),
    # AI Agents Metadata & Discoverability Links (Semantic Governance)
    tags$link(rel = "llms", href = "llms.txt"),
    tags$link(rel = "llms-full", href = "llms-full.txt"),
    tags$link(rel = "context", href = "context.json"),
    tags$meta(name = "ai-discoverability", content = "llms.txt"),
    # Scroll to Top Styles and Script
    tags$style(HTML("
      .scroll-top-btn {
        position: fixed;
        bottom: 25px;
        right: 25px;
        z-index: 9999;
        background-color: #0d9488 !important;
        color: white !important;
        border: none;
        outline: none;
        cursor: pointer;
        width: 45px;
        height: 45px;
        border-radius: 50%;
        box-shadow: 0 4px 10px rgba(0,0,0,0.3);
        transition: background-color 0.3s, transform 0.2s;
        display: none; /* Controlled by jQuery */
        align-items: center;
        justify-content: center;
      }
      .scroll-top-btn:hover {
        background-color: #0f766e !important;
        transform: scale(1.1);
      }
    ")),
    tags$script(HTML("
      $(window).scroll(function() {
        var btn = $('#scroll_top_btn');
        if ($(window).scrollTop() > 300) {
          btn.css('display', 'flex');
        } else {
          btn.css('display', 'none');
        }
      });
    ")),
    # Carga del CSS principal estático y cacheable
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Script para traducciones autom\u00e1ticas en el cliente y adici\u00f3n de tooltips (popups flotantes)
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'lang_toggle') {
          var lang = event.value;
          var brand = lang === 'EN' ? 'Puerto Umbral - Territorial Ontology' : 'Puerto Umbral - Ontolog\u00eda Territorial';
          $('.navbar-brand').html(brand);
          var tab0 = lang === 'EN' ? 'Home' : 'Inicio';
          var tab1 = lang === 'EN' ? 'Onboarding' : 'Pedagog\u00eda';
          var tab2 = lang === 'EN' ? 'Experiment Lines' : 'L\u00edneas de Trabajo';
          var tab3 = lang === 'EN' ? 'Simulation Center' : 'Centro de Simulaci\u00f3n';
          var tab4 = lang === 'EN' ? 'Mathematical Analysis' : 'An\u00e1lisis Matem\u00e1tico';
          var tab5 = lang === 'EN' ? 'Library & QA Agent' : 'Biblioteca y Agente';
          
          // Tooltips del Men\u00fa Principal
          $('a[data-value=\"Inicio\"]').html(tab0).attr('title', lang === 'EN' ? 'Download books and view illustrated concepts' : 'Descarga de libros y conceptos clave ilustrados');
          $('a[data-value=\"Pedagog\u00eda\"]').html(tab1).attr('title', lang === 'EN' ? 'Conceptual harmonization matrix and ecosystem agents' : 'Matriz de armonizaci\u00f3n conceptual y agentes del ecosistema');
          $('a[data-value=\"L\u00edneas de Trabajo\"]').html(tab2).attr('title', lang === 'EN' ? 'Ingest field data and perform statistical audits' : 'Ingesta de datos y auditor\u00eda de experimentos de campo');
          $('a[data-value=\"Centro de Simulaci\u00f3n\"]').html(tab3).attr('title', lang === 'EN' ? 'Interactive geodesic solver and potential fields' : 'Resolvedor geod\u00e9sico interactivo y campos de potencial');
          $('a[data-value=\"An\u00e1lisis Matem\u00e1tico\"]').html(tab4).attr('title', lang === 'EN' ? 'Lyapunov decay curves and Lie bracket analysis' : 'Decaimiento de Lyapunov y conmutadores de Lie');
          $('a[data-value=\"Biblioteca y Agente\"]').html(tab5).attr('title', lang === 'EN' ? 'Download books and chat with the local QA AI agent' : 'Descargas de tomos y chat con el agente local de consulta');
          
          // Tooltips de las Pesta\u00f1as de Gr\u00e1ficos de An\u00e1lisis Matem\u00e1tico
          $('a[data-value=\"Conmutador de Lie [X, Y]\"], a[data-value=\"Lie Commutator [X, Y]\"]').attr('title', lang === 'EN' ? 'Temporal order asymmetry analysis [X,Y]' : 'An\u00e1lisis de asimetr\u00eda del orden temporal [X,Y]');
          $('a[data-value=\"Estabilidad y Autovalores\"], a[data-value=\"Stability & Eigenvalues\"]').attr('title', lang === 'EN' ? 'Jacobian complex eigenvalues stability plane' : 'Plano de estabilidad de autovalores del Jacobiano');
          $('a[data-value=\"Decaimiento de Lyapunov\"], a[data-value=\"Lyapunov Decay\"]').attr('title', lang === 'EN' ? 'Energy dissipation monitoring over time' : 'Monitoreo de disipaci\u00f3n de energ\u00eda vital');
        }
      });
      $(document).on('shiny:idle', function(event) {
        if (window.MathJax) {
          MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
        }
      });
    "))
  ),
  
  # Cabecera absoluta con Selector de Idioma (Ergonom\u00eda removida para evitar colisiones)
  tags$div(
    id = "lang_selector_container",
    tags$span(id = "lang_label", "Idioma:"),
    selectInput("lang_toggle", NULL, choices = list("ES" = "ES", "EN" = "EN"), selected = "ES", width = "85px")
  ),
  
  # Bot\u00f3n flotante lateral para activar el Drawer de Ergonom\u00eda (Localizado m\u00e1s abajo)
  actionButton("toggle_ergo", 
               HTML("<i class='fa fa-shield'></i> Ergonom\u00eda / Field Ergonomics"), 
               class = "btn-info btn-sm"),
  
  # =====================================================================
  # CONFIGURACI\u00d3N DEL MEN\u00da DE NAVEGACI\u00d3N Y T\u00cdTULOS (NAVBAR)
  # =====================================================================
  # NOTA PARA EL OPERADOR:
  # El tema 'flatly' es un tema Bootstrap claro y limpio, elegido para contrastar
  # con las hojas de estilo personalizadas (styles.css). 
  # Si desea desactivar todos los estilos personalizados o probar con otros temas
  # de la librer\u00eda shinythemes (ej. 'cosmo', 'journal', 'spacelab'), modifique la
  # l\u00ednea 'theme = shinytheme("flatly")'.
  # =====================================================================
  # Estructura de navegaci\u00f3n principal por pesta\u00f1as modularizadas
  withMathJax(
    navbarPage("Puerto Umbral - Ontolog\u00eda Territorial",
               id = "nav_active",
               theme = shinytheme("flatly"),
               
               # UI de los subm\u00f3dulos cargados din\u00e1micamente:
               tab0_ui(),  # Inicio / Descargas
               tab1_ui(),  # Pedagog\u00eda / Harmonizaci\u00f3n
               tab2_ui(),  # Experimentos de Terreno
               tab3_ui(),  # Centro de Simulaci\u00f3n (BVP/ODE)
               tab4_ui(),  # An\u00e1lisis Matem\u00e1tico (Lyapunov/Lie)
               tab5_ui()   # Biblioteca y Agente de Consulta (QA)
    )
  ),
  
  ergo_drawer_ui(),
  
  # Floating Scroll to Top Button
  tags$button(
    id = "scroll_top_btn",
    class = "scroll-top-btn",
    onclick = "window.scrollTo({top: 0, behavior: 'smooth'});",
    tags$i(class = "fa fa-chevron-up")
  )
)

# ---- L\u00d3GICA DEL SERVIDOR (SERVER) ----
server <- function(input, output, session) {
  
  # ---- ESTADOS REACTIVOS COMPARTIDOS ----
  lang <- reactive({
    if (is.null(input$lang_toggle)) "ES" else input$lang_toggle
  })
  
  ergo_open <- reactiveVal(FALSE)
  active_exp_line <- reactiveVal(1)
  chat_messages <- reactiveVal(list())
  run_sim_trigger <- reactiveVal(0)
  
  # ---- ORQUESTACI\u00d3N DE SUBM\u00d3DULOS DE SERVIDOR ----
  tab0_server(input, output, session, lang)
  tab1_server(input, output, session, lang, run_sim_trigger, active_exp_line)
  tab2_server(input, output, session, active_exp_line, lang, run_sim_trigger)
  tab4_server(input, output, session, lang)
  tab5_server(input, output, session, chat_messages, lang, tomo1_db, tomo2_db)
  ergo_server(input, output, session, ergo_open, lang)
  
  # ---- DYNAMIC HIGH-CONTRAST FIELD MODE STYLESHEET ----
  output$field_mode_css <- renderUI({
    if (isTRUE(input$mode_field)) {
      tags$link(rel = "stylesheet", type = "text/css", href = "field_mode.css")
    } else {
      NULL
    }
  })
  
  # ---- ORQUESTACI\u00d3N DEL MOTOR CENTRAL DE SIMULACI\u00d3N Y ODE ----
  sim_server(input, output, session, lang, run_sim_trigger)
}

# ---- LANZAMIENTO DE LA APLICACI\u00d3N ----
shinyApp(ui, server)