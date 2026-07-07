# modules/tab0_home.R
# Module for Home Landing Page (Ontolog\u00eda Po\u00e9tica del Territorio)

tab0_ui <- function() {
  tabPanel("Inicio",
           withMathJax(uiOutput("tab0_home_ui"))
  )
}

tab0_server <- function(input, output, session, lang) {
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  output$tab0_home_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    
    div(class = "container-fluid", style = "padding: 30px; margin: 0 auto;",
      fluidRow(
        column(2),
        column(8,
      # HEADER SECTION
      div(class = "panel-glass text-center", style = "padding: 40px; margin-bottom: 30px; border-bottom: 4px solid #6366f1; border-radius: 12px;",
        h1(style = "color: #0369a1; font-size: 2.8rem; font-weight: 800; margin-bottom: 15px;",
           trans("Puerto Umbral", "Puerto Umbral")),
        h2(style = "color: #b45309; font-size: 1.6rem; font-weight: 600; margin-bottom: 20px;",
           trans("Ontolog\u00eda Territorial del Espacio Relacional", "Territorial Ontology of Relational Space")),
        p(style = "color: #1e293b; font-size: 1.15rem; line-height: 1.7; max-width: 800px; margin: 0 auto;",
          trans(
            "Bienvenido a la plataforma interactiva de simulaci\u00f3n y an\u00e1lisis de Geotensores. Aqu\u00ed se materializan y validan emp\u00edricamente los postulados de la Ontolog\u00eda Pedestre formulados en el Tomo I y formalizados matem\u00e1ticamente en el Tomo II.",
            "Welcome to the interactive platform for simulation and analysis of Geotensors. Here, the postulates of the Pedestrian Ontology formulated in Volume I and mathematically formalized in Volume II are materialized and empirically validated."
          ))
      ),
      
      # DOWNLOAD CARDS SECTION
      h3(style = "color: #0369a1; margin-top: 40px; margin-bottom: 20px; font-weight: 700;", 
         trans("Descarga de la Obra Completa", "Download the Complete Work")),
      fluidRow(
        column(6,
          div(class = "panel-glass", style = "padding: 30px; height: 100%; display: flex; flex-direction: column; justify-content: space-between; border-radius: 12px;",
            div(
              h4(style = "color: #0f766e; font-weight: 700; margin-bottom: 15px;", 
                 trans("Tomo I: Ontolog\u00eda del Habitar Colectivo", "Volume I: Ontology of Collective Dwelling")),
              p(style = "color: #334155; font-size: 1.05rem; line-height: 1.6;",
                trans(
                  "Establece los fundamentos filos\u00f3ficos de la co-presencia, las derivas Wu Wei y las redes de cuidado espont\u00e1neas. Propone la deconstrucci\u00f3n del espacio absoluto euclidiano en favor de una m\u00e9trica subjetiva y relacional del territorio urbano.",
                  "Establishes the philosophical foundations of co-presence, Wu Wei drifts, and spontaneous care networks. It proposes the deconstruction of absolute Euclidean space in favor of a subjective and relational metric of urban territory."
                ))
            ),
            div(style = "margin-top: 20px;",
              tags$a(
                href = "../media/docs/tomo_i.pdf",
                download = "Puerto_Umbral_Tomo_I.pdf",
                class = "btn btn-success btn-lg",
                style = "width: 100%; font-weight: bold; display: block; text-align: center;",
                trans("Descargar Tomo I (PDF)", "Download Volume I (PDF)")
              )
            )
          )
        ),
        column(6,
          div(class = "panel-glass", style = "padding: 30px; height: 100%; display: flex; flex-direction: column; justify-content: space-between; border-radius: 12px;",
            div(
              h4(style = "color: #0f766e; font-weight: 700; margin-bottom: 15px;", 
                 trans("Tomo II: Geotensores e Instrumentaci\u00f3n", "Volume II: Geotensors & Instrumentation")),
               p(style = "color: #334155; font-size: 1.05rem; line-height: 1.6;",
                trans(
                  "Formaliza matem\u00e1ticamente la ontolog\u00eda pedestre a contracorriente a trav\u00e9s de la m\u00e9trica de Riemann, resolvedores num\u00e9ricos de contorno (BVP), la deformaci\u00f3n lineal de Weyl y el conmutador algebraico de Lie para medir las asimetr\u00edas de poder territorial.",
                  "Mathematically formalizes the pedestrian ontology through Riemannian metrics, boundary value solvers (BVP), Weyl linear deformation, and the Lie algebraic commutator to measure territorial power asymmetries."
                ))
            ),
            div(style = "margin-top: 20px;",
              tags$a(
                href = "../media/docs/tomo_ii.pdf",
                download = "Puerto_Umbral_Tomo_II.pdf",
                class = "btn btn-success btn-lg",
                style = "width: 100%; font-weight: bold; display: block; text-align: center;",
                trans("Descargar Tomo II (PDF)", "Download Volume II (PDF)")
              )
            )
          )
        )
      ),
      
      # EXPLANATORY ILLUSTRATIONS SECTION (Large previews, not stamps)
      h3(style = "color: #0369a1; margin-top: 50px; margin-bottom: 25px; font-weight: 700;", 
         trans("Conceptos Clave Ilustrados", "Key Illustrated Concepts")),
      
      # 1. Snell Refraction
      div(class = "panel-glass", style = "padding: 30px; margin-bottom: 30px; border-radius: 12px;",
        fluidRow(
          column(5,
            div(style = "text-align: center;",
              tags$img(src = "images/refraction_didactic.png", style = "width: 100%; max-width: 560px; border-radius: 8px; border: 2px solid rgba(255,255,255,0.1); box-shadow: 0 4px 15px rgba(0,0,0,0.5);")
            )
          ),
          column(7,
            h4(style = "color: #b91c1c; font-weight: 700; font-size: 1.35rem;", trans("Refracci\u00f3n Peatonal (Ley de Snell)", "Pedestrian Refraction (Snell's Law)")),
            p(style = "color: #334155; font-size: 1.1rem; line-height: 1.6; margin-top: 15px;",
              trans(
                "Cuando los habitantes cruzan l\u00edmites municipales o comunales segregados, experimentan un cambio brusco en la fricci\u00f3n urbana. Al igual que la luz al cambiar de medio \u00f3ptico, las trayectorias peatonales se refractan (desv\u00edan su \u00e1ngulo) debido a la variaci\u00f3n de la velocidad intr\u00ednseca del habitar.",
                "When residents cross segregated municipal boundaries, they experience a sharp change in urban friction. Just like light changing optical media, pedestrian paths refract (deflecting their angle) due to the variation in the intrinsic speed of dwelling."
              )),
            actionButton("go_to_refraction", trans("Simular Refracci\u00f3n", "Simulate Refraction"), class = "btn-info", style = "margin-top: 15px; font-weight: bold;")
          )
        )
      ),
      
      # 2. Hessian Curvature
      div(class = "panel-glass", style = "padding: 30px; margin-bottom: 30px; border-radius: 12px;",
        fluidRow(
          column(5,
            div(style = "text-align: center;",
              tags$img(src = "images/hessian_didactic.png", style = "width: 100%; max-width: 560px; border-radius: 8px; border: 2px solid rgba(255,255,255,0.1); box-shadow: 0 4px 15px rgba(0,0,0,0.5);")
            )
          ),
          column(7,
            h4(style = "color: #b91c1c; font-weight: 700; font-size: 1.35rem;", trans("Curvatura Hessiana y Autopoiesis", "Hessian Curvature & Autopoiesis")),
            p(style = "color: #334155; font-size: 1.1rem; line-height: 1.6; margin-top: 15px;",
              trans(
                "La autopoiesis comunitaria invierte la curvatura del potencial local del suelo. Espacios que el capital condena como fosas de exclusi\u00f3n repulsora se transforman en atractores locales de amparo e infraestructura autogestionada (ollas comunes, asambleas), reconfigurando el tensor de Riemann.",
                "Community autopoiesis inverts the curvature of local land potential. Spaces condemned by capital as repulsive exclusion pits transform into local attractors of shelter and self-managed infrastructure (soup kitchens, assemblies), reconfiguring the Riemann tensor."
              )),
            actionButton("go_to_hessian", trans("Simular Autopoiesis", "Simulate Autopoiesis"), class = "btn-info", style = "margin-top: 15px; font-weight: bold;")
          )
        )
      ),
      
      # 3. Ledoit-Wolf Covariance
      div(class = "panel-glass", style = "padding: 30px; margin-bottom: 30px; border-radius: 12px;",
        fluidRow(
          column(5,
            div(style = "text-align: center;",
              tags$img(src = "images/ledoit_didactic.png", style = "width: 100%; max-width: 560px; border-radius: 8px; border: 2px solid rgba(255,255,255,0.1); box-shadow: 0 4px 15px rgba(0,0,0,0.5);")
            )
          ),
          column(7,
            h4(style = "color: #b91c1c; font-weight: 700; font-size: 1.35rem;", trans("Regularizaci\u00f3n de Moran y Ledoit-Wolf", "Moran & Ledoit-Wolf Regularization")),
            p(style = "color: #334155; font-size: 1.1rem; line-height: 1.6; margin-top: 15px;",
              trans(
                "Para estimar fielmente el geotensor a partir de datos emp\u00edricos escasos o con ruido, la plataforma aplica encogimiento lineal de Ledoit-Wolf y filtrado geoestad\u00edstico de Moran. Esto estabiliza la m\u00e9trica territorial e impide la aparici\u00f3n de singularidades no f\u00edsicas en los autovalores.",
                "To faithfully estimate the geotensor from sparse or noisy empirical data, the platform applies Ledoit-Wolf linear shrinkage and Moran geostatistical filtering. This stabilizes the territorial metric and prevents non-physical singularities in the eigenvalues."
              )),
            actionButton("go_to_regularization", trans("Ver Regularizaci\u00f3n", "View Regularization"), class = "btn-info", style = "margin-top: 15px; font-weight: bold;")
          )
        )
      ),
      
      # 4. Caputo Fractional Memory
      div(class = "panel-glass", style = "padding: 30px; margin-bottom: 30px; border-radius: 12px;",
        fluidRow(
          column(5,
            div(style = "text-align: center;",
              tags$img(src = "images/memory_didactic.png", style = "width: 100%; max-width: 560px; border-radius: 8px; border: 2px solid rgba(255,255,255,0.1); box-shadow: 0 4px 15px rgba(0,0,0,0.5);")
            )
          ),
          column(7,
            h4(style = "color: #b91c1c; font-weight: 700; font-size: 1.35rem;", trans("Memoria Fraccionaria del Trauma (Caputo)", "Fractional Trauma Memory (Caputo)")),
            p(style = "color: #334155; font-size: 1.1rem; line-height: 1.6; margin-top: 15px;",
              trans(
                "El trauma urbano y los hitos de violencia sobre el territorio no desaparecen inmediatamente; persisten en la memoria colectiva del suelo. Utilizando el operador de Caputo para derivadas de orden fraccionario (0 < alpha < 1), modelamos c\u00f3mo el pasado altera de forma persistente y no local el habitar y caminar presente.",
                "Urban trauma and violence milestones on the territory do not disappear immediately; they persist in the collective memory of the soil. Using the Caputo operator for fractional order derivatives (0 < alpha < 1), we model how the past persistently and non-locally alters present dwelling and walking."
              )),
            actionButton("go_to_memory", trans("Simular Memoria", "Simulate Memory"), class = "btn-info", style = "margin-top: 15px; font-weight: bold;")
          )
        )
      ),
      
      # FOOTER ROUTING BUTTONS
      div(class = "text-center", style = "margin-top: 50px; padding: 20px;",
        h3(style = "color: #0369a1; margin-bottom: 25px; font-weight: 700;", trans("Navegaci\u00f3n del Ecosistema", "Ecosystem Navigation")),
        div(style = "display: flex; gap: 15px; justify-content: center; flex-wrap: wrap;",
          actionButton("go_to_sim_panel", trans("Simulador Geod\u00e9sico 2D/3D", "Geodesic Simulator 2D/3D"), class = "btn-primary btn-lg", style = "padding: 12px 25px; font-weight: bold;"),
          actionButton("go_to_math_panel", trans("An\u00e1lisis de Estabilidad", "Stability Analysis"), class = "btn-warning btn-lg", style = "padding: 12px 25px; font-weight: bold;"),
          actionButton("go_to_agent_panel", trans("Agente de Consultas", "QA Inquiry Agent"), class = "btn-info btn-lg", style = "padding: 12px 25px; font-weight: bold;")
        )
      )
        ),
        column(2)
      )
    )
  })
  
  # 1. Download handlers
  output$download_tomo1_home <- downloadHandler(
    filename = function() { "Puerto_Umbral_Tomo_I.pdf" },
    content = function(file) {
      file.copy("www/docs/tomo_i.pdf", file)
    }
  )
  
  output$download_tomo2_home <- downloadHandler(
    filename = function() { "Puerto_Umbral_Tomo_II.pdf" },
    content = function(file) {
      file.copy("www/docs/tomo_ii.pdf", file)
    }
  )
  
  # 2. Page routing handlers (interacting with parent navbar)
  observeEvent(input$go_to_refraction, {
    updateNavbarPage(session = session, "nav_active", selected = "L\u00edneas de Trabajo")
    updateSelectInput(session = session, "exp_choice", selected = "exp7")
  })
  
  observeEvent(input$go_to_hessian, {
    updateNavbarPage(session = session, "nav_active", selected = "L\u00edneas de Trabajo")
    updateSelectInput(session = session, "exp_choice", selected = "exp3")
  })
  
  observeEvent(input$go_to_regularization, {
    updateNavbarPage(session = session, "nav_active", selected = "L\u00edneas de Trabajo")
    updateSelectInput(session = session, "exp_choice", selected = "exp5")
  })
  
  observeEvent(input$go_to_memory, {
    updateNavbarPage(session = session, "nav_active", selected = "L\u00edneas de Trabajo")
    updateSelectInput(session = session, "exp_choice", selected = "exp4")
  })
  
  observeEvent(input$go_to_sim_panel, {
    updateNavbarPage(session = session, "nav_active", selected = "Centro de Simulaci\u00f3n")
  })
  
  observeEvent(input$go_to_math_panel, {
    updateNavbarPage(session = session, "nav_active", selected = "An\u00e1lisis Matem\u00e1tico")
  })
  
  observeEvent(input$go_to_agent_panel, {
    updateNavbarPage(session = session, "nav_active", selected = "Biblioteca y Agente")
  })
  

}
