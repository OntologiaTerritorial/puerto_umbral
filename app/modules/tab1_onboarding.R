# modules/tab1_onboarding.R
# Pure Functional Landing/Pedagogy Page Module (Bilingually separated, fully escaped, high-contrast)

# UI Definition called from app.R UI block
tab1_ui <- function() {
  tabPanel("Pedagog\u00eda",
           uiOutput("tab1_landing_ui")
  )
}

# Server Definition called from app.R server block
tab1_server <- function(input, output, session, lang, run_sim_trigger, active_exp_line) {
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  output$tab1_landing_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    
    fluidPage(
      # REGISTRO DE SOPORTE MATHJAX (por compatibilidad local)
      withMathJax(),
      
      # SECCION 1: ENCABEZADO DE BIENVENIDA ESTILO PORTADA PREMIUM
      fluidRow(
        column(12,
          div(style = "padding: 40px; margin-bottom: 30px; text-align: center; border-radius: 16px; background: linear-gradient(135deg, #0f172a 0%, #1e1b4b 50%, #020617 100%); border: 1px solid rgba(255,255,255,0.08); box-shadow: 0 10px 30px rgba(0,0,0,0.5); position: relative; overflow: hidden;",
            # Ambient light
            div(style = "position: absolute; top: -50%; left: -50%; width: 200%; height: 200%; background: radial-gradient(circle, rgba(14,165,233,0.12) 0%, transparent 60%); pointer-events: none;"),
            
            # Epigraph in Lora style
            tags$p(style = "font-style: italic; font-family: 'Lora', 'Georgia', serif; font-size: 1.2rem; color: #94a3b8; max-width: 800px; margin: 0 auto 25px auto; line-height: 1.6; border-bottom: 1px dashed rgba(255,255,255,0.15); padding-bottom: 20px;",
                   trans("\u201cEl espacio no es un recept\u00e1culo vac\u00edo donde se disponen las cosas, sino la trama tensional que resulta de sus relaciones y afectos.\u201d",
                         "\u201cSpace is not an empty receptacle where things are arranged, but the tensive web that results from their relations and affects.\u201d")),
            
            span(style = "font-size: 0.85rem; text-transform: uppercase; letter-spacing: 4px; color: #38bdf8; font-weight: 700; display: block; margin-bottom: 8px;",
                 trans("OBRA DE INVESTIGACI\u00d3N TERRITORIAL", "TERRITORIAL RESEARCH MONOGRAPH")),
            h1(style = "color: #ffffff; font-weight: 800; margin: 0 0 15px 0; font-size: 2.8rem; letter-spacing: -1px; text-shadow: 0 2px 10px rgba(0,0,0,0.5); font-family: 'Outfit', sans-serif;",
               trans("GEOTENSORES Y ONTOLOG\u00cdA TERRITORIAL", "GEOTENSORS & TERRITORIAL ONTOLOGY")),
            h2(style = "color: #cbd5e1; font-size: 1.3rem; font-weight: 400; margin: 0 auto 25px auto; max-width: 800px; line-height: 1.5;",
               HTML(trans("D\u00edptico Operacional: <span style='color: #fbbf24; font-weight: 600;'>Tomo I</span> (Intuiciones Relacionales) y <span style='color: #38bdf8; font-weight: 600;'>Tomo II</span> (Formalismo M\u00e9trico)",
                          "Operational Diptych: <span style='color: #fbbf24; font-weight: 600;'>Volume I</span> (Relational Intuitions) & <span style='color: #38bdf8; font-weight: 600;'>Volume II</span> (Metric Formalism)"))) ,
            
            div(style = "display: flex; justify-content: center; gap: 15px; flex-wrap: wrap;",
                span(class = "badge", style = "background: rgba(251,191,36,0.1); border: 1px solid rgba(251,191,36,0.3); color: #fbbf24; padding: 6px 12px; font-size: 0.85rem; border-radius: 8px;", trans("Tomo I: Geograf\u00eda Afectiva", "Vol I: Affective Geography")),
                span(class = "badge", style = "background: rgba(56,189,248,0.1); border: 1px solid rgba(56,189,248,0.3); color: #38bdf8; padding: 6px 12px; font-size: 0.85rem; border-radius: 8px;", trans("Tomo II: F\u00edsica Riemanniana", "Vol II: Riemannian Physics")),
                span(class = "badge", style = "background: rgba(16,185,129,0.1); border: 1px solid rgba(16,185,129,0.3); color: #10b981; padding: 6px 12px; font-size: 0.85rem; border-radius: 8px;", trans("Tr\u00edada de Agentes Locales", "Triad of Local Agents"))
            )
          )
        )
      ),
      
      # SECCION 2: MATRIZ Y AGENTES
      fluidRow(
        # COLUMNA IZQUIERDA: Matriz de Armonizacion Conceptual (Ancho: 7/12)
        column(7,
          h3(style = "color: #0369a1; font-weight: 600; margin-top: 0; margin-bottom: 15px; border-bottom: 1px solid rgba(0,0,0,0.08); padding-bottom: 8px;",
             trans("Armonizaci\u00f3n Conceptual: Tomo I \u2194 Tomo II", "Conceptual Harmonization: Volume I \u2194 Volume II")),
          
          tags$p(style = "font-size: 0.95rem; color: #475569; margin-bottom: 20px;",
                 trans("La siguiente matriz describe c\u00f3mo las intuiciones relacionales del primer volumen se traducen en estructuras y observables matem\u00e1ticos en el segundo:",
                       "The following matrix describes how the relational intuitions of the first volume translate into mathematical structures and observables in the second:")),
          
          # Contenedor de la Tabla en HTML
          div(class = "panel-glass", style = "padding: 15px; margin-bottom: 20px; overflow-x: auto;",
              HTML(paste0(
                "<table class='table table-striped table-bordered' style='margin-bottom: 0; width: 100%; color: #334155 !important;'>",
                "<thead>",
                "  <tr>",
                "    <th style='width: 30%; color: #0f766e;'>", trans("Concepto (Tomo I)", "Concept (Volume I)"), "</th>",
                "    <th style='width: 40%; color: #b45309;'>", trans("Formalizaci\u00f3n (Tomo II)", "Formalization (Volume II)"), "</th>",
                "    <th style='width: 30%; color: #0f766e;'>", trans("Significado F\u00edsico", "Physical Meaning"), "</th>",
                "  </tr>",
                "</thead>",
                "<tbody>",
                "  <tr>",
                "    <td><b>", trans("P\u00edxel Ontol\u00f3gico", "Ontological Pixel"), "</b></td>",
                "    <td>", trans("$$\\pi_i = (\\text{id}, x_i, y_i, \\text{altitud}, \\text{NDVI}, H_i(t), \\lambda_i)$$", "$$\\pi_i = (\\text{id}, x_i, y_i, \\text{altitude}, \\text{NDVI}, H_i(t), \\lambda_i)$$"), "</td>",
                "    <td>", trans("M\u00ednima celda de est\u00e1tica con memoria y latencia.", "Minimum static cell with memory and latency."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Territorio como Devenir", "Territory as Becoming"), "</b></td>",
                "    <td>$$\\partial_t g_{ij} = -2P_{ij} + \\nabla_{(i} \\Omega_{j)}$$</td>",
                "    <td>", trans("M\u00e9trica din\u00e1mica deformada por el Poder y la Voluntad.", "Dynamic metric deformed by Power and Will."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Poder (Deformador)", "Power (Deformer)"), "</b></td>",
                "    <td>", trans("$$P_{ij} \\quad (\\text{Tensor de Poder})$$", "$$P_{ij} \\quad (\\text{Power Tensor})$$"), "</td>",
                "    <td>", trans("Fricci\u00f3n social y segregaci\u00f3n que curvan las geod\u00e9sicas.", "Social friction and segregation that curve geodesics."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Voluntad (Fuerza)", "Will (Force)"), "</b></td>",
                "    <td>", trans("$$\\Omega_i \\quad (\\text{Vector de Voluntad})$$", "$$\\Omega_i \\quad (\\text{Will Vector})$$"), "</td>",
                "    <td>", trans("Fuerza comunitaria que tracciona y repara el espacio.", "Community force that pulls and repairs space."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Sentido (Atractor)", "Sense (Attractor)"), "</b></td>",
                "    <td>", trans("$$V_S \\ (\\text{potencial}) \\ \\ \\ \\text{o} \\ \\ \\ S_i = \\nabla_i V_S$$", "$$V_S \\ (\\text{potential}) \\ \\ \\ \\text{or} \\ \\ \\ S_i = \\nabla_i V_S$$"), "</td>",
                "    <td>", trans("Campo directriz que orienta el caminar colectivo.", "Direction field guiding collective walking."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Delimitaci\u00f3n (Borde)", "Boundary (Edge)"), "</b></td>",
                "    <td>", trans("$$\\alpha V + \\beta \\frac{\\partial V}{\\partial n} = \\gamma \\quad (\\text{Condici\u00f3n de Robin})$$", "$$\\alpha V + \\beta \\frac{\\partial V}{\\partial n} = \\gamma \\quad (\\text{Robin Condition})$$"), "</td>",
                "    <td>", trans("Porosidad y refracci\u00f3n selectiva en la piel del territorio.", "Porosity and selective refraction at the territorial skin."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Trauma (Memoria)", "Trauma (Memory)"), "</b></td>",
                "    <td>", trans("$${}^{C}D^\\alpha_t V(t) = -\\nu V(t) \\quad (\\text{Caputo})$$", "$${}^{C}D^\\alpha_t V(t) = -\\nu V(t) \\quad (\\text{Caputo})$$"), "</td>",
                "    <td>", trans("Decaimiento no markoviano de cola pesada de la memoria.", "Non-Markovian heavy-tailed decay of memory."), "</td>",
                "  </tr>",
                "  <tr>",
                "    <td><b>", trans("Resistencia / Cuidado", "Resistance / Care"), "</b></td>",
                "    <td>$$\\dot{V}_{Lyapunov} \\le 0$$</td>",
                "    <td>", trans("Disipaci\u00f3n de tensiones y retorno al equilibrio m\u00e9trico.", "Dissipation of tension and return to metric equilibrium."), "</td>",
                "  </tr>",
                "</tbody>",
                "</table>"
              ))
          )
        ),
        
        # COLUMNA DERECHA: Descripcion de Agentes del Ecosistema (Ancho: 5/12)
        column(5,
          h3(style = "color: #0369a1; font-weight: 600; margin-top: 0; margin-bottom: 15px; border-bottom: 1px solid rgba(0,0,0,0.08); padding-bottom: 8px;",
             trans("Agentes del Ecosistema", "Ecosystem Agents")),
          
          tags$p(style = "font-size: 0.95rem; color: #475569; margin-bottom: 20px;",
                 trans("La plataforma Puerto Umbral est\u00e1 habitada por una tr\u00edada de agentes adaptativos locales aut\u00f3nomos:",
                "The Puerto Threshold platform is inhabited by a triad of autonomous local adaptive agents:")),
          
          # Agente 1: Agente Pedestre (Cuidado)
          div(class = "panel-glass", style = "margin-bottom: 15px; padding: 15px; border-left: 4px solid #fbbf24;",
            h4(style = "color: #b45309; margin-top: 0; margin-bottom: 6px; font-size: 0.95rem; font-weight: 600;",
               trans("1. Agente Pedestre (Actor de Cuidado)", "1. Pedestrian Agent (Care Actor)")),
            tags$p(style = "font-size: 0.82rem; color: #334155; line-height: 1.45; margin-bottom: 0;",
                   trans("Calcula trayectorias geod\u00e9sicas BVP din\u00e1micas en el EQT, alimentando con sus fallos una Memoria Colectiva de Trauma persistente local (con decaimiento fraccionario del 6%), guiando a futuros caminantes a esquivar predictivamente hotspots hist\u00f3ricos.",
                         "Calculates dynamic BVP geodesics in the EQT, feeding its failures into a persistent local Collective Memory of Trauma (with 6% fractional decay) that guides future walkers to predictively avoid historical hotspots."))
          ),
          
          # Agente 2: Agente Auditor Vecinal
          div(class = "panel-glass", style = "margin-bottom: 15px; padding: 15px; border-left: 4px solid #38bdf8;",
            h4(style = "color: #0369a1; margin-top: 0; margin-bottom: 6px; font-size: 0.95rem; font-weight: 600;",
               trans("2. Agente Auditor Vecinal (Validaci\u00f3n)", "2. Neighborhood Audit Agent (Validation)")),
            tags$p(style = "font-size: 0.82rem; color: #334155; line-height: 1.45; margin-bottom: 0;",
                   trans("Inspecciona campa\u00f1as de terreno, validando privacidad FAIR/CARE y outliers espaciales, calibrando activamente su sensibilidad de tolerancia estadística Z (de 1.96 a 2.58) basándose en la retroalimentación de ruido del investigador.",
                         "Inspects field campaigns, validating FAIR/CARE privacy compliance and spatial outliers, actively calibrating its statistical Z-threshold sensitivity (from 1.96 to 2.58) based on researcher noise feedback."))
          ),
          
          # Agente 3: Agente Bibliotecario (Biblioteca Cognitiva)
          div(class = "panel-glass", style = "margin-bottom: 15px; padding: 15px; border-left: 4px solid #10b981;",
            h4(style = "color: #0f766e; margin-top: 0; margin-bottom: 6px; font-size: 0.95rem; font-weight: 600;",
               trans("3. Agente Bibliotecario (Compa\u00f1ero Cognitivo)", "3. Library Agent (Cognitive Companion)")),
            tags$p(style = "font-size: 0.82rem; color: #334155; line-height: 1.45; margin-bottom: 0;",
                   trans("Asiste en la consulta de la obra te\u00f3rica mediante s\u00edntesis de voz, aprende nuevos conceptos din\u00e1micamente mediante el comando 'aprender:' y registra preguntas no resueltas en una bit\u00e1cora de fallas para su autoperfeccionamiento continuo.",
                         "Assists in querying the theoretical work using voice synthesis, dynamically learns new concepts via the 'learn:' command, and logs unresolved queries in a closed-loop failure log for continuous self-improvement."))
          ),
          
          # --- TARJETA DE ACCION FLOTANTE (Redireccion al Simulador) ---
          div(class = "panel-glass", style = "padding: 20px; text-align: center; background: rgba(14, 165, 233, 0.04); border: 1px solid rgba(14, 165, 233, 0.2);",
            h4(style = "color: #0f172a; margin-top: 0; font-weight: 600;", trans("\u00a1Listo para Experimentar!", "Ready to Experiment!")),
            tags$p(style = "font-size: 0.85rem; color: #475569; margin-bottom: 15px;",
                   trans("Acceda al Centro de Simulaci\u00f3n para disparar geod\u00e9sicas, alterar potenciales o correr an\u00e1lisis de estabilidad sobre el relieve real de Pe\u00f1alol\u00e9n.",
                         "Go to the Simulation Center to launch geodesics, distort potentials, or run stability analysis on the real-world terrain of Pe\u00f1alol\u00e9n.")),
            actionButton("go_to_sim_from_home", trans("Ir al Centro de Simulaci\u00f3n", "Go to Simulation Center"), class = "btn-primary w-100",
                         onclick = "Shiny.setInputValue('nav_active', 'Centro de Simulaci\u00f3n');")
          )
        )
      ),
      
      # ROW 3: INSTRUMENTOS Y RUBRICAS DE CAMPO (Novedad para complementar Pedagogia y Apendices)
      fluidRow(style = "margin-top: 30px;",
        column(12,
          h3(style = "color: #0ea5e9; font-weight: 600; margin-top: 0; margin-bottom: 15px; border-bottom: 1px solid rgba(0,0,0,0.08); padding-bottom: 8px;",
             trans("Instrumentos y R\u00fabricas de Campo (Ap\u00e9ndices)", "Field Instruments & Rubrics (Appendices)")),
          tags$p(style = "font-size: 0.95rem; color: #475569; margin-bottom: 20px;",
                 trans("La operacionalizaci\u00f3n de los postulados te\u00f3ricos requiere herramientas estructuradas de validaci\u00f3n en terreno y salvaguardas de gobernanza descritas en los ap\u00e9ndices:",
                       "The operationalization of theoretical postulates requires structured field validation tools and governance safeguards described in the appendices:"))
        )
      ),
      
      fluidRow(style = "margin-bottom: 40px;",
        # Apendice E: Rubrica de Compatibilidad
        column(6,
          div(class = "panel-glass", style = "padding: 20px; border-top: 4px solid #0284c7; min-height: 280px;",
            h4(style = "color: #0369a1; margin-top: 0; margin-bottom: 12px; font-weight: 600;",
               trans("R\u00fabrica de Compatibilidad Tensiva (Ap\u00e9ndice E)", "Tensive Compatibility Rubric (Appendix E)")),
            tags$p(style = "font-size: 0.85rem; color: #334155; line-height: 1.6;",
                   trans("Eval\u00faa el nivel de acoplamiento y fricci\u00f3n entre la planificaci\u00f3n urbana institucional y las pr\u00e1cticas org\u00e1nicas del caminar peatonal en tres dimensiones operativas:",
                         "Evaluates the coupling level and friction between institutional urban planning and organic practices of pedestrian walking across three operational dimensions:")),
            tags$ul(style = "font-size: 0.82rem; color: #475569; padding-left: 20px; line-height: 1.5; margin-bottom: 0;",
              tags$li(HTML(trans("<b>Dimensi\u00f3n Geom\u00e9trica:</b> Mide la congruencia entre las geod\u00e9sicas de cuidado simuladas y los obst\u00e1culos f\u00edsicos o de plusval\u00eda.",
                                 "<b>Geometric Dimension:</b> Measures congruence between simulated care geodesics and physical or surplus value obstacles."))),
              tags$li(HTML(trans("<b>Dimensi\u00f3n Temporal:</b> Analiza la asimetr\u00eda cronol\u00f3gica del conmutador de Lie [X,Y] (prioridad vecinal vs presi\u00f3n del capital).",
                                 "<b>Temporal Dimension:</b> Analyzes the chronological asymmetry of the Lie commutator [X,Y] (neighborhood priority vs capital pressure)."))),
              tags$li(HTML(trans("<b>Dimensi\u00f3n Trascendental:</b> Valora la conexi\u00f3n afectiva, memoria colectiva del trauma (Caputo) y los commons ecol\u00f3gicos locales.",
                                 "<b>Transcendental Dimension:</b> Values affective connection, collective memory of trauma (Caputo), and local ecological commons.")))
            )
          )
        ),
        # Apendice D: Gobernanza de Datos
        column(6,
          div(class = "panel-glass", style = "padding: 20px; border-top: 4px solid #10b981; min-height: 280px;",
            h4(style = "color: #0f766e; margin-top: 0; margin-bottom: 12px; font-weight: 600;",
               trans("Gobernanza Vecinal de Datos (Ap\u00e9ndice D)", "Neighborhood Data Governance (Appendix D)")),
            tags$p(style = "font-size: 0.85rem; color: #334155; line-height: 1.6;",
                   trans("Establece los protocolos \u00e9ticos de co-dise\u00f1o comunitario para la ingesta de informaci\u00f3n en terreno, inspirados en los principios CARE y FAIR:",
                         "Establishes ethical protocols for community co-design in field data collection, inspired by CARE and FAIR principles:")),
            tags$ul(style = "font-size: 0.82rem; color: #475569; padding-left: 20px; line-height: 1.5; margin-bottom: 0;",
              tags$li(HTML(trans("<b>Soberan\u00eda de los Datos:</b> Control comunitario directo sobre las bases de datos vectoriales y cartograf\u00edas locales.",
                                 "<b>Data Sovereignty:</b> Direct community control over spatial vector databases and local cartographies."))),
              tags$li(HTML(trans("<b>Desidentificaci\u00f3n Activa:</b> Exigencia de remoci\u00f3n absoluta de metadatos de trazas GPS y firmas de dispositivos antes de la ingesta.",
                                 "<b>Active De-identification:</b> Mandatory removal of metadata from GPS tracks and device signatures prior to ingestion."))),
              tags$li(HTML(trans("<b>Acceso Abierto Responsable:</b> Los resultados se depositan en repositorios p\u00fablicos como Zenodo bajo licencias no-comerciales.",
                                 "<b>Responsible Open Access:</b> Results deposited in public repositories like Zenodo under non-commercial licenses.")))
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$go_to_sim_from_home, {
    updateTabsetPanel(session, "nav_active", selected = "Centro de Simulaci\u00f3n")
  })
}
