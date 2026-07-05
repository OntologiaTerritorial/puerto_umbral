# modules/tab4_matematica.R
# Module for Mathematical Analysis Tab (Bilingually separated, Plotly integrated, escaped unicode)

tab4_ui <- function() {
  tabPanel("An\u00e1lisis Matem\u00e1tico",
           uiOutput("tab4_portal_ui")
  )
}

tab4_server <- function(input, output, session, lang) {
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  output$tab4_portal_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    
    fluidPage(
      fluidRow(
        # Left column: HUD and local parameter explanation
        column(5,
          div(class = "panel-glass hud-glass", style = "margin-bottom: 20px; padding: 20px;",
            h3(style = "color:#0284c7; font-weight:600; margin-top:0;", 
               title = trans("Muestra los atributos detallados, autovalores m\u00e9tricos y niveles de cohesi\u00f3n del p\u00edxel ontol\u00f3gico seleccionado.",
                             "Displays detailed attributes, metric eigenvalues, and cohesion levels of the selected ontological pixel."),
               trans("HUD de P\u00edxel Ontol\u00f3gico", "Ontological Pixel HUD")),
            uiOutput("pixel_fiche_ui"),
            tags$p(style = "font-size:0.85rem; color:#475569; margin-top:10px; font-style:italic;", 
                   trans("Nota: Seleccione el modo de clic 'Ver Ficha' en el Mapa 2D del Centro de Simulaci\u00f3n y haga clic sobre una manzana para cargar aqu\u00ed sus autovalores y capa de cohesi\u00f3n.",
                         "Note: Set click mode to 'View Fiche' on the 2D Map in the Simulation Center and click a block to load its eigenvalues and cohesion layer here."))
          ),
          div(class = "panel-glass hud-glass", style = "padding: 20px;",
            h3(style = "color:#0d9488; font-weight:600; margin-top:0;", 
               title = trans("Resumen de m\u00e9tricas del resolvedor: fricci\u00f3n intr\u00ednseca, torsi\u00f3n de geod\u00e9sica, amortiguamiento y regularizaci\u00f3n de covarianza.",
                             "Summary of solver metrics: intrinsic friction, geodesic torsion, damping, and covariance regularization."),
               trans("M\u00e9tricas del Solucionador & Regularizaci\u00f3n", "Solver Metrics & Regularization")),
            fluidRow(style = "margin-bottom: 15px;",
              column(4, div(class = "card-kpi", div(class = "kpi-title", trans("Fricci\u00f3n", "Friction")), uiOutput("kpi_fric"), div(class = "kpi-unit", "||v||_g"))),
              column(4, div(class = "card-kpi", div(class = "kpi-title", trans("Torsi\u00f3n", "Torsion")), uiOutput("kpi_tors"), div(class = "kpi-unit", "rad"))),
              column(4, div(class = "card-kpi", div(class = "kpi-title", trans("Eficiencia", "Efficiency")), uiOutput("kpi_eff"), div(class = "kpi-unit", "WuWei/L_e")))
            ),
            checkboxInput("use_lw", trans("Activar Regularizaci\u00f3n Ledoit-Wolf", "Enable Ledoit-Wolf Regularization Map"), value = TRUE),
            uiOutput("bvp_metrics_card"),
            uiOutput("costo_vital_box"),
            uiOutput("snell_diagnostic_panel"),
            uiOutput("poetic_ontological_report"),
            uiOutput("ai_counselors"),
            uiOutput("ontological_narrator"),
            downloadButton("download_math_report", trans("Descargar Reporte Matem\u00e1tico (TXT)", "Download Math Report (TXT)"), class = "btn-info w-100", style = "margin-top: 15px; font-weight:600;")
          )
        ),
        
        # Right column: Mathematical graphs (Plotly)
        column(7,
          div(class = "panel-glass", style = "padding:20px;",
            tabsetPanel(id = "math_plots_tabs",
              tabPanel(trans("Conmutador de Lie [X, Y]", "Lie Commutator [X, Y]"),
                div(style = "margin-top: 15px;",
                  plotlyOutput("plotly_lie_mesh", height = "380px"),
                  uiOutput("lie_interpretation_text"),
                  tags$p(style = "margin-top: 12px; font-size:0.85rem; color:#475569; line-height: 1.5;",
                         trans("El \u00c1lgebra de Lie en el Tomo II formaliza c\u00f3mo el orden temporal de las intervenciones p\u00fablicas altera el resultado territorial. La asimetr\u00eda neta [X, Y] (conmutador) indica que si el capital (X) ingresa antes de la red de cuidado (Y), el espacio se fractura permanentemente. Si la red de cuidado (Y) se establece primero, act\u00faa como un escudo protector.",
                               "The Lie Algebra in Volume II formalizes how the temporal order of public interventions alters the territorial result. The net asymmetry [X, Y] (commutator) indicates that if capital investment (X) enters before the care network (Y), the space remains permanently fractured. If the care network (Y) is established first, it acts as a protective shield."))
                )
              ),
              
              tabPanel(trans("Estabilidad y Autovalores", "Stability & Eigenvalues"),
                div(style = "margin-top: 15px;",
                  plotlyOutput("jacobian_stability_plot", height = "380px"),
                  uiOutput("jacobian_interpretation_text"),
                  tags$p(style = "margin-top: 12px; font-size:0.85rem; color:#475569; line-height: 1.5;",
                         trans("Los autovalores complejos del Jacobiano eval\u00faan la estabilidad local del EQT del p\u00edxel seleccionado. Los polos en la regi\u00f3n roja (Re\u03bb > 0) representan zonas inestables (gentrificadoras o de peligro) donde el habitante pierde el control de su trayectoria. La regi\u00f3n verde (Re\u03bb < 0) indica estabilidad territorial.",
                               "The complex eigenvalues of the Jacobian assess the local stability of the selected pixel's EQT. Poles in the red region (Re\u03bb > 0) represent unstable zones (gentrifying or dangerous) where the walker loses trajectory control. The green region (Re\u03bb < 0) indicates territorial stability."))
                )
              ),
              
              tabPanel(trans("Decaimiento de Lyapunov", "Lyapunov Decay"),
                div(style = "margin-top: 15px;",
                  plotlyOutput("lyapunov_decay_plot", height = "380px"),
                  uiOutput("lyapunov_interpretation_text"),
                  tags$p(style = "margin-top: 12px; font-size:0.85rem; color:#475569; line-height: 1.5;",
                         trans("La funci\u00f3n de Lyapunov modela la disipaci\u00f3n de la energ\u00eda vital bajo el amortiguamiento territorial. Aumentar el factor de amortiguamiento (\u03a9) drena la energ\u00eda cin\u00e9tica del peat\u00f3n, simulando la fatiga social y la inercia que lo atrapa en las deformaciones del manifold urbano.",
                               "The Lyapunov function models the dissipation of vital energy under territorial damping. Increasing the damping factor (\u03a9) drains the walker's kinetic energy, simulating social fatigue and inertia trapping them inside urban manifold deformations."))
                )
              )
            )
          )
        )
      ),
      
      # Collapsible Math Explanatory Glossary at the bottom
      fluidRow(style = "margin-top: 20px;",
        column(12,
          div(class = "panel-glass", style = "padding:25px;",
            h3(style = "color:#b45309; font-weight:600; margin-top:0; margin-bottom:15px; border-bottom:1px solid rgba(255,255,255,0.08); padding-bottom:8px;",
               title = trans("Consulta el compendio de f\u00f3rmulas f\u00edsicas y operadores matem\u00e1ticos fundamentales del Tomo II.",
                             "Consult the compendia of physical formulas and fundamental mathematical operators from Volume II."),
               trans("Glosario Matem\u00e1tico de la F\u00edsica Territorial (Tomo II)", "Mathematical Glossary of Territorial Physics (Volume II)")),
            
            fluidRow(
              column(4,
                div(style = "background:rgba(255,255,255,0.05); border:1px solid rgba(0,0,0,0.08); padding:15px; border-radius:6px; min-height:220px; display: flex; flex-direction: column; justify-content: space-between;",
                  div(
                    h4(style = "color:#0284c7; margin-top:0;", trans("M\u00e9trica del Manifold", "Manifold Metric")),
                    HTML("$$g_{ij} = \\eta_{ij} + \\lambda \\cdot \\partial_i V \\partial_j V$$"),
                    tags$p(style = "font-size:0.8rem; color:#334155; margin-top:8px;",
                           trans("Deforma la distancia euclidiana base incorporando las gradientes del potencial socio-ecol\u00f3gico (V). A mayor gradiente, mayor es la dilataci\u00f3n del espacio de transporte.",
                                 "Deforms the base Euclidean distance by incorporating the gradients of the socio-ecological potential (V). A higher gradient yields greater expansion of the transport space."))
                  )
                )
              ),
              column(4,
                div(style = "background:rgba(255,255,255,0.05); border:1px solid rgba(0,0,0,0.08); padding:15px; border-radius:6px; min-height:220px; display: flex; flex-direction: column; justify-content: space-between;",
                  div(
                    h4(style = "color:#0284c7; margin-top:0;", trans("Ecuaci\u00f3n Geod\u00e9sica", "Geodesic Equation")),
                    HTML("$$\\frac{d^2 x^k}{ds^2} + \\Gamma^k_{ij} \\frac{dx^i}{ds} \\frac{dx^j}{ds} = 0$$"),
                    tags$p(style = "font-size:0.8rem; color:#334155; margin-top:8px;",
                           trans("Describe la trayectoria de menor fricci\u00f3n de los peatones (Wu Wei). Los s\u00edmbolos de Christoffel representan la curvatura inducida por las tensiones territoriales.",
                                 "Describes the path of least friction for pedestrians (Wu Wei). The Christoffel symbols represent the curvature induced by territorial tensions."))
                  )
                )
              ),
              column(4,
                div(style = "background:rgba(255,255,255,0.05); border:1px solid rgba(0,0,0,0.08); padding:15px; border-radius:6px; min-height:220px; display: flex; flex-direction: column; justify-content: space-between;",
                  div(
                    h4(style = "color:#0284c7; margin-top:0;", trans("Conmutador de Lie", "Lie Commutator")),
                    HTML("$$[X, Y]^k = X^i \\partial_i Y^k - Y^i \\partial_i X^k$$"),
                    tags$p(style = "font-size:0.8rem; color:#334155; margin-top:8px;",
                           trans("Mide la no-conmutatividad del espacio territorial. La asimetr\u00eda resultante representa c\u00f3mo el orden en que se ejecutan los planes altera irreversiblemente la topolog\u00eda.",
                                 "Measures the non-commutativity of the territorial space. The resulting asymmetry represents how the execution order of policies irreversibly alters the topology."))
                  )
                )
              )
            )
          )
        )
      )
    )
  })
}
