# modules/tab3_simulacion.R
# Dynamic Simulation Tab UI Wrapper

tab3_ui <- function() {
  tabPanel("Centro de Simulaci\u00f3n",
           uiOutput("tab3_simulator_ui")
  )
}
