# modules/translations.R
# Dictionary and translations helper functions

# Dictionary of terms (can be expanded here to clean up UI strings)
tr_db <- list(
  control_territorio = list(es = "Control del Territorio", en = "Territorial Control"),
  sec1_title = list(es = "1. Configuraci\u00f3n de Escenario", en = "1. Scenario Configuration"),
  sec2_title = list(es = "2. Eventos y Distorsiones", en = "2. Events & Distortions"),
  sec3_title = list(es = "3. Campos de Fuerza Globales", en = "3. Global Force Fields"),
  sec4_title = list(es = "4. Geometr\u00eda y Solucionador", en = "4. Geometry & Solver"),
  sec5_title = list(es = "5. Cargar Datos Externos (Zenodo)", en = "5. Load External Data (Zenodo)")
)

get_tr <- function(key, lang = "ES") {
  lang_lower <- tolower(lang)
  val <- tr_db[[key]][[lang_lower]]
  if (is.null(val)) return(key)
  return(val)
}
