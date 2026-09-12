# ==============================================================================
# scripts/deploy_shinyapps.R
# Automatizacion de Despliegue en shinyapps.io / Posit Connect
# Plataforma de Ontologia Territorial y Geotensores (Tomo II)
# ==============================================================================

cat("========================================================================\n")
cat("PUERTO UMBRAL: DESPLIEGUE EN SERVIDORES SHINY (SHINYAPPS.IO)\n")
cat("========================================================================\n\n")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Instalando paquete 'rsconnect'...\n")
  install.packages("rsconnect", repos = "https://cloud.r-project.org/")
}
library(rsconnect)

# Determinar la ruta de la aplicacion
app_dir <- "."
if (basename(getwd()) != "app" && dir.exists("puerto_umbral_zenodo_bundle/app")) {
  app_dir <- "puerto_umbral_zenodo_bundle/app"
} else if (basename(getwd()) != "app" && dir.exists("app")) {
  app_dir <- "app"
}

if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("Error: No se encontro 'app.R' en la ruta especificada: ", normalizePath(app_dir))
}

cat("Directorio de la aplicacion:", normalizePath(app_dir), "\n\n")

# Verificar si existen credenciales en variables de entorno
account_env <- Sys.getenv("SHINYAPPS_ACCOUNT")
token_env   <- Sys.getenv("SHINYAPPS_TOKEN")
secret_env  <- Sys.getenv("SHINYAPPS_SECRET")

if (nzchar(account_env) && nzchar(token_env) && nzchar(secret_env)) {
  cat("Configurando cuenta shinyapps.io desde variables de entorno...\n")
  rsconnect::setAccountInfo(name = account_env, token = token_env, secret = secret_env)
  cat("Cuenta autenticada exitosamente para:", account_env, "\n\n")
} else {
  accounts <- rsconnect::accounts()
  if (is.null(accounts) || nrow(accounts) == 0) {
    cat("AVISO: No se encontraron cuentas configuradas en rsconnect.\n")
    cat("Para vincular su cuenta de shinyapps.io, ejecute primero:\n")
    cat("  rsconnect::setAccountInfo(\n")
    cat("    name   = '<SU_NOMBRE_DE_CUENTA>',\n")
    cat("    token  = '<SU_TOKEN_DE_SHINYAPPS>',\n")
    cat("    secret = '<SU_SECRET_DE_SHINYAPPS>'\n")
    cat("  )\n\n")
  } else {
    cat("Cuentas configuradas disponibles:\n")
    print(accounts[, c("name", "server")])
    cat("\n")
  }
}

# Lista blanca de archivos para optimizar el tamano del bundle (<70 MB)
whitelist_files <- c(
  "app.R",
  "global.R",
  "geotensor_experimentos.db",
  "mobile.html",
  "fibe_terreno.html",
  "modules",
  "scripts",
  "www"
)

cat("Iniciando despliegue hacia shinyapps.io...\n")
cat("Parametros:\n")
cat("  - appName:     puerto-umbral\n")
cat("  - appTitle:    Puerto Umbral - Ontologia Territorial Tomo II\n")
cat("  - logLevel:    verbose\n")
cat("  - forceUpdate: TRUE\n\n")

tryCatch({
  rsconnect::deployApp(
    appDir       = app_dir,
    appFiles     = whitelist_files,
    appName      = "puerto-umbral",
    appTitle     = "Puerto Umbral - Ontologia Territorial Tomo II",
    appPrimaryDoc = "app.R",
    logLevel     = "verbose",
    forceUpdate  = TRUE
  )
  cat("\n========================================================================\n")
  cat("DESPLIEGUE EXITOSO EN SHINYAPPS.IO!\n")
  cat("La plataforma se encuentra en linea y con respiracion propia.\n")
  cat("Acceso gabinete: https://<cuenta>.shinyapps.io/puerto-umbral/\n")
  cat("Acceso terreno:  https://<cuenta>.shinyapps.io/puerto-umbral/mobile.html\n")
  cat("Acceso FIBE:     https://<cuenta>.shinyapps.io/puerto-umbral/fibe_terreno.html\n")
  cat("========================================================================\n")
}, error = function(e) {
  cat("\nError o interrupcion durante el despliegue:\n")
  message(e)
  cat("\nSi no configuro sus credenciales, copie y pegue su comando de autorizacion:\n")
  cat("rsconnect::setAccountInfo(name='...', token='...', secret='...')\n")
})

