# scripts/compile_shinylive.R
# Script de automatizacion para compilar Puerto Umbral a WebAssembly (Shinylive)
# Permite el despliegue serverless y offline de la app en GitHub Pages.

cat("========================================================================\n")
cat("PUERTO UMBRAL: COMPILADOR WEBBERLESS (SHINYLIVE)\n")
cat("========================================================================\n")

# Verificar dependencias
if (!requireNamespace("shinylive", quietly = TRUE)) {
  cat("El paquete 'shinylive' no esta instalado.\n")
  cat("Para instalarlo, ejecute en su consola de R:\n")
  cat("  install.packages('shinylive')\n")
  quit(status = 1)
}

# Definir directorios
app_dir <- "."
# Si el script se ejecuta fuera de la carpeta 'app', ajustar
if (basename(getwd()) != "app" && dir.exists("puerto_umbral_zenodo_bundle/app")) {
  app_dir <- "puerto_umbral_zenodo_bundle/app"
} else if (basename(getwd()) != "app" && dir.exists("app")) {
  app_dir <- "app"
}

dest_dir <- file.path(dirname(app_dir), "docs")

cat("Directorio de la aplicacion:", normalizePath(app_dir), "\n")
cat("Directorio de salida (GitHub Pages):", normalizePath(dest_dir), "\n\n")

cat("Compilando aplicacion R-Shiny a WebAssembly (Wasm)...\n")
tryCatch({
  shinylive::export(appdir = app_dir, destdir = dest_dir)
  cat("\n========================================================================\n")
  cat("COMPILACION EXITOSA!\n")
  cat("------------------------------------------------------------------------\n")
  cat("1. La aplicacion estatica compilada se guardo en la carpeta 'docs/'.\n")
  cat("2. Para desplegar en GitHub Pages:\n")
  cat("   - Suba la carpeta 'docs/' a su repositorio de GitHub.\n")
  cat("   - En Settings -> Pages de su repo, seleccione la carpeta '/docs' como origen.\n")
  cat("3. Para probar localmente sin internet, ejecute en R:\n")
  cat("   shinylive::run_static(dest_dir)\n")
  cat("========================================================================\n")
}, error = function(e) {
  cat("\nERROR DURANTE LA COMPILACION:\n")
  print(e)
  quit(status = 1)
})
