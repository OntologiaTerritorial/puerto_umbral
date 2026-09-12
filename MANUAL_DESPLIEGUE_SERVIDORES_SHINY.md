# Manual Maestro de Alojamiento y Despliegue en Servidores Shiny
## Puerto Umbral: Plataforma de Ontología Territorial y Geotensores (Tomo II)
**Centro de Inteligencia Territorial (CIT), Universidad Adolfo Ibáñez**  
**Autor:** John Treimun Ríos (`john.treimun.r@uai.cl`)

---

## 1. Visión y Arquitectura de "Respiración Propia"

Puerto Umbral implementa computacionalmente la **Física Intrínseca Territorial** desarrollada a lo largo del Tomo II de *Ontología Territorial*. 

Para que la plataforma posea **respiración propia** —operatividad continua, autonomía de cálculo y vinculación orgánica con las comunidades—, el sistema articula dos pulmones complementarios bajo la tecnología de **Servidores Shiny**:

1. **El Pulmón de Gabinete (Visualización y Simulación Compleja):**  
   Aplicación interactiva multi-modular en R Shiny (`app.R`, `global.R`, `modules/`), con resolvedor geodésico de mínima fricción (`deSolve`), análisis espectral de matrices de Riemann, decaimiento no markoviano de Caputo y monitoreo de estabilidad de Lyapunov.
2. **El Pulmón de Terreno (Levantamiento Situado y Escucha Ontológica):**  
   Herramientas móviles ultraligeras (`mobile.html` y `fibe_terreno.html`) servidas directamente desde la carpeta `www/` de la aplicación Shiny. Operan en cualquier celular o tablet con **cero dependencia de servidores en el momento del levantamiento** (persistencia en `localStorage`), GPS con semáforo de precisión y exportación de fichas IEO y FIBE listas para ser contrastadas en la aplicación Shiny.

```
+-------------------------------------------------------------------------------+
|                       SERVIDOR SHINY (shinyapps.io / Docker)                  |
|                                                                               |
|   +-----------------------------------------------------------------------+   |
|   |                      R Shiny Engine (app.R / global.R)                |   |
|   |  - 10 Líneas de Experimentos Físicos                                  |   |
|   |  - Resolvedor Geodésico Wu Wei                                       |   |
|   |  - Auditor Comunitario CARE/FAIR                                      |   |
|   +-----------------------------------+-----------------------------------+   |
|                                       |                                       |
|                  +--------------------+--------------------+                  |
|                  |                                         |                  |
|        +---------v---------+                     +---------v---------+        |
|        | Base SQLite SSOT  |                     |  Directorio www/  |        |
|        | 10 Experimentos   |                     | mobile.html (IEO) |        |
|        | 70.096 POPs RMS   |                     | fibe_terreno.html |        |
|        +-------------------+                     +---------+---------+        |
+------------------------------------------------------------|------------------+
                                                             |
                                           Acceso Celular    | (HTTP / PWA)
                                           vía Navegador Móvil
                                                             v
                                            +---------------------------------+
                                            |       TRABAJO DE CAMPO          |
                                            |  - GPS Wu Wei (Lat/Lon)         |
                                            |  - Ficha IEO & FIBE Municipal   |
                                            |  - Persistencia Offline         |
                                            |  - Descarga CSV -> Carga Shiny  |
                                            +---------------------------------+
```

---

## 2. Alojamiento en la Nube con ShinyApps.io (Posit Cloud)

**shinyapps.io** es la plataforma PaaS administrada por Posit optimizada para alojar aplicaciones Shiny con alta disponibilidad, certificados SSL automáticos y escalamiento sin necesidad de administrar servidores Linux.

### Paso 1: Requisitos Previos y Cuenta
1. Regístrese gratuitamente o inicie sesión en [https://www.shinyapps.io/](https://www.shinyapps.io/).
2. En el panel lateral, diríjase a **Account** -> **Tokens**.
3. Haga clic en **Show** y luego en **Show Secret** para copiar el comando de autorización completo:
   ```r
   rsconnect::setAccountInfo(
     name   = "<SU_NOMBRE_DE_CUENTA>",
     token  = "<SU_TOKEN>",
     secret = "<SU_SECRET>"
   )
   ```

### Paso 2: Configuración del Entorno Local
En su consola de R (o RStudio), instale el cliente de despliegue oficial:
```r
install.packages("rsconnect", repos = "https://cloud.r-project.org/")
library(rsconnect)
```
Pegue y ejecute su comando `setAccountInfo(...)`. Con esto, su equipo queda vinculado de manera segura con su cuenta de shinyapps.io.

### Paso 3: Despliegue Automatizado con un Clic
Puerto Umbral incluye el script de automatización `scripts/deploy_shinyapps.R`.

Para desplegar, ejecute desde la consola de R:
```r
source("scripts/deploy_shinyapps.R")
```
O desde la terminal del sistema operativo (bash o PowerShell):
```bash
Rscript scripts/deploy_shinyapps.R
```

El script ejecuta internamente:
```r
rsconnect::deployApp(
  appDir        = "app",
  appFiles      = c("app.R", "global.R", "geotensor_experimentos.db", 
                    "mobile.html", "fibe_terreno.html", "modules", "scripts", "www"),
  appName       = "puerto-umbral",
  appTitle      = "Puerto Umbral - Ontologia Territorial Tomo II",
  appPrimaryDoc = "app.R",
  logLevel      = "verbose",
  forceUpdate   = TRUE
)
```

### Paso 4: Optimización de Recursos en el Panel de shinyapps.io
1. Ingrese a [https://www.shinyapps.io/](https://www.shinyapps.io/) -> **Applications** -> **puerto-umbral** -> **Settings**.
2. **Instance Size:** Seleccione **Large (1 GB)** o **X-Large (2 GB)** para garantizar que la integración numérica de geodésicas (`deSolve`) y las operaciones tensoriales operen con máxima fluidez.
3. **Instance Idle Timeout:** Ajuste a **15 o 30 minutos** para balancear el consumo de horas de cómputo con la disponibilidad para los censistas de campo.
4. **Max Worker Processes:** Configure en **1 o 2**.

### Paso 5: Enlaces Operativos Resultantes
Una vez finalizado el despliegue, su plataforma estará disponible en internet:
- **Plataforma Principal (Gabinete Desktop):**  
  `https://<SU_CUENTA>.shinyapps.io/puerto-umbral/`
- **Módulo Terreno Móvil (Rúbrica IEO + GPS Wu Wei):**  
  `https://<SU_CUENTA>.shinyapps.io/puerto-umbral/mobile.html`
- **Ficha Básica de Emergencia (FIBE Municipal Terreno):**  
  `https://<SU_CUENTA>.shinyapps.io/puerto-umbral/fibe_terreno.html`

---

## 3. Despliegue en Servidor Institucional con Docker (Shiny Server)

Para servidores locales, estaciones de trabajo de campaña o servidores dedicados (GORE RM, CIT-UAI, municipalidades) que requieran autonomía total sin internet o gobernanza de datos soberana.

### Archivos de Despliegue Incluidos
- `Dockerfile`: Construido sobre `rocker/shiny:latest` con todas las bibliotecas de sistema C++ (GDAL, GEOS, PROJ, SQLite3) y paquetes R necesarios.
- `docker-compose.yml`: Orquestador con mapeo de puertos y volúmenes persistentes.
- `shiny-server.conf`: Configuración con tiempos de inactividad ampliados y sanitización condicional.

### Comandos de Ejecución
Desde la raíz del bundle o proyecto:
```bash
# 1. Construir y levantar el contenedor en segundo plano
docker compose up -d

# 2. Verificar el estado del servicio y los logs en tiempo real
docker compose logs -f

# 3. Acceder localmente en el navegador
# http://localhost:3838/
```

### Acceso desde Dispositivos Móviles en la Misma Red Wi-Fi
Si el servidor o laptop de campaña está conectado a una red Wi-Fi local (o un router portátil sin internet en terreno):
1. Obtenga la IP local de la máquina anfitriona (ejemplo: `192.168.1.50`).
2. Desde cualquier celular conectado a esa red, abra en el navegador:
   - `http://192.168.1.50:3838/` (Plataforma completa)
   - `http://192.168.1.50:3838/mobile.html` (Terreno IEO)
   - `http://192.168.1.50:3838/fibe_terreno.html` (Terreno FIBE)

---

## 4. Despliegue Serverless con WebAssembly (Shinylive en GitHub Pages)

Para difusión científica de acceso abierto, docencia universitaria y reproducibilidad universal sin costos de infraestructura.

Puerto Umbral está adaptado para compilar a **R-WebAssembly (Shinylive)**:
```r
# En consola de R:
source("app/scripts/compile_shinylive.R")
```
El script exporta la versión Wasm estática a la carpeta `/docs`. Al subir esta carpeta a GitHub con GitHub Pages habilitado, la aplicación corre 100% en el navegador del visitante sin necesidad de ningún servidor activo.

---

## 5. Protocolo de Trabajo de Campo (Flujo Terreno -> Gabinete)

El circuito operativo que garantiza que el modelo teórico se nutra empíricamente del territorio se estructura en 4 pasos:

### Paso A: Despliegue en Terreno
El encuestador, censista o dirigente vecinal abre en su celular:
`https://<cuenta>.shinyapps.io/puerto-umbral/mobile.html`

### Paso B: Captura Offline
1. **Pestaña 📝 Ficha IEO:**  
   Ingresa el ID de la Manzana censal (POP), evalúa en escala de 1 a 5 la fricción peatonal, barreras físicas y cohesión vecinal usando el teclado numérico defensivo.
2. **Pestaña 🛰️ GPS Wu Wei:**  
   Inicia el trazador de trayectoria peatonal. El semáforo visual garantiza precisión métrica (🟢 < 10m).
3. **Pestaña 💾 Datos:**  
   Verifica los registros guardados en la memoria del celular (`localStorage`). No se pierde ningún dato si se apaga el teléfono o se pierde la señal.

### Paso C: Exportación de Campaña
Al finalizar el recorrido, el usuario presiona **Descargar CSV Fichas IEO** y **Descargar Trayectos (CSV / GeoJSON)** directamente a su teléfono o lo envía por correo/WhatsApp.

### Paso D: Carga y Validación en Shiny
1. En el servidor Shiny (`https://<cuenta>.shinyapps.io/puerto-umbral/`), el equipo de gabinete ingresa a la pestaña **Líneas de Trabajo**.
2. En la columna derecha (**Validación Analítica Post-Terreno**), carga el archivo CSV de terreno mediante el botón **Cargar Campaña de Terreno (CSV)**.
3. **Contraste Inmediato:**  
   El motor gráfico contrasta la fricción observada en la calle con la calculada por el tensor métrico $g_{ij}$, reportando el $R^2$, el p-valor y el ajuste del modelo.
4. **Auditoría CARE/FAIR:**  
   El usuario puede ejecutar el **Agente Auditor Vecinal** para verificar que los datos cumplan con los principios de soberanía y no estigmatización comunitaria.

---

## 6. Correspondencia de las 10 Líneas de Trabajo en Puerto Umbral

| N° | Línea de Trabajo | Fenómeno Físico / Ontológico | Módulo en Shiny |
|---|---|---|---|
| **1** | Refracción de Borde | Ley de Snell socio-espacial ($\sin 	heta_1 / \sin 	heta_2 = \eta_2 / \eta_1$) | `tab2_experimentos.R` / `sim_server.R` |
| **2** | Desviación Geodésica | Ecuación de Jacobi y segregación urbana ($D^2 J^i / ds^2 + R^i_{jkl} T^j T^k J^l = 0$) | `tab2_experimentos.R` / `sim_server.R` |
| **3** | Autopoiesis Colectiva | Inversión Hessiana por presión comunitaria ($H_{ij} < 0$ para $P \ge P_{	ext{crit}}$) | `tab2_experimentos.R` / `sim_server.R` |
| **4** | Memoria del Trauma | Derivada fraccionaria no markoviana de Caputo (${}^C D^lpha V(t) = -\omega V(t)$) | `tab2_experimentos.R` / `global.R` |
| **5** | Métrica de Moran | Regularización de Ledoit-Wolf bajo baja densidad muestral ($\Sigma_{LW}$) | `tab2_experimentos.R` / `sim_server.R` |
| **6** | Fronteras Ecológicas | Condiciones de borde de Robin en Quebrada de Macul ($lpha V + eta \partial_n V = \gamma$) | `tab2_experimentos.R` / `sim_server.R` |
| **7** | Refracción de Capital | Especulación del suelo y despojo de plusvalía periurbana ($g_{ij} 	o R_{	ext{ratio}} g_{ij}$) | `tab2_experimentos.R` / `sim_server.R` |
| **8** | Deformación MBHT 4D | Varianza interdimensional SUBDERE ($T_{\mu
u} = 	ext{diag}(D_{	ext{amb}}, D_{	ext{seg}}, D_{	ext{soc}}, D_{	ext{acc}})$) | `tab2_experimentos.R` / `sim_server.R` |
| **9** | Anillos de Einstein RMS | Lentes gravitacionales y Macro-Anillo de Vespucio ($	heta_E = \sqrt{4 G M_{	ext{onto}} D_{ls} / c^2 D_s}$) | `tab2_experimentos.R` / `sim_server.R` |
| **10** | Escenarios SUT-2050 | 97 hotspots de duelo territorial y simulación prospectiva ($\Lambda(p) = \int K \lVert 
abla \Phi Vert$) | `tab2_experimentos.R` / `sim_server.R` |

---

## 7. Verificación de Salud y Diagnóstico Rápido

Para comprobar que el entorno Shiny local está listo antes de desplegar:
```r
# Verificar paquetes requeridos
pkgs <- c("shiny", "shinyjs", "leaflet", "plotly", "deSolve", "RSQLite", "sf", "rsconnect")
faltantes <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(faltantes) == 0) {
  cat("✓ Todas las dependencias R estan instaladas y listas para desplegar.\n")
} else {
  cat("✗ Faltan paquetes:", paste(faltantes, collapse = ", "), "\n")
  cat("Ejecute: install.packages(c(" , paste(sprintf("'%s'", faltantes), collapse = ", "), "))\n")
}
```

---
*Puerto Umbral v5.4 — Centro de Inteligencia Territorial (CIT-UAI) — 2026*
