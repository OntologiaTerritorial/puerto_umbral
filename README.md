# Puerto Umbral: Zenodo Scientific Bundle  
## Plataforma base de Ontología Territorial y Geotensores — Tomo II

🌐 **Plataforma en vivo / Live App:** [https://ontologiaterritorial.github.io/puerto_umbral/](https://ontologiaterritorial.github.io/puerto_umbral/)

**Versión de la app:** Puerto Umbral v5.4  
**Versión del bundle Zenodo:** v1.0  

Este repositorio auto-contenido alberga la aplicación interactiva, el motor de simulación, la base SQLite de experimentos, los scripts de generación de datos y el material pedagógico-divulgativo correspondiente al **Tomo II de *Ontología Territorial***.

Puerto Umbral implementa computacionalmente la física intrínseca territorial desarrollada en la obra, articulando simulación geodésica, análisis matemático, pedagogía territorial, consulta documental, carga de materiales audiovisuales y protocolos de validación empírica.

This self-contained repository contains the interactive application, simulation engine, SQLite experiment database, data-generation scripts, and pedagogical-dissemination materials for **Volume II of *Territorial Ontology***.

Puerto Umbral computationally implements the intrinsic territorial physics developed in the work, integrating geodesic simulation, mathematical analysis, territorial pedagogy, document-based consultation, audiovisual material support, and empirical validation protocols.

---

## Estructura del Bundle / Bundle Structure

- `/app`: código fuente de la aplicación Shiny (`app.R`, `global.R`, módulos en `/modules`, recursos estáticos en `/www`) e insumos operativos.
- `/scripts`: scripts de generación y reconstrucción de la base experimental, incluyendo `generar_experimentos_db.py`.
- `/docs`: guías de usuario, manuales operativos de terreno, plantillas IEO, metadatos de difusión y manuscritos PDF de la obra.
- `/data` o archivos incluidos: datos simulados, archivos JSON, insumos de prueba y base SQLite `geotensor_experimentos.db`, según la versión del bundle.

---

## Correspondencia teoría–app / Theory–App Correspondence

Puerto Umbral traduce a componentes computacionales las principales estructuras formales del Tomo II:

- **Espacio de Calidad Territorial (EQT):** construcción de superficies territoriales y geometrías de calidad.
- **Métrica intrínseca y símbolos de Christoffel:** cálculo local de deformación geométrica.
- **Geodésicas tensivas:** resolvedor de trayectorias de mínima fricción territorial.
- **Memoria de Caputo:** modelamiento discreto de persistencia temporal y trauma territorial.
- **Conmutador de Lie:** análisis de no-conmutatividad en secuencias de intervención.
- **Lyapunov:** monitoreo de estabilidad y disipación de energía territorial.
- **Ledoit-Wolf:** regularización de matrices locales bajo baja densidad muestral.
- **Donut Jittering / Langevin:** anonimización geométrica y protección de privacidad.
- **IPF:** calibración demográfica y consistencia multiescalar.
- **Condiciones de Robin y refracción:** simulación de bordes, umbrales y fronteras permeables.

---

## Características principales / Main Features

1. **Interfaz de alta legibilidad para gabinete y terreno:** diseño claro, contraste reforzado y compatibilidad con mapas, tablas y gráficos interactivos.
2. **Matriz de armonización conceptual:** puente interactivo entre conceptos del Tomo I y operadores físico-matemáticos del Tomo II.
3. **Ecosistema de agentes:** agentes pedestres, agente cognitivo de consulta y módulos de exploración territorial.
4. **Centro de simulación:** resolvedor geodésico, campos de potencial, perfiles peatonales, eventos territoriales y mapas conectados 2D/3D.
5. **Análisis matemático:** módulos para conmutadores de Lie, estabilidad, autovalores, decaimiento de Lyapunov y memoria fraccionaria.
6. **Ergonomía de terreno:** modo de alto contraste, recomendaciones de campaña y soporte para condiciones de levantamiento.
7. **Carga, videos y materiales complementarios:** espacio para integrar insumos audiovisuales, materiales pedagógicos y archivos de apoyo.
8. **Carga y regeneración de datos:** base SQLite incluida, scripts de generación y soporte para insumos simulados.
9. **Material pedagógico y documental:** acceso integrado a tomos, guías, plantillas y material de apoyo.

## Aprendizaje Colectivo y Memoria Fluyente / Collective Learning & Flowing Memory

Puerto Umbral está diseñada bajo los principios **CARE** (Soberanía y Beneficio Colectivo de Datos) para garantizar el respeto de los saberes locales. La memoria de la plataforma no se recopila de forma centralizada ni invasiva; sigue fluyendo libremente a través de las donaciones soberanas y el uso de los agentes de software, los cuales crecen con cada ciclo de aporte comunitario:

*   **Memoria Conversacional (Chat):** Las consultas con el Agente de Biblioteca son totalmente locales y privadas. El usuario decide cuándo descargar su cápsula de conversación (JSON) y donarla al repositorio para enriquecer el caché cognitivo del bot (`qa_cache.json`).
*   **Memoria de Geodésicas (Trauma Colectivo):** Las simulaciones de tránsito pedestre fallidas o interrumpidas por barreras del entorno acumulan una huella en la grilla del territorio. Esta matriz de trauma (`pre_trained_memory.json`) puede ser exportada por los usuarios y fusionada colectivamente mediante un operador de máximo local para que futuros agentes hereden este mapa de fricciones históricas de la comunidad.

*La memoria sigue fluyendo y aprendiendo en un ciclo abierto de ciencia ciudadana, a través de los agentes de software que crecen con las donaciones y el habitar de la comunidad.*

---

## Estado de esta versión / Version Status

Esta versión corresponde a una **base funcional reproducible** de Puerto Umbral asociada al Tomo II. Está diseñada como infraestructura inicial abierta para exploración, docencia, validación territorial y extensión comunitaria.

No constituye un producto comercial cerrado, sino una plataforma científica y pedagógica extensible, orientada a la crítica, adaptación y uso situado por investigadores, equipos técnicos y comunidades.

---

## Requisitos / Requirements

La aplicación está escrita en **R/Shiny** y utiliza una base de datos relacional **SQLite** para maximizar portabilidad y auto-contención.

### Software requerido

- R >= 4.0
- RStudio, opcional
- Python 3.8+, solo si se desea regenerar la base SQLite

### Dependencias R

Ejecute en R:

```r
install.packages(c(
  "shiny", "shinyjs", "leaflet", "plotly", "deSolve",
  "RSQLite", "DBI", "ggplot2", "htmltools", "shinythemes",
  "jsonlite", "dplyr", "sf", "RColorBrewer", "scales",
  "fields", "akima"
))
```

*Nota:* El paquete `sf` puede requerir dependencias geoespaciales del sistema operativo. Se recomienda instalarlo previamente y verificar su carga con `library(sf)`.

---

## Ejecución / Running the Application

Desde la raíz del bundle, ejecute:

```r
shiny::runApp("app")
```

Asegúrese de que la base `geotensor_experimentos.db` esté disponible en el directorio esperado por la aplicación.

---

## Regeneración de Datos y Pre-entrenamiento / Data Regeneration & Pre-training

Para reconstruir la base de experimentos SQLite desde los scripts incluidos:

```bash
python scripts/generar_experimentos_db.py
```

Para regenerar la memoria sintética pre-entrenada de los agentes pedestres (matriz de trauma colectivo):

```bash
Rscript app/scripts/pre_train_agents.R
```

El script genera los insumos y la base de datos utilizados por Puerto Umbral.

---

## Validación de campo / Field Validation

El bundle incorpora la metodología de validación territorial asociada al Piloto Magallanes 2026. La aplicación permite contrastar observaciones peatonales levantadas mediante la Rúbrica IEO con el resolvedor de física de geotensores, calculando indicadores como coeficiente de determinación, significación estadística y ajuste entre trayectoria observada y trayectoria simulada.

The bundle incorporates the territorial validation methodology associated with the 2026 Magallanes Pilot. The application enables comparison between pedestrian field observations collected through the IEO Rubric and the geotensor physics solver.

---

## Privacidad y datos / Privacy and Data

El bundle está preparado para trabajar con datos simulados y/o anonimizados. Cualquier uso con datos reales debe respetar principios de privacidad, consentimiento, anonimización geométrica y soberanía comunitaria de datos.

---

## Despliegue en GitHub Pages (Serverless) / GitHub Pages Deployment

Esta plataforma está configurada para ser compilada a WebAssembly e implementada de manera 100% estática y serverless en **GitHub Pages** mediante **Shinylive**:

1. **Compilación Local:** Para compilar manualmente los scripts de R Shiny a WebAssembly y guardarlos en el directorio de GitHub Pages (`/docs`), ejecuta en tu consola de R:
   ```r
   source("app/scripts/compile_shinylive.R")
   ```
2. **Prueba Estática Local:** Para levantar un servidor web local que ejecute la versión WebAssembly estática compilada (sin necesidad de tener R escuchando activamente en el backend), ejecuta:
   ```r
   shinylive::run_static("docs")
   ```
3. **Automatización:** El repositorio incluye un workflow de GitHub Actions que compila y despliega automáticamente la aplicación en la rama `gh-pages` en cada `git push`.

---

## Cita / Citation

Treimun Ríos, J. (2026). *Puerto Umbral: Zenodo Scientific Bundle for Territorial Ontology, Volume II*. Centro de Inteligencia Territorial, Universidad Adolfo Ibáñez.

---

## Licencia / License

Este paquete científico se distribuye bajo licencia **Creative Commons Attribution 4.0 International (CC BY 4.0)**, salvo que algún archivo específico indique otra licencia.

**CIT-UAI / John Treimun Ríos (2026)**