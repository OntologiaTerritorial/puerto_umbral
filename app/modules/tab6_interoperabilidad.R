# modules/tab6_interoperabilidad.R
# Módulo de Interoperabilidad Geoespacial con IDE Chile (SNIT / OGC / REST API)
# Cumple con la Guía de Interoperabilidad del Estado (SEGPRES) y estándares WMS/WFS

tab6_ui <- function() {
  tabPanel("Interoperabilidad (IDE Chile)",
           uiOutput("tab6_interop_ui")
  )
}

tab6_server <- function(input, output, session, lang) {
  
  trans <- function(es_txt, en_txt) {
    if (identical(lang(), "EN")) en_txt else es_txt
  }
  
  # Live interactive Leaflet map for OGC preview
  output$tab6_map <- renderLeaflet({
    f_einstein <- "www/data/einstein_rings_rms.geojson"
    if (!file.exists(f_einstein)) f_einstein <- "app/www/data/einstein_rings_rms.geojson"
    if (!file.exists(f_einstein)) f_einstein <- "puerto_umbral_zenodo_bundle/app/www/data/einstein_rings_rms.geojson"
    
    f_lat <- "www/data/hotspots_latencia_rms.geojson"
    if (!file.exists(f_lat)) f_lat <- "app/www/data/hotspots_latencia_rms.geojson"
    if (!file.exists(f_lat)) f_lat <- "puerto_umbral_zenodo_bundle/app/www/data/hotspots_latencia_rms.geojson"
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite (Esri)") %>%
      setView(lng = -70.648, lat = -33.456, zoom = 11)
      
    # Layer 1: Einstein Rings
    if (file.exists(f_einstein)) {
      tryCatch({
        d_e <- jsonlite::fromJSON(f_einstein)
        coords_e <- do.call(rbind, d_e$features$geometry$coordinates)
        props_e <- d_e$features$properties
        
        popup_e <- sprintf(
          "<b>%s</b><br><b>ID:</b> %s<br><b>Fricción Relacional:</b> %s<br><b>Altitud:</b> %.1f m<br><b>Red de Cuidado:</b> %s",
          props_e$anillo_einstein, props_e$pixel_id, format(round(props_e$friccion_relacional, 1), big.mark = ","),
          props_e$altitud_m, props_e$red_cuidado
        )
        
        is_ring <- props_e$anillo_einstein == "Macro-Anillo Vespucio"
        colors_e <- ifelse(is_ring, "#d97706", "#0284c7")
        radius_e <- ifelse(is_ring, 7, 4)
        
        m <- m %>% addCircleMarkers(
          lng = coords_e[, 1], lat = coords_e[, 2],
          radius = radius_e, color = colors_e, fillColor = colors_e,
          fillOpacity = 0.75, weight = 1, popup = popup_e,
          group = trans("Anillos de Einstein (Línea 9)", "Einstein Rings (Line 9)")
        )
      }, error = function(e) NULL)
    }
    
    # Layer 2: Latency Hotspots 2050
    if (file.exists(f_lat)) {
      tryCatch({
        d_l <- jsonlite::fromJSON(f_lat)
        coords_l <- do.call(rbind, d_l$features$geometry$coordinates)
        props_l <- d_l$features$properties
        
        popup_l <- sprintf(
          "<b>Hotspot de Duelo Territorial</b><br><b>ID:</b> %s<br><b>Sector:</b> %s<br><b>Latencia (Lambda):</b> %.2f<br><b>Fricción Duelo:</b> %s<br><b>Escenario:</b> %s",
          props_l$pixel_id, props_l$cobertura, props_l$latencia_lambda,
          format(round(props_l$friccion_duelo, 1), big.mark = ","), props_l$escenario
        )
        
        m <- m %>% addCircleMarkers(
          lng = coords_l[, 1], lat = coords_l[, 2],
          radius = 8, color = "#dc2626", fillColor = "#ef4444",
          fillOpacity = 0.85, weight = 2, popup = popup_l,
          group = trans("Hotspots de Duelo SUT-2050 (Línea 10)", "Territorial Grief Hotspots 2050 (Line 10)")
        )
      }, error = function(e) NULL)
    }
    
    # Layer controls
    m <- m %>% addLayersControl(
      baseGroups = c("CartoDB Positron", "OpenStreetMap", "Satélite (Esri)"),
      overlayGroups = c(
        trans("Anillos de Einstein (Línea 9)", "Einstein Rings (Line 9)"),
        trans("Hotspots de Duelo SUT-2050 (Línea 10)", "Territorial Grief Hotspots 2050 (Line 10)")
      ),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    return(m)
  })
  
  output$tab6_interop_ui <- renderUI({
    is_en <- identical(lang(), "EN")
    
    div(class = "container-fluid", style = "padding: 25px; max-width: 1200px; margin: 0 auto;",
      
      # Dynamic Host Update Script
      tags$script(HTML("
        $(document).ready(function() {
          var base = window.location.origin + window.location.pathname.replace(/\\/+$/, '');
          if ($('#wms_url_field').length && window.location.origin.indexOf('http') === 0) {
            $('#wms_url_field').val(base + '/ogc/wms_capabilities.xml');
          }
          if ($('#wfs_url_field').length && window.location.origin.indexOf('http') === 0) {
            $('#wfs_url_field').val(base + '/ogc/wfs_capabilities.xml');
          }
        });
      ")),
      
      # HEADER BANNER
      div(class = "panel-glass", style = "padding: 30px; margin-bottom: 25px; border-left: 5px solid #0d9488; border-radius: 12px;",
        h2(style = "color: #0f766e; font-weight: 700; margin-top: 0;", 
           trans("Interoperabilidad Geoespacial y Servicios OGC (IDE Chile)", 
                 "Geospatial Interoperability & OGC Services (IDE Chile)")),
        p(style = "color: #334155; font-size: 1.05rem; line-height: 1.6; margin-bottom: 0;",
          trans(
            "Puerto Umbral cumple con los estándares del Sistema Nacional de Coordinación de Información Territorial (SNIT / IDE Chile) y las directrices de la División de Gobierno Digital (SEGPRES). Todas las capas de la física intrínseca (Geotensores, Anillos de Einstein, Hotspots de Latencia y Mallas Censales) están disponibles mediante protocolos abiertos para su consumo directo en QGIS, ArcGIS, visores ministeriales y plataformas comunales.",
            "Puerto Umbral complies with the standards of the National Territorial Information Coordination System (SNIT / IDE Chile) and Digital Government guidelines (SEGPRES). All intrinsic physics layers (Geotensors, Einstein Rings, Latency Hotspots, and Census Grids) are exposed via open OGC protocols for direct integration into QGIS, ArcGIS, and ministerial GIS viewers."
          )
        )
      ),
      
      fluidRow(
        # COLUMNA IZQUIERDA: SERVICIOS OGC (WMS / WFS)
        column(6,
          div(class = "panel-glass", style = "padding: 25px; min-height: 520px; border-radius: 12px;",
            h3(style = "color: #0284c7; font-weight: 700; margin-top: 0;", 
               trans("1. Servicios de Mapas y Entidades OGC", "1. OGC Map & Feature Services")),
            p(style = "color: #475569; font-size: 0.95rem; line-height: 1.5;",
              trans("Conecte el catálogo de capas georreferenciadas directamente en su software SIG sin necesidad de descargar archivos estáticos:",
                    "Connect the georeferenced layer catalog directly into your GIS software without manual downloads:")),
            
            # WMS Card
            div(style = "background: rgba(2, 132, 199, 0.05); border: 1px solid rgba(2, 132, 199, 0.2); border-radius: 8px; padding: 15px; margin-bottom: 15px;",
              h4(style = "color: #0369a1; font-weight: 600; margin-top: 0;", "🗺️ OGC WMS 1.3.0 (Web Map Service)"),
              p(style = "font-size: 0.85rem; color: #334155; margin-bottom: 8px;",
                trans("Para visualizar capas renderizadas georreferenciadas en SIRGAS-Chile (EPSG:4674 / EPSG:4326 / EPSG:32719).",
                      "To render georeferenced map layers in SIRGAS-Chile (EPSG:4674 / EPSG:4326 / EPSG:32719).")),
              tags$label(style = "font-size: 0.8rem; font-weight: bold; color: #0369a1;", "URL GetCapabilities WMS:"),
              tags$div(style = "display: flex; gap: 6px;",
                tags$input(type = "text", class = "form-control input-sm", readonly = "readonly",
                           value = "https://ontologiaterritorial.github.io/puerto_umbral/ogc/wms_capabilities.xml",
                           id = "wms_url_field", style = "font-family: monospace; font-size: 0.8rem;"),
                tags$button(class = "btn btn-info btn-sm", onclick = "navigator.clipboard.writeText($('#wms_url_field').val()); alert('URL WMS copiada al portapapeles');", "Copiar")
              )
            ),
            
            # WFS Card
            div(style = "background: rgba(13, 148, 136, 0.05); border: 1px solid rgba(13, 148, 136, 0.2); border-radius: 8px; padding: 15px; margin-bottom: 15px;",
              h4(style = "color: #0f766e; font-weight: 600; margin-top: 0;", "📐 OGC WFS 2.0.0 (Web Feature Service)"),
              p(style = "font-size: 0.85rem; color: #334155; margin-bottom: 8px;",
                trans("Para descargar geometrías vectoriales completas y tablas de atributos en formato GeoJSON o GML 3.2.",
                      "To retrieve raw vector geometries and attribute tables in GeoJSON or GML 3.2 format.")),
              tags$label(style = "font-size: 0.8rem; font-weight: bold; color: #0f766e;", "URL GetCapabilities WFS:"),
              tags$div(style = "display: flex; gap: 6px;",
                tags$input(type = "text", class = "form-control input-sm", readonly = "readonly",
                           value = "https://ontologiaterritorial.github.io/puerto_umbral/ogc/wfs_capabilities.xml",
                           id = "wfs_url_field", style = "font-family: monospace; font-size: 0.8rem;"),
                tags$button(class = "btn btn-success btn-sm", onclick = "navigator.clipboard.writeText($('#wfs_url_field').val()); alert('URL WFS copiada al portapapeles');", "Copiar")
              )
            ),
            
            # QGIS QLR One-Click
            div(style = "background: rgba(16, 185, 129, 0.05); border: 1px solid rgba(16, 185, 129, 0.2); border-radius: 8px; padding: 15px;",
              h4(style = "color: #059669; font-weight: 600; margin-top: 0;", "📥 Conexión Directa QGIS (.qlr)"),
              p(style = "font-size: 0.85rem; color: #334155; margin-bottom: 10px;",
                trans("Descargue el archivo de definición de capas de QGIS y arrástrelo sobre su proyecto para cargar automáticamente los Anillos de Einstein y Hotspots con simbología oficial.",
                      "Download the QGIS layer definition file and drag it into your project to load Einstein Rings and Hotspots with official styling.")),
              tags$a(href = "ogc/puerto_umbral_qgis.qlr", download = "puerto_umbral_qgis.qlr", class = "btn btn-success btn-sm w-100",
                     style = "font-weight: 600; text-align: center;",
                     tagList(icon("download"), trans(" Descargar Definición de Capas QGIS (.qlr)", " Download QGIS Layer Definition (.qlr)")))
            )
          )
        ),
        
        # COLUMNA DERECHA: REST API Y GEOJSON
        column(6,
          div(class = "panel-glass", style = "padding: 25px; min-height: 520px; border-radius: 12px;",
            h3(style = "color: #b45309; font-weight: 700; margin-top: 0;", 
               trans("2. REST API Geoespacial (OpenAPI 3.0)", "2. Geospatial REST API (OpenAPI 3.0)")),
            p(style = "color: #475569; font-size: 0.95rem; line-height: 1.5;",
              trans("Endpoints JSON/GeoJSON para integración en visores web (Leaflet, MapLibre, Cesium) y sistemas de información geográfica municipales:",
                    "JSON/GeoJSON endpoints for integration into web map viewers (Leaflet, MapLibre) and municipal GIS applications:")),
            
            # Endpoint 1: Anillos de Einstein
            div(style = "background: rgba(245, 158, 11, 0.05); border: 1px solid rgba(245, 158, 11, 0.2); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
              tags$span(class = "badge badge-primary", style = "background:#0284c7; font-size:0.75rem;", "GET"),
              tags$b(style = "font-family: monospace; font-size: 0.85rem; margin-left: 8px;", "/api/einstein_rings_rms.geojson"),
              p(style = "font-size: 0.8rem; color: #475569; margin: 4px 0 6px 0;",
                trans("Polígonos de los 6 anillos barriales y Macro-Anillo de Vespucio (radio, masa y deflexión).",
                      "Polygons for the 6 local rings and Vespucio Macro-Ring (radius, mass, and deflection).")),
              tags$a(href = "data/einstein_rings_rms.geojson", target = "_blank", class = "btn btn-default btn-xs", "Ver GeoJSON →")
            ),
            
            # Endpoint 2: Hotspots de Duelo
            div(style = "background: rgba(245, 158, 11, 0.05); border: 1px solid rgba(245, 158, 11, 0.2); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
              tags$span(class = "badge badge-primary", style = "background:#0284c7; font-size:0.75rem;", "GET"),
              tags$b(style = "font-family: monospace; font-size: 0.85rem; margin-left: 8px;", "/api/hotspots_latencia_rms.geojson"),
              p(style = "font-size: 0.8rem; color: #475569; margin: 4px 0 6px 0;",
                trans("97 puntos críticos de duelo territorial no resuelto (Lambda >= 0.70) según Caputo fraccionario.",
                      "97 critical unresolved territorial grief hotspots (Lambda >= 0.70) based on Caputo memory.")),
              tags$a(href = "data/hotspots_latencia_rms.geojson", target = "_blank", class = "btn btn-default btn-xs", "Ver GeoJSON →")
            ),
            
            # Endpoint 3: Resumen Comunal
            div(style = "background: rgba(245, 158, 11, 0.05); border: 1px solid rgba(245, 158, 11, 0.2); border-radius: 8px; padding: 12px; margin-bottom: 15px;",
              tags$span(class = "badge badge-primary", style = "background:#0284c7; font-size:0.75rem;", "GET"),
              tags$b(style = "font-family: monospace; font-size: 0.85rem; margin-left: 8px;", "/api/comunas_rms_perfiles.json"),
              p(style = "font-size: 0.8rem; color: #475569; margin: 4px 0 6px 0;",
                trans("Estadísticas de NTI, fricción media, latencia y centroides de las 52 comunas de la RMS.",
                      "NTI, friction, latency, and centroid statistics across all 52 communes of the RMS.")),
              tags$a(href = "data/comunas_rms_perfiles.json", target = "_blank", class = "btn btn-default btn-xs", "Ver JSON →")
            ),
            
            # OpenAPI Link
            tags$a(href = "ogc/openapi_geotensores.json", target = "_blank", class = "btn btn-warning btn-sm w-100",
                   style = "font-weight: 600; text-align: center;",
                   tagList(icon("code"), trans(" Descargar Especificación OpenAPI 3.0 (JSON)", " Download OpenAPI 3.0 Specification (JSON)")))
          )
        )
      ),
      
      # SECCIÓN DE VALIDACIÓN EN VIVO (VISOR INTERACTIVO LEAFLET OGC)
      div(class = "panel-glass", style = "margin-top: 25px; padding: 25px; border-radius: 12px; border-top: 4px solid #0284c7;",
        h3(style = "color: #0369a1; font-weight: 700; margin-top: 0;",
           tagList(icon("map-marked-alt"), trans(" 3. Visor de Validación en Vivo (OGC / GeoJSON Preview)", " 3. Live Validation Viewer (OGC / GeoJSON Preview)"))),
        p(style = "color: #475569; font-size: 0.95rem; line-height: 1.5; margin-bottom: 15px;",
          trans("Este visor simula en tiempo real la ingesta de las capas OGC/GeoJSON de Puerto Umbral por una plataforma ministerial del Estado de Chile:",
                "This interactive viewer simulates in real time the ingestion of Puerto Umbral OGC/GeoJSON layers by a Chilean State platform:")),
        leafletOutput("tab6_map", height = "500px")
      ),
      
      # SECCIÓN DE CAPAS OFICIALES CONSUMIDAS DEL ESTADO
      div(class = "panel-glass", style = "margin-top: 25px; padding: 25px; border-radius: 12px; background: rgba(15, 23, 42, 0.02);",
        h3(style = "color: #0f172a; font-weight: 700; margin-top: 0;", 
           trans("4. Capas Oficiales del Estado Integradas (Consumo WMS)", "4. Official State Layers Integrated (WMS Ingestion)")),
        p(style = "color: #475569; font-size: 0.95rem; line-height: 1.5;",
          trans("Para asegurar la coherencia multiescalar, el motor de Puerto Umbral contrasta los geotensores con los servicios WMS oficiales del Estado de Chile:",
                "To ensure multiscale consistency, the Puerto Umbral engine contrasts geotensors against official Chilean State WMS services:")),
        fluidRow(
          column(4,
            div(style = "background: white; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
              tags$b("🏛️ IDE Minvu / PRMS:"),
              tags$p(style = "font-size: 0.8rem; color: #64748b; margin-top: 4px;", "Límites urbanos intercomunales, campamentos y zonificación.")
            )
          ),
          column(4,
            div(style = "background: white; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
              tags$b("🌿 IDE MMA / Santuarios:"),
              tags$p(style = "font-size: 0.8rem; color: #64748b; margin-top: 4px;", "Áreas bajo protección oficial, humedales y santuarios naturales.")
            )
          ),
          column(4,
            div(style = "background: white; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
              tags$b("📊 INE / Censo 2024:"),
              tags$p(style = "font-size: 0.8rem; color: #64748b; margin-top: 4px;", "Manzanas censales oficiales y unidades territoriales de base.")
            )
          )
        )
      )
    )
  })
}
