# Manifest / Manifiesto del Bundle Zenodo

Este archivo describe la estructura de los directorios y componentes principales provistos en el Zenodo Scientific Bundle de **Puerto Umbral**.

This file describes the directory structure and main components provided in the **Puerto Umbral** Zenodo Scientific Bundle.

---

## Estructura de Directorios / Directory Structure

### `/app`
*   `app.R`: Orquestador principal e interfaz de usuario de la plataforma R/Shiny.
*   `global.R`: Carga de datos, regularización de covarianza de Ledoit-Wolf, resolvedores físicos (geodésicas BVP y memoria Caputo), traducciones e inicializaciones globales.
*   `geotensor_experimentos.db`: Base de datos relacional SQLite que almacena los registros experimentales y simulados de los escenarios de campo.
*   `/modules`: Directorio de sub-módulos Shiny estructurados por pestañas:
    *   `ergo_drawer.R`: Utilidades de interfaz táctil y ergonomía de terreno.
    *   `sim_server.R`: Motor y lógica del servidor para el Centro de Simulación y resolvedor geodésico.
    *   `tab0_home.R`: Página de inicio y panel de navegación principal.
    *   `tab1_onboarding.R`: Módulo de inducción y matriz de armonización teórica.
    *   `tab2_experimentos.R`: Ingesta de datos de terreno y auditoría estadística.
    *   `tab3_simulacion.R` & `tab3_ui_definition.R`: Visualizadores del simulador 2D/3D y layouts.
    *   `tab4_matematica.R`: Curvas de Lyapunov, autovalores del Jacobiano y conmutadores de Lie.
    *   `tab5_biblioteca.R`: Biblioteca interactiva y chat inteligente de consulta cognitivo.
    *   `translations.R`: Módulo de internacionalización (ES/EN).
*   `/scripts`:
    *   `pre_train_agents.R`: Script en R para simular recorridos geodésicos aleatorios y entrenar la memoria de trauma de los agentes.
*   `/www`: Recursos estáticos de la interfaz:
    *   `styles.css` & `field_mode.css`: Hojas de estilo para visualización de escritorio y modo de alto contraste de terreno.
    *   `/data`: Archivos de datos y cachés, como la matriz de memoria de trauma `pre_trained_memory.json`, `qa_cache.json` y los textos indizados `tomo_i_text.json` y `tomo_ii_text.json`.
    *   `/docs`: Manuscritos oficiales en PDF (`tomo_i.pdf` y `tomo_ii.pdf`).
    *   `/images`: Esquemas conceptuales y diagramas didácticos explicativos.
    *   `/video`: Directorio destinado a alojar el video demostrativo (`tutorial.mp4`).

### `/scripts`
*   `generar_experimentos_db.py`: Script en Python para inicializar y poblar la base SQLite `geotensor_experimentos.db` con datos simulados y coeficientes base.
*   `consolidar_memorias.py`: Script en Python para consolidar las Cápsulas de Memoria (JSON) exportadas en el chat y enriquecer el caché local de preguntas y respuestas (`qa_cache.json`).

### `/docs`
*   `manual_usuario_puerto_umbral.md` & `manual_usuario_puerto_umbral.docx`: Manual de usuario paso a paso y guía de operación en formatos Markdown y Word.
*   `video_podcast_metadata.md`: Guión, notas y metadatos asociados al video demostrativo y podcast de la obra.

### Archivos de Alineación de IA y Gobernanza Semántica / AI Alignment & Semantic Governance
*   `llms.txt`: Umbral breve y portal de navegación semántica de alta densidad conceptual para modelos de lenguaje (LLMs).
*   `llms-full.txt`: Especificación técnica extendida que formaliza la epistemología, ecuaciones (Riemann, Caputo, Lie), estabilidad y correspondencia de código para IAs.
*   `context.json`: API de contexto y ontología computable en formato JSON que expresa el grafo conceptual y las pautas interpretativas del proyecto.
*   `.well-known/llms.txt`: Copia de alineación en ruta estandarizada internacional para agentes rastreadores web.
*   `MANUAL_CONSOLIDACION_MEMORIAS.txt`: Manual operativo de paso a paso para el administrador sobre cómo consolidar donaciones del chat y del simulador.
*   `robots.txt`: Archivo de enrutamiento y permisos para crawlers y agentes rastreadores web.
*   `sitemap.xml`: Mapa de indexación automatizada para motores de búsqueda semántica y científica.

