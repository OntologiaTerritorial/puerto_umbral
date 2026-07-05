# Manual de Usuario y Guía de Operación: Puerto Umbral

## Plataforma Computacional de Ontología Territorial y Geotensores (V5.4)

Este manual proporciona una guía paso a paso para la instalación, configuración y operación de **Puerto Umbral**, el resolvedor interactivo de física intrínseca territorial asociado al **Tomo II** de *Ontología Territorial*.

---

## 1. Requisitos del Sistema

Antes de iniciar la aplicación, asegúrese de tener instalado el siguiente software en su computadora:

1.  **R (Versión 4.0 o superior):** Lenguaje de programación base. [Descargar R](https://cran.r-project.org/)
2.  **RStudio (Recomendado):** Entorno de desarrollo para ejecutar la app cómodamente. [Descargar RStudio](https://posit.co/download/rstudio-desktop/)
3.  **Python (Versión 3.8 o superior):** Requerido únicamente si desea regenerar la base de datos experimental desde los scripts de origen.

---

## 2. Instalación de Dependencias de R

Abra R o RStudio y ejecute el siguiente comando en la Consola para instalar todas las dependencias necesarias de la aplicación:

```r
install.packages(c(
  "shiny", "shinyjs", "leaflet", "plotly", "deSolve",
  "RSQLite", "DBI", "ggplot2", "htmltools", "shinythemes",
  "jsonlite", "dplyr", "sf", "RColorBrewer", "scales",
  "fields", "akima"
))
```

*Nota: Asegúrese de tener una conexión a internet activa durante la instalación de estos paquetes.*

---

## 3. Estructura del Bundle de Archivos

El bundle de Puerto Umbral se distribuye con la siguiente estructura de directorios:

*   `/app`: Código fuente principal de la aplicación R/Shiny.
    *   `app.R`: Interfaz de usuario (UI) y servidor.
    *   `global.R`: Configuraciones iniciales y carga de bases de datos.
    *   `geotensor_experimentos.db`: Base de datos SQLite precargada con los experimentos.
    *   `/modules`: Módulos de las pestañas (Simulación, Biblioteca, Matemática, etc.).
    *   `/www`: Recursos estáticos (estilos CSS, imágenes didácticas, textos JSON de los Tomos y cachés de memoria de agentes).
*   `/scripts`: Scripts complementarios en Python y R (ej. `generar_experimentos_db.py` y `pre_train_agents.R`).
*   `/docs`: Guías documentales, manuscritos en PDF y metadatos de difusión.

---

## 4. Ejecución Paso a Paso de la Aplicación

### Opción A: Desde RStudio (Recomendado)
1.  Abra RStudio.
2.  Vaya a la esquina superior derecha y seleccione **Open Project** o abra el archivo `global.R` o `app.R` ubicado en la carpeta `/app`.
3.  Haga clic en el botón **Run App** (icono de play) en la barra de herramientas del editor de RStudio.

### Opción B: Desde la Consola de R (Línea de Comandos)
1.  Abra su terminal o consola de R.
2.  Establezca el directorio de trabajo en la raíz del bundle (donde se encuentra la carpeta `/app`).
3.  Ejecute la siguiente instrucción:
    ```r
    shiny::runApp("app")
    ```

La aplicación se abrirá automáticamente en su navegador web predeterminado (usualmente en la dirección local `http://127.0.0.1:XXXX`).

---

## 5. Guía de Operación de Módulos Clave

### 5.1 Centro de Simulación Geodésica (Tramos 2D y 3D)
*   **Propósito:** Visualizar cómo el Poder, el Sentido y la Delimitación curvan las trayectorias de los caminantes territoriales.
*   **Paso a Paso:**
    1.  Vaya a la pestaña **Simulación**.
    2.  Seleccione un perfil peatonal (Comunitario, Corporativo, Estatal).
    3.  Ajuste los controles deslizantes para alterar las fuerzas activas (Poder, Sentido, Delimitación).
    4.  Observe en tiempo real cómo la trayectoria simulada se desvía de la geodésica pura en el gráfico interactivo 3D.

### 5.2 Agente de Conocimiento Local (Biblioteca & Chat Offline)
*   **Propósito:** Consultar e interrogar la base teórica de los Tomos I y II de forma offline mediante un cónclave de agentes cognitivos.
*   **Paso a Paso:**
    1.  Vaya a la pestaña **Biblioteca**.
    2.  En la sección derecha, seleccione con qué experto del cónclave desea hablar (Filósofo Territorial, Matemático de Geotensores, Diseñador de Puerto Umbral).
    3.  Escriba su consulta en la caja de texto (ej. *«¿Cómo se vincula el conmutador de Lie con las políticas urbanas?»*) y presione **Enviar**.
    4.  El agente responderá extrayendo y citando directamente fragmentos semánticos de los tomos guardados en caché.

### 5.3 Módulo de Validación Empírica (Rúbrica IEO)
*   **Propósito:** Cargar datos tomados en terreno con la Rúbrica IEO (Indicador de Esfuerzo Ontológico) y contrastar la trayectoria observada con la física del modelo.
*   **Paso a Paso:**
    1.  Vaya a la pestaña **Experimentos**.
    2.  Seleccione el experimento de terreno correspondiente.
    3.  Visualice las métricas de correlación ($R^2$, significación y error cuadrático medio) que validan el calce del solucionador geodésico con las trayectorias de caminantes reales.

---

## 6. Resolución de Problemas (Troubleshooting)

*   **Error: `No se encuentra geotensor_experimentos.db`:**
    *   Asegúrese de que el archivo de base de datos se encuentra dentro de la carpeta `/app` o en la raíz del proyecto.
    *   Si desea regenerar la base de datos desde cero, ejecute en su terminal:
        ```bash
        python scripts/generar_experimentos_db.py
        ```
*   **El Video Demostrativo se muestra en negro o vacío:**
    *   Esto ocurre si no se ha incluido el archivo `tutorial.mp4` en `app/www/video/`. La aplicación está diseñada para no colapsar ante esta ausencia. Simplemente coloque el archivo de video con el nombre exacto `tutorial.mp4` en dicha carpeta y recargue la aplicación para habilitar el reproductor.
*   **Falta de Memoria de los Agentes:**
    *   La app utiliza archivos de caché locales en `/app/www/data/`. Asegúrese de no modificar ni borrar `pre_trained_memory.json` ni `qa_cache.json`, ya que contienen el entrenamiento base de comportamiento y respuestas del cónclave de expertos.

---

## 7. Video Tutorial Bilingüe y Soporte Multimedia

En la carpeta `/app/www/video/` se incluye un tutorial dinámico optimizado de 2 minutos para aprender a operar la plataforma rápidamente de forma eficiente y sin ruido:

*   **Video Consolidado:** `tutorial_consolidado.mp4` (peso optimizado de ~10 MB, con narración en off en español).
*   **Subtítulos en Español:** `tutorial_es.srt`
*   **Subtítulos en Inglés (English Subtitles):** `tutorial_en.srt`

### Cómo reproducir con subtítulos:
1. Abra el reproductor multimedia de su preferencia (como VLC Media Player o Windows Media Player).
2. Cargue el video `tutorial_consolidado.mp4`.
3. Arrastre el archivo de subtítulos `.srt` de su elección (ej. `tutorial_es.srt`) sobre la ventana del reproductor, o selecciónelo en el menú **Subtítulos > Añadir archivo de subtítulos**.
