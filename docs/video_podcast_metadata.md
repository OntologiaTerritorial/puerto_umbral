# Video & Podcast Metadata and Technical Scripts (Tomo II)

Este documento detalla las especificaciones técnicas, guiones conceptuales y metadatos de difusión para la producción de materiales audiovisuales de soporte para el **Bundle de Zenodo del Tomo II (Puerto Umbral)**.

---

## 1. Ficha Técnica de Producción Audiovisual

### Video Demostrativo
*   **Formato de Video**: MP4 (H.264 / AVC)
*   **Resolución**: 1920x1080 (Full HD, 1080p)
*   **Tasa de Fotogramas (FPS)**: 60 FPS (para transiciones de animación fluidas)
*   **Audio**: Estéreo AAC-LC, 48 kHz, 320 kbps
*   **Canales de Difusión**: Zenodo (Repositorio Científico), YouTube, Sitios Académicos.

### Podcast Conceptual
*   **Formato de Audio**: MP3 / WAV (LPCM para masterización)
*   **Tasa de Muestreo**: 44.1 kHz, 24 bits
*   **Configuración**: Estéreo, con paneo sutil para simular espacialidad tridimensional.
*   **Duración Estimada**: 12-15 minutos.

---

## 2. Guión Técnico: Video Demostrativo de la App Shiny
*Duración estimada: 5 minutos.*

| Tiempo | Escena Visual | Narración (Voz en Off) / Texto |
| :--- | :--- | :--- |
| **0:00 - 0:30** | Paneos rápidos de la app Shiny en Modo Campo (Alto Contraste) y Modo Oscuro. Primer plano del mosaico de 7 tarjetas en la pestaña de Onboarding. | *"Bienvenidos al resolvedor interactivo de Puerto Umbral para el Tomo II. Esta herramienta científica auto-contenida nos permite modelar la precordillera de Peñalolén como un manifold territorial deformado por tensiones sociales y físicas."* |
| **0:30 - 1:30** | Se hace clic en 'Cargar Experimento 1'. La app cambia a la simulación geodésica. Se muestra la desviación del peatón en el borde (refracción de Snell). Luego se pasa al 'Experimento 3 (Autopoiesis)' superando $P \ge P_{crit}$ para ver cómo el repulsor se torna atractor. | *"A través de 7 experimentos interactivos, exploramos desde la refracción de borde de Snell en límites periurbanos hasta la inversión Hessiana local de autopoiesis territorial bajo presión crítica, donde la exclusión da paso al cuidado vecinal."* |
| **1:30 - 2:30** | Visualización en pantalla dividida: a la izquierda, el mapa Leaflet interactivo con popups poéticos (fuente Serif); a la derecha, la malla Plotly 3D rotando suavemente con la topografía de altitud y NDVI. | *"El mapa y la malla 3D no son inertes: hablan de lo que les pasa. Los popups poéticos de fuente Serif nos sitúan en la ontología de cuidados, traduciendo variables métricas a la prosa literaria del habitar cotidiano."* |
| **2:30 - 3:45** | Se navega a la pestaña de Análisis Matemático. Se muestra el decaimiento de Lyapunov. Se desplaza el slider de la derivada fraccionaria Caputo y se observa en tiempo real cómo la curva de decaimiento se ralentiza en contraste con el decaimiento exponencial. | *"En la pestaña de estabilidad, la aproximación discreta de Grunwald-Letnikov nos permite computar en tiempo real la memoria del trauma. A menor orden fraccionario de Caputo, la disipación se ralentiza, emulando cómo las cicatrices históricas del territorio persisten en el transitar presente."* |
| **3:45 - 4:30** | Se activa el 'Modo Solo (Auto-Ceguera)' y se muestra cómo el mapa oculta las capas teóricas. Luego, se sube un CSV de campo y se genera el diagrama de dispersión con regresión lineal y métricas de Pearson y $R^2$. | *"Inspirados en las expediciones de Magallanes, implementamos el protocolo de doble ciego. Con el Modo Solo, el observador peatonal está ciego a las curvas teóricas. Posteriormente, cargamos los datos de campo para validar estadísticamente el resolvedor mediante regresiones lineales y p-valores."* |
| **4:30 - 5:00** | Pantalla de cierre con enlace de Zenodo, DOI del proyecto y créditos institucionales de investigación. | *"Descargue el bundle auto-contenido en Zenodo y ejecute Puerto Umbral bajo su propio mando local. La ciencia territorial al servicio de la justicia comunitaria."* |

---

## 3. Guión Conceptual: Podcast "Diálogos del Manifold"
*Una conversación ficticia entre dos Consejeros Territoriales: **Consejero Métrica (Físico-Geoestadístico)** y **Consejera Ontológica (Poeta-Comunitaria)**.*

*   **[Efecto de Sonido]**: Viento andino y agua fluyendo por una acequia de Peñalolén. Música ambiental minimalista con sintetizadores cálidos.
*   **Consejero Métrica**: *"Es fascinante ver cómo el relieve de la precordillera de Peñalolén impone restricciones insalvables a una cuidadora que camina con un coche de bebé o a un adulto mayor. No podemos seguir planificando con distancias Euclidianas planas. La métrica $g_{\mu\nu}$ no es constante; la fricción de ladera altera la geodésica."*
*   **Consejera Ontológica**: *"Exacto, Métrica. Lo que tú llamas tensor métrico, las comunidades lo experimentan como el declive vital de la pendiente. Es lo que el Tomo II llama la colina invisible de gentrificación. Caminar cuesta arriba no es solo un esfuerzo cardíaco; es sortear la expulsión de tu propio barrio por el capital inmobiliario."*
*   **Consejero Métrica**: *"Y la matemática nos da el lenguaje para modelarlo sin perder rigor. Cuando introducimos la ecuación geodésica de Euler-Lagrange, demostramos que el peatón sigue un trayecto de mínima acción, un Wu Wei físico. Pero al cruzar al Santuario de Peñalolén, las condiciones de borde de Robin actúan como un escudo ecológico. Canalizan el flujo evitando la pendiente hostil."*
*   **Consejera Ontológica**: *"Es hermoso porque conecta la rigurosidad estadística con la ontología del trauma. Por ejemplo, en el Experimento 4, la memoria del suelo. No es un sistema sin memoria o markoviano. La derivada fraccionaria de Caputo nos permite mathematizar la persistencia. La cicatriz urbana de la exclusión queda grabada en el suelo y altera los flujos de caminata por generaciones."*
*   **Consejero Métrica**: *"Sí, y para validar estas hipótesis en terreno, no podemos quedarnos en la teoría. Por eso los equipos valientes que van al territorio aplican la ficha del Índice de Estado Observado (IEO). Diseñamos un protocolo riguroso de doble ciego, inspirado en el informe de Magallanes. Si el observador de campo ve la geodésica simulada, se contamina el dato. Con el Modo Solo, el observador camina ciego, y luego contrastamos sus observaciones con el modelo mediante regresiones y correlación de Pearson."*
*   **Consejera Ontológica**: *"Es el espíritu de la ciencia comunitaria: metodologías estrictas para realidades complejas, llevadas en un bundle auto-contenido que puede correr localmente en cualquier computador sin internet. Un Puerto Umbral al alcance de todos."*
*   **[Efecto de Sonido]**: La música ambiental sube de volumen lentamente y se desvanece.

---

## 4. Metadatos de Publicación (Zenodo Metadata)

*   **Title**: Puerto Umbral: Self-Contained Scientific Bundle for Differential Geometry and Urban Geotensors (Tomo II)
*   **Description**: This repository contains the complete self-contained scientific bundle for Tomo II. It includes the R-Shiny reactive application for geodesic simulations, the Python trajectories engine, a populated SQLite database with synthetic and real urban experiments, and the pedagogical field guidelines for blind data collection.
*   **Keywords**: Differential Geometry, Urban Geotensors, R-Shiny, Fractional Calculus, Caputo Derivative, Ledoit-Wolf Shrinkage, Double-Blind Sampling.
*   **License**: Creative Commons Attribution 4.0 International (CC-BY-4.0)
*   **Version**: 1.0.0
*   **Language**: spa / eng (Bilingual Documentation)
