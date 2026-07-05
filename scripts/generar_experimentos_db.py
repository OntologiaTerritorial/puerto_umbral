import sqlite3
import os
import shutil
import math
import random

def main():
    db_name = "geotensor_experimentos.db"
    
    # Smart path detection to support both development environment and Zenodo bundle root
    if os.path.exists("puerto_umbral_zenodo_bundle"):
        src_db = os.path.join("app", db_name)
        dst_dir = os.path.join("puerto_umbral_zenodo_bundle", "app")
        copy_mode = True
    elif os.path.exists("app"):
        src_db = os.path.join("app", db_name)
        dst_dir = "app"
        copy_mode = False
    else:
        src_db = db_name
        dst_dir = "."
        copy_mode = False
        
    os.makedirs(dst_dir, exist_ok=True)
    dst_db = os.path.join(dst_dir, db_name)
    
    if copy_mode:
        print(f"Copying source database from '{src_db}' to '{dst_db}'...")
        if os.path.exists(src_db):
            shutil.copy2(src_db, dst_db)
        else:
            print("Warning: Source database not found. Creating a new one from scratch.")
    else:
        print(f"Operating database at: {dst_db}")
        
    conn = sqlite3.connect(dst_db)
    cursor = conn.cursor()
    
    # 1. Create tables if they do not exist
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS experimentos (
        id INTEGER PRIMARY KEY,
        contexto TEXT NOT NULL,
        escala TEXT NOT NULL,
        descripcion TEXT,
        fecha_inicio TEXT NOT NULL
    );
    """)
    
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS pixeles (
        id TEXT PRIMARY KEY,
        experimento_id INTEGER NOT NULL,
        padre_pixel_id TEXT,
        x REAL NOT NULL,
        y REAL NOT NULL,
        altitud REAL NOT NULL,
        ndvi REAL,
        cobertura TEXT,
        red_cuidado TEXT DEFAULT 'Ninguno',
        FOREIGN KEY (experimento_id) REFERENCES experimentos(id),
        FOREIGN KEY (padre_pixel_id) REFERENCES pixeles(id)
    );
    """)
    
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS pixel_relaciones (
        origen_id TEXT,
        destino_id TEXT,
        tipo_relacion TEXT CHECK(tipo_relacion IN ('cooperacion_cuidado', 'flujo_diario', 'dependencia')),
        fuerza_vinculo REAL NOT NULL,
        PRIMARY KEY (origen_id, destino_id, tipo_relacion),
        FOREIGN KEY (origen_id) REFERENCES pixeles(id),
        FOREIGN KEY (destino_id) REFERENCES pixeles(id)
    );
    """)
    
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS pixel_memorias (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        pixel_id TEXT NOT NULL,
        timestamp_simulado REAL NOT NULL,
        tipo_hito TEXT CHECK(tipo_hito IN ('lugar_culto', 'sitio_memoria', 'nodo_resistencia')),
        descripcion TEXT,
        atraccion_H_i REAL DEFAULT 1.0,
        FOREIGN KEY (pixel_id) REFERENCES pixeles(id)
    );
    """)
    
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS pixel_latencias (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        pixel_id TEXT NOT NULL,
        timestamp_simulado REAL NOT NULL,
        tipo_tension TEXT CHECK(tipo_tension IN ('gentrificacion_poder', 'conflicto_delito', 'barrera_limite')),
        magnitud_friccion REAL NOT NULL,
        FOREIGN KEY (pixel_id) REFERENCES pixeles(id)
    );
    """)
    
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS geodesicas_registro (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        experimento_id INTEGER NOT NULL,
        timestamp_simulado REAL NOT NULL,
        origen_id TEXT NOT NULL,
        destino_id TEXT NOT NULL,
        accion_S REAL NOT NULL,
        eficiencia REAL NOT NULL,
        energia_final REAL NOT NULL,
        estado TEXT NOT NULL,
        trayectoria_coords TEXT NOT NULL,
        FOREIGN KEY (experimento_id) REFERENCES experimentos(id),
        FOREIGN KEY (origen_id) REFERENCES pixeles(id),
        FOREIGN KEY (destino_id) REFERENCES pixeles(id)
    );
    """)
    
    # Create the IEO (Field survey data) table for statistical validation contrast
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS pixel_ieo_campo (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        pixel_id TEXT NOT NULL,
        muestra_n INTEGER,
        friccion_observada REAL,
        cohesion_observada REAL,
        FOREIGN KEY (pixel_id) REFERENCES pixeles(id)
    );
    """)
    
    # 2. Migrate existing Experiment 1 (Peñalolén) to Experiment 6 (Santuario)
    print("Migrating original Peñalolén data from experiment_id = 1 to 6...")
    cursor.execute("UPDATE pixeles SET experimento_id = 6 WHERE experimento_id = 1;")
    cursor.execute("UPDATE geodesicas_registro SET experimento_id = 6 WHERE experimento_id = 1;")
    
    # 3. Clean up any existing data for synthetic experiments to ensure idempotency
    cursor.execute("DELETE FROM experimentos;")
    cursor.execute("DELETE FROM pixeles WHERE experimento_id != 6;")
    cursor.execute("DELETE FROM pixel_relaciones WHERE origen_id LIKE '%_synth_%';")
    cursor.execute("DELETE FROM pixel_memorias WHERE pixel_id LIKE '%_synth_%';")
    cursor.execute("DELETE FROM pixel_latencias WHERE pixel_id LIKE '%_synth_%';")
    cursor.execute("DELETE FROM pixel_ieo_campo;")
    
    # 4. Insert descriptions in 'experimentos' table
    experimentos_list = [
        (1, "Refracción de Borde (Snell)", "Comuna de Borde", "Grilla sintética con un límite abrupto de métrica horizontal/vertical para observar la refracción de Snell peatonal.", "2026-06-26"),
        (2, "Desviación Geodésica (Segregación)", "Comuna del Vacío", "Espacio con un obstáculo de alta fricción central que simula segregación espacial y desvía las geodésicas.", "2026-06-26"),
        (3, "Autopoiesis Territorial (Atractor Solidario)", "Comuna Autopoietica", "Grilla donde el nodo central cambia de repulsor a atractor cuando la presión social supera el umbral crítico P_crit.", "2026-06-26"),
        (4, "Memoria del Trauma (Caputo Fraccionario)", "Comuna de la Memoria", "Escenario con nodos históricos de trauma y decaimiento de memoria no markoviano de orden fraccionario.", "2026-06-26"),
        (5, "Métrica de Moran (Ledoit-Wolf)", "Comuna Geoestadística", "Malla con ruido espacial Gaussiano y matrices inestables listas para validación Moran y regularización Ledoit-Wolf.", "2026-06-26"),
        (6, "Santuario de Peñalolén (Robin Boundary)", "Peñalolén precordillera", "Caso real del Santuario Quebrada de Macul con límites ecológicos asimétricos y condiciones de Robin.", "2026-06-26"),
        (7, "Refracción de Capital (Urbano-Rural)", "Borde Urbano-Rural", "Transición y salto de plusvalía y especulación inmobiliaria en el límite periurbano.", "2026-06-26")
    ]
    cursor.executemany("INSERT INTO experimentos VALUES (?, ?, ?, ?, ?);", experimentos_list)
    
    # 5. Populate Synthetic Experiments (1, 2, 3, 4, 5, 7)
    grid_size = 12
    # UTM center offset for Santiago scale
    x_base = 352000
    y_base = 6292000
    spacing = 200 # 200 meters between pixels
    
    for exp_id in [1, 2, 3, 4, 5, 7]:
        print(f"Generating synthetic grid for Experiment {exp_id}...")
        pixels_data = []
        relaciones_data = []
        memorias_data = []
        latencias_data = []
        ieo_data = []
        
        for r in range(grid_size):
            for c in range(grid_size):
                p_id = f"exp{exp_id}_synth_{r}_{c}"
                x_coord = x_base + c * spacing
                y_coord = y_base + r * spacing
                
                # Default baseline attributes
                alt = 700.0
                ndvi = 0.4
                cobertura = "Urbano"
                red_cuidado = "Media"
                
                # Experiment-specific structural parameters
                if exp_id == 1:
                    # Snell transition: Northern half (r >= 6) is high-friction urban, Southern (r < 6) is low-friction rural
                    if r >= 6:
                        cobertura = "Urbano"
                        ndvi = 0.2
                        alt = 700.0
                        red_cuidado = "Ninguno"
                    else:
                        cobertura = "Rural"
                        ndvi = 0.7
                        alt = 600.0
                        red_cuidado = "Media"
                        
                elif exp_id == 2:
                    # Segregation: circular high-friction hill in the center
                    dist_to_center = math.sqrt((r - 5.5)**2 + (c - 5.5)**2)
                    alt = 600.0 + 300.0 * math.exp(-0.15 * (dist_to_center**2))
                    ndvi = 0.5 - 0.3 * math.exp(-0.15 * (dist_to_center**2))
                    if dist_to_center < 2.5:
                        cobertura = "Centro Segregado"
                        red_cuidado = "Ninguno"
                        
                elif exp_id == 3:
                    # Autopoiesis: Central neighborhood node (5, 5) with high potential
                    dist_to_center = math.sqrt((r - 5.5)**2 + (c - 5.5)**2)
                    alt = 650.0 + 100.0 * math.exp(-0.2 * (dist_to_center**2))
                    if dist_to_center < 1.5:
                        cobertura = "Nodo Autopoiético"
                        red_cuidado = "Media"
                        
                elif exp_id == 4:
                    # Trauma: Historical trauma events
                    dist_to_grimaldi = math.sqrt((r - 3)**2 + (c - 3)**2)
                    dist_to_center = math.sqrt((r - 8)**2 + (c - 7)**2)
                    alt = 680.0
                    if dist_to_grimaldi < 1.0:
                        cobertura = "Sitio Memoria A"
                        red_cuidado = "Media"
                    elif dist_to_center < 1.0:
                        cobertura = "Sitio Memoria B"
                        red_cuidado = "Media"
                        
                elif exp_id == 5:
                    # Moran/LW: Add random noise to ndvi and red_cuidado
                    random.seed(r * 13 + c * 37)
                    alt = 700.0 + random.uniform(-10.0, 10.0)
                    ndvi = max(0.1, min(0.9, 0.4 + random.normalvariate(0.0, 0.15)))
                    red_cuidado = random.choice(["Ninguno", "Media"])
                    
                elif exp_id == 7:
                    # Capital: Urban-Rural border at column c = 6
                    if c >= 6:
                        cobertura = "Urbano Especulativo"
                        alt = 650.0
                        ndvi = 0.15
                        red_cuidado = "Ninguno"
                    else:
                        cobertura = "Rural Periurbano"
                        alt = 620.0
                        ndvi = 0.6
                        red_cuidado = "Media"
                
                pixels_data.append((p_id, exp_id, None, x_coord, y_coord, alt, ndvi, cobertura, red_cuidado))
                
                # Generate field observations (IEO) template data for statistical validation
                # The observed data has correlation with simulated metric but contains field noise
                simulated_metric = alt * (1.0 - ndvi)
                obs_noise = random.normalvariate(0.0, 15.0)
                obs_fricc = simulated_metric * 0.8 + obs_noise
                obs_cohes = (1.0 - ndvi) * 100.0 + random.normalvariate(0.0, 10.0)
                ieo_data.append((p_id, 1, max(0.0, obs_fricc), max(0.0, min(100.0, obs_cohes))))
                
                # Populate relations with 4 cardinal neighbors
                for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                    nr, nc = r + dr, c + dc
                    if 0 <= nr < grid_size and 0 <= nc < grid_size:
                        n_id = f"exp{exp_id}_{nr}_{nc}"
                        
                        # Base weight
                        weight = 1.0
                        
                        # Exp-specific relational structures
                        if exp_id == 1:
                            # Within urban half: link is weak. Within rural half: link is strong.
                            if r >= 6 and nr >= 6:
                                weight = 0.8
                            elif r < 6 and nr < 6:
                                weight = 3.5
                            else:
                                # Crossing border
                                weight = 1.8
                        elif exp_id == 3:
                            # Strong internal connections in the autopoietic neighborhood
                            if dist_to_center < 3.0:
                                weight = 4.0
                        elif exp_id == 7:
                            # Boundary crossings show high speculative pressure
                            if (c == 5 and nc == 6) or (c == 6 and nc == 5):
                                weight = 5.0
                                
                        relaciones_data.append((p_id, n_id, "flujo_diario", weight))
                        
                # Populate historical memories (Caputo trauma, especially for exp 4)
                if exp_id == 4:
                    if r == 3 and c == 3:
                        # Major historical trauma event (Villa Grimaldi replica)
                        for year_offset, trauma_val in [(2018, 0.95), (2020, 0.85), (2022, 0.75), (2024, 0.65), (2026, 0.55)]:
                            memorias_data.append((p_id, year_offset, "sitio_memoria", "Clausura y represión espacial", trauma_val))
                    elif r == 8 and c == 7:
                        # Minor historical event
                        for year_offset, trauma_val in [(2021, 0.80), (2023, 0.60), (2025, 0.40)]:
                            memorias_data.append((p_id, year_offset, "nodo_resistencia", "Desalojo de huerto", trauma_val))
                else:
                    # Generic placeholder memory for other experiments
                    if r == 5 and c == 5:
                        memorias_data.append((p_id, 2025.0, "lugar_culto", "Inauguración plaza", 0.3))
                        
                # Populate latent frictions (Lyapunov)
                if exp_id == 2:
                    # Friction spikes in the central segregation region
                    dist_to_center = math.sqrt((r - 5.5)**2 + (c - 5.5)**2)
                    friction_val = 15000.0 * math.exp(-0.25 * (dist_to_center**2))
                    latencias_data.append((p_id, 2026.0, "barrera_limite", friction_val))
                elif exp_id == 3:
                    # Friction decreases as autopoietic cohesion rises
                    dist_to_center = math.sqrt((r - 5.5)**2 + (c - 5.5)**2)
                    friction_val = 8000.0 * (1.0 - math.exp(-0.2 * (dist_to_center**2)))
                    latencias_data.append((p_id, 2026.0, "gentrificacion_poder", friction_val))
                elif exp_id == 5:
                    # Random noisy frictions for Moran calculations
                    f_noise = max(500.0, 5000.0 + random.normalvariate(0.0, 2000.0))
                    latencias_data.append((p_id, 2026.0, "conflicto_delito", f_noise))
                else:
                    latencias_data.append((p_id, 2026.0, "barrera_limite", 1200.0))
        
        # Write to SQLite
        cursor.executemany("INSERT INTO pixeles VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);", pixels_data)
        
        # Deduplicate relationships
        relaciones_data_dict = {}
        for o_id, d_id, t_rel, weight in relaciones_data:
            key = (o_id, d_id, t_rel)
            relaciones_data_dict[key] = weight
        
        rel_insert = [(k[0], k[1], k[2], v) for k, v in relaciones_data_dict.items()]
        cursor.executemany("INSERT OR IGNORE INTO pixel_relaciones VALUES (?, ?, ?, ?);", rel_insert)
        
        if memorias_data:
            cursor.executemany("INSERT INTO pixel_memorias (pixel_id, timestamp_simulado, tipo_hito, descripcion, atraccion_H_i) VALUES (?, ?, ?, ?, ?);", memorias_data)
        if latencias_data:
            cursor.executemany("INSERT INTO pixel_latencias (pixel_id, timestamp_simulado, tipo_tension, magnitud_friccion) VALUES (?, ?, ?, ?);", latencias_data)
        if ieo_data:
            cursor.executemany("INSERT INTO pixel_ieo_campo (pixel_id, muestra_n, friccion_observada, cohesion_observada) VALUES (?, ?, ?, ?);", ieo_data)
            
    # Commit and close
    conn.commit()
    conn.close()
    print("Database updated and structured successfully for all 7 experiments!")

if __name__ == "__main__":
    main()
