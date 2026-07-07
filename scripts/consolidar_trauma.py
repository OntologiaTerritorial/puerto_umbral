# scripts/consolidar_trauma.py
import os
import re
import json
import urllib.request

# Configuración del repositorio
REPO_OWNER = "OntologiaTerritorial"
REPO_NAME = "puerto_umbral"
API_URL = f"https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/issues?state=open"

# Encontrar ruta local al archivo de memoria colectiva
local_path = "app/www/data/pre_trained_memory.json"
if not os.path.exists(local_path):
    local_path = "puerto_umbral_zenodo_bundle/app/www/data/pre_trained_memory.json"

if not os.path.exists(local_path):
    print(f"Error: No se pudo encontrar el archivo local pre_trained_memory.json")
    exit(1)

print("=== INICIANDO CONSOLIDACIÓN AUTOMÁTICA DE MEMORIA GEODÉSICA (TRAUMA) ===")
print(f"Buscando donaciones en los Issues abiertos de {REPO_OWNER}/{REPO_NAME}...")

try:
    req = urllib.request.Request(
        API_URL,
        headers={"User-Agent": "PuertoUmbral-Consolidator/1.0"}
    )
    with urllib.request.urlopen(req) as response:
        issues = json.loads(response.read().decode("utf-8"))
except Exception as e:
    print(f"Error al conectar con la API de GitHub: {e}")
    exit(1)

# Cargar la matriz local existente
try:
    with open(local_path, "r", encoding="utf-8") as f:
        local_matrix = json.load(f)
except Exception as e:
    print(f"Error al leer la matriz local: {e}")
    exit(1)

donaciones_procesadas = 0

for issue in issues:
    title = issue.get("title", "")
    # Filtrar por título característico de la plantilla de trauma
    if "Donacion de Memoria Colectiva de Trauma" in title or "Trauma Hotspots" in title:
        issue_num = issue.get("number")
        body = issue.get("body", "")
        
        print(f"\nProcesando Issue #{issue_num}: '{title}'...")
        
        # Buscar el bloque JSON usando expresiones regulares
        match = re.search(r"```json\s*(.*?)\s*```", body, re.DOTALL)
        if not match:
            print(f"  [Advertencia] No se encontró un bloque de código ```json``` en el cuerpo del Issue.")
            continue
            
        json_str = match.group(1).strip()
        
        try:
            donated_matrix = json.loads(json_str)
        except Exception as e:
            print(f"  [Error] No se pudo parsear el bloque JSON del Issue: {e}")
            continue
            
        # Validación de estructura (Matriz 30x30)
        if not isinstance(donated_matrix, list) or len(donated_matrix) != 30:
            print("  [Error] La matriz donada no es una lista válida de 30 filas.")
            continue
            
        valid = True
        for idx, row in enumerate(donated_matrix):
            if not isinstance(row, list) or len(row) != 30:
                print(f"  [Error] La fila {idx + 1} de la matriz donada no tiene exactamente 30 elementos.")
                valid = False
                break
            # Validar que todos los valores sean numéricos
            if not all(isinstance(val, (int, float)) for val in row):
                print(f"  [Error] La fila {idx + 1} contiene valores no numéricos.")
                valid = False
                break
                
        if not valid:
            continue
            
        # Fusión colectiva: Tomar el valor MÁXIMO celda por celda para acumular hotspots
        merged_matrix = []
        for i in range(30):
            merged_row = []
            for j in range(30):
                # max() previene que los traumas se "pisen", acumulando las peores fricciones
                merged_row.append(max(local_matrix[i][j], donated_matrix[i][j]))
            merged_matrix.append(merged_row)
            
        # Actualizar la matriz local para la siguiente iteración
        local_matrix = merged_matrix
        donaciones_procesadas += 1
        print(f"  [Éxito] Donación del Issue #{issue_num} fusionada correctamente en memoria.")

if donaciones_procesadas > 0:
    # Escribir la matriz fusionada final a disco
    try:
        with open(local_path, "w", encoding="utf-8") as f:
            json.dump(local_matrix, f, indent=2)
        print("\n=======================================================")
        print(f"PROCESO TERMINADO: Se consolidaron {donaciones_procesadas} donación(es) con éxito.")
        print(f"Archivo actualizado: {local_path}")
        print("Por favor, haz commit de este cambio y cierra los Issues consolidados en GitHub.")
        print("=======================================================")
    except Exception as e:
        print(f"Error al guardar la matriz actualizada en disco: {e}")
else:
    print("\nNo se encontraron Issues abiertos con donaciones de memoria de trauma pendientes de procesar.")
