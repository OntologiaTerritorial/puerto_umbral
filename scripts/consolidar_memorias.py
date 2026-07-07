# scripts/consolidar_memorias.py
# Script para consolidar "Cápsulas de Memoria" (JSON) en el caché de preguntas/respuestas (qa_cache.json)
# de la plataforma Puerto Umbral, respetando los principios CARE de soberanía colectiva.

import os
import json
import re
from datetime import datetime

# Rutas del bundle
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MEMORIAS_DIR = os.path.join(BASE_DIR, "memorias_recibidas")
PROCESADAS_DIR = os.path.join(MEMORIAS_DIR, "procesadas")
CACHE_FILE = os.path.join(BASE_DIR, "app", "www", "data", "qa_cache.json")

# Lista de stop words sencillas en español e inglés para filtrar palabras clave
STOP_WORDS = set([
    "el", "la", "los", "las", "un", "una", "unos", "unas", "de", "del", "a", "al", "en", "y", "o", "u",
    "que", "que", "es", "son", "un", "para", "por", "con", "sin", "sobre", "entre", "bajo", "como",
    "the", "a", "an", "of", "to", "in", "on", "at", "for", "with", "by", "about", "between", "how", "what", "is"
])

def clean_html(text):
    """Elimina etiquetas HTML de la respuesta del agente."""
    # Eliminar bloques de cabeceras de personas como <b>[Cónclave: ...]</b>
    text = re.sub(r"<b>\[Cónclave:[^\]]*\]</b><br>", "", text, flags=re.IGNORECASE)
    text = re.sub(r"<b>\[Conclave:[^\]]*\]</b><br>", "", text, flags=re.IGNORECASE)
    # Eliminar notas al pie de especialidad como * Enfoque ergonómico:...
    text = re.sub(r"<br><br><i>\* Enfoque[^<]*</i>", "", text, flags=re.IGNORECASE)
    text = re.sub(r"<br><br><i>\* Community[^<]*</i>", "", text, flags=re.IGNORECASE)
    text = re.sub(r"<br><br><i>\* Ergonomic[^<]*</i>", "", text, flags=re.IGNORECASE)
    # Eliminar etiquetas HTML genéricas
    text = re.sub(r"<[^>]+>", "", text)
    return text.strip()

def extract_keywords(question):
    """Extrae palabras clave de la pregunta para el indexado."""
    words = re.findall(r"\b\w{3,}\b", question.lower()) # palabras de 3 o más letras
    keywords = [w for w in words if w not in STOP_WORDS]
    # Retornar únicas conservando orden
    return list(dict.fromkeys(keywords))

def main():
    # Asegurar la existencia de las carpetas
    os.makedirs(MEMORIAS_DIR, exist_ok=True)
    os.makedirs(PROCESADAS_DIR, exist_ok=True)

    print(f"=== CONSOLIDADOR DE MEMORIAS PUERTO UMBRAL ===")
    print(f"Buscando archivos de memoria en: {MEMORIAS_DIR}\n")

    # Cargar qa_cache.json existente
    if os.path.exists(CACHE_FILE):
        try:
            with open(CACHE_FILE, "r", encoding="utf-8") as f:
                qa_cache = json.load(f)
        except Exception as e:
            print(f"Error al leer qa_cache.json: {e}")
            return
    else:
        print(f"Archivo de caché no encontrado en {CACHE_FILE}. Creando estructura vacía.")
        qa_cache = {}

    json_files = [f for f in os.listdir(MEMORIAS_DIR) if f.endswith(".json") and os.path.isfile(os.path.join(MEMORIAS_DIR, f))]
    
    if not json_files:
        print("No se encontraron nuevas cápsulas de memoria (.json) para procesar.")
        print(f"Deje los archivos de memoria descargados en la carpeta: {MEMORIAS_DIR} e intente de nuevo.")
        return

    nuevas_entradas = 0

    for file_name in json_files:
        file_path = os.path.join(MEMORIAS_DIR, file_name)
        print(f"Procesando: {file_name}")

        try:
            with open(file_path, "r", encoding="utf-8") as f:
                messages = json.load(f)
        except Exception as e:
            print(f"  Error al leer {file_name}: {e}. Omitiendo.")
            continue

        # Recorrer mensajes en pares (role = user -> role = agent)
        i = 0
        while i < len(messages) - 1:
            msg_user = messages[i]
            msg_agent = messages[i+1]

            if msg_user.get("role") == "user" and msg_agent.get("role") == "agent":
                pregunta = msg_user.get("text", "").strip()
                respuesta_sucia = msg_agent.get("text", "")
                respuesta = clean_html(respuesta_sucia)

                if pregunta and respuesta:
                    # Generar una clave única usando timestamp y contador
                    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                    concept_key = f"capsula_{timestamp}_{nuevas_entradas}"
                    
                    keywords = extract_keywords(pregunta)
                    
                    # Estructura del caché
                    qa_cache[concept_key] = {
                        "keywords": keywords,
                        "title": {
                            "es": pregunta,
                            "en": pregunta # Valor por defecto
                        },
                        "answer": {
                            "es": respuesta,
                            "en": respuesta # Valor por defecto
                        }
                    }
                    nuevas_entradas += 1
                i += 2
            else:
                i += 1

        # Mover archivo procesado a la carpeta de procesadas
        dest_path = os.path.join(PROCESADAS_DIR, file_name)
        # Si existe, eliminarlo antes de mover
        if os.path.exists(dest_path):
            os.remove(dest_path)
        os.rename(file_path, dest_path)
        print(f"  Archivo archivado en: {dest_path}")

    # Guardar qa_cache.json enriquecido
    if nuevas_entradas > 0:
        try:
            with open(CACHE_FILE, "w", encoding="utf-8") as f:
                json.dump(qa_cache, f, ensure_ascii=False, indent=2)
            print(f"\n[+] Proceso finalizado con éxito.")
            print(f"Se agregaron {nuevas_entradas} nuevas entradas de memoria al qa_cache.json.")
        except Exception as e:
            print(f"Error al escribir qa_cache.json: {e}")
    else:
        print("\nNo se encontraron nuevas entradas válidas en los archivos.")

if __name__ == "__main__":
    main()
