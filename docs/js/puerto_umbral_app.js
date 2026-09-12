// Puerto Umbral Next - WebGL 3D Deck.gl + MapLibre Application
// Ontología Territorial | Tomo II: Geotensores
// Autor: John Treimun Ríos

let deckInstance = null;
let rawData = [];
let einsteinData = null;
let hotspotsData = null;

// Initial View State centered on Santiago Basin
const INITIAL_VIEW_STATE = {
  longitude: -70.648,
  latitude: -33.456,
  zoom: 10.5,
  pitch: 50,
  bearing: -20,
  maxZoom: 18,
  minZoom: 8
};

let currentViewState = { ...INITIAL_VIEW_STATE };

// Active Color/Height Metric State
let activeHeightMetric = 'fric'; // 'fric', 'z', 'trau', 'lyap'
let activeColorMetric = 'ndvi';  // 'ndvi', 'fric', 'slp', 'nti'
let heightScaleMultiplier = 1.0;
let showEinsteinRings = true;
let showHotspots = true;
let showCampamentos = true;

// Color scales
function getMetricColor(d) {
  if (activeColorMetric === 'ndvi') {
    // NDVI: Red/Brown (-0.1) -> Yellow (0.1) -> Green (0.5+)
    const v = d.ndvi;
    if (v < 0.05) return [217, 119, 6, 200];     // Dry / Impervious
    if (v < 0.15) return [234, 179, 8, 200];     // Moderate
    if (v < 0.30) return [132, 204, 22, 200];    // Green
    return [22, 163, 74, 220];                   // Dense canopy / Cordillera
  } else if (activeColorMetric === 'fric') {
    // Friction: Blue (low) -> Amber -> Red (extreme)
    const f = d.fric;
    if (f < 1500) return [2, 132, 199, 180];
    if (f < 3500) return [13, 148, 136, 200];
    if (f < 7000) return [217, 119, 6, 220];
    return [225, 29, 72, 240];
  } else if (activeColorMetric === 'slp') {
    // Slope ALOS PALSAR: Green (flat <3°) -> Yellow -> Red (>15°)
    const s = d.slp;
    if (s < 2.0) return [34, 197, 94, 180];
    if (s < 5.0) return [234, 179, 8, 200];
    if (s < 12.0) return [249, 115, 22, 220];
    return [185, 28, 28, 240];
  } else {
    // NTI
    const n = d.nti;
    if (n < 0.3) return [59, 130, 246, 180];
    if (n < 0.6) return [168, 85, 247, 200];
    return [236, 72, 153, 220];
  }
}

function getElevation(d) {
  if (activeHeightMetric === 'z') {
    // Physical Topography from ALOS PALSAR DEM
    return Math.max(0, (d.z - 400) * 1.5 * heightScaleMultiplier);
  } else if (activeHeightMetric === 'fric') {
    // Geotensorial Friction
    return Math.max(50, (d.fric / 3.0) * heightScaleMultiplier);
  } else if (activeHeightMetric === 'trau') {
    // Historical Trauma Memory (Caputo)
    return Math.max(20, (d.trau * 2500) * heightScaleMultiplier);
  } else {
    // Lyapunov decay
    return Math.max(20, (d.lyap * 1500) * heightScaleMultiplier);
  }
}

function updateDeckLayers() {
  if (!deckInstance) return;

  const layers = [];

  // 1. POP Columns Layer (ALOS PALSAR + Geotensors)
  if (rawData && rawData.length > 0) {
    layers.push(
      new deck.ColumnLayer({
        id: 'pop-columns',
        data: rawData,
        diskResolution: 6,
        radius: 120,
        extruded: true,
        pickable: true,
        elevationScale: 1,
        getPosition: d => [d.lng, d.lat],
        getElevation: getElevation,
        getFillColor: getMetricColor,
        getLineColor: [255, 255, 255, 60],
        lineWidthMinPixels: 1,
        updateTriggers: {
          getElevation: [activeHeightMetric, heightScaleMultiplier],
          getFillColor: [activeColorMetric]
        }
      })
    );
  }

  // 2. Einstein Rings (Macro-Anillo de Vespucio & Local Rings)
  if (showEinsteinRings && einsteinData && einsteinData.features) {
    layers.push(
      new deck.GeoJsonLayer({
        id: 'einstein-rings-layer',
        data: einsteinData,
        pickable: true,
        stroked: true,
        filled: true,
        pointType: 'circle',
        getPointRadius: d => (d.properties.anillo_einstein.includes('Vespucio') ? 280 : 160),
        getFillColor: d => (d.properties.anillo_einstein.includes('Vespucio') ? [217, 119, 6, 220] : [245, 158, 11, 180]),
        getLineColor: [255, 255, 255, 200],
        getLineWidth: 2
      })
    );
  }

  // 3. Hotspots de Duelo Territorial SUT-2050
  if (showHotspots && hotspotsData && hotspotsData.features) {
    layers.push(
      new deck.GeoJsonLayer({
        id: 'hotspots-grief-layer',
        data: hotspotsData,
        pickable: true,
        stroked: true,
        filled: true,
        pointType: 'circle',
        getPointRadius: 320,
        getFillColor: [225, 29, 72, 230],
        getLineColor: [255, 255, 255, 240],
        getLineWidth: 3
      })
    );
  }

  deckInstance.setProps({ layers });
}

// Tooltip formatter
function getTooltip({ object }) {
  if (!object) return null;
  const p = object.properties || object;

  if (p.anillo_einstein) {
    return {
      html: `
        <div style="font-weight:700; color:#38bdf8; margin-bottom:4px;">🔭 Anillo de Einstein (Deflexión Gravitacional)</div>
        <div><b>Tipo:</b> ${p.anillo_einstein}</div>
        <div><b>Pixel ID:</b> ${p.pixel_id}</div>
        <div><b>Fricción Relacional:</b> ${Number(p.friccion_relacional).toLocaleString()}</div>
        <div><b>Altitud:</b> ${p.altitud_m} msnm</div>
      `,
      style: { backgroundColor: 'rgba(15,23,42,0.95)', color: '#fff' }
    };
  }

  if (p.escenario) {
    return {
      html: `
        <div style="font-weight:700; color:#f43f5e; margin-bottom:4px;">🩸 Hotspot de Duelo Territorial (SUT 2050)</div>
        <div><b>Sector:</b> ${p.cobertura}</div>
        <div><b>Latencia (Lambda):</b> ${p.latencia_lambda}</div>
        <div><b>Fricción Duelo:</b> ${Number(p.friccion_duelo).toLocaleString()}</div>
      `,
      style: { backgroundColor: 'rgba(15,23,42,0.95)', color: '#fff' }
    };
  }

  // Regular POP
  return {
    html: `
      <div style="font-weight:700; color:#38bdf8; margin-bottom:4px;">📍 Píxel Ontológico (POP ${p.com})</div>
      <div><b>ID Manzana:</b> ${p.id}</div>
      <div style="margin-top:4px; border-top:1px solid #334155; padding-top:4px;">
        <b>Cota ALOS PALSAR:</b> ${p.z} msnm<br>
        <b>Pendiente:</b> ${p.slp}°<br>
        <b>NDVI Estival 2025:</b> ${p.ndvi}<br>
        <b>Fricción Tensorial:</b> ${Number(p.fric).toLocaleString()}<br>
        <b>Norma NTI:</b> ${p.nti}<br>
        ${p.tech ? '<span style="color:#ef4444; font-weight:bold;">⚠️ Campamento TECHO a <300m</span>' : ''}
      </div>
    `,
    style: { backgroundColor: 'rgba(15,23,42,0.95)', color: '#fff' }
  };
}

// Initialize Application
async function initApp() {
  const container = document.getElementById('deck-canvas-container');
  if (!container) return;

  deckInstance = new deck.DeckGL({
    container: 'deck-canvas-container',
    mapLib: maplibregl,
    mapStyle: 'https://basemaps.cartocdn.com/gl/positron-gl-style/style.json',
    initialViewState: INITIAL_VIEW_STATE,
    controller: true,
    getTooltip: getTooltip,
    onViewStateChange: ({ viewState }) => {
      currentViewState = viewState;
    }
  });

  // Load datasets in parallel
  try {
    const [tensoresRes, einsteinRes, hotspotsRes] = await Promise.all([
      fetch('data/rms_tensores_3d.json').then(r => r.json()),
      fetch('data/einstein_rings_rms.geojson').then(r => r.json()),
      fetch('data/hotspots_latencia_rms.geojson').then(r => r.json())
    ]);

    rawData = tensoresRes;
    einsteinData = einsteinRes;
    hotspotsData = hotspotsRes;

    document.getElementById('stat-total-pops').innerText = rawData.length.toLocaleString();
    updateDeckLayers();
  } catch (err) {
    console.error('Error cargando datos geoespaciales:', err);
  }

  setupUIEvents();
}

function setupUIEvents() {
  // Height Metric Selector
  document.querySelectorAll('input[name="height-metric"]').forEach(el => {
    el.addEventListener('change', e => {
      activeHeightMetric = e.target.value;
      updateDeckLayers();
    });
  });

  // Color Metric Selector
  document.querySelectorAll('input[name="color-metric"]').forEach(el => {
    el.addEventListener('change', e => {
      activeColorMetric = e.target.value;
      updateDeckLayers();
    });
  });

  // Height Slider
  const slider = document.getElementById('height-slider');
  if (slider) {
    slider.addEventListener('input', e => {
      heightScaleMultiplier = parseFloat(e.target.value);
      updateDeckLayers();
    });
  }

  // Checkboxes
  const chkEinstein = document.getElementById('chk-einstein');
  if (chkEinstein) {
    chkEinstein.addEventListener('change', e => {
      showEinsteinRings = e.target.checked;
      updateDeckLayers();
    });
  }

  const chkHotspots = document.getElementById('chk-hotspots');
  if (chkHotspots) {
    chkHotspots.addEventListener('change', e => {
      showHotspots = e.target.checked;
      updateDeckLayers();
    });
  }

  // Camera presets
  const btn3D = document.getElementById('btn-cam-3d');
  if (btn3D) {
    btn3D.addEventListener('click', () => {
      deckInstance.setProps({
        viewState: { ...currentViewState, pitch: 55, bearing: -25, transitionDuration: 1000 }
      });
    });
  }

  const btn2D = document.getElementById('btn-cam-2d');
  if (btn2D) {
    btn2D.addEventListener('click', () => {
      deckInstance.setProps({
        viewState: { ...currentViewState, pitch: 0, bearing: 0, transitionDuration: 1000 }
      });
    });
  }

  const btnReset = document.getElementById('btn-cam-reset');
  if (btnReset) {
    btnReset.addEventListener('click', () => {
      deckInstance.setProps({
        viewState: { ...INITIAL_VIEW_STATE, transitionDuration: 1000 }
      });
    });
  }

  // Floating Panel Toggle
  const toggleBtn = document.getElementById('toggle-panel-btn');
  const panel = document.getElementById('panel-control');
  if (toggleBtn && panel) {
    toggleBtn.addEventListener('click', () => {
      panel.classList.toggle('collapsed');
      toggleBtn.innerText = panel.classList.contains('collapsed') ? '⚙️ Controles' : '✕ Cerrar';
    });
  }
}

// Tab navigation handler
window.switchTab = function(tabId) {
  document.querySelectorAll('.tab-section').forEach(sec => sec.classList.remove('active'));
  document.querySelectorAll('.nav-link-custom').forEach(l => l.classList.remove('active'));

  const targetSec = document.getElementById(tabId);
  const targetNav = document.getElementById('nav-' + tabId);

  if (targetSec) targetSec.classList.add('active');
  if (targetNav) targetNav.classList.add('active');

  // Trigger deck resize if entering map tab
  if (tabId === 'sec-mapa-3d' && deckInstance) {
    setTimeout(() => {
      deckInstance.redraw(true);
    }, 100);
  }
};

document.addEventListener('DOMContentLoaded', initApp);
