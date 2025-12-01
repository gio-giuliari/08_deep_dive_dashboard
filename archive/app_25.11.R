library(shiny)
library(shinyjs)
library(shinythemes)
library(sf)
library(readxl)
library(dplyr)
library(stringr)
library(leaflet)
library(viridisLite)
library(htmlwidgets)

sf::sf_use_s2(FALSE)

# --------------------------- DATA PREP ---------------------------------------

# --- ADM2 SHAPEFILE (GROUND TRUTH FOR PCODES + GEOMETRY) ---------------------
adm2 <- st_read("input/Admin2/SDN_Admin2.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

# --------------------------- ANA EXCEL DATA ----------------------------------
ana_data <- read_xlsx("input/99_ANA_Flagging_Deep_Analysis.xlsx") %>%
  rename(
    ADM2_PCODE      = `Unit of Analysis`,
    ADM2_EN         = `Unit of Analysis Label`,
    SUMMARY_RAW     = `SUMMARY PRELIMINARY FLAG CONCLUSION`,
    deep_dive_raw   = `deep dive decision`,
    main_driver_raw = `Main driver`
  )

# --------------------------- SUMMARY NORMALISATION ---------------------------
normalize_value <- function(x) {
  y <- x |>
    str_replace_all("\u00A0", " ") |>
    str_replace_all("[–—-]", "-") |>
    str_squish() |>
    str_to_lower()
  
  case_when(
    str_detect(y, "risk") & str_detect(y, "excess") & str_detect(y, "mort") ~ "Risk of Excess Mortality",
    str_detect(y, "excess") & str_detect(y, "mort")                         ~ "Excess Mortality",
    str_detect(y, "insuff") | str_detect(y, "probe")                        ~ "Insufficient Data/Probe with Shocks and Vulnerability",
    str_detect(y, "acute")                                                  ~ "Acute Needs",
    TRUE                                                                    ~ NA_character_
  )
}

# --------------------------- CLEAN ANA DATA ----------------------------------
ana_clean <- ana_data %>%
  distinct() %>%
  mutate(
    deep_dive   = str_squish(as.character(deep_dive_raw)),
    deep_dive   = na_if(deep_dive, ""),
    main_driver = str_squish(as.character(main_driver_raw)),
    main_driver = na_if(main_driver, ""),
    SUMMARY     = normalize_value(SUMMARY_RAW)
  )

# IMPORTANT: make a *compact* ANA table and join it onto adm2, like in the working app
ana_deep <- ana_clean %>%
  distinct(ADM2_PCODE, .keep_all = TRUE) %>%  # ensure at most one row per ADM2_PCODE
  select(ADM2_PCODE, deep_dive, main_driver, SUMMARY)

summary_data <- adm2 %>%
  left_join(ana_deep, by = "ADM2_PCODE") %>%   # start from adm2, then attach ANA attributes
  arrange(SUMMARY, ADM1_EN, ADM2_EN)

# --------------------------- PALETTE: DEEP DIVE ------------------------------
deep_dive_levels <- c(
  "EM",
  "RoEM (Suspect EM)",
  "RoEM",
  "AN (Suspect RoEM)",
  "AN"
)

deep_dive_colors <- c(
  "EM"                = "#8B0000",
  "RoEM (Suspect EM)" = "#B22222",
  "RoEM"              = "#FF4500",
  "AN (Suspect RoEM)" = "#FFA500",
  "AN"                = "#FFD700"
)

summary_data$deep_dive <- factor(
  summary_data$deep_dive,
  levels = deep_dive_levels
)

pal_deepdive <- colorFactor(
  palette  = deep_dive_colors,
  domain   = deep_dive_levels,
  na.color = "#f0f0f0"
)

# --------------------------- PALETTE: MAIN DRIVER ----------------------------
main_driver_levels <- sort(unique(na.omit(summary_data$main_driver)))
n_drivers <- length(main_driver_levels)

driver_palette <- if (n_drivers <= 9) {
  c(
    "#D73027", "#FC8D59", "#FEE08B", "#D9EF8B",
    "#91CF60", "#1A9850", "#4575B4", "#313695", "#542788"
  )[seq_len(n_drivers)]
} else {
  turbo(n_drivers)
}

driver_colors <- setNames(driver_palette, main_driver_levels)

pal_driver <- colorFactor(
  palette  = driver_colors,
  domain   = main_driver_levels,
  na.color = "#f0f0f0"
)

# --------------------------- SELECTIZE CHOICES -------------------------------
adm2_nog <- summary_data %>%
  st_set_geometry(NULL) %>%
  arrange(ADM1_EN, ADM2_EN)

adm2_choices <- split(adm2_nog$ADM2_EN, adm2_nog$ADM1_EN)

# --------------------------- CENTROIDS ---------------------------------------
cent_sf <- st_point_on_surface(summary_data)
cent_coords <- st_coordinates(cent_sf)

summary_centroids <- data.frame(
  ADM2_EN = summary_data$ADM2_EN,
  lon     = cent_coords[, "X"],
  lat     = cent_coords[, "Y"],
  stringsAsFactors = FALSE
)

# --------------------------- NULL COALESCE -----------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b



# ================================ UI ==========================================

ui <- navbarPage(
  id = "navbarPage",
  theme = shinytheme("flatly"),
  collapsible = TRUE,
  title = div(HTML(
    '<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">ANA Dashboard</a>'
  )),
  windowTitle = "ANA Deep Dive Tool",
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"
      ),
      # Auto-focus select box + JS connector
      tags$script(HTML("
  // Focus the locality select when the app loads
  Shiny.addCustomMessageHandler('focusLocSelect', function(msg) {
    var el = $('#loc_select')[0];
    if (el && el.selectize) {
      el.selectize.focus();
    }
  });

  // Global references for Leaflet map & connector config
  window._anaMap = window._anaMap || null;
  window._anaConnectorCfg = window._anaConnectorCfg || null;

  // Smooth 'flat → up → flat' connector + centroid dot
  function anaRedrawConnector() {
    var map = window._anaMap;
    var cfg = window._anaConnectorCfg;
    if (!map) return;

    // Remove existing line & dot
    if (map._connectorLayer) {
      map.removeLayer(map._connectorLayer);
      map._connectorLayer = null;
    }
    if (map._connectorDot) {
      map.removeLayer(map._connectorDot);
      map._connectorDot = null;
    }

    if (!cfg || !cfg.active || cfg.lat === undefined || cfg.lng === undefined) return;

    var panel = document.getElementById('info-panel');
    if (!panel) return;

    var mapContainer = map.getContainer();
    var mapRect = mapContainer.getBoundingClientRect();
    var rect = panel.getBoundingClientRect();

    // If panel is hidden, do nothing
    if (rect.width === 0 || rect.height === 0) return;

    // Start = centroid in container coords
    var p0 = map.latLngToContainerPoint([cfg.lat, cfg.lng]);

    // End = left-middle of info panel in container coords
    var anchorX = rect.left - mapRect.left;
    var anchorY = rect.top - mapRect.top + rect.height / 2;
    var p3 = L.point(anchorX, anchorY);

    var dx = p3.x - p0.x;
    var dy = p3.y - p0.y;

    // If almost vertical, just draw a short straight segment
    if (Math.abs(dx) < 10) {
      if (!map.getPane('connectorPane')) {
        map.createPane('connectorPane');
        var paneStraight = map.getPane('connectorPane');
        paneStraight.style.zIndex = 650;
        paneStraight.style.pointerEvents = 'none';
      }

      var straightCoords = [
        map.containerPointToLatLng(p0),
        map.containerPointToLatLng(p3)
      ];

      var straight = L.polyline(straightCoords, {
        pane: 'connectorPane',
        color: '#666666',
        weight: 0.8,
        opacity: 0.45,
        lineJoin: 'round',
        lineCap: 'round',
        smoothFactor: 2
      }).addTo(map);

      var dotStraight = L.circleMarker([cfg.lat, cfg.lng], {
        pane: 'connectorPane',
        radius: 2.5,
        color: '#666666',
        weight: 0.8,
        opacity: 0.7,
        fillColor: '#ffffff',
        fillOpacity: 0.9
      }).addTo(map);

      map._connectorLayer = straight;
      map._connectorDot   = dotStraight;
      return;
    }

    // Cubic Bézier with horizontal tangents at both ends:
    // start flat, curve smoothly, end flat.
    var c1 = L.point(
      p0.x + dx * 0.33,
      p0.y                 // flat at start
    );
    var c2 = L.point(
      p3.x - dx * 0.33,
      p3.y                 // flat at end
    );

    var coords = [];
    var steps = 120; // very smooth, no visible nodes
    for (var i = 0; i <= steps; i++) {
      var t = i / steps;
      var oneMinusT = 1 - t;

      var x =
        oneMinusT * oneMinusT * oneMinusT * p0.x +
        3 * oneMinusT * oneMinusT * t * c1.x +
        3 * oneMinusT * t * t * c2.x +
        t * t * t * p3.x;

      var y =
        oneMinusT * oneMinusT * oneMinusT * p0.y +
        3 * oneMinusT * oneMinusT * t * c1.y +
        3 * oneMinusT * t * t * c2.y +
        t * t * t * p3.y;

      var latlng = map.containerPointToLatLng([x, y]);
      coords.push(latlng);
    }

    // Ensure a top pane for connector
    if (!map.getPane('connectorPane')) {
      map.createPane('connectorPane');
      var pane = map.getPane('connectorPane');
      pane.style.zIndex = 650;           // above polygons
      pane.style.pointerEvents = 'none'; // don't block clicks
    }

    // Thin, low-opacity, smooth grey line
    var line = L.polyline(coords, {
      pane: 'connectorPane',
      color: '#666666',
      weight: 0.8,
      opacity: 0.45,
      lineJoin: 'round',
      lineCap: 'round',
      smoothFactor: 2
    }).addTo(map);

    // Small centroid dot
    var dot = L.circleMarker([cfg.lat, cfg.lng], {
      pane: 'connectorPane',
      radius: 2.5,
      color: '#666666',
      weight: 0.8,
      opacity: 0.7,
      fillColor: '#ffffff',
      fillOpacity: 0.9
    }).addTo(map);

    map._connectorLayer = line;
    map._connectorDot   = dot;
  }

  // Receive connector config from R and (re)draw
  Shiny.addCustomMessageHandler('updateConnector', function(msg) {
    window._anaConnectorCfg = msg;

    function ensureDraw(attempts) {
      attempts = attempts || 0;
      if (!window._anaMap) {
        if (attempts < 20) {
          setTimeout(function() { ensureDraw(attempts + 1); }, 100);
        }
        return;
      }
      anaRedrawConnector();
      if (!window._anaMap._connectorMoveHandlerAttached) {
        window._anaMap.on('zoomend moveend', function() {
          anaRedrawConnector();
        });
        window._anaMap._connectorMoveHandlerAttached = true;
      }
    }

    ensureDraw(0);
  });
"))
      
      ,
      tags$style(HTML("
html, body {
  margin: 0;
  padding: 0;
}
body {
  padding-top: 0 !important;
}

/* Full-page map shell */
#clusterWrap {
  position: fixed;
  top: 50px;         /* just under the fixed navbar */
  left: 0;
  right: 0;
  bottom: 0;
}
#clusterWrap .leaflet-container {
  height: 100% !important;
  width: 100% !important;
}

/* Left control panel */
#cluster-controls {
  position: absolute;
  top: 76px;
  left: 22px;
  width: 260px;
  z-index: 1000;
}
#cluster-controls .panel { padding: 10px 10px 8px 10px; }
#cluster-controls h5 { margin: 8px 0 6px; font-weight: 700; }

#cluster-controls .help {
  font-size: 11px;
  color: #666;
  margin-bottom: 6px;
}
#cluster-controls .form-group {
  margin-bottom: 8px;
}
#cluster-controls .selectize-control.single .selectize-input {
  min-height: 30px;
  padding: 4px 8px;
  font-size: 12px;
}
#cluster-controls .selectize-dropdown {
  font-size: 12px;
}

/* Right info panel */
#info-panel {
  position: absolute;
  top: 76px;
  right: 22px;
  width: 280px;
  z-index: 1000;
}
#info-panel .panel {
  padding: 10px 12px 8px 12px;
  font-size: 12px;
  line-height: 1.3;
}
#info-panel h4 {
  margin-top: 0;
  margin-bottom: 6px;
  font-weight: 700;
  font-size: 14px;
}
"))
    )
  ),
  tabPanel(
    "Maps 🌍",
    div(
      id = "clusterWrap",
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Left controls
      absolutePanel(
        id = "cluster-controls", class = "panel panel-default",
        fixed = TRUE, draggable = TRUE,
        width = 260,
        top = 76, left = 22,
        div(
          class = "panel",
          style = "padding:8px 8px 6px; font-size:12px; line-height:1.25;",
          
          h5(tags$i(class = "fa fa-layer-group"), " Map"),
          radioButtons(
            "map_type", label = NULL, inline = TRUE,
            choices = c("Deep dive" = "deepdive",
                        "Main driver" = "driver"),
            selected = "deepdive"
          ),
          
          h5(tags$i(class = "fa fa-map-marker-alt"), " Locality"),
          div(
            class = "help",
            "Localities are grouped by state. Start typing to search."
          ),
          selectizeInput(
            "loc_select",
            label    = NULL,
            choices  = adm2_choices,
            multiple = FALSE,
            options  = list(
              placeholder = "Start typing a locality name…",
              openOnFocus = TRUE
            )
          )
        )
      ),
      
      # Right info panel
      shinyjs::hidden(
        absolutePanel(
          id = "info-panel", class = "panel panel-default",
          fixed = TRUE, draggable = FALSE,
          width = 280,
          top = 76, right = 22,
          div(
            class = "panel",
            h4(textOutput("info_loc_name")),
            tags$hr(style = "margin:6px 0 8px 0;"),
            tags$p(
              "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ",
              "Sed non risus. Suspendisse lectus tortor, dignissim sit amet, ",
              "adipiscing nec, ultricies sed, dolor."
            )
          )
        )
      ),
      
      # Bottom-right logo + socials
      div(
        style = "position: fixed; bottom: 16px; right: 8px;
                 display: flex; flex-direction: column;
                 align-items: center; justify-content: flex-end; gap: 6px;",
        
        a(
          href   = "https://www.impact-initiatives.org",
          target = "_blank",
          class  = "impact",
          style  = "display: inline-block; margin-bottom: 70px;",
          tags$img(
            src   = "logo.png",
            style = "width: 160px; height: auto; display: block;
                     transform: rotate(-90deg); transform-origin: center center;"
          )
        ),
        
        a(
          href   = "https://x.com/impact_init?lang=en",
          target = "_blank",
          class  = "twitter",
          style  = "color: black; font-size: 30px;",
          HTML('<i class="fab fa-twitter"></i>')
        ),
        
        a(
          href   = "https://www.linkedin.com/company/impact-initiatives",
          target = "_blank",
          class  = "linkedin",
          style  = "color: black; font-size: 30px;",
          HTML('<i class="fab fa-linkedin"></i>')
        )
      )
    )
  )
)

# =============================== SERVER =======================================

server <- function(input, output, session) {
  
  # Auto-focus select box once UI is ready
  session$onFlushed(function() {
    session$sendCustomMessage("focusLocSelect", list())
  }, once = TRUE)
  
  # Base leaflet: simple theme, zoomed in on Sudan, zoom control top-right
  output$map <- renderLeaflet({
    leaflet::leaflet(
      options = leaflet::leafletOptions(
        worldCopyJump      = TRUE,
        preferCanvas       = TRUE,
        zoomControl        = TRUE,
        attributionControl = FALSE,
        scrollWheelZoom    = TRUE
      )
    ) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::setView(lng = 30, lat = 13, zoom = 6) |>
      htmlwidgets::onRender("
        function(el,x){
          // Grab the underlying Leaflet map instance
          var map = this;
          if (HTMLWidgets && HTMLWidgets.find) {
            var w = HTMLWidgets.find(el.id);
            if (w && w.getMap) {
              map = w.getMap();
            }
          }
          window._anaMap = map;
          if (map.zoomControl) map.zoomControl.setPosition('topright');
        }
      ")
  })
  
  # Helper to draw base polygons
  draw_base_polygons <- function(map_type) {
    if (map_type == "deepdive") {
      map_dat <- summary_data %>%
        mutate(
          label_val = if_else(
            is.na(deep_dive),
            "No data",
            as.character(deep_dive)
          )
        )
      
      leafletProxy("map") %>%
        clearGroup("base") %>%
        clearControls() %>%
        addPolygons(
          data        = map_dat,
          group       = "base",
          layerId     = ~ADM2_PCODE,
          fillColor   = ~pal_deepdive(deep_dive),
          color       = "#555555",
          weight      = 0.7,
          opacity     = 1,
          fillOpacity = 0.7,
          label       = ~paste0(ADM2_EN, " — ", label_val),
          highlightOptions = highlightOptions(
            weight = 1.4,
            color  = "#222222",
            opacity = 1,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomleft",
          colors   = unname(deep_dive_colors),
          labels   = names(deep_dive_colors),
          title    = "Deep dive decision",
          opacity  = 0.9
        )
      
    } else {
      map_dat <- summary_data %>%
        mutate(
          label_val = if_else(
            is.na(main_driver),
            "No data",
            as.character(main_driver)
          )
        )
      
      leafletProxy("map") %>%
        clearGroup("base") %>%
        clearControls() %>%
        addPolygons(
          data        = map_dat,
          group       = "base",
          layerId     = ~ADM2_PCODE,
          fillColor   = ~pal_driver(main_driver),
          color       = "#555555",
          weight      = 0.7,
          opacity     = 1,
          fillOpacity = 0.7,
          label       = ~paste0(ADM2_EN, " — ", label_val),
          highlightOptions = highlightOptions(
            weight = 1.4,
            color  = "#222222",
            opacity = 1,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomleft",
          colors   = unname(driver_colors),
          labels   = names(driver_colors),
          title    = "Main driver of RoEM",
          opacity  = 0.9
        )
    }
  }
  
  # Helper to draw selection overlay and trigger JS connector
  draw_selection_overlay <- function(map_type, loc_name) {
    leafletProxy("map") %>% clearGroup("selected")
    
    if (!nzchar(loc_name)) {
      session$sendCustomMessage("updateConnector", list(active = FALSE))
      return(invisible(NULL))
    }
    
    if (map_type == "deepdive") {
      map_dat <- summary_data %>%
        mutate(
          label_val = if_else(
            is.na(deep_dive),
            "No data",
            as.character(deep_dive)
          )
        )
      sel <- map_dat[map_dat$ADM2_EN == loc_name, , drop = FALSE]
      if (nrow(sel) == 0) {
        session$sendCustomMessage("updateConnector", list(active = FALSE))
        return(invisible(NULL))
      }
      col <- pal_deepdive(sel$deep_dive)
    } else {
      map_dat <- summary_data %>%
        mutate(
          label_val = if_else(
            is.na(main_driver),
            "No data",
            as.character(main_driver)
          )
        )
      sel <- map_dat[map_dat$ADM2_EN == loc_name, , drop = FALSE]
      if (nrow(sel) == 0) {
        session$sendCustomMessage("updateConnector", list(active = FALSE))
        return(invisible(NULL))
      }
      col <- pal_driver(sel$main_driver)
    }
    
    leafletProxy("map") %>%
      addPolygons(
        data        = sel,
        group       = "selected",
        fillColor   = col,
        fillOpacity = 0.9,
        color       = "#222222",
        weight      = 1.8,
        opacity     = 1
      )
    
    cent <- summary_centroids[summary_centroids$ADM2_EN == loc_name, , drop = FALSE]
    if (nrow(cent) == 0) {
      session$sendCustomMessage("updateConnector", list(active = FALSE))
      return(invisible(NULL))
    }
    
    session$sendCustomMessage(
      "updateConnector",
      list(
        active = TRUE,
        lat    = cent$lat[1],
        lng    = cent$lon[1]
      )
    )
  }
  
  # Base polygons when map type changes
  observeEvent(input$map_type, {
    mt <- input$map_type %||% "deepdive"
    draw_base_polygons(mt)
    draw_selection_overlay(mt, input$loc_select %||% "")
  }, ignoreInit = FALSE)
  
  # Right panel + selection overlay
  output$info_loc_name <- renderText({
    input$loc_select %||% ""
  })
  
  observeEvent(input$loc_select, {
    loc <- input$loc_select %||% ""
    
    if (nzchar(loc)) {
      shinyjs::show("info-panel")
    } else {
      shinyjs::hide("info-panel")
    }
    
    mt <- input$map_type %||% "deepdive"
    draw_selection_overlay(mt, loc)
  }, ignoreInit = TRUE)
  
  # Map click -> update selectize
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    row <- adm2_nog[adm2_nog$ADM2_PCODE == click$id, ]
    if (nrow(row) == 1) {
      updateSelectizeInput(session, "loc_select", selected = row$ADM2_EN)
    }
  })
}

shinyApp(ui, server)
