# app

# note: remote shiny server restarts the app only when file app.R has changes


# init --------------------------------------------------------------------

# packages ----
library(shiny)
library(shiny.router)
library(leaflet)
library(DT)
library(dplyr)
library(sf)
library(stringr)

# functions ----
source("R/data.R")
source("R/oao_spatial_filter.R")
source("R/leaflet_czechrep.R")
source("R/dt_meta.R")

# constants ----
sleep <- 0.4
url_da <- "https://digiarchiv.aiscr.cz/results?entity=projekt&f_organizace="
url_da_coords <- "https://digiarchiv.aiscr.cz/results?mapa=true&loc_rpt="
icon_link <- icon("fas fa-link")
icon_ext_link <- icon("fas fa-external-link-alt")
icon_map_link <<- icon("fas fa-map-marked-alt")


# ui funs -----------------------------------------------------------------

select_oao <- function(inputId, label, multiple = FALSE) {
  selectInput(inputId, label, 
              choices = c(Vyberte = "", 
                          setNames(oao_names_tab$ico, 
                                   oao_names_tab$nazev)),
              selectize = TRUE, multiple = multiple, width = "100%")
}

# data --------------------------------------------------------------------

oao_meta <- oao_meta("data/oao_meta.geojson")
oao_scope <- oao_sf("data/oao_scope.geojson")
oao_grid <- oao_sf("data/oao_grid.geojson")

ku_centroids <- oao_sf("data/ku.geojson") %>% 
  dplyr::arrange(ku) %>% 
  dplyr::transmute(ku = paste0(ku, " (okr. ", okr, ")"))

oao_rep <- oao_scope %>% 
  dplyr::filter(area >= 7.8e10) %>% 
  sf::st_drop_geometry()

oao_names_tab <- oao_meta %>% 
  dplyr::select(ico, nazev) %>% 
  dplyr::arrange(nazev)

oao_names_vec <- oao_names_tab$nazev %>% 
  setNames(oao_names_tab$ico)


# mapclick page -----------------------------------------------------------

# mapclick ui
mapclick_page <- div(
  fluidRow(
    column(
      6, fluidRow(
        column(
          6, HTML("<b>Kliknutím do mapy</b> zvolte bod zájmu, 
          případně se <b>přibližte na požadované katastrální území.</b> 
          Organizace, které jsou v dané oblasti oprávněny provádět 
          archeologický výzkum se zobrazí vpravo.")),
        column(
          6, selectInput("ku", "Katastrální území:", 
                         choices = c(Vyberte = "", ku_centroids$ku),
                         selectize = TRUE, width = "100%"))),
      leafletOutput("clickmap")),
    column(
      6, tabsetPanel(
        # tab: výzkumy v zadané vzdálenosti provádí (grid)
        tabPanel(
          "Výzkumy provádí",
          fluidRow(
            column(
              7, tags$div(
                style = 'padding: 15px;',
                "V okruhu ",
                tags$b(textOutput("buffer", inline = TRUE)),
                HTML("<b> km</b> archeologický výzkum v posledních <b>5 letech</b>
                prováděly organizace uvedené v tabulce níže.
                Organizace jsou řazeny sestupně dle počtu 
                archeologických výzkumů v zadané vzdálenosti."))),
            column(
              5, tags$div(
                style = 'padding-top: 10px;',
                sliderInput("buffer", "Zvolte vzdálenost",
                            min = 5, max = 20,
                            value = 5, step = 5,
                            ticks = FALSE, post = " km")))),
          h4("Výzkumy v zadané vzdálenosti"),
          tableOutput("tab_grid"),
          uiOutput("link_da_buffer")),
        # tab: oprávnění mají (poly)
        tabPanel(
          "Oprávnění mají",
          HTML("<div style = 'padding: 15px;'>
               Na zadaném území mohou archeologický 
               výzkum provádět organizace uvedené v tabulce níže.
               Organizace jsou řazeny vzestupně dle velikosti území, 
               na kterém jsou oprávněny provádět archeologický výzkum.</div>"),
          fluidRow(
            column(
              6, h4("Oprávnění ve vybrané oblasti"), 
              tableOutput("tab_poly")),
            column(
              6, h4("Oprávnění na celé území ČR"),
              tableOutput("tab_rep"))),
          uiOutput("link_da_cell"))),
      # link to the selected clickmap point 
      uiOutput("link_click", inline = TRUE))))

# mapclick server
mapclick_server <- function(input, output, session) {
  
  # reactives ----
  # selected ku
  ku <- reactive({
    input$ku
  })
  
  # change map zoom
  zoom <- reactive({
    input$clickmap_zoom
  })
  
  # coordinates of map click
  click <- reactive({
    
    if (isTruthy(input$ku) & !isTruthy(input$clickmap_click)) {
      ku_centroid <- dplyr::filter(ku_centroids, ku == input$ku)
      list(
        coords = list(
          lat = round(st_coordinates(ku_centroid)[[2]], 5),
          lng = round(st_coordinates(ku_centroid)[[1]], 5)),
        sf = ku_centroid)
    } else if (!isTruthy(input$ku) & isTruthy(input$clickmap_click)) { #!is.null(input$clickmap_click)
      list(
        coords = list(
          lat = round(as.numeric(input$clickmap_click$lat), 5),
          lng = round(as.numeric(input$clickmap_click$lng), 5)),
        sf = click2sf(input$clickmap_click))
    } else {
      NULL
    }
  })
  
  # x <- read_sf(here::here("app/data/ku.geojson"))
  # y <- x %>% dplyr::arrange(ku) %>% 
  #   dplyr::transmute(ku = paste0(ku, " (okr. ", okr, ")")) %>% 
  #   filter(ku == "Adolfovice (okr. Jeseník)")
  # st_geometry(y)
  # y
  
  # buffer around the click
  click_buffer_bbox <- reactive({
    click_buffer(click()$sf, input$buffer)
  })
  
  # box/cell of the given click
  click_cell_bbox <- reactive({
    click_cell(click()$sf)
  })
  
  
  # observers ----
  # zoom map to selected ku
  observeEvent(ku(), {
    leaflet_zoom(ku(), ku_centroids)
  })
  
  # clear ku after zoom
  observeEvent(zoom(), {
    updateSelectInput(inputId = "ku", selected = "")
  })
  
  # add click marker to the map after click
  observeEvent(click(), {
    leaflet_czechrep_add_marker(click()$coords)
  })
  
  # update click marker from url
  observe({
    url_lat <- get_query_param(field = "lat")
    url_lng <- get_query_param(field = "lng")
    
    if (!is.null(url_lat) & !is.null(url_lng)) {
      url_coords <- list(
        lat = round(as.numeric(url_lat), 5), 
        lng = round(as.numeric(url_lng), 5))
      
      leaflet_czechrep_add_marker(url_coords)
    }
  })
  
  # update url from click lat/lng
  observe({
    if (!is.null(click()$coords$lat) | !is.null(click()$coords$lng)) {
      change_page(
        paste0(
          "#!/?lat=", click()$coords$lat,
          "&lng=", click()$coords$lng))
    }
  })
  
  # remove click marker after ku zoom
  observeEvent(ku(), {
    leafletProxy("clickmap") %>%
      removeMarker("poi")
  })
  
  # outputs ----
  # clickmap - main map
  output$clickmap <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  # buffer text string
  output$buffer <- renderText({
    input$buffer
  })
  
  # tables ----
  # tab with filtered oao from polygon
  output$tab_poly <- renderTable({
    req(click())
    oao_filter_poly(oao_scope, click()$sf, oao_rep,
                    oao_names_vec, client_url())
  }, align = "cl", sanitize.text.function = function(x) x)
  
  # tab with filtered oao from grid
  output$tab_grid <- renderTable({
    req(click())
    oao_filter_grid(oao_grid, click()$sf, input$buffer,
                    oao_names_vec, client_url())
  }, align = "cl", sanitize.text.function = function(x) x)
  
  # tab with oao for whole country
  output$tab_rep <- renderTable({
    oao_rep %>% 
      dplyr::mutate(name = oao_names_vec[ico],
                    link = paste0("<a href='", client_url(), 
                                  "detail?oao=", ico, "/'>", 
                                  icon_map_link, "</a>")) %>% 
      dplyr::select(Detail = link, Organizace = name)
  }, align = "cl", sanitize.text.function = function(x) x)
  
  # links ---- 
  # link to the selected point
  output$link_click <- renderUI({
    req(click())
    HTML(
      paste0(
        "Link na tento bod: <a href=", client_url(), 
        "?lat=", click()$coords$lat,
        "&lng=", click()$coords$lng, ">", 
        icon_link, " ", client_url(), 
        "?lat=", click()$coords$lat,
        "&lng=", click()$coords$lng, "</a>"))
  })
  
  # link to DA based on buffer
  output$link_da_buffer <- renderUI({
    req(click())
    tagList(
      "Zobrazit vybranou oblast v ",
      tags$a(
        icon_ext_link, "Digitálním archivu AMČR",
        href = paste0(
          url_da_coords,
          click_buffer_bbox()[2], ",",
          click_buffer_bbox()[1], ",",
          click_buffer_bbox()[4], ",",
          click_buffer_bbox()[3],
          "&entity=projekt"),
        target = "_blank"))
  })
  
  # link to DA based on grid cell
  output$link_da_cell <- renderUI({
    req(click())
    tagList(
      "Zobrazit okolí vybraného bodu v ",
      tags$a(
        icon_ext_link, "Digitálním archivu AMČR",
        href = paste0(
          url_da_coords,
          click_cell_bbox()[2], ",",
          click_cell_bbox()[1], ",",
          click_cell_bbox()[4], ",",
          click_cell_bbox()[3],
          "&entity=projekt"),
        target = "_blank"))
  })
  
}


# details page ------------------------------------------------------------

# details ui
details_page <- div(
  fluidRow(
    column(
      4, select_oao("oao", label = "Organizace:", multiple = FALSE),
      fluidRow(
        column(
          4, checkboxInput("poly", "Zobrazit působnost", value = TRUE)),
        column(
          4, checkboxInput("grid", "Zobrazit akce", value = TRUE)),
        column(
          4, checkboxInput("addr", "Zobrazit adresu", value = FALSE))),
      tags$hr(),
      uiOutput("detail")),
    column(
      8, leafletOutput("map"))))

# details server
details_server <- function(input, output, session) {
  
  # reactives ----
  oao_scope_flt <- reactive({
    oao_filter(oao_scope, input$oao)
  })
  
  oao_grid_flt <- reactive({
    oao_filter(oao_grid, input$oao)
  })
  
  oao_meta_flt <- reactive({
    oao_filter(oao_meta, input$oao)
  })
  
  # observers ----
  # update selectInput from url
  observe({
    oao_url <- get_query_param(field = "oao")
    
    if (!is.null(oao_url)) {
      updateSelectInput(inputId = "oao", selected = oao_url)
    }
  })
  
  # update url from selectInput
  observe({
    if (input$oao != "") {
      change_page(paste0("detail?oao=", input$oao))
    }
  })
  
  # clear map when oao is switched
  observeEvent(input$oao, {
    leafletProxy("map") %>%
      clearShapes() %>%
      leaflet::setView(zoom = 8, lng = 15.4730, lat = 49.8175)
  })
  
  # show/hide address on map
  observe({
    if (!input$addr) {
      leafletProxy("map") %>%
        removeMarker("addr")
    } else {
      leafletProxy("map", data = oao_meta_flt()) %>%
        addMarkers(layerId = "addr", 
                   popup = paste0(
                     tags$b(oao_meta_flt()$nazev), tags$br(),
                     oao_meta_flt()$web, tags$br(), oao_meta_flt()$adresa))
    }
  })
  
  # show/hide polygon
  observe({
    if (!input$poly) {
      leafletProxy("map") %>%
        removeShape("poly")
    } else {
      leafletProxy("map", data = oao_scope_flt()) %>%
        addPolygons(layerId = "poly", fill = NA, color = "#3E3F3A", weight = 6)
    }
  })
  
  # show/hide grid
  observe({
    if (!input$grid) {
      leafletProxy("map") %>%
        removeShape(oao_grid_flt()$ctverec) %>%
        removeControl("legend")
    } else {
      pal <- colorNumeric(palette = "YlGnBu", domain = oao_grid_flt()$scaled)
      leafletProxy("map", data = oao_grid_flt()) %>%
        addPolygons(layerId = oao_grid_flt()$ctverec, color = ~pal(scaled),
                    stroke =  FALSE, fillOpacity = 0.6) %>%
        addControl("<img src='legend.png' width=110 height=40>",
                   position = "bottomleft", layerId = "legend")
    }
  })
  
  # outputs ----
  # text with details about oao
  output$detail <- renderText({
    req(input$oao)
    if (!is.na(oao_meta_flt()$spec_text)) {
      includeHTML(paste0("text/", oao_meta_flt()$spec_text, ".html"))
    } else {
      oao_meta_flt() %>% dplyr::transmute(
        text = HTML(paste0(
          "<h3>", nazev, "</h3>",
          "<p>", web, "</p>",
          "<p>IČO: ", ico, "</p>",
          "<h4>Adresa</h4>",
          "<p>", adresa, "</p>",
          "<h4>Detaily oprávnění</h4>",
          "<p>", opravneni, "</p>",
          if (!is.na(note)) {
            paste0("<p>", note, "</p>")
          },
          "<h4>Územní působnost</h4>",
          "<p>", uzemi, "</p>",
          "<p>Zobrazit projekty vybrané organizace v ",
          "<a href='", url_da, 
          stringr::str_replace_all(nazev_zkraceny, "\\s", "%20"), 
          "'>", 
          icon_ext_link, " Digitálním archivu AMČR", "</a><br>",
          "Tato stránka: <a href=", client_url(), "detail?oao=", ico, ">", 
          icon_link, " ", client_url(), "detail?oao=", ico, "</a>"))) %>% 
        dplyr::pull(text)
    }
  })
  
  # map
  output$map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
}


# list page ---------------------------------------------------------------

# list ui
list_page <- div(
  select_oao("oao_multiple", label = "Filtrovat organizace:", 
             multiple = TRUE),
  DT::dataTableOutput("table"),
  tags$p(
    style = "margin-top:6px;",
    "Celkem evidujeme ", textOutput("n_oao", inline = TRUE), " organizací."))

# list server
list_server <- function(input, output, session) {
  
  # reactives ----
  oao_meta_multi_flt <- reactive({
    oao_filter(oao_meta, input$oao_multiple)
  })
  
  # output ----
  # DT table
  output$table <- DT::renderDataTable({
    if (!is.null(input$oao_multiple)) {
      oao_meta_multi_flt() %>% 
        sf::st_drop_geometry() %>% 
        dt_data_prep(url = client_url()) %>% 
        dt_create()
    } else {
      oao_meta %>% 
        sf::st_drop_geometry() %>% 
        dt_data_prep(url = client_url()) %>% 
        dt_create()
    }
  })
  
  # nr of oao
  output$n_oao <- renderText({
    nrow(oao_meta)
  })
}


# about page --------------------------------------------------------------

# about ui
about_page <- div(
  fluidRow(
    column(3, includeMarkdown("text/about_left.md")),
    column(9, includeMarkdown("text/about_right.md"))),
  fluidRow(
    column(12, includeMarkdown("text/about_footer.md"))))


# server calls ------------------------------------------------------------

leaflet_map <- oao_scope %>% 
  leaflet_czechrep()


# navbar ------------------------------------------------------------------

menubar <- tags$nav(
  class = "navbar navbar-inverse navbar-static-top",
  # menu visible on large screens
  tags$div(
    class = "container-fluid hidden-xs hidden-sm hidden-md",
    tags$div(
      class = "navbar-header",
      tags$a(
        class = "navbar-brand", href = "#!/",
        icon("fas fa-map-marked-alt"), "Mapa archeologických organizací")),
    tags$ul(
      class = "nav navbar-nav",
      tags$li(
        a(href = route_link("/"),
          icon("fas fa-search"), "Hledej podle polohy")),
      tags$li(
        a(href = route_link("detail"),
          icon("fas fa-map"), "Mapa působnosti")),
      tags$li(
        a(href = route_link("list"),
          icon("fas fa-bars"), "Seznam organizací")),
      tags$li(
        a(href = "https://amcr-info.aiscr.cz/oznameni",
          target = "_blank",
          icon_ext_link, "Oznámit stavební záměr")),
      tags$li(
        a(href = route_link("about"),
          icon("fas fa-info-circle"), "O aplikaci"))),
    tags$div(
      class = "navbar-right navbar-logo",
      a(href = 'https://www.aiscr.cz/', target = '_blank',
        tags$img(src = 'AISCR_CZ_H_White.png', height = '60px')))),
  # menu visible on small screens
  tags$div(
    class = "container visible-xs visible-sm visible-md",
    tags$div(
      class = "navbar-header",
      tags$a(
        class = "navbar-brand", href = "#!/",
        icon("fas fa-map-marked-alt"), "Mapa OAO")),
    tags$ul(
      class = "nav navbar-nav",
      tags$li(
        a(href = route_link("/"),
          icon("fas fa-search"), "Hledej")),
      tags$li(
        a(href = route_link("detail"),
          icon("fas fa-map"), "Mapa")),
      tags$li(
        a(href = route_link("list"),
          icon("fas fa-bars"), "Seznam")),
      tags$li(
        a(href = route_link("about"),
          icon("fas fa-info-circle"), "Aplikace")))))


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  title = "Mapa OAO",
  theme = "main.css",
  tags$head(includeHTML("google-analytics.html")),
  menubar,
  router_ui(
    route("/", mapclick_page),
    route("detail", details_page),
    route("list", list_page),
    route("about", about_page)))


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  router_server()
  
  # get current url
  client_url <<- reactive({
    client <- reactiveValuesToList(session$clientData)
    paste0(client$url_protocol, "//",
           client$url_hostname, ":",
           client$url_port, client$url_pathname, "#!/")
  })
  
  mapclick_server(input, output, session)
  
  details_server(input, output, session)
  
  list_server(input, output, session)
  
  # greeter ----
  
  greeter <- modalDialog(
    title = "Organizace s oprávněním provádět archeologický výzkum",
    easyClose = TRUE, 
    size = "l",
    includeHTML("text/greeter.html"),
    footer = modalButton(label = "Zavřít"))
  
  showModal(greeter)
  
}


# app ---------------------------------------------------------------------

shinyApp(ui, server, enableBookmarking = "url")

