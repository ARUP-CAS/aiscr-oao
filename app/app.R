# app

# note: remote shiny server restarts the app only when file app.R has changes
#

# packages ----------------------------------------------------------------

library(shiny)
library(shiny.router)
library(leaflet)
library(DT)
library(dplyr)
library(sf)
library(stringr)

source("R/data.R")
source("R/oao_spatial_filter.R")
source("R/leaflet_czechrep.R")
source("R/dt_meta.R")


# constant ----------------------------------------------------------------

sleep <- 0.4
url_da <- "https://digiarchiv.aiscr.cz/results?entity=projekt&f_organizace="
url_da_coords <- "https://digiarchiv.aiscr.cz/results?mapa=true&loc_rpt="
icon_ext_link <- icon("fas fa-external-link-alt") # "fas fa-link"
icon_map_link <<- icon("fas fa-map-marked-alt")


# ui funs -----------------------------------------------------------------

select_oao <- function(inputId, label, multiple = FALSE) {
  selectInput(inputId, label, 
              choices = c(Vyberte = "", 
                          setNames(oao_names_tab$ico, 
                                   oao_names_tab$nazev)),
              selectize = TRUE, multiple = multiple, 
              width = "100%")
}

# data input --------------------------------------------------------------

oao_meta <- oao_meta("data/oao_meta.geojson")
oao_scope <- oao_sf("data/oao_scope.geojson")
oao_grid <- oao_sf("data/oao_grid.geojson")

oao_rep <- oao_scope %>% 
  dplyr::filter(area >= 7.8e10) %>% 
  sf::st_drop_geometry()

oao_names_tab <- oao_meta %>% 
  dplyr::select(ico, nazev) %>% 
  dplyr::arrange(nazev)

oao_names_vec <- oao_names_tab$nazev %>% 
  setNames(oao_names_tab$ico)


# navbar ui ---------------------------------------------------------------

menubar <- tags$nav(
  class = "navbar navbar-inverse navbar-static-top",
  # menu visible on large screens
  tags$div(
    class = "container-fluid hidden-xs hidden-sm hidden-md",
    tags$div(
      class = "navbar-header",
      tags$a(
        class = "navbar-brand", href = "#!/",
        icon("fas fa-map-marked-alt"), "Mapa archeologických organizací")
    ),
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
          icon("fas fa-info-circle"), "O aplikaci")),
    ),
    tags$div(
      class = "navbar-right navbar-logo",
      a(href = 'https://www.aiscr.cz/', target = '_blank',
        tags$img(src = 'AISCR_CZ_H_White.png', height = '60px')))
  ),
  # menu visible on small screens
  tags$div(
    class = "container visible-xs visible-sm visible-md",
    tags$div(
      class = "navbar-header",
      tags$a(
        class = "navbar-brand", href = "#!/",
        icon("fas fa-map-marked-alt"), "Mapa OAO")
    ),
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
          icon("fas fa-info-circle"), "Aplikace")),
    )
  )
)

# mapclick page -----------------------------------------------------------

# mapclick ui
mapclick_page <- div(
  fluidRow(
    column(
      6, leafletOutput("clickmap")
    ),
    column(
      6, tabsetPanel(
        # výzkumy v zadané vzdálenosti provádí tab (grid)
        tabPanel(
          "Výzkumy provádí",
          fluidRow(
            column(
              7, tags$div(
                style = 'padding: 15px;',
                tags$b("Kliknutím do mapy zvolte bod zájmu."),
                "V okruhu ",
                textOutput("buffer", inline = TRUE),
                " km archeologický výzkum v posledních 5 letech
                prováděly organizace uvedené v tabulce níže.
                Organizace jsou řazeny sestupně dle počtu 
                archeologických výzkumů v zadané vzdálenosti.")
            ),
            column(
              5, tags$div(
                style = 'padding-top: 10px;',
                sliderInput("buffer", "Zvolte vzdálenost",
                            min = 5, max = 20,
                            value = 5, step = 5,
                            ticks = FALSE, post = " km")
              )
            )
          ),
          h4("Výzkumy v zadané vzdálenosti"),
          tableOutput("tab_grid"),
          uiOutput("link_da_buffer")
        ),
        # oprávnění mají tab (poly)
        tabPanel(
          "Oprávnění mají",
          HTML("<div style = 'padding: 15px;'>
               <b>Kliknutím do mapy zvolte bod zájmu.</b>
               Na zadaném území mohou archeologický 
               výzkum provádět organizace uvedené v tabulce níže.
               Organizace jsou řazeny vzestupně dle velikosti území, 
               na kterém jsou oprávněny provádět archeologický výzkum.</div>"),
          fluidRow(
            column(
              6, h4("Oprávnění ve vybrané oblasti"), 
              tableOutput("tab_poly")
            ),
            column(
              6, h4("Oprávnění na celé území ČR"),
              tableOutput("tab_rep")
            )
          ),
          uiOutput("link_da_cell")
        )
      )
    )
  )
)

# mapclick server
mapclick_server <- function(input, output, session) {
  
  # buffer text string
  output$buffer <- renderText({
    input$buffer
  })
  
  # main map
  output$clickmap <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  # coordinates of map click
  click_sf <- reactive({
    click2sf(input$clickmap_click)
  })
  
  # add click marker to the map
  observeEvent(input$clickmap_click, {
    leaflet_czechrep_add_marker(input$clickmap_click)
  })
  
  # filter oao
  output$tab_poly <- renderTable({
    req(input$clickmap_click)
    oao_filter_poly(oao_scope, click_sf(), oao_rep, 
                    oao_names_vec, client_url())
  }, align = "cl", sanitize.text.function = function(x) x)
  
  output$tab_grid <- renderTable({
    req(input$clickmap_click)
    oao_filter_grid(oao_grid, click_sf(), input$buffer, 
                    oao_names_vec, client_url())
  }, align = "cl", sanitize.text.function = function(x) x)
  
  output$tab_rep <- renderTable({
    oao_rep %>% 
      dplyr::mutate(name = oao_names_vec[ico],
                    link = paste0("<a href='", client_url(), 
                                  "detail?oao=", ico, "/'>", 
                                  icon_map_link, "</a>")) %>% 
      dplyr::select(Detail = link, Organizace = name)
  }, align = "cl", sanitize.text.function = function(x) x)
  
  # links to da
  # buffer around the click
  click_buffer_bbox <- reactive({
    click_buffer(click_sf(), input$buffer)
  })
  
  output$link_da_buffer <- renderUI({
    req(input$clickmap_click)
    tagList(
      "Zobrazit vybranou oblast v ",
      tags$a(
        icon_ext_link, "Digitálním archivu AMČR.",
        href = paste0(url_da_coords,
                      click_buffer_bbox()[2], ",",
                      click_buffer_bbox()[1], ",",
                      click_buffer_bbox()[4], ",",
                      click_buffer_bbox()[3],
                      "&entity=projekt"),
        target = "_blank"
      ))
  })
  
  # box/cell of the given click
  click_cell_bbox <- reactive({
    click_cell(click_sf())
  })
  
  output$link_da_cell <- renderUI({
    req(input$clickmap_click)
    tagList(
      "Zobrazit okolí vybraného bodu v ",
      tags$a(
        icon_ext_link, "Digitálním archivu AMČR.",
        href = paste0(url_da_coords,
                      click_cell_bbox()[2], ",",
                      click_cell_bbox()[1], ",",
                      click_cell_bbox()[4], ",",
                      click_cell_bbox()[3],
                      "&entity=projekt"),
        target = "_blank"
      ))
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
          6, checkboxInput("poly", "Zobrazit územní působnost", value = TRUE)
        ),
        column(
          6, checkboxInput("grid", "Zobrazit akce", value = TRUE)
        )
      ),
      tags$hr(),
      uiOutput("detail")
    ),
    column(
      8, leafletOutput("map")
    )
  )
)

# details server
details_server <- function(input, output, session) {
  
  # reactive data
  oao_scope_flt <- reactive({
    oao_filter(oao_scope, input$oao)
  })
  
  oao_grid_flt <- reactive({
    oao_filter(oao_grid, input$oao)
  })
  
  oao_meta_flt <- reactive({
    oao_filter(oao_meta, input$oao)
  })
  
  # text details
  output$detail <- renderText({
    req(input$oao)
    if (!is.na(oao_meta_flt()$spec_text)) {
      includeHTML(paste0("text/", oao_meta_flt()$spec_text, ".html"))
    } else {
      oao_meta_flt() %>% dplyr::transmute(
        text = HTML(paste0(
          "<h3>", nazev, "</h3>",
          "<p>Webové stránky: ", web, "</p>",
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
          icon_ext_link, " Digitálním archivu AMČR", "</a>."
        ))) %>% 
        dplyr::pull(text)
    }
  })
  
  # map
  output$map <- renderLeaflet({
    Sys.sleep(sleep)
    leaflet_map
  })
  
  # clear map when oao is switched
  observeEvent(input$oao, {
    leafletProxy("map") %>%
      clearShapes() %>%
      leaflet::setView(zoom = 8, lng = 15.4730, lat = 49.8175)
  })
  
  # show/hide poly
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
}


# list page ---------------------------------------------------------------

list_page <- div(
  select_oao("oao_multiple", label = "Filtrovat organizace:", 
             multiple = TRUE),
  DT::dataTableOutput("table"),
  tags$p(style = "margin-top:6px;",
         "Celkem evidujeme ", textOutput("n_oao", inline = TRUE), " organizací.")
)

# list server
list_server <- function(input, output, session) {
  
  oao_meta_multi_flt <- reactive({
    oao_filter(oao_meta, input$oao_multiple)
  })
  
  # DT table
  output$table <- DT::renderDataTable({
    if (!is.null(input$oao_multiple)) {
      oao_meta_multi_flt() %>% 
        dt_data_prep(url = client_url()) %>% 
        dt_create()
    } else {
      oao_meta %>% 
        dt_data_prep(url = client_url()) %>% 
        dt_create()
    }
  })
  
  output$n_oao <- renderText({
    nrow(oao_meta)
  })
}


# about page --------------------------------------------------------------

about_page <- div(
  fluidRow(
    column(
      3, includeMarkdown("text/about_left.md")
    ),
    column(
      9, includeMarkdown("text/about_right.md")
    )
  ),
  fluidRow(
    column(
      12, includeMarkdown("text/about_footer.md")
    )
  )
)


# server calls ------------------------------------------------------------

leaflet_map <- oao_scope %>% 
  leaflet_czechrep()


# router ------------------------------------------------------------------

router <- make_router(
  route("/", mapclick_page, mapclick_server),
  route("detail", details_page, details_server),
  route("list", list_page, list_server),
  route("about", about_page)
)


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  title = "Mapa OAO",
  theme = "main.css",
  tags$head(includeHTML("google-analytics.html")),
  menubar,
  router$ui,
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # get current url
  client_url <<- reactive({
    client <- reactiveValuesToList(session$clientData)
    paste0(client$url_protocol, "//",
           client$url_hostname, ":",
           client$url_port, client$url_pathname, "#!/")
  })
  
  # observe({
  #   print(
  #     # reactiveValuesToList(session$clientData)
  #     client_url()
  #     )
  # })
  
  router$server(input, output, session)
  
  # greeter ---------------------------------------------------------------
  
  greeter <- modalDialog(
    title = "Organizace s oprávněním provádět archeologický výzkum",
    easyClose = TRUE, 
    size = "l",
    includeHTML("text/greeter.html"),
    footer = modalButton(label = "Zavřít")
  )
  
  showModal(greeter)
  
  # parsing url parameter ?oao=ico
  observe({
    oao_url <- get_query_param(field = "oao")
    
    if (!is.null(oao_url)) {
      updateSelectInput(inputId = "oao", selected = oao_url)
    }
  })
}


# app ---------------------------------------------------------------------

shinyApp(ui, server, enableBookmarking = "url")

