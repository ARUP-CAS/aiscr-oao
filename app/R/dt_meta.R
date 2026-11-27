
#' Create DT
#'
#' @param data Prepared tibble with metadata. 
#'
#' @return
#' @export
#'
#' @examples
dt_create <- function(data) {
  # 0-based DataTables column index from column names, so targets and JS
  # below follow the column order set in dt_data_prep()
  idx <- function(cols) match(cols, names(data)) - 1L
  hide_small <- paste(idx(c("web", "email", "telefon")), collapse = ", ")
  # cell text wraps to new lines beyond `em` width (display only; search,
  # sort and filters use the raw value, or the tag-free text if strip_html)
  wrap_js <- function(em, strip_html = FALSE) DT::JS(
    "function(data, type) {",
    "  if (data == null) data = '';",
    sprintf("  if (type === 'display') return '<div style=\"max-width:%gem;white-space:normal\">' + data + '</div>';", em),
    if (strip_html) "  return $('<div>').html(data).text().trim();" else "  return data;",
    "}")

  dt <- data |>
    DT::datatable(
      escape = FALSE,
      extensions = 'Scroller',
      rownames = FALSE,
      filter = list(position = "top", clear = TRUE, plain = FALSE),
      callback = DT::JS(
        # keep selected values of dropdown filters visible as "buttons";
        # DT by default hides them on blur and shows the JSON text instead
        "$(table.table().container()).find('thead td[data-type=\"factor\"]').each(function() {",
        "  var $x = $(this).children('div').last();",
        "  var $input = $(this).children('div').first().children('input');",
        # skip the header copy Scroller puts in the scroll body (no <select>)
        "  var sel = ($x.children('select')[0] || {}).selectize;",
        "  if (!sel) return;",
        "  sel.settings.placeholder = 'Vše'; sel.updatePlaceholder();",
        "  sel.off('blur');",
        # while a dropdown is open ('show'), DT lets it overflow the header,
        # but also makes the body overflow visible and the outer container
        # scrollable (a scrollX workaround); the wheel then scrolls the header
        # away with the rows, so keep the body as the only scroller
        "  $x.on('show', function() {",
        "    $(table.table().container()).find('.dataTables_scrollBody').css('overflow', 'auto');",
        "    $(table.table().container()).find('.dataTables_scroll').css('overflow-x', '');",
        "  });",
        # the chip stays visible after blur, so fire DT's 'show'/'hide' on
        # focus/blur ourselves to clip and unclip the header correctly
        "  sel.on('focus', function() { $x.trigger('show'); });",
        "  sel.on('blur', function() {",
        "    if (sel.getValue().length === 0) {",
        "      $x.hide(); $input.parent().show();",
        "    }",
        "    $x.trigger('hide');",
        "    $input.trigger('blur');",
        "  });",
        "});",
        # hide web, email and phone on windows up to 1440 px wide (laptops, tablets, phones)
        "var mq = window.matchMedia('(max-width: 1440px)');",
        "function fitColumns() {",
        sprintf("  table.columns([%s]).visible(!mq.matches, false);", hide_small),
        "  table.columns.adjust().draw(false);",
        "}",
        "fitColumns();",
        "mq.addEventListener('change', fitColumns);"
      ),
      colnames = c(
        "Organizace" = "label",
        "AMČR ID" = "amcr_id",
        "Typ organizace" = "typ",
        "Zřizovatel" = "zrizovatel",
        "Typ zřizovatele" = "typ_zrizovatele",
        # "IČO" = "ico",
        "Webové stránky" = "web",
        "Email" = "email",
        "Telefon" = "telefon",
        # "Adresa" = "adresa",
        "Platnost oprávnění<br>MK ČR" = "mk_to",
        "Platnost dohody<br>s AV ČR" = "av_to",
        "Dohoda o<br>užívání AMČR" = "amcr"),
      options = list(
        dom = "ti",
        # Czech summary line under the table: number of displayed organisations
        # and their breakdown by "Typ organizace"; follows filters
        infoCallback = DT::JS(
          "function(settings, start, end, max, total) {",
          "  var api = new $.fn.dataTable.Api(settings);",
          "  if (total === 0) return 'Filtru neodpovídá žádná organizace (celkem ' + max + ')';",
          "  var counts = {};",
          sprintf("  api.column(%d, {search: 'applied'}).data().each(function(v) {", idx("typ")),
          "    counts[v] = (counts[v] || 0) + 1;",
          "  });",
          "  var parts = Object.keys(counts).sort(function(a, b) {",
          "    return counts[b] - counts[a] || a.localeCompare(b, 'cs');",
          "  }).map(function(k) { return k + ': ' + counts[k]; });",
          "  var head = total === max ? 'Celkem ' + max + ' organizací' :",
          "    'Zobrazeno ' + total + ' z ' + max + ' organizací';",
          "  return head + ' — ' + parts.join(' · ');",
          "}"
        ),
        deferRender = TRUE,
        scrollY = "calc(100vh - 340px)",
        scroller = TRUE,
        columnDefs = list(
          # organisation name is a link to its detail page; search and sort
          # run on the plain name, not on the HTML
          list(targets = idx("label"), render = wrap_js(16, strip_html = TRUE)),
          list(targets = idx("typ"), render = wrap_js(9)),
          list(targets = idx("zrizovatel"), render = wrap_js(12)),
          list(className = 'dt-center', targets = idx(c("mk_to", "av_to", "amcr"))),
          # column filters only on: Organizace, AMČR ID, Typ organizace,
          # Zřizovatel, Typ zřizovatele, Platnost MK ČR, Platnost AV ČR
          list(searchable = FALSE, targets = idx(c("web", "email", "telefon", "amcr"))))
      )
    )

  # Czech placeholder in the column filter row
  dt$x$filterHTML <- gsub('placeholder="All"', 'placeholder="Vše"',
                          dt$x$filterHTML, fixed = TRUE)
  dt
}


#' Prepare data for DT
#'
#' @param data A tibble with metadata.
#'
#' @return
#' @export
#'
#' @examples
dt_data_prep <- function(data, url) {
  data %>% 
    dplyr::mutate(
      # adresa = str_replace(adresa, ", ", "<br>"),
      # multi-select column filters (DT offers a dropdown for factors)
      dplyr::across(
        c("typ", "zrizovatel", "typ_zrizovatele"),
        \(x) factor(x, levels = stringr::str_sort(unique(stats::na.omit(x)), locale = "cs"))),
      dplyr::across(
        dplyr::ends_with(c("from", "to")), 
        \(x) as.Date(x, format = "%d. %m. %Y")),
      telefon = stringr::str_replace_all(telefon, ",\\s", "<br>"),
      mk_to = dplyr::if_else(is.na(mk_to) & mk_neomezena, "na dobu<br>neurčitou", as.character(mk_to)),
      av_to = dplyr::if_else(is.na(av_to) & av_neomezena, "na dobu<br>neurčitou", as.character(av_to)),
      # organisation name links to its detail page, map icon in front
      label = paste0("<a href='", url, "detail?oao=", amcr_id, "/'>",
                     icon_map_link, " ", label, "</a>"),
      amcr = if_else(amcr, "&#x2714;", "&#10008;")) %>% 
    dplyr::select(label, amcr_id, typ, zrizovatel, typ_zrizovatele, web, email, telefon, mk_to, av_to, amcr)
}

