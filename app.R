library(shiny)
options(shiny.maxRequestSize = 200*1024^2)

library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(tools)

# ---- helpers ----
norm01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) return(rep(NA_real_, length(x)))
    (x - rng[1]) / (rng[2] - rng[1])
}

to_posix_from_plotly <- function(x) {
    if (is.numeric(x)) {
        if (x > 1e12) as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC")
        else          as.POSIXct(x,       origin = "1970-01-01", tz = "UTC")
    } else {
        suppressWarnings(ymd_hms(x, tz = "UTC", quiet = TRUE))
    }
}

# Robust reader for uploaded annotations (expects columns: datetime, label)
read_annotations_csv <- function(path) {
    ann <- read_csv(path, show_col_types = FALSE, progress = FALSE)
    req(all(c("datetime", "label") %in% names(ann)))
    # Parse datetime to UTC POSIXct (accepts ISO-8601 or "YYYY-MM-DD HH:MM:SS")
    if (!inherits(ann$datetime, "POSIXt")) {
        parsed <- suppressWarnings(ymd_hms(ann$datetime, tz = "UTC", quiet = TRUE))
        if (all(is.na(parsed))) {
            parsed <- suppressWarnings(parse_datetime(ann$datetime, locale = locale(tz = "UTC")))
        }
        validate(need(!all(is.na(parsed)), "Could not parse 'datetime' in uploaded annotations CSV."))
        ann$datetime <- parsed
    } else {
        attr(ann$datetime, "tzone") <- "UTC"
    }
    ann %>%
        transmute(datetime = .data$datetime, label = as.character(.data$label)) %>%
        distinct(datetime, label, .keep_all = FALSE) %>%
        arrange(datetime)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

ui <- fluidPage(
    h3("Interactive Plot Annotation"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "file_joined",
                "Upload joined CSV (with datetime + CO₂/CH₄ columns)",
                accept = c(".csv")
            ),
            helpText(HTML(
                "CSV must include <code>datetime</code>, <code>lgr_CO2_mean</code>, ",
                "<code>spl_conc_mean</code>, <code>lgr_CH4_mean</code>, ",
                "<code>lpl_conc_mean</code>."
            )),
            tags$hr(),
            # ---- NEW: Upload existing annotations to continue working ----
            fileInput(
                "file_ann",
                "Upload annotations CSV (to continue annotating)",
                accept = c(".csv")
            ),
            helpText(HTML(
                "Annotations CSV must include <code>datetime</code> (UTC) and <code>label</code>."
            )),
            tags$hr(),
            downloadButton("download_annotations", "Download annotations (CSV)")
        ),
        mainPanel(
            plotlyOutput("plot", height = "480px"),
            tags$br(),
            strong("Annotations (sorted, de-duplicated):"),
            verbatimTextOutput("clickinfo")
        )
    )
)

server <- function(input, output, session) {
    # Track uploaded filename (base, no extension) for download naming
    rv <- reactiveValues(
        clicked_time = NULL,
        annotations = data.frame(datetime = as.POSIXct(character()), label = character()),
        label_choices = c("COM3-OPEN", "COM3-CLOSE"),
        uploaded_base = "data",
        xrange = NULL
    )
    
    observeEvent(input$file_joined, {
        if (!is.null(input$file_joined$name)) {
            rv$uploaded_base <- file_path_sans_ext(basename(input$file_joined$name))
        }
    }, ignoreInit = TRUE)
    
    # ---- Reactive: load the joined CSV ----
    df_joined <- reactive({
        req(input$file_joined)
        df <- read_csv(input$file_joined$datapath, show_col_types = FALSE)
        
        required <- c("datetime", "lgr_CO2_mean", "spl_conc_mean", "lgr_CH4_mean", "lpl_conc_mean")
        missing <- setdiff(required, names(df))
        validate(need(length(missing) == 0,
                      paste("Missing columns in CSV:", paste(missing, collapse = ", "))))
        
        if (!inherits(df$datetime, "POSIXt")) {
            parsed <- suppressWarnings(ymd_hms(df$datetime, tz = "UTC", quiet = TRUE))
            if (all(is.na(parsed))) {
                parsed <- suppressWarnings(parse_datetime(df$datetime, locale = locale(tz = "UTC")))
            }
            validate(need(!all(is.na(parsed)), "Could not parse 'datetime'. Use ISO-8601 or YYYY-MM-DD HH:MM:SS."))
            df$datetime <- parsed
        } else {
            attr(df$datetime, "tzone") <- "UTC"
        }
        
        arrange(df, datetime)
    })
    
    # ---- Reactive: normalized data frame ----
    df_norm <- reactive({
        df <- df_joined()
        df %>%
            mutate(
                lgr_CO2_norm = norm01(lgr_CO2_mean),
                spl_CO2_norm = norm01(spl_conc_mean),
                lgr_CH4_norm = norm01(lgr_CH4_mean),
                lpl_CH4_norm = norm01(lpl_conc_mean)
            )
    })
    
    # ---- De-duplicated view of annotations (by datetime + label) ----
    ann_dedup <- reactive({
        if (nrow(rv$annotations) == 0) return(rv$annotations)
        rv$annotations %>%
            distinct(datetime, label, .keep_all = FALSE) %>%
            arrange(datetime)
    })
    
    # ---- Plot ----
    output$plot <- renderPlotly({
        req(df_norm())
        dfn <- df_norm()
        
        plt <- plot_ly(dfn, x = ~datetime) %>%
            add_lines(y = ~lgr_CO2_norm, name = "LGR CO₂",  line = list(color = 'red')) %>%
            add_lines(y = ~spl_CO2_norm, name = "SPL CO₂ proxy", line = list(color = 'orange')) %>%
            add_lines(y = ~lgr_CH4_norm, name = "LGR CH₄", line = list(color = 'blue')) %>%
            add_lines(y = ~lpl_CH4_norm, name = "LPL CH₄ proxy", line = list(color = 'green')) %>%
            layout(
                title = "Normalized CO₂ and CH₄ Concentrations",
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Normalized Value (0–1)", range = c(0, 1)),
                edits = list(
                    annotationPosition = TRUE,  # prevent dragging labels
                    shapePosition = TRUE       # allow dragging vertical lines
                ),
                uirevision = "keep"           # <- preserve UI state across re-renders
            ) %>%
            config(editable = TRUE)
        
        ann_sorted <- ann_dedup()
        if (nrow(ann_sorted) > 0) {
            # 1) non-draggable vertical markers as segments
            vdat <- data.frame(
                x    = ann_sorted$datetime,
                xend = ann_sorted$datetime,
                y    = 0,
                yend = 1
            )
            plt <- plt %>%
                add_segments(
                    data = vdat,
                    x = ~x, xend = ~xend, y = ~y, yend = ~yend,
                    hoverinfo = "skip",
                    showlegend = FALSE,
                    line = list(color = "black", dash = "dot"),
                    inherit = FALSE
                )
            
            # 2) draggable labels (annotations only)
            annos <- lapply(seq_len(nrow(ann_sorted)), function(i) {
                list(
                    x = ann_sorted$datetime[i],
                    y = 1.02,
                    text = ann_sorted$label[i],
                    xref = "x", yref = "paper",
                    showarrow = FALSE,
                    textangle = -90,
                    font = list(size = 10)
                )
            })
            plt <- plt %>% layout(annotations = annos)
        }
        
        # Preserve last zoom/pan on x if available
        if (!is.null(rv$xrange) && length(rv$xrange) == 2) {
            plt <- plt %>% layout(xaxis = list(range = rv$xrange))
        }
        plt
    })
    
    # ---- Click to annotate: modal with select + new label ----
    observeEvent(event_data("plotly_click"), {
        click <- event_data("plotly_click")
        if (is.null(click)) return()
        rv$clicked_time <- to_posix_from_plotly(click$x)
        
        showModal(modalDialog(
            title = "Add Annotation",
            div(
                selectInput("label_select", "Choose a label",
                            choices = rv$label_choices, selected = rv$label_choices[1]),
                textInput("label_new", "Or enter a new label", placeholder = "Type a new label (optional)")
            ),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("save_label", "Save", class = "btn-primary")
            ),
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$save_label, {
        req(rv$clicked_time)
        new_text <- trimws(input$label_new %||% "")
        chosen <- if (nzchar(new_text)) new_text else input$label_select
        
        if (nzchar(new_text) && !(new_text %in% rv$label_choices)) {
            rv$label_choices <- c(rv$label_choices, new_text)
        }
        
        # Append then de-duplicate (single row per datetime+label)
        rv$annotations <- bind_rows(
            rv$annotations,
            data.frame(datetime = rv$clicked_time, label = chosen, stringsAsFactors = FALSE)
        ) %>%
            distinct(datetime, label, .keep_all = FALSE) %>%
            arrange(datetime)
        
        removeModal()
    })
    
    # This to save the last plot layout
    observeEvent(event_data("plotly_relayout"), {
        ev <- event_data("plotly_relayout")
        if (is.null(ev) || length(ev) == 0) return()
        
        # Keep your saved x-range behavior
        if (isTRUE(ev[["xaxis.autorange"]])) {
            rv$xrange <- NULL
        }
        if (!is.null(ev[["xaxis.range[0]"]]) && !is.null(ev[["xaxis.range[1]"]])) {
            xr0 <- to_posix_from_plotly(ev[["xaxis.range[0]"]])
            xr1 <- to_posix_from_plotly(ev[["xaxis.range[1]"]])
            if (!any(is.na(c(xr0, xr1)))) rv$xrange <- c(xr0, xr1)
        }
        
        ann <- ann_dedup()
        if (nrow(ann) == 0) return()
        
        # Only process X moves for annotations; ignore .y to keep horizontal-only behavior
        ann_x_keys <- grep("^annotations\\[[0-9]+\\]\\.x$", names(ev), value = TRUE)
        if (length(ann_x_keys) == 0) return()
        
        update_ann_at_index <- function(i, new_x) {
            new_time <- to_posix_from_plotly(new_x)
            if (is.na(new_time)) return()
            
            # snap to nearest 5s
            new_time <- round_to_5s(new_time)
            
            ann <- ann_dedup()
            old_time <- ann$datetime[i]
            lbl <- ann$label[i]
            
            rv$annotations <- rv$annotations %>%
                mutate(
                    datetime = if_else(
                        datetime == old_time & label == lbl,
                        as.POSIXct(new_time, tz = "UTC"),
                        datetime
                    )
                ) %>%
                distinct(datetime, label, .keep_all = FALSE) %>%
                arrange(datetime)
            
            attr(rv$annotations$datetime, "tzone") <- "UTC"
        }
        
        for (k in ann_x_keys) {
            # Plotly annotation indices are 0-based
            i <- as.integer(sub("^annotations\\[([0-9]+)\\]\\.x$", "\\1", k)) + 1L
            if (i >= 1 && i <= nrow(ann)) update_ann_at_index(i, ev[[k]])
        }
        
        # We intentionally do nothing with keys like "annotations[i].y"
        # so vertical drags are ignored and labels snap back to y=1.02.
    }, ignoreInit = TRUE)
    
    # ---- NEW: Load existing annotations from CSV and merge ----
    observeEvent(input$file_ann, {
        req(input$file_ann$datapath)
        uploaded <- read_annotations_csv(input$file_ann$datapath)
        
        # Merge with current session annotations, de-duplicate, sort
        rv$annotations <- bind_rows(rv$annotations, uploaded) %>%
            distinct(datetime, label, .keep_all = FALSE) %>%
            arrange(datetime)
        
        # Expand label choices with any new labels found in file
        new_labels <- setdiff(unique(uploaded$label), rv$label_choices)
        if (length(new_labels) > 0) {
            rv$label_choices <- c(rv$label_choices, new_labels)
        }
    }, ignoreInit = TRUE)
    
    # ---- Sorted table of annotations ----
    output$clickinfo <- renderPrint({
        ann_dedup()
    })
    
    # ---- Round to 5s ----
    round_to_5s <- function(time) {
        secs <- as.numeric(time)              # seconds since epoch
        snapped <- round(secs / 5) * 5        # nearest 5-second multiple
        as.POSIXct(snapped, origin = "1970-01-01", tz = "UTC")
    }
    
    # ---- Download merged annotations (sorted, de-duplicated) ----
    output$download_annotations <- downloadHandler(
        filename = function() {
            paste0(rv$uploaded_base, "_annotations_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            ann <- ann_dedup()
            if (nrow(ann) == 0) {
                ann <- data.frame(datetime = as.POSIXct(character()), label = character())
            }
            # Write UTC ISO-8601
            ann$datetime <- format(ann$datetime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            write.csv(ann, file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)