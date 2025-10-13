# File: harmonized_analysis_rm.R - Column filtering, plotting, and spatial selection for harmonized dataset

# OUTPUT harmonized_gt: full dataset table

output$harmonized_gt <- DT::renderDataTable({
   req(harmonized_data())
   harmonized_data()
})


output$filtered_harmonized_gt <- DT::renderDataTable({ # OUTPUT filtered table
   req(filtered_data())
   filtered_data()
})

# Reactive data based on selected columns
selected_data <- reactive({ # REACTIVE subset of chosen columns
   req(input$select_columns)
   harmonized_data() %>% select(all_of(input$select_columns))
})


output$filter_cols_ui <- renderUI({ # OUTPUT column picker UI
   req(harmonized_data())
   harmo_df <- harmonized_data()
   selectInput(
      inputId = "select_columns",
      label = "Select Columns of Interest",
      choices = colnames(harmo_df),
      multiple = TRUE,
      selected = c(
         "Parent.ID", "Site.Coordinates",
         "Owen.s.Notation.for.sequencing"
      ),
      width = "100%"
   )
})

# Step 2: Generate filter UI based on selected columns
output$filter_inputs_ui <- renderUI({ # OUTPUT dynamic filter controls
   req(harmonized_data())
   df <- harmonized_data()
   req(input$select_columns)

   build_block <- function(id, label_txt, control, slider = FALSE) {
      htmltools::div(
         class = paste("filter-block", if (slider) "filter-block-slider" else "filter-block-other"),
         style = "margin-bottom:14px; padding:4px 6px 8px 14px; border-left:3px solid #ddd;",
         htmltools::tags$label(`for` = id, style = "font-weight:600; display:block; margin-bottom:4px;", label_txt),
         control
      )
   }

   controls <- lapply(input$select_columns, function(col) {
      id <- paste0("filter_", col)
      col_data <- df[[col]]
      if (is.null(col_data)) {
         return(NULL)
      }
      if (is.numeric(col_data)) {
         rng <- range(col_data, na.rm = TRUE)
         if (!all(is.finite(rng))) rng <- c(0, 1)
         span <- diff(rng)
         # Determine display precision based on span
         disp_digits <- if (!is.finite(span) || span == 0) {
            2
         } else if (span >= 100) {
            0
         } else if (span >= 1) {
            2
         } else if (span >= 0.01) {
            3
         } else {
            4
         }
         fmt_num <- function(x) {
            out <- formatC(x, format = "f", digits = disp_digits, drop0trailing = TRUE)
            sub("\\.$", "", out)
         }
         pretty_min <- as.numeric(fmt_num(rng[1]))
         pretty_max <- as.numeric(fmt_num(rng[2]))
         step_val <- if (span > 0) signif(span / 100, digits = 2) else 1
         if (!is.finite(step_val) || step_val <= 0) step_val <- 1
         ctrl <- sliderInput(id, label = NULL, min = pretty_min, max = pretty_max, value = c(pretty_min, pretty_max), step = step_val, ticks = FALSE, width = "100%")
         build_block(id, col, ctrl, slider = TRUE)
      } else if (is.factor(col_data) || is.character(col_data)) {
         choices <- sort(unique(col_data))
         ctrl <- pickerInput(id, label = NULL, choices = choices, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))
         build_block(id, col, ctrl, slider = FALSE)
      } else {
         NULL
      }
   })

   controls <- Filter(Negate(is.null), controls)

   htmltools::tagList(controls) # Styling consolidated into www/app.css
})

## Note: dynamic selected range text intentionally omitted (lifecycle ordering caused missing outputs previously).
# Reactive data after filtering
filtered_data <- reactive({ # REACTIVE apply filters to selection
   req(input$select_columns)
   req(harmonized_data())
   filtered <- selected_data()
   harmo_df <- harmonized_data()

   for (col in input$select_columns) {
      filter_var <- input[[paste0("filter_", col)]]
      if (is.numeric(harmo_df[[col]]) && !is.null(filter_var)) {
         filtered <- filtered %>% dplyr::filter(.data[[col]] >= filter_var[1] & .data[[col]] <= filter_var[2])
      } else if ((is.factor(harmo_df[[col]]) || is.character(harmo_df[[col]])) && !is.null(filter_var)) {
         filtered <- filtered %>% dplyr::filter(.data[[col]] %in% filter_var)
      }
   }
   filtered
})

output$plot_selector_ui <- renderUI({ # OUTPUT scatter variable selectors
   req(input$select_columns)
   selected_cols <- input$select_columns

   tagList(
      selectInput(
         "x_var",
         "Select X-axis Variable:",
         choices = selected_cols,
         selected = selected_cols[1]
      ),
      selectInput(
         "y_var",
         "Select Y-axis Variable:",
         choices = selected_cols,
         selected = selected_cols[2]
      ),
      selectInput(
         "color_var",
         "Select Color Variable:",
         choices = selected_cols,
         selected = selected_cols[3]
      )
   )
})

output$filtered_plot <- renderPlotly({ # OUTPUT scatter plot
   req(filtered_data(), input$y_var, input$x_var, input$color_var)
   fp <- filtered_data() %>%
      ggplot(aes(
         x = .data[[input$x_var]],
         y = .data[[input$y_var]],
         color = .data[[input$color_var]],
         text = paste0(
            "<b>", input$x_var, ":</b> ", .data[[input$x_var]], "<br>",
            "<b>", input$y_var, ":</b> ", .data[[input$y_var]], "<br>",
            "<b>", input$color_var, ":</b> ", .data[[input$color_var]]
         )
      )) +
      geom_point() +
      theme_bw() +
      theme(
         text = element_text(size = 15),
         axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(title = paste(input$y_var, "vs", input$x_var))
   base_name <- paste0("plot_", input$y_var, "_vs_", input$x_var)
   plotly::ggplotly(fp, tooltip = "text") %>%
      plotly::layout(margin = list(l = 60, r = 30, t = 60, b = 70), font = list(size = 14)) %>%
      add_export_config(base_name)
})


output$plot_selector_boxplot_ui <- renderUI({ # OUTPUT boxplot variable selectors
   req(input$select_columns)
   selected_cols <- input$select_columns

   tagList(
      selectInput(
         "group_var_bp",
         "Select columns to plot:",
         choices = selected_cols,
         selected = selected_cols[1],
         multiple = TRUE
      ),
      selectInput(
         "x_var_bp",
         "Select X-axis Variable:",
         choices = selected_cols,
         selected = selected_cols[2]
      )
   )
})


output$boxplot_filt <- renderPlotly({ # OUTPUT dynamic boxplot / categorical counts
   req(filtered_data())
   filt_plot <- filtered_data()
   grouping_var <- input$group_var_bp
   x_axis_groups <- input$x_var_bp
   dbg_log("boxplot_filt", "incoming grouping_var=", paste(grouping_var, collapse = ","), "x_axis=", x_axis_groups)
   if (is.null(grouping_var) || !length(grouping_var) || is.null(x_axis_groups) || !nzchar(x_axis_groups)) {
      dbg_log("boxplot_filt", "Missing grouping or x var; abort")
      return(NULL)
   }
   if (any(setdiff(grouping_var, names(filt_plot)) |> length() > 0) || !(x_axis_groups %in% names(filt_plot))) {
      dbg_log("boxplot_filt", "Required columns absent")
      return(NULL)
   }
   exp <- filt_plot %>% dplyr::filter(!is.na(.data[[x_axis_groups]]))
   if (!nrow(exp)) {
      dbg_log("boxplot_filt", "All rows removed after NA filter on x")
      return(NULL)
   }
   pivot_cols <- setdiff(grouping_var, x_axis_groups)
   if (!length(pivot_cols)) {
      exp_long <- exp %>%
         dplyr::mutate(.synthetic = .data[[x_axis_groups]]) %>%
         tidyr::pivot_longer(cols = dplyr::all_of(".synthetic"), values_to = "Value", names_to = "Columns_of_Interest") %>%
         dplyr::mutate(Columns_of_Interest = x_axis_groups)
      num_cols <- 1L
   } else {
      exp_long <- exp %>%
         tidyr::pivot_longer(cols = dplyr::all_of(pivot_cols), values_to = "Value", names_to = "Columns_of_Interest") %>%
         data.frame()
      num_cols <- length(pivot_cols)
   }
   dbg_log("boxplot_filt", "pivot_cols=", paste(pivot_cols, collapse = ","), "rows=", nrow(exp_long))
   if (!is.numeric(exp_long$Value)) {
      exp_long$Value <- ifelse(is.na(exp_long$Value), "(NA)", as.character(exp_long$Value))
      agg_cat <- exp_long %>%
         dplyr::group_by(Columns_of_Interest, .data[[x_axis_groups]], Value, .drop = FALSE) %>%
         dplyr::summarise(n = dplyr::n(), .groups = "drop")
      if (!nrow(agg_cat)) {
         return(NULL)
      }
      p_cat <- agg_cat %>%
         ggplot2::ggplot(ggplot2::aes(
            x = .data[[x_axis_groups]], y = n, fill = Value,
            text = paste0(
               "<b>", x_axis_groups, ":</b> ", .data[[x_axis_groups]], "<br>",
               "<b>Facet:</b> ", Columns_of_Interest, "<br>",
               "<b>Value:</b> ", Value, "<br><b>Count:</b> ", n
            )
         )) +
         ggplot2::geom_col(position = "stack") +
         ggplot2::facet_wrap(~Columns_of_Interest, scales = "free_y") +
         ggplot2::theme_bw() +
         ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), text = ggplot2::element_text(size = 15), legend.position = "bottom") +
         ggplot2::labs(x = NULL, y = "Count", fill = "Value", title = "Categorical distribution of selected columns", subtitle = "No numeric columns selected: displaying counts instead of box plots")
      # Dynamic spacing for rotated tick labels
      tick_vals <- unique(agg_cat[[x_axis_groups]])
      if (is.factor(tick_vals)) tick_vals <- levels(tick_vals)
      max_chars <- suppressWarnings(max(nchar(as.character(tick_vals)), na.rm = TRUE))
      if (!is.finite(max_chars) || max_chars <= 0) max_chars <- 1
      standoff_px <- 10 + min(70, ceiling(max_chars * 1.1))
      bottom_margin <- 60 + min(200, max_chars * 5)
      # export helper now centralized (Helpers/export_helpers.R)
      base_name <- paste0("cat_counts_", x_axis_groups)
      p_catly <- plotly::ggplotly(p_cat, tooltip = "text") %>%
         plotly::layout(
            xaxis = list(title = list(text = x_axis_groups, standoff = standoff_px)),
            margin = list(b = bottom_margin)
         ) %>%
         add_export_config(base_name)
      return(p_catly)
   }
   # Simplified numeric path to avoid plotly::ggplotly merge issues
   # Compute counts per (facet, x) to annotate if desired
   count_df <- exp_long %>%
      dplyr::group_by(Columns_of_Interest, .data[[x_axis_groups]]) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

   p_num <- exp_long %>%
      ggplot2::ggplot(ggplot2::aes(
         x = .data[[x_axis_groups]], y = Value,
         text = paste0(
            "<b>", x_axis_groups, ":</b> ", .data[[x_axis_groups]], "<br>",
            "<b>Facet:</b> ", Columns_of_Interest, "<br>",
            "<b>Value:</b> ", Value
         )
      )) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.65, size = 1.4, color = "#333333") +
      ggplot2::geom_text(
         data = count_df,
         ggplot2::aes(x = .data[[x_axis_groups]], y = Inf, label = n),
         vjust = -0.5, size = 3, inherit.aes = FALSE
      ) +
      ggplot2::facet_wrap(~Columns_of_Interest, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), text = ggplot2::element_text(size = 15)) +
      ggplot2::labs(x = NULL, y = "")

   tick_vals <- unique(exp_long[[x_axis_groups]])
   if (is.factor(tick_vals)) tick_vals <- levels(tick_vals)
   max_chars <- suppressWarnings(max(nchar(as.character(tick_vals)), na.rm = TRUE))
   if (!is.finite(max_chars) || max_chars <= 0) max_chars <- 1
   standoff_px <- 10 + min(70, ceiling(max_chars * 1.1))
   bottom_margin <- 60 + min(200, max_chars * 5)
   # export helper centralized
   base_name <- paste0("boxplot_", paste(pivot_cols, collapse = "_"), "_by_", x_axis_groups)
   plotly::ggplotly(p_num, tooltip = "text") %>%
      plotly::layout(
         xaxis = list(title = list(text = x_axis_groups, standoff = standoff_px)),
         margin = list(b = bottom_margin)
      ) %>%
      add_export_config(base_name)
})

# Render the map with sample locations
selected_map_samples <- reactiveVal(character()) # REACTIVE selected map samples
map_render_cache <- reactiveVal(NULL) # REACTIVE cached map data

output$map2 <- renderLeaflet({ # OUTPUT interactive leaflet map
   req(filtered_data())
   map_df <- filtered_data()
   full_df <- tryCatch(harmonized_data(), error = function(e) NULL)

   dbg_log("map2", "incoming rows=", nrow(map_df), "cols=", ncol(map_df))

   # 1. Ensure Date column exists: attempt to source from full harmonized data using key match
   if (!"Date" %in% names(map_df) && !is.null(full_df)) {
      candidate_dates <- c("Date", "Date.filtered", "Date.of.Sample.Collection", "Date.of.Sampling")
      key_cols <- intersect(c("Parent.ID", "Owen.s.Notation.for.sequencing"), names(map_df))
      if (length(key_cols) == 2 && all(key_cols %in% names(full_df))) {
         full_key <- paste0(full_df[[key_cols[1]]], "|", full_df[[key_cols[2]]])
         sub_key <- paste0(map_df[[key_cols[1]]], "|", map_df[[key_cols[2]]])
         for (cand in candidate_dates) {
            if (cand %in% names(full_df)) {
               idx <- match(sub_key, full_key)
               map_df$Date <- full_df[[cand]][idx]
               dbg_log("map2", "Attached Date from", cand, "nonNA=", sum(!is.na(map_df$Date)))
               break
            }
         }
      } else if (!is.null(full_df) && "Date" %in% names(full_df) && nrow(full_df) >= nrow(map_df)) {
         map_df$Date <- full_df$Date[seq_len(nrow(map_df))]
         dbg_log("map2", "Attached Date by positional fallback nonNA=", sum(!is.na(map_df$Date)))
      }
   }

   # 2. Attach numeric Latitude/Longitude if parsing target columns missing
   if (!all(c("Latitude", "Longitude") %in% names(map_df)) && !is.null(full_df) && all(c("Latitude", "Longitude") %in% names(full_df))) {
      key_cols <- intersect(c("Parent.ID", "Owen.s.Notation.for.sequencing"), names(map_df))
      if (length(key_cols) == 2 && all(key_cols %in% names(full_df))) {
         full_key <- paste0(full_df[[key_cols[1]]], "|", full_df[[key_cols[2]]])
         sub_key <- paste0(map_df[[key_cols[1]]], "|", map_df[[key_cols[2]]])
         idx <- match(sub_key, full_key)
         map_df$Latitude <- suppressWarnings(as.numeric(full_df$Latitude[idx]))
         map_df$Longitude <- suppressWarnings(as.numeric(full_df$Longitude[idx]))
         dbg_log("map2", "Reattached numeric lat/lon nonNA lat=", sum(!is.na(map_df$Latitude)), "lon=", sum(!is.na(map_df$Longitude)))
      }
   }

   # Uses global parse_multi_date() helper (Helpers/parse_multi_date.R) to avoid duplication.

   processed_data2 <- map_df %>%
      dplyr::mutate(
         parsed_coords = if (!all(c("Latitude", "Longitude") %in% names(.)) && "Site.Coordinates" %in% names(.)) {
            purrr::map(Site.Coordinates, ~ safe_parse_dms_pair_legacy(.x))
         } else {
            vector("list", length = n())
         },
         Latitude = if (!"Latitude" %in% names(.)) purrr::map_dbl(parsed_coords, ~ .x$latitude) else suppressWarnings(as.numeric(Latitude)),
         Longitude = if (!"Longitude" %in% names(.)) purrr::map_dbl(parsed_coords, ~ .x$longitude) else suppressWarnings(as.numeric(Longitude))
      ) %>%
      {
         if ("Date" %in% names(.)) {
            dplyr::mutate(.,
               sample = paste0(Parent.ID, ": ", Owen.s.Notation.for.sequencing),
               Date = parse_multi_date(Date),
               DateNumeric = as.numeric(Date)
            )
         } else {
            dplyr::mutate(.,
               sample = paste0(Parent.ID, ": ", Owen.s.Notation.for.sequencing),
               Date = as.Date(NA_character_),
               DateNumeric = NA_real_
            )
         }
      }

   valid_coord <- with(processed_data2, !is.na(Latitude) & !is.na(Longitude))
   valid_date <- !is.na(processed_data2$DateNumeric)
   dbg_log("map2", "valid_coord=", sum(valid_coord), "valid_date=", sum(valid_date))
   # Coordinate failure diagnostics
   if (isTRUE(getOption("ecokmer.debug", FALSE))) {
      failed_idx <- which(!valid_coord)
      if (length(failed_idx)) {
         failed_raw <- unique(processed_data2$Site.Coordinates[failed_idx])
         dbg_log("map2", "coord_fail_unique=", length(failed_raw), "examples=", paste(utils::head(failed_raw, 5), collapse = " | "))
      }
      if (sum(valid_date) == 0 && "Date" %in% names(processed_data2)) {
         raw_date_classes <- paste(class(map_df$Date), collapse = ",")
         dbg_log("map2", "All Date NA after parsing. Raw class=", raw_date_classes)
         if (!is.null(full_df)) {
            present_cands <- intersect(candidate_dates, names(full_df))
            dbg_log("map2", "Full dataset date candidates present:", paste(present_cands, collapse = ","))
         }
      }
   }
   if (isTRUE(getOption("ecokmer.debug", FALSE))) {
      dbg_log("map2", "Latitude summary")
      capture.output(print(summary(processed_data2$Latitude))) %>%
         paste(collapse = "\n") %>%
         cat("\n")
      dbg_log("map2", "Longitude summary")
      capture.output(print(summary(processed_data2$Longitude))) %>%
         paste(collapse = "\n") %>%
         cat("\n")
      dbg_log("map2", "DateNumeric summary")
      capture.output(print(summary(processed_data2$DateNumeric))) %>%
         paste(collapse = "\n") %>%
         cat("\n")
   }

   if (sum(valid_coord) == 0) {
      dbg_log("map2", "No valid coordinates; returning placeholder map")
      return(leaflet() %>% addTiles() %>% addControl("No valid geographic points to display", position = "topright"))
   }

   # Exclude invalid coordinate rows from mapping (retain for count display)
   excluded_n <- sum(!valid_coord)
   if (excluded_n > 0) {
      dbg_log("map2", "Excluding", excluded_n, "missing coord rows")
   }
   map_render_df <- processed_data2[valid_coord, , drop = FALSE]
   # TODO(feature): Allow user to choose an arbitrary numeric or categorical column
   # for coloring (dynamic palette). This will require generalizing determine_date_palette
   # to a generic palette factory with type dispatch (numeric -> gradient, categorical -> discrete).
   dp <- determine_date_palette(map_render_df$DateNumeric)
   pal <- dp$pal
   date_domain <- dp$date_domain
   legend_title <- dp$legend_title
   no_date_flag <- dp$no_date_flag
   dbg_log("map2", "palette_domain=", paste(date_domain, collapse = ","), "no_date_flag=", no_date_flag)

   # Additional diagnostics: classify invalid coordinate reasons (approximate)
   if (isTRUE(getOption("ecokmer.debug", FALSE))) {
      invalid_df <- processed_data2[!valid_coord, c("Parent.ID", "Owen.s.Notation.for.sequencing", "Site.Coordinates")]
      if (nrow(invalid_df)) {
         invalid_df$reason <- dplyr::case_when(
            is.na(invalid_df$Site.Coordinates) ~ "missing_site_coordinates",
            grepl("blank", invalid_df$Parent.ID, ignore.case = TRUE) ~ "blank_parent_id",
            grepl("blank|kit", invalid_df$Site.Coordinates, ignore.case = TRUE) ~ "blank_text_coord",
            TRUE ~ "no_numeric_or_parse_failed"
         )
         reason_counts <- sort(table(invalid_df$reason), decreasing = TRUE)
         dbg_log("map2", "invalid_coord_reasons=", paste(paste(names(reason_counts), reason_counts, sep = ":"), collapse = ","))
      }
   }

   # Prepare cluster icon creation using existing marker colors (fillColor) instead of title attr
   if (all(is.finite(date_domain))) {
      js_icon_code <- paste(
         "function (cluster){",
         "var markers=cluster.getAllChildMarkers();",
         "var Rs=[],Gs=[],Bs=[];",
         "markers.forEach(function(m){ var c = m.options.fillColor || m.options.color; if(c){",
         "var ctx=document.createElement('canvas').getContext('2d'); ctx.fillStyle=c; var rgb=ctx.fillStyle;",
         "var m2=rgb.match(/#?([0-9a-fA-F]{6})/); if(m2){ var hex=m2[1];",
         "Rs.push(parseInt(hex.substring(0,2),16));",
         "Gs.push(parseInt(hex.substring(2,4),16));",
         "Bs.push(parseInt(hex.substring(4,6),16)); } }});",
         "function median(arr){ if(!arr.length) return 136; arr.sort(function(a,b){return a-b;}); var mid=Math.floor(arr.length/2); if(arr.length%2){ return arr[mid]; } else { return Math.round((arr[mid-1]+arr[mid])/2); } }",
         "if(Rs.length===0){ return new L.DivIcon({ html: '<div style=\\\"background:#888;border:2px solid #fff;border-radius:20px;width:40px;height:40px;line-height:40px;text-align:center;font-weight:600;color:#000;\\\">'+cluster.getChildCount()+'</div>', className: 'marker-cluster marker-cluster-median', iconSize: new L.Point(40,40)}); }",
         "var r=median(Rs), g=median(Gs), b=median(Bs);",
         "var color='rgb('+r+','+g+','+b+')';",
         "return new L.DivIcon({ html: '<div style=\\\"background:'+color+';border:2px solid #fff;border-radius:20px;width:40px;height:40px;line-height:40px;text-align:center;font-weight:600;color:#000;\\\">'+cluster.getChildCount()+'</div>', className: 'marker-cluster marker-cluster-median', iconSize: new L.Point(40,40)});",
         "}",
         collapse = " "
      )
      js_icon <- htmlwidgets::JS(js_icon_code)
   } else {
      js_icon <- NULL
   }

   # Cache for selection/download
   map_render_cache(map_render_df)

   mp <- leaflet(map_render_df) %>%
      addTiles() %>%
      addCircleMarkers(
         ~Longitude, ~Latitude,
         layerId = ~sample,
         label = ~ paste0("Sample: ", sample, " | Date: ", format(Date, "%Y-%m-%d")),
         color = ~ pal(DateNumeric),
         fillColor = ~ pal(DateNumeric),
         fillOpacity = 1,
         stroke = TRUE,
         weight = 1,
         clusterOptions = if (!is.null(js_icon)) markerClusterOptions(iconCreateFunction = js_icon) else markerClusterOptions()
      ) %>%
      addDrawToolbar(
         targetGroup = "drawn",
         editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
         polylineOptions = FALSE,
         markerOptions = FALSE,
         circleOptions = FALSE,
         circleMarkerOptions = FALSE,
         rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = "#3366FF", weight = 2, fillOpacity = 0.15)),
         polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions = drawShapeOptions(color = "#33AA33", weight = 2, fillOpacity = 0.15))
      )
   if (!no_date_flag) {
      mp <- mp %>% addLegend(
         position = "bottomright",
         pal = pal,
         values = ~DateNumeric,
         title = legend_title,
         labFormat = function(type, cuts, p) {
            format(as.Date(cuts, origin = "1970-01-01"), "%Y-%m-%d")
         },
         opacity = 1
      )
   }
   mp %>% addControl(html = htmltools::tags$div(
      style = "background:rgba(255,255,255,0.8);padding:4px 8px;border-radius:4px;font-size:12px;",
      paste0(
         "Plotted ", nrow(map_render_df), " points (excluded ", excluded_n, ")",
         if (!no_date_flag) " · Cluster color ≈ median date" else " · No date available for color scale"
      )
   ), position = "topright")
})

# Marker selection toggle
observeEvent(input$map2_marker_click, { # OBSERVE toggle marker selection
   click <- input$map2_marker_click
   if (is.null(click$id) || !nzchar(click$id)) {
      return()
   }
   cur <- selected_map_samples()
   if (click$id %in% cur) {
      cur <- setdiff(cur, click$id)
   } else {
      cur <- c(cur, click$id)
   }
   selected_map_samples(cur)
})

# Clear selection button handler
observeEvent(input$clear_map_selection, { # OBSERVE clear selections
   selected_map_samples(character())
})

# Highlight selected markers overlay
observeEvent(selected_map_samples(), { # OBSERVE highlight selected markers
   sel <- selected_map_samples()
   df <- map_render_cache()
   if (is.null(df)) {
      return()
   }
   proxy <- leafletProxy("map2")
   proxy %>% clearGroup("selected")
   if (length(sel)) {
      sel_df <- df[df$sample %in% sel, , drop = FALSE]
      if (nrow(sel_df)) {
         proxy %>% addCircleMarkers(
            data = sel_df,
            lng = ~Longitude, lat = ~Latitude,
            layerId = ~sample,
            color = "#000000", fillColor = "#FFD700", fillOpacity = 1,
            radius = 9, stroke = TRUE, weight = 2,
            group = "selected",
            label = ~ paste0("(Selected) ", sample)
         )
      }
   }
})

output$map_selection_status <- renderText({ # OUTPUT selection count
   paste0("Selected: ", length(selected_map_samples()))
})

output$download_map_selection <- downloadHandler( # DOWNLOAD selected map rows
   filename = function() {
      # High-resolution timestamp: YYYYMMDD_HHMMSS_mmm
      ts <- format(Sys.time(), "%Y%m%d_%H%M%OS3") # %OS3 gives milliseconds
      paste0("map_selection_", ts, ".csv")
   },
   content = function(file) {
      df <- map_render_cache()
      sel <- selected_map_samples()
      if (is.null(df) || !length(sel)) {
         utils::write.csv(data.frame(), file, row.names = FALSE)
      } else {
         # Drop list columns (e.g., parsed_coords) to avoid write.table list error
         sub <- df[df$sample %in% sel, , drop = FALSE]
         is_list_col <- vapply(sub, is.list, logical(1))
         if (any(is_list_col)) {
            sub <- sub[, !is_list_col, drop = FALSE]
         }
         utils::write.csv(sub, file, row.names = FALSE)
      }
   }
)

# Multi-select via drawn shapes (rectangle / polygon)
observeEvent(input$map2_draw_new_feature, { # OBSERVE polygon/rectangle multi-select
   feat <- input$map2_draw_new_feature
   if (is.null(feat)) {
      return()
   }
   df <- map_render_cache()
   if (is.null(df) || nrow(df) == 0) {
      return()
   }

   # Expecting GeoJSON-like structure: geometry$type and geometry$coordinates
   geom <- feat$geometry
   if (is.null(geom$type) || is.null(geom$coordinates)) {
      return()
   }
   coords <- geom$coordinates
   # For Polygon/Rectangle: coords[[1]] is list of lng/lat pairs
   if (geom$type %in% c("Polygon", "Rectangle")) {
      poly <- coords[[1]]
      # Convert to matrix (lng, lat)
      poly_mat <- do.call(rbind, lapply(poly, function(pt) c(pt[[1]], pt[[2]])))
      if (nrow(poly_mat) < 3) {
         return()
      }
      # Ray casting point-in-polygon
      pip <- function(px, py, poly_xy) {
         x <- poly_xy[, 1]
         y <- poly_xy[, 2]
         n <- length(x)
         inside <- logical(length(px))
         for (i in seq_along(px)) {
            j <- n
            c <- FALSE
            for (k in seq_len(n)) {
               if (((y[k] > py[i]) != (y[j] > py[i])) &&
                  (px[i] < (x[j] - x[k]) * (py[i] - y[k]) / ((y[j] - y[k]) + 1e-12) + x[k])) {
                  c <- !c
               }
               j <- k
            }
            inside[i] <- c
         }
         inside
      }
      inside <- pip(df$Longitude, df$Latitude, poly_mat)
      ids <- df$sample[inside]
      if (length(ids)) {
         # Add (union) to existing selection
         cur <- unique(c(selected_map_samples(), ids))
         selected_map_samples(cur)
      }
   } else if (geom$type == "Point") {
      # Ignore point drawings (disabled) but guard for future
      return()
   }
})



# Dynamic UI for download buttons based on data mode
output$download_buttons_ui <- renderUI({ # OUTPUT mode-specific download buttons
   req(input$data_mode)

   if (input$data_mode == "online") {
      tagList(
         actionButton("process_records", "Process Filtered Records to Download", class = "btn btn-primary"),
         br(), br(),
         actionButton("download_records", "Download from DataFed", class = "btn btn-success")
      )
   } else {
      tagList(
         downloadButton("download_filtered_csv", "Download Filtered Records", class = "btn btn-primary")
      )
   }
})

observeEvent(input$process_records, { # OBSERVE prepare filtered records (add title or save)
   req(filtered_data())
   req(input$data_mode) # Require data mode to be set

   filt_df <- filtered_data()
   n <- nrow(filt_df)

   if (input$data_mode == "online") {
      # For online mode: prepare for DataFed download by creating title column
      filt_df$title <- paste0(filt_df$Owen.s.Notation.for.sequencing, "_preprocessed")
      write.csv(filt_df, "./Data/downloads/filtered_records.csv", row.names = FALSE)
      showNotification(sprintf("Processed %s filtered records for DataFed download (written to Data/downloads/filtered_records.csv).", n), type = "message")
   } else {
      # For offline mode: no processing needed, just save the filtered data directly
      write.csv(filt_df, "./Data/downloads/filtered_records.csv", row.names = FALSE)
      showNotification(sprintf("Saved %s filtered records locally.", n), type = "message")
   }
})


# filt_df <- read.csv("./Data/downloads/merged_records.csv")
# filt_df <- filt_df[481:482,]
# write.csv(filt_df, "./Data/downloads/filtered_records.csv", row.names = FALSE)
#
# filt_df <- read.csv("./Data/downloads/filtered_records.csv")

observeEvent(input$download_records, { # OBSERVE run Python download for filtered CSV
   # NOTE: datafed_file_download.py may call sys.exit(0) on success; reticulate reports this as SystemExit.
   # We treat SystemExit(0) as a successful completion to avoid confusing the user with an error-like message.
   req(input$data_mode == "online") # Only for online mode
   req(filtered_data())
   req(input$dfuser, input$dfpwd) # Require credentials

   # Write creds into temp env vars visible only to this R process
   Sys.setenv(
      DF_USER = input$dfuser,
      DF_PASS = input$dfpwd
   )

   # Count how many titles will be requested
   df_filt <- read.csv("./Data/downloads/filtered_records.csv")
   n_req <- if ("title" %in% names(df_filt)) sum(!is.na(df_filt$title) & nzchar(df_filt$title)) else NA_integer_
   showNotification(paste0("Starting DataFed download...", if (!is.na(n_req)) sprintf(" (%s records)", n_req) else ""), type = "message")
   # Popup removed per request – rely on corner notifications only.

   tryCatch(
      {
         py_run_string("import sys; sys.argv = ['Scripts/datafed_file_download.py', '--input', 'Data/downloads/filtered_records.csv', '--column', 'title']; exec(open('Scripts/datafed_file_download.py').read())")
         showNotification("DataFed download completed successfully.", type = "success")
      },
      error = function(e) {
         err <- tryCatch(reticulate::py_last_error(), error = function(.) NULL)
         if (!is.null(err) && identical(err$type, "SystemExit")) {
            val <- as.character(err$value)
            if (grepl("\\b0\\b", val)) {
               message("[datafed] Suppressing benign SystemExit(0) from datafed_file_download.py")
               showNotification("DataFed download completed (SystemExit 0).", type = "message")
               return(invisible(NULL))
            }
         }
         showNotification(paste("DataFed download failed:", e$message), type = "error")
      }
   )
})

# Direct CSV download for offline mode
output$download_filtered_csv <- downloadHandler( # DOWNLOAD offline filtered CSV
   filename = function() {
      paste0("filtered_records_", Sys.Date(), ".csv")
   },
   content = function(file) {
      req(filtered_data())
      filt_df <- filtered_data()
      write.csv(filt_df, file, row.names = FALSE)
   }
)
