## File: helper_fns_data_analysis.R - Data processing & plotting helpers (contract relies on attributes set by process_data)
## TODO: Some deprecated/commented analyze_data code could be removed entirely if no longer referenced.

utils::globalVariables(c(
   "Category", "Predictor_Value", "variable", "value", "data_long", "group_sizes",
   "max_val", "Columns_of_Interest", "Value", "group_sizes_long", "n", "File",
   "lineplot_plot", "Owen.s.Notation.for.sequencing", "diversity"
))

# Central debug logger (shared across modules) ---------------------------------
dbg_log <- function(tag, ...) { # FUNCTION: conditional debug logger controlled by option ecokmer.debug
   if (isTRUE(getOption("ecokmer.debug", FALSE))) {
      cat("[", tag, "] ", paste(..., collapse = " "), "\n", sep = "")
   }
}

# Provide pipe for linters when not building as a package
`%>%` <- dplyr::`%>%` # FUNCTION: pipe re-export for non-package context

#### functions for processing and plotting ####
###### process data ######
process_data <- function(config_info) { # FUNCTION: build filtered + engineered dataset from config list
   filepath <- config_info$filepath
   filename <- config_info$filename
   sheetname <- config_info$sheetname
   lineskips <- config_info$lineskips
   sample_column <- config_info$sample_column
   grouping_info <- config_info$grouping_information
   # grouping_category <- config_info$grouping_category
   # groupings <- config_info$groups
   # categorical_column <- config_info$categorical_column
   # categorical_date_column  <- config_info$categorical_date_column
   # sub_grouping_category <- config_info$sub_grouping_category
   # sub_groupings <- config_info$sub_groups
   y_values <- config_info$y_values
   log1p_y_values <- config_info$log1p_y_values
   date_information <- config_info$date_information
   date_cname <- date_information$column_name
   dates_range <- unlist(date_information$dates_range)
   date_format <- date_information$date_format

   # Load data (xlsx with sheet or csv only)
   if (stringr::str_detect(filename, ".xlsx")) {
      # if working with xlsx file check that we have sheetname as well
      if (length(sheetname) != 1) {
         stop(paste0("If working an xlsx file, specify the sheetname in the config file"))
      }
      # we will need to change the start row
      data_df <- openxlsx::read.xlsx(paste0(filepath, "/", filename), sheet = sheetname, startRow = as.numeric(lineskips), detectDates = TRUE)
   } else {
      # otherwise we are working with csv files
      if (!stringr::str_detect(filename, ".csv")) {
         stop(paste0("Currently, this script only operates with an xlsx or csv file, please convert your data to one of those file types"))
      }
      data_df <- read.csv(paste0(filepath, "/", filename), sep = ",", header = TRUE, comment.char = "#")
   }

   names(data_df) <- make.names(names(data_df))

   # Normalise names to safe symbols
   sample_column_mn <- make.names(sample_column)
   y_values_mn <- make.names(y_values)
   # Removed unused variable log1p_y_values_mn (previously make.names(log1p_y_values)); can restore if future feature requires.
   date_cname_mn <- make.names(date_cname)
   grouping_cnames_mn <- make.names(grouping_info$column_name)

   # Defensive: ensure required grouping columns actually exist post-normalisation.
   # This closes a silent failure mode where a removed/renamed column produced empty filters instead of a clear error.
   if (length(grouping_cnames_mn)) {
      missing_group_cols <- setdiff(grouping_cnames_mn, names(data_df))
      if (length(missing_group_cols) > 0) {
         if (isTRUE(getOption("ecokmer.debug", FALSE))) {
            message("[ecokmer.debug] missing grouping columns=", paste(missing_group_cols, collapse = ","))
         }
         stop("Grouping column(s) missing in data: ", paste(missing_group_cols, collapse = ", "))
      }
   }

   # Date handling
   if (length(dates_range) > 0) {
      dates_range <- as.Date(dates_range, format = date_format)
   }

   # convert date column to be as.Date()
   if (!is.null(date_cname_mn)) {
      data_df[date_cname_mn] <- lapply(data_df[date_cname_mn], as.Date, format = date_format)
   }

   # filter to just the dates of interest
   if (length(dates_range) > 0) {
      data_df <- data_df[data_df[[date_cname_mn]] >= min(dates_range) & data_df[[date_cname_mn]] <= max(dates_range), ]
      # also remove NA dates
      data_df <- data_df[!is.na(data_df[[date_cname_mn]]), ]
   }

   # Convert placeholders -> numeric; fill NA with 0; optional log1p transform
   filtered_data_df <- data_df

   # convert N/A to NA
   filtered_data_df[y_values_mn][filtered_data_df[y_values_mn] == "N/A"] <- NA
   # ensure y values are numeric now
   filtered_data_df[y_values_mn] <- lapply(filtered_data_df[y_values_mn], as.numeric)

   # set NA to 0
   # replace y_values that are NA or 0 with the value 1
   filtered_data_df[y_values_mn][is.na(filtered_data_df[y_values_mn])] <- 0
   # log1p transform the y-values that need to be transformed
   if (length(log1p_y_values) > 0) {
      filtered_data_df[log1p_y_values] <- log1p(filtered_data_df[log1p_y_values])
   }
   grouping_list <- list()

   # Filter by grouping selections (supports !negation)
   for (i in seq_along(grouping_cnames_mn)) {
      grouping_cname <- grouping_cnames_mn[i]
      all_values <- unique(filtered_data_df[[grouping_cname]])
      possible_values <- grouping_info$values[[i]]

      # determine if we are working with ! (not) or otherwise
      if (any(stringr::str_detect(possible_values, "!"))) {
         bad_values <- stringr::str_remove(possible_values, "!")
         grouping_values <- all_values[which(!all_values %in% bad_values)]
      } else {
         grouping_values <- possible_values
      }

      # filter
      filtered_data_df <- filtered_data_df[filtered_data_df[[grouping_cname]] %in% grouping_values, ]
      grouping_list[[i]] <- grouping_values
   }

   all_columns <- filtered_data_df

   # Subset to essential columns
   filtered_data_df <- filtered_data_df %>%
      dplyr::select(
         !!as.symbol(sample_column_mn), dplyr::all_of(grouping_cnames_mn),
         dplyr::all_of(y_values_mn),
         !!as.symbol(date_cname_mn)
      )

   # add attributes
   attributes(filtered_data_df)$config_info <- config_info
   attributes(filtered_data_df)$processed_info$sample_column <- sample_column
   attributes(filtered_data_df)$processed_info$grouping_info <- grouping_info
   attributes(filtered_data_df)$processed_info$grouping_values <- grouping_list
   attributes(filtered_data_df)$processed_info$y_values_mn <- y_values_mn
   attributes(filtered_data_df)$processed_info$date_cname_mn <- date_cname_mn
   attributes(filtered_data_df)$processed_info$dates_range <- dates_range
   attributes(filtered_data_df)$all_columns <- all_columns

   # return object
   return(filtered_data_df)
}


# ##### analyze data ######
# analyze_data <- function(processed_data){
#   # check that processed_data is data.frame
#   if(!"data.frame" %in% class(processed_data)) {
#     stop(paste0("processed_data must be a data.frame"))
#   }
#   # processed_data must be made with the process_data function so we have config_info as an attribute
#   if(is.null(attributes(processed_data)$config_info)){
#     stop(paste0("processed_data must be a data.frame created using theprocess_data function"))
#   }
#
#   # make.names information
#   grouping_category_mn <- attributes(processed_data)$processed_info$grouping_category_mn
#   y_values_mn <- attributes(processed_data)$processed_info$y_values_mn
#   groupings <- attributes(processed_data)$processed_info$groupings
#
#   # nest the data
#   processed_df_nest <- processed_data %>%
#     tidyr::pivot_longer(col = grouping_category_mn,
#                         names_to = "Category",
#                         values_to = "Predictor_Value") %>%
#     dplyr::group_by(Category) %>%
#     tidyr::nest()
#
#   # remove NA values for each category of interest
#   processed_df_nest <- processed_df_nest %>%
#     dplyr::mutate(data_noNA = purrr::map(data,function(dat){
#       filtered_data_df = dat[!is.na(dat[["Predictor_Value"]]), ]
#       return(filtered_data_df)
#     }))
#
#   # run statistics
#   processed_df_nest <- processed_df_nest %>%
#     dplyr::mutate(stats_df = purrr::map(data_noNA, function(dat){
#       res_list <- vector("list", length(y_values_mn))
#
#       for(i in 1:length(res_list)){
#         dat_sub <- dat %>%
#           dplyr::filter(Predictor_Value %in% groupings)
#         formula_str <- as.formula(paste0(y_values_mn[i], " ~ Predictor_Value"))
#         anova_mod <- aov(formula_str, data = dat_sub)
#         tukey_results <- TukeyHSD(anova_mod)
#         res <- tukey_results$Predictor_Value
#
#         res <- res %>% data.frame() %>%
#           dplyr::mutate(Response_Variable = y_values_mn[i]) %>%
#           tibble::rownames_to_column(var = "Comparison")
#
#         res_list[[i]] <- res
#       }
#
#       res_df <- res_list %>%
#         dplyr::bind_rows()
#
#       return(res_df)
#     }))
#
#   analyzed_df <- processed_df_nest %>%
#     dplyr::select(-dplyr::starts_with("Data")) %>%
#     tidyr::unnest(cols = c(stats_df))
#
#   # add in attributes
#   attributes(analyzed_df)$config_info <- attributes(processed_data)$config_info
#   attributes(analyzed_df)$processed_info$grouping_category_mn <- attributes(processed_data)$processed_info$grouping_category_m
#   attributes(analyzed_df)$processed_info$groupings <- attributes(processed_data)$processed_info$groupings
#   attributes(analyzed_df)$processed_info$sub_grouping_category_mn <- attributes(processed_data)$processed_info$sub_grouping_category_mn
#   attributes(analyzed_df)$processed_info$sub_groupings <- attributes(processed_data)$processed_info$sub_groupings
#   attributes(analyzed_df)$processed_info$y_values_mn <- attributes(processed_data)$processed_info$y_values_mn
#   attributes(analyzed_df)$processed_info$date_cname_mn <- attributes(processed_data)$processed_info$date_cname_mn
#   attributes(analyzed_df)$processed_info$dates_range <- attributes(processed_data)$processed_info$dates_range
#   attributes(analyzed_df)$original_data <- processed_data
#   attributes(analyzed_df)$all_columns <- attributes(processed_data)$all_columns
#
#   return(analyzed_df)
# }

##### boxplot data ######
boxplot_data <- function(processed_data) { # FUNCTION: generate grouped boxplot ggplot objects (list) from processed dataset
   # check that processed_data is data.frame
   if (!"data.frame" %in% class(processed_data)) {
      stop(paste0("processed_data must be a data.frame"))
   }
   # processed_data must be made with the process_data function so we have config_info as an attribute
   if (is.null(attributes(processed_data)$config_info)) {
      stop(paste0("processed_data must be a data.frame created using theprocess_data function"))
   }

   # make.names information
   grouping_cnames_mn <- make.names(attributes(processed_data)$processed_info$grouping_info$column_name)
   y_values_mn <- attributes(processed_data)$processed_info$y_values_mn
   groupings <- attributes(processed_data)$processed_info$grouping_values

   # nest the data
   processed_df_nest <- processed_data %>%
      tidyr::pivot_longer(
         col = dplyr::all_of(grouping_cnames_mn),
         names_to = "Category",
         values_to = "Predictor_Value"
      ) %>%
      dplyr::group_by(Category) %>%
      tidyr::nest()

   # remove NA values for each category of interest
   processed_df_nest$data_noNA <- purrr::map2(processed_df_nest$data, groupings, function(dat, grp) {
      filtered_data_df <- dat[!is.na(dat[["Predictor_Value"]]), ]
      filtered_data_df <- filtered_data_df %>%
         dplyr::filter(Predictor_Value %in% grp)

      return(filtered_data_df)
   })

   # convert to long data for all y-values
   processed_df_nest$data_long <- purrr::map(processed_df_nest$data_noNA, function(dat) {
      long_data_df <- dat %>%
         tidyr::pivot_longer(
            cols = dplyr::all_of(y_values_mn),
            names_to = "variable", values_to = "value"
         ) %>%
         data.frame()

      return(long_data_df)
   })

   # create boxplot
   processed_df_nest <- processed_df_nest %>%
      dplyr::mutate(boxplot_plot = purrr::map2(data_long, Category, function(dat, category) {
         if (length(unique(dat$Predictor_Value)) > 1) {
            p_boxplot <- ggplot2::ggplot(
               dat,
               ggplot2::aes(x = factor(Predictor_Value), y = value)
            ) +
               ggplot2::geom_boxplot(outlier.shape = NA) +
               ggplot2::geom_jitter(width = 0.2, alpha = 0.5, shape = 16) +
               ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 2, strip.position = "right") +
               ggplot2::theme_bw() +
               ggplot2::labs(
                  title = paste("Combined Comparison of y-values by", category),
                  x = category,
                  y = "Values"
               ) +
               ggplot2::theme(
                  axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
                  axis.text.y = ggplot2::element_text(size = 6),
                  strip.text = ggplot2::element_text(size = 6),
                  legend.text = ggplot2::element_text(size = 6),
                  legend.title = ggplot2::element_text(size = 6),
                  legend.position = "bottom"
               ) +
               ggplot2::guides(colour = "none")
         } else {
            p_boxplot <- NULL
         }
         return(p_boxplot)
      }))
   processed_df_nest <- processed_df_nest %>%
      dplyr::select(Category, boxplot_plot)

   # add in attributes
   attributes(processed_df_nest)$original_data <- processed_data
   attributes(processed_df_nest)$config_info <- attributes(processed_data)$config_info

   return(processed_df_nest)
}

##### lineplot data #####
lineplot_data <- function(processed_data) { # FUNCTION: generate grouped line+jitter plots with adaptive x-axis scaling
   # check that processed_data is data.frame
   if (!"data.frame" %in% class(processed_data)) {
      stop(paste0("processed_data must be a data.frame"))
   }
   # processed_data must be made with the process_data function so we have config_info as an attribute
   if (is.null(attributes(processed_data)$config_info)) {
      stop(paste0("processed_data must be a data.frame created using theprocess_data function"))
   }

   # make.names information
   # make.names information
   # make.names information
   grouping_cnames_mn <- make.names(attributes(processed_data)$processed_info$grouping_info$column_name)
   y_values_mn <- attributes(processed_data)$processed_info$y_values_mn
   groupings <- attributes(processed_data)$processed_info$grouping_values
   date_information_mn <- attributes(processed_data)$processed_info$date_cname_mn

   processed_df_nest <- processed_data %>%
      tidyr::pivot_longer(
         col = dplyr::all_of(grouping_cnames_mn),
         names_to = "Category",
         values_to = "Predictor_Value"
      ) %>%
      dplyr::group_by(Category) %>%
      tidyr::nest()

   # remove NA values for each category of interest
   processed_df_nest$data_noNA <- purrr::map2(processed_df_nest$data, groupings, function(dat, grp) {
      filtered_data_df <- dat[!is.na(dat[["Predictor_Value"]]), ]
      filtered_data_df <- filtered_data_df %>%
         dplyr::filter(Predictor_Value %in% grp)

      return(filtered_data_df)
   })

   # convert to long data for all y-values
   processed_df_nest$data_long <- purrr::map(processed_df_nest$data_noNA, function(dat) {
      long_data_df <- dat %>%
         tidyr::pivot_longer(
            cols = dplyr::all_of(y_values_mn),
            names_to = "variable", values_to = "value"
         ) %>%
         data.frame()

      return(long_data_df)
   })

   processed_df_nest <- processed_df_nest %>%
      dplyr::mutate(lineplot_plot = purrr::map2(data_long, Category, function(dat, category) {
         if (length(unique(dat$Predictor_Value)) > 1) {
            p_lineplot <- dat %>%
               ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(date_information_mn), y = value, color = variable)) +
               ggplot2::geom_jitter(ggplot2::aes(shape = factor(Predictor_Value))) +
               ggplot2::geom_smooth(se = FALSE) +
               ggplot2::theme_bw() +
               ggplot2::labs(
                  title = paste("Line Plot for ", category),
                  y = "Values",
                  color = "",
                  shape = paste(category)
               ) +
               ggplot2::theme(
                  axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
                  axis.text.y = ggplot2::element_text(size = 6),
                  strip.text = ggplot2::element_text(size = 6),
                  legend.text = ggplot2::element_text(size = 6),
                  legend.title = ggplot2::element_text(size = 6),
                  legend.position = "bottom"
               )
            # Apply axis scaling / de-overlap after base plot assembled
            x_vec <- dat[[date_information_mn]]
            if (inherits(x_vec, "Date")) {
               p_lineplot <- p_lineplot + ggplot2::scale_x_date(
                  expand = ggplot2::expansion(mult = c(0, 0.05)),
                  guide = ggplot2::guide_axis(check.overlap = TRUE)
               )
            } else if (is.numeric(x_vec)) {
               x_unique <- sort(unique(x_vec))
               rng <- range(x_unique, na.rm = TRUE)
               # Target at most ~8 ticks for readability
               brks <- pretty(rng, n = 8)
               # Ensure within data range
               brks <- brks[brks >= rng[1] & brks <= rng[2]]
               # If still dense, thin by keeping every other
               if (length(brks) > 8) {
                  brks <- brks[seq(1, length(brks), by = 2)]
               }
               # Format labels: trim excessive significant figures while preserving needed decimals
               span <- diff(rng)
               dec <- if (!is.finite(span) || span == 0) {
                  2
               } else {
                  # choose decimals so that about 200 units of resolution across span (cap between 0 and 4)
                  d <- ceiling(max(0, min(4, -log10(span / 200))))
                  if (!is.finite(d)) 2 else d
               }
               lbls <- formatC(brks, format = "f", digits = dec, drop0trailing = TRUE)
               # Store for downstream plotly layout (ggplotly may ignore guide_axis thinning)
               attr(p_lineplot, "._x_breaks_numeric") <- brks
               attr(p_lineplot, "._x_labels_numeric") <- lbls
               p_lineplot <- p_lineplot + ggplot2::scale_x_continuous(
                  expand = ggplot2::expansion(mult = c(0, 0.06)),
                  breaks = brks,
                  labels = lbls,
                  guide = ggplot2::guide_axis(check.overlap = TRUE)
               )
            }
         } else {
            p_lineplot <- NULL
         }
         return(p_lineplot)
      }))

   processed_df_nest <- processed_df_nest %>%
      dplyr::select(Category, lineplot_plot)

   # add in attributes
   attributes(processed_df_nest)$original_data <- processed_data
   attributes(processed_df_nest)$config_info <- attributes(processed_data)$config_info

   return(processed_df_nest)
}

# npo processing
npo_processing <- function(config_info) { # FUNCTION: process Nonpareil output files and merge with metadata
   # config information
   filepath_npo <- config_info$filepath_npo
   filepath_metadata <- config_info$filepath_metadata
   skip_rows <- as.numeric(config_info$skip_rows)
   joining_column <- config_info$joining_column

   # CHECKS/ERROR MESSAGES
   # metadata checks
   # check that metadata file path is only length 1
   if (length(filepath_metadata) != 1) {
      stop(paste0("filepath_metadata must be of length 1"))
   }

   # check that metadata is a real file
   if (!file.exists(filepath_metadata)) {
      stop(paste0("filepath_metadata does not contain a proper file"))
   }

   # must be a csv or xlsx file
   if (!(endsWith(filepath_metadata, ".xlsx") || endsWith(filepath_metadata, ".csv"))) {
      stop(paste0("filepath_metadata must be a file path for either a csv file or an xlsx file"))
   }

   # load in metadata
   # check that filepath and filename are not NULL
   if (endsWith(filepath_metadata, ".xlsx")) {
      # if we are working with xlsx files
      metadata <- readxl::read_excel(filepath_metadata, skip = skip_rows)
   } else {
      # otherwise we are working with csv files
      metadata <- read.csv(paste0(filepath_metadata, header = TRUE))
   }

   # npo checks
   # check that npo objects are real file
   if (!all(file.exists(filepath_npo))) {
      stop(paste0("At least one of the files listed in filepath_npo does not exist or is mistyped"))
   }

   # get the actual files
   all_files <- list.files(filepath_npo)
   # this folder has non npo objects as well - so remove them
   npo_files <- all_files[endsWith(list.files(filepath_npo), ".npo")]
   filepath_npo <- paste0(filepath_npo, "/", npo_files)

   # must be an npo file
   if (!all(endsWith(filepath_npo, ".npo"))) {
      stop(paste0("filepath_npo must be contain file paths for only npo objects"))
   }

   # skip_rows
   # must be of length 1
   if (length(skip_rows) > 1) {
      stop(paste0("skip_rows must be of length 1"))
   }

   # must be numeric
   if (!is.numeric(skip_rows)) {
      stop(paste0("skip_rows must be a numeric argument"))
   }

   # must be 0 or greater
   if (skip_rows < 0) {
      stop(paste0("skip_rows must be greater than or equal to 0"))
   }

   # joining column
   # must be of length 1
   if (length(joining_column) > 1) {
      stop(paste0("joining_column must be of length 1"))
   }

   # must be a column in metadata
   if (!joining_column %in% names(metadata)) {
      stop(paste0("joining_column must be a name of a column in the metadata file"))
   }

   # FUNCTION
   # run Nonpareil.set to get the diversity values
   nps <- Nonpareil::Nonpareil.set(
      filepath_npo,
      plot = FALSE
   )

   # convert this information into a data.frame
   summary_data <- summary(nps) %>%
      data.frame() %>%
      tibble::rownames_to_column(var = "File") %>%
      dplyr::mutate(File = stringr::str_remove(File, ".fastq_nonparuil_out"))



   # Extract the common part from the 'File' column in df1
   summary_data <- summary_data %>%
      mutate(File = str_extract(File, "^([0-9]+-[A-Za-z]+-[A-Za-z]+-[a-zA-Z0-9]+)"))

   # add in metadata
   summary_data_meta <- summary_data %>%
      dplyr::left_join(metadata, by = c("File" = joining_column))

   # return summary data with metadata attached
   return(summary_data_meta)
}




# new boxplot functions
diversity_plot <- function(dataset, sequences, grouping_var, title = NULL, xlab = NULL, ylab = NULL, file_name,
                           min_value = 16, max_value = 23) { # FUNCTION: diversity boxplot with per-group n labels (single metric)






   exp <- dataset %>%
      dplyr::filter(Owen.s.Notation.for.sequencing %in% sequences) %>%
      dplyr::filter(!is.na(!!as.symbol(grouping_var)))

   if (is.null(ylab)) {
      ylab <- "Diversity"
   }

   if (is.null(xlab)) {
      xlab <- grouping_var
   }

   group_sizes <- exp %>%
      dplyr::group_by(!!as.symbol(grouping_var)) %>%
      dplyr::mutate(
         n = dplyr::n(),
         max_val = max(diversity, na.rm = TRUE)
      )
   p <- exp %>%
      ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(grouping_var), y = diversity)) +
      ggplot2::geom_boxplot(data = subset(group_sizes, n > 1)) + # Show boxplot only for groups with >1 obs
      ggplot2::geom_point(data = subset(group_sizes, n == 1)) +
      ggplot2::geom_label(data = group_sizes, ggplot2::aes(label = n, y = max_val + 0.25)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
         axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust = 1),
         text = ggplot2::element_text(size = 15)
      ) +
      ggplot2::labs(
         y = ylab,
         title = title,
         x = xlab
      ) +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(20))

   if (!is.null(max_value) & !is.null(min_value)) {
      p <- p +
         ggplot2::ylim(c(min_value, max_value))
   }

   # ggsave(filename = here("Figures",paste0(file_name,".pdf")), device = "pdf", plot = p,
   #        width = 5, height = 5, units = "in")

   p
}


mult_cols_plot <- function(dataset, sequences,
                           grouping_var,
                           mult_cols,
                           title = NULL, xlab = NULL, ylab = NULL, file_name,
                           min_value = NULL, max_value = NULL) { # FUNCTION: faceted multi-metric boxplots with per-group counts
   # make the data long
   exp <- dataset %>%
      dplyr::filter(Owen.s.Notation.for.sequencing %in% sequences) %>%
      dplyr::filter(!is.na(!!as.symbol(grouping_var)))

   exp_long <- exp %>%
      tidyr::pivot_longer(
         cols = dplyr::all_of(mult_cols),
         values_to = "Value",
         names_to = "Columns_of_Interest"
      ) %>%
      data.frame()

   num_cols <- length(mult_cols)

   # Count number of observations per group
   group_sizes_long <- exp_long %>%
      dplyr::filter(!is.na(!!as.symbol(grouping_var))) %>%
      dplyr::group_by(!!as.symbol(grouping_var)) %>%
      dplyr::mutate(n = dplyr::n() / num_cols) %>%
      dplyr::group_by(Columns_of_Interest, !!as.symbol(grouping_var)) %>%
      dplyr::mutate(max_val = max(Value, na.rm = TRUE))

   # plot ratios
   p <- exp_long %>%
      ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(grouping_var), y = Value)) +
      ggplot2::geom_boxplot(data = subset(group_sizes_long, n > 1), outlier.shape = NA) + # Show boxplot only for groups with >1 obs
      ggplot2::geom_point(data = subset(group_sizes_long, n == 1)) +
      ggplot2::geom_label(data = group_sizes_long, ggplot2::aes(label = n, y = max_val + 0.25)) +
      ggplot2::theme_bw() +
      ggplot2::geom_jitter() +
      ggplot2::theme(
         axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust = 1),
         text = ggplot2::element_text(size = 15)
      ) +
      ggplot2::facet_wrap(~Columns_of_Interest) +
      ggplot2::labs(
         y = ylab,
         title = title,
         x = xlab
      ) +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(20))

   if (!is.null(max_value) & !is.null(min_value)) {
      p <- p +
         ylim(c(min_value, max_value))
   }

   # ggsave(filename = here("Figures",paste0(file_name,".pdf")), device = "pdf", plot = p,
   #        width = 6, height = 5, units = "in")
   #
   p
}
