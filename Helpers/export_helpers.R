# File: export_helpers.R - Centralized Plotly export config (PNG/SVG buttons)
# TODO: Consider parameterizing height/width for SVG export rather than using layout-derived dimensions only.

if (!exists("add_export_config", inherits = FALSE)) {
    add_export_config <- function(p, fname, scale = 3) { # FUNCTION: attach unified PNG+SVG export buttons to plotly object
        stopifnot(!missing(p), !missing(fname))
        # Single SVG path reused across app
        svg_download_icon <- list(
            width = 1000,
            height = 1000,
            path = "M500 80v520l200-200 70 70-270 270-270-270 70-70 200 200V80h70z M220 660h560v150H220z"
        )
        p %>% plotly::config(
            toImageButtonOptions = list(format = "png", filename = fname, scale = scale),
            modeBarButtonsToAdd = list(
                list(
                    name = "Download SVG",
                    icon = svg_download_icon,
                    click = htmlwidgets::JS(paste0(
                        "function(gd){Plotly.downloadImage(gd,{format:'svg',filename:'", fname, "',height:gd._fullLayout.height,width:gd._fullLayout.width});}"
                    ))
                )
            )
        )
    }
}
