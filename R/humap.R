#' Create a humap plot
#'
#' \code{humap} creates a ggplot object, using its own set of "geoms", i.e.,
#' human body oulines on which your data are projected in a heatmap-like
#' fashion. \code{humapr} is an extension of ggplot2, and objects created with
#' the \code{humap} function can be modified with standard ggplot2
#' setting--e.g., calls to \code{theme()}.
#'
#' If you don't supply a `side` argument humapr will assume default to mirroring
#' mode. If `side` argument is supplied, it will default to left/right
#' discrimate mode and show both body halves
#'
#' If proj = "both" you cannot map right/left-discrimate data.
#'
#' \code{controls} is a list of more specific parameters to modify details in
#' the apperance of the humap. These controls are currently available:
#' \code{vert_adj} has "minimal" as default, but may also be set to "even" for
#' vertical even distribution, or "smart" to avoid crossing annovation lines.
#' \code{na_fill} specificies the fill of zero-count regions. \code{mid_include}
#' is a logical indicating whether midline observations should be included;
#' currently, if this is set to TRUE, a midline observation will count as half
#' an observations on each side. Default is FALSE.
#'
#' @param data tidy data frame (more under details)
#' @param loc.var column in data frame containing localisation codes for
#'   observations. Character string.
#' @param lr.var variable indication left and right side of observations. Values
#'   must be "left" or "right". May also be a list that defines the variable (=
#'   data frame column) holding the variables, and which value(s) in that column
#'   correspond(s) to left and right. So, lr.var = "xx" or lr.var = list(var =
#'   "xx", left = c("left", "l"), right = "r")
#' @param gender n(eutral) (default), f(emale), m(male)
#' @param type body
#' @param proj character string defining the projected front, back
#' @param half character string defining how to deal with body halves. "both"
#'   discriminates the left half from the right; "mirror" merges observations
#'   in, e.g., right and left side of the chest; "left"/"right" yields only the
#'   indicated half and ignores observations on the other half.
#' @param annotate \code{freq} (defaults) shows only absolute and relative
#'   frequencies. \code{all} include region names. \code{none} omits labels
#'   altogether.
#' @param anno_gp grid::gpar object. Default is fontsize = 9 and col = "black".
#' @param combine a \emph{list} of vectors naming the regions to be combined and
#'   mapped as one. E.g., list(arm = c("shoulder", "arm", "elbow", "wrist"))
#' @param bridge used to bridge between the user's data format and the native
#'   format of humapr. Further explanations to follow
#' @param na_rm should missing data be removed? Defaults is \code{FALSE}.
#' @param controls list of controls for fine-tuning. See details.
#' @return A ggplot object with layout settings suitable for the kinds of geoms
#'   used in \code{humap}.
#'
#' @export
#' @import ggplot2
#' @importFrom stats na.exclude na.omit setNames
#' @importFrom magrittr %>%

humap <- function(data, loc.var, lr.var = NULL, type = "body", gender = "neutral",
                  proj = "simple", half = "both", annotate = "freq", anno_gp = NULL,
                  bridge = NULL, na_rm = FALSE, combine = NULL, controls = NULL) {

    # Safety moves and housekeeping
    if (missing(data)) stop("Please, include data.")
    if (missing(loc.var)) stop("Please, specify a 'loc.var'.")
    housekeeping(match.call()[-c(1, 2)], formals()[-1])
        # Forces controls$mid_include = FALSE (for now, let's see later)

    # Import relevant map (as SpatialPolygon from R/sysdata.rda)
    mapname <- sprintf("%s_%s_%s", h_env$type, h_env$gender, h_env$proj)
    # h_env$map <- humapr:::maps[[mapname]]$map
    # h_env$mapdf <- humapr:::maps[[mapname]]$mapdf # df with grouped polygon coordinates
    h_env$map <- maps[[mapname]]$map
    h_env$mapdf <- maps[[mapname]]$mapdf # df with grouped polygon coordinates
    h_env$pids <- as.data.frame(h_env$map)$Layer %>% # polygon ids
        setNames(seq(.))
    h_env$regions <- grep("_outline", h_env$pids, value = TRUE, invert = TRUE)
        # exclude potential outline polygons/lines from 'regions'

    # Ensure valid user-supplied regions in "combine", if relevant
    if (!is.null(h_env$combine))
        test_combined(h_env$half, h_env$combine, h_env$pids)

    # Convert user formats with bridge argument, if relevant
    if (!is.null(h_env$bridge))
        data <- build_bridge(data, h_env$bridge, h_env$type)

    # Add mapped_loc variable to user's data frame
    data <- generate_mapped_loc(data, h_env$loc.var, h_env$lr.var,
                                h_env$regions, h_env$half, h_env$combine)

    # Generate (preliminary) data for annotations, if relevant
    if (h_env$annotate %in% c("all", "freq"))
        prep_annotations(data$mapped_loc, h_env$combine, h_env$type,
                         h_env$gender, h_env$proj, h_env$half)

    # Removing missing data, if so desired by user
    if (h_env$na_rm)
        data <- data[!is.na(data$mapped_loc), ]

    # Build ggplot object
    ggplot2::ggplot(data, aes(x = mapped_loc, fill = ..count.., group = 1)) +
        ggplot2::guides(fill = if (h_env$annotate == "none") NULL else FALSE) +
        geom_humap(stat = "count", na.rm = h_env$na_rm) +
        ggplot2::theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.line = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid = element_blank(),
                       legend.title = element_blank()) +
        ggplot2::scale_fill_gradient(low = "#56B1F7", high = "#132B43")
}
