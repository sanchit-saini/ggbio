## TODO::
## Let's load a RefSeq data
## naming the interval
## two mode? packed, full with name (default)
## reduce is just a stat transformation at lower level
setGeneric("geom_rect", function(data, ...) standardGeneric("geom_rect"))

setMethod("geom_rect", "ANY", function(data, ...) {
  ggplot2::geom_rect(data = data, ...)
})

## alignment should be convenient toggle with chevron...
setMethod("geom_rect", "GRanges", function(data, ...,
                                           xlab = "", ylab = "",
                                           main = "", stat = c("stepping", "identity"),
                                           rect.height = NULL,
                                           group.selfish = TRUE) {
    args <- list(...)

    Layer(
        data = data,
        mapping = parseArgsForAes(args),
        geom = Geom("rect"),
        stat = Stat(match.arg(stat)),
        facets = args$facets,
        labels = list(x = xlab, y = ylab, main = main),
        params = list(
            rect.height = rect.height,
            group.selfish = group.selfish,
            all.args = args,
            non.aes.args = parseArgsForNonAes(args)
        ),
        operations = function(x) {
            if (!length(data)) # for empty data
                plot <- NULL
            else # call to stepping/identity according to set stat
                plot <- do.call(quo_name(x@stat@name), list(x))
            # create facet
            facet <- build_facet(x@data, x@params$all.args, facet_grid, facet_wrap)

            plot <- c(list(plot), list(facet))

            # Why y label is not shown in identity?
            if (identical(x@stat, "stepping"))
                args <- c(x@labels , list(fallback = c(x = "", y = "")))
            else
                args <- c(x@labels , list(fallback = c(x = "")))

            labels <- do.call(Labels, args)
            plot <- c(plot, labels)
      }
    )
})


## create plot from layer object for GRanges data with 'rect' geometry
## and 'stepping' statistical transformation
GRanges_stepping_rect <- function(x) {

    # map variables from layers
    args <- x@params$args
    aes.args <- x@mapping
    non.aes.args <- x@params$non.aes.args
    facets <- x@facets
    group.selfish <- x@params$group.selfish

    # set default rect.height
    if (is.null(x@params$rect.height))
        rect.height <- 0.4

    # set user specified extend.size or default value
    if ("extend.size" %in% names(non.aes.args))
        extend.size <- non.aes.args$extend.size
    else
        extend.size <- 0

    # set user specified group or default value
    if ("group" %in% names(args))
        group <- quo_name(args$group)
    else
        group <- "stepping"

    # split data by facet
    grl <- splitByFacets(data, facets)

    # Add stepping column to evry GRanges
    grl <- endoapply(grl, make_addStepping, aes.args,
                     group.selfish, extend.size = extend.size)

    # convert GRangesList to data.frame
    df <- mold(unlist(grl))

    # remove args before create geom
    aes.args <- remove_args(aes.args, c("xmin", "xmax", "ymin", "ymax", "data", "group", "size"))
    non.aes.args <- remove_args(non.aes.args, c("xmin", "xmax", "ymax", "ymax", "data", "facets"))

    ## geom_segment

    # remove args to overcome 1 pixel problem
    seg.aes.args <- remove_args(aes.args, c("fill", "y", "xend", "yend", "x"))

    # append to aes
    seg.aes.args <- append_aes(seg.aes.args, x = substitute(start),
                               xend = substitute(start),
                               y = substitute(stepping - rect.height),
                               yend = substitute(stepping + rect.height))

    # create geom_segment
    seg.args <- c(list(data = df), list(seg.aes.args), non.aes.args)
    plot <- list(do.ggcall(ggplot2::geom_segment, seg.args))

    ## geom_rect

    # append to aes
    rect.aes.args <- append_aes(aes.args, xmin = substitute(start),
                                xmax = substitute(end),
                                ymin = substitute(stepping - rect.height),
                                ymax = substitute(stepping + rect.height))

    # create geom_rect
    rect.args <- c(list(data = df), list(rect.aes.args), non.aes.args)
    plot <- c(plot, list(do.ggcall(ggplot2::geom_rect, rect.args)))

    # set color for strand
    plot <- .changeStrandColor(plot, rect.aes.args)

    ## FIXME:
    # subset 'df' for [,j] with c("stepping", group) and remove duplicate
    # value along stepping column
    df.sub <- group_df(df, group)
    # scale y on the bases of group and group.selfish
    y_scale <- scale_y_continuous_by_group(df.sub, group, group.selfish)

    # append y scale to the plot
    plot <- c(plot, y_scale)
}

GRanges_identity_rect <- function(x) {
  
}

