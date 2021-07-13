setClassUnion("character_OR_expression_OR_NULL",
              c("expression", "character_OR_NULL"))

setClass("Tracks", representation(grobs = "PlotList", # working plots, not reall 'Grob'
                                 plot = "list", # original plots passed into tracks
                                 backup = "list", # backup of the whole tracks object
                                 heights = "numericORunit",
                                 xlim = "numeric",
                                 ylim = "list",
                                 xlab = "character_OR_NULL",
                                 main = "character_OR_expression_OR_NULL",
                                 main.height =  "numericORunit",
                                 scale.height =  "numericORunit",
                                 xlab.height =  "numericORunit",
                                 theme = "theme_OR_NULL",
                                 fixed = "logical",
                                 labeled = "logical",
                                 mutable = "logical",
                                 hasAxis = "logical",
                                 padding = "numericORunit",
                                 label.bg.color = "character",
                                 label.bg.fill = "character",
                                 label.text.color = "character",
                                 label.text.cex = "numeric",
                                 label.text.angle = "numeric",
                                 track.plot.color = "character_OR_NULL",
                                 track.bg.color = "character_OR_NULL",
                                 label.width = "unit"))

.tracks.theme <- setdiff(slotNames("Tracks"), c("backup", "grobs"))

tracks <- function(..., heights, xlim, xlab = NULL, main = NULL,
                   title = NULL,
                   theme = NULL,
                   track.plot.color = NULL,
                   track.bg.color = NULL,
                   main.height = unit(1.5, "lines"),
                   scale.height = unit(1, "lines"),
                   xlab.height = unit(1.5, "lines"),
                   padding = unit(-1, "lines"),
                   label.bg.color =  "white",
                   label.bg.fill = "gray80",
                   label.text.color = "black",
                   label.text.cex = 1,
                   label.text.angle = 90,
                   label.width = unit(2.5, "lines")) {
    args <- list(...)
    dots <- reduceListOfPlots(args)
    ## return plots if not
    dots <- genPlots(dots)

    if (is.numeric(padding) && !is.unit(padding))
        padding <- unit(padding, "lines")

    if (is.numeric(main.height) && !is.unit(main.height))
        main.height <- unit(main.height, "lines")

    if (is.numeric(scale.height) && !is.unit(scale.height))
        scale.height <- unit(scale.height, "lines")

    if (is.numeric(xlab.height) && !is.unit(xlab.height))
        xlab.height <- unit(xlab.height, "lines")

    if (!is.null(title) && is.null(main))
        main <- title

    ## convert to Plot object with extra slots
    plotList <- do.call(PlotList, dots)

    fixed <- vapply(plotList, fixed, logical(1L))
    mutable <- vapply(plotList, mutable, logical(1L))
    hasAxis <- vapply(plotList, hasAxis, logical(1L))
    labeled <- vapply(plotList, labeled, logical(1L))
    isIdeo <- vapply(plotList, is, "Ideogram", FUN.VALUE = logical(1L))
    isBlank <- vapply(plotList, function(x) x@blank, logical(1L))

    ## get height
    if (missing(heights))
        heights <- getHeight(plotList)
    else
        heights <- parseHeight(heights, length(plotList))

    ## ylim
    ylim <- lapply(plotList[!fixed & !isIdeo & !isBlank], function(grob) {
        scales::expand_range(getLimits(grob)$ylim, mul = 0.05)
    })

    wh <- NULL

    ## xlim
    if (missing(xlim)) {
        idx <- vapply(args, function(x) is(x, "GenomicRanges_OR_GRangesList"), logical(1L))
        if (any(idx)) {
            gr <- args[idx]
            gr <- lapply(gr, function(x) range(unlist(x)))
            gr <- do.call(c, unname(gr))
            gr <- suppressWarnings(range(gr))
            chrs <- unique(as.character(seqnames(gr)))
            if (length(chrs) > 1) {
                stop("seqnames of passed GRanges has to be the same for tracks")
            }
            wh <- gr
        }

        xid <- !fixed & !isIdeo & !isBlank
        if (sum(xid)) {
            lst <- lapply(plotList[xid], function(obj) {
                res <- getLimits(obj)
                data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
            })
            res <- do.call(rbind, lst)
            xlim <- c(min(res$xmin), max(res$xmax))
            xlim <- scales::expand_range(xlim, mul = 0.1)
        } else {
            xlim <- c(0, 1)
        }
    } else {
      if (is(xlim,"GRanges")) {
          wh <- xlim
          xlim <- c(start(ranges(reduce(xlim, ignore.strand = TRUE))),
                    end(ranges(reduce(xlim, ignore.strand = TRUE))))
      }
      if (is(xlim, "IRanges"))
          xlim <- c(start(xlim), end(xlim))
      if (is.numeric(xlim))
          xlim <- range(xlim)
    }

    ## sync xlim when construct them??
    if (!is.null(wh)) {
        plotList <- lapply(plotList, function(x) {
            x + xlim(wh)
        })
        plotList <- do.call(PlotList, plotList)
    }

    ## plot background
    N <- length(plotList)
    if (is.null(track.plot.color)) {
        if (is.null(track.bg.color))
            track.plot.color <- vapply(plotList, bgColor, character(1L))
        else
            track.plot.color <- rep(track.bg.color, length(plotList))
    }
    stopifnot(length(track.plot.color) == N | length(track.plot.color) == 1)

    ## backup: record a state
    backup <- list(grobs = plotList, plot = dots, heights = heights,
                   xlim = xlim, ylim = ylim, xlab = xlab, main = main,
                   main.height = main.height, scale.height = scale.height,
                   xlab.height = xlab.height, theme = theme, mutable = mutable,
                   hasAxis = hasAxis, fixed = fixed, padding = padding,
                   labeled = labeled, label.bg.color = label.bg.color,
                   label.bg.fill = label.bg.fill,
                   label.text.color = label.text.color,
                   label.text.angle = label.text.angle,
                   track.plot.color = track.plot.color,
                   track.bg.color = track.bg.color,
                   label.text.cex = label.text.cex,
                   label.width = label.width)
    track_args <- list(backup = backup)
    track_args <- c("Tracks", track_args, backup)
    tracks <- do.call(new, track_args)
    ggplot2:::set_last_plot(tracks)
    tracks
}

setMethod("summary", "Tracks", function(object) {
    cat("-------------------------------------------\n")
    cat("Tracks contains: ", length(object@grobs), " graphic objects\n")
    cat("-------------------------------------------\n")
    cat("xlim:", object@xlim, "\n")
    cat("heights", object@heights, "\n")
    cat("fixed", object@fixed, "\n")
    cat("track.plot.color", object@track.plot.color, "\n")
    cat("-------------------------------------------\n")
})

setAs("Tracks", "grob", function(from) {
    args <- list(heights = from@heights,
                 padding = from@padding,
                 track.plot.color = from@track.plot.color,
                 track.bg.color = from@track.bg.color,
                 main = from@main,
                 xlab = from@xlab,
                 main.height = from@main.height,
                 scale.height = from@scale.height,
                 xlab.height = from@xlab.height)
    grobs <- do.call(alignPlots, c(list(from@grobs), args))
})

print.Tracks <- function(x) {
    grid.newpage()
    grid.draw(as(x, "grob"))
    ggplot2:::set_last_plot(x)
}

setMethod("show", "Tracks", function(object) {
    print(object)
    ggplot2:::set_last_plot(object)
})

setMethod("+", signature = c("Tracks", "ANY"), function(e1, e2) {
    N <- length(e1@grobs)
    .theme <- intersect(names(attributes(e2)), .tracks.theme)
    idx <- vapply(e1@grobs, mutable, logical(1L))
    for (i in seq_len(N)[idx]) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    if (length(.theme)) {
        for (z in seq_len(length(.theme))) {
            slot(e1, .theme[z]) <- attr(e2, .theme[z])
        }
    }
    e1
})

setMethod("+", signature = c("Tracks", "theme"), function(e1, e2) {
    N <- length(e1@grobs)
    .theme <- intersect(names(attributes(e2)), .tracks.theme)
    idx <- vapply(e1@grobs, mutable, logical(1L))
    for (i in seq_len(N)[idx]) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    if (length(.theme)) {
        for (z in seq_len(length(.theme))) {
            slot(e1, .theme[z]) <- attr(e2, .theme[z])
        }
    }
    e1@theme <- e2
    e1
})

setOldClass("zoom")
setMethod("+", signature = c("Tracks", "zoom"), function(e1, e2) {
    xlim <- e1@xlim
    e1@xlim <- .zoom(xlim, as.numeric(e2))$limits$x
    N <- length(e1@grobs)
    for (i in seq_len(N)) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    e1
})

setOldClass("position_c")
setMethod("+", signature = c("Tracks", "position_c"), function(e1, e2) {
    if ("x" %in% e2$aesthetics) {
        if (!is.null(e2$limits))
            e1@xlim <- e2$limits
    }
    N <- length(e1@grobs)
    for (i in seq_len(N)) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    e1
})

setOldClass("cartesian")
setMethod("+", signature = c("Tracks", "cartesian"), function(e1, e2) {
    if (!is.null(e2$limits$x))
        e1@xlim <- e2$limits$x
    if (!is.null(e2$limits$y)) {
        for (i in seq_len(length(e1@ylim))) {
            if (!fixed(e1@grobs[[i]]) && !is(e1@grobs[[i]], "Ideogram"))
                e1@ylim[[i]] <- e2$limits$y
        }
    }
    N <- length(e1@grobs)
    for (i in seq_len(N)) {
        if (!fixed(e1@grobs[[i]]))
            e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    e1
})

xlim_car <- function(x) {
    class(x) <- c(class(x), "xlim")
    x
}

setMethod("xlim", "numeric", function(obj, ...) {
    if (length(list(...)))
        obj <- c(obj, ...)
    if (length(obj) > 2) {
        obj <- range(obj)
    }
    res <- ggplot2::coord_cartesian(xlim = obj)
    xlim_car(res)
})

setMethod("xlim", "IRanges", function(obj, ...) {
    xlim <- c(start(obj), end(obj))
    res <- ggplot2::coord_cartesian(xlim = xlim)
    xlim_car(res)
})

setMethod("xlim", "GRanges", function(obj, ...) {
    xlim <- c(start(ranges(reduce(obj, ignore.strand = TRUE))),
              end(ranges(reduce(obj, ignore.strand = TRUE))))
    res <- ggplot2::coord_cartesian(xlim = xlim)
    chr <- unique(as.character(seqnames(obj)))
    attr(res, "chr") <- chr
    attr(res, "ori") <- obj
    xlim_car(res)
})

setMethod("xlim", "Tracks", function(obj, ...) {
    obj@xlim
})

setReplaceMethod("xlim", c("Tracks", "IRanges"), function(x, value) {
    xlim <- c(start(value), end(value))
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i) {
        ylim <- x@ylim[[i]]
        s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
        if (i %in% which(!x@fixed))
            x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setReplaceMethod("xlim", c("Tracks", "GRanges"), function(x, value) {
    xlim <- c(start(ranges(reduce(value, ignore.strand = TRUE))),
              end(ranges(reduce(value, ignore.strand = TRUE))))
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i) {
        ylim <- x@ylim[[i]]
        s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
        if (i %in% which(!x@fixed))
            x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setReplaceMethod("xlim", c("Tracks", "numeric"), function(x, value) {
    xlim <- range(value)
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i) {
        ylim <- x@ylim[[i]]
        s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
        if (i %in% which(!x@fixed))
            x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setGeneric("reset", function(obj, ...) standardGeneric("reset"))
setMethod("reset", "Tracks", function(obj) {
    nms <- setdiff(slotNames(obj), "backup")
    for (nm in nms) {
        slot(obj, nm) <- obj@backup[[nm]]
    }
    xlim(obj) <- obj@xlim
    obj
})

setGeneric("backup", function(obj, ...) standardGeneric("backup"))
setMethod("backup", "Tracks", function(obj) {
    nms <- setdiff(slotNames(obj), "backup")
    for (nm in nms) {
        obj@backup[[nm]] <- slot(obj, nm)
    }
    obj
})

getHeight <- function(dts) {
    hts <- do.call(unit.c, lapply(dts, height))
    hts
}

parseHeight <- function(hts, n) {
    if (length(hts) != n && length(hts) != 1)
        stop("Heights must be of length 1 or numbers of graphics")
    if (is.numeric(hts) && !is.unit(hts)) {
        if (length(hts) == 1)
            res <- rep(unit(1, "null"), n)
        if (length(hts) == n)
            res <- unit(hts, "null")
    } else if (is.unit(hts)) {
        res <- hts
    }
    res
}

## combining
## do something fun here, make combination method for Tracks
## support
## 1. c(Tracks, Tracks)
## 2. Tracks + Tracks
## 3. Tracks(Tracks, Tracks)
## 4. Tracks + plot (not yet)
setMethod("+", signature = c("Tracks", "Tracks"), function(e1, e2) {
    e1 <- c(e1, e2)
    e1
})

setMethod("c", "Tracks",  function(x, ...) {
    if (missing(x)) {
        args <- unname(list(...))
        x <- args[[1L]]
    } else {
        args <- unname(list(x, ...))
    }

    if (length(args) == 1L)
        return(x)

    arg_is_null <- vapply(args, is.null, FUN.VALUE = logical(1L))
    isClassValid <- vapply(args, is, class(x), FUN.VALUE = logical(1L))

    if (any(arg_is_null))
        args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
    if (!all(isClassValid))
        stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")

    lst <- lapply(args, function(x) {
        x@grobs
    })
    ## FIXME: how to keep other attributes?
    res <- do.call(tracks, do.call(c, lst))
    res
})

setMethod("cbind", "Tracks",  function(...) {
    args <- list(...)
    isTrack <- vapply(args, is, "Tracks", FUN.VALUE = logical(1L))
    if (all(isTrack)) {
        lst <- lapply(args, as, "grob")
        res <- do.call(cbind, lst)
    } else {
        stop("need to be of class Tracks")
    }
    grid.draw(res)
})

setMethod("rbind", "Tracks",  function(...) {
    args <- list(...)
    isTrack <- vapply(args, is, "Tracks", FUN.VALUE = logical(1L))
    if (all(isTrack)) {
        lst <- lapply(args, as, "grob")
        res <- do.call(rbind, lst)
    } else {
        stop("need to be of class Tracks")
    }
    grid.draw(res)
})

setMethod("[", c("Tracks", "numeric", "missing", "ANY"),
          function(x, i, j, ..., drop=TRUE) {
              i <- as.integer(i)
              initialize(x,
                  grobs = x@grobs[i],
                  plot = x@plot[i],
                  labeled = x@labeled[i],
                  heights = x@heights[i],
                  xlim = x@xlim,
                  ylim = x@ylim,
                  xlab = x@xlab,
                  main = x@main,
                  main.height = x@main.height,
                  scale.height = x@scale.height,
                  xlab.height = x@xlab.height,
                  theme = x@theme,
                  mutable = x@mutable[i],
                  hasAxis = x@hasAxis[i],
                  fixed = x@fixed[i],
                  padding = x@padding,
                  label.bg.color = x@label.bg.color[i],
                  label.bg.fill = x@label.bg.fill[i],
                  label.text.color = x@label.text.color[i],
                  label.text.angle = x@label.text.angle[i],
                  track.plot.color = x@track.plot.color[i],
                  track.bg.color = x@track.bg.color[i],
                  label.text.cex = x@label.text.cex[i],
                  label.width = x@label.width)
          })


gtable_filter_grobs <- function(g, type) {
    rowSums(vapply(type, function(t) startsWith(g$layout$name, t),
                   logical(length(g$layout$name)))) > 0L
}

# set title of top level plot (generated by patchwork)
patchwork_set_title <- function(plot, title) {
    theme <- plot$patches$annotation$theme
    theme <- theme + theme(plot.title = element_text(hjust = 0.5))
    plot + patchwork::plot_annotation(title = title, theme = theme)
}

# set x label of top level plot (generated by patchwork)
patchwork_set_xlab <- function(plot, label) {
    # take backup of annotation
    annotation <- plot$patches$annotation
    label <- ggplot() + geom_text(aes(x = .5, y = .5), label = label) + theme_void()
    # create design (e.g for 2 plots it generates "ab\ncc")
    total_plots <- length(plot$patches$plots) + 1L
    design <- c(letters[seq_len(total_plots)], "\n", rep(letters[total_plots + 1L], total_plots))
    design <- paste0(design, collapse = "")
    # add label
    plot <- (patchwork::plot_spacer() + plot) + label +
            patchwork::plot_layout(heights = c(97, 3), widths = c(0, 100), design = design)
    # restore annotation backup
    plot$patches$annotation <- annotation
    plot
}

# set background color of top level plot (generated by patchwork)
patchwork_set_bgcolor <- function(plot, color) {
    theme <- plot$patches$annotation$theme
    theme <- theme + theme(plot.background = element_rect(fill = color, color = color))
    plot + patchwork::plot_annotation(theme = theme)
}

alignPlots <- function(..., vertical = TRUE, widths = NULL,
                       heights = NULL, height = NULL,
                       width = NULL, padding = NULL,
                       track.plot.color = NULL,
                       track.bg.color = NULL,
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.angle = 90,
                       label.text.cex = 1,
                       label.width = unit(2.5, "lines"),
                       main.height = unit(1.5, "lines"),
                       scale.height = unit(1, "lines"),
                       xlab.height = unit(1, "lines"),
                       main = NULL,
                       xlab = NULL,
                       remove.y.axis = c(T, T),
                       remove.x.axis = c(T, T)) {
    library(patchwork)
    if (is.numeric(scale.height) && !is.unit(scale.height))
        scale.height <- unit(scale.height, "lines")

    if (is.numeric(main.height) && !is.unit(main.height))
        main.height <- unit(main.height, "lines")

    if (!is.null(height) && is.null(heights))
        heights <- height

    if (!is.null(width) && is.null(widths))
        widths <- width

    grobs <- list(...)

    if (length(grobs)) {
        if (length(grobs) == 1  && !is.ggplot(grobs[[1]]) && is.list(grobs[[1]]))
            grobs <- grobs[[1]]
    } else {
        return(ggplot())
    }

    plots <- lapply(grobs, function(x) {
        if (is(x, "GGbio"))
            x <- x@ggplot
        x
    })

    N <- length(plots)

    if (length(track.plot.color) == 1)
        track.plot.color <- rep(track.plot.color, N)

    # change background color of every plot those are inside top level plot
    plots <- lapply(seq_len(N), function(i) {
        color <- track.plot.color[i]
        theme <- theme(plot.background = element_rect(fill = color, color = color),
                       legend.background = element_rect(fill = color, color = color))
        plots[[i]] + theme
    })

    # remove y axis for given which(remove.y.axis)
    if (any(remove.y.axis)) {
        theme <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        for (i in which(remove.y.axis))
            plots[[i]] <- plots[[i]] + theme + ylab("")
    }

     # remove x axis for given which(remove.y.axis)
    if (any(remove.x.axis)) {
        theme <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                       plot.title = element_blank()) # <!> Why remove the plot title?

        for (i in which(remove.x.axis))
            plots[[i]] <- plots[[i]] + theme + xlab("")
    }

    # patchwork do all the alignment work
    if (vertical)
        plot <- patchwork::wrap_plots(plots, ncol = 1L)
    else
        plot <- patchwork::wrap_plots(plots, ncol = 2L)

    if (vertical) {
        # working on top level plot
        if (length(main))
            plot <- patchwork_set_title(plot, main)
        if (length(xlab))
            plot <- patchwork_set_xlab(plot, xlab)
    }

    # change background color of top level plot
    plot <- patchwork_set_bgcolor(plot, track.bg.color)

    # To fix aesthetics color issue with contained and top level plot
    color <- track.bg.color
    theme <- theme(plot.background = element_rect(fill = color, color = color),
                   legend.background = element_rect(fill = color, color = color))

    for (i in seq_len(N-1))
        plot$patches$plots[[i]] <- plot$patches$plots[[i]] + theme
    # last plot can be manipulated directly(patchwork)
    plot <- plot + theme

    labels <- vapply(grobs, labeled, logical(1L))
    label_names <- names(labels)

    plot
}

align.plots <- alignPlots

ggbioGrob <- function(x) {
    if (is(x, "GGbio"))
        ggplot2::ggplotGrob(x@ggplot)
    else
        ggplot2::ggplotGrob(x)
}

getLegendGrob <- function(plot) {
    gtable <- ggbioGrob(plot)
    gtable <- gtable_filter(gtable, "guide-box")
}

arrangeGrobByParsingLegend <- function(..., nrow = NULL, ncol = NULL,
                                       widths = c(4, 1), legend.idx = NULL) {
    lst <- list(...)
    if (length(lst) == 1 && is.list(lst[[1]]))
        lst <- lst[[1]]

    legends <- lapply(lst, getLegendGrob)

    plots <- lapply(lst, function(x) {
        x <- x + theme(legend.position = "none", aspect.ratio = 1)
        ggbioGrob(x)
    })

    if (!is.null(legend.idx))
        legends <- legends[legend.idx]
    legends <- do.call(gridExtra::arrangeGrob, c(legends, list(ncol = 1)))
    plots <- do.call(gridExtra::arrangeGrob, c(plots, list(nrow = nrow, ncol = ncol)))
    print(gridExtra::grid.arrange(plots, legends, ncol = 2, widths = widths))
}
