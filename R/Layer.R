## Formal representation of Geometry
setClass("Geom", representation(name = "character"))

# Geom object validity function
is_valid_geom <- function(object) {
    valid_geoms <- c("rect")
    geom_name <- object@name
    if (!geom_name %in% valid_geoms) {
        paste(geom_name, "is not a valid geom")
    }
}

setValidity("Geom", is_valid_geom)

# constructor
Geom <- function(x) {
    geom <- new("Geom")
    geom <- initialize(geom, name = x)
    geom
}

## Formal representation of Statistical transformation
setClass("Stat", representation(name = "character"))

# Stats object validity function
is_valid_stat <- function(object) {
    valid_stats <- c("stepping", "identity")
    stat_name <- object@name
    if (!stat_name %in% valid_stats) {
        paste(stat_name, "is not a valid stat")
    }
}

setValidity("Stat", is_valid_stat)

# constructor
Stat <- function(x) {
    stat <- new("Stat")
    stat <- initialize(stat, name = x)
    stat
}

# Class Unions
setClassUnion("formula_OR_NULL", c("formula", "NULL"))
setClassUnion("Geom_OR_NULL", c("Geom", "NULL"))
setClassUnion("Stat_OR_NULL", c("Stat", "NULL"))

setClass("Layer", representation(data = "ANY",
                                 mapping = "uneval",
                                 geom = "Geom_OR_NULL",
                                 stat = "Stat_OR_NULL",
                                 facets = "formula_OR_NULL",
                                 labels = "list",
                                 params = "list",
                                 operations = "function"))

# constructor
Layer <- function(data, mapping, geom = NULL, stat = NULL, facets = NULL,
                  labels = list(), params = list(), operations) {

    if (missing(operations))
        stop("Attempted to create layer with no 'operations'.")

    layer <- new("Layer", data = data, mapping = mapping, geom = geom,
                 stat = stat, facets = facets, labels = labels, params = params)

    operations(layer)
}

setGeneric("stepping", function(x) standardGeneric("stepping"))

setMethod("stepping", "Layer", function(x) {

    # for GRanges when geom = "rect"
    if (is(x@data, "GRanges") && identical(x@geom@name, "rect")) {
        return(GRanges_stepping_rect(x))
    }

})


setGeneric("identity", function(x) standardGeneric("identity"))

setMethod("identity", "Layer", function(x) {

    # for GRanges when geom = "rect"
    if (is(x@data, "GRanges") && identical(x@geom@name, "rect")) {
        return(GRanges_identity_rect(x))
    }

})



