setGeneric("fortify", function(data, ...) standardGeneric("fortify"))

setMethod("fortify", "GRanges", function(data, ...) {
    # by setting names to NULL, reset's them back to 1:n
    names(data) <- NULL
    as.data.frame(data)
})
