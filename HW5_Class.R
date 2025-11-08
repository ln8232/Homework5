## HW5 Class/Methods

#--- Class ---#
setClass(
    Class = 'sparse_numeric',
    slots = c(
        value = 'numeric',
        pos = 'integer',
        length = 'integer'
    )
)

#--- Validity ---#
setValidity(
    Class = 'sparse_numeric',
    method = function(object) {
        # Check for NA in value and pos vectors
        if (anyNA(object@value)) return('Elements in value cannot be NA')
        if (anyNA(object@pos)) return('Elements in pos cannot be NA')
        # Check if pos only contain positive integers
        if (any(object@pos <= 0)) return('Elements in pos must be positive integers')
        # Check if length is a single positive integer
        if (length(object@length) != 1 || object@length <= 0) return('Length must be a single positive integer')
        # Check if length of value and pos are the same
        if (length(object@value) != length(object@pos)) return('val and pos must contain the same number of elements')
        # Check if elements in pos are unique (indices)
        if (anyDuplicated(object@pos)) return('pos must not contain duplicate elements')
        # Check if element in pos is within length
        if (any(object@pos > object@length)) return('Elements in pos cannot be greater than length')
        TRUE
    }
)

#--- Generic functions ---#

# Add
setGeneric(
    name = 'sparse_add',
    def = function(x, y, ...) {
        standardGeneric('sparse_add')
    }
)

# Subtract
setGeneric(
    name = 'sparse_sub',
    def = function(x, y, ...) {
        standardGeneric('sparse_sub')
    }
)

# Multiply
setGeneric(
    name = 'sparse_mult',
    def = function(x, y, ...) {
        standardGeneric('sparse_mult')
    }
)

# Cross product
setGeneric(
    name = 'sparse_crossprod',
    def = function(x, y, ...) {
        standardGeneric('sparse_crossprod')
    }
)

# Mean (additional method)
setGeneric(
    name = 'sparse_mean',
    def = function(x, ...) {
        standardGeneric('sparse_mean')
    }
)

#--- Set Methods ---#

# Add
setMethod(
    f = 'sparse_add',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        pos <- sort(unique(c(x@pos, y@pos)))
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        res <- numeric(length(pos))
        names(res) <- pos
        inX <- names(res) %in% names(xVals)
        inY <- names(res) %in% names(yVals)
        res[inX] <- xVals[names(res)[inX]]
        res[inY] <- res[inY] + yVals[names(res)[inY]]
        temp <- res != 0
        new(
            Class = 'sparse_numeric',
            value = as.numeric(res[temp]),
            pos = as.integer(names(res)[temp]),
            length = x@length
        )
    }
)

setMethod(
    f = '+',
    signature = c(e1 = 'sparse_numeric', e2 = 'sparse_numeric'),
    definition = function(e1, e2) {
        sparse_add(e1, e2)
    }
)

# Subtract
setMethod(
    f = 'sparse_sub',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        pos <- sort(unique(c(x@pos, y@pos)))
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        res <- numeric(length(pos))
        names(res) <- pos
        inX <- names(res) %in% names(xVals)
        inY <- names(res) %in% names(yVals)
        res[inX] <- xVals[names(res)[inX]]
        res[inY] <- res[inY] - yVals[names(res)[inY]]
        temp <- res != 0
        new(
            Class = 'sparse_numeric',
            value = as.numeric(res[temp]),
            pos = as.integer(names(res)[temp]),
            length = x@length
        )
    }
)

setMethod(
    f = '-',
    signature = c(e1 = 'sparse_numeric', e2 = 'sparse_numeric'),
    definition = function(e1, e2) {
        sparse_sub(e1, e2)
    }
)

# Multiply
setMethod(
    f = 'sparse_mult',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        pos <- sort(unique(c(x@pos, y@pos)))
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        res <- numeric(length(pos))
        names(res) <- pos
        inX <- names(res) %in% names(xVals)
        inY <- names(res) %in% names(yVals)
        res[inX] <- xVals[names(res)[inX]]
        res[inY] <- res[inY] * yVals[names(res)[inY]]
        temp <- res != 0
        new(
            Class = 'sparse_numeric',
            value = as.numeric(res[temp]),       # remove names
            pos = as.integer(names(res)[temp]),
            length = x@length
        )
    }
)

setMethod(
    f = '*',
    signature = c(e1 = 'sparse_numeric', e2 = 'sparse_numeric'),
    definition = function(e1, e2) {
        sparse_mult(e1, e2)
    }
)

# Cross product
setMethod(
    f = 'sparse_crossprod',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        # Check if length of vectors are the same
        if (x@length != y@length) stop('Sparse vectors must be the same length')
        # Setup
        xVals <- setNames(x@value, x@pos)
        yVals <- setNames(y@value, y@pos)
        # Compute cross product
        sum(xVals[intersect(names(xVals), names(yVals))] * yVals[intersect(names(xVals), names(yVals))])
    }
)

# Show
setMethod(
    f = 'show',
    signature = 'sparse_numeric',
    definition = function(object) {
        cat('sparse_numeric object of length', object@length, '\n')
        cat('Non-zero elements:\n')
        print(data.frame(pos = object@pos, value = object@value))
    }
)

# Plot
setMethod(
    f = 'plot',
    signature = c(x = 'sparse_numeric', y = 'sparse_numeric'),
    definition = function(x, y, ...) {
        pos <- intersect(x@pos, y@pos)
        plot(pos, x@value[match(pos, x@pos)], 
             type = 'p', xlab = 'Position', ylab = 'Value', ...)
        points(pos, y@value[match(pos, y@pos)])
    }
)

# Mean (additional method)
setMethod(
    f = 'sparse_mean',
    signature = 'sparse_numeric',
    definition = function(x, ...) {
        sum(x@value) / x@length
    }
)

#--- Coercions ---#

setAs("numeric", "sparse_numeric", function(from) {
    new("sparse_numeric", value = from[from != 0], pos = which(from != 0), length = length(from))
})

setAs("sparse_numeric", "numeric", function(from) {
    out <- numeric(from@length)
    out[from@pos] <- from@value
    out
})
