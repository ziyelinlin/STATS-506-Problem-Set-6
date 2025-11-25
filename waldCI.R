# class_definition
setClass("waldCI", slots = c(level = "numeric", mean = "numeric", sterr = "numeric", lb = "numeric", ub = "numeric"))


# constructor
makeWaldCI <- function(level, mean = NULL, sterr = NULL, lb = NULL, ub = NULL) {
  toNum1 <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  lvl <- as.numeric(level)
  m <- toNum1(mean)
  se <- toNum1(sterr)
  l <- toNum1(lb)
  u <- toNum1(ub)
  
  have_mean_se <- !(is.na(m) || is.na(se))
  have_bounds <- !(is.na(l) || is.na(u))
  
  if (!have_mean_se && !have_bounds)
    stop("Please provide either (mean & sterr) or (lb & ub).")
  
  z <- if (is.finite(lvl)) qnorm((1 + lvl)/2) else NA_real_
  
  # only (mean, sterr) 
  if (have_mean_se && !have_bounds) {
    # validate what user supplied
    obj <- new("waldCI", level = lvl, mean = m, sterr = se, lb = NA_real_, ub = NA_real_)
    # catches negative/NaN/Inf sterr
    validObject(obj)                      
    # derive bounds and revalidate full object
    obj@lb <- obj@mean - z * obj@sterr
    obj@ub <- obj@mean + z * obj@sterr
    validObject(obj)
    return(obj)
  }
  
  # only (lb, ub)
  if (!have_mean_se && have_bounds) {
    # validate what user supplied
    obj <- new("waldCI", level = lvl, mean = NA_real_, sterr = NA_real_, lb = l, ub = u)
    # catches lb>ub, Inf/NaN
    validObject(obj)                      
    # derive mean, sterr and revalidate full object
    obj@mean <- (obj@lb + obj@ub) / 2
    obj@sterr <- (obj@ub - obj@lb) / (2 * z)
    validObject(obj)
    return(obj)
  }
  
  # if both pairs supplied, build, enforce consistency by validator 
  obj <- new("waldCI", level = lvl, mean = m, sterr = se, lb = l, ub = u)
  validObject(obj)
  obj
}


# validator
setValidity("waldCI", function(object){
  error <- character(0)
  
  # level must be scalar numeric, finite, 0<level<1
  lvl <- object@level
  
  if (!is.numeric(lvl) || 
      length(lvl) != 1L ||
      is.na(lvl) || 
      !is.finite(lvl) ||
      lvl <= 0 || 
      lvl >= 1) {
    return("`level` must be a single finite number strictly between 0 and 1.")
  }
  
  # which fields are present
  have_mean <- length(object@mean) == 1L && !is.na(object@mean)
  have_sterr <- length(object@sterr) == 1L && !is.na(object@sterr)
  have_lb <- length(object@lb) == 1L && !is.na(object@lb)
  have_ub <- length(object@ub) == 1L && !is.na(object@ub)
  
  have_mean_se <- have_mean && have_sterr
  have_bounds <- have_lb && have_ub
  
  # completeness 
  if (!have_mean_se && !have_bounds)
    error <- c(error, 
               "Please provide either both (`mean` & `sterr`) or both (`lb` & `ub`).")
  if (xor(have_mean, have_sterr))
    error <- c(error, 
               "Both `mean` and `sterr` must be provided together.")
  if (xor(have_lb, have_ub))
    error <- c(error, 
               "Both `lb` and `ub` must be provided together.")
  if (length(error)) 
    return(error)
  
  # check finiteness only for supplied pair
  if (have_mean_se) {
    if (!is.numeric(object@mean) || !is.finite(object@mean))
      return("`mean` must be a single finite numeric.")
    if (!is.numeric(object@sterr) || !is.finite(object@sterr))
      return("`sterr` must be a single finite numeric.")
    # sterr rule first
    if (object@sterr < 0)
      return("`sterr` cannot be negative.")
  }
  
  if (have_bounds) {
    if (!is.numeric(object@lb) || !is.finite(object@lb))
      return("`lb` must be finite (no NA/NaN/Inf).")
    if (!is.numeric(object@ub) || !is.finite(object@ub))
      return("`ub` must be finite (no NA/NaN/Inf).")
    # bounds rule first 
    if (!(object@lb <= object@ub))
      return("`lb` must be less than or equal to `ub`.")
  }
  
  # ensure consistency
  if (have_mean_se && have_bounds && isTRUE(is.finite(lvl))) {
    z <- qnorm((1 + lvl)/2)
    lb_calc <- object@mean - z * object@sterr
    ub_calc <- object@mean + z * object@sterr
    tol <- 1e-8
    if (!isTRUE(all.equal(lb_calc, object@lb, tolerance = tol)) ||
        !isTRUE(all.equal(ub_calc, object@ub, tolerance = tol))) {
      return("Inconsistent: (mean, sterr, level) do not match (lb, ub).")
    }
  }
  
  TRUE
})


# show_method
setMethod("show", "waldCI", function(object){
  lvl <- object@level
  z <- if (is.finite(lvl)) qnorm((1+lvl) / 2) else NA_real_
  
  have_mean_se <- (length(object@mean)==1L && !is.na(object@mean)) && 
    (length(object@sterr)==1L && !is.na(object@sterr))
  have_bounds <- (length(object@lb)==1L && !is.na(object@lb)) &&
    (length(object@ub)==1L && !is.na(object@ub))
  
  if (have_bounds) {
    lb <- object@lb
  } else if (have_mean_se) {
    lb <- object@mean - z * object@sterr
  } else {
    lb <- NA_real_
  }
  
  if (have_bounds) {
    ub <- object@ub
  } else if (have_mean_se) {
    ub <- object@mean + z * object@sterr
  } else {
    ub <- NA_real_
  }
  
  cat(sprintf("Wald CI (level = %.2f)\n", lvl))
  cat(sprintf("  mean : %s\n", format(object@mean, digits = 6)))
  cat(sprintf("  sterr: %s\n", format(object@sterr, digits = 6)))
  cat(sprintf("  lb   : %s\n", format(lb, digits = 6)))
  cat(sprintf("  ub   : %s\n", format(ub, digits = 6)))
  
  if (is.finite(lb) && is.finite(ub) && lb == ub)
    cat("  note : zero-width interval (sterr == 0)\n")
})


# accessors
setGeneric("lb", function(object) standardGeneric("lb"))
setGeneric("ub", function(object) standardGeneric("ub"))
setGeneric("mean", function(x, ...) standardGeneric("mean")) 
setGeneric("sterr", function(object) standardGeneric("sterr"))
setGeneric("level", function(object) standardGeneric("level"))

setMethod("lb", "waldCI", function(object) object@lb)
setMethod("ub", "waldCI", function(object) object@ub)
setMethod("mean", "waldCI", function(x, ...) x@mean)
setMethod("sterr", "waldCI", function(object) object@sterr)
setMethod("level", "waldCI", function(object) object@level)

# setter
setGeneric("lb<-", function(object, value) standardGeneric("lb<-"))
setGeneric("ub<-", function(object, value) standardGeneric("ub<-"))
setGeneric("mean<-", function(object, value) standardGeneric("mean<-"))
setGeneric("sterr<-", function(object, value) standardGeneric("sterr<-"))
setGeneric("level<-", function(object, value) standardGeneric("level<-"))

setMethod("lb<-", "waldCI", function(object, value) {
  object@lb <- as.numeric(value)
  
  # validate bounds first (avoid recomputing sterr yet)
  object@mean <- NA_real_
  object@sterr <- NA_real_
  validObject(object)  
  
  # derive mean/sterr and re-validate
  if (length(object@ub) == 1L && !is.na(object@ub)) {
    z <- qnorm((1 + object@level) / 2)
    object@mean <- (object@lb + object@ub) / 2
    object@sterr <- (object@ub - object@lb) / (2*z)
    validObject(object)
  }
  object
})

setMethod("ub<-", "waldCI", function(object, value) {
  object@ub <- as.numeric(value)
  
  # validate bounds first (avoid recomputing sterr yet)
  object@mean <- NA_real_
  object@sterr <- NA_real_
  validObject(object)  
  
  # derive mean/sterr and re-validate
  if (length(object@lb) == 1L && !is.na(object@lb)) {
    z <- qnorm((1 + object@level) / 2)
    object@mean <- (object@lb + object@ub) / 2
    object@sterr <- (object@ub - object@lb) / (2*z)
    validObject(object)
  }
  object
})

setMethod("mean<-", "waldCI", function(object, value) {
  object@mean <- as.numeric(value)
  
  # validate mean/sterr first (don’t derive bounds yet)
  object@lb <- NA_real_
  object@ub <- NA_real_
  validObject(object)  # catches missing/partial/finite issues on mean/sterr
  
  if (length(object@sterr) == 1L && !is.na(object@sterr)) {
    z <- qnorm((1 + object@level) / 2)
    object@lb <- object@mean - z*object@sterr
    object@ub <- object@mean + z*object@sterr
    validObject(object)
  }
  object
})

setMethod("sterr<-", "waldCI", function(object, value) {
  object@sterr <- as.numeric(value)
  
  # validate mean/sterr first (don’t derive bounds yet)
  object@lb <- NA_real_
  object@ub <- NA_real_
  validObject(object) 
  
  if (length(object@mean) == 1L && !is.na(object@mean)) {
    z <- qnorm((1 + object@level) / 2)
    object@lb <- object@mean - z*object@sterr
    object@ub <- object@mean + z*object@sterr
    validObject(object)
  }
  object
})

setMethod("level<-", "waldCI", function(object, value) {
  object@level <- as.numeric(value)
  z <- qnorm((1 + object@level) / 2)
  
  # recompute the derived pair 
  if (length(object@mean) == 1L && !is.na(object@mean) &&
      length(object@sterr) == 1L && !is.na(object@sterr)) {
    object@lb <- object@mean - z*object@sterr
    object@ub <- object@mean + z*object@sterr
  } else if (length(object@lb) == 1L && !is.na(object@lb) &&
             length(object@ub) == 1L && !is.na(object@ub)) {
    object@mean  <- (object@lb + object@ub) / 2
    object@sterr <- (object@ub - object@lb) / (2*z)
  }
  validObject(object)
  object
})


# contains
setGeneric("contains", function(object, value) standardGeneric("contains"))

setMethod("contains", signature(object="waldCI", value="numeric"),
          function(object, value) {
            if (!(length(object@lb) == 1L && 
                  length(object@ub) == 1L &&
                  is.finite(object@lb) && 
                  is.finite(object@ub))) {
              stop("contains() requires finite bounds.")
            }
            
            v <- as.numeric(value)
            (v >= object@lb) & (v <= object@ub)
          })


# overlaps
setGeneric("overlap", function(x, y) standardGeneric("overlap"))

setMethod("overlap", signature(x="waldCI", y="waldCI"),
          function(x, y) {
            (length(x@lb)==1L && !is.na(x@lb)) &&
              (length(x@ub)==1L && !is.na(x@ub)) &&
              (length(y@lb)==1L && !is.na(y@lb)) &&
              (length(y@ub)==1L && !is.na(y@ub)) &&
              (max(x@lb, y@lb) <= min(x@ub, y@ub))
          })


# as_numeric
setMethod("as.numeric", "waldCI", function(x, ...) c(lb = x@lb, ub = x@ub))

# transformCI
setGeneric("transformCI", function(x, f) standardGeneric("transformCI"))

setMethod("transformCI", signature(x = "waldCI", f = "function"),
          function(x, f) {
            warning("Only monotonic functions make sense.")
            
            if (length(x@lb) != 1L || 
                is.na(x@lb) || 
                length(x@ub) != 1L || 
                is.na(x@ub)) {
              stop("transformCI requires finite bounds in the waldCI object.")
            }
            
            L <- x@lb
            U <- x@ub
            
            if (!is.finite(L) || !is.finite(U)) {
              stop("Bounds must be finite for transformCI.")
            }
            
            pts  <- seq(L, U, length.out = 101)
            vals <- vapply(pts, function(t) as.numeric(f(t)), numeric(1))
            
            if (any(!is.finite(vals))) {
              stop("`f` must return finite numeric values on [lb, ub].")
            }
            
            # monotonicity check
            tol <- 1e-10
            inc_ok <- all(diff(vals) >= -tol)
            dec_ok <- all(diff(vals) <=  tol)
            
            if (!inc_ok && !dec_ok) {
              stop("`f` is not monotone on [lb, ub]; transformation refused.")
            }
            
            # transform interval, if decreasing, swap endpoints to keep lb <= ub
            L2 <- f(L)
            U2 <- f(U)
            
            if (dec_ok && (U2 < L2)) {
              tmp <- L2
              L2 <- U2
              U2 <- tmp
            }
            
            makeWaldCI(level = x@level, lb = L2, ub = U2)
          })