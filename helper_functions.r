### These are a collection of helper functions and utilities.  Mostly wrappers to repetitive code


valueIfNull <- function(x, value) {
## a wrapper to check if x is null and if so returns value
    if (is.null(x))
      return(value)
    else 
      return(x)
}


is.char_of_length1 <- function(x, fail.if.not=FALSE, showWarnings=TRUE, hint=NULL) {
  x.nm <- substitute(x)
  x.nm <- capture.output(x.nm)
  is_str <- is.character(x)
  is_l1  <- length(x) == 1
  if (is_str && is_l1)
    return(TRUE)
  msg <- paste0("'", x.nm, "' must be a string vector of length 1.\n   ")
  msg <- paste0(msg, ifelse(is.null(x), "It is NULL ", paste0("It has length ", length(x), ifelse(is_str, "", sprintf(", but it is not a string (it is a %s)", class(x)[[1]]) ))))
  if (!is.null(hint))
    msg <- sprintf("%s\n             \n   HINT: %s", msg, hint)
  if (fail.if.not)
    stop(msg)
  warning(msg)
  return(FALSE)
}

isErr <- function(x)  {  
  if (isTRUE(attr(x, "isErr")))
    return(TRUE)
  ## ELSE
  return( inherits(try(eval(x), silent=TRUE), "try-error") )
}

inDebugMode <- function(...)
  return (FALSE)



addColsFrom_ <- function(
      DT.receiving
    , DT.giving
    , colsToBring  = setdiff(names(DT.giving), joinCols.giv)
    , joinCols.rec = joinCols
    , joinCols.giv = joinCols
    , joinCols     = key(DT.receiving)
    , nms.newCols  = colNamesFromVector(colsToBring)
    , showWarnings = TRUE) {


  ## Bank the original keys
  key.bak.r <- key(DT.receiving)
  key.bak.g <- key(DT.giving)

  ## Capture the names, For error mesages
  DT.receiving.nm <- capture.output(substitute(DT.receiving))
  DT.giving.nm <- capture.output(substitute(DT.giving))

  if (!truelength(DT.receiving) || !truelength(DT.giving))
    stop ("Check the truelength(DT) -- self reference will fail\nHINT:  Did you load these from disk?\n You can use setDT(DT) to fix")
  
  ## Error Check 
  if (any(joinCols.rec %ni% names(DT.receiving)))
    stop("Missing columns from ", DT.receiving.nm, ": \n", commaSep(setdiff(joinCols.rec, names(DT.receiving))))
  if (any(joinCols.giv %ni% names(DT.giving)))
    stop("Missing columns from ", DT.giving.nm, ": \n", commaSep(setdiff(joinCols.giv, names(DT.giving))))

  ## new names cannot already exist in DT.receiving
  if (any(nms.newCols %in% names(DT.receiving)))
    stop ("Some 'nms.newCols' already exist in ", DT.receiving.nm, ": \n", commaSep(intersect(nms.newCols, names(DT.receiving))))
  ## colsToBring must all be in DT.giving
  if (any(colsToBring %ni% names(DT.giving)))
    stop ("Some 'colsToBring' do not exist in ", DT.giving.nm, ": \n", commaSep(setdiff(colsToBring, names(DT.giving))))

  lj.r <- length(joinCols.rec)
  lj.g <- length(joinCols.giv)
  if (lj.r < lj.g)
    stop(sprintf("length of joinCols.rec (%i) is smaller than length of joinCols.giv (%i)", lj.r, lj.g))
  if (lj.r > lj.g)
    verboseMsg(showWarnings, sprintf("length of joinCols.rec (%i) is larger than length of joinCols.giv (%i)", lj.r, lj.g))

  ## TODO CHECK CLASS
  classes.r <- DT.receiving[ , sapply(.SD, class), .SDcols = joinCols.rec]
  classes.g <- DT.giving[    , sapply(.SD, class), .SDcols = joinCols.giv]

  if (!all(classes.r == classes.g[seq(classes.r)]))
    stop ("classes are not the same for the join columns. Join will fail")

  browser(expr=inDebugMode(c("addColsFrom_")), text="in addColsFrom() before changing keys")

  setkeyIfNot(DT.receiving, joinCols.rec, superset.ok=TRUE, verbose=FALSE)
  setkeyIfNot(DT.giving,    joinCols.giv, superset.ok=TRUE, verbose=FALSE)

  caught <- try({
    j.expr <- sprintf("(nms.newCols) := list(%s)", commaSep(sprintf("`i.%s`", colsToBring)))
    DT.receiving[DT.giving, j = eval(parse(text=j.expr)), allow.cartesian=TRUE]
  }, silent=TRUE)

  
  ## Confirm that the columns have been brought over -- which will not be the case if there were no rows in common
  if (showWarnings && any(nms.newCols %ni% names(DT.receiving))) {
    ## NOTE TO SELF:  I think this would catch a no-match
    # DT.receiving[, key(DT.receiving), with=FALSE][, .test := TRUE][DT.giving[, key(DT.giving), with=FALSE]][, all(is.na(.test))]
    if (all(nms.newCols %ni% names(DT.receiving)))
      warning ("None of the 'colsToBring' made it into ", DT.receiving.nm, "\nThis is possibly due to no shared matches in the key values")
    else 
      warning ("Some of the 'colsToBring' did not make it into ", DT.receiving.nm, "\nIt is unlikely that this is due to no shared matches in the key values, since the expected result is that none would have made it over. \nPlease investigate")
  }

  ## Put the keys back, specifically, before failing on error
  setkeyIfNot(DT.receiving, key.bak.r, superset.ok=FALSE, verbose=FALSE)
  setkeyIfNot(DT.giving,    key.bak.g, superset.ok=FALSE, verbose=FALSE)

  if (isErr(caught))
    stop("Attempting to add the columns failed with the following error:\n", attributes(caught)$condition$message)

  return(invisible(DT.receiving))
}


isSuperSet <- function(x, setToSearchIn, must.start.with=TRUE, strict=FALSE, verbose="only if different") {
  
  if (is.data.table(x) && is.data.table(setToSearchIn)) {
    must.start.with <- TRUE
    x <- key(x)
    setToSearchIn <- key(setToSearchIn)
  }
  

  ## If either is NULL (ie, from key(DT)) then cannot be superset
  if (!length(x) || !length(setToSearchIn))
    return(FALSE)

  if (length(x) > length(setToSearchIn))
    return(FALSE)
  

  if (must.start.with)
    issuper <- (all(setToSearchIn[seq(x)] == x))
  else 
    issuper <- all(x %in% setToSearchIn)


  ## If must be strict, then length of the set must be larger than that of x
  if (issuper && strict)
    issuper <- length(setToSearchIn) > length(x)

  if (!issuper && identical(verbose, "only if different")) {
    if (length(wh <- x[x %ni% setToSearchIn])) {
      message("The following set diff exists")
      allSetDiff(x, setToSearchIn)
    }
  }

  return(issuper)
}

setkeyIfNot <- function(DT, ..., superset.ok=TRUE, organize=FALSE, verbose=TRUE, warnForColNameInEnv=TRUE) {
## sets the key to a DT, however, first checks if 
##  the key is already set to the given column(s)
##
## if ... is only one argument and it is a variable of strings, the values of that var will be used
##    unless it is ALSO a column name of DT, in which case it is treated as a column name but will throw a warning. 
## if .... are missing, they default to names(DT)
##
## Purpose of this function is to save the overhead 
##    of setting the key when a key is already set.
## 
## superset.ok :  if the current key is a superset of the new key, do nothing
##
## organize : If TRUE will setcolorderpt(DT, keycols)


## TODO: 
##  This does not work (indexing a character vector).  Why? 
##         setkeyIfNot(sparse.DT, colsGrouped[1:2])
  
    ###                                                                                           ###
    ###   INFO ON TIMING:                                                                         ###
    ###                                                                                           ###
    ###     given a 1,991,816 x 13 DT,  and two numeric columns as keys,                          ###
    ###     which are already set, we get the following timings:                                  ###
    ###                                                                                           ###
    ###         Unit: microseconds  (ran 16 Times)                                                ###
    ###             expr         min           lq       median          uq         max neval      ###
    ###               sk 1169691.701 1232075.2535 1262345.5290 1293192.183 1392595.847    16      ###
    ###          skIfNot      14.422      15.1835      32.0665      77.529      92.712    16      ###
    ###                                                                                           ###
    ###                                                                                           ###

      
  if (is.character(DT))
    DT <- get(DT, envir=parent.frame())

  # grab the dots
  if (missing(...))
    dots <- names(DT)
  else 
    dots <- as.character(substitute(list(...))[-1])

  browser(expr=inDebugMode("setkey", "setkeyIfNot"), text="in setkeyIfNot(), right after dots taken.")

  # if dots has only one value, and it is an object name AND it is not a column name of DT
  # then substitute its value for 
  if (length(dots) == 1) {
    ## Three possibilities

    if (dots %in% names(DT)) {
      ## Throw a warning if also exists in parent.frame, except for column 'date' (a comonly used column name)
      if (warnForColNameInEnv && exists(dots, envir=parent.frame()) && dots != "date")
        warning ("Ambiguous key selected:\n\t`", dots, "` is a variable name AND a column name of the data.table.\n\nThe key will be set to the single column, `", dots, "`\nHINT: set  warnForColNameInEnv=FALSE  to hide this message")
      ## nothing else to do. 'dots' is fine

    ## If dots is the name of a vector in the parent frame, get it
    } else if ( exists(dots, envir=parent.frame()) ) {
        dots <- get(dots, envir=parent.frame())

    ## Otherwise, try using the dots themselves
    } else {
      ## presumably the '...' are some unevaluated expression
      dots.uneval <- as.list(substitute(list(...)))[-1L]
      dots.eval   <- unlist(lapply(dots.uneval, eval, envir=parent.frame()))

      ## error-check:  Confirm they eval'd to characters that are in names(DT)
      if (!is.character(dots.eval) && length(dots.eval)) {
            if (verbose) {
              cat("dots.eval before the error is: ")
              print(dput(dots.eval))
            }
            stop("invalid input")
      } else if (!all(dots.eval %in% names(DT))) {
          stop ("the following are not in names(DT):\n\t", paste_l(dots.eval[!dots.eval %in% names(DT)] ))
      }

      ## Otherwise, dots.eval is good, use that.
      dots <- dots.eval
    }
  }


  ## Convert character(0) to NULL
  if (!length(dots))
    dots <- NULL

  # ## Allow for  setkeyIfNot(DT, NULL)
  # if (!length(dots))  # dots %in% c("NULL", "c()", "character()", "character(0)") || 
  #   return(invisible(setkey(DT, NULL)))

   
  ## Make sure dots are unique
  if (any(dups <- duplicated(dots)) ) {
    warning("Arguments passed for the new key are NOT unique. Offenders are: \n\t  ", paste_l(dots[dups]))
    dots <- unique(dots)
  }

  ## Verbose output, useful for debugging, but don't want it all the time. 
  # verboseMsg(verbose, "Using the following for new key: ", paste(dots, collapse=",  "), time=FALSE)

  # grab the current key to compare against
  current <- key(DT)

  ## considered superset only if allowed and actually is superset
  ##  Using strict for the verboseMsg at the end:  If we are not setting the key and currentIsSuperSet is TRUE, 
  ##     then we know it was the reason why.
  currentIsSuperSet <- superset.ok && isSuperSet(dots, set=current, must.start.with=TRUE, strict=TRUE, verbose=FALSE)

  ## Key needs to be set iff they are NOT idenitcal AND  not a superset (the latter dependent on superset.ok)
  keyNeedsSetting <- (!identical(current, dots)) && !currentIsSuperSet  && !(length(current) + length(dots) == 0)

  # if they are not the same, change the key and return TRUE
  if (keyNeedsSetting) {
    setkeyv(DT, dots)
    verboseMsg(verbose, "Key has been set", time=FALSE)
  } else 
    verboseMsg(verbose, "Key did not need to be set", if (currentIsSuperSet) {" (current key is a superset)"} else "", time=FALSE)

  if (organize) {
    setcolorderpt(DT, startCols=dots)
  }

  return(invisible(DT))
}