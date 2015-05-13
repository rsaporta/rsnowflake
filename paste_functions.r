  # -------------------------------------------------------------------------------------------------------------------------  #
  #  -----------------------------------------------------------------------------------------------------------------------   #
  #                                                                                                                            #
  #           File Name              :  paste functions.r                                                                      #
  #           Last Updated Funclist  :  19 Feb 2015,  1:03 PM (Thursday)                                                       #
  #                                                                                                                            #
  #           Author Name            :  Rick Saporta                                                                           #
  #           Author Email           :  RickSaporta@gmail.com                                                                  #
  #           Author URL             :  www.github.com/rsaporta                                                                #
  #                                                                                                                            #
  #           Packages Called        :  NA                                                                                     #
  #           Packages Used via NS   :  NA                                                                                     #
  #                                                                                                                            #
  #  -----------------------------------------------------------------------------------------------------------------------   #
  #                                                                                                                            #
  #   paste.call         ( ordr )                                                                                              #
  #   revString          ( x )                                                                                                 #
  #   pasteQ             ( ..., q="'", wrap=w, w="(", sep="", C=", ", qr=str_rev(q), and=NULL                                  #
  #                        , dict.parens=getDict("dict.parens"), showWarnings=TRUE )                                           #
  #   pasteQand          ( ..., and="&", w="", sep="", C=", ", q="'" )                                                         #
  #   pasteNoBlanks      ( ..., sep=" ", collapse=NULL, na.rm=FALSE )                                                          #
  #   f                  ( ..., sep2=sep, collapse2=collapse )                                                                 #
  #   paste_l            ( x, cols=3, spacer=", \t", eolAlsoHasSpacer=isTRUE(cols == 1), endl="\n", preline=""                 #
  #                        , collapse="", usefw=TRUE, sameWidth=FALSE, extra=2, na.replace="", none=NULL                       #
  #                        , maxLineWidth=88, completeLastColumn=TRUE )                                                        #
  #   pasteC             ( ..., C="" )                                                                                         #
  #   pasteAND           ( ..., C=" AND " )                                                                                    #
  #   commaSep           ( ..., C=", ", sep="", preserveNULL=FALSE )                                                           #
  #   paste_             ( ... )                                                                                               #
  #   pastendl           ( ..., collapse=NULL, .nostart=FALSE )                                                                #
  #   pasteBars          ( x, spaces=2, bar="|", collapse=NULL, centerFirst=TRUE, splitFirst=TRUE, splitOn="\n" )              #
  #   pasteR             ( x="-", n, lineBreak=FALSE )                                                                         #
  #   str_rev            ( s )                                                                                                 #
  #                                                                                                                            #
  #                                                                                                                            #
  #                                                     <END FUNCS>                                                            #
  #  -----------------------------------------------------------------------------------------------------------------------   #
  # -------------------------------------------------------------------------------------------------------------------------  #


## Combine url with parameters for REST calls
pasteURL <- function(base_url, ...) {
  dots <- as.list(...)
  dots <- list(...)
  nms <- as.character(as.list(substitute(list(...))[-1]))
  setattr(dots, "names", nms)
  Ls <- sapply(dots, length)
  if (any(Ls > 1))
    stop ("Don't know how to combine url options when more than one option value (ie, see:  ", pasteC(nms[Ls > 1], C=", "), ")")
  dots <- dots[Ls > 0]

  # needs_quotes <- !sapply(dots, is.numeric)
  # dots[needs_quotes] <- sapply(dots[needs_quotes], function(d) paste0("'", d, "'"))

  params <- sprintf("%s=%s", names(dots), dots) %>% pasteC(C="&")

  return(paste(base_url, params, sep="?"))
}

punct_sep <- function(..., sep="([[:punct:]]|\\s)*") {
    paste("", ..., "", sep=sep)
}

pasteUnion <- function(..., collapse=" UNION ", sep="") {
  paste("(", ..., ")", collapse=collapse, sep=sep)
}

# wrapper of paste call with defaults
paste.call <- function(ordr) {
  if(inherits(ordr[[1]], "name"))
    ordr <- ordr[-1]
  paste0("(", paste(ordr, collapse=", "), ")")
}


# like paste0, but with collapse="" 
pasteC <- function(..., C="")
  paste(..., collapse=C)
pasteAND <- function(..., C=" AND ")
  paste(..., collapse=C)
commaSep <- function(..., C=", ", sep="", preserveNULL=FALSE) {
  if (isTRUE(preserveNULL) && is.null(unlist(list(...))))
    return(NULL)
  paste(..., sep=sep, collapse=C)
}
paste_ <- function(...)
  paste(..., collapse="_")
pastendl <- function(..., collapse=NULL, .nostart=FALSE) {
  ret <- paste(..., sep="\n", collapse=collapse)  
  if (.nostart)
    gsub("^\\n+", "", ret)
  else 
    ret
}
pasteBars <- function(x, spaces=2, bar="|", collapse=NULL, centerFirst=TRUE, splitFirst=TRUE, splitOn="\n") {
  if (splitFirst)
    x <- unlist(strsplit(x, splitOn), use.names=FALSE)
  if (centerFirst)
    x <- center(x)
  paste0(paste0(bar, pasteR(" ", spaces)),  x, paste0(pasteR(" ", spaces), bar), collapse=collapse)
}

pasteR <- function(x="-", n, lineBreak=FALSE) {
## Repeats x n-times in a flat string
##  When n is non-positive, a blank string, "", is returned

  missing.n <- (missing(n))

  # if n is logical, we will assume it to be lineBreak
  # We want to check if n is logical, but this will fail if n is misisng.
  # therefor, we have to short circuit. 
  if (!missing.n && is.logical(n)) {
    lineBreak <- n
    missing.n <- TRUE
  }

  ## allow for `pasteR(n)`
  if (missing.n && is.numeric(x)) {
    n <- x
    x <- "-"
  }

  # if n is not a single number, iterate
  if (length(n) > 1) {
    if (length(n) == length(x))
      return(mapply(pasteR, x, n, lineBreak=lineBreak))
    return( sapply(n, function(n1) pasteR(x, n1, lineBreak=lineBreak)) )
  }

  if (is.factor(n))
    n <- nchar(as.character(n))

  # use the chararacter length of n, if it is a string
  if(is.character(n))
    n <- nchar(n)

  # Negative values are considered 0. 
  n[n<0] <- 0

  # otehrwise, simple return
  paste0(pasteC(rep(unlist(x), n)), if (lineBreak) {"\n"})
}

str_rev <- function(s) {
## usedin pasteQ
  if (!length(s) || all(nchar(s) == 1))
    return(s)
  sapply(strsplit(s, ""), function(x) pasteC(rev(x)))
}

revString <- function(x) {
## This is an older version of this function. 
## TODO:  Get rid of this function, but rename str_rev to revString
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
}



pasteQ <- function(...,  q="'", wrap=w, w="(", sep="", C=", ", qr=str_rev(q), and=NULL, dict.parens=getDict("dict.parens"), showWarnings=TRUE) { 
  # Encloses the terms in a quotes.
  # If `wrap` is not NULL, also adds those to each end. 
  #    `wrap` defaults to "("..")" and should be set to NULL/FALSE to turn off

  ## Check for user-error in argument name
  if ("collapse" %in% names(list(...)) && showWarnings)
    warning ("\n", pasteR(30), "\nFunction pasteQ() does NOT use argument 'collapse='\nInstead use 'C='  (capital 'C')\n\nWhat follows is the match.call():\n", paste("\t", capture.output(head(sys.calls(), -7)), collapse="\n"), "\n", pasteR(30))

  # alternates to NULL should be interpreted to NULL
  if (is.null(wrap) || is.na(wrap) || wrap=="" || identical(wrap, FALSE))
    wrap <- NULL

  # `wrapR` is the closing-equiv of `wrap.` If no such equiv found, use `wrap`.
  wrapR <- dict.parens[wrap]
  wrapR <- ifelse(is.na(wrapR), wrap, wrapR)

  ## First take as a list, because different elements may need to be coerced differently
  elements <- unlist(lapply(list(...), as.character), use.names=FALSE)

  ## If the user trully wants a NULL qr, then pasteQ is not the correct function for them. 
  if (is.null(qr))
    qr <- q

  ## to combine with and, collapse the last two elements
  tl <- c()
  if (!is.null(and) && length(elements) > 1) {
    if (!grepl("^\\s", and) && !grepl("\\s$", and))
      and <- paste0(" ", and, " ")
    tl <- paste(q, tail(elements, 2), qr, sep=sep, collapse=and)
    elements <- head(elements, -2)
  }

  # if there were only exactly two elements
  h <- { if (!length(elements) && length(tl))
          c()
        else 
          paste(q, elements, qr, sep=sep, collapse=C)
       }

  full <- paste(c(h, tl), collapse=C)

  return( paste0(wrap, full, wrapR) )
}

pasteQand <- function(..., and="&", w="", sep="", C=", ", q="'") {
## Wrapper function for pasteQ, using ' & ' and no warpping paren, but yes ', '
  pasteQ(..., and=and, w=w, sep=sep, C=C, q=q)
}



pasteNoBlanks <- function(..., sep=" ", collapse=NULL, na.rm=FALSE) { 
  dots <- list(...)
  # remove NAs
  if(na.rm)
    dots <- dots[!is.na(dots)]

  # remove blanks
  remove <- identical(sep, paste0(dots, sep)) | (nchar(dots)==0)  |  (lapply(dots, length)==0)
  dots   <- dots[!remove] 

  f <- function(..., sep2=sep, collapse2=collapse){
    paste(..., sep=sep2, collapse=collapse2)
  }

  # return pasted value
  return(Reduce(f, dots))
}


# ------------------------------------------------------------------ #



paste_l2 <- function(x, cols=3, use.names=FALSE, endl="\n", preline="", colWidth="auto") {

  if (is.list(x))
    x <- unlist(x, use.names=use.names)

  if (!length(x))
    return(character())

  if (!is.character(x))
    x <- as.character(x)

  remainder <- length(x) %% cols
  if (remainder != 0)
    x <- c(x, rep("", cols - remainder))
  
  if (is.numeric(colWidth))
    x <- c(rep(sprintf(paste0("%", colWidth - 1, "s"), ""), cols), x)

  M <- matrix(x, ncol=cols, byrow=TRUE)

  old_width <- getOption("width")
  options(width=500)
  cot <- capture.output(print(M, quote=FALSE))[-1L]
  options(width=old_width)

  if (is.numeric(colWidth))
    cot <- cot[-1L]

  ret <- sub("^\\s*\\[\\d+,\\]\\s?", "", cot)

  if (!is.null(preline) && preline != "")
    ret <- paste(preline, ret)

  paste(ret, collapse="\n")
}


paste_l <- function(x, cols=3, spacer=", \t", eolAlsoHasSpacer=isTRUE(cols==1), endl="\n", preline="", collapse=""
                  , usefw=TRUE, sameWidth=FALSE, extra=2, na.replace="", none=NULL
                  , maxLineWidth=88, completeLastColumn=TRUE) {

## TODO:  change name of `eolAlsoHasSpacer` to `eolsGetSpacer` or similar
if (!missing(maxLineWidth))
  warning("`maxLineWidth` is not yet implemented.")

## wrapper function to collapse x into `cols` many columns. 
## Each element of x is separated by spacer. 
##   if eolAlsoHasSpacer is TRUE, spacer is added to alll of x
##   else spacer is added only to the none-eol elements of x
## note that the elements of x should all be length one
##       and should all be coerceible into character.
##      Otherwise, results could be unpredictable. 
## usefw : if TRUE, use fw to format the columns
## sameWidth : only applies if usefw is TRUE. Should cols all have the same width or should each be its own minwidth. 
## (x not used) singlereturn : if TRUE, return value gets paste'd before returning
##  Examples: 
##            cat("\n",paste_l(LETTERS, 5))
##            cat("\n",paste_l(LETTERS, " - "))
##            cat("\n",paste_l(LETTERS, 4, " - "))


  # Shorthand to allow for the second argument to be the spacer
  #  This feature is undocumented. 
  if (!length(x))
    return(x)

  if (is.list(x)) {
    if (!all(1==sapply(x, length)))
      warning("The `x` sent to paste_l is a list and results may not display right")
    x <- unlist(x, recursive=TRUE, use.names=FALSE)
  }

  if (is.character(cols) && missing(spacer)) {
    spacer <- cols
    cols <- 3
  }

  if (missing(spacer) && sameWidth) 
    spacer <- ", "

  if (!length(x))
    return(none)

  if (is.numeric(spacer))
    spacer <- pasteR(" ", spacer)

  # allow the user to use a logical value for collapse=FALSE
  if (identical(collapse, FALSE))
    collapse <- NULL
  if (identical(collapse, TRUE))
    collapse <- ""

  ## replace NA's, normally with blank space
  if (!is.null(na.replace))
    x[is.na(x)] <- na.replace

  ## Calculate 
  inds.endl <- seq_along(x) %% cols == 0

  # add spacer to the other elements of x (or to all of x, if flagged)
  L <- length(x)

  # ensure cols is not larger than L
  cols <- min(L, cols)
  if (eolAlsoHasSpacer) 
    x[-L] <- paste0(x[-L], spacer)
  else {
    # preserve the last element
    x.last <- x[L] 
    x[!inds.endl] <- paste0(x[!inds.endl], spacer)
    x[L] <- x.last
  }

  ## complete the last columns if needed
  if (completeLastColumn && (L %% cols != 0) ) {
    n.mis <- cols - (L%%cols)
    x <- c(x, rep("", n.mis))
    L <- L + n.mis 
  }


  if(usefw && !sameWidth) {
      ## For this case, ret will be structured such that 
      ##   ret[[1]] is the first element (column) of each line
      ##   ret[[2]] is the second element (column) of each line
      ##   etc
  
      ret <- tapply(x, ((seq_along(x) %% cols) - 1) %% cols, fwc, extra=extra ) 
      # Add a line break only if there are strictly more elements than requested columns
      if (cols < L)
        ret[[cols]] <- paste0(ret[[cols]], endl)
      # part of ret will get recycled in the final paste. We need to padd it ot prevent this
      sizes <- sapply(ret, length)
      if(any(wh <- sizes < max(sizes)))
          ret[wh] <- lapply(ret[wh], c, "")
  
      # Paste in any preline
      ret[[1]] <- paste0(preline, ret[[1]])
  
      ## THIS GETS RETURNED
      return(do.call(paste0, c(as.list(ret), collapse=collapse)))
      # OLD:         return(do.call(paste0, as.list(ret)))
  } else {
  
      if (usefw && sameWidth)
          x <- fwc(x, extra=extra)
  
      x[inds.endl] <- paste0(x[inds.endl], endl)
      # collapse into a single parag
      return(paste(preline, x, collapse=collapse)) # Old, before preline: return(paste(x, collapse=""))
  }
}
