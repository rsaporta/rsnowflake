
verboseQry <- function(qry, max.width=72L, max.lines=20L, spaces=3L
                      , add.dots=TRUE
                      , shortCircuit.ifendl.detected=nchar(qry) < 20000
                      , indentAnd=FALSE
                      , indentOr=FALSE
                      , all=FALSE)
{
  # max lines, not implemented

## TODO :  make sure `limit` gets shown at the end

  if (length(qry)>1)
    stop("qry must be of length 1")

  if (!length(qry)) {
    warning("Invalid query (zero length) sent to `verboseQry`\n")
    return(invisible(NULL))
  }

  ## Remove any starting blank lines
  qry <- gsub("^\n+", "", qry)

  ## Check if is Insert Query, which will be treated differently
  isInsertQry <- (grepl("^\\s*INSERT", qry[qry != ""][[1]]))

  ## Check if is SELECT Qury (or subquery)
  isSelectQry <- (grepl("^\\s*SELECT\\b", qry[qry != ""][[1]]))



  pat.union <- "\\s*UNION\\s*(\\b*(\\()?SELECT)"    ;"\\) parser"  ## for parser
  rep.union <- "@@UNION@@"
  isUnionQry <- grepl(pat.union, qry, ignore.case=TRUE)

  sp <- pasteR(" ", spaces)
  sp <- paste0("\n", sp)

  ## synonym
  p <- paste0

  # If line breaks are in the qry, don't mess with it, just pad it
  if (grepl("\\n", qry) && shortCircuit.ifendl.detected && !isInsertQry && !isUnionQry)
    return(p(sp, gsub("\\n", sp, qry)))

  patOf <- function(word) 
      p("(^|\\s)", word ,"\\b\\s*")

  ## TODO 20150120: 
  ##  Add a Linebreak and **[indent]**
  ##  Check if preceeded by another value
  ##  SELECT:  Preceded by "(", "SELECT", "WHERE", "FROM"
  ##  FROM  :  Preceded by  "WHERE", "FROM"
  ##  WHERE :  Preceded by  "WHERE"

  if (isUnionQry) { 
      qry <- gsub(pat.union, paste0(rep.union, "\\1"), qry, ignore.case=TRUE)
      return(pasteC(lapply(strsplit(qry, rep.union)[[1]], verboseQry), C = "\n               UNION\n\n", sep=""))
  }

  qry <- .verboseQry_chop_line_1(qry)

  ## Do not do these modifications when qry is an insert query without a select clause
  ## Otherwise, random "AND" and "OR" etc will be picked up from the insert data by accident
  if (!(isInsertQry && !isSelectQry)) {
    qry <- gsub(patOf("select"), p(sp, "SELECT "),  qry, ignore.case=TRUE)
    qry <- gsub(patOf("where"),  p(sp, "WHERE  "),  qry, ignore.case=TRUE)
    qry <- gsub(patOf("from"),   p(sp, "FROM   "),  qry, ignore.case=TRUE)
    qry <- gsub(patOf("and"),    p(sp, "  AND  "),  qry, ignore.case=TRUE)
    # qry <- gsub(patOf("or"),     p(sp, "   OR  "),  qry, ignore.case=TRUE)
    qry <- gsub(patOf("limit"),  p(sp, "LIMIT  "),  qry, ignore.case=TRUE)

    ## Order By / Group By
    qry <- gsub(patOf("((order|group)( by)?\\s+)"),    p(sp, "\\U\\2  "),    qry, ignore.case=TRUE, perl=TRUE)

    ## Joins
    qry <- gsub(patOf("(left|right|outer|inner|full outer)?( join)"),    p(sp, "\\U\\2\\3 "),    qry, ignore.case=TRUE, perl=TRUE)

    ## Some specific select items to split on, such if they are not the first item
    qry <- gsub("([A-Za-z]), ((cast|count|sum|datediff|min|max)\\()", p("\\1", sp, "     , \\U\\2"),   qry, ignore.case=TRUE, perl=TRUE)
    ")" ## for parsers

    ## fix "extract .. from ... "
    qry <- gsub(p("(extract\\s*\\('[A-Za-z]+'\\s*)" , sp, "FROM   ", "([A-Za-z\\._\"]+\\s*\\))"),  "\\1 FROM \\2",  qry, ignore.case=TRUE)
  }

  ## VALUES, specific to insert queries: 
  qry <- gsub(patOf("values"), p(ifelse(isInsertQry, "\n", sp), "VALUES\n"),   qry, ignore.case=TRUE)

  ## Split up the query by line breaks
  qry <- strsplit(qry, "\n")[[1]]
  ## drop any intial blank lines
  qry <- qry[min(which(qry != "")):length(qry)]


  ## Insert Statements get chopped differently
  if (isInsertQry) {

    qry <- qry[qry != ""]

    ## if the first line contains the columns, split those out into their own line
    qry[[1]] <- sub("(INSERT INTO .+?) *\\(", "\\1[##CUT_HERE##]  (",  qry[[1]])
    qry <- c(strsplit(qry[[1]], "\\[##CUT_HERE##\\]")[[1]],  "  ", qry[2:length(qry)])

    ## which line has values
    line.vals.start <- grep("\\bVALUES$", qry)
    line.vals <- grep("\\(.+\\)", qry)
    ## the "INSERT INTO .. " line will also have parens. We dont want that
    line.vals <- line.vals[line.vals > line.vals.start]

    # we want to keep at most 12 values lines. Also, make sure all line.vals are sequential
    if (length(line.vals) > 20 && all(diff(line.vals)==1) ) {
      qry[line.vals[[7]]] <- "   ......   "
      qry <- qry[-line.vals[8:(length(line.vals)-6)]]
      ## the remaining line vals are at line.vals[[1]], plus the next 12, except for the ellipses
      line.vals <- line.vals[[1]] - 1 + c(1:6, 8:21)  # the (-1) because we are adding one. Easier to index. 
      line.vals <- line.vals[line.vals <= length(qry)]
    }

    ## increase the max.width if not explicitly set
    if (missing(max.width))
      max.width <- max(max.width, getOption("width"))

    ## Indent the vlaues forward two spaces
    qry[line.vals] <- paste0("  ", qry[line.vals])

    ## if max.width is longer than, say 50, take out the middle of the values (instead of the end as will happen by default)
    if (max.width > 50) {
        toolong <- which(nchar(qry) > max.width)
        toolong <- intersect(toolong, c(2, line.vals)) ## adding in the columns at line 2
        nc.toolong <- nchar(qry[toolong])
        qry[toolong] <- paste0( substr(qry[toolong], 1, max.width-50) , " ..  "
                              , substr(qry[toolong], nc.toolong-44, nc.toolong )
                              )
    }

    ## remove any dual-blank lines
    blank.lines <- which(qry=="")
    if (any(wh.blank <- which(diffNA(blank.lines) == 1)))
      qry <- qry[-blank.lines[wh.blank]]
  }

  maxbrks <- as.integer(round(max.lines / 5) - 1)
  qry <- sapply(qry, chopLine, maxNumberOfBreaks=maxbrks, dotsBeyondMax=add.dots, padToSecondSpace=TRUE, padding=10L, width=max.width, flex=0.9*max.width, USE.NAMES=FALSE)
  qry <- unlist(strsplit(qry, "\n"), use.names=FALSE)
        #  FASTER THAN:  strsplit(paste(qry, collapse="\n"), "\n")[[1]]

  # drop blank lines
  if (length(qry) > max.lines && !isInsertQry)
      qry <- qry[qry!=""]

  # combine lesser lines
  if (length(qry) > max.lines) {
    ## for insertquery, stop at line 5, not line 2
    for (i in length(qry):(2+2.5*(isInsertQry && qry[3]=="  ")))
      if (nchar(qry[[i-1]]) + nchar(gsub("^\\s+", " ", qry[[i]])) < max.width) {
              qry[[i-1]] <-  paste(qry[[i-1]], gsub("^\\s+", " ", qry[[i]])) 
              qry[[i]] <- ""
      }
    qry <- qry[qry!=""]
  }

  if (indentAnd)
    qry <- gsub("^(\\s*)(AND) ", "\\1       \\2 ", qry, perl=TRUE)
  if (indentOr)
    qry <- gsub("^(\\s*)(OR) ", "\\1       \\2 ", qry, perl=TRUE)

  ## CROP, UNLESS FLAGGED NOT-TO
  if (!all) {
    ## only modify if not isInsertQry (or is more than 3x as long)
    if ((length(qry) > max.lines && !isInsertQry) || length(qry) > 3*max.lines) {
      if (!max.lines > 3)
        qry <- c(qry[1:max.lines], paste(pasteR(" ", spaces), "  ....."))
      else 
      ## Show last three lines as well
        qry <- c(qry[1:(max.lines-3)], paste(pasteR(" ", spaces), "....."), qry[(length(qry)+(-2:0))])      
    }
  }

  return(paste(qry, collapse="\n"))
}

## shorthand
.m <- makeQry


## Chop line 1
.verboseQry_chop_line_1 <- function(qry, max.width) {

  if (missing(max.width)) {
    if (exists("max.width", env=parent.frame()))
      max.width <- get("max.width", env=parent.frame())
    else
      max.width <- 100
  }

  pat.select <- "(^\\s*SELECT\\s*)\\s"

  if (grepl(pat.select, qry[[1]], ignore.case=TRUE) && grepl("\\sFROM\\s", qry[[1]], ignore.case=TRUE)) {
      first_from <- regexpr("from", qry[[1]], ignore.case=TRUE)
      lineone <- substr(qry[[1]], 1, first_from-1)
      rest_of_the_lines <- substr(qry[[1]], first_from, nchar(qry[[1]]))
      select  <- gsub(paste0(pat.select, ".*"), "\\1", lineone, ignore.case=TRUE)
      lineone <- gsub(pat.select, "", lineone, ignore.case=TRUE)
      # select
      # lineone
      nc.select <- nchar(select)
      ret.select <- chopAfterWord(lineone, ",", maxLength = max.width - nc.select, after=FALSE)
      L.rs <- length(ret.select)
      if (L.rs > 1)
        ret.select[2:L.rs] <- paste0(pasteR(" ", nc.select), ret.select[2:L.rs])
      ret.select[1] <- paste(select, ret.select[1])
      qry[[1]] <- pasteC(c(ret.select, rest_of_the_lines), C="\n")
  }
  return(qry)
}
