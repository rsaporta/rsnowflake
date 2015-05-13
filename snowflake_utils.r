  # ------------------------------------------------------------------------------------------------------------------------------------------------------  #
  #  ----------------------------------------------------------------------------------------------------------------------------------------------------   #
  #                                                                                                                                                         #
  #           File Name              :  SF_Utils.R                                                                                                          #
  #           Last Updated Funclist  :  12 May 2015,  9:11 PM (Tuesday)                                                                                     #
  #                                                                                                                                                         #
  #           Author Name            :  Rick Saporta                                                                                                        #
  #           Author Email           :  RickSaporta@gmail.com                                                                                               #
  #           Author URL             :  www.github.com/rsaporta                                                                                             #
  #                                                                                                                                                         #
  #           Packages Called        :  RODBC                                                                                                               #
  #           Packages Used via NS   :  NA                                                                                                                  #
  #                                                                                                                                                         #
  #  ----------------------------------------------------------------------------------------------------------------------------------------------------   #
  #                                                                                                                                                         #
  #   sfErrorParse          ( qres, qry, return_error="auto" )                                                                                              #
  #   sfSetWarehouseDetails ( name, ..., size=NULL, type=NULL, time_started=now() - 30, showWarnings=TRUE )                                                 #
  #   sfWarehouseOn         ( name=getOption("snowflake_defaultwh"), size=NULL, type=NULL, con=sfGetCon(), notify=TRUE                                      #
  #                           , verbose=TRUE, snowflake_inuse=TRUE )                                                                                        #
  #   sfWarehouseOff        ( name="LOAD_WH", con=sfGetCon(), verbose=TRUE )                                                                                #
  #   sfQry                 ( qry, connex=sfGetCon(), warehouse=NULL, to.dt=exists("as.data.table")                                                         #
  #                           , refresh_con_if_expired=TRUE                                                                                                 #
  #                           , verbose.max.width=getOption("width", 80) * 0.8, verbose.max.lines=22L                                                       #
  #                           , verbose.add.dots=nchar(qry) < 5000, verbose.shortCircuit=TRUE                                                               #
  #                           , verbose.indentAnd=grepl("\\bOR\\b", qry), verbose.indentOr=FALSE, verbose=TRUE                                              #
  #                           , verbose.key=verbose, verbose.firstOfMonth=TRUE, ... )                                                                       #
  #   sfGetCon              ( uid=getOption("snowflake_uid"), pwd=getOption("snowflake_pwd"), assign=TRUE                                                   #
  #                           , envir=globalenv(), refresh=NULL )                                                                                           #
  #   dbschematbl           ( dbname=if (!is.null(schema)) "DEV" else NULL, schema=NULL, tbl=NULL )                                                         #
  #   sfShowCols            ( tbl, schema=NULL, namelike=NULL, sort=TRUE, include.types=FALSE, returnRaw=FALSE                                              #
  #                           , similar=TRUE, verbose=FALSE, simplify=TRUE, dbname="DEV", warehouse=NULL, ... )                                             #
  #   sfShowTables          ( schema=NULL, namelike=NULL, dbname="DEV", con=sfGetCon(), verbose=TRUE, ignore.case=TRUE                                      #
  #                           , cleanBytes=TRUE, nomatch=c("all", "none"), showWarnings=TRUE, ... )                                                         #
  #   sfShowWarehouses      ( namelike=NULL                                                                                                                 #
  #                           , state=c("ALL", "RUNNING", "STARTED", "SUSPENDED", "HIBERNATED"), details=c("auto", "minimial", "all"), ..., con=sfGetCon()  #
  #                           , showWarnings=TRUE, verbose=FALSE )                                                                                          #
  #   getNextHour           ( time=now(), thresh_seconds=0 )                                                                                                #
  #   sfGetWarehouseDetails ( name, ..., size=NULL, type=NULL, time_started=now() - 30, showWarnings=TRUE )                                                 #
  #                                                                                                                                                         #
  #                                                                                                                                                         #
  #                                                                    <END FUNCS>                                                                          #
  #  ----------------------------------------------------------------------------------------------------------------------------------------------------   #
  # ------------------------------------------------------------------------------------------------------------------------------------------------------  #

sfErrorParse <- function(qres, qry, return_error="auto", return_query="auto", fail_on_query_error=FALSE) {
## ARGS: 
##   qres - the raw return from sqlQuery()
##   qry  - the original query (often included in qres. It will be gsubbed out)
##  return_error -- if FALSE, returns NULL,  if TRUE returns the parsed error
##                  Only returns the error if the erro is unknown. Otherwise returns NULL


  # Most errors are of the form
  # #   [1] "<Error Number> SQL compilation error:\n<Error Message>"
  # #   [2] "[RODBC] ERROR: Could not SQLExecDirect '<ORIGINAL QUERY>'"
  # 
  # Warehouse Errors are of the form
  # #   [1] "22000 90063 Invalid state. Warehouse 'LOAD_WH' cannot be resumed since it is not suspended."
  # #   [2] "[RODBC] ERROR: Could not SQLExecDirect 'ALTER WAREHOUSE LOAD_WH RESUME USING  WAREHOUSE_SIZE = SMALL ;'"
  #
  # Other errors include: 
  # #   HY000 50110 [Snowflake][Support] (50110) Conversion from string to number failed due to overflow with value:

  nc_qr <- nchar(qry)

  if (mnchar(qres) < 700) {
    message("\n", pasteR("*", 60), "\nHere is the original error\n\n")
    dput(qres)
    message("\n", pasteR("*", 60), "\n")
  }

  pat.known_errors <- c("SQL compilation error:\\n", "Invalid state. ", "Conversion from")
  pat.line1 <- regOr(pat.known_errors, escape=FALSE)
  pat.line2 <- "[RODBC] ERROR: "

  Error_header <- "ERROR SF SQL::  "

  ## TEMPORARY -- CONFIRM I HAVE THE REGEX CORRECT
  if (grepl("SQL compilation error", qres[[1]])) {
    if (grepl(pat.line1, qres[[1]], fixed=FALSE))
      message ("REGEX IN sfErrorParse() is  [OK]")
    else
      message ("REGEX IN sfErrorParse() is  WRONG FOR : \n", pasteR(60), "\n", qres[[1]], "\n", pasteR(60), "\n")
    message("\n~~~~~~~ vvvv   [ real error parsing below ]  vvvv ~~~~~~~~~\n")
  }

  ## We expect that an error be of length 2 and to be of a certain pattern.
  ## If it fails to match this criteria, inform the user.
  if (length(qres) != 2 || !(grepl(pat.line1, qres[[1]], fixed=FALSE) && grepl(pat.line2, qres[[2]], fixed=TRUE))) {
    message ("in sfErrorParse() -- received an error that was not as expected")
    ## replace the original query
    ret <- gsub(pattern=qry, repl="< ORIGINAL QRY WENT HERE >", x=qres, fixed=TRUE)
    msg <- paste(Error_header, paste(ret, collapse="\n       "))
    ## For 'auto' return_error, the criteria to NOT return the error is that the error is two strings and the second is the expected pattern
    ## In other words, we simply have not anticipated the specific details of the error
    if (identical(return_error, "auto"))
        return_error <- (length(qres) == 2 && grepl(pat.line2, qres[[2]]))
    if (identical(return_query, "auto"))
        return_query <-  nc_qr < 500

  ## PARSE THE RROR AS EXPECTED
  } else {
    
    ## If the second line is as expected, remove it
    full_line2 <- sprintf("%sCould not SQLExecDirect '%s'", pat.line2, qry)
    qres[[2]] <- gsub(pat=full_line2, "", x=qres[[2]], fixed=TRUE)
    qres <- trim(qres)
    if (qres[[2]] == "")
      qres <- qres[[1]]


    if (length(qres) == 1) {
      ret <- strsplit(qres, pat.line1) [[1]] %>% trim %>% setNames(nm=c("error_no", "error_msg")) %>% rev
      msg <- sprintf("%s %s  (%s)", Error_header, ret[["error_msg"]], ret[["error_no"]])
    } else {
      ret <- qres
      msg <- sprintf("%s ", Error_header, paste(qres, collapse="\n       "))
    }

    if (identical(return_query, "auto"))
        return_query <-  nc_qr < 250
  }

  if (isTRUE(return_query)) {
    msg <- sprintf("%s\n\n    ---------- * ORIGINAL QUERY: * ---------- \n%s", msg, sub("^\n", "", x ={if (nc_qr < 1000) verboseQry(qry) else qry}))
  }

  ret[["original_qry"]] <- qry

  ## set error attribute
  setattr(ret, "isErr", TRUE)

  if (fail_on_query_error)
    stop(msg, call.=FALSE)

  cat(msg, fill=TRUE, sep="")

  return(invisible(ret))
}

sfUpdateWH_QryLastRan <- function(name, size=NULL, type=NULL, last_ran=now()) {
  is.char_of_length1(name, fail.if.not=TRUE)

  list_name <- .sf.make_list_name(name, size=size, type=type)
  
  current_options <- getOption("sf_warehouses", default = list())

  ## Initialize if missing
  if (is.null(current_options[[list_name]][["last_ran"]]))
    current_options[[list_name]][["last_ran"]] <- list()
  if (is.null(current_options[[list_name]][["name"]]))
    current_options[[list_name]][["name"]] <- name

  current_options[[list_name]][["last_ran"]] <- now()

  options("sf_warehouses" = current_options)
  return(invisible(current_options))
}

sfQry <- function(qry
                , connex=sfGetCon()
                , warehouse=getOption("snowflake_defaultwh")
                , to.dt=exists("as.data.table")
                , refresh_con_if_expired = TRUE
                , fail_on_query_error = TRUE

                , verbose.max.width=getOption("width", 80) * 0.8
                , verbose.max.lines=22L
                , verbose.add.dots=nchar(qry) < 5000
                , verbose.shortCircuit=TRUE
                , verbose.indentAnd=grepl("\\bOR\\b", qry)
                , verbose.indentOr=FALSE
                , verbose=TRUE
                , verbose.key=verbose
                , verbose.firstOfMonth=TRUE

                ## Used only for details updating
                , size=NULL
                , type=NULL

                , ...
) {

  ## Make sure package is loaded
  require(RODBC)

  if (length(qry) != 1)
    stop ("qry has length ", length(qry), ". It should have length 1.  Use sapply or similar")


  if (!inherits(connex, "RODBC"))
    stop ("connex is not of type 'RODBC'")


  ## TODO - warehouse - check, turn on, etc. 
  ## ...... warehouse ...... ?? 

  if (verbose) {
     hr <- paste0(pasteR(verbose.max.width+3), "\n")
     cat("\n\n\t    Running query on snowflake using ", ifelse(is.null(warehouse), "default", paste0("'", warehouse, "'")), " warehouse. [Began at ", timeStamp(frmt="%R %p") ,"] \n   ", hr
        , verboseQry(qry, max.lines=verbose.max.lines, max.width=verbose.max.width, shortCircuit.ifendl.detected=verbose.shortCircuit, indentAnd=verbose.indentAnd, indentOr=verbose.indentOr, add.dots=verbose.add.dots)
        , "\n   ", hr, sep="")
  }

  ret <- try(sqlQuery(connex, qry, stringsAsFactors=FALSE))
  browser(expr=inDebugMode("sfQry"), text="in sfQry()")

  ## Update the last_ran timestamp
  sfUpdateWH_QryLastRan(name=warehouse, size=size, type=type, last_ran=now()  )

  ## IF con IS EXPIRED, REFRESH AND TRY AGAIN
  pat.expired <- c("first argument is not an open RODBC channel", "Authentication token has expired") %>% regOr
  if (refresh_con_if_expired) {
    if (isErr(ret) || (is.character(ret) && any(grepl(pat.expired, ret)))) {
      message("connex was expired, refreshing then trying the query again")
      connex <- sfGetCon(refresh=TRUE)
      ret <- sqlQuery(connex, qry, stringsAsFactors=FALSE)
    }
  }

  ## CHECK FOR ERROR
  if (is.character(ret) && length(ret) < 5 && any(grepl("error", ret, ignore.case=TRUE)))
    return(sfErrorParse(qres=ret, qry=qry, fail_on_query_error=fail_on_query_error))
  ## ELSE, NO ERROR, continue. 

  ## Convert to data.table; wrap in try() so as to not fail after having waited for the query to run
  try (ret <- as.data.table(ret))
  return(ret)
}

sfGetCon <- function(uid=getOption("snowflake_uid"), pwd=getOption("snowflake_pwd"), drv=getOption("snowflake_driver"), assign=TRUE, envir=globalenv(), refresh=NULL) {

  if (is.null(drv))
    stop ("drv cannot be NULL.\n\nHINT: run   setSnowflakeOptions()")

  options("snowflake_inuse" = TRUE)

  if (!isTRUE(refresh) && exists("sfcon", envir=envir)) {
    sfcon <- get("sfcon", envir=envir)
    if (!isConExpired(sfcon))
      return(get("sfcon", envir=envir))
  }

  sfcon <- odbcConnect(drv, uid=uid, pwd=pwd)

  if (isTRUE(assign))
    assign("sfcon", sfcon, envir=envir)
  return(sfcon)
}


dbschematbl <- function(dbname=if (!is.null(schema)) "DEV" else NULL, schema=NULL, tbl=NULL) {
  if (is.null(dbname) && is.null(schema) && is.null(tbl))
    return(NULL)

  if (is.null(schema) && !is.null(dbname) && !is.null(tbl))
    stop ("schema cannot be NULL if both dbname and tbl are NOT null")


  if (!is.null(dbname) && !is.null(schema) && grepl(sprintf("^%s.", dbname), schema, ignore.case=TRUE)) {
    dbname <- schema
    schema <- NULL
  }

  paste0(dbname, if (!is.null(schema)) ".", schema, if (!is.null(tbl)) ".", tbl)
}


sfShowCols <- function(tbl, schema=NULL, namelike=NULL, sort=TRUE, include.types=FALSE, returnRaw=FALSE, similar=TRUE, verbose=FALSE, simplify=TRUE
                     , dbname="DEV"
                     , warehouse = NULL
                     , ...
) { 
  dst <- dbschematbl(dbname=dbname, schema=schema, tbl=tbl)
  DT.cols <- sfQry(paste("DESC TABLE ", dst))
  return(DT.cols$name)
}


sfDesc <- function(tbl=NULL, schema=NULL, dbname="DEV", con=sfGetCon(), verbose=TRUE, ignore.case=TRUE, cleanBytes=TRUE, nomatch=c("all", "none"), details=c("auto", "all", "minimal"), comments_included=TRUE, showWarnings=TRUE, ...) {
  dst <- dbschematbl(dbname=dbname, schema=schema, tbl=tbl)
  qry <- sprintf("DESC TABLE %s", dst) %>% setQry
  ret <- sfQry(qry, verbose=FALSE)

  if ("kind" %in% names(ret) && all(ret[["kind"]] == "COLUMN"))
    ret[, kind := NULL]
  if ("default" %in% names(ret) && all(is.na(ret[["default"]])))
    ret[, default := NULL]

  nr <- nrow(ret)
  blank_vec <- rep("", nr)
  if (nr)
    for (col in names(ret))
      if (all(is.na(ret[[col]])))
        ret[, (col) := blank_vec]

  return(ret)
}
sfShowTables <- function(schema=NULL, namelike=NULL, dbname="DEV", con=sfGetCon(), verbose=TRUE, ignore.case=TRUE, cleanBytes=TRUE, nomatch=c("all", "none"), details=c("auto", "all", "minimal"), comments_included=TRUE, showWarnings=TRUE, ...) {
  # if (length(schema) > 1) {
  #   args <- collectArgs()
  #   return(rbindlist(lapply()))
  # }

  nomatch <- match.arg(nomatch)
  details <- match.arg(details)


  #  dst <- dbschematbl(dbname=dbname, schema=schema, tbl=NULL)
  #
  #  qry <- paste("SHOW TABLES", if (!is.null(dst))  " IN ", dst, if (!is.null(pattern)) ifelse(nchar(pattern), paste(" LIKE ", pasteQ(pattern, C=NULL, w="")), "")) %>% setQry
  #
  #  if (length(qry) > 1) {
  #    ret <- qry %>% lapply(sfQry, verbose=verbose, con=con)
  #    try({ret %<>% rbindlist}, silent=TRUE)
  #  } else {
  #    ret <- qry %>% sfQry(con=con, verbose=verbose)
  #  }


  ## ALTERNATE - do the searching in R
  qry <- paste("SHOW TABLES IN ", dbschematbl(dbname, "*")) %>% setQry
  ret <- qry %>% sfQry(qry=., con=con, verbose=verbose)

  if (!nrow(ret))
    stop ("Internal Error. Querying for SHOW TABLES did not return any results")

  ## IDENTIFY WHICH ROWS MATCH FOR SCHEMA
  if (!is.null(schema)) {
    matched_schema <- 
        gsub(sprintf("^%s\\.", dbname), "", schema, ignore.case=TRUE) %>% 
        regOr %>% grepl(pat=., x=ret[["schema_name"]], ignore.case=ignore.case)
  } else 
    matched_schema <- TRUE


  ## IDENTIFY WHICH ROWS MATCH FOR TBL
  if (!is.null(namelike)) {
    matched_tbl <- 
      gsub(sprintf("^(%s\\.)?%s\\.", dbname %>% valueIfNull(""), schema %>% valueIfNull("")), "", namelike, ignore.case=TRUE) %>%
      regOr %>% grepl(pat=., x=ret[["name"]], ignore.case=ignore.case)
  } else 
    matched_tbl  <- TRUE

  matched_rows <- matched_schema & matched_tbl
  if (!any(matched_rows)) {
    if (showWarnings)
      warning ("No tables found matching schema = '", schema, "' and namelike = '", namelike, "'   --   Returning all tables")
    if (nomatch == "none")
      return(ret[FALSE])
  } else
    ret <- ret[matched_rows]


  if (details != "all") {
    is_minimal <- identical(details, "minimial")
    if (is_minimal ||  all(ret$owner == "SYSADMIN"))
      ret[, owner := NULL]
    if (is_minimal ||  all(ret$account_name == ret$account_name[[1]]))
      ret[, account_name := NULL]
    if (all(ret$kind == "TABLE"))
      ret[, kind := NULL]
    if (is_minimal ||  all(is.na(ret$cluster_by)))
      ret[, cluster_by := NULL]

    if ("comment" %in% names(ret)) {
      if (is_minimal ||  all(is.na(ret$comment)) || all(ret$comment == ret$comment[[1]]))
        ret[, comment := NULL]
      else 
        setcolorder(ret, c(setdiff(names(ret), "comment"), "comment"))
    }

    if (all(ret$schema_name == ret$schema_name[[1]]))
      ret[, schema_name := NULL]
    if (all(ret$database_name == ret$database_name[[1]]))
      ret[, database_name := NULL]

    if (isTRUE(cleanBytes))
      ret[, bytes := formatBytes(bytes)]
    ret[, created_on := as.Date(created_on)]
  }

  if (identical(comments_included, FALSE) && "comment" %in% names(ret))
    ret[, comment := NULL]

  ret <- ret[!grepl("20150506$", name)]

  return(ret)
}


# sfShowWarehouses()
sfShowWarehouses <- function(namelike=NULL, state=c("ALL", "RUNNING", "STARTED", "SUSPENDED", "HIBERNATED"), details=c("auto", "minimial", "all"), ..., con=sfGetCon(), showWarnings=TRUE, verbose=FALSE) {

  ## Check that state is correct
  state <- toupper(state)
  if (length(state) == 1 && state %in% c("RUNNING", "RUN"))
    stop ("There is no state called 'running'\n\nHINT: Did you mean   sate = 'started'")
  state <- match.arg(state)
  if (state == "SUSPENDED") {
    warning ("Snowflake uses inconsistent terminology regarding warhouse 'state'/'status' -- programatically, please use 'HIBERNATED' instead of 'SUSPENDED'", call.=FALSE)
    state <- "HIBERNATED"
  }

  details <- match.arg(details)

  qry <- "SHOW WAREHOUSES"
  if (!is.null(namelike))
    qry <- sprintf("%s LIKE '%s'", qry, namelike)

  ret <- sfQry(qry, con=con, verbose=verbose)
  
  ## If 'ALL' is one of the states, then we are not filtering
  if (!("ALL" %in% state)) {
    total_rows_returned <- nrow(ret)
    e <- environment()
    ret <- ret[state %in% (get("state", envir=e))]
    if (showWarnings && nrow(ret) == 0) {
      if (total_rows_returned == 0)
        warning ("No warehouses found, regardless of state")
      else
        warning ("No warehouses found for state '", state, "' -- however ", total_rows_returned, " warehouse(s) found with other states")
    }
  }

  ## TODO: Separate these, check if they are other than the default values
  if (details != "all") {
    colsDropping <- c("created_on", "auto_resume", "owner", "comment", "uuid", "is_default", "is_current") # "suspended"
    colsAuto <- list(available="", running=0, queued=0, stranded=0, pendings=0, failed=0) #  actives=0

    if (details == "minimial")
      ret[, (colsAuto) := NULL]
    else if (details == "auto") {
      for (nm.col in names(colsAuto)) {
        if (all(is.na(ret[[nm.col]])) || all(ret[[nm.col]] == colsAuto[[nm.col]]))
          ret[, (nm.col) := NULL]
      }
    }
    ret[, (colsDropping) := NULL]
  }

  warehouse_details <- sfGetWarehouseDetails()
  if (!identical(warehouse_details, list())) {
    DT.warehouse_details <- rbindlist(lapply(warehouse_details, as.data.table), use.names=TRUE)
    # DT.warehouse_details[, state := factor("On ", levels=levels(ret$state))] ## For now, assume all warehouses are "On"
    DT.warehouse_details[, state := "On"] ## For now, assume all warehouses are "On"
    ## setkey either to just name, state or to name, size, type, statea
    if (!anyDuplicated(DT.warehouse_details$name))
      setkey(DT.warehouse_details, name, state)
    else
      setkey(DT.warehouse_details, name, size, type, state)

    colsToBring <- c("time_running", mins_left="minutes_left_in_hour")
    if (all(colsToBring %in% names(DT.warehouse_details)))
      addColsFrom_(ret, DT.warehouse_details, colsToBring=colsToBring, joinCols=key(DT.warehouse_details), showWarnings=FALSE)

    if ("mins_left" %in% names(ret))
      ret[, mins_left := removeNA(gsub("mins .*", "mins", mins_left), "")]
    if ("time_running" %in% names(ret))
      ret[, time_running := removeNA(gsub("mins .*", "mins", time_running), "")]
  }

  ## Clean up part 2 -- after addColsFrom_
  if (details != "all") {
    ret[, size  := toFactorWithExpectedLevels(size,  lev=c("XSMALL", "SMALL", "MEDIUM", "LARGE", "XLARGE"), lab=c("xS", "S", "M", "L", "XL"), cleanDuplicates=TRUE)]
    ret[, type  := toFactorWithExpectedLevels(type,  lev=c("STANDARD", "ENTERPRISE"), lab=c("STANDARD", "ENTERPRISE"), cleanDuplicates=TRUE)]
    ret[, state := toFactorWithExpectedLevels(state, lev=c("STARTED", "HIBERNATED"),  lab=c("On ", "Off"), cleanDuplicates=TRUE)]

    ret[, auto_suspend := fwSecs(auto_suspend)]
    ret[, nodes_active := sprintf("%2i / %2i", actives, actives + suspended)]
    ret[, c("actives", "suspended") := NULL]

    setkeyIfNot(ret, key=c("type", "size", "name", "state"), organize=TRUE, verbose=FALSE)
  }

  return(ret)
}


getNextHour <- function(time=now(), thresh_seconds=0) {
    time_pl <- as.POSIXlt(time)
    now_pl  <- as.POSIXlt(now())

    ## Check that the timezones match
    zone.t <- time_pl$zone
    zone.n <- now_pl$zone
    if (!identical(zone.t, zone.n))
      warning (sprintf("Warning differing time zones for time_pl (%s) and now_pl (%s)", valueIfNull(zone.t, "NULL"), valueIfNull(zone.t, "NULL")))

    ## Bring over time zone information
    attr(now_pl, "tzone") <- attr(time_pl, "tzone", exact=TRUE)

    ## Bring over these individual pieces
    pieces_to_preserve <- c("zone", "gmtoff", "isdst", "sec", "min")

    now_pl$zone    <-  time_pl$zone   
    now_pl$gmtoff  <-  time_pl$gmtoff     
    now_pl$isdst   <-  time_pl$isdst   
    now_pl$sec     <-  time_pl$sec 
    now_pl$min     <-  time_pl$min 


    ## Add hours until threshold exceeded
    while (as.numeric(now_pl) - as.numeric(now()) <= thresh_seconds)
      now_pl$hour <- now_pl$hour + 1

    return(as.POSIXct(now_pl))
}

.sf.make_list_name <- function(name, size, type) {

  is.char_of_length1(name, fail.if.not=TRUE)

  if (!is.null(size))
    is.char_of_length1(size, fail.if.not=TRUE)
  if (!is.null(type))
    is.char_of_length1(type, fail.if.not=TRUE)

  sprintf("%s_%s_%s", name, valueIfNull(size, "default"), valueIfNull(type, "default"))
}


sfGetWarehouseDetails <- function(name, ..., size=NULL, type=NULL, time_started=now() - 30, showWarnings=TRUE) {

  add_warehouse_details <- function(wh_opts) {
    if (is.null(wh_opts$time_started))
      return(wh_opts)
    wh_opts$as_of <- now()
    wh_opts$time_running <- fwTDiff(start=wh_opts$time_started, end=wh_opts$as_of)
    wh_opts$next_hour <- getNextHour(wh_opts$time_started, thresh_seconds=180)
    wh_opts$minutes_left_in_hour <- fwTDiff(start=wh_opts$as_of, end=wh_opts$next_hour)
    wh_opts
  }

  all_warehouse_details <- getOption("sf_warehouses", default=list())

  if (missing(name) && missing(size) && missing(type))
    return(lapply(all_warehouse_details, add_warehouse_details))

  if (!is.character(name))
    stop ("name should be a character")

  size %<>% valueIfNull("default")
  type %<>% valueIfNull("default")

  list_name <- .sf.make_list_name(name, size, type)

  opts <- all_warehouse_details[[list_name]]

  if (!is.null(opts))
    opts <- add_warehouse_details(opts)

  return(opts)
}

sfSetWarehouseDetails <- function(name, ..., size=NULL, type=NULL, time_started=now() - 30, last_ran=NA, showWarnings=TRUE) {
  force(time_started)

  if (!is.character(name))
    stop ("name should be a character")


  ## if NULL set to default
  size %<>% valueIfNull("default")
  type %<>% valueIfNull("default")


  ## Confirm all of these expected inputs are of length one
  ## (NULLs have already been converted to 'default')
  is.char_of_length1(name)
  is.char_of_length1(size)
  is.char_of_length1(type)
  is.char_of_length1(as.character(time_started))


  list_name <- .sf.make_list_name(name, size, type)

  dots <- list(...)
  nms_dots <- substitute(list(...)) %>% as.list %>% .[-1] %>% as.character
  setnamesIfBlank_(dots, nms_dots)

  ## Put together the list of options.  Wrapping time_started in list to (a) ensure the whole vector gets coerced and (b) to keep it as POSIX
  opts <- c(name=name, size=size, type=type, dots, time_started=list(time_started[[1]]), last_ran=list(last_ran) )

  
  current_options <- getOption("sf_warehouses", default=list())
  
  if (showWarnings && !is.null(current_options[[list_name]]))
    warning ("Option '", list_name, "' already exists in options('sf_warehouses')", call.=FALSE)

  current_options[[list_name]] <- opts
  
  options(sf_warehouses = current_options)

  return (invisible(current_options))
}

sfWarehouseOn <- function(name=getOption("snowflake_defaultwh"), size=NULL, type=NULL, con=sfGetCon(), notify=TRUE, verbose=TRUE, snowflake_inuse=TRUE) {
## Note that if the warhouse is already running, this will fail

  if (isTRUE(snowflake_inuse))
    options("snowflake_inuse" = TRUE)

  start_time <- now()

  if (is.null(name))
    stop ("name is null.\nHINT: et options('snowflake_defaultwh') or pass the name explicitly")

  qry <- sprintf("ALTER WAREHOUSE %s RESUME", name)


  size_param <- 
    if (!is.null(size)) {
      size <- match.arg(toupper(size), choices=c("XSMALL", "SMALL", "MEDIUM", "LARGE", "XLARGE"))
      # qry <- paste(qry, sprintf(" WAREHOUSE_SIZE = %s", toupper(size)))
      sprintf(" WAREHOUSE_SIZE = %s", toupper(size))
    }
  type_param <- 
    if (!is.null(type)) {
      type <- match.arg(tolower(type), choices=c("standard", "enterprise"))
      # qry <- paste(qry, sprintf(" WAREHOUSE_TYPE = %s", toupper(type)))
      sprintf(" WAREHOUSE_TYPE = %s", toupper(type))
    }

  ## Add in the parameters, assuming at least one is not NULL
  params <- c(size_param, type_param)
  if (!is.null(params)) {
    qry <- paste(qry, "USING", paste(params, collapse=" "))
  }

  ## Add a semicolon and set the class to be nice and neat
  qry <- paste(qry, ";")
  setQry(qry)


  verboseMsg(verbose, "Provisioning warehouse '", name, "'", if (!is.null(params)) paste("  with params", params), sep="", minw=88)
  ret <- sfQry(qry, con=con, verbose=verbose)

  if (!isErr(ret))
    sfSetWarehouseDetails(name=name, size=size, type=type, time_started=now()-5)

  if (notify)
    notifyIfTimeConsuming(msg="", subj=sprintf("Warehouse %s is initizlized with size %s", name, valueIfNull(size, "default")), start_time=start_time, end_time=now(), minutes=1)
  
  return(invisible(ret))
}

if (FALSE)
  sfWarehouseOn(size="L", type="stan")

## TODO for warehouse provisioning
# , auto_suspend_minutes=15
# AUTO_SUSPEND
# Specifies the period of inactivity (in seconds) after which a warehouse will be automatically suspended. A NULL value specifies that the warehouse is never automatically suspended due to inactivity.
# AUTO_RESUME
# Specifies whether to automatically resume a warehouse when it is accessed. Valid values are ‘TRUE’ and ‘FALSE’.
# INITIALLY_SUSPENDED
# Specifies whether the warehouse is created initially in suspended state. Valid values are ‘TRUE’ or ‘FALSE’.



sfWarehouseOff <- function(name="LOAD_WH", con=sfGetCon(), verbose=TRUE) {
  qry <- sprintf("ALTER WAREHOUSE %s SUSPEND", name)
  sfQry(qry, con=con, verbose=verbose)
}



