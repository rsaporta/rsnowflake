## SET THESE OPTIONS AS APPROPRIATE

setSnowflakeOptions <- function(uid=NULL, pwd=NULL, wh="LOAD_WH", drv="SnowflakeDSII", inuse=TRUE) {

  ## Username
  options("snowflake_uid" = uid)
  
  ## Password
  options("snowflake_pwd" = pwd)

  ## Default warehouse
  options("snowflake_defaultwh" = wh)

  ## Default Driver -- make sure to set it up properly outside of R
  options("snowflake_driver" = drv)

  ## Flag to indicate whether snowflake is currently in use
  ## Used in some wrappers that are themselves useful for other DBs, eg postgres
  options("snowflake_inuse" = inuse)
}

showSnowflakeOptions <- function() {
  uid <-  getOption("snowflake_uid", default="NULL")
  pwd <-  getOption("snowflake_pwd", default="NULL")
  wh  <-  getOption("snowflake_defaultwh", default="NULL")
  drv <-  getOption("snowflake_driver", default="NULL")
  inuse <-  getOption("snowflake_inuse", default="NULL")

  fmt <- "%20s  =  %-15s"
  cat(sprintf(fmt, "USERNAME", uid)
    , sprintf(fmt, "PASSWORD", if (pwd == "NULL") pwd else "********")
    , sprintf(fmt, "DEFAULT WAREHOUSE", wh)
    , sprintf(fmt, "DRIVER NAME", drv)
    , sprintf(fmt, "SNOWFLAKE IN USE", as.character(inuse))
    , sep = "\n")
  return(invisible(NULL))
}