## rsnowflake
R functions for the snowflake package

`Snowflake` is a relatively new database system.
There is as of yet no `R` package for it.

These are a collection of functions wrapping RODBC functionality.

This is very much a work in progress.

## Basic usage

*Clone the repo*  
git clone https://github.com/rsaporta/rsnowflake

*Make sure the snowflake driver is setup*  
\<Follow instructions provided by snowflake\>


## in R

    ### Make sure data.table and magrittr are installed
    install.packages("data.table")
    install.packages("magrittr")

    ### Source the files
    files <- c("helper_functions.r", "paste_functions.r", "snowflake_options.r", "snowflake_utils.r", "verboseQry.r")
    files <- paste0("/path/to/files/", files)
    invisible(lapply(f2, source))

    ### Setup snowflake options
    setSnowflakeOptions(uid = <username>, pwd = <password>)

### EXAMPLES

    ## See which warehouses are available
    sfShowWarehouses()
    ## Turn on a warehouse
    sfWarehouseOn(size="SMALL")

    ## Show the tables
    sfShowTables(schema="PUBLIC")

    ## Run a query
    ## (The connection will be established automatically)
    qry <- "SELECT count(*) FROM PUBLIC.myTable WHERE customerID in ('ABCD', 'DDFDER', 'DFDFD')"
    sfQry(qry)

    ## Turn off a warehouse
    sfWarehouseOff()
