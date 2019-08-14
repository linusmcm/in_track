#########################################################################################################################
# This function is to connect EDW through library "RODBC"
# 20190812 - Larry Wang 
# Property of The University of Tasmania
#########################################################################################################################

# Load library
library(RODBC)
library(RMySQL)
library(shiny)
library(tidyverse)
library(dbplyr)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
library(RODBC)
library(DBI)
library(pool)

options(mysql = list(
  "host" = "remotemysql.com",
  "port" = 3306,
  "user" = "g6Mj2lugZA",
  "password" = "ipYc79lTd0"
))

con <- dbConnect(odbc::odbc()
                 , Driver = "MySQL ODBC 8.0 Unicode Driver"
                 , Server = options()$mysql$host
                 , Database = options()$mysql$user 
                 , uid = options()$mysql$user
                 , pwd = options()$mysql$password)

dbDisconnect(con)
if (requireNamespace("RMySQL", quietly = TRUE)) 
{
  pool <- pool::dbPool(
    drv = RMySQL::MySQL(),
    dbname = options()$mysql$user,
    host = options()$mysql$host,
    username = options()$mysql$user,
    password = options()$mysql$password
  )
}

con <- DBI::dbConnect(odbc::odbc()
                      , Driver = "Oracle in OraClient12Home2"
                      , Host   = "exa2-scan.its.utas.edu.au"
                      , SVC    = "edwdev_maa"
                      , UID    = rstudioapi::askForPassword("Database user")
                      , PWD    = rstudioapi::askForPassword("Database password")
                      , Port   = 1521)




# Input userid and password to set up connection.
myconn <-odbcConnect(options()$mysql$host
                     , uid = options()$mysql$user
                     , pwd = options()$mysql$password
                     , case="tolower")


# Retrieve and list all the tables 
tblist <- sqlTables(myconn)
# View(tblist)


# Retrieve and view one table to test
tb_UTAS_MODEL <- sqlFetch(myconn,"DATA_MART.STG_INITIATIVE_TRACKING")
View(tb_UTAS_MODEL)


# Test to insert table
sqlSave(myconn, 
        tb_UTAS_MODEL,
        tablename = "DATA_MART.STG_INITIATIVE_TRACKING", 
        append = TRUE, 
        rownames = FALSE)


# Test to retrieve column owner from target table
owner <- sqlQuery(myconn, "select OWNER from DATA_MART.STG_INITIATIVE_TRACKING")

# Test to delete row from target table
sqlQuery(myconn, "DELETE FROM DATA_MART.STG_INITIATIVE_TRACKING WHERE STRATEGY = 5")

# Close connection
odbcClose(myconn)

