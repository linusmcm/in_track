# Load library
library(RJDBC)
#install.packages("RJDBC",dep=TRUE)


jdbcDriver = JDBC("oracle.jdbc.OracleDriver",classPath=paste0(getwd(),"/OJDBC/ojdbc6.jar"))
myconn  <- dbConnect(jdbcDriver
               , "jdbc:oracle:thin:@//exa2-scan.its.utas.edu.au:1521/edwdev_maa"
               , rstudioapi::askForPassword("Database user")
               , rstudioapi::askForPassword("Database password"))


# Retrieve and list all the tables 
tblist <- dbGetTables(myconn)
View(tblist)


# Retrieve and view one table to test
tb_UTAS_MODEL <- dbReadTable(myconn,"DATA_MART.STG_INITIATIVE_TRACKING")
View(tb_UTAS_MODEL)


# Test to insert table
# dbWriteTable(myconn, 
#         tb_UTAS_MODEL,
#         tablename = "DATA_MART.STG_INITIATIVE_TRACKING", 
#         append = TRUE, 
#         rownames = FALSE)


# Test to retrieve column owner from target table
owner <- dbGetQuery(myconn, "select OWNER from DATA_MART.STG_INITIATIVE_TRACKING")

# Test to delete row from target table
dbGetQuery(myconn, "DELETE FROM DATA_MART.STG_INITIATIVE_TRACKING WHERE STRATEGY = 5")

# Close connection
dbDisconnect(myconn)

