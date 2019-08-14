#########################################################################################################################
# This function is to connect EDW through library "RODBC"
# 20190812 - Larry Wang 
# Property of The University of Tasmania
#########################################################################################################################

# Load library
library("RODBC")


# Input userid and password to set up connection.
myconn <-odbcConnect("EDWDEV_64BIT", 
                     uid=rstudioapi::askForPassword("Database user"), 
                     pwd=rstudioapi::askForPassword("Database password"))


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

