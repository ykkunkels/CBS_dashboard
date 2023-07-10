
############################

## SMALL


if(!require('odbc')){install.packages('odbc', dep = TRUE)};library('odbc')

# Verbinding
verbinding_iSR <- odbcDriverConnect("driver={SQL SERVER};server=SQL_HSR_ANA_PRD\\i01,50001;database=HSR_ANA_PRD;trusted_connection=true")

verbinding_iSR <- dbConnect(odbc(),
                            Driver = "SQL SERVER",
                            Server = "SQL_HSR_ANA_PRD\\i01,50001",
                            Database = "HSR_ANA_PRD")


jaar_transacties <- "2021"
rekening_EB <- "EB"

# Query met select van de data, where kan ook anders/ minder zijn. Aanrader is hier zeker te selecteren. Alles is namelijk veel.
query_small <- paste0("SELECT Jaar, Periode, Status
                             FROM 	tbl_SR_Data_Transacties  
                             WHERE 	Jaar >= ('",jaar_transacties,"') AND Rekening = ('",rekening_EB,"')
                             GROUP BY Jaar, Periode, Status")

start_time <- Sys.time()
# data_transacties <- sqlQuery(verbinding_iSR, query_small)
data_transacties_odbc <- dbGetQuery(verbinding_iSR, query_small)

end_time <- Sys.time()

end_time - start_time




## Benchmark RODBC vs. ODBC query time

# intialise generic vars
jaar_transacties <- "2021"
rekening_EB <- "EB"
query_broad <- paste0("SELECT * FROM 	tbl_SR_Data_Transacties  
                       WHERE 	Jaar >= ('",jaar_transacties,"')")

# RODBC
if(!require('RODBC')){install.packages('RODBC', dep = TRUE)};library('RODBC')
verbinding_iSR <- odbcDriverConnect("driver={SQL SERVER};server=SQL_HSR_ANA_PRD\\i01,50001;database=HSR_ANA_PRD;trusted_connection=true")
start_time_RODBC <- Sys.time()
data_transacties <- sqlQuery(verbinding_iSR, query_broad)
end_time_RODBC <- Sys.time()
end_time_RODBC - start_time_RODBC


# ODBC
if(!require('odbc')){install.packages('odbc', dep = TRUE)};library('odbc')
verbinding_iSR <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")
start_time_ODBC <- Sys.time()
data_transacties_odbc <- dbGetQuery(verbinding_iSR, query_broad)
end_time_ODBC <- Sys.time()
end_time_ODBC - start_time_ODBC