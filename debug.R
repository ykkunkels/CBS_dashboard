
#### Debug


#! Get JPS info from SQL server
tbl_SR_JPSReferentie <- dbGetQuery(dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD"), 
                                   paste0("SELECT * FROM tbl_SR_JPSReferentie"))
