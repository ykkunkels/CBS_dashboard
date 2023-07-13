
###################################
### SQL - Rosetta Stone         ###
###                             ###
### Translating SQL to R        ###
### Source: logfile sqltext.sql ###
### YKK - 13-07-2023            ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###

## 0. Basic Operations ----
# Define SQL connection
connection <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")

## 1. Get data ----
#  1.1. 'Ophalen iSR-tabellen'

#  1.1.1. tbl_SR_JPS_wtps ----
temp_query <- paste0("
                      SELECT  JPS_Id, Jaar, Periode, Status, Jaar+'-'+Periode+'-'+Status AS JPS, 
                      Jaar+'-'+Status AS JS, Fase, 
                      
                      CAST(CAST(Jaar AS varchar(4)) + '-' + 
                      CASE LEFT(Status,1)
                      WHEN 'P' THEN '50'
                      WHEN 'R' THEN '60' + CASE WHEN ISNULL(Replace(Status,'R',''),'')='' THEN '' ELSE '_' + Status END
                      WHEN 'D' THEN '70'
                      WHEN 'V' THEN CAST(40-ISNULL(Replace(Status,'V',''),0) AS varchar(2))
                      WHEN 'Z' THEN CAST(20-ISNULL(Replace(Status,'Z',''),0) AS varchar(2))
                      ELSE '00' END + CAST(REPLACE(Periode,'Y','5') AS varchar(1)) AS varchar(15))
                      AS VolgordeJPS  
                      ",
                     #INTO ##tbl_SR_JPS_wtps FROM tbl_SR_JPS AS t #commented out because this might alter(?) the actual online SQL dataset
                     "
                      FROM tbl_SR_JPS AS t 
                     ")

tbl_SR_JPS_wtps <- dbGetQuery(connection, temp_query)

# remake in R
#! "VolgordeJPS" manueel repliceren is niet triviaal
# tbl_SR_JPS_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_JPS"))
# temp_colnames <- c("JPS_Id", "Jaar", "Periode", "Status", "Fase")
# tbl_SR_JPS_wtps_R <- tbl_SR_JPS_wtps_R[, temp_colnames]
# tbl_SR_JPS_wtps_R$JPS <- with(tbl_SR_JPS_wtps_R, paste0(Jaar, "-", Periode, "-", Status))
# tbl_SR_JPS_wtps_R$JS <- with(tbl_SR_JPS_wtps_R, paste0(Jaar, "-", Status))
# tbl_SR_JPS_wtps_R <- tbl_SR_JPS_wtps_R[, c(1:4, 6, 7, 5)]
# 
# tbl_SR_JPS_wtps_R$VolgordeJPS <- with(tbl_SR_JPS_wtps_R, paste0(Jaar, "-", Status))
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("Z3", 17, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("Z2", 18, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("Z1", 19, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("Z", 20, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("V", 40, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("P", 50, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- tbl_SR_JPS_wtps_R[tbl_SR_JPS_wtps_R$Status == "R", ]
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("D", 70, tbl_SR_JPS_wtps_R$VolgordeJPS)
# tbl_SR_JPS_wtps_R$VolgordeJPS <- gsub("N", "00", tbl_SR_JPS_wtps_R$VolgordeJPS)
# 
# temp_data <- tbl_SR_JPS_wtps_R[grep("R", tbl_SR_JPS_wtps_R$VolgordeJPS), "VolgordeJPS"]
# temp_data <- paste0(substr(temp_data, 1, 5), "60_", substr(temp_data, 6, nchar(temp_data[1])))
# tbl_SR_JPS_wtps_R[grep("R", tbl_SR_JPS_wtps_R$VolgordeJPS), "VolgordeJPS"] <- temp_data
# 
# tbl_SR_JPS_wtps_R$VolgordeJPS <- paste0(tbl_SR_JPS_wtps_R$VolgordeJPS, with(tbl_SR_JPS_wtps_R, paste0(gsub("Y", 5, Periode))))
# 
# tbl_SR_JPS_wtps_R[!(tbl_SR_JPS_wtps$VolgordeJPS == tbl_SR_JPS_wtps_R$VolgordeJPS), ]
# table(tbl_SR_JPS_wtps$VolgordeJPS == tbl_SR_JPS_wtps_R$VolgordeJPS)

#! repliceer "VolgordeJPS" via SQL view
tbl_SR_JPS_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM vw_SR_VolgordeJPS"))
temp_colnames <- c("JPS_Id", "Jaar", "Periode", "Status", "JPS", "JS", "Fase", "VolgordeJPS")
tbl_SR_JPS_wtps_R <- tbl_SR_JPS_wtps_R[, temp_colnames]


# tbl_SR_SectorClassificatie_wtps
temp_query <- paste0("
                      SELECT  t.*, j.JS, LEFT(j.VolgordeJPS,7) + j.JS AS VJPS  INTO ##tbl_SR_SectorClassificatie_wtps FROM tbl_SR_SectorClassificatie AS t 
                      INNER JOIN ##tbl_SR_JPS_wtps AS j ON t.JPS_Id = j.JPS_Id 
                      WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                      FROM ##tbl_SR_SectorClassificatie_wtps
                     ")

tbl_SR_SectorClassificatie_wtps <- dbGetQuery(connection, temp_query)

# Remake in R
tbl_SR_SectorClassificatie <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_SectorClassificatie"))



