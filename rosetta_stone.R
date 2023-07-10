
###################################
### SQL - Rosetta Stone         ###
###                             ###
### Translating SQL to R        ###
### Source: logfile sqltext.sql ###
### YKK - 07-07-2023            ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###


## 0. Basic Operations ----

# Define SQL connection
connection <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")


## 1. Get data ----

# 1.1. 'Ophalen iSR-tabellen' ----

# tbl_SR_JPS_wtps
query <- paste0("
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
                ",
                #AS VolgordeJPS  INTO ##tbl_SR_JPS_wtps FROM tbl_SR_JPS AS t #commented out because this alters the actual online SQL dataset
                "
                FROM tbl_SR_JPS AS t 
                ")

tbl_SR_JPS_wtps <- dbGetQuery(connection, query)


# tbl_SR_SectorClassificatie_wtps
query <- paste0("
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_SectorClassificatie_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_SectorClassificatie_wtps

                SELECT  t.*, j.JS, LEFT(j.VolgordeJPS,7) + j.JS AS VJPS  INTO ##tbl_SR_SectorClassificatie_wtps FROM tbl_SR_SectorClassificatie AS t 
                INNER JOIN ##tbl_SR_JPS_wtps AS j ON t.JPS_Id = j.JPS_Id 
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_SectorClassificatie_wtps
                ")

tbl_SR_SectorClassificatie_wtps <- dbGetQuery(connection, query)

