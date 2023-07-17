
###################################
### SQL - Rosetta Stone         ###
###                             ###
### Translating SQL to R        ###
### Source: logfile sqltext.sql ###
### YKK - 17-07-2023            ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###

## 0. Basic Operations ----
# Define SQL connection
connection <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")

## 1. Get data ----
#  1.1. 'Ophalen iSR-tabellen' ----

#  1.1.1. ##tbl_SR_JPS_wtps ----
#! WORKS
temp_query <- paste0("
                      IF OBJECT_ID('tempdb..##tbl_SR_JPS_wtps') Is Not Null
                      DROP TABLE ##tbl_SR_JPS_wtps
                      
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
                      AS VolgordeJPS  INTO ##tbl_SR_JPS_wtps FROM tbl_SR_JPS AS t 
                     ")

tbl_SR_JPS_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_JPS_wtps")
tbl_SR_JPS_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_JPS_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM vw_SR_VolgordeJPS"))
temp_colnames <- c("JPS_Id", "Jaar", "Periode", "Status", "JPS", "JS", "Fase", "VolgordeJPS")
tbl_SR_JPS_wtps_R <- tbl_SR_JPS_wtps_R[, temp_colnames]


#  1.1.2. ##tbl_SR_SectorClassificatie_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_SectorClassificatie_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_SectorClassificatie_wtps
                     
                     SELECT  t.*, j.JS, LEFT(j.VolgordeJPS,7) + j.JS AS VJPS  INTO ##tbl_SR_SectorClassificatie_wtps FROM tbl_SR_SectorClassificatie AS t 
                     INNER JOIN ##tbl_SR_JPS_wtps AS j ON t.JPS_Id = j.JPS_Id 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_SectorClassificatie_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_SectorClassificatie_wtps")
tbl_SR_SectorClassificatie_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_SectorClassificatie_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_SectorClassificatie"))


#  1.1.3. ##tbl_SR_SectorClassificatieRelatie_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_SectorClassificatieRelatie_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_SectorClassificatieRelatie_wtps
                     SELECT  t.*  INTO ##tbl_SR_SectorClassificatieRelatie_wtps FROM tbl_SR_SectorClassificatieRelatie AS t 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_SectorClassificatieRelatie_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_SectorClassificatieRelatie_wtps")
tbl_SR_SectorClassificatieRelatie_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_SectorClassificatieRelatie_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_SectorClassificatieRelatie"))


#  1.1.4. ##tbl_SR_TransactieClassificatie_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_TransactieClassificatie_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_TransactieClassificatie_wtps
                     SELECT  t.*, j.JS, LEFT(j.VolgordeJPS,7) + j.JS AS VJPS  INTO ##tbl_SR_TransactieClassificatie_wtps FROM tbl_SR_TransactieClassificatie AS t 
                     INNER JOIN ##tbl_SR_JPS_wtps AS j ON t.JPS_Id = j.JPS_Id 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_TransactieClassificatie_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_TransactieClassificatie_wtps")
tbl_SR_TransactieClassificatie_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_TransactieClassificatie_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_TransactieClassificatie"))


#  1.1.5. ##tbl_SR_TransactieClassificatieRelatie_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_TransactieClassificatieRelatie_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_TransactieClassificatieRelatie_wtps
                     SELECT  t.*, c.Naam AS Omschrijving, c.Volgorde INTO ##tbl_SR_TransactieClassificatieRelatie_wtps FROM tbl_SR_TransactieClassificatieRelatie AS t 
                     INNER JOIN tbl_SR_TransactieClassificatie c ON t.JPS_id=c.JPS_Id AND t.TransactieSoort=c.TransactieSoort AND t.Transactie_Ouder=c.Transactie 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_TransactieClassificatieRelatie_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_TransactieClassificatieRelatie_wtps")
tbl_SR_TransactieClassificatieRelatie_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_TransactieClassificatieRelatie_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_TransactieClassificatieRelatie"))


#  1.1.6. ##tbl_SR_Rechten_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_Rechten_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_Rechten_wtps
                     SELECT  t.*  INTO ##tbl_SR_Rechten_wtps FROM tbl_SR_Rechten AS t 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_Rechten_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_Rechten_wtps")
tbl_SR_Rechten_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_Rechten_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_Rechten"))


#  1.1.7. ##tbl_SR_OnderdeelCodes_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_OnderdeelCodes_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_OnderdeelCodes_wtps
                     SELECT  t.*  INTO ##tbl_SR_OnderdeelCodes_wtps FROM tbl_SR_OnderdeelCodes AS t 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_OnderdeelCodes_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_OnderdeelCodes_wtps")
tbl_SR_OnderdeelCodes_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_OnderdeelCodes_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_OnderdeelCodes"))


#  1.1.8. ##tbl_SR_SaldiClassificatieRelatie_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_SaldiClassificatieRelatie_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_SaldiClassificatieRelatie_wtps
                     SELECT  t.*, c.Naam AS Omschrijving INTO ##tbl_SR_SaldiClassificatieRelatie_wtps FROM tbl_SR_SaldiClassificatieRelatie AS t 
                     INNER JOIN tbl_SR_TransactieClassificatie c ON t.JPS_id=c.JPS_Id AND t.Saldo=c.Transactie AND c.TransactieSoort = 'S'
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_SaldiClassificatieRelatie_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_SaldiClassificatieRelatie_wtps")
tbl_SR_SaldiClassificatieRelatie_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
# tbl_SR_SaldiClassificatieRelatie_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tbl_SR_SaldiClassificatieRelatie")) #! too large


#  1.1.9. ##tbl_SR_JPS_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tbl_SR_JPS_wtps') Is Not Null
                     DROP TABLE ##tbl_SR_JPS_wtps
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
                     AS VolgordeJPS  INTO ##tbl_SR_JPS_wtps FROM tbl_SR_JPS AS t 
                     WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                     ")

tbl_SR_JPS_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_JPS_wtps")
tbl_SR_JPS_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
tbl_SR_JPS_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM vw_SR_VolgordeJPS"))
temp_colnames <- c("JPS_Id", "Jaar", "Periode", "Status", "JPS", "JS", "Fase", "VolgordeJPS")
tbl_SR_JPS_wtps_R <- tbl_SR_JPS_wtps_R[, temp_colnames]


#  1.2. 'Ophalen rechten' ----

#  1.2.1. ##tmptbl_TransactieRecht_wtps ----
temp_query <- paste0("
                     IF OBJECT_ID('tempdb..##tmptbl_TransactieRecht_wtps') Is Not Null
                     DROP TABLE ##tmptbl_TransactieRecht_wtps
                     
                     IF EXISTS(SELECT TOP 1 * FROM ##tbl_SR_JPS_wtps
                     WHERE JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) AND Fase<>'Afgesloten')
                    
                     BEGIN
                     DELETE ##tbl_SR_Rechten_wtps
                     WHERE JPS_Id In (SELECT JPS_Id FROM ##tbl_SR_JPS_wtps
                     WHERE JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) AND Fase='Afgesloten') 
                     END
                     
                     SELECT Transactie, Cast('' AS VARCHAR(500)) As Pids INTO ##tmptbl_TransactieRecht_wtps
                     FROM ##tbl_SR_Rechten_wtps
                     WHERE Transactie Is Not Null 
                     GROUP BY Transactie ORDER BY Transactie
                     
                     DECLARE @strJaar nvarchar(4)
                     DECLARE @strTransactie nvarchar(150)
                     DECLARE @strPid nvarchar(150)
                     DECLARE curTransacties CURSOR LOCAL FORWARD_ONLY READ_ONLY
                     FOR SELECT Transactie, Max(Jaar) AS Jaar, Pid FROM ##tbl_SR_Rechten_wtps
                     WHERE Transactie Is Not Null 
                     GROUP BY Transactie, Pid ORDER BY Transactie, Max(Jaar) DESC, Pid
                     OPEN curTransacties
                     FETCH NEXT FROM curTransacties INTO @strTransactie, @strJaar, @strPid
                     WHILE (@@FETCH_STATUS <> -1)
                     
                     BEGIN
                     UPDATE ##tmptbl_TransactieRecht_wtps
                     SET Pids = Pids + @strPid + ';' 
                     WHERE Transactie = @strTransactie AND Pids Not Like '%' + @strPid + '%' 
                     FETCH NEXT FROM curTransacties INTO @strTransactie, @strJaar, @strPid
                     END
                     Close curTransacties
                     DEALLOCATE curTransacties
                     ")

tmptbl_TransactieRecht_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tmptbl_TransactieRecht_wtps")
tmptbl_TransactieRecht_wtps <- dbGetQuery(connection, temp_query)

# remake in R via SQL view
# tmptbl_TransactieRecht_wtps_R <- dbGetQuery(connection, paste0("SELECT * FROM tmptbl_TransactieRecht_wtps")) # Invalid object name 'tmptbl_TransactieRecht_wtps'.


#  1.3. 'Naamloze bewerkingen' ----
temp_query <- paste0("
                     SELECT pvt.Transactie, pvt.Naam, pvt.Volgorde, 
                     CASE WHEN IsNull([0],'')>='0' THEN [0] WHEN IsNull([1],'')>='0' THEN [1] WHEN IsNull([2],'')>='0' THEN [2] END AS [Niveau_2023-Z], 
                     CASE WHEN IsNull([0],'9')<'0' THEN '0 -' + [0] WHEN IsNull([1],'9')<'0' THEN '1 -' + [1] WHEN IsNull([2],'9')<'0' THEN '2 -' + [2] END AS AfwijkendNiveau, p.Pids 
                     FROM ( 
                           SELECT Volgorde, Transactie, Naam, Niveau, CAST(Niveau AS varchar(20)) AS VJPS 
                           FROM ##tbl_SR_TransactieClassificatie_wtps
                           WHERE JPS_ID = 1955
                           UNION 
                           SELECT n.Volgorde, n.Transactie, n.Naam, n.Niveau, ' ' + SUBSTRING(Max(n.VJPS),8,20) AS VJPS 
                           FROM ##tbl_SR_TransactieClassificatie_wtps AS n 
                           LEFT JOIN (SELECT * FROM ##tbl_SR_TransactieClassificatie_wtps WHERE JPS_ID = 1955) AS j ON n.Transactie = j.Transactie AND n.Niveau = j.Niveau 
                           WHERE j.Transactie Is Null 
                           GROUP BY n.Volgorde, n.Transactie, n.Naam, n.Niveau 
                          ) AS t 
                      PIVOT 
                      (Max(VJPS) 
                      FOR Niveau In ([0],[1],[2]) 
                      ) AS pvt
                      LEFT JOIN ##tmptbl_TransactieRecht_wtps p ON pvt.Transactie = p.Transactie 
                      ORDER BY pvt.Volgorde
                     ")

temp_object_1 <- dbGetQuery(connection, temp_query)


temp_query <- paste0("
                     SELECT pvt.Sector, pvt.Naam, 
                     CASE WHEN IsNull([0],'')>='0' THEN [0] WHEN IsNull([1],'')>='0' THEN [1] WHEN IsNull([2],'')>='0' THEN [2] END AS [Niveau_2023-Z], 
                     IsNull(CASE WHEN IsNull([0],'9')<'0' THEN 'LR 0 -' + [0] WHEN IsNull([1],'9')<'0' THEN 'LR 1 -' + [1] WHEN IsNull([2],'9')<'0' THEN 'LR 2 -' + [2] END,'') + ' ' 
                     + IsNull(CASE WHEN IsNull([100],'9')<'0' THEN 'FR 0 -' + [100] WHEN IsNull([101],'9')<'0' THEN 'FR 1 -' + [101] WHEN IsNull([102],'9')<'0' THEN 'FR 2 -' + [102] END ,'') AS AfwijkendNiveau 
                     FROM ( 
                           SELECT Sector, Naam, CAST(Niveau AS varchar(20)) AS VJPS, CASE WHEN Rekening = 'LT' THEN Niveau ELSE 100 + Niveau END AS Niveau 
                           FROM ##tbl_SR_SectorClassificatie_wtps
                           WHERE JPS_ID = 1955
                           UNION 
                           SELECT n.Sector, n.Naam, ' ' + SUBSTRING(Max(n.VJPS),8,20) AS VJPS, CASE WHEN n.Rekening = 'LT' THEN n.Niveau ELSE 100 + n.Niveau END AS Niveau
                           FROM ##tbl_SR_SectorClassificatie_wtps AS n 
                           LEFT JOIN (SELECT * FROM ##tbl_SR_SectorClassificatie_wtps WHERE JPS_ID = 1955) AS j ON n.Sector = j.Sector AND n.Niveau = j.Niveau 
                                      WHERE j.Sector Is Null 
                                      GROUP BY n.Sector, n.Naam, n.Rekening, n.Niveau 
                          ) AS t 
                        PIVOT 
                        (Max(VJPS) 
                            FOR Niveau In ([0],[1],[2],[100],[101],[102]) 
                        ) AS pvt
                        ORDER BY pvt.Sector
                        
                        SET NOCOUNT ON 
                        IF OBJECT_ID('tempdb..##tbl_SR_Data_Transacties_wtps') Is Not Null
                        DROP TABLE ##tbl_SR_Data_Transacties_wtps
                        CREATE TABLE ##tbl_SR_Data_Transacties_wtps ( 
                        [VolgordeJPS] [varchar](15) NOT NULL,
                        [JPS] [varchar](15) NOT NULL,
                        [JPS_Id] [int] NOT NULL,
                        [Jaar] [varchar](4) NOT NULL,
                        [Periode] [varchar](2) NOT NULL, 
                        [Status] [varchar](5) NOT NULL, 
                        [Fase] [nvarchar](255) NULL, 
                        [Rekening] [varchar](15) NOT NULL, 
                        [Sector] [varchar](15) NOT NULL, 
                        [TegenSector] [varchar](15) NULL, 
                        [Transactie] [varchar](15) NOT NULL, 
                        [TransactieSoort] [varchar](15) NOT NULL, 
                        [Onderdeel] [varchar](15) NOT NULL, 
                        [Waarde] [int] NOT NULL, 
                        [Waarde_Type] [varchar](15) NOT NULL, 
                        [Definitief] [int] NOT NULL) 
                     ")

temp_object_2 <- dbGetQuery(connection, temp_query)


#  1.4. 'Ophalen data' ----

#  1.4.1. ##tbl_SR_Data_Transacties_wtps ----
temp_query <- paste0("
                     INSERT INTO ##tbl_SR_Data_Transacties_wtps (VolgordeJPS, JPS, JPS_Id, Jaar, Periode, Status, 
                     Fase, Rekening, Sector, TegenSector, Transactie, TransactieSoort, Onderdeel, Waarde, Waarde_Type, Definitief) 
                     SELECT j.VolgordeJPS, j.JPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, d.Rekening, d.Sector, d.TegenSector, d.Transactie, d.TransactieSoort, Onderdeel, Sum(d.Waarde) AS Waarde, Waarde_Type, Definitief 
                     FROM tbl_SR_Data_Transacties d 
                     INNER JOIN ##tbl_SR_JPS_wtps AS j ON d.JPS_Id = j.JPS_Id 
                     WHERE d.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) AND d.Definitief = 1
                     GROUP BY j.VolgordeJPS, j.JPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, Rekening, Sector, TegenSector, Transactie, TransactieSoort, Onderdeel, Waarde_Type, Definitief
                     HAVING Sum(d.Waarde) <> 0
                     ")

tbl_SR_Data_Transacties_wtps <- dbGetQuery(connection, temp_query)

temp_query <- paste0("SELECT * FROM ##tbl_SR_Data_Transacties_wtps")
tbl_SR_Data_Transacties_wtps <- dbGetQuery(connection, temp_query)




