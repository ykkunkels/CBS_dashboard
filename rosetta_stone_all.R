

# Define SQL connection
connection <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", Database = "HSR_ANA_PRD")

# Query all
query <- paste0("
                SET NOCOUNT ON
                print 'log4net'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                SELECT 'SynchronisatieFout '+CAST(Logging_id as varchar), Exception, Message FROM tbl_SR_Log4net WHERE Exception <> '' AND Logging_id BETWEEN 
                (SELECT Max(Logging_id) FROM tbl_SR_Log4net WHERE UspNaam='usp_SR_Synchronize_ANA' AND Message='Start van de stored procedure') 
                 AND (SELECT Max(Logging_id) FROM tbl_SR_Log4net WHERE UspNaam='usp_SR_Synchronize_ANA' AND Message='Einde van de stored procedure') 
                
                SELECT LogStartTijd, CASE WHEN UspNaam='usp_SR_Synchronize_ANA' AND Message='Einde van de stored procedure' THEN 'Klaar' ElSE 'Bezig' END AS Bezig FROM tbl_SR_Log4net AS L 
                INNER JOIN (SELECT MAX(Logging_id) AS LId FROM tbl_SR_Log4net ) AS m ON L.Logging_id = m.LId
                
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde log4net'
                go
                
                print 'Ophalen iSR-tabellen'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
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
                
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_JPS_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde Ophalen iSR-tabellen'
                go
                
                print 'Ophalen iSR-tabellen'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                
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
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_SectorClassificatieRelatie_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_SectorClassificatieRelatie_wtps
                SELECT  t.*  INTO ##tbl_SR_SectorClassificatieRelatie_wtps FROM tbl_SR_SectorClassificatieRelatie AS t 
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_SectorClassificatieRelatie_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_TransactieClassificatie_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_TransactieClassificatie_wtps
                SELECT  t.*, j.JS, LEFT(j.VolgordeJPS,7) + j.JS AS VJPS  INTO ##tbl_SR_TransactieClassificatie_wtps FROM tbl_SR_TransactieClassificatie AS t 
                INNER JOIN ##tbl_SR_JPS_wtps AS j ON t.JPS_Id = j.JPS_Id 
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_TransactieClassificatie_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_TransactieClassificatieRelatie_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_TransactieClassificatieRelatie_wtps
                SELECT  t.*, c.Naam AS Omschrijving, c.Volgorde INTO ##tbl_SR_TransactieClassificatieRelatie_wtps FROM tbl_SR_TransactieClassificatieRelatie AS t 
                INNER JOIN tbl_SR_TransactieClassificatie c ON t.JPS_id=c.JPS_Id AND t.TransactieSoort=c.TransactieSoort AND t.Transactie_Ouder=c.Transactie 
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_TransactieClassificatieRelatie_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_Rechten_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_Rechten_wtps
                SELECT  t.*  INTO ##tbl_SR_Rechten_wtps FROM tbl_SR_Rechten AS t 
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_Rechten_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_OnderdeelCodes_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_OnderdeelCodes_wtps
                SELECT  t.*  INTO ##tbl_SR_OnderdeelCodes_wtps FROM tbl_SR_OnderdeelCodes AS t 
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_OnderdeelCodes_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
                IF OBJECT_ID('tempdb..##tbl_SR_SaldiClassificatieRelatie_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_SaldiClassificatieRelatie_wtps
                SELECT  t.*, c.Naam AS Omschrijving INTO ##tbl_SR_SaldiClassificatieRelatie_wtps FROM tbl_SR_SaldiClassificatieRelatie AS t 
                INNER JOIN tbl_SR_TransactieClassificatie c ON t.JPS_id=c.JPS_Id AND t.Saldo=c.Transactie AND c.TransactieSoort = 'S'
                WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_SaldiClassificatieRelatie_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                SET NOCOUNT ON 
                DECLARE @notempty as int 
                SET @notempty = 0 
                WHILE @notempty = 0 
                BEGIN 
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
                SELECT @notempty = COUNT(*) FROM ##tbl_SR_JPS_wtps
                END
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                go
                
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde Ophalen iSR-tabellen'
                go
                
                print 'ophalen rechten'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                SET NOCOUNT ON 
                IF OBJECT_ID('tempdb..##tmptbl_TransactieRecht_wtps') Is Not Null
                   DROP TABLE ##tmptbl_TransactieRecht_wtps
                
                go
                IF EXISTS(SELECT TOP 1 * FROM ##tbl_SR_JPS_wtps
                WHERE JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) AND Fase<>'Afgesloten')
                BEGIN
                   DELETE ##tbl_SR_Rechten_wtps
                   WHERE JPS_Id In (SELECT JPS_Id FROM ##tbl_SR_JPS_wtps
                                   WHERE JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) AND Fase='Afgesloten') 
                END
                
                go
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
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen rechten'
                go
                
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
                   LEFT JOIN (SELECT * FROM ##tbl_SR_TransactieClassificatie_wtps WHERE JPS_ID = 1955       ) AS j ON n.Transactie = j.Transactie AND n.Niveau = j.Niveau 
                   WHERE j.Transactie Is Null 
                   GROUP BY n.Volgorde, n.Transactie, n.Naam, n.Niveau 
                ) AS t 
                PIVOT 
                (Max(VJPS) 
                FOR Niveau In ([0],[1],[2]) 
                ) AS pvt
                LEFT JOIN ##tmptbl_TransactieRecht_wtps p ON pvt.Transactie = p.Transactie 
                ORDER BY pvt.Volgorde
                go
                SELECT pvt.Sector, pvt.Naam, 
                CASE WHEN IsNull([0],'')>='0' THEN [0] WHEN IsNull([1],'')>='0' THEN [1] WHEN IsNull([2],'')>='0' THEN [2] END AS [Niveau_2023-Z], 
                IsNull(CASE WHEN IsNull([0],'9')<'0' THEN 'LR 0 -' + [0] WHEN IsNull([1],'9')<'0' THEN 'LR 1 -' + [1] WHEN IsNull([2],'9')<'0' THEN 'LR 2 -' + [2] END,'') + ' ' 
                + IsNull(CASE WHEN IsNull([100],'9')<'0' THEN 'FR 0 -' + [100] WHEN IsNull([101],'9')<'0' THEN 'FR 1 -' + [101] WHEN IsNull([102],'9')<'0' THEN 'FR 2 -' + [102] END ,'') AS AfwijkendNiveau 
                FROM ( 
                   SELECT Sector, Naam, CAST(Niveau AS varchar(20)) AS VJPS,     CASE WHEN Rekening = 'LT' THEN Niveau ELSE 100 + Niveau END AS Niveau 
                   FROM ##tbl_SR_SectorClassificatie_wtps
                   WHERE JPS_ID = 1955
                   UNION 
                   SELECT n.Sector, n.Naam, ' ' + SUBSTRING(Max(n.VJPS),8,20) AS VJPS,     CASE WHEN n.Rekening = 'LT' THEN n.Niveau ELSE 100 + n.Niveau END AS Niveau
                   FROM ##tbl_SR_SectorClassificatie_wtps AS n 
                   LEFT JOIN (SELECT * FROM ##tbl_SR_SectorClassificatie_wtps WHERE JPS_ID = 1955   ) AS j ON n.Sector = j.Sector AND n.Niveau = j.Niveau 
                   WHERE j.Sector Is Null 
                   GROUP BY n.Sector, n.Naam, n.Rekening, n.Niveau 
                ) AS t 
                PIVOT 
                (Max(VJPS) 
                FOR Niveau In ([0],[1],[2],[100],[101],[102]) 
                ) AS pvt
                ORDER BY pvt.Sector
                go
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
                go
                print 'Ophalen data'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:36:07
                INSERT INTO ##tbl_SR_Data_Transacties_wtps (VolgordeJPS, JPS, JPS_Id, Jaar, Periode, Status, 
                Fase, Rekening, Sector, TegenSector, Transactie, TransactieSoort, Onderdeel, Waarde, Waarde_Type, Definitief) 
                SELECT j.VolgordeJPS, j.JPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, d.Rekening, d.Sector, d.TegenSector, d.Transactie, d.TransactieSoort, Onderdeel, Sum(d.Waarde) AS Waarde, Waarde_Type, Definitief 
                FROM tbl_SR_Data_Transacties d 
                INNER JOIN ##tbl_SR_JPS_wtps AS j ON d.JPS_Id = j.JPS_Id 
                WHERE d.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) AND d.Definitief = 1
                GROUP BY j.VolgordeJPS, j.JPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, Rekening, Sector, TegenSector, Transactie, TransactieSoort, Onderdeel, Waarde_Type, Definitief
                HAVING Sum(d.Waarde) <> 0
                -- 16:36:24
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen data'
                go
                
                SET NOCOUNT ON 
                IF OBJECT_ID('tempdb..##tmptbl_TransactiesTotaal_Deel1_wtps') Is Not Null
                   DROP TABLE ##tmptbl_TransactiesTotaal_Deel1_wtps
                IF OBJECT_ID('tempdb..##tmptbl_TransactiesTotaal_wtps') Is Not Null
                   DROP TABLE ##tmptbl_TransactiesTotaal_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Transacties_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Transacties_wtps
                
                go
                print 'doortellen sectoren'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:36:25
                SELECT D.JPS, D.VolgordeJPS, D.JPS_Id, D.Jaar, D.Periode, D.Status, D.Fase, D.Rekening, S.Sector_Ouder AS Sector,       
                TS.Sector_Ouder AS TegenSector, D.Transactie, D.TransactieSoort, D.Onderdeel, Sum(D.Waarde) AS Waarde,       
                D.Waarde_Type, D.Definitief, S.Tellen_Ouder AS NiveauSector, TS.Tellen_Ouder AS NiveauTegenSector 
                INTO ##tmptbl_TransactiesTotaal_Deel1_wtps
                 FROM ##tbl_SR_Data_Transacties_wtps AS D 
                INNER JOIN ##tbl_SR_SectorClassificatieRelatie_wtps AS S ON D.JPS_Id = S.JPS_Id AND D.Rekening = S.Rekening AND D.Sector = S.Sector_Kind 
                LEFT JOIN ##tbl_SR_SectorClassificatieRelatie_wtps AS TS ON D.JPS_Id = TS.JPS_Id AND D.Rekening = TS.Rekening AND D.TegenSector = TS.Sector_Kind 
                WHERE S.Sector_Ouder <> 'S' AND (TS.Sector_Ouder <> 'S' OR TS.Sector_Ouder Is Null)
                GROUP BY D.JPS, D.VolgordeJPS, D.JPS_Id, D.Jaar, D.Periode, D.Status, D.Fase, D.Rekening, S.Sector_Ouder, TS.Sector_Ouder, D.Transactie,       
                D.TransactieSoort, D.Onderdeel, D.Waarde_Type, D.Definitief, S.Tellen_Ouder, TS.Tellen_Ouder 
                -- 16:37:51
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde doortellen sectoren'
                go
                
                print 'toevoegen pids'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:37:51
                SELECT dt.*, t.Niveau AS NiveauTransactie, p.Pids, t.Volgorde, t.Naam 
                 INTO ##tmptbl_TransactiesTotaal_wtps FROM ##tmptbl_TransactiesTotaal_Deel1_wtps AS dt 
                INNER JOIN ##tbl_SR_TransactieClassificatie_wtps AS t ON dt.Transactie = t.Transactie AND dt.TransactieSoort = t.TransactieSoort AND dt.JPS_Id = t.JPS_Id 
                LEFT JOIN ##tmptbl_TransactieRecht_wtps p ON dt.Transactie = p.Transactie 
                WHERE Waarde <> 0
                -- 16:38:01
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde toevoegen pids'
                go
                
                print 'ophalen processtap'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:01
                SELECT d.JPS, d.VolgordeJPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, d.Rekening, d.Sector, d.TegenSector, d.Transactie, d.TransactieSoort, 
                d.Onderdeel AS Onderdeel, Sum(d.Waarde) as Waarde, d.Waarde_Type, d.Definitief, SUM(CASE WHEN d.Transactiesoort In ('P','B') THEN d.Waarde*-1 ELSE d.Waarde END) AS WaardeSaldo 
                FROM ##tmptbl_TransactiesTotaal_wtps AS d 
                WHERE NiveauTransactie=1 AND NiveauSector=1 AND (NiveauTegenSector=1 OR NiveauTegenSector Is Null) 
                GROUP BY JPS, VolgordeJPS, JPS_Id, Jaar, Periode, Status, Fase, Rekening, Sector, TegenSector, Transactie, TransactieSoort, d.Onderdeel, Waarde_Type, Definitief 
                UNION
                SELECT DISTINCT j.JPS, j.VolgordeJPS, j.JPS_Id, j.Jaar, j.Periode, j.Status, 'Opleveringen', d.Rekening, d.Sector, d.TegenSector, 'AF.31', d.TransactieSoort, '10', 0, d.Waarde_Type, 1 AS Definitief, 0 
                FROM ##tmptbl_TransactiesTotaal_wtps AS d, ##tbl_SR_JPS_wtps AS j 
                WHERE NiveauTransactie=1 AND NiveauSector=1 AND (NiveauTegenSector=1 OR NiveauTegenSector Is Null) 
                AND d.Rekening = 'FT' AND d.TegenSector = 'S.11' AND d.Sector = 'S.11' AND d.TransactieSoort = 'P' 
                AND j.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) 
                ORDER BY VolgordeJPS DESC, Sector, Fase, Rekening, TegenSector, Transactie, TransactieSoort
                -- 16:38:26
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen processtap'
                go
                
                print 'ophalen DataProcesstapTegenSectorS2'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:26
                SELECT d.JPS, d.VolgordeJPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, d.Rekening, d.Sector, d.TegenSector, d.Transactie, d.TransactieSoort, d.Onderdeel AS Onderdeel, 
                Sum(d.Waarde) as Waarde, d.Waarde_Type, d.Definitief, SUM(CASE WHEN d.Transactiesoort In ('P','B') THEN d.Waarde*-1 ELSE d.Waarde END) AS WaardeSaldo 
                FROM ##tmptbl_TransactiesTotaal_wtps AS d 
                WHERE Waarde_Type = 'B' AND NiveauTransactie=1 AND NiveauSector=1 AND NiveauTegenSector=1 AND TegenSector Like 'S.2%' AND Sector Not Like 'S.2%' 
                GROUP BY JPS, VolgordeJPS, JPS_Id, Jaar, Periode, Status, Fase, Rekening, Sector, TegenSector, Transactie, TransactieSoort, d.Onderdeel, Waarde_Type, Definitief
                UNION
                SELECT DISTINCT j.JPS, j.VolgordeJPS, j.JPS_Id, j.Jaar, j.Periode, j.Status, 'Opleveringen', d.Rekening, d.Sector, d.TegenSector, 'D.41A', d.TransactieSoort, '10', 0, d.Waarde_Type, 1 AS Definitief, 0 
                FROM ##tmptbl_TransactiesTotaal_wtps AS d, ##tbl_SR_JPS_wtps AS j 
                WHERE Waarde_Type = 'B' AND NiveauTransactie=1 AND NiveauSector=1 AND NiveauTegenSector=1 AND d.Rekening = 'LT' AND d.TegenSector Like 'S.2%' AND Sector Not Like 'S.2%' 
                AND j.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984) 
                ORDER BY VolgordeJPS DESC, Sector, Fase, Rekening, TegenSector, Transactie, TransactieSoort 
                -- 16:38:32
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen processtapTS2'
                go
                
                print 'ophalen DataProcesstapSectorS2'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:32
                SELECT d.JPS, d.VolgordeJPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, d.Rekening, d.Sector, d.TegenSector, d.Transactie, d.TransactieSoort, d.Onderdeel AS Onderdeel, 
                Sum(d.Waarde) as Waarde, d.Waarde_Type, d.Definitief, SUM(CASE WHEN d.Transactiesoort In ('P','B') THEN d.Waarde ELSE d.Waarde*-1 END) AS WaardeSaldo 
                FROM ##tmptbl_TransactiesTotaal_wtps AS d 
                 WHERE Waarde_Type = 'R' AND NiveauTransactie=1 AND NiveauSector=1  AND Sector Like 'S.2%' 
                 GROUP BY JPS, VolgordeJPS, JPS_Id, Jaar, Periode, Status, Fase, Rekening, Sector, TegenSector,  Transactie, TransactieSoort,  d.Onderdeel, Waarde_Type, Definitief
                 ORDER BY VolgordeJPS DESC, Sector, Fase, Rekening, TegenSector, Transactie, TransactieSoort
                -- 16:38:36
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen processtapS2'
                go
                
                print 'ophalen commentaar'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:36
                SELECT d.Jaar+'-'+d.Periode+'-'+d.Status AS JPS, d.JPS_Id, d.Jaar, d.Periode, d.Status, d.Fase, d.Functie, d.Iteratie, d.Rekening, d.Sector, d.TegenSector, 
                d.Transactie, d.TransactieSoort, d.Onderdeel, d.Waarde, d.Waarde_Type, CAST(d.Definitief as int) AS Definitief, CAST(d.Commentaar as varchar(250)) AS Commentaar 
                FROM tbl_SR_Data_Transacties AS d 
                WHERE (d.Commentaar Is Not Null AND d.Commentaar <> '') AND d.JPS_Id In (1955,1950,1954,1953,1952,1951) AND d.Onderdeel In (10, 16, 17, 18) AND d.Waarde <> 0  AND d.Definitief = 1
                ORDER BY d.Jaar+'-'+d.Periode+'-'+d.Status DESC, d.Sector, d.Rekening, d.TegenSector, d.Transactie, d.TransactieSoort, d.Onderdeel
                -- 16:38:45
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen commentaar'
                go
                
                print 'aanmaken transacties'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:45
                SELECT t.JPS, t.VolgordeJPS, t.JPS_Id, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.TransactieSoort, t.Onderdeel, 
                Sum(t.Waarde) As Waarde, t.Pids, t.Naam AS Omschrijving, t.Volgorde, LEFT(t.Transactie,3) AS Afk, t.NiveauSector, t.NiveauTegenSector, t.NiveauTransactie 
                INTO ##tmptbl_Transacties_wtps
                FROM ##tmptbl_TransactiesTotaal_wtps AS t 
                WHERE t.Waarde_Type='R' AND t.NiveauTransactie=1 
                GROUP BY t.JPS, t.VolgordeJPS, t.JPS_Id, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.TransactieSoort, t.Onderdeel, t.Pids, t.Naam, t.Volgorde, t.NiveauSector, t.NiveauTegenSector, t.NiveauTransactie
                HAVING Sum(t.Waarde) <> 0
                -- 16:38:49
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde aanmaken transacties'
                go
                
                SET NOCOUNT ON 
                IF OBJECT_ID('tempdb..##tmptbl_Export_Transacties_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_Transacties_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Export_SV_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_SV_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Export_Transactie_Identiteit_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_Transactie_Identiteit_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Export_SXen_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_SXen_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Export_NegBalans_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_NegBalans_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Export_Jaar_Kwartaal_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_Jaar_Kwartaal_wtps
                IF OBJECT_ID('tempdb..##tmptbl_Export_Existentie_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_Existentie_wtps
                go
                
                print 'ophalen SX'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:49
                SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, 
                Abs(Sum(t.Verschil)) AS Verschil, Sum(t.WaardeRand) AS WaardeRand, Sum(t.WaardeBinnenwerk) AS WaardeBinnenwerk, t.Pids 
                INTO ##tmptbl_Export_SXen_wtps
                FROM ( 
                  SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, 
                  Sum(t.Waarde) AS Verschil, Sum(t.Waarde) AS WaardeRand, 0 AS WaardeBinnenwerk, t.Pids 
                  FROM ##tmptbl_Transacties_wtps AS t 
                  WHERE  t.Onderdeel <> '61' AND     (t.Transactie = 'P.119C' OR t.Transactie Like 'A%' OR t.Transactie Like 'D.%')
                	  AND t.Transactie Not In ('AF.11','D.11') AND t.Transactie Not Like 'D.63%'
                	  AND t.Transactie Not Like 'D.2%' AND t.Transactie Not Like 'D.3%'
                	  AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)
                  GROUP BY t.JPS, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, t.Pids
                  UNION 
                  SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, 
                  Sum(t.Waarde*-1) AS Verschil, 0 AS WaardeRand, Sum(t.Waarde) AS WaardeBinnenwerk, t.Pids 
                  FROM ##tmptbl_TransactiesTotaal_wtps AS t 
                  WHERE  t.Onderdeel <> '61' AND  t.Waarde_Type='B' AND t.NiveauTransactie=1 AND (t.Transactie = 'P.119C' OR t.Transactie Like 'A%' OR t.Transactie Like 'D.%')
                	  AND t.Transactie Not In ('AF.11','D.11') AND t.Transactie Not Like 'D.63%'
                	  AND t.Transactie Not Like 'D.2%' AND t.Transactie Not Like 'D.3%'
                	  AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)
                  GROUP BY t.JPS, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, t.Pids 
                ) AS t 
                GROUP BY t.JPS, t.Jaar, t.Periode, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, t.Pids 
                HAVING Abs(Sum(t.Verschil)) > 0 ORDER BY Abs(Sum(t.Verschil)) DESC
                -- 16:38:54
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen SX'
                go
                
                print 'ophalen transactie-id'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:38:57
                SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.Rekening, Abs(Sum(t.Waarde)) AS Verschil, t.Pids 
                INTO ##tmptbl_Export_Transactie_Identiteit_wtps
                 FROM ( 
                SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.Rekening, Sum(t.Waarde) AS Waarde, t.Pids
                FROM ##tmptbl_Transacties_wtps AS t
                WHERE  t.Onderdeel <> '61' AND  t.Transactiesoort In ('A','M') AND ((t.Transactie In ('P.119C','NP')
                	OR t.Transactie Like 'A%' OR t.Transactie Like 'D.%') AND t.Transactie Not In ('AF.11','D.211A','D.213A') AND t.Transactie Not Like 'D.63%' )
                	AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)
                GROUP BY t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.Rekening, t.Pids
                UNION ALL
                SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.Rekening, Sum(t.Waarde*-1) AS Waarde, t.Pids
                FROM ##tmptbl_Transacties_wtps AS t
                WHERE  t.Onderdeel <> '61' AND  t.Transactiesoort In ('P','B') AND ((t.Transactie In ('P.119C','NP')
                	OR t.Transactie Like 'A%' OR t.Transactie Like 'D.%') AND t.Transactie Not In ('AF.11','D.211A','D.213A')   AND t.Transactie Not Like 'D.63%' ) 
                	AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)
                GROUP BY  t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.Rekening, t.Pids
                ) AS t
                GROUP BY t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.Rekening, t.Pids
                HAVING Abs(Sum(t.Waarde)) > 0 ORDER BY Abs(Sum(t.Waarde)) DESC
                -- 16:38:58
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen transactie-id'
                go
                
                print 'ophalen verschil J en Ks'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:39:01
                SELECT t.Jaar+'-'+t.Status AS JS, t.Jaar, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, 
                Sum(t.WaardeJaren) AS WaardeJaren, Sum(t.WaardeKwartalen) AS WaardeKwartalen,Abs(Sum(WaardeJaren)-Sum(WaardeKwartalen)) AS VerschilJK, t.Pids 
                INTO ##tmptbl_Export_Jaar_Kwartaal_wtps
                 FROM ( 
                SELECT t.Jaar, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, Sum(t.Waarde) AS WaardeJaren, 0 AS WaardeKwartalen, t.Pids
                FROM ##tmptbl_Transacties_wtps AS t
                WHERE  t.Onderdeel <> '61' AND  t.Transactie <> 'P.4' AND t.Rekening Not In ('BB','EB') AND t.Periode='Y' AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)
                GROUP BY t.Jaar, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, t.Pids
                UNION
                SELECT t.Jaar, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, 0 AS WaardeJaren, Sum(t.Waarde) AS WaardeKwartalen, t.Pids
                FROM ##tmptbl_Transacties_wtps AS t
                WHERE  t.Onderdeel <> '61' AND  t.Transactie <> 'P.4' AND t.Rekening Not In ('BB','EB') AND t.periode <> 'Y' AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)
                GROUP BY t.Jaar, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, t.Pids
                ) AS t
                GROUP BY t.Jaar, t.Status, t.Rekening, t.Sector, t.Transactie, t.Transactiesoort, t.Pids
                HAVING Abs(Sum(WaardeJaren)-Sum(WaardeKwartalen)) > 0 
                ORDER BY Abs(Sum(WaardeJaren)-Sum(WaardeKwartalen)) DESC
                -- 16:39:02
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen verschil J en Ks'
                go
                
                print 'ophalen Existentie JK'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:39:04
                SELECT t.JS, t.Jaar, t.Status, t.Rekening, t.TransactieSoort, t.Transactie, t.Sector, t.TegenSector, Sum(t.WaardeJ)AS WaardeJ, Sum(t.WaardeQ) AS WaardeQ, Waarde_Type, t.Pids 
                INTO ##tmptbl_Export_Existentie_wtps
                 FROM ( 
                SELECT t.Jaar+'-'+t.status AS JS, t.Jaar, t.Status, t.Rekening, t.TransactieSoort, t.Transactie, t.Sector, t.TegenSector, CASE WHEN t.Periode = 'Y' THEN t.Waarde Else 0 END AS WaardeJ, 
                CASE WHEN (t.Periode = '1' AND t.Rekening = 'BB') OR (t.Periode = '4' AND t.Rekening = 'EB') OR (t.Periode <> 'Y' AND t.Rekening <> 'BB' AND t.Rekening <> 'EB') THEN t.Waarde Else 0 END AS WaardeQ, Waarde_Type, t.Pids
                FROM ##tmptbl_TransactiesTotaal_wtps AS t
                WHERE  t.Onderdeel <> '61' AND t.NiveauSector=1 And (t.NiveauTegenSector=1 Or t.NiveauTegenSector Is Null) And t.NiveauTransactie=1
                ) AS t
                GROUP BY t.JS, t.Jaar, t.Status, t.Rekening, t.TransactieSoort, t.Transactie, t.Sector, t.TegenSector, Waarde_Type, t.Pids
                HAVING (Sum(WaardeJ)=0 And Sum(WaardeQ)<>0) Or (Sum(WaardeJ)<>0 And Sum(WaardeQ)=0)
                ORDER BY t.JS, t.Rekening, t.TransactieSoort, t.Transactie, t.Sector, t.Waarde_Type DESC, t.TegenSector
                -- 16:39:12
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen Existentie JK'
                go
                
                print 'ophalen EB'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:39:14
                SELECT t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.TransactieSoort, t.Sector, IsNull(t.TegenSector,'Rand') AS TegenSector, Sum(t.Waarde) AS BalansWaarde, t.Pids 
                INTO ##tmptbl_Export_NegBalans_wtps
                FROM ##tmptbl_TransactiesTotaal_wtps AS t
                WHERE (((t.Rekening='EB' AND t.Transactie<>'P.4') OR (t.Rekening='LT' AND t.Transactie<>'D.31'))  AND t.NiveauTransactie=1 AND t.NiveauSector=1 AND (t.NiveauTegenSector=1 OR t.NiveauTegenSector Is Null)) 
                GROUP BY t.JPS, t.Jaar, t.Periode, t.Status, t.Transactie, t.TransactieSoort, t.Sector, t.TegenSector, t.Pids
                HAVING Sum(t.Waarde)<0
                -- 16:39:17
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen EB'
                go
                
                print 'ophalen SV'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:39:20
                SELECT t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, t.Sector, t.Onderdeel, Sum(t.Waarde) AS StatVerschil, t.NiveauSector 
                INTO ##tmptbl_Export_SV_wtps
                 FROM ( 
                SELECT t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, t.Sector, t.Onderdeel, Sum(t.Waarde) AS Waarde, t.NiveauSector
                FROM ##tmptbl_Transacties_wtps AS t
                WHERE t.Transactie <> 'P.4' AND t.Transactiesoort In ('A','B') AND t.Rekening In ('FT','LT')
                GROUP BY t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, t.Sector, t.Onderdeel, t.NiveauSector 
                UNION ALL SELECT t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, t.Sector, t.Onderdeel, Sum(t.Waarde*-1) AS Waarde, t.NiveauSector
                FROM ##tmptbl_Transacties_wtps AS t 
                WHERE t.Transactie <> 'P.4' AND t.Transactiesoort In ('P','M') AND t.Rekening In ('FT','LT')
                GROUP BY t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, t.Sector, t.Onderdeel, t.NiveauSector
                ) AS t
                GROUP BY t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, t.Sector, t.Onderdeel, t.NiveauSector
                -- 16:39:21
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen SV'
                go
                
                SET NOCOUNT ON 
                IF OBJECT_ID('tempdb..##tmptbl_Export_Saldi_wtps') Is Not Null
                   DROP TABLE ##tmptbl_Export_Saldi_wtps
                IF OBJECT_ID('tempdb..#SaldoSchema') Is Not Null
                   DROP TABLE #SaldoSchema 
                IF OBJECT_ID('tempdb..##tbl_SR_SaldiClassificatieRelatie_wtps') Is Not Null
                   DROP TABLE ##tbl_SR_SaldiClassificatieRelatie_wtps
                IF OBJECT_ID('tempdb..#cte_saldi') Is Not Null
                   DROP TABLE #cte_saldi 
                
                go
                
                print 'ophalen saldischema en classificaties'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:39:23
                SET NOCOUNT ON 
                SELECT  t.*, CASE WHEN t.TransactieSoort = 'S' Then t.Transactie ELSE '' END AS SaldoKind
                INTO #SaldoSchema
                FROM tbl_SR_SaldoSchema AS t
                 WHERE t.JPS_Id In (1955,1950,1954,1953,1952,1951,1811,1810,1809,1808,1807,1802,1806,1805,1804,1803,1520,1524,1523,1522,1521,1957,2065,2038,2011,1984)
                -- dan voor deze jpssen de ClassificatieRelaties opnieuw ophalen
                ;WITH cte_saldi
                AS
                (   -- ophalen alle saldiberekening
                   SELECT s.JPS_Id, s.Jaar, s.Periode, s.Status, s.Rekening_Data, s.Volgnummer, s.Saldo,
                       s.Sector_Gekozen , s.Sector_Data, s.Transactie, s.TransactieSoort, s.Factor, SaldoKind
                   FROM #SaldoSchema AS s
                   Union ALL
                
                   -- dan joinen met zichzelf om de kinderen op te halen
                   SELECT s.JPS_Id, s.Jaar, s.Periode, s.Status, s.Rekening_Data, s.Volgnummer, s.Saldo,
                       s.Sector_Gekozen, b.Sector_Data, b.Transactie, b.TransactieSoort, b.Factor*s.Factor AS Factor, s.SaldoKind
                   FROM cte_saldi AS s
                   INNER JOIN #SaldoSchema AS b ON s.JPS_Id = b.JPS_Id AND
                       s.Sector_Gekozen = b.Sector_Gekozen And s.Rekening_Data = b.Rekening_Data And s.Transactie = b.Saldo
                 )
                SELECT s.*, c.Naam AS Omschrijving, c.Volgorde AS VolgordeSaldo, 'V' + t.Volgorde AS Volgorde
                INTO #cte_saldi
                FROM cte_saldi s
                INNER JOIN ##tbl_SR_TransactieClassificatie_wtps c ON s.JPS_id=c.JPS_Id AND s.Saldo=c.Transactie AND c.TransactieSoort='S'
                INNER JOIN ##tbl_SR_TransactieClassificatie_wtps t ON s.JPS_id=t.JPS_Id AND s.Transactie=t.Transactie AND s.TransactieSoort=t.TransactieSoort
                WHERE t.TransactieSoort <> 'S' -- alleen de records meenemen die geen saldo meer bevatten
                
                SELECT cte.JPS_Id, cte.Jaar, cte.Periode, cte.[Status], cte.Rekening_Data AS Rekening, 
                    cte.Volgnummer, cte.Saldo, cte.Omschrijving, cte.Sector_Gekozen, cte.Sector_Data,
                    CASE WHEN cte.SaldoKind = '' THEN cte.Transactie ELSE cte.SaldoKind END AS SaldoKind,
                    CASE WHEN cte.SaldoKind = '' THEN cte.TransactieSoort ELSE 'S' END AS SaldoTransactieSoort,
                    CASE WHEN cte.SaldoKind = '' THEN cte.Volgorde ELSE cte.VolgordeSaldo END AS Volgorde,
                    cte.Transactie, cte.TransactieSoort, cte.Factor, LTNiv, FTNiv, LT, FT
                INTO ##tbl_SR_SaldiClassificatieRelatie_wtps
                FROM #cte_saldi AS cte
                INNER JOIN
                -- ophalen niveau's van de gekozen sectoren per rekening
                (   SELECT DISTINCT sfl.JPS_Id, sfl.Volgnummer, sfl.Saldo, sfl.Sector_Gekozen, LTNiv, FTNiv, LT, FT
                    FROM
                    (   SELECT s.JPS_Id, s.Volgnummer, s.Saldo, s.Sector_Gekozen, Max(s.LT) AS LT, MAX(s.FT) AS FT
                        FROM (
                            SELECT DISTINCT s.JPS_Id, s.Volgnummer, s.Saldo, s.Sector_Gekozen,
                                CASE WHEN s.Rekening_Data = 'LT' THEN s.Rekening_Data END AS LT,
                                CASE WHEN s.Rekening_Data <> 'LT' THEN 'FR' END AS FT
                            FROM dbo.tbl_SR_SaldoSchema AS s
                        ) AS s
                        GROUP BY s.JPS_Id, s.Volgnummer, s.Saldo, s.Sector_Gekozen
                    ) AS sfl
                    LEFT JOIN
                    (   SELECT JPS_Id, Sector, Max(LTNiv) AS LTNiv, MAX(FTNiv) AS FTNiv
                            FROM (
                            SELECT JPS_Id, Sector,
                                CASE WHEN Rekening = 'LT' THEN Niveau END AS LTNiv,
                                CASE WHEN Rekening <> 'LT' THEN Niveau END AS FTNiv
                            FROM dbo.tbl_SR_SectorClassificatie
                        ) AS sc
                        GROUP BY JPS_Id, Sector
                    ) AS sc ON sfl.JPS_Id = sc.JPS_Id AND sfl.Sector_Gekozen = sc.Sector
                ) AS sc ON cte.JPS_Id = sc.JPS_Id AND cte.Volgnummer = sc.Volgnummer AND cte.Saldo = sc.Saldo
                    AND cte.Sector_Gekozen = sc.Sector_Gekozen
                
                go
                
                -- 16:42:13
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen saldischema en classificaties'
                go
                
                print 'ophalen saldi'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:42:13
                SELECT t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, SC.Sector_Ouder AS Sector, SC.Tellen_Ouder AS Integratieniveau, s.Volgnummer, s.Saldo, s.Omschrijving, t.Rekening,
                t.Onderdeel AS Onderdeel, s.SaldoKind AS Transactie, s.Volgorde, s.SaldoTransactieSoort AS Transactiesoort, 
                Sum(t.Waarde * s.factor) AS SaldoWaarde, 
                CASE WHEN s.SaldoTransactieSoort = 'S' THEN Sum(t.Waarde * s.factor) ELSE Sum(t.Waarde) END AS Waarde 
                INTO ##tmptbl_Export_Saldi_wtps
                FROM ##tbl_SR_Data_Transacties_wtps AS t 
                INNER JOIN ##tbl_SR_SaldiClassificatieRelatie_wtps AS s ON t.JPS_Id = s.JPS_Id AND t.Sector = s.Sector_Data AND t.Rekening = s.Rekening
                AND t.Transactie = s.Transactie AND t.TransactieSoort = s.TransactieSoort 
                INNER JOIN ##tbl_SR_SectorClassificatieRelatie_wtps AS SC ON t.JPS_Id = SC.JPS_Id AND t.Rekening = SC.Rekening 
                WHERE SC.Sector_Ouder <> 'S' AND Waarde_Type = 'R' 
                   AND ( s.Sector_Gekozen = SC.Sector_Kind 
                    OR ( s.Sector_Gekozen = SC.Sector_Ouder AND s.LT = s.FT AND 
                        (s.LTNiv = 0 AND s.FTNiv = 1) OR (s.LTNiv = 1 AND s.FTNiv = 0) ) ) 
                GROUP BY t.JPS, t.VolgordeJPS, t.Jaar, t.Periode, t.Status, SC.Sector_Ouder, SC.Tellen_Ouder,
                s.Volgnummer, s.Saldo, s.Omschrijving, t.Rekening, s.SaldoKind, s.Volgorde, s.SaldoTransactieSoort, t.Onderdeel
                -- 16:42:57
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen saldi'
                go
                
                print 'zetdataexcel saldidetail'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:42:57
                SELECT * FROM ( SELECT * FROM ##tmptbl_Export_Saldi_wtps
                WHERE Integratieniveau=1 AND Waarde<>0 
                UNION 
                SELECT JPS, VolgordeJPS, Jaar, Periode, Status, Sector, Integratieniveau, Volgnummer, 'Totaal ' + Saldo AS Saldo, Omschrijving, 
                   '' AS Rekening, Onderdeel, '' AS Transactie, 'V99999' as Volgorde, 'S' AS Transactiesoort, SUM(SaldoWaarde) AS SaldoWaarde, SUM(SaldoWaarde) AS Waarde 
                FROM ##tmptbl_Export_Saldi_wtps
                WHERE Integratieniveau=1 AND Waarde<>0 
                GROUP BY JPS, VolgordeJPS, Jaar, Periode, Status, Sector, Integratieniveau, Volgnummer, Saldo, Omschrijving, Onderdeel
                ) AS t 
                 ORDER BY VolgordeJPS DESC, Volgnummer, Saldo, Sector, Transactie
                -- 16:43:10
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen saldi'
                go
                
                print 'zetdataexcel SaldiDataDetailSectorS2'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:43:10
                SELECT * FROM ##tmptbl_Export_Saldi_wtps WHERE Integratieniveau=1 AND Waarde<>0 AND Sector Like 'S.2%'  
                ORDER BY VolgordeJPS DESC, Volgnummer, Saldo, Sector, Transactie
                -- 16:43:12
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen saldi'
                go
                
                print 'zetdataexcel saldigeteld'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:43:12
                SELECT JPS, VolgordeJPS, Jaar, Periode, Status, Sector, Volgnummer, Saldo, Omschrijving, SUM(SaldoWaarde) AS Waarde 
                FROM ##tmptbl_Export_Saldi_wtps 
                GROUP BY JPS, VolgordeJPS, Jaar, Periode, Status, Sector, Volgnummer, Saldo, Omschrijving 
                ORDER BY VolgordeJPS DESC, Sector, Volgnummer
                -- 16:43:18
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen saldi'
                go
                
                print 'zetdataexcel ExtraData'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:43:18
                SELECT j.JPS, j.VolgordeJPS, j.Jaar, j.Periode, j.Status, Sector, Transactie, Waarde 
                FROM dbo.tbl_SR_Extra_Data d 
                INNER JOIN ##tbl_SR_JPS_wtps j ON d.JPS_Id = j.JPS_Id 
                ORDER BY VolgordeJPS DESC, Sector, Transactie
                -- 16:43:20
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen ExtraData'
                go
                
                print 'zetdataexcel EMUSchuld'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:43:20
                SELECT * FROM ##tbl_SR_Data_Transacties_wtps AS D 
                WHERE Sector LIKE 'S.13%' AND TransactieSoort='P' AND Transactie LIKE '%H' 
                ORDER BY VolgordeJPS DESC, Sector, Transactie
                -- 16:43:23
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen EMUSchuld'
                go
                
                print 'zetdataexcel DataTellen2'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:43:23
                SELECT D.*, t.Niveau AS NiveauTransactie 
                FROM ##tbl_SR_Data_Transacties_wtps AS D 
                INNER JOIN ##tbl_SR_TransactieClassificatie_wtps AS t ON D.Transactie = t.Transactie AND D.TransactieSoort = t.TransactieSoort AND D.JPS_Id = t.JPS_Id 
                WHERE t.Niveau = 2 AND Waarde <> 0
                ORDER BY VolgordeJPS DESC, Sector, Transactie
                -- 16:43:28
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde ophalen DataTellen2'
                go
                
                print 'zetdataexcel jpsen'
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                -- 16:44:38
                DECLARE @jpsen AS VARCHAR(MAX)
                 SELECT @jpsen = COALESCE(@jpsen + ';' ,'') + JPS  FROM ##tbl_SR_JPS_wtps 
                 ORDER BY VolgordeJPS
                 SELECT @jpsen
                -- 16:44:38
                print CONVERT (nvarchar(10), CURRENT_TIMESTAMP, 108)
                print 'einde jpsen'
                go
                ")


SQL_data_all <- dbGetQuery(connection, query)
