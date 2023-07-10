# Voorbeeldje data ophalen iSR

# Verbinding
verbinding_iSR <- odbcDriverConnect("driver={SQL SERVER};server=SQL_HSR_ANA_PRD\\i01,50001;database=HSR_ANA_PRD;trusted_connection=true")

# Query met select van de data, where kan ook anders/ minder zijn. Aanrader is hier zeker te selecteren. Alles is namelijk veel.
query_transacties <- paste0("SELECT Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type, sum(Waarde) AS Waarde 
                       FROM 	tbl_SR_Data_Transacties  
                      WHERE 	Jaar >= ('",jaar_transacties,"') AND Rekening = ('",rekening_EB,"')
                      OR      Jaar >= ('",jaar_transacties,"') AND Rekening = ('",rekening_LT,"')
                      GROUP BY Jaar, Periode, Status, Rekening, Sector, Tegensector, Transactie, Transactiesoort, Waarde_Type")
data_transacties <- sqlQuery(verbinding_iSR, query_transacties)
data_transacties$jps <- paste0(data_transacties$Jaar, data_transacties$Status, data_transacties$Periode)

# Er zijn ook andere tabellen behalve de tbl_SR_Data_Transacties waar nuttige data in staan