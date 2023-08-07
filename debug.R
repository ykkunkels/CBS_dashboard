
################################
### TEST Shiny CBS Dashboard ###
### Version 0.0.17           ###
### #!Debug                  ###
### YKK - 07-08-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###

#! Connect to SQL Server
if(!require('DBI')){install.packages('DBI', dep = TRUE)};library('BDI')
verbinding_iSR <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD//i01,50001", 
                            Database = "HSR_ANA_PRD")

#! Get JPS info from SQL server ----
# tbl_SR_JPSReferentie
tbl_SR_JPSReferentie <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM tbl_SR_JPSReferentie"))


# tbl_SR_JPS_wtps
tbl_SR_JPS_wtps <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM tbl_SR_JPS"))


# tbl_SR_SectorClassificatie
tbl_SR_SectorClassificatie <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM tbl_SR_SectorClassificatie"))

# vw_SR_VolgordeJPS
vw_SR_VolgordeJPS <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM vw_SR_VolgordeJPS"))


# Difference between tables
temp_index <- !(tbl_SR_JPS_wtps$VolgordeJPS == tbl_SR_JPS_wtps_R$VolgordeJPS)

tbl_SR_JPS_wtps$VolgordeJPS[temp_index]
tbl_SR_JPS_wtps_R$VolgordeJPS[temp_index]


temp_out <- data.frame("from_SQL_logfile" = tbl_SR_JPS_wtps$VolgordeJPS[temp_index], "from_SQL_view" = tbl_SR_JPS_wtps_R$VolgordeJPS[temp_index])

write.csv(temp_out, "Verschil in VolgordeJPS variabele.csv")

if(any(FALSE %in% (tbl_SR_JPS_wtps$VolgordeJPS == tbl_SR_JPS_wtps_R$VolgordeJPS))){
  message("Warning: Inconsistency in 'VolgordeJPS' variable")
}


#--------------------------------------------------------------------------------

## Data selection & values check

temp_index <- c("JPS", "Sector", "Transactie", "TransactieSoort", "Waarde_Type", "Rekening", "Waarde")
temp_data <- tbl_SR_Data_Transacties_wtps[, temp_index]

temp_data <- temp_data[(temp_data[, "JPS"] == "2021-Y-D"), ]
temp_data <- temp_data[(temp_data[, "Waarde_Type"] == "R"), ]
temp_data <- temp_data[(temp_data[, "Sector"] == "S.11"), ]
temp_data <- temp_data[(temp_data[, "Rekening"] != "BB"), ]

# aggregate
temp_out <- aggregate(Waarde ~ Rekening + TransactieSoort + Transactie, temp_data, sum)

# order
temp_out <- temp_out[order(temp_out$Rekening, temp_out$TransactieSoort), ]

# # write df
# write.csv(temp_out, "df_Excel_compare3.csv")

#---

## Get data more directly from SQL server

## Set timer start
rosetta_timer_start <- Sys.time()

temp_query <- paste0("
                      SELECT * FROM tbl_SR_Data_Transacties 
                      WHERE Jaar='2021' AND Periode='Y' AND Status='D' AND
                      Waarde_Type='R' AND Sector='S.11' AND NOT Rekening='BB'
                     ")
tbl_SR_Data_Transacties <- dbGetQuery(verbinding_iSR, temp_query)

temp_index <- c("Sector", "Transactie", "TransactieSoort", "Waarde_Type", "Rekening", "Waarde")
tbl_SR_Data_Transacties <- tbl_SR_Data_Transacties[, temp_index]

# aggregate
temp_out <- aggregate(Waarde ~ Rekening + TransactieSoort + Transactie, tbl_SR_Data_Transacties, sum)

# order
temp_out <- temp_out[order(temp_out$Rekening, temp_out$TransactieSoort), ]

## Set timer end
rosetta_timer_end <- Sys.time()

rosetta_time_taken <- round(rosetta_timer_end - rosetta_timer_start, 2)
rosetta_time_taken

