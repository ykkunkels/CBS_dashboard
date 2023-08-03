
################################
### TEST Shiny CBS Dashboard ###
### Version 0.0.16           ###
### #!Debug                  ###
### YKK - 03-08-2023         ###
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

# read prepared Excel value files
if(!require('openxlsx')){install.packages('openxlsx', dep = TRUE)};library('openxlsx')
temp_path <- "//cbsp.nl/productie/secundair/IT_NR/Werk/OntwikkelOmgeving/Dashboard ve-R-nieuwen/Ondersteuningsbestanden/LT_M.xlsx"
eee <- read.xlsx(temp_path, colNames = FALSE)

temp_path <- "//cbsp.nl/productie/secundair/IT_NR/Werk/OntwikkelOmgeving/Dashboard ve-R-nieuwen/Ondersteuningsbestanden/LT_B.xlsx"
fff <- read.xlsx(temp_path, colNames = FALSE)

temp_path <- "//cbsp.nl/productie/secundair/IT_NR/Werk/OntwikkelOmgeving/Dashboard ve-R-nieuwen/Ondersteuningsbestanden/EB_A.xlsx"
ggg <- read.xlsx(temp_path, colNames = FALSE)




# Initialise empty df
# transactie_names <- c("P.11A", "P.12A", "D.41A", "P.119C", "D.421", "D.422", "D.43", 
#                       "D.441", "D.4431", "D.4432", "D.45", "D.6122", "D.721", "D.75", "D.92", "D.99")
transactie_names <- eee$X1
# excel_waardes <- c(1161954, 10817, 14754, -1553, 85351, 129, 9762, 106, 15, 12, 14, 11522, 3884, 3253, 400, 516)
excel_waardes <- eee$X2

df_Excel_compare <- data.frame("Transactie" = transactie_names, "2021-Y-D" = rep(NA, 16), 
                               "Excel_waardes" = excel_waardes, "Verschil" = rep(NA, 16))
colnames(df_Excel_compare)[2] <- c("2021-Y-D")

# selecting data
index_Sector <- tbl_SR_Data_Transacties_wtps$Sector == "S.11"
aaa <- tbl_SR_Data_Transacties_wtps[index_Sector, ]


index_LT <- aaa[, "Rekening"] == "LT"
bbb <- aaa[index_LT, ]


index_M <- bbb[, "TransactieSoort"] == "M"
ccc <- bbb[index_M, ]


index_JPS <- ccc$JPS == "2021-Y-D"
ddd <- ccc[index_JPS, ]

# fill df
for (i in 1:nrow(df_Excel_compare)) {
  
  df_Excel_compare$`2021-Y-D`[i] <- sum(ddd[ddd$Transactie == df_Excel_compare$Transactie[i], "Waarde"])
  
}

df_Excel_compare$Verschil <- (df_Excel_compare$`2021-Y-D` - df_Excel_compare$Excel_waardes)


# write df
write.csv(df_Excel_compare, "df_Excel_compare.csv")


