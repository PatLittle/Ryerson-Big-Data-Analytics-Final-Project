install.packages("openxlsx")


library(openxlsx)
library(readxl)
library(tidyverse)
library(fs)
library(readxl)


wb<-loadWorkbook("downloads-012020-012021.xlsx")
removeWorksheet(wb, 1)
removeWorksheet(wb, 1)
saveWorkbook(wb,"downloads.xlsx", overwrite = T)

path<-"downloads.xlsx"
dls<-lapply(excel_sheets(path), read_excel, path = path)

dl_df<-map_dfr(dls,`[`, c("ID / Identificateur","Title English / Titre en anglais","Number of downloads / Nombre de téléchargements"))
df_df<-na.omit(df)
dl_df$`Title English / Titre en anglais`<-NULL


