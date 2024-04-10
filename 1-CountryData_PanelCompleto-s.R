#Project: Country Data
#Author:Monica Cueto
#Setting directory-----
rm(list = ls())
panel <-  paste(getwd(),"/Final_PanelCompleto",sep="")
#Packages-------------
pacman::p_load(data.table, dplyr, ggplot2, gridExtra, Hmisc, httr, lubridate,
               openxlsx, padr, readr, readxl, rio, stringr, tidyverse, writexl,
               XML, reshape2)
####################### Merge CreditType y EconomicSector-----------
#Abriendo los datos finales-------------
EconomicSector <- readxl::read_excel("/Final_EconomicSector/Data.ES/Clean.ES/EconomicSector.final.xlsx")
CrediType <- readxl::read_excel("/Final_CreditType/Data/Clean/CreditType.final.xlsx")
#Creando un identificador para el merge------
EconomicSector$myeark <- format(EconomicSector$Date, "%Y-%m")
EconomicSector$key <- paste(EconomicSector$Country,EconomicSector$myeark)
EconomicSector <- subset( EconomicSector, select = -myeark )
EconomicSector <- subset( EconomicSector, select = -Date )
EconomicSector <- subset( EconomicSector, select = -Country )
#
CrediType$myeark <- format(CrediType$Date, "%Y-%m")
CrediType$key <- paste(CrediType$Country,CrediType$myeark)
CrediType <- subset( CrediType, select = -myeark )
#Merging data-----------
PanelCompleto.final <- merge(CrediType,EconomicSector,
                             by = "key", all=TRUE)
#Con las variables importantes-----
PanelCompleto.final <- subset( PanelCompleto.final, select = -key )

####################### Exportando-----------
PanelCompleto.final <- PanelCompleto.final %>% drop_na(Date)
class(PanelCompleto.final$Date)
PanelCompleto.final$Date <- as.Date(PanelCompleto.final$Date, format = "%Y-%m-%d")
write.csv(
  PanelCompleto.final,paste("/Final_PanelCompleto/Clean.PC/PanelCompleto.final.csv",sep="/"),
  row.names = FALSE
)
write_xlsx(
  PanelCompleto.final,paste("/Final_PanelCompleto/Clean.PC/PanelCompleto.final.xlsx",sep="/"))
####################### Table summarize---------
#Abriendo los datos-------------
PanelCompleto <- readxl::read_excel("/Final_PanelCompleto/Clean.PC/PanelCompleto.final.xlsx")
old_PanelCompleto <- readxl::read_excel("/Final_PanelCompleto/Tables.PC/PanelCompleto.final.xlsx")
PanelCompleto$Date <- as.Date(PanelCompleto$Date, format = "%Y-%m-%d")
old_PanelCompleto$Date <- as.Date(old_PanelCompleto$Date, format = "%Y-%m-%d")
#De factor a numerico-----
class(PanelCompleto$Agricultural)
cols = c(3:41)
PanelCompleto[, cols] = apply(PanelCompleto[, cols], 2, function(x)
  as.numeric(as.character(x)))
old_PanelCompleto[, cols] = apply(old_PanelCompleto[, cols], 2, function(x)
  as.numeric(as.character(x)))
#Last date by country----
col1 <- data.frame(old_PanelCompleto %>%
                     group_by(Country) %>%
                     summarise(old.date = max(Date), old.obser = n()))
col2 <- data.frame(PanelCompleto %>%
                     group_by(Country) %>%
                     summarise(new.date = max(Date), new.obser = n()))
table.summary <- merge(col1,col2,
                       by = "Country", all=TRUE)
table.summary$Updated.obser <- table.summary$new.obser-table.summary$old.obser
keeps9 <- c('Country',"old.date","old.obser","new.date","new.obser",
            "Updated.obser")
table.summary <- table.summary[keeps9]
colnames(table.summary)[2] <- "(1)"
colnames(table.summary)[3] <- "(2)"
colnames(table.summary)[4] <- "(3)"
colnames(table.summary)[5] <- "(4)"
colnames(table.summary)[6] <- "(5)"
#Tabla final------
png("/Final_PanelCompleto/Tables.PC/Table.PC.summary.png")
p <- tableGrob(table.summary)
grid.arrange(top = "Results summary", p)
grid::grid.draw(p)
dev.off()

