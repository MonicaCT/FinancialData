#Project: Country Data
#Author:Monica Cueto
#Setting directory-----
rm(list = ls())
raw <-  paste(getwd(),"/Data.ES/Raw.ES",sep="")
clean <-  paste(getwd(),"/Data.ES/Clean.ES",sep="")
tables <-  paste(getwd(),"/Outputs.ES/Tables.ES",sep="")
#Packages-------------
#install.packages("pacman")
pacman::p_load(data.table, dplyr, ggplot2, gridExtra, Hmisc, httr, lubridate,
               openxlsx, padr, readr, readxl, rio, stringr, tidyverse, writexl,
               XML, reshape2)

#Abriendo el archivo final-------
EconomicSector <- read_excel(paste(raw,"EconomicSector_panel.xlsx",sep="/"))
#Con el archivo descargado
class(EconomicSector$Date)
EconomicSector$Date <- as.Date(EconomicSector$Date, format = "%Y-%m-%d")
class(EconomicSector$Agricultural)
cols = c(3:9)
EconomicSector[, cols] = apply(EconomicSector[, cols], 2, function(x)
  as.numeric(as.character(x)))

######################### INDEX18------------------------
#Anhos-------------
EconomicSector$year2 <- 2018
EconomicSector$year3 <- 2019
EconomicSector$year4 <- 2020
EconomicSector$year <- format(EconomicSector$Date, "%Y")
#Para todos los paises-------------
#for(i in range(EconomicSector$year)){
EconomicSector <-mutate(EconomicSector,id1=ifelse(year2==year,1,NA))
EconomicSector$id <- EconomicSector %>% group_indices(id1,Country)
EconomicSector$idc <- EconomicSector %>% group_indices(Country)
EconomicSector <-mutate(EconomicSector,id2=ifelse(id>=15,NA,id))
#cierre del anho 2018
EconomicSector$Industry18 <- ave(EconomicSector$Industry, EconomicSector$id2, FUN = last)
EconomicSector$T.productive18 <- ave(EconomicSector$T.productive, EconomicSector$id2, FUN = last)
EconomicSector$Agricultural18 <- ave(EconomicSector$Agricultural, EconomicSector$id2, FUN = last)
EconomicSector$T.credit18 <- ave(EconomicSector$T.credit, EconomicSector$id2, FUN = last)
EconomicSector$Individuals18 <- ave(EconomicSector$Individuals, EconomicSector$id2, FUN = last)
EconomicSector$Remaining18 <- ave(EconomicSector$Remaining, EconomicSector$id2, FUN = last)
EconomicSector$Commerce18 <- ave(EconomicSector$Commerce, EconomicSector$id2, FUN = last)
# Con los valores importantes 2018
EconomicSector <-mutate(EconomicSector,Industry18=ifelse(id2>=15,NA,Industry18))
EconomicSector <-mutate(EconomicSector,T.productive18=ifelse(id2>=15,NA,T.productive18))
EconomicSector <-mutate(EconomicSector,Agricultural18=ifelse(id2>=15,NA,Agricultural18))
EconomicSector <-mutate(EconomicSector,T.credit18=ifelse(id2>=15,NA,T.credit18))
EconomicSector <-mutate(EconomicSector,Individuals18=ifelse(id2>=15,NA,Individuals18))
EconomicSector <-mutate(EconomicSector,Remaining18=ifelse(id2>=15,NA,Remaining18))
EconomicSector <-mutate(EconomicSector,Commerce18=ifelse(id2>=15,NA,Commerce18))
# Rellenar los valores 2018
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(Industry18, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(T.productive18, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(Agricultural18, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(T.credit18, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(Individuals18, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(Remaining18, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc) %>% fill(Commerce18, .direction = "down")
#Para Chile----------------
#Para el anho 2020
EconomicSector <-mutate(EconomicSector,idh=ifelse(year4==year,1,NA))
EconomicSector$id20 <- EconomicSector %>% group_indices(idh,Country)
EconomicSector$idc20 <- EconomicSector %>% group_indices(Country)
EconomicSector <-mutate(EconomicSector,id3=ifelse(id20>=15,NA,id20))
#cierre del anho 2020
EconomicSector$Industry20 <- ave(EconomicSector$Industry, EconomicSector$id3, FUN = first)
EconomicSector$T.productive20 <- ave(EconomicSector$T.productive, EconomicSector$id3, FUN = first)
EconomicSector$Agricultural20 <- ave(EconomicSector$Agricultural, EconomicSector$id3, FUN = first)
EconomicSector$T.credit20 <- ave(EconomicSector$T.credit, EconomicSector$id3, FUN = first)
EconomicSector$Individuals20 <- ave(EconomicSector$Individuals, EconomicSector$id3, FUN = first)
EconomicSector$Remaining20 <- ave(EconomicSector$Remaining, EconomicSector$id3, FUN = first)
EconomicSector$Commerce20 <- ave(EconomicSector$Commerce, EconomicSector$id3, FUN = first)
# Con los valores importantes 2020
EconomicSector <-mutate(EconomicSector,Industry20=ifelse(id20>=15,NA,Industry20))
EconomicSector <-mutate(EconomicSector,T.productive20=ifelse(id20>=15,NA,T.productive20))
EconomicSector <-mutate(EconomicSector,Agricultural20=ifelse(id20>=15,NA,Agricultural20))
EconomicSector <-mutate(EconomicSector,T.credit20=ifelse(id20>=15,NA,T.credit20))
EconomicSector <-mutate(EconomicSector,Individuals20=ifelse(id20>=15,NA,Individuals20))
EconomicSector <-mutate(EconomicSector,Remaining20=ifelse(id20>=15,NA,Remaining20))
EconomicSector <-mutate(EconomicSector,Commerce20=ifelse(id20>=15,NA,Commerce20))
# Rellenar los valores 2020
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Industry20, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(T.productive20, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Agricultural20, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(T.credit20, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Individuals20, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Remaining20, .direction = "down")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Commerce20, .direction = "down")
#
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Industry20, .direction = "up")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(T.productive20, .direction = "up")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Agricultural20, .direction = "up")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(T.credit20, .direction = "up")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Individuals20, .direction = "up")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Remaining20, .direction = "up")
EconomicSector <- EconomicSector %>% group_by(idc20) %>% fill(Commerce20, .direction = "up")
# Con los valores importantes 2020
EconomicSector <-mutate(EconomicSector,Industry18=ifelse(Country=="Chile",Industry20,Industry18))
EconomicSector <-mutate(EconomicSector,T.productive18=ifelse(Country=="Chile",T.productive20,T.productive18))
EconomicSector <-mutate(EconomicSector,Agricultural18=ifelse(Country=="Chile",Agricultural20,Agricultural18))
EconomicSector <-mutate(EconomicSector,T.credit18=ifelse(Country=="Chile",T.credit20,T.credit18))
EconomicSector <-mutate(EconomicSector,Individuals18=ifelse(Country=="Chile",Individuals20,Individuals18))
EconomicSector <-mutate(EconomicSector,Remaining18=ifelse(Country=="Chile",Remaining20,Remaining18))
EconomicSector <-mutate(EconomicSector,Commerce18=ifelse(Country=="Chile",Commerce20,Commerce18))

#El indice para todos los paises-------------
# Dividiendo cada mes entre el cierre de 2018
EconomicSector$iIndustry18 <- (EconomicSector$Industry/EconomicSector$Industry18)*100
EconomicSector$iT.productive18 <- (EconomicSector$T.productive/EconomicSector$T.productive18)*100
EconomicSector$iAgricultural18 <- (EconomicSector$Agricultural/EconomicSector$Agricultural18)*100
EconomicSector$iT.credit18 <- (EconomicSector$T.credit/EconomicSector$T.credit18)*100
EconomicSector$iIndividuals18 <- (EconomicSector$Individuals/EconomicSector$Individuals18)*100
EconomicSector$iRemaining18 <- (EconomicSector$Remaining/EconomicSector$Remaining18)*100
EconomicSector$iCommerce18 <- (EconomicSector$Commerce/EconomicSector$Commerce18)*100

######################## INDEX18 deflactado---------------
#Trabajando el IPC---------
ipc <- read_excel("/Final_CreditType/Data/Clean/IPC.xlsx")
class(ipc$ipc)
#Ratio ipc---------------------------------------------
ipc$year2 <- 2018
#for(i in range(ipc$year)){
ipc <-mutate(ipc,id1=ifelse(year2==year,1,NA))
ipc$id <- ipc %>% group_indices(id1,countryi)
ipc$idc <- ipc %>% group_indices(countryi)
ipc <-mutate(ipc,id2=ifelse(id<=16,id,NA))
#cierre del anho 2018
ipc$ipc18 <- ave(ipc$ipc, ipc$id2, FUN = last)
# Con los valores importantes 2018
ipc <-mutate(ipc,ipc18=ifelse(id2<=16,ipc18,NA))
# Rellenar los valores 2018
ipc <- ipc %>% group_by(idc) %>% fill(ipc18, .direction = "down")
# Dividiendo cada mes entre el cierre de 2018
ipc$iipc18 <- ipc$ipc18/ipc$ipc
class(ipc$Date)
ipc$Date <- as.Date(ipc$Date, format = "%Y-%m-%d")
#Ratio ipc para Chile---------------------------------------------
ipc$year3 <- 2020
#for(i in range(ipc$year)){
ipc <-mutate(ipc,id1=ifelse(year3==year,1,NA))
ipc$id <- ipc %>% group_indices(id1,countryi)
ipc$idc <- ipc %>% group_indices(countryi)
ipc <-mutate(ipc,id2=ifelse(id>=17,NA,id))
#cierre del anho 2020
ipc$ipc20 <- ave(ipc$ipc, ipc$id2, FUN = first)
# Con los valores importantes 2020
ipc <-mutate(ipc,ipc20=ifelse(id2<=16,ipc20,NA))
# Rellenar los valores 2020
ipc <- ipc %>% group_by(idc) %>% fill(ipc20, .direction = "down")
ipc <- ipc %>% group_by(idc) %>% fill(ipc20, .direction = "up")
# Dividiendo cada mes entre el cierre de 2020
ipc$iipc20 <- ipc$ipc20/ipc$ipc
#Creando un identificador para el merge------
ipc$myeark <- format(ipc$Date, "%Y-%m")
ipc$key <- paste(ipc$countryi,ipc$myeark)
colnames(ipc)
keep.ipc <- c("key","ipc","iipc18","iipc20")
ipc <- ipc[keep.ipc]
#
EconomicSector$myeark <- format(EconomicSector$Date, "%Y-%m")
EconomicSector$key <- paste(EconomicSector$Country,EconomicSector$myeark)
#Merging data-----------
EconomicSector <- merge(EconomicSector,
                    ipc,
                    by = "key", all=TRUE)
#Indice 18ipc deflactado-------------
EconomicSector$iIndustry18ipc <- EconomicSector$iipc18*EconomicSector$iIndustry18
EconomicSector$iT.productive18ipc <- EconomicSector$iipc18*EconomicSector$iT.productive18
EconomicSector$iAgricultural18ipc <- EconomicSector$iipc18*EconomicSector$iAgricultural18
EconomicSector$iT.credit18ipc <- EconomicSector$iipc18*EconomicSector$iT.credit18
EconomicSector$iIndividuals18ipc <- EconomicSector$iipc18*EconomicSector$iIndividuals18
EconomicSector$iRemaining18ipc <- EconomicSector$iipc18*EconomicSector$iRemaining18
EconomicSector$iCommerce18ipc <- EconomicSector$iipc18*EconomicSector$iCommerce18
#Indice 20ipc deflactado-------------
EconomicSector$iIndustry20ipc <- EconomicSector$iipc20*EconomicSector$iIndustry18
EconomicSector$iT.productive20ipc <- EconomicSector$iipc20*EconomicSector$iT.productive18
EconomicSector$iAgricultural20ipc <- EconomicSector$iipc20*EconomicSector$iAgricultural18
EconomicSector$iT.credit20ipc <- EconomicSector$iipc20*EconomicSector$iT.credit18
EconomicSector$iIndividuals20ipc <- EconomicSector$iipc20*EconomicSector$iIndividuals18
EconomicSector$iRemaining20ipc <- EconomicSector$iipc20*EconomicSector$iRemaining18
EconomicSector$iCommerce20ipc <- EconomicSector$iipc20*EconomicSector$iCommerce18
#El indice para Chile-------------
EconomicSector <- mutate(EconomicSector,iIndustry18ipc=
                                          ifelse(Country=="Chile",iIndustry20ipc,iIndustry18ipc))
EconomicSector <- mutate(EconomicSector,iT.productive18ipc=
                                              ifelse(Country=="Chile",iT.productive20ipc,iT.productive18ipc))
EconomicSector <- mutate(EconomicSector,iAgricultural18ipc=
                                              ifelse(Country=="Chile",iAgricultural20ipc,iAgricultural18ipc))
EconomicSector <- mutate(EconomicSector,iT.credit18ipc=
                                          ifelse(Country=="Chile",iT.credit20ipc,iT.credit18ipc))
EconomicSector <- mutate(EconomicSector,iIndividuals18ipc=
                                             ifelse(Country=="Chile",iIndividuals20ipc,iIndividuals18ipc))
EconomicSector <- mutate(EconomicSector,iRemaining18ipc=
                                           ifelse(Country=="Chile",iRemaining20ipc,iRemaining18ipc))
EconomicSector <- mutate(EconomicSector,iCommerce18ipc=
                                          ifelse(Country=="Chile",iCommerce20ipc,iCommerce18ipc))

####################### Exportando-----------------------
#Filtrando las variables importantes-------
colnames(EconomicSector)
keep.final <- c(
"Date"   ,            "Country" ,          
"Agricultural" ,      "Industry"  ,         "Commerce"   ,       
"Remaining",          "T.productive" ,      "T.credit" ,         
"Individuals" ,
"iIndustry18" ,       "iT.productive18",    "iAgricultural18" ,  
"iT.credit18" ,       "iIndividuals18" ,    "iRemaining18" ,     
"iCommerce18" ,       
"iIndustry18ipc",     "iT.productive18ipc",
"iAgricultural18ipc" ,"iT.credit18ipc",     "iIndividuals18ipc" ,
"iRemaining18ipc",    "iCommerce18ipc"
)

EconomicSector.final <- EconomicSector[keep.final]
#Eliminando valores vacios ----------------------------------
EconomicSector.final <- EconomicSector.final %>% drop_na(Date)
#Exportando el archivo final--------
write.csv(
  EconomicSector.final,paste("Data.ES/Clean.ES/EconomicSector.final.csv",sep="/"),
  row.names = FALSE
)
write_xlsx(
  EconomicSector.final,paste("Data.ES/Clean.ES/EconomicSector.final.xlsx",sep="/"))
####################### Table summarize---------
#Abriendo los datos-------------
EconomicSector <- readxl::read_excel("Data.ES/Clean.ES/EconomicSector.final.xlsx")
old_EconomicSector <- readxl::read_excel("Outputs.ES/Tables.ES/EconomicSector.final.xlsx")
EconomicSector$Date <- as.Date(EconomicSector$Date, format = "%Y-%m-%d")
old_EconomicSector$Date <- as.Date(old_EconomicSector$Date, format = "%Y-%m-%d")
#De factor a numerico-----
class(EconomicSector$Agricultural)
cols = c(3:23)
EconomicSector[, cols] = apply(EconomicSector[, cols], 2, function(x)
  as.numeric(as.character(x)))
old_EconomicSector[, cols] = apply(old_EconomicSector[, cols], 2, function(x)
  as.numeric(as.character(x)))
#Last date by country----
col1 <- data.frame(old_EconomicSector %>%
                     group_by(Country) %>%
                     summarise(old.date = max(Date), old.obser = n()))
col2 <- data.frame(EconomicSector %>%
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
png("Outputs.ES/Tables.ES/Table.ES.summary.png")
p <- tableGrob(table.summary)
grid.arrange(top = "Results summary", p)
grid::grid.draw(p)
dev.off()

