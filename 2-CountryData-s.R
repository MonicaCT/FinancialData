#Project: Country Data
#Author:Monica Cueto
#Setting directory-----
rm(list = ls())
backup <-  paste(getwd(),"/Data/Backup",sep="")
raw <-  paste(getwd(),"/Data/Raw",sep="")
clean <-  paste(getwd(),"/Data/Clean",sep="")
nauto <-  paste(getwd(),"/Data/Nauto",sep="")
tables <-  paste(getwd(),"/Outputs/Tables",sep="")
#Packages-------------
pacman::p_load(data.table, dplyr, ggplot2, gridExtra, Hmisc, httr, lubridate,
               openxlsx, padr, readr, readxl, rio, stringr, tidyverse, writexl,
               XML, reshape2)

#Eleccion del rezago mensual----
rezago <- 2
#Parametros-----------
todaym <- format(Sys.time(), "%m")
atodaym <- as.numeric(format(Sys.time(), "%m"))
todayy <- as.numeric(format(Sys.time(), "%Y"))
year2 <- as.numeric(format(Sys.time(), "%Y"))
year <- as.numeric(todayy)%%100
atodaym <- atodaym-rezago #Dos meses antes
ctodaym <- atodaym+1 #1 mes despues se reporta
len <- nchar(gsub("\\D", "", ctodaym))
ctodaym <- ifelse(len==1,paste("0",ctodaym,sep=""),ctodaym)
month <- c(atodaym)
#Diccionarios----------------
amonth=ifelse(atodaym==1,"Enero",atodaym)
amonth=ifelse(atodaym==2,"Febrero",amonth)
amonth=ifelse(atodaym==3,"Marzo",amonth)
amonth=ifelse(atodaym==4,"Abril",amonth)
amonth=ifelse(atodaym==5,"Mayo",amonth)
amonth=ifelse(atodaym==6,"Junio",amonth)
amonth=ifelse(atodaym==7,"Julio",amonth)
amonth=ifelse(atodaym==8,"Agosto",amonth)
amonth=ifelse(atodaym==9,"Septiembre",amonth)
amonth=ifelse(atodaym==10,"Octubre",amonth)
amonth=ifelse(atodaym==11,"Noviembre",amonth)
amonth=ifelse(atodaym==12,"Diciembre",amonth)
#
amonth1=ifelse((atodaym-1)==1,"Enero",atodaym)
amonth1=ifelse((atodaym-1)==2,"Febrero",amonth1)
amonth1=ifelse((atodaym-1)==3,"Marzo",amonth1)
amonth1=ifelse((atodaym-1)==4,"Abril",amonth1)
amonth1=ifelse((atodaym-1)==5,"Mayo",amonth1)
amonth1=ifelse((atodaym-1)==6,"Junio",amonth1)
amonth1=ifelse((atodaym-1)==7,"Julio",amonth1)
amonth1=ifelse((atodaym-1)==8,"Agosto",amonth1)
amonth1=ifelse((atodaym-1)==9,"Septiembre",amonth1)
amonth1=ifelse((atodaym-1)==10,"Octubre",amonth1)
amonth1=ifelse((atodaym-1)==11,"Noviembre",amonth1)
amonth1=ifelse((atodaym-1)==12,"Diciembre",amonth1)
#
amonth2=ifelse(atodaym==1,"Ene",atodaym)
amonth2=ifelse(atodaym==2,"Feb",amonth2)
amonth2=ifelse(atodaym==3,"Mar",amonth2)
amonth2=ifelse(atodaym==4,"Abr",amonth2)
amonth2=ifelse(atodaym==5,"May",amonth2)
amonth2=ifelse(atodaym==6,"Jun",amonth2)
amonth2=ifelse(atodaym==7,"Jul",amonth2)
amonth2=ifelse(atodaym==8,"Ago",amonth2)
amonth2=ifelse(atodaym==9,"Sep",amonth2)
amonth2=ifelse(atodaym==10,"Oct",amonth2)
amonth2=ifelse(atodaym==11,"Nov",amonth2)
amonth2=ifelse(atodaym==12,"Dic",amonth2)
#
amonth3=ifelse(atodaym==1,"ene",atodaym)
amonth3=ifelse(atodaym==2,"feb",amonth3)
amonth3=ifelse(atodaym==3,"mar",amonth3)
amonth3=ifelse(atodaym==4,"abr",amonth3)
amonth3=ifelse(atodaym==5,"may",amonth3)
amonth3=ifelse(atodaym==6,"jun",amonth3)
amonth3=ifelse(atodaym==7,"jul",amonth3)
amonth3=ifelse(atodaym==8,"ago",amonth3)
amonth3=ifelse(atodaym==9,"sept",amonth3)
amonth3=ifelse(atodaym==10,"oct",amonth3)
amonth3=ifelse(atodaym==11,"nov",amonth3)
amonth3=ifelse(atodaym==12,"dic",amonth3)
#
amonth3a=ifelse((atodaym-1)==1,"ene",atodaym)
amonth3a=ifelse((atodaym-1)==2,"feb",amonth3a)
amonth3a=ifelse((atodaym-1)==3,"mar",amonth3a)
amonth3a=ifelse((atodaym-1)==4,"abr",amonth3a)
amonth3a=ifelse((atodaym-1)==5,"may",amonth3a)
amonth3a=ifelse((atodaym-1)==6,"jun",amonth3a)
amonth3a=ifelse((atodaym-1)==7,"jul",amonth3a)
amonth3a=ifelse((atodaym-1)==8,"ago",amonth3a)
amonth3a=ifelse((atodaym-1)==9,"sept",amonth3a)
amonth3a=ifelse((atodaym-1)==10,"oct",amonth3a)
amonth3a=ifelse((atodaym-1)==11,"nov",amonth3a)
amonth3a=ifelse((atodaym-1)==12,"dic",amonth3a)
#
amonth4=ifelse(atodaym==1,"jan",atodaym)
amonth4=ifelse(atodaym==2,"feb",amonth4)
amonth4=ifelse(atodaym==3,"mar",amonth4)
amonth4=ifelse(atodaym==4,"apr",amonth4)
amonth4=ifelse(atodaym==5,"may",amonth4)
amonth4=ifelse(atodaym==6,"jun",amonth4)
amonth4=ifelse(atodaym==7,"jul",amonth4)
amonth4=ifelse(atodaym==8,"aug",amonth4)
amonth4=ifelse(atodaym==9,"sep",amonth4)
amonth4=ifelse(atodaym==10,"oct",amonth4)
amonth4=ifelse(atodaym==11,"nov",amonth4)
amonth4=ifelse(atodaym==12,"dec",amonth4)
#
amonth_p=ifelse(atodaym==1,"Enero",atodaym)
amonth_p=ifelse(atodaym==2,"Febrero",amonth_p)
amonth_p=ifelse(atodaym==3,"Marzo",amonth_p)
amonth_p=ifelse(atodaym==4,"Abril",amonth_p)
amonth_p=ifelse(atodaym==5,"Mayo",amonth_p)
amonth_p=ifelse(atodaym==6,"Junio",amonth_p)
amonth_p=ifelse(atodaym==7,"Julio",amonth_p)
amonth_p=ifelse(atodaym==8,"Agosto",amonth_p)
amonth_p=ifelse(atodaym==9,"Setiembre",amonth_p)
amonth_p=ifelse(atodaym==10,"Octubre",amonth_p)
amonth_p=ifelse(atodaym==11,"Noviembre",amonth_p)
amonth_p=ifelse(atodaym==12,"Diciembre",amonth_p)
#
amonthi=ifelse(atodaym==1,"Jan",atodaym)
amonthi=ifelse(atodaym==2,"Feb",amonthi)
amonthi=ifelse(atodaym==3,"Mar",amonthi)
amonthi=ifelse(atodaym==4,"Apr",amonthi)
amonthi=ifelse(atodaym==5,"May",amonthi)
amonthi=ifelse(atodaym==6,"Jun",amonthi)
amonthi=ifelse(atodaym==7,"Jul",amonthi)
amonthi=ifelse(atodaym==8,"Aug",amonthi)
amonthi=ifelse(atodaym==9,"Sep",amonthi)
amonthi=ifelse(atodaym==10,"Oct",amonthi)
amonthi=ifelse(atodaym==11,"Nov",amonthi)
amonthi=ifelse(atodaym==12,"Dec",amonthi)
#
amonthio=ifelse(atodaym==1+1,"Jan",atodaym)
amonthio=ifelse(atodaym==2+1,"Feb",amonthio)
amonthio=ifelse(atodaym==3+1,"Mar",amonthio)
amonthio=ifelse(atodaym==4+1,"Apr",amonthio)
amonthio=ifelse(atodaym==5+1,"May",amonthio)
amonthio=ifelse(atodaym==6+1,"Jun",amonthio)
amonthio=ifelse(atodaym==7+1,"Jul",amonthio)
amonthio=ifelse(atodaym==8+1,"Aug",amonthio)
amonthio=ifelse(atodaym==9+1,"Sep",amonthio)
amonthio=ifelse(atodaym==10+1,"Oct",amonthio)
amonthio=ifelse(atodaym==11+1,"Nov",amonthio)
amonthio=ifelse(atodaym==12+1,"Dec",amonthio)
#
month=ifelse(month=="1","ENE",month)
month=ifelse(month=="2","FEB",month)
month=ifelse(month=="3","MAR",month)
month=ifelse(month=="4","ABR",month)
month=ifelse(month=="5","MAY",month)
month=ifelse(month=="6","JUN",month)
month=ifelse(month=="7","JUL",month)
month=ifelse(month=="8","AGO",month)
month=ifelse(month=="9","SEPT",month)
month=ifelse(month=="10","OCT",month)
month=ifelse(month=="11","NOV",month)
month=ifelse(month=="12","DIC",month)
#
amonth=ifelse(atodaym==1,"Jan",atodaym)
amonth=ifelse(atodaym==2,"Feb",amonth)
amonth=ifelse(atodaym==3,"Mar",amonth)
amonth=ifelse(atodaym==4,"Apr",amonth)
amonth=ifelse(atodaym==5,"May",amonth)
amonth=ifelse(atodaym==6,"Jun",amonth)
amonth=ifelse(atodaym==7,"Jul",amonth)
amonth=ifelse(atodaym==8,"Aug",amonth)
amonth=ifelse(atodaym==9,"Sep",amonth)
amonth=ifelse(atodaym==10,"Oct",amonth)
amonth=ifelse(atodaym==11,"Nov",amonth)
amonth=ifelse(atodaym==12,"Dec",amonth)
#
amonth=ifelse(atodaym==1,"Enero",atodaym)
amonth=ifelse(atodaym==2,"Febrero",amonth)
amonth=ifelse(atodaym==3,"Marzo",amonth)
amonth=ifelse(atodaym==4,"Abril",amonth)
amonth=ifelse(atodaym==5,"Mayo",amonth)
amonth=ifelse(atodaym==6,"Junio",amonth)
amonth=ifelse(atodaym==7,"Julio",amonth)
amonth=ifelse(atodaym==8,"Agosto",amonth)
amonth=ifelse(atodaym==9,"Septiembre",amonth)
amonth=ifelse(atodaym==10,"Octubre",amonth)
amonth=ifelse(atodaym==11,"Noviembre",amonth)
amonth=ifelse(atodaym==12,"Diciembre",amonth)
amonth
amonth1=ifelse((atodaym-1)==1,"Enero",atodaym)
amonth1=ifelse((atodaym-1)==2,"Febrero",amonth1)
amonth1=ifelse((atodaym-1)==3,"Marzo",amonth1)
amonth1=ifelse((atodaym-1)==4,"Abril",amonth1)
amonth1=ifelse((atodaym-1)==5,"Mayo",amonth1)
amonth1=ifelse((atodaym-1)==6,"Junio",amonth1)
amonth1=ifelse((atodaym-1)==7,"Julio",amonth1)
amonth1=ifelse((atodaym-1)==8,"Agosto",amonth1)
amonth1=ifelse((atodaym-1)==9,"Septiembre",amonth1)
amonth1=ifelse((atodaym-1)==10,"Octubre",amonth1)
amonth1=ifelse((atodaym-1)==11,"Noviembre",amonth1)
amonth1=ifelse((atodaym-1)==12,"Diciembre",amonth1)
amonth1

amonth2=ifelse(atodaym==1,"Ene",atodaym)
amonth2=ifelse(atodaym==2,"Feb",amonth2)
amonth2=ifelse(atodaym==3,"Mar",amonth2)
amonth2=ifelse(atodaym==4,"Abr",amonth2)
amonth2=ifelse(atodaym==5,"May",amonth2)
amonth2=ifelse(atodaym==6,"Jun",amonth2)
amonth2=ifelse(atodaym==7,"Jul",amonth2)
amonth2=ifelse(atodaym==8,"Ago",amonth2)
amonth2=ifelse(atodaym==9,"Sep",amonth2)
amonth2=ifelse(atodaym==10,"Oct",amonth2)
amonth2=ifelse(atodaym==11,"Nov",amonth2)
amonth2=ifelse(atodaym==12,"Dic",amonth2)
amonth2
amonth3=ifelse(atodaym==1,"ene",atodaym)
amonth3=ifelse(atodaym==2,"feb",amonth3)
amonth3=ifelse(atodaym==3,"mar",amonth3)
amonth3=ifelse(atodaym==4,"abr",amonth3)
amonth3=ifelse(atodaym==5,"may",amonth3)
amonth3=ifelse(atodaym==6,"jun",amonth3)
amonth3=ifelse(atodaym==7,"jul",amonth3)
amonth3=ifelse(atodaym==8,"ago",amonth3)
amonth3=ifelse(atodaym==9,"sept",amonth3)
amonth3=ifelse(atodaym==10,"oct",amonth3)
amonth3=ifelse(atodaym==11,"nov",amonth3)
amonth3=ifelse(atodaym==12,"dic",amonth3)
amonth3
amonth3a=ifelse((atodaym-1)==1,"ene",atodaym)
amonth3a=ifelse((atodaym-1)==2,"feb",amonth3a)
amonth3a=ifelse((atodaym-1)==3,"mar",amonth3a)
amonth3a=ifelse((atodaym-1)==4,"abr",amonth3a)
amonth3a=ifelse((atodaym-1)==5,"may",amonth3a)
amonth3a=ifelse((atodaym-1)==6,"jun",amonth3a)
amonth3a=ifelse((atodaym-1)==7,"jul",amonth3a)
amonth3a=ifelse((atodaym-1)==8,"ago",amonth3a)
amonth3a=ifelse((atodaym-1)==9,"sept",amonth3a)
amonth3a=ifelse((atodaym-1)==10,"oct",amonth3a)
amonth3a=ifelse((atodaym-1)==11,"nov",amonth3a)
amonth3a=ifelse((atodaym-1)==12,"dic",amonth3a)
amonth3a
amonth4=ifelse(atodaym==1,"ene",atodaym)
amonth4=ifelse(atodaym==2,"feb",amonth4)
amonth4=ifelse(atodaym==3,"mar",amonth4)
amonth4=ifelse(atodaym==4,"apr",amonth4)
amonth4=ifelse(atodaym==5,"may",amonth4)
amonth4=ifelse(atodaym==6,"jun",amonth4)
amonth4=ifelse(atodaym==7,"jul",amonth4)
amonth4=ifelse(atodaym==8,"aug",amonth4)
amonth4=ifelse(atodaym==9,"sep",amonth4)
amonth4=ifelse(atodaym==10,"oct",amonth4)
amonth4=ifelse(atodaym==11,"nov",amonth4)
amonth4=ifelse(atodaym==12,"dec",amonth4)
amonth4

#*########## ARGENTINA ---------------------------------------
# Reading file -----
tcargentina <- rio::import("http://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/seriese.xls", sheet = "PRESTAMOS", range = "A6:U9000")
# If dataframe exists
exists <- exists('tcargentina') && is.data.frame(get('tcargentina'))
if(exists==FALSE){ 
  tcargentina <- read_excel(paste(backup,"/old_argentina.xls",sep=""),
                            sheet = "PRESTAMOS", range = "A6:U9000")   
}
# Renombrando variables --------------------------------------------
colnames(tcargentina) # get column names
# Rename column names
names(tcargentina)[names(tcargentina) == "...1"] <- "Date"
names(tcargentina)[names(tcargentina) == "Hipotecarios...4"] <- "Mortgage1"
names(tcargentina)[names(tcargentina) == "Personales...6"] <- "ConsumerCredit1"
names(tcargentina)[names(tcargentina) == "Tarjetas...7"] <- "CreditCard1"
names(tcargentina)[names(tcargentina) == "pesos"] <- "total1"
names(tcargentina)[names(tcargentina) == "Hipotecarios...12"] <- "Mortgage2"
names(tcargentina)[names(tcargentina) == "Personales...14"] <- "ConsumerCredit2"
names(tcargentina)[names(tcargentina) == "Tarjetas...15"] <- "CreditCard2"
names(tcargentina)[names(tcargentina) == "dólares"] <- "total2"
names(tcargentina)[names(tcargentina) == "Sector Privado...19"] <- "dsprivado"
names(tcargentina)[names(tcargentina) == "Sector Privado...21"] <- "psprivado"
#Encontrando los valores de los dolares
tcargentina$dolares <- tcargentina$dsprivado/tcargentina$total2
# Cambiando "2" a pesos argentinos
tcargentina$Mortgage2 <- tcargentina$Mortgage2*tcargentina$dolares
tcargentina$CreditCard2 <- tcargentina$CreditCard2*tcargentina$dolares
tcargentina$ConsumerCredit2 <- tcargentina$ConsumerCredit2*tcargentina$dolares
tcargentina$total2 <- tcargentina$total2*tcargentina$dolares
# Agregando los resultados-------
tcargentina$Mortgage <-
  rowSums(tcargentina[, c(
    "Mortgage1",
    "Mortgage2")], na.rm = TRUE)
tcargentina$CreditCard <-
  rowSums(tcargentina[, c(
    "CreditCard1",
    "CreditCard2")], na.rm = TRUE)

tcargentina$ConsumerCredit <-
  rowSums(tcargentina[, c(
    "ConsumerCredit1",
    "ConsumerCredit2")], na.rm = TRUE)
tcargentina$Total <-
  rowSums(tcargentina[, c(
    "total1",
    "total2")], na.rm = TRUE)
#Con las variables que interesan
keeps <- c("Date",
           "Mortgage",
           "ConsumerCredit",
           "CreditCard",
           "Total")
tcargentina = tcargentina[keeps]
# Variable Date --------------------------------------------
tcargentina <- tcargentina[!(
  tcargentina$Date == "Prom.Ene.-02"
  | tcargentina$Date == "Prom.Feb.-02"
  | tcargentina$Date == "Prom.Mar.-02"
  | tcargentina$Date == "Prom.Abr.-02"
  | tcargentina$Date == "Prom.May.-02"
  | tcargentina$Date == "Prom.Jun.-02"
  | tcargentina$Date == "Prom.Jul.-02"
  | tcargentina$Date == "Prom.Ago.-02"
  | tcargentina$Date == "Prom.Sep.-02"
  | tcargentina$Date == "Prom.Oct.-02"
  | tcargentina$Date == "Prom.Nov.-02"
  | tcargentina$Date == "Prom.Dic.-02"
  | tcargentina$Date == "Prom.Ene.-03"
  | tcargentina$Date == "Prom.Feb.-03"
  | tcargentina$Date == "Prom.Mar.-03"
  | tcargentina$Date == "Prom.Abr.-03"
  | tcargentina$Date == "Prom.May.-03"
  | tcargentina$Date == "Prom.Jun.-03"
  | tcargentina$Date == "Prom.Jul.-03"
  | tcargentina$Date == "Prom.Ago.-03"
  | tcargentina$Date == "Prom.Sep.-03"
  | tcargentina$Date == "Prom.Oct.-03"
  | tcargentina$Date == "Prom.Nov.-03"
  | tcargentina$Date == "Prom.Dic.-03"
  | tcargentina$Date == "Prom.Ene.-04"
  | tcargentina$Date == "Prom.Feb.-04"
  | tcargentina$Date == "Prom.Mar.-04"
  | tcargentina$Date == "Prom.Abr.-04"
  | tcargentina$Date == "Prom.May.-04"
  | tcargentina$Date == "Prom.Jun.-04"
  | tcargentina$Date == "Prom.Jul.-04"
  | tcargentina$Date == "Prom.Ago.-04"
  | tcargentina$Date == "Prom.Sep.-04"
  | tcargentina$Date == "Prom.Oct.-04"
  | tcargentina$Date == "Prom.Nov.-04"
  | tcargentina$Date == "Prom.Dic.-04"
  | tcargentina$Date == "Prom.Ene.-05"
  | tcargentina$Date == "Prom.Feb.-05"
  | tcargentina$Date == "Prom.Mar.-05"
  | tcargentina$Date == "Prom.Abr.-05"
  | tcargentina$Date == "Prom.May.-05"
  | tcargentina$Date == "Prom.Jun.-05"
  | tcargentina$Date == "Prom.Jul.-05"
  | tcargentina$Date == "Prom.Ago.-05"
  | tcargentina$Date == "Prom.Sep.-05"
  | tcargentina$Date == "Prom.Oct.-05"
  | tcargentina$Date == "Prom.Nov.-05"
  | tcargentina$Date == "Prom.Dic.-05"
  | tcargentina$Date == "Prom.Ene.-06"
  | tcargentina$Date == "Prom.Feb.-06"
  | tcargentina$Date == "Prom.Mar.-06"
  | tcargentina$Date == "Prom.Abr.-06"
  | tcargentina$Date == "Prom.May.-06"
  | tcargentina$Date == "Prom.Jun.-06"
  | tcargentina$Date == "Prom.Jul.-06"
  | tcargentina$Date == "Prom.Ago.-06"
  | tcargentina$Date == "Prom.Sep.-06"
  | tcargentina$Date == "Prom.Oct.-06"
  | tcargentina$Date == "Prom.Nov.-06"
  | tcargentina$Date == "Prom.Dic.-06"
  | tcargentina$Date == "Prom.Ene.-07"
  | tcargentina$Date == "Prom.Feb.-07"
  | tcargentina$Date == "Prom.Mar.-07"
  | tcargentina$Date == "Prom.Abr.-07"
  | tcargentina$Date == "Prom.May.-07"
  | tcargentina$Date == "Prom.Jun.-07"
  | tcargentina$Date == "Prom.Jul.-07"
  | tcargentina$Date == "Prom.Ago.-07"
  | tcargentina$Date == "Prom.Sep.-07"
  | tcargentina$Date == "Prom.Oct.-07"
  | tcargentina$Date == "Prom.Nov.-07"
  | tcargentina$Date == "Prom.Dic.-07"
  | tcargentina$Date == "Prom.Ene.-08"
  | tcargentina$Date == "Prom.Feb.-08"
  | tcargentina$Date == "Prom.Mar.-08"
  | tcargentina$Date == "Prom.Abr.-08"
  | tcargentina$Date == "Prom.May.-08"
  | tcargentina$Date == "Prom.Jun.-08"
  | tcargentina$Date == "Prom.Jul.-08"
  | tcargentina$Date == "Prom.Ago.-08"
  | tcargentina$Date == "Prom.Sep.-08"
  | tcargentina$Date == "Prom.Oct.-08"
  | tcargentina$Date == "Prom.Nov.-08"
  | tcargentina$Date == "Prom.Dic.-08"
  | tcargentina$Date == "Prom.Ene.-09"
  | tcargentina$Date == "Prom.Feb.-09"
  | tcargentina$Date == "Prom.Mar.-09"
  | tcargentina$Date == "Prom.Abr.-09"
  | tcargentina$Date == "Prom.May.-09"
  | tcargentina$Date == "Prom.Jun.-09"
  | tcargentina$Date == "Prom.Jul.-09"
  | tcargentina$Date == "Prom.Ago.-09"
  | tcargentina$Date == "Prom.Sep.-09"
  | tcargentina$Date == "Prom.Oct.-09"
  | tcargentina$Date == "Prom.Nov.-09"
  | tcargentina$Date == "Prom.Dic.-09"
  | tcargentina$Date == "Prom.Ene.-10"
  | tcargentina$Date == "Prom.Feb.-10"
  | tcargentina$Date == "Prom.Mar.-10"
  | tcargentina$Date == "Prom.Abr.-10"
  | tcargentina$Date == "Prom.May.-10"
  | tcargentina$Date == "Prom.Jun.-10"
  | tcargentina$Date == "Prom.Jul.-10"
  | tcargentina$Date == "Prom.Ago.-10"
  | tcargentina$Date == "Prom.Sep.-10"
  | tcargentina$Date == "Prom.Oct.-10"
  | tcargentina$Date == "Prom.Nov.-10"
  | tcargentina$Date == "Prom.Dic.-10"
  | tcargentina$Date == "Prom.Ene.-11"
  | tcargentina$Date == "Prom.Feb.-11"
  | tcargentina$Date == "Prom.Mar.-11"
  | tcargentina$Date == "Prom.Abr.-11"
  | tcargentina$Date == "Prom.May.-11"
  | tcargentina$Date == "Prom.Jun.-11"
  | tcargentina$Date == "Prom.Jul.-11"
  | tcargentina$Date == "Prom.Ago.-11"
  | tcargentina$Date == "Prom.Sep.-11"
  | tcargentina$Date == "Prom.Oct.-11"
  | tcargentina$Date == "Prom.Nov.-11"
  | tcargentina$Date == "Prom.Dic.-11"
  | tcargentina$Date == "Prom.Ene.-12"
  | tcargentina$Date == "Prom.Feb.-12"
  | tcargentina$Date == "Prom.Mar.-12"
  | tcargentina$Date == "Prom.Abr.-12"
  | tcargentina$Date == "Prom.May.-12"
  | tcargentina$Date == "Prom.Jun.-12"
  | tcargentina$Date == "Prom.Jul.-12"
  | tcargentina$Date == "Prom.Ago.-12"
  | tcargentina$Date == "Prom.Sep.-12"
  | tcargentina$Date == "Prom.Oct.-12"
  | tcargentina$Date == "Prom.Nov.-12"
  | tcargentina$Date == "Prom.Dic.-12"
  | tcargentina$Date == "Prom.Ene.-13"
  | tcargentina$Date == "Prom.Feb.-13"
  | tcargentina$Date == "Prom.Mar.-13"
  | tcargentina$Date == "Prom.Abr.-13"
  | tcargentina$Date == "Prom.May.-13"
  | tcargentina$Date == "Prom.Jun.-13"
  | tcargentina$Date == "Prom.Jul.-13"
  | tcargentina$Date == "Prom.Ago.-13"
  | tcargentina$Date == "Prom.Sep.-13"
  | tcargentina$Date == "Prom.Oct.-13"
  | tcargentina$Date == "Prom.Nov.-13"
  | tcargentina$Date == "Prom.Dic.-13"
  | tcargentina$Date == "Prom.Ene.-14"
  | tcargentina$Date == "Prom.Feb.-14"
  | tcargentina$Date == "Prom.Mar.-14"
  | tcargentina$Date == "Prom.Abr.-14"
  | tcargentina$Date == "Prom.May.-14"
  | tcargentina$Date == "Prom.Jun.-14"
  | tcargentina$Date == "Prom.Jul.-14"
  | tcargentina$Date == "Prom.Ago.-14"
  | tcargentina$Date == "Prom.Sep.-14"
  | tcargentina$Date == "Prom.Oct.-14"
  | tcargentina$Date == "Prom.Nov.-14"
  | tcargentina$Date == "Prom.Dic.-14"
  | tcargentina$Date == "Prom.Ene.-15"
  | tcargentina$Date == "Prom.Feb.-15"
  | tcargentina$Date == "Prom.Mar.-15"
  | tcargentina$Date == "Prom.Abr.-15"
  | tcargentina$Date == "Prom.May.-15"
  | tcargentina$Date == "Prom.Jun.-15"
  | tcargentina$Date == "Prom.Jul.-15"
  | tcargentina$Date == "Prom.Ago.-15"
  | tcargentina$Date == "Prom.Sep.-15"
  | tcargentina$Date == "Prom.Oct.-15"
  | tcargentina$Date == "Prom.Nov.-15"
  | tcargentina$Date == "Prom.Dic.-15"
  | tcargentina$Date == "Prom.Ene.-16"
  | tcargentina$Date == "Prom.Feb.-16"
  | tcargentina$Date == "Prom.Mar.-16"
  | tcargentina$Date == "Prom.Abr.-16"
  | tcargentina$Date == "Prom.May.-16"
  | tcargentina$Date == "Prom.Jun.-16"
  | tcargentina$Date == "Prom.Jul.-16"
  | tcargentina$Date == "Prom.Ago.-16"
  | tcargentina$Date == "Prom.Sep.-16"
  | tcargentina$Date == "Prom.Oct.-16"
  | tcargentina$Date == "Prom.Nov.-16"
  | tcargentina$Date == "Prom.Dic.-16"
  | tcargentina$Date == "Prom.Ene.-17"
  | tcargentina$Date == "Prom.Feb.-17"
  | tcargentina$Date == "Prom.Mar.-17"
  | tcargentina$Date == "Prom.Abr.-17"
  | tcargentina$Date == "Prom.May.-17"
  | tcargentina$Date == "Prom.Jun.-17"
  | tcargentina$Date == "Prom.Jul.-17"
  | tcargentina$Date == "Prom.Ago.-17"
  | tcargentina$Date == "Prom.Sep.-17"
  | tcargentina$Date == "Prom.Oct.-17"
  | tcargentina$Date == "Prom.Nov.-17"
  | tcargentina$Date == "Prom.Dic.-17"
  | tcargentina$Date == "Prom.Ene.-18"
  | tcargentina$Date == "Prom.Mar.-18"
  | tcargentina$Date == "Prom.Abr.-18"
  | tcargentina$Date == "Prom.May.-18"
  | tcargentina$Date == "Prom.Jun.-18"
  | tcargentina$Date == "Prom.Jul.-18"
  | tcargentina$Date == "Prom.Ago.-18"
  | tcargentina$Date == "Prom.Sep.-18"
  | tcargentina$Date == "Prom.Oct.-18"
  | tcargentina$Date == "Prom.Nov.-18"
  | tcargentina$Date == "Prom.Dic.-18"
  | tcargentina$Date == "Prom.Ene.-19"
  | tcargentina$Date == "Prom.Feb.-19"
  | tcargentina$Date == "Prom.Mar.-19"
  | tcargentina$Date == "Prom.Abr.-19"
  | tcargentina$Date == "Prom.May.-19"
  | tcargentina$Date == "Prom.Jun.-19"
  | tcargentina$Date == "Prom.Jul.-19"
  | tcargentina$Date == "Prom.Ago.-19"
  | tcargentina$Date == "Prom.Sep.-19"
  | tcargentina$Date == "Prom.Oct.-19"
  | tcargentina$Date == "Prom.Nov.-19"
  | tcargentina$Date == "Prom.Dic.-19"
  | tcargentina$Date == "Prom.Ene.-20"
  | tcargentina$Date == "Prom.Feb.-20"
  | tcargentina$Date == "Prom.Mar.-20"
  | tcargentina$Date == "Prom.Abr.-20"
  | tcargentina$Date == "Prom.May.-20"
  | tcargentina$Date == "Prom.Jun.-20"
  | tcargentina$Date == "Prom.Jul.-20"
  | tcargentina$Date == "Prom.Ago.-20"
  | tcargentina$Date == "Prom.Sep.-20"
  | tcargentina$Date == "Prom.Oct.-20"
  | tcargentina$Date == "Prom.Nov.-20"
  | tcargentina$Date == "Prom.Dic.-20"
  | tcargentina$Date == "Prom.Ene.-21"
  | tcargentina$Date == "Prom.Feb.-21"
  | tcargentina$Date == "Prom.Mar.-21"
  | tcargentina$Date == "Prom.Abr.-21"
  | tcargentina$Date == "Prom.May.-21"
  | tcargentina$Date == "Prom.Jun.-21"
  | tcargentina$Date == "Prom.Jul.-21"
  | tcargentina$Date == "Prom.Ago.-21"
  | tcargentina$Date == "Prom.Sep.-21"
  | tcargentina$Date == "Prom.Oct.-21"
  | tcargentina$Date == "Prom.Nov.-21"
  | tcargentina$Date == "Prom.Dic.-21"
  | tcargentina$Date == "Prom.Ene.-22"
  | tcargentina$Date == "Prom.Feb.-22"
  | tcargentina$Date == "Prom.Mar.-22"
  | tcargentina$Date == "Prom.Abr.-22"
  | tcargentina$Date == "Prom.May.-22"
  | tcargentina$Date == "Prom.Jun.-22"
  | tcargentina$Date == "Prom.Jul.-22"
  ), ]
tcargentina
tcargentina %>% filter_all(all_vars(!is.na(.))) #observaciones vacias
tcargentina <-
  tcargentina %>% filter(Total >= 1000000) #solo saldo credito
#Format date
tcargentina$Date = openxlsx::convertToDate(tcargentina$Date)
# Datos mensuales ---------------------------------------
#Encontrar el ultimo dia segun mes
#Creando variables year, month and day
tcargentina$month <- format(tcargentina$Date, "%m")
tcargentina$year <- format(tcargentina$Date, "%Y")
tcargentina$day <- format(tcargentina$Date, "%D")
tcargentina$my <-
  format(as.Date(tcargentina$Date, format = "%Y-%m-%d"), "%Y-%m")
#Encontrando el valor maximo
tcargentina$maxvalue <-
  ave(tcargentina$Date, tcargentina$my, FUN = max)
#Mantener los valores finales
tcargentina <- filter(tcargentina, maxvalue == Date)
keeps4 <- c("Date", "year",
            "CreditCard",
            "ConsumerCredit",
            "Mortgage",
            "Total")
tcargentina = tcargentina[keeps4]
#Variable pais
tcargentina$Pais <- "Argentina"
# Eliminando desde cierta fecha ------
tcargentina <- tcargentina[tcargentina$Date >= "2018-01-01",]
# Variables para el panel----------
tcargentina$Microcredit <- NA
tcargentina$SMEs <- NA
tcargentina$BusinessCredit <- NA
tcargentina$Leasing <- NA
tcargentina$CommercialCredit <- NA
tcargentina$Government <- NA
tcargentina$PersonalCredit <- NA
str(tcargentina)
# if dataframe !exist-------------
exists <- exists('tcargentina') && is.data.frame(get('tcargentina'))
if (exists==FALSE){
  tcargentina <- read.csv(paste(backup,"/old_tcargentina.csv",sep=""))
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcargentina,paste(backup,"old_tcargentina.csv",sep="/"),
  row.names = FALSE
)





#*########## BOLIVIA ---------------------------------------
# Reading the file ---------------------------------------------
####rm(list = ls())
tcbol <-
  read_excel(paste(nauto,"/FinancialSystemBolivia.xlsx",sep="") ,
             sheet = "TypeCredit",
             range = "A2:I1900")
# If dataframe exists
exists <- exists('tcbol') && is.data.frame(get('tcbol'))
if(exists==FALSE){ 
  tcbol <- read_excel(paste(backup,"/old_bolivia.xls",sep=""),
                      sheet = "TypeCredit",
                      range = "A2:I1900")  
}
# Renombrando variables --------------------------------------------
colnames(tcbol) # get column names
# Rename column names
names(tcbol)[names(tcbol) == "...1"] <- "Date"
names(tcbol)[names(tcbol) == "Empresarial"] <- "BusinessCredit"
names(tcbol)[names(tcbol) == "Pyme"] <- "SMEs"
names(tcbol)[names(tcbol) == "Microcredito"] <- "Microcredit"
names(tcbol)[names(tcbol) == "Hipotecaria"] <- "Mortgage"
names(tcbol)[names(tcbol) == "Hipotecaria de vivienda social"] <-
  "chiposocial"
names(tcbol)[names(tcbol) == "Hipotecaria (no social)"] <-
  "chiponosocial"
names(tcbol)[names(tcbol) == "Consumo"] <- "ConsumerCredit"
names(tcbol)[names(tcbol) == "Total"] <- "Total"
# Con las variables que interesan ---------------------------------------------
keeps <- c("Date",
           "BusinessCredit",
           "SMEs",
           "Microcredit",
           "Mortgage",
           "ConsumerCredit",
           "Total")
tcbolivia = tcbol[keeps]
# Variable Date --------------------------------------------
tcbolivia$Date <- as.Date(tcbolivia$Date, "%Y/%m/%d")
class(tcbolivia$Date)
#Variable pais
tcbolivia$Pais <- "Bolivia"
# Eliminando desde cierta Date ------
tcbolivia <-
  tcbolivia[tcbolivia$Date <= paste(year2,"-",(atodaym),"-","01",sep=""), ]
# Eliminando valores vacios ----------------------------------
tcbolivia <- tcbolivia %>% drop_na(Date)
# Variables para el panel
tcbolivia$Leasing <- NA
tcbolivia$Government <- NA
tcbolivia$PersonalCredit <- NA
tcbolivia$CreditCard <- NA
tcbolivia$CommercialCredit <- NA
#
tcbolivia$year <- format(tcbolivia$Date, "%Y")
str(tcbolivia)
# if dataframe !exist-------------
exists <- exists('tcbolivia') && is.data.frame(get('tcbolivia'))
if (exists==FALSE){
  tcbolivia <- read.csv(paste(backup,"/old_tcbolivia.csv",sep=""))
}else{break}


# Exportando --------------------------------------------
write.csv(
  tcbolivia,paste(backup,"old_tcbolivia.csv",sep="/"),
  row.names = FALSE
)







#*########## BRAZIL ---------------------------------------
# Trabajando con el ultimo dato disponible--------
brasilu <- paste(raw,"Brasilu.xlsx",sep="/")
#if (file.exists(brasilu)){
# Tabel 5 ---------------------------------------
tcbrat5 <-
  read_excel(brasilu, sheet = "Tabela 5", range = "A12:L1000")
# analyzing
colnames(tcbrat5)
# Rename column names
names(tcbrat5)[names(tcbrat5) == "...1"] <- "year"
names(tcbrat5)[names(tcbrat5) == "...2"] <- "m"
names(tcbrat5)[names(tcbrat5) == "...3"] <- "n"
names(tcbrat5)[names(tcbrat5) == "...4"] <- "PJ.desc.cuentas.cobrar"
names(tcbrat5)[names(tcbrat5) == "...5"] <- "PJ.desc.cheques"
names(tcbrat5)[names(tcbrat5) == "...6"] <- "PJ.ant.fac.tarjetas"
names(tcbrat5)[names(tcbrat5) == "...7"] <- "PJ.cap.trabajo365"
names(tcbrat5)[names(tcbrat5) == "...8"] <- "PJ.cap.trabajo+365"
names(tcbrat5)[names(tcbrat5) == "...9"] <- "PJ.techo.girat"
names(tcbrat5)[names(tcbrat5) == "...10"] <- "PJ.capital.trabajoT"
names(tcbrat5)[names(tcbrat5) == "...11"] <- "PJ.cta.garantizado"
names(tcbrat5)[names(tcbrat5) == "...12"] <- "PJ.cheque.especial"
tcbrat5

#Eliminando empty values
#tcbrat5 <- tcbrat5[!is.na(tcbrat5$techo.girat),]
tcbrat5 <- tcbrat5[!is.na(tcbrat5$m),]

#Con las variables que interesan
keeps <- c("year",
           "m",
           "PJ.desc.cuentas.cobrar",
           "PJ.desc.cheques",
           "PJ.ant.fac.tarjetas",
           "PJ.cap.trabajo365",
           "PJ.cap.trabajo+365",
           "PJ.techo.girat",
           "PJ.capital.trabajoT",
           "PJ.cta.garantizado",
           "PJ.cheque.especial"
)
tcbrasilt5 = tcbrat5[keeps]

#Variable mes numeral
tcbrasilt5$id[tcbrasilt5$m == "Jan"] <- c(1)
tcbrasilt5$id[tcbrasilt5$m == "Fev"] <- c(2)
tcbrasilt5$id[tcbrasilt5$m == "Mar"] <- c(3)
tcbrasilt5$id[tcbrasilt5$m == "Abr"] <- c(4)
tcbrasilt5$id[tcbrasilt5$m == "Mai"] <- c(5)
tcbrasilt5$id[tcbrasilt5$m == "Jun"] <- c(6)
tcbrasilt5$id[tcbrasilt5$m == "Jul"] <- c(7)
tcbrasilt5$id[tcbrasilt5$m == "Ago"] <- c(8)
tcbrasilt5$id[tcbrasilt5$m == "Set"] <- c(9)
tcbrasilt5$id[tcbrasilt5$m == "Out"] <- c(10)
tcbrasilt5$id[tcbrasilt5$m == "Nov"] <- c(11)
tcbrasilt5$id[tcbrasilt5$m == "Dez"] <- c(12)

tcbrasilt5$month <- ""
tcbrasilt5$month[tcbrasilt5$m == "Jan"] <- c("Jan")
tcbrasilt5$month[tcbrasilt5$m == "Fev"] <- c("Feb")
tcbrasilt5$month[tcbrasilt5$m == "Mar"] <- c("Mar")
tcbrasilt5$month[tcbrasilt5$m == "Abr"] <- c("Apr")
tcbrasilt5$month[tcbrasilt5$m == "Mai"] <- c("May")
tcbrasilt5$month[tcbrasilt5$m == "Jun"] <- c("Jun")
tcbrasilt5$month[tcbrasilt5$m == "Jul"] <- c("Jul")
tcbrasilt5$month[tcbrasilt5$m == "Ago"] <- c("Aug")
tcbrasilt5$month[tcbrasilt5$m == "Set"] <- c("Sep")
tcbrasilt5$month[tcbrasilt5$m == "Out"] <- c("Oct")
tcbrasilt5$month[tcbrasilt5$m == "Nov"] <- c("Nov")
tcbrasilt5$month[tcbrasilt5$m == "Dez"] <- c("Dec")
#Variable anho
tcbrasilt5 <- tcbrasilt5 %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt5$date <-
  lubridate::ymd(paste0(tcbrasilt5$year, tcbrasilt5$month, "01"))
#Nos quedamos con las variables que nos importan
keeps2 <-
  c(
    "date",
    "year",
    "month",
    #"id.t",
    #"id",
    "PJ.desc.cuentas.cobrar",
    "PJ.desc.cheques",
    "PJ.ant.fac.tarjetas",
    "PJ.cap.trabajo365",
    "PJ.cap.trabajo+365",
    "PJ.techo.girat",
    "PJ.capital.trabajoT",
    "PJ.cta.garantizado",
    "PJ.cheque.especial"
  )
tcbrasilt5 = tcbrasilt5[keeps2]

# Tabel 5A ---------------------------------------
######rm(list = ls())
tcbrat5a <-
  read_excel(brasilu, sheet = "Tabela 5A", range = "A12:O1000")
# analyzing
colnames(tcbrat5a)
# Rename column names
names(tcbrat5a)[names(tcbrat5a) == "...1"] <- "year"
names(tcbrat5a)[names(tcbrat5a) == "...2"] <- "m"
names(tcbrat5a)[names(tcbrat5a) == "...3"] <- "n"
names(tcbrat5a)[names(tcbrat5a) == "...4"] <- "PJ.adq.vehiculos"
names(tcbrat5a)[names(tcbrat5a) == "...5"] <- "PJ.adq.obienes"
names(tcbrat5a)[names(tcbrat5a) == "...6"] <- "PJ.arrend.vehiculos"
names(tcbrat5a)[names(tcbrat5a) == "...7"] <- "PJ.arrend.obienes"
names(tcbrat5a)[names(tcbrat5a) == "...8"] <- "PJ.arrend.total"
names(tcbrat5a)[names(tcbrat5a) == "...9"] <- "PJ.vendedor"
names(tcbrat5a)[names(tcbrat5a) == "...10"] <- "PJ.comprador"
names(tcbrat5a)[names(tcbrat5a) == "...11"] <- "n2"
names(tcbrat5a)[names(tcbrat5a) == "...12"] <- "n3"
tcbrat5a

#Eliminando empty values
#tcbrat5a <- tcbrat5a[!is.na(tcbrat5a$vendedor), ]
tcbrat5a <- tcbrat5a[!is.na(tcbrat5a$m),]

#Con las variables que interesan
colnames(tcbrat5a)

keeps <- c("year",
           "m",
           "PJ.adq.vehiculos",
           "PJ.adq.obienes",
           "PJ.arrend.vehiculos",
           "PJ.arrend.obienes",
           "PJ.arrend.total",
           "PJ.vendedor",
           "PJ.comprador"
)
tcbrasilt5a = tcbrat5a[keeps]

#Variable mes numeral
tcbrasilt5a$id <- ""
tcbrasilt5a$month <- ""
tcbrasilt5a$id[tcbrasilt5a$m == "Jan"] <- c(1)
tcbrasilt5a$id[tcbrasilt5a$m == "Fev"] <- c(2)
tcbrasilt5a$id[tcbrasilt5a$m == "Mar"] <- c(3)
tcbrasilt5a$id[tcbrasilt5a$m == "Abr"] <- c(4)
tcbrasilt5a$id[tcbrasilt5a$m == "Mai"] <- c(5)
tcbrasilt5a$id[tcbrasilt5a$m == "Jun"] <- c(6)
tcbrasilt5a$id[tcbrasilt5a$m == "Jul"] <- c(7)
tcbrasilt5a$id[tcbrasilt5a$m == "Ago"] <- c(8)
tcbrasilt5a$id[tcbrasilt5a$m == "Set"] <- c(9)
tcbrasilt5a$id[tcbrasilt5a$m == "Out"] <- c(10)
tcbrasilt5a$id[tcbrasilt5a$m == "Nov"] <- c(11)
tcbrasilt5a$id[tcbrasilt5a$m == "Dez"] <- c(12)

tcbrasilt5a$month[tcbrasilt5a$m == "Jan"] <- c("Jan")
tcbrasilt5a$month[tcbrasilt5a$m == "Fev"] <- c("Feb")
tcbrasilt5a$month[tcbrasilt5a$m == "Mar"] <- c("Mar")
tcbrasilt5a$month[tcbrasilt5a$m == "Abr"] <- c("Apr")
tcbrasilt5a$month[tcbrasilt5a$m == "Mai"] <- c("May")
tcbrasilt5a$month[tcbrasilt5a$m == "Jun"] <- c("Jun")
tcbrasilt5a$month[tcbrasilt5a$m == "Jul"] <- c("Jul")
tcbrasilt5a$month[tcbrasilt5a$m == "Ago"] <- c("Aug")
tcbrasilt5a$month[tcbrasilt5a$m == "Set"] <- c("Sep")
tcbrasilt5a$month[tcbrasilt5a$m == "Out"] <- c("Oct")
tcbrasilt5a$month[tcbrasilt5a$m == "Nov"] <- c("Nov")
tcbrasilt5a$month[tcbrasilt5a$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt5a <- tcbrasilt5a %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt5a$date <-
  format(lubridate::ymd(paste0(
    tcbrasilt5a$year, tcbrasilt5a$month, "01"
  )), "%m-%Y")
colnames(tcbrasilt5a)
#Nos quedamos con las variables que nos importan
keeps2 <- c(
  "PJ.adq.vehiculos",
  "PJ.adq.obienes",
  "PJ.arrend.vehiculos",
  "PJ.arrend.obienes",
  "PJ.arrend.total",
  "PJ.vendedor",
  "PJ.comprador"
)
tcbrasilt5a = tcbrasilt5a[keeps2]

# Tabel 5B ---------------------------------------
######rm(list = ls())
tcbrat5b <-
  read_excel(brasilu, sheet = "Tabela 5B", range = "A12:O1000")
# analyzing
colnames(tcbrat5b)
# Rename column names
names(tcbrat5b)[names(tcbrat5b) == "...1"] <- "year"
names(tcbrat5b)[names(tcbrat5b) == "...2"] <- "m"
names(tcbrat5b)[names(tcbrat5b) == "...3"] <- "n"
names(tcbrat5b)[names(tcbrat5b) == "...4"] <-
  "PJ.tarjcredito.portpar"
names(tcbrat5b)[names(tcbrat5b) == "...5"] <-
  "PJ.tarjcredito.alavista"
names(tcbrat5b)[names(tcbrat5b) == "...6"] <- "PJ.tarjcredito.total"
names(tcbrat5b)[names(tcbrat5b) == "...7"] <- "PJ.acc"
names(tcbrat5b)[names(tcbrat5b) == "...8"] <- "PJ.finan.import"
names(tcbrat5b)[names(tcbrat5b) == "...9"] <- "PJ.finan.export"
names(tcbrat5b)[names(tcbrat5b) == "...10"] <- "PJ.trans.exterior"
names(tcbrat5b)[names(tcbrat5b) == "...11"] <- "PJ.otrost5"
names(tcbrat5b)[names(tcbrat5b) == "...12"] <- "PJ.totalt5"
tcbrat5b

#Eliminando empty values
#tcbrat5b <- tcbrat5b[!is.na(tcbrat5b$vendedor), ]
tcbrat5b <- tcbrat5b[!is.na(tcbrat5b$m),]

#Con las variables que interesan
colnames(tcbrat5b)

keeps <- c("year",
           "m",
           "PJ.tarjcredito.portpar",
           "PJ.tarjcredito.alavista",
           "PJ.tarjcredito.total",
           "PJ.acc",
           "PJ.finan.import",
           "PJ.finan.export",
           "PJ.trans.exterior",
           "PJ.otrost5" ,
           "PJ.totalt5"
)
tcbrasilt5b = tcbrat5b[keeps]

#Variable mes numeral
tcbrasilt5b$id <- ""
tcbrasilt5b$month <- ""
tcbrasilt5b$id[tcbrasilt5b$m == "Jan"] <- c(1)
tcbrasilt5b$id[tcbrasilt5b$m == "Fev"] <- c(2)
tcbrasilt5b$id[tcbrasilt5b$m == "Mar"] <- c(3)
tcbrasilt5b$id[tcbrasilt5b$m == "Abr"] <- c(4)
tcbrasilt5b$id[tcbrasilt5b$m == "Mai"] <- c(5)
tcbrasilt5b$id[tcbrasilt5b$m == "Jun"] <- c(6)
tcbrasilt5b$id[tcbrasilt5b$m == "Jul"] <- c(7)
tcbrasilt5b$id[tcbrasilt5b$m == "Ago"] <- c(8)
tcbrasilt5b$id[tcbrasilt5b$m == "Set"] <- c(9)
tcbrasilt5b$id[tcbrasilt5b$m == "Out"] <- c(10)
tcbrasilt5b$id[tcbrasilt5b$m == "Nov"] <- c(11)
tcbrasilt5b$id[tcbrasilt5b$m == "Dez"] <- c(12)

tcbrasilt5b$month[tcbrasilt5b$m == "Jan"] <- c("Jan")
tcbrasilt5b$month[tcbrasilt5b$m == "Fev"] <- c("Feb")
tcbrasilt5b$month[tcbrasilt5b$m == "Mar"] <- c("Mar")
tcbrasilt5b$month[tcbrasilt5b$m == "Abr"] <- c("Apr")
tcbrasilt5b$month[tcbrasilt5b$m == "Mai"] <- c("May")
tcbrasilt5b$month[tcbrasilt5b$m == "Jun"] <- c("Jun")
tcbrasilt5b$month[tcbrasilt5b$m == "Jul"] <- c("Jul")
tcbrasilt5b$month[tcbrasilt5b$m == "Ago"] <- c("Aug")
tcbrasilt5b$month[tcbrasilt5b$m == "Set"] <- c("Sep")
tcbrasilt5b$month[tcbrasilt5b$m == "Out"] <- c("Oct")
tcbrasilt5b$month[tcbrasilt5b$m == "Nov"] <- c("Nov")
tcbrasilt5b$month[tcbrasilt5b$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt5b <- tcbrasilt5b %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt5b$date <-
  format(lubridate::ymd(paste0(
    tcbrasilt5b$year, tcbrasilt5b$month, "01"
  )), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt5b)
keeps2 <- c(
  "PJ.tarjcredito.portpar",
  "PJ.tarjcredito.alavista",
  "PJ.tarjcredito.total",
  "PJ.acc",
  "PJ.finan.import",
  "PJ.finan.export",
  "PJ.trans.exterior" ,
  "PJ.otrost5",
  "PJ.totalt5"
)
tcbrasilt5b = tcbrasilt5b[keeps2]

# Tabel 6 ---------------------------------------
######rm(list = ls())
tcbrat6 <-
  read_excel(brasilu, sheet = "Tabela 6", range = "A14:O1000")
# analyzing
colnames(tcbrat6)
# Rename column names
names(tcbrat6)[names(tcbrat6) == "...1"] <- "year"
names(tcbrat6)[names(tcbrat6) == "...2"] <- "m"
names(tcbrat6)[names(tcbrat6) == "...3"] <- "n"
names(tcbrat6)[names(tcbrat6) == "...4"] <- "cheque.esp"
names(tcbrat6)[names(tcbrat6) == "...5"] <-
  "cred.PersonalCredit.nconsig"
names(tcbrat6)[names(tcbrat6) == "...6"] <-
  "cred.PersonalCredit.consig.servpub"
names(tcbrat6)[names(tcbrat6) == "...7"] <-
  "cred.PersonalCredit.consig.sp"
names(tcbrat6)[names(tcbrat6) == "...8"] <-
  "cred.PersonalCredit.consig.binss"
names(tcbrat6)[names(tcbrat6) == "...9"] <-
  "cred.PersonalCredit.consig.total"
names(tcbrat6)[names(tcbrat6) == "...10"] <-
  "cred.PersonalCredit.total"
names(tcbrat6)[names(tcbrat6) == "...11"] <- "n"
tcbrat6

#Eliminando empty values
#tcbrat6 <- tcbrat6[!is.na(tcbrat6$vendedor), ]
tcbrat6 <- tcbrat6[!is.na(tcbrat6$m),]

#Con las variables que interesan
colnames(tcbrat6)

keeps <- c("year",
           "m",
           "cheque.esp",
           "cred.PersonalCredit.nconsig",
           "cred.PersonalCredit.consig.servpub",
           "cred.PersonalCredit.consig.sp",
           "cred.PersonalCredit.consig.binss",
           "cred.PersonalCredit.consig.total",
           "cred.PersonalCredit.total"
)
tcbrasilt6 = tcbrat6[keeps]

#Variable mes numeral
tcbrasilt6$id <- ""
tcbrasilt6$month <- ""
tcbrasilt6$id[tcbrasilt6$m == "Jan"] <- c(1)
tcbrasilt6$id[tcbrasilt6$m == "Fev"] <- c(2)
tcbrasilt6$id[tcbrasilt6$m == "Mar"] <- c(3)
tcbrasilt6$id[tcbrasilt6$m == "Abr"] <- c(4)
tcbrasilt6$id[tcbrasilt6$m == "Mai"] <- c(5)
tcbrasilt6$id[tcbrasilt6$m == "Jun"] <- c(6)
tcbrasilt6$id[tcbrasilt6$m == "Jul"] <- c(7)
tcbrasilt6$id[tcbrasilt6$m == "Ago"] <- c(8)
tcbrasilt6$id[tcbrasilt6$m == "Set"] <- c(9)
tcbrasilt6$id[tcbrasilt6$m == "Out"] <- c(10)
tcbrasilt6$id[tcbrasilt6$m == "Nov"] <- c(11)
tcbrasilt6$id[tcbrasilt6$m == "Dez"] <- c(12)

tcbrasilt6$month[tcbrasilt6$m == "Jan"] <- c("Jan")
tcbrasilt6$month[tcbrasilt6$m == "Fev"] <- c("Feb")
tcbrasilt6$month[tcbrasilt6$m == "Mar"] <- c("Mar")
tcbrasilt6$month[tcbrasilt6$m == "Abr"] <- c("Apr")
tcbrasilt6$month[tcbrasilt6$m == "Mai"] <- c("May")
tcbrasilt6$month[tcbrasilt6$m == "Jun"] <- c("Jun")
tcbrasilt6$month[tcbrasilt6$m == "Jul"] <- c("Jul")
tcbrasilt6$month[tcbrasilt6$m == "Ago"] <- c("Aug")
tcbrasilt6$month[tcbrasilt6$m == "Set"] <- c("Sep")
tcbrasilt6$month[tcbrasilt6$m == "Out"] <- c("Oct")
tcbrasilt6$month[tcbrasilt6$m == "Nov"] <- c("Nov")
tcbrasilt6$month[tcbrasilt6$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt6 <- tcbrasilt6 %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt6$date <-
  format(lubridate::ymd(paste0(tcbrasilt6$year, tcbrasilt6$month, "01")), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt6)
keeps2 <- c(
  "cheque.esp",
  "cred.PersonalCredit.nconsig",
  "cred.PersonalCredit.consig.servpub",
  "cred.PersonalCredit.consig.sp",
  "cred.PersonalCredit.consig.binss",
  "cred.PersonalCredit.consig.total",
  "cred.PersonalCredit.total"
)
tcbrasilt6 = tcbrasilt6[keeps2]

# Tabel 6A ---------------------------------------
######rm(list = ls())
tcbrat6a <-
  read_excel(brasilu, sheet = "Tabela 6A", range = "A14:O1000")
# analyzing
colnames(tcbrat6a)
# Rename column names
names(tcbrat6a)[names(tcbrat6a) == "...1"] <- "year"
names(tcbrat6a)[names(tcbrat6a) == "...2"] <- "m"
names(tcbrat6a)[names(tcbrat6a) == "...3"] <- "n"
names(tcbrat6a)[names(tcbrat6a) == "...4"] <- "adq.vehiculos"
names(tcbrat6a)[names(tcbrat6a) == "...5"] <- "adq.obienes"
names(tcbrat6a)[names(tcbrat6a) == "...6"] <-
  "tcredito.gira.regular"
names(tcbrat6a)[names(tcbrat6a) == "...7"] <-
  "tcredito.gira.irregular"
names(tcbrat6a)[names(tcbrat6a) == "...8"] <- "tcredito.gira.total"
names(tcbrat6a)[names(tcbrat6a) == "...9"] <- "tcredito.entrega"
names(tcbrat6a)[names(tcbrat6a) == "...10"] <- "tcredito.alavista"
names(tcbrat6a)[names(tcbrat6a) == "...11"] <- "tcredito.total"
names(tcbrat6a)[names(tcbrat6a) == "...12"] <- "n"
tcbrat6a

#Eliminando empty values
#tcbrat6a <- tcbrat6a[!is.na(tcbrat6a$vendedor), ]
tcbrat6a <- tcbrat6a[!is.na(tcbrat6a$m),]

#Con las variables que interesan
colnames(tcbrat6a)

keeps <- c("year",
           "m",
           "adq.vehiculos",
           "adq.obienes",
           "tcredito.gira.regular",
           "tcredito.gira.irregular",
           "tcredito.gira.total",
           "tcredito.entrega",
           "tcredito.alavista",
           "tcredito.total"
)
tcbrasilt6a = tcbrat6a[keeps]

#Variable mes numeral
tcbrasilt6a$id <- ""
tcbrasilt6a$month <- ""
tcbrasilt6a$id[tcbrasilt6a$m == "Jan"] <- c(1)
tcbrasilt6a$id[tcbrasilt6a$m == "Fev"] <- c(2)
tcbrasilt6a$id[tcbrasilt6a$m == "Mar"] <- c(3)
tcbrasilt6a$id[tcbrasilt6a$m == "Abr"] <- c(4)
tcbrasilt6a$id[tcbrasilt6a$m == "Mai"] <- c(5)
tcbrasilt6a$id[tcbrasilt6a$m == "Jun"] <- c(6)
tcbrasilt6a$id[tcbrasilt6a$m == "Jul"] <- c(7)
tcbrasilt6a$id[tcbrasilt6a$m == "Ago"] <- c(8)
tcbrasilt6a$id[tcbrasilt6a$m == "Set"] <- c(9)
tcbrasilt6a$id[tcbrasilt6a$m == "Out"] <- c(10)
tcbrasilt6a$id[tcbrasilt6a$m == "Nov"] <- c(11)
tcbrasilt6a$id[tcbrasilt6a$m == "Dez"] <- c(12)

tcbrasilt6a$month[tcbrasilt6a$m == "Jan"] <- c("Jan")
tcbrasilt6a$month[tcbrasilt6a$m == "Fev"] <- c("Feb")
tcbrasilt6a$month[tcbrasilt6a$m == "Mar"] <- c("Mar")
tcbrasilt6a$month[tcbrasilt6a$m == "Abr"] <- c("Apr")
tcbrasilt6a$month[tcbrasilt6a$m == "Mai"] <- c("May")
tcbrasilt6a$month[tcbrasilt6a$m == "Jun"] <- c("Jun")
tcbrasilt6a$month[tcbrasilt6a$m == "Jul"] <- c("Jul")
tcbrasilt6a$month[tcbrasilt6a$m == "Ago"] <- c("Aug")
tcbrasilt6a$month[tcbrasilt6a$m == "Set"] <- c("Sep")
tcbrasilt6a$month[tcbrasilt6a$m == "Out"] <- c("Oct")
tcbrasilt6a$month[tcbrasilt6a$m == "Nov"] <- c("Nov")
tcbrasilt6a$month[tcbrasilt6a$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt6a <- tcbrasilt6a %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt6a$date <-
  format(lubridate::ymd(paste0(
    tcbrasilt6a$year, tcbrasilt6a$month, "01"
  )), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt6a)
keeps2 <- c(
  "adq.vehiculos",
  "adq.obienes",
  "tcredito.gira.regular",
  "tcredito.gira.irregular",
  "tcredito.gira.total",
  "tcredito.entrega",
  "tcredito.alavista",
  "tcredito.total"
)
tcbrasilt6a = tcbrasilt6a[keeps2]

# Tabel 6B ---------------------------------------
######rm(list = ls())
tcbrat6b <-
  read_excel(brasilu, sheet = "Tabela 6B", range = "A12:O1000")
# analyzing
colnames(tcbrat6b)
# Rename column names
names(tcbrat6b)[names(tcbrat6b) == "...1"] <- "year"
names(tcbrat6b)[names(tcbrat6b) == "...2"] <- "m"
names(tcbrat6b)[names(tcbrat6b) == "...3"] <- "n"
names(tcbrat6b)[names(tcbrat6b) == "...4"] <- "arrend.vehiculos"
names(tcbrat6b)[names(tcbrat6b) == "...5"] <- "arrend.obienes"
names(tcbrat6b)[names(tcbrat6b) == "...6"] <- "arrend.total"
names(tcbrat6b)[names(tcbrat6b) == "...7"] <- "desc.cheques"
names(tcbrat6b)[names(tcbrat6b) == "...8"] <- "comp.deudas"
names(tcbrat6b)[names(tcbrat6b) == "...9"] <- "otrost6"
names(tcbrat6b)[names(tcbrat6b) == "...10"] <- "total.ngiratorio"
names(tcbrat6b)[names(tcbrat6b) == "...11"] <- "total.rotativo"
names(tcbrat6b)[names(tcbrat6b) == "...12"] <- "total"
tcbrat6b

#Eliminando empty values
#tcbrat6b <- tcbrat6b[!is.na(tcbrat6b$vendedor), ]
tcbrat6b <- tcbrat6b[!is.na(tcbrat6b$m),]

#Con las variables que interesan
colnames(tcbrat6b)

keeps <- c("year",
           "m",
           "arrend.vehiculos",
           "arrend.obienes",
           "arrend.total",
           "desc.cheques",
           "comp.deudas",
           "otrost6",
           "total.ngiratorio",
           "total.rotativo",
           "total"
)
tcbrasilt6b = tcbrat6b[keeps]

#Variable mes numeral
tcbrasilt6b$id <- ""
tcbrasilt6b$month <- ""
tcbrasilt6b$id[tcbrasilt6b$m == "Jan"] <- c(1)
tcbrasilt6b$id[tcbrasilt6b$m == "Fev"] <- c(2)
tcbrasilt6b$id[tcbrasilt6b$m == "Mar"] <- c(3)
tcbrasilt6b$id[tcbrasilt6b$m == "Abr"] <- c(4)
tcbrasilt6b$id[tcbrasilt6b$m == "Mai"] <- c(5)
tcbrasilt6b$id[tcbrasilt6b$m == "Jun"] <- c(6)
tcbrasilt6b$id[tcbrasilt6b$m == "Jul"] <- c(7)
tcbrasilt6b$id[tcbrasilt6b$m == "Ago"] <- c(8)
tcbrasilt6b$id[tcbrasilt6b$m == "Set"] <- c(9)
tcbrasilt6b$id[tcbrasilt6b$m == "Out"] <- c(10)
tcbrasilt6b$id[tcbrasilt6b$m == "Nov"] <- c(11)
tcbrasilt6b$id[tcbrasilt6b$m == "Dez"] <- c(12)

tcbrasilt6b$month[tcbrasilt6b$m == "Jan"] <- c("Jan")
tcbrasilt6b$month[tcbrasilt6b$m == "Fev"] <- c("Feb")
tcbrasilt6b$month[tcbrasilt6b$m == "Mar"] <- c("Mar")
tcbrasilt6b$month[tcbrasilt6b$m == "Abr"] <- c("Apr")
tcbrasilt6b$month[tcbrasilt6b$m == "Mai"] <- c("May")
tcbrasilt6b$month[tcbrasilt6b$m == "Jun"] <- c("Jun")
tcbrasilt6b$month[tcbrasilt6b$m == "Jul"] <- c("Jul")
tcbrasilt6b$month[tcbrasilt6b$m == "Ago"] <- c("Aug")
tcbrasilt6b$month[tcbrasilt6b$m == "Set"] <- c("Sep")
tcbrasilt6b$month[tcbrasilt6b$m == "Out"] <- c("Oct")
tcbrasilt6b$month[tcbrasilt6b$m == "Nov"] <- c("Nov")
tcbrasilt6b$month[tcbrasilt6b$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt6b <- tcbrasilt6b %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt6b$date <-
  format(lubridate::ymd(paste0(
    tcbrasilt6b$year, tcbrasilt6b$month, "01"
  )), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt6b)
keeps2 <- c(
  "arrend.vehiculos",
  "arrend.obienes",
  "arrend.total",
  "desc.cheques",
  "comp.deudas",
  "otrost6",
  "total.ngiratorio",
  "total.rotativo",
  "total"
)
tcbrasilt6b = tcbrasilt6b[keeps2]

# Tabel 7 ---------------------------------------
######rm(list = ls())
tcbrat7 <-
  read_excel(brasilu, sheet = "Tabela 7", range = "A12:O1000")
# analyzing
colnames(tcbrat7)
# Rename column names
names(tcbrat7)[names(tcbrat7) == "...1"] <- "year"
names(tcbrat7)[names(tcbrat7) == "...2"] <- "m"
names(tcbrat7)[names(tcbrat7) == "...3"] <- "n"
names(tcbrat7)[names(tcbrat7) == "...4"] <- "PJ.crural.imp.merc"
names(tcbrat7)[names(tcbrat7) == "...5"] <- "PJ.crural.tarifa.reg"
names(tcbrat7)[names(tcbrat7) == "...6"] <- "PJ.crural.total"
names(tcbrat7)[names(tcbrat7) == "...7"] <-
  "PJ.financ.inmob.imp.merc"
names(tcbrat7)[names(tcbrat7) == "...8"] <-
  "PJ.financ.inmob.tarifa.reg"
names(tcbrat7)[names(tcbrat7) == "...9"] <- "PJ.financ.inmob.total"
names(tcbrat7)[names(tcbrat7) == "...10"] <-
  "PJ.cred.rec.bndes.kgirat"
names(tcbrat7)[names(tcbrat7) == "...11"] <-
  "PJ.cred.rec.bndes.finver"
names(tcbrat7)[names(tcbrat7) == "...12"] <-
  "PJ.cred.rec.bndes.fagro"
names(tcbrat7)[names(tcbrat7) == "...13"] <-
  "PJ.cred.rec.bndes.total"
names(tcbrat7)[names(tcbrat7) == "...14"] <- "PJ.otros"
names(tcbrat7)[names(tcbrat7) == "...15"] <- "PJ.total"
tcbrat7

#Eliminando empty values
#tcbrat7 <- tcbrat7[!is.na(tcbrat7$vendedor), ]
tcbrat7 <- tcbrat7[!is.na(tcbrat7$m),]

#Con las variables que interesan
colnames(tcbrat7)

keeps <- c("year",
           "m",
           "PJ.crural.imp.merc",
           "PJ.crural.tarifa.reg",
           "PJ.crural.total",
           "PJ.financ.inmob.imp.merc",
           "PJ.financ.inmob.tarifa.reg",
           "PJ.financ.inmob.total",
           "PJ.cred.rec.bndes.kgirat",
           "PJ.cred.rec.bndes.finver",
           "PJ.cred.rec.bndes.fagro",
           "PJ.cred.rec.bndes.total",
           "PJ.otros",
           "PJ.total"
)
tcbrasilt7 = tcbrat7[keeps]

#Variable mes numeral
tcbrasilt7$id <- ""
tcbrasilt7$month <- ""
tcbrasilt7$id[tcbrasilt7$m == "Jan"] <- c(1)
tcbrasilt7$id[tcbrasilt7$m == "Fev"] <- c(2)
tcbrasilt7$id[tcbrasilt7$m == "Mar"] <- c(3)
tcbrasilt7$id[tcbrasilt7$m == "Abr"] <- c(4)
tcbrasilt7$id[tcbrasilt7$m == "Mai"] <- c(5)
tcbrasilt7$id[tcbrasilt7$m == "Jun"] <- c(6)
tcbrasilt7$id[tcbrasilt7$m == "Jul"] <- c(7)
tcbrasilt7$id[tcbrasilt7$m == "Ago"] <- c(8)
tcbrasilt7$id[tcbrasilt7$m == "Set"] <- c(9)
tcbrasilt7$id[tcbrasilt7$m == "Out"] <- c(10)
tcbrasilt7$id[tcbrasilt7$m == "Nov"] <- c(11)
tcbrasilt7$id[tcbrasilt7$m == "Dez"] <- c(12)

tcbrasilt7$month[tcbrasilt7$m == "Jan"] <- c("Jan")
tcbrasilt7$month[tcbrasilt7$m == "Fev"] <- c("Feb")
tcbrasilt7$month[tcbrasilt7$m == "Mar"] <- c("Mar")
tcbrasilt7$month[tcbrasilt7$m == "Abr"] <- c("Apr")
tcbrasilt7$month[tcbrasilt7$m == "Mai"] <- c("May")
tcbrasilt7$month[tcbrasilt7$m == "Jun"] <- c("Jun")
tcbrasilt7$month[tcbrasilt7$m == "Jul"] <- c("Jul")
tcbrasilt7$month[tcbrasilt7$m == "Ago"] <- c("Aug")
tcbrasilt7$month[tcbrasilt7$m == "Set"] <- c("Sep")
tcbrasilt7$month[tcbrasilt7$m == "Out"] <- c("Oct")
tcbrasilt7$month[tcbrasilt7$m == "Nov"] <- c("Nov")
tcbrasilt7$month[tcbrasilt7$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt7 <- tcbrasilt7 %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt7$date <-
  format(lubridate::ymd(paste0(tcbrasilt7$year, tcbrasilt7$month, "01")), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt7)
keeps2 <- c(
  "PJ.crural.imp.merc",
  "PJ.crural.tarifa.reg",
  "PJ.crural.total",
  "PJ.financ.inmob.imp.merc",
  "PJ.financ.inmob.tarifa.reg",
  "PJ.financ.inmob.total",
  "PJ.cred.rec.bndes.kgirat",
  "PJ.cred.rec.bndes.finver",
  "PJ.cred.rec.bndes.fagro",
  "PJ.cred.rec.bndes.total",
  "PJ.otros",
  "PJ.total"
)
tcbrasilt7 = tcbrasilt7[keeps2]

# Tabel 8 ---------------------------------------
######rm(list = ls())
tcbrat8 <-
  read_excel(brasilu, sheet = "Tabela 8", range = "A12:O1000")
# analyzing
colnames(tcbrat8)
# Rename column names
names(tcbrat8)[names(tcbrat8) == "...1"] <- "year"
names(tcbrat8)[names(tcbrat8) == "...2"] <- "m"
names(tcbrat8)[names(tcbrat8) == "...3"] <- "n"
names(tcbrat8)[names(tcbrat8) == "...4"] <- "crural.imp.merc"
names(tcbrat8)[names(tcbrat8) == "...5"] <- "crural.tarifa.reg"
names(tcbrat8)[names(tcbrat8) == "...6"] <- "crural.total"
names(tcbrat8)[names(tcbrat8) == "...7"] <- "financ.inmob.imp.merc"
names(tcbrat8)[names(tcbrat8) == "...8"] <-
  "financ.inmob.tarifa.reg"
names(tcbrat8)[names(tcbrat8) == "...9"] <- "financ.inmob.total"
names(tcbrat8)[names(tcbrat8) == "...10"] <- "cred.rec.bndes.finver"
names(tcbrat8)[names(tcbrat8) == "...11"] <- "cred.rec.bndes.fagro"
names(tcbrat8)[names(tcbrat8) == "...12"] <- "cred.rec.bndes.total"
names(tcbrat8)[names(tcbrat8) == "...13"] <- "Microcredit"
names(tcbrat8)[names(tcbrat8) == "...14"] <- "otros"
names(tcbrat8)[names(tcbrat8) == "...15"] <- "total"
tcbrat8

#Eliminando empty values
tcbrat8 <- tcbrat8[!is.na(tcbrat8$m),]

#Con las variables que interesan
colnames(tcbrat8)

keeps <- c("year",
           "m",
           "crural.imp.merc",
           "crural.tarifa.reg",
           "crural.total",
           "financ.inmob.imp.merc",
           "financ.inmob.tarifa.reg",
           "financ.inmob.total",
           "cred.rec.bndes.finver",
           "cred.rec.bndes.fagro",
           "cred.rec.bndes.total",
           "Microcredit",
           "otros",
           "total"
)

tcbrasilt8 = tcbrat8[keeps]

#Variable mes numeral
tcbrasilt8$id <- ""
tcbrasilt8$month <- ""
tcbrasilt8$id[tcbrasilt8$m == "Jan"] <- c(1)
tcbrasilt8$id[tcbrasilt8$m == "Fev"] <- c(2)
tcbrasilt8$id[tcbrasilt8$m == "Mar"] <- c(3)
tcbrasilt8$id[tcbrasilt8$m == "Abr"] <- c(4)
tcbrasilt8$id[tcbrasilt8$m == "Mai"] <- c(5)
tcbrasilt8$id[tcbrasilt8$m == "Jun"] <- c(6)
tcbrasilt8$id[tcbrasilt8$m == "Jul"] <- c(7)
tcbrasilt8$id[tcbrasilt8$m == "Ago"] <- c(8)
tcbrasilt8$id[tcbrasilt8$m == "Set"] <- c(9)
tcbrasilt8$id[tcbrasilt8$m == "Out"] <- c(10)
tcbrasilt8$id[tcbrasilt8$m == "Nov"] <- c(11)
tcbrasilt8$id[tcbrasilt8$m == "Dez"] <- c(12)

tcbrasilt8$month[tcbrasilt8$m == "Jan"] <- c("Jan")
tcbrasilt8$month[tcbrasilt8$m == "Fev"] <- c("Feb")
tcbrasilt8$month[tcbrasilt8$m == "Mar"] <- c("Mar")
tcbrasilt8$month[tcbrasilt8$m == "Abr"] <- c("Apr")
tcbrasilt8$month[tcbrasilt8$m == "Mai"] <- c("May")
tcbrasilt8$month[tcbrasilt8$m == "Jun"] <- c("Jun")
tcbrasilt8$month[tcbrasilt8$m == "Jul"] <- c("Jul")
tcbrasilt8$month[tcbrasilt8$m == "Ago"] <- c("Aug")
tcbrasilt8$month[tcbrasilt8$m == "Set"] <- c("Sep")
tcbrasilt8$month[tcbrasilt8$m == "Out"] <- c("Oct")
tcbrasilt8$month[tcbrasilt8$m == "Nov"] <- c("Nov")
tcbrasilt8$month[tcbrasilt8$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt8 <- tcbrasilt8 %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt8$date <-
  format(lubridate::ymd(paste0(tcbrasilt8$year, tcbrasilt8$month, "01")), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt8)
keeps2 <- c(
  "crural.imp.merc",
  "crural.tarifa.reg",
  "crural.total",
  "financ.inmob.imp.merc",
  "financ.inmob.tarifa.reg",
  "financ.inmob.total",
  "cred.rec.bndes.finver",
  "cred.rec.bndes.fagro",
  "cred.rec.bndes.total",
  "Microcredit",
  "otros",
  "total"
)
tcbrasilt8 = tcbrasilt8[keeps2]

# EMPRESAS ------------------------
# Tabel 27 ---------------------------------------
######rm(list = ls())
tcbrat27 <-
  read_excel(brasilu, sheet = "Tabela 27", range = "A12:O1000")
# analyzing
colnames(tcbrat27)
# Rename column names
names(tcbrat27)[names(tcbrat27) == "...1"] <- "year"
names(tcbrat27)[names(tcbrat27) == "...2"] <- "m"
names(tcbrat27)[names(tcbrat27) == "...3"] <- "n"
names(tcbrat27)[names(tcbrat27) == "...4"] <- "EMP.equi.mpme"
names(tcbrat27)[names(tcbrat27) == "...5"] <- "EMP.equi.grande"
names(tcbrat27)[names(tcbrat27) == "...6"] <- "EMP.equi.total"
names(tcbrat27)[names(tcbrat27) == "...7"] <- "EMP.defec.mpme"
names(tcbrat27)[names(tcbrat27) == "...8"] <- "EMP.defec.grande"
names(tcbrat27)[names(tcbrat27) == "...9"] <- "EMP.defec.total"
names(tcbrat27)[names(tcbrat27) == "...10"] <-
  "EMP.maybal.riesgo.mpme"
names(tcbrat27)[names(tcbrat27) == "...11"] <-
  "EMP.maybal.riesgo.grande"
names(tcbrat27)[names(tcbrat27) == "...12"] <-
  "EMP.maybal.riesgo.total"
tcbrat27

#Eliminando empty values
#tcbrat27 <- tcbrat27[!is.na(tcbrat27$vendedor), ]
tcbrat27 <- tcbrat27[!is.na(tcbrat27$m),]

#Con las variables que interesan
colnames(tcbrat27)

keeps <- c("year",
           "m",
           "EMP.equi.mpme",
           "EMP.equi.grande",
           "EMP.equi.total",
           "EMP.defec.mpme",
           "EMP.defec.grande",
           "EMP.defec.total",
           "EMP.maybal.riesgo.mpme",
           "EMP.maybal.riesgo.grande",
           "EMP.maybal.riesgo.total"
)

tcbrasilt27 = tcbrat27[keeps]

#Variable mes numeral
tcbrasilt27$id <- ""
tcbrasilt27$month <- ""
tcbrasilt27$id[tcbrasilt27$m == "Jan"] <- c(1)
tcbrasilt27$id[tcbrasilt27$m == "Fev"] <- c(2)
tcbrasilt27$id[tcbrasilt27$m == "Mar"] <- c(3)
tcbrasilt27$id[tcbrasilt27$m == "Abr"] <- c(4)
tcbrasilt27$id[tcbrasilt27$m == "Mai"] <- c(5)
tcbrasilt27$id[tcbrasilt27$m == "Jun"] <- c(6)
tcbrasilt27$id[tcbrasilt27$m == "Jul"] <- c(7)
tcbrasilt27$id[tcbrasilt27$m == "Ago"] <- c(8)
tcbrasilt27$id[tcbrasilt27$m == "Set"] <- c(9)
tcbrasilt27$id[tcbrasilt27$m == "Out"] <- c(10)
tcbrasilt27$id[tcbrasilt27$m == "Nov"] <- c(11)
tcbrasilt27$id[tcbrasilt27$m == "Dez"] <- c(12)

tcbrasilt27$month[tcbrasilt27$m == "Jan"] <- c("Jan")
tcbrasilt27$month[tcbrasilt27$m == "Fev"] <- c("Feb")
tcbrasilt27$month[tcbrasilt27$m == "Mar"] <- c("Mar")
tcbrasilt27$month[tcbrasilt27$m == "Abr"] <- c("Apr")
tcbrasilt27$month[tcbrasilt27$m == "Mai"] <- c("May")
tcbrasilt27$month[tcbrasilt27$m == "Jun"] <- c("Jun")
tcbrasilt27$month[tcbrasilt27$m == "Jul"] <- c("Jul")
tcbrasilt27$month[tcbrasilt27$m == "Ago"] <- c("Aug")
tcbrasilt27$month[tcbrasilt27$m == "Set"] <- c("Sep")
tcbrasilt27$month[tcbrasilt27$m == "Out"] <- c("Oct")
tcbrasilt27$month[tcbrasilt27$m == "Nov"] <- c("Nov")
tcbrasilt27$month[tcbrasilt27$m == "Dez"] <- c("Dec")

#Variable anho
tcbrasilt27 <- tcbrasilt27 %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt27$date <-
  format(lubridate::ymd(paste0(
    tcbrasilt27$year, tcbrasilt27$month, "01"
  )), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt27)
keeps2 <- c(
  "EMP.equi.mpme",
  "EMP.equi.grande",
  "EMP.equi.total",
  "EMP.defec.mpme",
  "EMP.defec.grande",
  "EMP.defec.total",
  "EMP.maybal.riesgo.mpme",
  "EMP.maybal.riesgo.grande",
  "EMP.maybal.riesgo.total"
)

tcbrasilt27 = tcbrasilt27[keeps2]

# GOBIERNO------------------------
# Tabel 29 ---------------------------------------
tcbrat29 <- read_excel(brasilu,sheet="Tabela 29", range = "A13:O1000")
# analyzing
colnames(tcbrat29)
# Rename column names
names(tcbrat29)[names(tcbrat29) == "...1"] <- "year"
names(tcbrat29)[names(tcbrat29) == "...2"] <- "m"
names(tcbrat29)[names(tcbrat29) == "...3"] <- "n"
names(tcbrat29)[names(tcbrat29) == "...4"] <- "sect.privado_pj"
names(tcbrat29)[names(tcbrat29) == "...5"] <- "sect.privado_pf"
names(tcbrat29)[names(tcbrat29) == "...6"] <- "sect.privado"
names(tcbrat29)[names(tcbrat29) == "...7"] <- "gob.federal"
names(tcbrat29)[names(tcbrat29) == "...8"] <- "gob.munic"
names(tcbrat29)[names(tcbrat29) == "...9"] <- "Government"
names(tcbrat29)[names(tcbrat29) == "...10"] <- "total"
tcbrat29

#Eliminando empty values
#tcbrat29 <- tcbrat29[!is.na(tcbrat29$vendedor), ]
tcbrat29 <- tcbrat29[!is.na(tcbrat29$m), ]

#Con las variables que interesan
colnames(tcbrat29)

keeps <- c("year","m","Government")
tcbrasilt29 = tcbrat29[keeps]

#Variable mes numeral
tcbrasilt29$id <- ""
tcbrasilt29$month <- ""
tcbrasilt29$id[tcbrasilt29$m=="Jan"] <- c(1)
tcbrasilt29$id[tcbrasilt29$m=="Fev"] <- c(2)
tcbrasilt29$id[tcbrasilt29$m=="Mar"] <- c(3)
tcbrasilt29$id[tcbrasilt29$m=="Abr"] <- c(4)
tcbrasilt29$id[tcbrasilt29$m=="Mai"] <- c(5)
tcbrasilt29$id[tcbrasilt29$m=="Jun"] <- c(6)
tcbrasilt29$id[tcbrasilt29$m=="Jul"] <- c(7)
tcbrasilt29$id[tcbrasilt29$m=="Ago"] <- c(8)
tcbrasilt29$id[tcbrasilt29$m=="Set"] <- c(9)
tcbrasilt29$id[tcbrasilt29$m=="Out"] <- c(10)
tcbrasilt29$id[tcbrasilt29$m=="Nov"] <- c(11)
tcbrasilt29$id[tcbrasilt29$m=="Dez"] <- c(12)

tcbrasilt29$month[tcbrasilt29$m=="Jan"] <- c("Jan")
tcbrasilt29$month[tcbrasilt29$m=="Fev"] <- c("Feb")
tcbrasilt29$month[tcbrasilt29$m=="Mar"] <- c("Mar")
tcbrasilt29$month[tcbrasilt29$m=="Abr"] <- c("Apr")
tcbrasilt29$month[tcbrasilt29$m=="Mai"] <- c("May")
tcbrasilt29$month[tcbrasilt29$m=="Jun"] <- c("Jun")
tcbrasilt29$month[tcbrasilt29$m=="Jul"] <- c("Jul")
tcbrasilt29$month[tcbrasilt29$m=="Ago"] <- c("Aug")
tcbrasilt29$month[tcbrasilt29$m=="Set"] <- c("Sep")
tcbrasilt29$month[tcbrasilt29$m=="Out"] <- c("Oct")
tcbrasilt29$month[tcbrasilt29$m=="Nov"] <- c("Nov")
tcbrasilt29$month[tcbrasilt29$m=="Dez"] <- c("Dec")

#Variable anho
tcbrasilt29 <- tcbrasilt29 %>% fill(year, .direction = "down")

#La variable "date"
tcbrasilt29$date <- format(lubridate::ymd(paste0(tcbrasilt29$year,tcbrasilt29$month,"01")), "%m-%Y")

#Nos quedamos con las variables que nos importan
colnames(tcbrasilt29)
keeps2 <- c("Government")

tcbrasilt29 = tcbrasilt29[keeps2]

# cbinPad function ------------------------------
cbindPad <- function(...) {
  args <- list(...)
  n <- sapply(args, nrow)
  mx <- max(n)
  pad <- function(x, mx) {
    if (nrow(x) < mx) {
      nms <- colnames(x)
      padTemp <- matrix(NA, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x) == 0) {
        return(padTemp)
      } else {
        return(rbind(x, padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args, pad, mx)
  return(do.call(cbind, rs))
}

# Adding data horizontally ------------------------------
tcbrasilm1 <- cbindPad(tcbrasilt5,
                       tcbrasilt5a,
                       tcbrasilt5b)

tcbrasilm2 <- cbindPad(tcbrasilm1,
                       tcbrasilt6,
                       tcbrasilt6a,
                       tcbrasilt6b)

tcbrasilu <- cbindPad(tcbrasilm2,
                     tcbrasilt7,
                     tcbrasilt8,
                     tcbrasilt27,
                     tcbrasilt29)

# Agregando variables -----------------------------------
colnames(tcbrasilu)
tcbrasilu$Date <- tcbrasilu$date
tcbrasilu$ktrabajo <- tcbrasilu$PJ.capital.trabajoT
tcbrasilu$Leasing <- tcbrasilu$PJ.arrend.total + tcbrasilu$arrend.total
tcbrasilu$CreditCard <-
  tcbrasilu$PJ.tarjcredito.total + tcbrasilu$tcredito.total
tcbrasilu$ConsumerCredit <- tcbrasilu$cred.PersonalCredit.total
tcbrasilu$Mortgage <-
  tcbrasilu$PJ.financ.inmob.total + tcbrasilu$financ.inmob.total
tcbrasilu$Microcredit <- tcbrasilu$Microcredit
tcbrasilu$SMEs <- tcbrasilu$EMP.equi.mpme
tcbrasilu$BusinessCredit <- tcbrasilu$EMP.equi.grande
#tcbrasilu$CommercialCredit <- tcbrasilu$BusinessCredit + tcbrasilu$SMEs
tcbrasilu$Government <- tcbrasilu$Government
tcbrasilu$Total <- tcbrasilu$PJ.totalt5+tcbrasilu$total

keeps3 <- c(
  "Date",
  "Leasing",
  "CreditCard",
  #"PersonalCredit",
  "Mortgage",
  "Microcredit",
  "ConsumerCredit",
  "SMEs",
  "BusinessCredit",
  "Government",
  "Total"
  #, "CommercialCredit"
)

tcbrasilu <- tcbrasilu[keeps3]

# Variables panel----
tcbrasilu$Pais <- "Brazil"
#tcbrasil$Government <- NA
tcbrasilu$CommercialCredit <- NA
tcbrasilu$PersonalCredit <- NA
tcbrasilu$year <- format(tcbrasilu$Date, "%Y")
#}else{break}
# if dataframe !exist-------------
exists <- exists('tcbrasilu') && is.data.frame(get('tcbrasilu'))
if (exists==FALSE){
  tcbrasilu <- read.csv(paste(backup,"/old_tcbrasil.csv",sep=""))
}else{break}
# Old data and latest------------------------------
tcbrasil <- read.csv(paste(backup,"/old_tcbrasil.csv",sep=""))
class(tcbrasil$Date)
tcbrasil$Date <- as.Date(tcbrasil$Date, format = "%Y-%m-%d")
tcbrasil <- rbind(tcbrasilu, tcbrasil)
# Eliminando pasados duplicados---------
tcbrasil <- tcbrasil %>% distinct(Date, .keep_all = TRUE)
# Ordenando los datos-------------------
tcbrasil <- tcbrasil[order(tcbrasil$Date),]
# Eliminando desde cierta fecha----------
tcbrasil <- tcbrasil[tcbrasil$Date >= "2018-01-01", ]
# Exportando --------------------------------------------
write.csv(
  tcbrasil,paste(backup,"old_tcbrasil.csv",sep="/"),
  row.names = FALSE
)




#*########## CHILE ---------------------------------------
# ConsumerCredit------------------
chilec <- paste(raw,"ConsumerCredit.xlsx",sep="/")
#if (file.exists(chilec)){
# Reading the file ConsumerCredit
tcchi <- read_excel(chilec,
                    sheet = "SBIF_DEUD_CCS_TRMD_MM$",
                    range = "A4:L9000")

# Renombrando variables ConsumerCredit---
colnames(tcchi) # get column names
# Rename column names
names(tcchi)[names(tcchi) == "Fecha"] <- "Date"
names(tcchi)[names(tcchi) == "Hasta 20 UF"] <- "Consumer1"
names(tcchi)[names(tcchi) == "Más de 20 UF - Hasta 50 UF"] <- "Consumer2"
names(tcchi)[names(tcchi) == "Más de 50 UF - Hasta 200 UF"] <- "Consumer3"
names(tcchi)[names(tcchi) == "Más de 200 UF - Hasta 400 UF"] <- "Consumer4"
names(tcchi)[names(tcchi) == "Más de 400 UF - Hasta 1.000 UF"] <- "Consumer5"
names(tcchi)[names(tcchi) == "Más de 1.000 UF - Hasta 3.000 UF"] <- "Consumer6"
names(tcchi)[names(tcchi) == "Más de 3.000 UF - Hasta 10.000 UF"] <- "Consumer7"
names(tcchi)[names(tcchi) == "Más de 10.000 UF - Hasta 50.000 UF"] <- "Consumer8"
names(tcchi)[names(tcchi) == "Más de 50.000 UF - Hasta 200.000 UF"] <- "Consumer9"
names(tcchi)[names(tcchi) == "Más de 200.000 UF - Hasta 500.000 UF"] <- "Consumer10"
names(tcchi)[names(tcchi) == "Más de 500.000 UF"] <- "Consumer11"
# 
tcchi$ConsumerCredit <-
  rowSums(tcchi[, c(
    "Consumer1",
    "Consumer2",
    "Consumer3",
    "Consumer4",
    "Consumer5",
    "Consumer6",
    "Consumer7",
    "Consumer8",
    "Consumer9",
    "Consumer10",
    "Consumer11")], na.rm = TRUE)

#Con las variables que interesan
keeps <- c("Date",
           "ConsumerCredit")
tcchile = tcchi[keeps]
# Eliminando casillas vacias ConsumerCredit---
tcchile <- tcchile %>% drop_na(Date)
# Eliminando desde cierta Date
tcchile <- tcchile[tcchile$Date > "2017-12-01", ]
# Nombre de las filas ConsumerCredit---
row.names(tcchile) <- tcchile$Date
# Datos mensuales ConsumerCredit---
#Encontrar el ultimo dia segun mes
#Creando variables year, month and day
tcchile$my <-
  format(as.Date(tcchile$Date, format = "%Y-%m-%d"), "%Y-%m")
class(tcchile$Date)
tcchile$Date <- as.Date(as.POSIXct(tcchile$Date))
#Encontrando el valor maximo
tcchile$maxvalue <- ave(tcchile$Date, tcchile$my, FUN = max)
#Mantener los valores finales
tcchile <- filter(tcchile, maxvalue == Date)
keeps4 <- c("Date",
            "ConsumerCredit")
tcchile_con = tcchile[keeps4]
#}else{break}
exists <- exists('tcchile_con') && is.data.frame(get('tcchile_con'))
if (exists==FALSE){
  break
  }
# Mortgage------------------------
chilem <- paste(raw,"Hipotecaria.xlsx",sep="/")
#if (file.exists(chilem)){
# Reading the file HIPOTECARIA
##rm(list = ls())
tcchi <- read_excel(chilem,
                    sheet = "SBIF_DEUD_CHV_TRMD_MM$",
                    range = "A4:L9000")
# Renombrando variables HIPOTECARIA ---
colnames(tcchi) # get column names
# Rename column names
names(tcchi)[names(tcchi) == "Fecha"] <- "Date"
names(tcchi)[names(tcchi) == "Hasta 20 UF"] <- "Hipo1"
names(tcchi)[names(tcchi) == "Más de 20 UF - Hasta 50 UF"] <- "Hipo2"
names(tcchi)[names(tcchi) == "Más de 50 UF - Hasta 200 UF"] <- "Hipo3"
names(tcchi)[names(tcchi) == "Más de 200 UF - Hasta 400 UF"] <- "Hipo4"
names(tcchi)[names(tcchi) == "Más de 400 UF - Hasta 1.000 UF"] <- "Hipo5"
names(tcchi)[names(tcchi) == "Más de 1.000 UF - Hasta 3.000 UF"] <- "Hipo6"
names(tcchi)[names(tcchi) == "Más de 3.000 UF - Hasta 10.000 UF"] <- "Hipo7"
names(tcchi)[names(tcchi) == "Más de 10.000 UF - Hasta 50.000 UF"] <- "Hipo8"
names(tcchi)[names(tcchi) == "Más de 50.000 UF - Hasta 200.000 UF"] <- "Hipo9"
names(tcchi)[names(tcchi) == "Más de 200.000 UF - Hasta 500.000 UF"] <- "Hipo10"
names(tcchi)[names(tcchi) == "Más de 500.000 UF"] <- "Hipo11"
# de sring to numeric
class(tcchi$Hipo10)
cols = c(2:12);    
tcchi[,cols] = apply(tcchi[,cols], 2, function(x) as.numeric(as.character(x)));
# 
tcchi$Mortgage <-
  rowSums(tcchi[, c(
    "Hipo1",
    "Hipo2",
    "Hipo3",
    "Hipo4",
    "Hipo5",
    "Hipo6",
    "Hipo7",
    "Hipo8",
    "Hipo9",
    "Hipo10",
    "Hipo11")], na.rm = TRUE)

#Con las variables que interesan
keeps <- c("Date",
           "Mortgage")
tcchile = tcchi[keeps]
# Eliminando desde cierta Date
tcchile <- tcchile[tcchile$Date > "2017-12-01", ]
# Datos mensuales HIPOTECARIA---
#Encontrar el ultimo dia segun mes
#Creando variables year, month and day
tcchile$my <-
  format(as.Date(tcchile$Date, format = "%Y-%m-%d"), "%Y-%m")
class(tcchile$Date)
tcchile$Date <- as.Date(as.POSIXct(tcchile$Date))
#Encontrando el valor maximo
tcchile$maxvalue <- ave(tcchile$Date, tcchile$my, FUN = max)
# Mantener los valores finales
tcchilem <- filter(tcchile, maxvalue == Date)
keeps4 <- c("Mortgage")
tcchile_hipo = tcchilem[keeps4]
#}else{break}
exists <- exists('tcchile_hipo') && is.data.frame(get('tcchile_hipo'))
if (exists==FALSE){
  break
}
# CommercialCredit-----------------
chileco <- paste(raw,"EnterprisesCredit.xlsx",sep="/")
#if (file.exists(chileco)){
# Reading the file CommercialCredit
tcchi <- read_excel(chileco,
                    sheet = "SBIF_CART_CCO_TAMDEU_MM$_MONT",
                    range = "A4:L9000")
# Renombrando variables CommercialCredit ---
colnames(tcchi) # get column names
names(tcchi)[names(tcchi) == "Fecha"] <- "Date"
names(tcchi)[names(tcchi) == "Micro"] <- "Microcredit"
# de sring to numeric
class(tcchi$Mega)
cols = c(2:12);    
tcchi[,cols] = apply(tcchi[,cols], 2, function(x) as.numeric(as.character(x)));
# 
tcchi$SMEs <-
  rowSums(tcchi[, c(
    "Mediano",
    "Pequeño")], na.rm = TRUE)
#
tcchi$CommercialCredit <-
  rowSums(tcchi[, c(
    "Grande",
    "Mega")], na.rm = TRUE)
# Eliminando observaciones vacias CommercialCredit ---
tcchi <- tcchi %>% drop_na(Date)
# Eliminando desde cierta Date
tcchi <- tcchi[tcchi$Date >= "2017-12-01", ]
#Con las variables que interesan
keeps <- c(
  #"Date",
  "Microcredit",
  "SMEs",
  "CommercialCredit")
tcchile_CommercialCredit = tcchi[keeps]
# Variable pais ------------------
tcchile_CommercialCredit$Pais <- "Chile"
#}else{break}
# Bolean options--------
exists <- exists('tcchile_CommercialCredit') && is.data.frame(get('tcchile_CommercialCredit'))
exists2 <- exists('tcchile_hipo') && is.data.frame(get('tcchile_hipo'))
exists3 <- exists('tcchile_con') && is.data.frame(get('tcchile_con'))
if (exists==TRUE&&exists2==TRUE&&exists3==TRUE){
  # cbinPad function ------------------------------
  cbindPad <- function(...) {
    args <- list(...)
    n <- sapply(args, nrow)
    mx <- max(n)
    pad <- function(x, mx) {
      if (nrow(x) < mx) {
        nms <- colnames(x)
        padTemp <- matrix(NA, mx - nrow(x), ncol(x))
        colnames(padTemp) <- nms
        if (ncol(x) == 0) {
          return(padTemp)
        } else {
          return(rbind(x, padTemp))
        }
      }
      else{
        return(x)
      }
    }
    rs <- lapply(args, pad, mx)
    return(do.call(cbind, rs))
  }
  
  # Uniendo los archivos independientes ------------------
  tcchile <- cbindPad(tcchile_con,
                      tcchile_hipo,
                      tcchile_CommercialCredit)
  
  #Variables Panel
  tcchile$CreditCard <- NA
  tcchile$PersonalCredit <- NA
  #tcchile$Microcredit <- NA
  #tcchile$SMEs <- NA
  tcchile$BusinessCredit <- NA
  tcchile$Government <- NA
  tcchile$Leasing <- NA
  
  # Sumando para el credito total-----------
  tcchile$year <- format(tcchile$Date, "%Y")
  
  tcchile$Total <-
    rowSums(tcchile[, c(
      "ConsumerCredit",
      "CommercialCredit",
      "Mortgage",
      "SMEs",
      "Microcredit"
    )], na.rm = TRUE)
}else{
  # Opening the old file-----
  tcchile <- read.csv(paste(backup,"/old_tcchile.csv",sep=""))
  }
# if dataframe !exist-------------
exists <- exists('tcchile') && is.data.frame(get('tcchile'))
if (exists==FALSE){
  tcchile <- read.csv(paste(backup,"/old_tcchile.csv",sep=""))
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcchile,paste(backup,"old_tcchile.csv",sep="/"),
  row.names = FALSE
)




#*########## COLOMBIA ---------------------------------------
# Datos disponibles------
colomb <- paste(raw,"colombialatest.xlsx",sep="/")
#if (file.exists(colomb)){
# Reading the file ----
tccol <-
  read_excel(colomb , sheet = "C5", range = "B31:AF5000")
# Renombrando variables ----
colnames(tccol) # get column names
# Rename column names
names(tccol)[names(tccol) == "43099"] <- "Date"
names(tccol)[names(tccol) == "239657.66876000003"] <-
  "CommercialCredit"
names(tccol)[names(tccol) == "124362.93369000003"] <-
  "ConsumerCredit"
names(tccol)[names(tccol) == "55874.297179999994"] <- "Mortgage"
names(tccol)[names(tccol) == "12128.739150000003"] <- "Microcredit"
tccol
# Con las variables que interesan ----
keeps <- c("Date",
           "CommercialCredit",
           "ConsumerCredit",
           "Microcredit",
           "Mortgage")
tccolombia = tccol[keeps]
# Variable Date ----
tccolombia$Date = openxlsx::convertToDate(tccolombia$Date)
# Eliminando observaciones vacias ----
tccolombiam <- tccolombia %>% drop_na(Date)
# Variable pais------
tccolombiam$Pais <- "Colombia"
# Sumando para el credito total----
tccolombiam$Total <-
  rowSums(tccolombiam[, c(
    "ConsumerCredit",
    "CommercialCredit",
    "Mortgage",
    "Microcredit"
  )], na.rm = TRUE)

# Anhadiendo las variables que faltan para el panel--------
tccolombiam$PersonalCredit <- NA
tccolombiam$SMEs <- NA
tccolombiam$BusinessCredit <- NA
tccolombiam$Leasing <- NA
#tccolombiam$CommercialCredit <- NA
tccolombiam$Government <- NA
#tccolombiam$ConsumerCredit <- NA
#tccolombiam$Microcredit <- NA
#tccolombiam$Mortgage <- NA
tccolombiam$CreditCard <- NA
tccolombiam$year <- format(tccolombiam$Date, "%Y")

#}else{break}
# if dataframe !exist-------------
exists <- exists('tccol') && is.data.frame(get('tccol'))
if (exists==FALSE){
  # Opening the old file-----
  tccolombiam <- read.csv(paste(backup,"/old_tccolombia.csv",sep=""))
}else{break}
# Exportando --------------------------------------------
write.csv(
  tccolombiam,paste(backup,"old_tccolombia.csv",sep="/"),
  row.names = FALSE
)




#*########## COSTA RICA ---------------------------------------
# Transformando el ultimo archivo disponible------
table = readHTMLTable(paste(raw,"crica_latest.xls",sep="/"))
#table <- list.clean(table, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(table, function(t) dim(t)[1]))
table[[which.max(n.rows)]]
costarica <- as.data.frame(table)
tccost <- costarica[-c(1:6),]
#if (file.exists(costar)){
# Renombrando variables --------------------------------------------
colnames(tccost) # get column names
# Rename column names
names(tccost)[names(tccost) == "Table9.V1"] <- "Date"
names(tccost)[names(tccost) == "Table9.V2"] <- "Agricultura"
names(tccost)[names(tccost) == "Table9.V3"] <- "Ganaderia"
names(tccost)[names(tccost) == "Table9.V4"] <- "Pesca"
names(tccost)[names(tccost) == "Table9.V5"] <- "Industria"
names(tccost)[names(tccost) == "Table9.V6"] <- "Mortgage"
names(tccost)[names(tccost) == "Table9.V7"] <- "Construccion"
names(tccost)[names(tccost) == "Table9.V8"] <- "Turismo"
names(tccost)[names(tccost) == "Table9.V9"] <- "Comercio"
names(tccost)[names(tccost) == "Table9.V10"] <- "Servicios"
names(tccost)[names(tccost) == "Table9.V11"] <- "ConsumerCredit"
names(tccost)[names(tccost) == "Table9.V12"] <- "Electricidad"
names(tccost)[names(tccost) == "Table9.V13"] <- "Transporte"
names(tccost)[names(tccost) == "Table9.V14"] <- "Almacenamiento"
names(tccost)[names(tccost) == "Table9.V15"] <- "Otros"
names(tccost)[names(tccost) == "Table9.V17"] <- "Total"
tccost

# Con las variables que interesan ---------------------------------------------
keeps <- c("Date",
           "ConsumerCredit",
           "Mortgage",
           "Total")
tccostarica = tccost[keeps]
# Eliminando observaciones vacias ----------------------------------
tccostaricam <- tccostarica %>% drop_na(Date)
# Variable Date --------------------------------------------
library(readr) 
tccostaricam$Date <- as.character(tccostaricam$Date)
tccostaricam$Date <-parse_date(tccostaricam$Date,"%B/%Y",locale=locale("es"))
# Variable pais------
tccostaricam$Pais <- "Costa Rica"
# Cambiando comas por puntos------
tccostaricam$Mortgage <- gsub(",", ".", tccostaricam$Mortgage)
tccostaricam$ConsumerCredit <- gsub(",", ".", tccostaricam$ConsumerCredit)
tccostaricam$Total <- gsub(",", ".", tccostaricam$Total)
cols = c(2:4)
tccostaricam[, cols] = apply(tccostaricam[, cols], 2, function(x)
  as.numeric(as.character(x)))
# Commercial credit----------------
tccostaricam$CommercialCredit <- tccostaricam$Total-(tccostaricam$ConsumerCredit
                                                     +tccostaricam$Mortgage)
# Anhadiendo las variables que faltan para el panel--------
tccostaricam$Microcredit <- NA
tccostaricam$CreditCard <- NA
tccostaricam$PersonalCredit <- NA
tccostaricam$SMEs <- NA
tccostaricam$BusinessCredit <- NA
tccostaricam$Leasing <- NA
tccostaricam$Government <- NA
#tccostaricam$ConsumerCredit <- NA
tccostaricam$year <- format(tccostaricam$Date, "%Y")

# Eliminando desde cierta fecha ------
tccostaricam <- tccostaricam[tccostaricam$Date >= "2018-01-01",]

# Eliminando los duplicados------
tccostaricam <- tccostaricam %>% distinct(Date, .keep_all = TRUE)
#}else{break}

exists <- exists('tccostaricam') && is.data.frame(get('tccostaricam'))
if (exists==FALSE){
  # Opening the old file-----
  tccostaricam <- read.csv(paste(backup,"/old_tccostaricam.csv",sep=""))
}else{break}
# Exportando --------------------------------------------
write.csv(
  tccostaricam,paste(backup,"old_tccostaricam.csv",sep="/"),
  row.names = FALSE
)






#*########## DOMINICAN REPUBLIC ---------------------------------------
#*Working with the data---------------
repdom <- paste(raw,"RepDominicana.xlsx",sep="/")
#if (file.exists(repdom)){
# Reading the file -----------------------
tcrepd <-
  read_excel(repdom, sheet = "Cartera, deudor y tipo entidad", range = "A9:ZZ100")
# Obeservaciones importantes -----------------------
colnames(tcrepd)
Comercialesm <- tcrepd[grepl("COMERCIALES",tcrepd$...1),]
Comerciales <- tcrepd[grepl("Comerciales",tcrepd$...1),]
dest <- exists('Comercialesm') && is.data.frame(get('Comercialesm'))
if (dest==TRUE){Comerciales <- rbind(Comerciales,Comerciales)}else{break}
Comerciales <- as.data.frame(t(as.matrix(Comerciales)))
keepcom <-c("V1","V2","V3","V4","V5")
Comerciales <- Comerciales[keepcom]
#
Consumom <- tcrepd[grepl("CONSUMO",tcrepd$...1),]
Consumo <- tcrepd[grepl("Consumo",tcrepd$...1),]
dest <- exists('Consumom') && is.data.frame(get('Consumom'))
if (dest==TRUE){Consumo <- rbind(Consumo,Consumom)}else{break}
Consumo <- as.data.frame(t(as.matrix(Consumo)))
keepcon <-c("V1","V2")
Consumom <- Consumo[keepcon]
#
Tarjetam <- tcrepd[grepl("TARJETA",tcrepd$...1),]
Tarjeta <- tcrepd[grepl("Tarjeta",tcrepd$...1),]
dest <- exists('Tarjetam') && is.data.frame(get('Tarjetam'))
if (dest==TRUE){Tarjeta <- rbind(Tarjeta,Tarjetam)}else{break}
Tarjeta <- as.data.frame(t(as.matrix(Tarjeta)))
keept <-c("V1")
Tarjeta <- Tarjeta[keept]
#
hipotecario <- tcrepd[grepl("HIPOTECARIOS",tcrepd$...1),]
hipotecario <- tcrepd[grepl("Hipotecarios",tcrepd$...1),]
dest <- exists('hipotecariom') && is.data.frame(get('hipotecariom'))
if (dest==TRUE){hipotecario <- rbind(hipotecario,hipotecariom)}else{break}
hipotecario <- as.data.frame(t(as.matrix(hipotecario)))
keeph <-c("V1","V2")
hipotecario <- hipotecario[keeph]
fecha <- tcrepd[grepl("DEUDOR",tcrepd$...1),]
fecha <- as.data.frame(t(as.matrix(fecha)))
#
Total <- tcrepd[grepl("TOTAL",tcrepd$...1),]
Total <- tcrepd[grepl("Total",tcrepd$...1),]
dest <- exists('Totalm') && is.data.frame(get('Totalm'))
if (dest==TRUE){Total <- rbind(Total,Totalm)}else{break}
Total <- as.data.frame(t(as.matrix(Total)))
keeph <-c("V1")
Total <- Total[keeph]
#
tcrepdominicana <- cbind(fecha,Comerciales,Consumo,Tarjeta,hipotecario,Total)
# Renombrando variables segunda parte-----------
colnames(tcrepdominicana)
colnames(tcrepdominicana)[1] <- "Date"
#colnames(tcrepdominicana)[2] <- "CommercialCredit"
colnames(tcrepdominicana)[3] <- "BusinessCredit"
colnames(tcrepdominicana)[4] <- "mediana"
colnames(tcrepdominicana)[5] <- "pequeña"
colnames(tcrepdominicana)[6] <- "Microcredit"
colnames(tcrepdominicana)[11] <- "CreditCard"
colnames(tcrepdominicana)[8] <- "ConsumerCredit"
colnames(tcrepdominicana)[13] <- "Mortgage"
colnames(tcrepdominicana)[14] <- "Total"
tcrepdominicana
tcrepdominicana <- tcrepdominicana[-(1:1),]
# De factor a numerico--------------
class(tcrepdominicana$BusinessCredit)
cols = c(2:14)
tcrepdominicana[, cols] = apply(tcrepdominicana[, cols], 2, function(x)
  as.numeric(as.character(x)))
# Agregando para SMEs ---------------
tcrepdominicana$SMEs <-
  rowSums(tcrepdominicana[, c("mediana", "pequeña")], na.rm = TRUE)
# Variable Date -------------------------
class(tcrepdominicana$Date)
numextract <- function(string) {
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
tcrepdominicana$year <- numextract(tcrepdominicana$Date)
tcrepdominicana$year <- as.numeric(tcrepdominicana$year)
tcrepdominicana$year <- tcrepdominicana$year * -1
tcrepdominicana$Date <- as.character(tcrepdominicana$Date)
tcrepdominicana$month <- substring(tcrepdominicana$Date, 1, 3)
#
tcrepdominicana$month[tcrepdominicana$month == "ene"] <- c("Jan")
tcrepdominicana$month[tcrepdominicana$month == "dic"] <- c("Dec")
tcrepdominicana$month[tcrepdominicana$month == "ago"] <- c("Aug")
tcrepdominicana$month[tcrepdominicana$month == "abr"] <- c("Apr")
#
tcrepdominicana$Date <-
  lubridate::ymd(paste0(tcrepdominicana$year,	tcrepdominicana$month,	01))
# Eliminando valores vacios ----------------------------------
tcrepdominicana <- tcrepdominicana %>% drop_na(Date)
# Con las variables que interesan-----------
keeps <- c(
  "Date",
  #"CommercialCredit",
  "BusinessCredit",
  "Mortgage",
  "Microcredit",
  "CreditCard",
  "SMEs",
  "ConsumerCredit",
  "Total"
)
tcrepdominicana = tcrepdominicana[keeps]
#Variable pais
tcrepdominicana$Pais <- "Dominican Republic"
# Eliminando desde cierta Date ------
tcrepdominicana <-
  tcrepdominicana[tcrepdominicana$Date >= "2018-01-01", ]
# Anhadiendo las variables que faltan para el panel--------
tcrepdominicana$CommercialCredit <- NA
tcrepdominicana$PersonalCredit <- NA
tcrepdominicana$Leasing <- NA
tcrepdominicana$Government <- NA
tcrepdominicana$year <- format(tcrepdominicana$Date, "%Y")

#}else{break}
# If datafarme !exists------
exists <- exists('tcrepd') && is.data.frame(get('tcrepd'))
if(exists==FALSE){ 
  tcrepdominicana <- read_csv(paste(backup,"/old_tcrepdominicana.csv",sep=""))   
}
# Exportando --------------------------------------------
write.csv(
  tcrepdominicana,paste(backup,"old_tcrepdominicana.csv",sep="/"),
  row.names = FALSE
)





#*########## ECUADOR ---------------------------------------
# Working with the data -----------------
ziplist <- unzip(paste(raw,"Ecuador_latest.zip",sep="/"), list = TRUE)
unzip(paste(raw,"Ecuador_latest.zip",sep="/"),exdir = raw)
ecua <- paste(raw,ziplist[1,1],sep="/")
#if (file.exists(ecua)){
# Trabajando el ultimo archivo disponible-------------
yearext=str_sub(ziplist[1,1],-14)
yearext=str_sub(yearext,1,4)
monthext=str_sub(ziplist[1,1],-18)
monthext=str_sub(monthext,1,3)
#
tcecua <-
  read_xlsx(ecua, sheet = "CONSCOND", range = "C7:AJ26")
tcecua <- tcecua[grepl("CARTERA",tcecua$CUENTA),]
names(tcecua)[names(tcecua) == "TOTAL BANCOS PRIVADOS"] <- monthext
names(tcecua)[names(tcecua) == "CUENTA"] <- "Saldo"
keeps <- c("Saldo",
           monthext)
tcecua = tcecua[keeps]
# Transponiendo el dataframe ---
tcecua <- as.data.frame(t(as.matrix(tcecua)))
# Rename column names
colnames(tcecua)
names(tcecua)[names(tcecua) == "V1"] <- "Total"
names(tcecua)[names(tcecua) == "V2"] <- "Comerial_prio"
names(tcecua)[names(tcecua) == "V3"] <- "ConsumerCredit_prio"
names(tcecua)[names(tcecua) == "V4"] <- "Mortgage"
names(tcecua)[names(tcecua) == "V5"] <- "Microcredit"
names(tcecua)[names(tcecua) == "V6"] <- "CreditoProductivo"
names(tcecua)[names(tcecua) == "V7"] <- "Comerial_ordi"
names(tcecua)[names(tcecua) == "V8"] <- "ConsumerCredit_ordi"
names(tcecua)[names(tcecua) == "V9"] <- "ViviendaSocial"
names(tcecua)[names(tcecua) == "V10"] <- "CreditoEducativo"
names(tcecua)[names(tcecua) == "V11"] <- "Government"
#Eliminando las filas no importantes
tcecuala <- tcecua[-c(1:1),]
#Variable year
tcecuala$year <- yearext
tcecuala$month <- monthext
#Cambiando meses
ifelse(monthext=="DIC","DEC",monthext)
ifelse(monthext=="ENE","JAN",monthext)
ifelse(monthext=="AGO","AUG",monthext)
ifelse(monthext=="ABR","APR",monthext)
#Variable pais
tcecuala$Pais <- "Ecuador"
class(tcecuala$Mortgage)
#From character to numeric
tcecuala$Comerial_prio <-
  as.numeric(as.character(tcecuala$Comerial_prio))
tcecuala$ConsumerCredit_prio <-
  as.numeric(as.character(tcecuala$ConsumerCredit_prio))
tcecuala$Mortgage <- as.numeric(as.character(tcecuala$Mortgage))
tcecuala$Microcredit <-
  as.numeric(as.character(tcecuala$Microcredit))
tcecuala$CreditoProductivo <-
  as.numeric(as.character(tcecuala$CreditoProductivo))
tcecuala$Comerial_ordi <-
  as.numeric(as.character(tcecuala$Comerial_ordi))
tcecuala$ConsumerCredit_ordi <-
  as.numeric(as.character(tcecuala$ConsumerCredit_ordi))
tcecuala$ViviendaSocial <-
  as.numeric(as.character(tcecuala$ViviendaSocial))
tcecuala$CreditoEducativo <-
  as.numeric(as.character(tcecuala$CreditoEducativo))
tcecuala$Government <-
  as.numeric(as.character(tcecuala$Government))

#Agregando variables
tcecuala$CommercialCredit <-
  rowSums(tcecuala[, c("CreditoProductivo","Comerial_prio","Comerial_ordi")], na.rm = TRUE)
tcecuala$ConsumerCredit <-
  rowSums(tcecuala[, c("ConsumerCredit_prio", "ConsumerCredit_ordi")], na.rm =
            TRUE)
tcecuala$Mortgage <-
  rowSums(tcecuala[, c("ViviendaSocial", "Mortgage")], na.rm =
            TRUE)
# Variable Date
tcecuala$Date <-
  lubridate::ymd(paste0(tcecuala$year, tcecuala$month, "01"))
keeps3 <-
  c(
    "Date",
    "CommercialCredit",
    "ConsumerCredit",
    "Mortgage",
    "Microcredit",
    #"BusinessCredit",
    "Government"
  )
tcecuala <- tcecuala[keeps3]
#Total
tcecuala$Total <-
  rowSums(tcecuala[, c("CommercialCredit",
                       "ConsumerCredit",
                       "Mortgage",
                       "Microcredit",
                       #"BusinessCredit",
                       "Government")], na.rm =
            TRUE)

# Anhadiendo las variables que faltan para el panel--------
tcecuala$Pais <- "Ecuador"
tcecuala$PersonalCredit <- NA
tcecuala$SMEs <- NA
tcecuala$BusinessCredit <- NA
tcecuala$Leasing <- NA
#tcecuala$CommercialCredit <- NA
tcecuala$Government <- NA
#tcecuala$ConsumerCredit <- NA
#tcecuala$Microcredit <- NA
#tcecuala$Mortgage <- NA
tcecuala$CreditCard <- NA
tcecuala$year <- format(tcecuala$Date, "%Y")

# Uniendo old y latest------
old_tcecuador <- read.csv(paste(backup,"/old_tcecuador.csv",sep=""))
tcecuador <- rbind(tcecuala, old_tcecuador)                   
colnames(old_tcecuador)
colnames(tcecuala)
# Eliminando pasados duplicados---------
tcecuador <- tcecuador %>% distinct(Date, .keep_all = TRUE)
#}else{break}
# Ordenando los datos-------------------
tcecuador <- tcecuador[order(tcecuador$Date),]
# if dataframe !exist------------
exists <- exists('tcecua') && is.data.frame(get('tcecua'))
if(exists==FALSE){ 
  tcecuador <- read_csv(paste(backup,"/old_tcecuador.csv",sep=""))   
}else{break}
# Exportando --------------------------------------------
write.csv(
  tcecuador,paste(backup,"old_tcecuador.csv",sep="/"),
  row.names = FALSE
)



#*########## EL SALVADOR ---------------------------------------
# Reading the file--------------
tcelsad <-
  as.data.table(read_xlsx(paste(raw,"ElSalvador_latest.xls",sep="/")))
write.csv(
  tcelsad,paste(raw,"ElSalvadoro.csv",sep="/"),
  row.names = FALSE
)
tcelsalv <- paste(raw,"ElSalvadoro.csv",sep="/")
#if (file.exists(tcelsalv)){
tcelsa <-
    read_csv(tcelsalv)
# Consumo---------------
Consumom <- tcelsad[grepl("consumo",tcelsad$Bancos),]
Consumo <- tcelsad[grepl("Consumo",tcelsad$Bancos),]
dest <- exists('Consumom') && is.data.frame(get('Consumom'))
if (dest==TRUE){Consumo <- rbind(Consumo,Consumom)}else{break}
#
Consumo <- as.data.frame(t(as.matrix(Consumo)))
class(Consumo$V1)
Consumo$V1 <- as.numeric(as.character(Consumo$V1))
Consumo$V2 <- as.numeric(as.character(Consumo$V2))
Consumo$V3 <- as.numeric(as.character(Consumo$V3))
Consumo$V4 <- as.numeric(as.character(Consumo$V4))
Consumo$V5 <- as.numeric(as.character(Consumo$V5))
Consumo$V6 <- as.numeric(as.character(Consumo$V6))
Consumo$V7 <- as.numeric(as.character(Consumo$V7))
#Max malue
Consumo$id <- 1
Consumo$maxv1 <- ave(Consumo$V1,Consumo$id,FUN=function(x) max(x, na.rm=T))
Consumo$maxv2 <- ave(Consumo$V2,Consumo$id,FUN=function(x) max(x, na.rm=T))
Consumo$maxv3 <- ave(Consumo$V3,Consumo$id,FUN=function(x) max(x, na.rm=T))
Consumo$maxv4 <- ave(Consumo$V4,Consumo$id,FUN=function(x) max(x, na.rm=T))
Consumo$maxv5 <- ave(Consumo$V5,Consumo$id,FUN=function(x) max(x, na.rm=T))
Consumo$maxv6 <- ave(Consumo$V6,Consumo$id,FUN=function(x) max(x, na.rm=T))
Consumo$maxv7 <- ave(Consumo$V7,Consumo$id,FUN=function(x) max(x, na.rm=T))
#
Consumo <- Consumo %>% distinct(Consumo$maxv1, .keep_all = TRUE)
Consumo <- as.data.frame(t(as.matrix(Consumo)))
Consumo$id2 <- 1
Consumo$maxv <- ave(Consumo$V1,Consumo$id2,FUN=function(x) max(x, na.rm=T))
Consumo <- filter(Consumo,maxv==V1)
keepsc <- c("maxv")
Consumo <- Consumo[keepsc]
names(Consumo)[names(Consumo) == "maxv"] <- "ConsumerCredit"
# Vivienda------------
Viviendam <- tcelsad[grepl("VIVIENDA",tcelsad$Bancos),]
Vivienda <- tcelsad[grepl("Vivienda",tcelsad$Bancos),]
dest <- exists('Viviendam') && is.data.frame(get('Viviendam'))
if (dest==TRUE){Vivienda <- rbind(Vivienda,Viviendam)}else{break}
#
Vivienda <- as.data.frame(t(as.matrix(Vivienda)))
class(Vivienda$V1)
Vivienda$V1 <- as.numeric(as.character(Vivienda$V1))
Vivienda$V2 <- as.numeric(as.character(Vivienda$V2))
Vivienda$V3 <- as.numeric(as.character(Vivienda$V3))
Vivienda$V4 <- as.numeric(as.character(Vivienda$V4))
Vivienda$V5 <- as.numeric(as.character(Vivienda$V5))
Vivienda$V6 <- as.numeric(as.character(Vivienda$V6))
Vivienda$V7 <- as.numeric(as.character(Vivienda$V7))
Vivienda$V8 <- as.numeric(as.character(Vivienda$V8))
Vivienda$V9 <- as.numeric(as.character(Vivienda$V9))
Vivienda$V10 <- as.numeric(as.character(Vivienda$V10))
Vivienda$V11 <- as.numeric(as.character(Vivienda$V11))
Vivienda$V12 <- as.numeric(as.character(Vivienda$V12))
Vivienda$V13 <- as.numeric(as.character(Vivienda$V13))
Vivienda$V14 <- as.numeric(as.character(Vivienda$V14))
#Max malue
Vivienda$id <- 1
Vivienda$maxv1 <- ave(Vivienda$V1,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv2 <- ave(Vivienda$V2,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv3 <- ave(Vivienda$V3,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv4 <- ave(Vivienda$V4,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv5 <- ave(Vivienda$V5,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv6 <- ave(Vivienda$V6,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv7 <- ave(Vivienda$V7,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv8 <- ave(Vivienda$V8,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv9 <- ave(Vivienda$V9,Vivienda$id,FUN=function(x) max(x, na.rm=T))
Vivienda$maxv10 <- ave(Vivienda$V10,Vivienda$id,FUN=function(x) max(x, na.rm=T))
#
Vivienda <- Vivienda %>% distinct(maxv4, .keep_all = TRUE)
Vivienda <- as.data.frame(t(as.matrix(Vivienda)))
Vivienda$id2 <- 1
Vivienda$maxv <- ave(Vivienda$Bancos,Vivienda$id2,FUN=function(x) max(x, na.rm=T))
Vivienda <- filter(Vivienda,maxv==Bancos)
keepsc <- c("Bancos")
Vivienda <- Vivienda[keepsc]
names(Vivienda)[names(Vivienda) == "Bancos"] <- "Mortgage"
# Total---------
Totalm <- tcelsad[grepl("TOTAL",tcelsad$Bancos),]
Total <- tcelsad[grepl("Total",tcelsad$Bancos),]
dest <- exists('Totalm') && is.data.frame(get('Totalm'))
if (dest==TRUE){Total <- rbind(Total,Totalm)}else{break}
#
Total <- as.data.frame(t(as.matrix(Total)))
class(Total$V1)
Total$V1 <- as.numeric(as.character(Total$V1))
Total$V2 <- as.numeric(as.character(Total$V2))
Total$V3 <- as.numeric(as.character(Total$V3))
Total$V4 <- as.numeric(as.character(Total$V4))
Total$V5 <- as.numeric(as.character(Total$V5))
Total$V6 <- as.numeric(as.character(Total$V6))
Total$V7 <- as.numeric(as.character(Total$V7))
Total$V8 <- as.numeric(as.character(Total$V8))
Total$V9 <- as.numeric(as.character(Total$V9))
Total$V10 <- as.numeric(as.character(Total$V10))
Total$V11 <- as.numeric(as.character(Total$V11))
Total$V12 <- as.numeric(as.character(Total$V12))
Total$V13 <- as.numeric(as.character(Total$V13))
Total$V14 <- as.numeric(as.character(Total$V14))
#Max malue
Total$id <- 1
Total$maxv1 <- ave(Total$V1,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv2 <- ave(Total$V2,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv3 <- ave(Total$V3,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv4 <- ave(Total$V4,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv5 <- ave(Total$V5,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv6 <- ave(Total$V6,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv7 <- ave(Total$V7,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv8 <- ave(Total$V8,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv9 <- ave(Total$V9,Total$id,FUN=function(x) max(x, na.rm=T))
Total$maxv10 <- ave(Total$V10,Total$id,FUN=function(x) max(x, na.rm=T))
#
Total <- Total %>% distinct(Total$maxv1, .keep_all = TRUE)
Total <- as.data.frame(t(as.matrix(Total)))
Total$id2 <- 1
Total$maxvcorrecto <- ave(Total$V1,Total$id2,FUN=function(x) max(x, na.rm=T))
Total <- filter(Total,maxvcorrecto==V1)
keepsc <- c("maxvcorrecto")
Total <- Total[keepsc]
names(Total)[names(Total) == "maxvcorrecto"] <- "Total"
# Uniendo bases-------------
tcelsala <- cbind(Consumo,Vivienda,Total)
#Otras variables
tcelsala$year <- year2
tcelsala$month <- amonthi
#La variable "date"
tcelsala$date <-
  format(lubridate::ymd(paste0(tcelsala$year		,	tcelsala$month		,	01))		,	 "%m-%Y")
# Variable Date
tcelsala$Date <-
  lubridate::ymd(paste0(tcelsala$year, tcelsala$month, "01"))
# from character to numeric
tcelsala$Mortgage <-
  as.numeric(as.character(tcelsala$Mortgage))
tcelsala$ConsumerCredit <-
  as.numeric(as.character(tcelsala$ConsumerCredit))
tcelsala$Total <-
  as.numeric(as.character(tcelsala$Total))
# Anhdiendo las variables que faltan para el panel
tcelsala$PersonalCredit <- NA
tcelsala$SMEs <- NA
tcelsala$BusinessCredit <- NA
tcelsala$Leasing <- NA
tcelsala$CommercialCredit <- tcelsala$Total-(tcelsala$Mortgage+tcelsala$ConsumerCredit)
tcelsala$Government <- NA
#tcelsala$ConsumerCredit <- NA
tcelsala$Microcredit <- NA
#tcelsala$Mortgage <- NA
tcelsala$CreditCard <- NA
tcelsala$id <- 2
tcelsala$Pais <- "El Salvador"

#Con las variables de interes
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)
tcelsala <- tcelsala[keeps4]
tcelsala$year <- format(tcelsala$Date, "%Y")
# Uniendo old y latest------
tcelsalvador<- read.csv(paste(backup,"/old_tcelsalvador.csv",sep=""))
tcelsalvador <- rbind(tcelsala, tcelsalvador)                   
colnames(tcelsalvador)
colnames(tcelsala)
# Eliminando pasados duplicados---------
tcelsalvador <- tcelsalvador %>% distinct(Date, .keep_all = TRUE)

#}else{break}
# Ordenando los datos-------------------
tcelsalvador <- tcelsalvador[order(tcelsalvador$Date),]

# If dataframe doesn't exists--------------
exists <- exists('tcelsala') && is.data.frame(get('tcelsala'))
if(exists==FALSE){ 
  tcelsalvador <- read_csv(paste(backup,"/old_tcelsalvador.csv",sep=""))   
}else{break}
# Exportando --------------------------------------------
write.csv(
  tcelsalvador,paste(backup,"old_tcelsalvador.csv",sep="/"),
  row.names = FALSE
)



#*########## GUATEMALA ---------------------------------------
# Trabajando el ultimo archivo disponible-----
tcguate <- paste(nauto,"Consulta_26.xlsx",sep="/")
#if (file.exists(tcguate)){
# Reading the file-----------
  tcguate <- read_excel(tcguate, range = "A6:F25")
colnames(tcguate) # get column names
names(tcguate)[names(tcguate) == "EMPRESARIAL MAYOR"] <-
  "BusinessCredit"
names(tcguate)[names(tcguate) == "EMPRESARIAL MENOR"] <- "SMEs"
names(tcguate)[names(tcguate) == "CONSUMO"] <-
  "ConsumerCredit"
names(tcguate)[names(tcguate) == "MICROCRÉDITO"] <- "Microcredit"
names(tcguate)[names(tcguate) == "HIPOTECARIO PARA VIVIENDA"] <-
  "Mortgage"

keeps <- c("BusinessCredit",
           "SMEs",
           "ConsumerCredit",
           "Microcredit",
           "Mortgage")
tcguate = tcguate[keeps]
# From character to numeric-----------
tcguate$SMEs <- as.numeric(gsub(",", "", tcguate$SMEs))
tcguate$ConsumerCredit <-
  as.numeric(gsub(",", "", tcguate$ConsumerCredit))
tcguate$Microcredit <-
  as.numeric(gsub(",", "", tcguate$Microcredit))
tcguate$Mortgage <-
  as.numeric(gsub(",", "", tcguate$Mortgage))
tcguate$BusinessCredit <-
  as.numeric(gsub(",", "", tcguate$BusinessCredit))
#
tcguate$SMEs <- as.numeric(tcguate$SMEs)
tcguate$ConsumerCredit <- as.numeric(tcguate$ConsumerCredit)
tcguate$Microcredit <- as.numeric(tcguate$Microcredit)
tcguate$Mortgage <- as.numeric(tcguate$Mortgage)
tcguate$BusinessCredit <- as.numeric(tcguate$BusinessCredit)

# Encontrando el maximo valor-------
tcguate$id <- 1
colnames(tcguate)
tcguate$mBusinessCredit <- ave(tcguate$BusinessCredit,tcguate$id,FUN=function(x) max(x, na.rm=T))
tcguate$mSMEs <- ave(tcguate$SMEs,tcguate$id,FUN=function(x) max(x, na.rm=T))
tcguate$mConsumerCredit <- ave(tcguate$ConsumerCredit,tcguate$id,FUN=function(x) max(x, na.rm=T))
tcguate$mMicrocredit <- ave(tcguate$Microcredit,tcguate$id,FUN=function(x) max(x, na.rm=T))
tcguate$mMortgage <- ave(tcguate$Mortgage,tcguate$id,FUN=function(x) max(x, na.rm=T))
tcguate <- filter(tcguate,mBusinessCredit==BusinessCredit)
tcguatela <- tcguate

# Variable Date-----------
tcguatela$year <- todayy
tcguatela$month <- atodaym
#La variable "date"
tcguatela$Date <-
  lubridate::ymd(paste0(tcguatela$year, tcguatela$month, "01"))
# Variable pais-------
tcguatela$Pais <- "Guatemala"
# Total de creditos--------
tcguatela$Total <- rowSums(tcguatela[,c(
  "Mortgage", "ConsumerCredit", "SMEs", 
  "BusinessCredit", "Microcredit")], na.rm=TRUE)
# Anhadiendo las variables que faltan para el panel---------------
tcguatela$PersonalCredit <- NA
#tcguatela$SMEs <- NA
#tcguatela$BusinessCredit <- NA
tcguatela$Leasing <- NA
tcguatela$CommercialCredit <- NA
tcguatela$Government <- NA
#tcguatela$ConsumerCredit <- NA
#tcguatela$Microcredit <- NA
#tcguatela$Mortgage <- NA
tcguatela$CreditCard <- NA
tcguatela$id <- 3
# Con las variables de interes-----------------
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)
tcguatela <- tcguatela[keeps4]
tcguatela$year <- format(tcguatela$Date, "%Y")
# Uniendo old y latest------
tcguatemala<- read_csv(paste(backup,"/old_tcguatemala.csv",sep=""))
tcguatemala <- rbind(tcguatela,tcguatemala)                   
colnames(tcguatemala)
colnames(tcguatela)
# Eliminando pasados duplicados---------
tcguatemala <- tcguatemala %>% distinct(Date, .keep_all = TRUE)
str(tcguatemala)
# Ordenando los datos-------------------
tcguatemala <- tcguatemala[order(tcguatemala$Date),]
#}else{break}
# If dataframe doesn't exists--------------
exists <- exists('tcguate') && is.data.frame(get('tcguate'))
if(exists==FALSE){ 
  tcguatemala <- read_csv(paste(backup,"/old_tcguatemala.csv",sep=""))   
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcguatemala,paste(backup,"old_tcguatemala.csv",sep="/"),
  row.names = FALSE
)


#*########## HONDURAS ---------------------------------------
# Trabajando con el ultimo documento -----------------------
hondu <- paste(nauto,"Estados Financieros.xlsx",sep="/")
tchond <-
  read_excel(hondu ,
             sheet = "ESF (Series Fechas)",
             range = "B8:ZZ122")
# Renombrando variables -----------------------
colnames(tchond)
fecha <- tchond[grepl("CUENTAS",tchond$...1),]
comerciales <- tchond[grepl("Comer",tchond$...1),]
microc <- tchond[grepl("Micro",tchond$...1),]
consumo <- tchond[grepl("Cons",tchond$...1),]
vivienda <- tchond[grepl("Vivienda",tchond$...1),]
Total <- tchond[grepl("Vigentes",tchond$...1),]
agro <- tchond[grepl("Agro",tchond$...1),]
tchonduras <- rbind(fecha,Total,comerciales,microc,consumo,vivienda,agro)
# Transponiendo el dataframe -----------------------
tchonduras <- as.data.frame(t(as.matrix(tchonduras)))
# Rename column names
colnames(tchonduras)
colnames(tchonduras)[1] <- "Date"
colnames(tchonduras)[5] <- "Total"
#colnames(tchonduras)[6] <- "CommercialCredit"
colnames(tchonduras)[7] <- "BusinessCredit"
colnames(tchonduras)[8] <- "SMEs"
colnames(tchonduras)[9] <- "Microcredit"
colnames(tchonduras)[10] <- "ConsumerCredit"
colnames(tchonduras)[11] <- "Mortgage"
colnames(tchonduras)[12] <- "Agropecuarios"
tchonduras
#Eliminando la primera fila
tchonduras <- tchonduras[-c(1:1),]
# Variable Date -------------------------
tchonduras$Date <- as.numeric(as.character(tchonduras$Date))
class(tchonduras$Date)
tchonduras$Date = openxlsx::convertToDate(tchonduras$Date)
# Con las variables que interesan-----------
keeps <- c(
  "Date",
  #"CommercialCredit",
  "BusinessCredit",
  "Mortgage",
  "Microcredit",
  "SMEs",
  "ConsumerCredit",
  "Total",
  "Agropecuarios"
)
tchonduras = tchonduras[keeps]
# Borrando filas vacias de date----------------
tchondurasm <- tchonduras %>% drop_na(Date)
tchonduras <- tchondurasm
#Variable pais
tchondurasm$Pais <- "Honduras"

# From character to numeric -----------------------------------------
cols = c(2:8);    
tchondurasm[,cols] = apply(tchondurasm[,cols], 2, function(x) as.numeric(as.character(x)));
class(tchondurasm$CommercialCredit)
# Agregando variable commercial credit-----
tchondurasm$BusinessCredit <- rowSums(tchondurasm
                                        [,c("BusinessCredit","Agropecuarios")],na.rm = TRUE)

# Anhadiendo las variables que faltan para el panel--------
tchondurasm$PersonalCredit <- NA
#tchondurasm$SMEs <- NA
#tchondurasm$BusinessCredit <- NA
tchondurasm$Leasing <- NA
tchondurasm$CommercialCredit <- NA
tchondurasm$Government <- NA
#tchondurasm$ConsumerCredit <- NA
#tchondurasm$Microcredit <- NA
#tchondurasm$Mortgage <- NA
tchondurasm$CreditCard <- NA
tchondurasm$id <- 5
#Con las variables de interes
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)
tchondurasm <- tchondurasm[keeps4]
tchondurasm$year <- format(tchondurasm$Date, "%Y")
# Uniendo old y latest (no se une)------
#Note: los resultados actualizados por la institucion son 
#muy diferentes con los anteriores, no se puede unir
#tchonduras<- read_csv(paste(backup,"/old_tchondurasm.csv",sep=""))
#tchonduras <- rbind(tchondurasm,tchonduras)                   
#colnames(tchonduras)
#colnames(tchondurasm)
# Ordenando los datos-------------------
#tchonduras <- tchonduras[order(tchonduras$Date),]
tchondurasm <- tchondurasm[order(tchondurasm$Date),]
# Eliminando pasados duplicados---------
#tchonduras <- tchonduras %>% distinct(Date, .keep_all = TRUE)
tchondurasm <- tchondurasm %>% distinct(Date, .keep_all = TRUE)
str(tchondurasm)
# Exportando --------------------------------------------
write.csv(
  tchondurasm,paste(backup,"old_tchonduras.csv",sep="/"),
  row.names = FALSE
)




#*########## MEXICO ---------------------------------------
#####BANCA MULTIPLE---------------------------------------
tcmex1 <- paste(nauto,paste("SH_BM_",year2,atodaym,".xls",sep=""),sep="/")
tcmex2 <- paste(nauto,paste("SH_BM_",year2,atodaym,".xlsm",sep=""),sep="/")
if (file.exists(tcmex1)){
# Reading the file
tcmex <-
  read_excel(tcmex1, sheet = "Hoja2"  , range = "B2:zz51")
# Renombrando variables
colnames(tcmex)[1] <- "filtros"
# Eliminando observaciones no importantes
fecha <- tcmex[grepl("Indicadores",tcmex$filtros),]
creditos <- tcmex[grepl("Créditos",tcmex$filtros),]
empresas <- tcmex[grepl("Empresas",tcmex$filtros),]
arrend <- tcmex[grepl("arrendamiento",tcmex$filtros),]
guber <- tcmex[grepl("gubernamentales",tcmex$filtros),]
consumo <- tcmex[grepl("Consumo",tcmex$filtros),]
tarjeta <- tcmex[grepl("Tarjeta",tcmex$filtros),]
vivienda <- tcmex[grepl("Vivienda",tcmex$filtros),]
tcmex2 <- txmex
tcmex <- rbind(fecha,arrend,consumo,creditos,empresas,guber,tarjeta,vivienda)
# Transponiendo el dataframe
tcmexico <- as.data.frame(t(as.matrix(tcmex)))
# Rename column names
colnames(tcmexico)
colnames(tcmexico)[1] <- "Date"
#colnames(tcmexico)[6] <- "CommercialCredites"
#colnames(tcmexico)[15] <- "BusinessCredites"
colnames(tcmexico)[2] <- "Leasingemp"
colnames(tcmexico)[16] <- "Government"
colnames(tcmexico)[4] <- "ConsumerCredit"
colnames(tcmexico)[17] <- "CreditCard"
colnames(tcmexico)[5] <- "PersonalCredit"
colnames(tcmexico)[3] <- "LeasingConsumerCredit"
colnames(tcmexico)[18] <- "Mortgage"
tcmexico
#Eliminando la primera fila
tcmexico <- tcmexico[-c(1),]
# Con las variables que interesan
keeps <- c(
  "Date",
  #"CommercialCredites",
  #"BusinessCredites",
  "Leasingemp",
  "Government",
  "Mortgage",
  "CreditCard",
  "LeasingConsumerCredit",
  "PersonalCredit",
  "ConsumerCredit"
)
tcmexico = tcmexico[keeps]
# Eliminando observaciones vacias
tcmexico <- tcmexico %>% drop_na(Date)
# Variable Date
class(tcmexico$Date)
tcmexico$Date <- as.numeric(as.character(tcmexico$Date))
tcmexico$Date = openxlsx::convertToDate(tcmexico$Date)
#Nombre de las filas --
row.names(tcmexico) <- tcmexico$Date
#De factor a numerico
cols = c(2:10)
tcmexico[, cols] = apply(tcmexico[, cols], 2, function(x)
  as.numeric(as.character(x)))
# Eliminando desde cierta Date
tcmexico <- tcmexico[tcmexico$Date >= "2018-01-01", ]
}else{break}
if (file.exists(tcmex2)){
  # Reading the file
  tcmex <-
    read_excel(tcmex2, sheet = "Hoja2"  , range = "B2:zz51")
  colnames(tcmex)[1] <- "filtros"
  # Eliminando observaciones no importantes
  fecha <- tcmex[grepl("Indicadores",tcmex$filtros),]
  creditos <- tcmex[grepl("Créditos",tcmex$filtros),]
  empresas <- tcmex[grepl("Empresas",tcmex$filtros),]
  arrend <- tcmex[grepl("arrendamiento",tcmex$filtros),]
  guber <- tcmex[grepl("gubernamentales",tcmex$filtros),]
  consumo <- tcmex[grepl("Consumo",tcmex$filtros),]
  tarjeta <- tcmex[grepl("Tarjeta",tcmex$filtros),]
  vivienda <- tcmex[grepl("Vivienda",tcmex$filtros),]
  tcmex2 <- tcmex
  tcmex <- rbind(fecha,arrend,consumo,creditos,empresas,guber,tarjeta,vivienda)
  # Transponiendo el dataframe
  tcmexico <- as.data.frame(t(as.matrix(tcmex)))
  # Rename column names
  colnames(tcmexico)
  colnames(tcmexico)[1] <- "Date"
  #colnames(tcmexico)[6] <- "CommercialCredites"
  #colnames(tcmexico)[15] <- "BusinessCredites"
  colnames(tcmexico)[2] <- "Leasingemp"
  colnames(tcmexico)[16] <- "Government"
  colnames(tcmexico)[4] <- "ConsumerCredit"
  colnames(tcmexico)[17] <- "CreditCard"
  colnames(tcmexico)[5] <- "PersonalCredit"
  colnames(tcmexico)[3] <- "LeasingConsumerCredit"
  colnames(tcmexico)[18] <- "Mortgage"
  tcmexico
  #Eliminando la primera fila
  tcmexico <- tcmexico[-c(1:1),]
  # Con las variables que interesan
  keeps <- c(
    "Date",
    #"CommercialCredites",
    #"BusinessCredites",
    "Leasingemp",
    "Government",
    "Mortgage",
    "CreditCard",
    "LeasingConsumerCredit",
    "PersonalCredit",
    "ConsumerCredit"
  )
  tcmexico = tcmexico[keeps]
  # Eliminando observaciones vacias
  tcmexico <- tcmexico %>% drop_na(Date)
  # Variable Date
  class(tcmexico$Date)
  tcmexico$Date <- as.numeric(as.character(tcmexico$Date))
  tcmexico$Date = openxlsx::convertToDate(tcmexico$Date)
  #De factor a numerico
  cols = c(2:8)
  tcmexico[, cols] = apply(tcmexico[, cols], 2, function(x)
    as.numeric(as.character(x)))
  # Eliminando desde cierta Date
  tcmexico <- tcmexico[tcmexico$Date >= "2018-01-01", ]
}else{break}
txmexico <- tcmexico
rm(list = ls()[grepl("tcmex", ls())])

#####BANCA MULTIPLE (empresas)--------
tcmexe <- paste(nauto,"MD_Emp_PETOTAL.TamanioEmpresas_MDR_V2.xlsx",sep="/")
if (file.exists(tcmexe)){
# Reading the file
tcmex <-
  read_excel(tcmexe, range = "A1:zz1000")
# Transponiendo el dataframe
tcmexicoem <- as.data.frame(t(as.matrix(tcmex)))
# Renombrando variables
colnames(tcmexicoem)
names(tcmexicoem)[names(tcmexicoem) == "V1"] <- "Date"
names(tcmexicoem)[names(tcmexicoem) == "V112"] <- "SMEs"
names(tcmexicoem)[names(tcmexicoem) == "V113"] <- "BusinessCredit"
names(tcmexicoem)[names(tcmexicoem) == "V114"] <- "Fideicomisos"
# Eliminando filas no importantes
tcmexicoem <- tcmexicoem[-c(1:2),]
# Con las variables que interesan
keeps <- c("Date",
           "SMEs",
           "BusinessCredit",
           "Fideicomisos")
tcmexicoem = tcmexicoem[keeps]
# Variable Date
class(tcmexicoem$Date)
numextract <- function(string) {
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
tcmexicoem$year <- numextract(tcmexicoem$Date)
tcmexicoem$year <- as.numeric(tcmexicoem$year)
tcmexicoem$month <- substring(tcmexicoem$Date, 1, 3)
#
tcmexicoem$month[tcmexicoem$month == "Ene"] <- c("Jan")
tcmexicoem$month[tcmexicoem$month == "Dic"] <- c("Dec")
tcmexicoem$month[tcmexicoem$month == "Ago"] <- c("Aug")
tcmexicoem$month[tcmexicoem$month == "Abr"] <- c("Apr")
#
tcmexicoem$Date <-
  lubridate::ymd(paste0(tcmexicoem$year,	tcmexicoem$month,	01))
#De factor a numerico
class(tcmexicoem$SMEs)
cols = c(2:4)
tcmexicoem[, cols] = apply(tcmexicoem[, cols], 2, function(x)
  as.numeric(as.character(x)))
# Eliminando observaciones vacias
tcmexicoem <- tcmexicoem %>% drop_na(Date)
# Eliminando desde cierta Date
tcmexicoem <- tcmexicoem[tcmexicoem$Date >= "2018-01-01", ]
# Ordenando de acuerdo a la Date
tcmexicoem <- tcmexicoem[nrow(tcmexicoem):1, ]
# Agregando Business Credit
tcmexicoem$BusinessCredit <- rowSums(tcmexicoem[,c("Fideicomisos","BusinessCredit")],na.rm = TRUE)
# Variable pais a una base
tcmexicoem$Pais <- "Mexico"
}else{break}
txmexicoem <- tcmexicoem
rm(list = ls()[grepl("tcmex", ls())])

#####BANCA DE DESARROLLO (Microcredit)---------------------------------------
tcmexmi <- paste(nauto,"SeriesDeTiempo.xlsx",sep="/")
if (file.exists(tcmexmi)){
# Reading the file -----------------------
tcmexico_mi <-
  read_excel(tcmexmi , range = "B18:C1000")
class(tcmexico_mi$Sistema)
# Renombrando variables -----------------------
colnames(tcmexico_mi)
names(tcmexico_mi)[names(tcmexico_mi) == "Fecha"] <- "Date"
names(tcmexico_mi)[names(tcmexico_mi) == "Sistema"] <- "Microcredit"

# Variable Date------------------
class(tcmexico_mi$Date)
tcmexico_mi$Date <- as.Date(tcmexico_mi$Date, "%Y-%m-%d")
# Adding to complete dates variables---------
#install.packages("padr")
tcmexico_mi <- tcmexico_mi %>% pad %>% fill_by_value(Microcredit)
tcmexico_mi$id2 <- 1
tcmexico_mi$month <- format(tcmexico_mi$Date, "%m")
tcmexico_mi$year <- format(tcmexico_mi$Date, "%Y")
tcmexico_mi$my <-
  format(as.Date(tcmexico_mi$Date, format = "%Y-%m-%d"), "%Y-%m")
#Encontrando el valor maximo
#tcmexico_mi$Microcredit3 <- ave(tcmexico_mi$Microcredit, tcargentina$month, FUN = max)
tcmexico_mi$maxvalue <-
  ave(tcmexico_mi$Date, tcmexico_mi$my, FUN = max)
#Mantener los valores finales
tcmexico_mi <- filter(tcmexico_mi, maxvalue == Date)
tcmexico_mi[tcmexico_mi == 0] <- NA
# Eliminando desde cierta Date ------
tcmexico_mi <- tcmexico_mi[tcmexico_mi$Date >= "2018-01-01", ]
}else{break}
# Adding data horizontally-----------
exists1 <- exists('txmexico') && is.data.frame(get('txmexico'))
exists2 <- exists('txmexicoem') && is.data.frame(get('txmexicoem'))
exists3 <- exists('tcmexico_mi') && is.data.frame(get('tcmexico_mi'))
if(exists1==TRUE&&exists2==TRUE&&exists3==TRUE){
  # cbinPad function ------------------------------
cbindPad <- function(...) {
  args <- list(...)
  n <- sapply(args, nrow)
  mx <- max(n)
  pad <- function(x, mx) {
    if (nrow(x) < mx) {
      nms <- colnames(x)
      padTemp <- matrix(NA, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x) == 0) {
        return(padTemp)
      } else {
        return(rbind(x, padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args, pad, mx)
  return(do.call(cbind, rs))
}
# Adding data horizontally------------------------------
tcmexico <- cbindPad(txmexico, txmexicoem, tcmexico_mi)
######Agregando variables de ambas bases  (2bases)---
class(tcmexico$ConsumerCredit)
tcmexico$CommercialCredit <- NA
tcmexico$Leasing <-
  rowSums(tcmexico[, c("Leasingemp", "LeasingConsumerCredit")], na.rm =
            TRUE)
# Con las variables que interesan 3ra-parte---
keeps2 <- c(
  "Date",
  "CommercialCredit",
  "BusinessCredit",
  "Leasing",
  "Government",
  "Mortgage",
  "CreditCard",
  "PersonalCredit",
  "ConsumerCredit",
  "SMEs",
  "Microcredit"
)
tcmexico = tcmexico[keeps2]
#De factor a numerico
txmexicoem$SMEs <- as.numeric(as.character(txmexicoem$SMEs))
class(tcmexico$PersonalCredit)
# Variable pais a una base------------------------
tcmexico$Pais <- "Mexico"
# Anhadiendo las variables que faltan para el panel--------
tcmexico$Pais <- "Mexico"
txmexico$PersonalCredit <- NA
#txmexico$SMEs <- NA
#txmexico$BusinessCredit <- NA
#txmexico$Leasing <- NA
txmexico$CommercialCredit <- NA
#txmexico$Government <- NA
#txmexico$ConsumerCredit <- NA
#txmexico$Microcredit <- NA
#txmexico$Mortgage <- NA
#txmexico$CreditCard <- NA
tcmexico$id <- 6
# Total de creditos--------
tcmexico$Total <- rowSums(tcmexico[,c(
  "Mortgage",
  "ConsumerCredit",
  #"PersonalCredit",
  #"SMEs",
  #"BusinessCredit",
  #"Leasing",
  "CommercialCredit"
  #"Government",
  #"Microcredit",
  #"CreditCard"
)], na.rm=TRUE)

# Con las variables de interes-------
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)
tcmexico <- tcmexico[keeps4]
tcmexico$year <- format(tcmexico$Date, "%Y")
tcmexico[tcmexico == 0] <- NA
}
# If dataframe doesn't exists--------------
exists <- exists('tcmexico') && is.data.frame(get('tcmexico'))
if(exists==FALSE){ 
  tcmexico <- read_csv(paste(backup,"/old_tcmexico.csv",sep=""))   
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcmexico,paste(backup,"old_tcmexico.csv",sep="/"),
  row.names = FALSE
)






#*########## NICARAGUA ---------------------------------------
# Trabajando el ultimo archivo disponible ------
tcnica <- paste(raw,"nicaragua_latest.xls",sep="/")
#if (file.exists(tcnica)){
tcnicar <-
  read_excel(tcnica, sheet = "V-10", range = "A8:K5000")
# Nombres variables
colnames(tcnicar)
# Rename column names
names(tcnicar)[names(tcnicar) == "Mes y año"] <- "Date"
names(tcnicar)[names(tcnicar) == "Comercial"] <-
  "Comercial"
names(tcnicar)[names(tcnicar) == "Agrícola"] <- "Agricola"
names(tcnicar)[names(tcnicar) == "Hipotecario para vivienda"] <-
  "Mortgage"
names(tcnicar)[names(tcnicar) == "Créditos personales"] <-
  "ConsumerCredit"
names(tcnicar)[names(tcnicar) == "Tarjetas de crédito"] <-
  "CreditCard"
names(tcnicar)[names(tcnicar) == "Otros (Arrend. Financiero)"] <-
  "Arrendamiento"
names(tcnicar)[names(tcnicar) == "Intereses y Com. Por cobrar"] <-
  "Intereses"
names(tcnicar)[names(tcnicar) == "Total"] <- "Total"
tcnicar

#From character to numeric
cols = c(2:11);    
tcnicar[,cols] = apply(tcnicar[,cols], 2, function(x) as.numeric(as.character(x)));

#Credito commercial
tcnicar$CommercialCredit <- tcnicar$Agricola+tcnicar$Ganadero+
  tcnicar$Industrial+tcnicar$Comercial

#Con las variables que interesan
colnames(tcnicar)
keeps <- c("Date",
           "CommercialCredit",
           "Mortgage",
           "ConsumerCredit",
           "CreditCard",
           "Total")
tcnicaragua = tcnicar[keeps]

# Eliminando valores vacias
tcnicaragua <- tcnicaragua %>% drop_na(Date)
tcnicaragua <- filter(tcnicaragua,Date>300000)
#Variable mes numeral
tcnicaragua$id[tcnicaragua$Date == "Enero"] <- c(1)
tcnicaragua$id[tcnicaragua$Date == "Febrero"] <- c(2)
tcnicaragua$id[tcnicaragua$Date == "Marzo"] <- c(3)
tcnicaragua$id[tcnicaragua$Date == "Abril"] <- c(4)
tcnicaragua$id[tcnicaragua$Date == "Mayo"] <- c(5)
tcnicaragua$id[tcnicaragua$Date == "Junio"] <- c(6)
tcnicaragua$id[tcnicaragua$Date == "Julio"] <- c(7)
tcnicaragua$id[tcnicaragua$Date == "Agosto"] <- c(8)
tcnicaragua$id[tcnicaragua$Date == "Septiembre"] <- c(9)
tcnicaragua$id[tcnicaragua$Date == "Octubre"] <- c(10)
tcnicaragua$id[tcnicaragua$Date == "Noviembre"] <- c(11)
tcnicaragua$id[tcnicaragua$Date == "Diciembre"] <- c(12)

tcnicaragua$month[tcnicaragua$Date == "Enero"] <- c("Jan")
tcnicaragua$month[tcnicaragua$Date == "Febrero"] <- c("Feb")
tcnicaragua$month[tcnicaragua$Date == "Marzo"] <- c("Mar")
tcnicaragua$month[tcnicaragua$Date == "Abril"] <- c("Apr")
tcnicaragua$month[tcnicaragua$Date == "Mayo"] <- c("May")
tcnicaragua$month[tcnicaragua$Date == "Junio"] <- c("Jun")
tcnicaragua$month[tcnicaragua$Date == "Julio"] <- c("Jul")
tcnicaragua$month[tcnicaragua$Date == "Agosto"] <- c("Aug")
tcnicaragua$month[tcnicaragua$Date == "Septiembre"] <- c("Sep")
tcnicaragua$month[tcnicaragua$Date == "Octubre"] <- c("Oct")
tcnicaragua$month[tcnicaragua$Date == "Noviembre"] <- c("Nov")
tcnicaragua$month[tcnicaragua$Date == "Diciembre"] <- c("Dec")

#Variable anho
tcnicaragua$id.t <-
  with(tcnicaragua, ave(rep(1, nrow(tcnicaragua)), id, FUN = seq_along))
#Cambiando los valores del id
tcnicaragua$year <- NA
tcnicaragua$year[tcnicaragua$id.t == 1] <- c(2002)
tcnicaragua$year[tcnicaragua$id.t == 2] <- c(2003)
tcnicaragua$year[tcnicaragua$id.t == 3] <- c(2004)
tcnicaragua$year[tcnicaragua$id.t == 4] <- c(2005)
tcnicaragua$year[tcnicaragua$id.t == 5] <- c(2006)
tcnicaragua$year[tcnicaragua$id.t == 6] <- c(2007)
tcnicaragua$year[tcnicaragua$id.t == 7] <- c(2008)
tcnicaragua$year[tcnicaragua$id.t == 8] <- c(2009)
tcnicaragua$year[tcnicaragua$id.t == 9] <- c(2010)
tcnicaragua$year[tcnicaragua$id.t == 10] <- c(2011)
tcnicaragua$year[tcnicaragua$id.t == 11] <- c(2012)
tcnicaragua$year[tcnicaragua$id.t == 12] <- c(2013)
tcnicaragua$year[tcnicaragua$id.t == 13] <- c(2014)
tcnicaragua$year[tcnicaragua$id.t == 14] <- c(2015)
tcnicaragua$year[tcnicaragua$id.t == 15] <- c(2016)
tcnicaragua$year[tcnicaragua$id.t == 16] <- c(2017)
tcnicaragua$year[tcnicaragua$id.t == 17] <- c(2018)
tcnicaragua$year[tcnicaragua$id.t == 18] <- c(2019)
tcnicaragua$year[tcnicaragua$id.t == 19] <- c(2020)
tcnicaragua$year[tcnicaragua$id.t == 20] <- c(2021)
tcnicaragua$year[tcnicaragua$id.t == 21] <- c(2022)
tcnicaragua$year[tcnicaragua$id.t == 22] <- c(2023)
tcnicaragua$year[tcnicaragua$id.t == 23] <- c(2024)
tcnicaragua$year[tcnicaragua$id.t == 24] <- c(2025)
tcnicaragua$year[tcnicaragua$id.t == 25] <- c(2026)
tcnicaragua$year[tcnicaragua$id.t == 26] <- c(2027)
tcnicaragua$year[tcnicaragua$id.t == 27] <- c(2028)
tcnicaragua$year[tcnicaragua$id.t == 28] <- c(2029)
tcnicaragua$year[tcnicaragua$id.t == 29] <- c(2030)
tcnicaragua$year[tcnicaragua$id.t == 30] <- c(2031)
tcnicaragua$year[tcnicaragua$id.t == 31] <- c(2032)
tcnicaragua$year[tcnicaragua$id.t == 32] <- c(2033)
tcnicaragua$year[tcnicaragua$id.t == 33] <- c(2034)
tcnicaragua$year[tcnicaragua$id.t == 34] <- c(2035)

#La variable "date"
tcnicaragua$Date <-
  lubridate::ymd(paste0(tcnicaragua$year, tcnicaragua$month, "01"))
#Nos quedamos con las variables que nos importan
colnames(tcnicaragua)
keeps2 <- c("Date",
            "CommercialCredit",
            "Mortgage",
            "ConsumerCredit",
            "CreditCard",
            "Total")
tcnicaragua = tcnicaragua[keeps2]
# Eliminando desde cierta Date
tcnicaragua <- tcnicaragua[tcnicaragua$Date >= "2018-01-01", ]
# Eliminando fechas duplicadas
tcnicaragua <- tcnicaragua %>% distinct(Date, .keep_all = TRUE)
#Variable pais
tcnicaragua$Pais <- "Nicaragua"
# Anhadiendo las variables que faltan para el panel
tcnicaragua$PersonalCredit <- NA
tcnicaragua$SMEs <- NA
tcnicaragua$BusinessCredit <- NA
tcnicaragua$Leasing <- NA
#tcnicaragua$CommercialCredit <- NA
tcnicaragua$Government <- NA
#tcnicaragua$ConsumerCredit <- NA
tcnicaragua$Microcredit <- NA
#tcnicaragua$Mortgage <- NA
#tcnicaragua$CreditCard <- NA
tcnicaragua$id <- 7
#Con las variables de interes
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)
tcnicaragua <- tcnicaragua[keeps4]
tcnicaragua$year <- format(tcnicaragua$Date, "%Y")
# Uniendo old y latest------
old_tcnicaragua<- read_csv(paste(backup,"/old_tcnicaragua.csv",sep=""))
tcnicaragua <- rbind(tcnicaragua,old_tcnicaragua)                   
colnames(old_tcnicaragua)
colnames(tcnicaragua)
# Eliminando pasados duplicados---------
tcnicaragua <- tcnicaragua %>% distinct(Date, .keep_all = TRUE)
str(tcnicaragua)
# Ordenando los datos-------------------
tcnicaragua <- tcnicaragua[order(tcnicaragua$Date),]
#}else{break}
# If dataframe doesn't exists--------------
exists <- exists('tcnicaragua') && is.data.frame(get('tcnicaragua'))
if(exists==FALSE){ 
  tcnicaragua <- read_csv(paste(backup,"/old_tcnicaragua.csv",sep=""))   
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcnicaragua,paste(backup,"old_tcnicaragua.csv",sep="/"),
  row.names = FALSE
)


#*########## PANAMA ---------------------------------------
# Ultimos archivos descargados-------
todaym <- format(Sys.time(), "%m")
atodaym <- as.numeric(format(Sys.time(), "%m"))
todayy <- format(Sys.time(), "%Y")
year2 <- as.numeric(format(Sys.time(), "%Y"))
year <- as.numeric(todayy)%%100
atodaym <- atodaym-rezago #Dos meses antes
ctodaym <- atodaym+1 #1 mes despues se reporta
len <- nchar(gsub("\\D", "", ctodaym))
ctodaym <- ifelse(len==1,paste("0",ctodaym,sep=""),ctodaym)
month <- c(atodaym)
# Trabajando con el ultimo archivo disponible ------
# CreditCard-----------
pana <- paste(raw,"Panama_cred.xlsx",sep="/")
listsheets <- as.matrix(excel_sheets(pana))
if (file.exists(pana)){
  tcpana.cred <- read_excel(pana, sheet = listsheets[length(listsheets)-1]
                           , range = "A3:Z12")
# Transponiendo el dataframe ---
tcpanam <- tcpana.cred
tcpanam <- as.data.frame(t(as.matrix(tcpanam)))
# Renombrando variables ---
colnames(tcpanam)
names(tcpanam)[names(tcpanam) == "V1"] <- "mes"
names(tcpanam)[names(tcpanam) == "V6"] <- "Leasing"
names(tcpanam)[names(tcpanam) == "V7"] <- "CreditCard"
#Variable year
tcpanam$year <- year2
#Quedandose con las variables importantes
keeps <- c("Leasing",
           "CreditCard",
           "mes",
           "year")
tcpanama = tcpanam[keeps]
#Variable month
tcpanama$month[tcpanama$mes == "Enero"] <- c("Jan")
tcpanama$month[tcpanama$mes == "Febrero"] <- c("Feb")
tcpanama$month[tcpanama$mes == "Marzo"] <- c("Mar")
tcpanama$month[tcpanama$mes == "Abril"] <- c("Apr")
tcpanama$month[tcpanama$mes == "Mayo"] <- c("May")
tcpanama$month[tcpanama$mes == "Junio"] <- c("Jun")
tcpanama$month[tcpanama$mes == "Julio"] <- c("Jul")
tcpanama$month[tcpanama$mes == "Agosto"] <- c("Aug")
tcpanama$month[tcpanama$mes == "Septiembre"] <- c("Sep")
tcpanama$month[tcpanama$mes == "Octubre"] <- c("Oct")
tcpanama$month[tcpanama$mes == "Noviembre"] <- c("Nov")
tcpanama$month[tcpanama$mes == "Diciembre"] <- c("Dec")
#La variable "date"
tcpanama$date <-
  lubridate::ymd(paste0(tcpanama$year, tcpanama$month, "01"))
#Eliminando observaciones vacias ---
tcpanama <- tcpanama %>% drop_na(date)
#Eliminando los meses que tienen problemas
tcpanama <- tcpanama[tcpanama$date <= paste(year2,"-",month,"-","01",sep=""), ]
#variables de interes
keeps6 <- c("Leasing", "CreditCard")
tcpanama <- tcpanama[keeps6]
#De factor a numerico
cols = c(1:2)
tcpanama[, cols] = apply(tcpanama[, cols], 2, function(x)
  as.numeric(as.character(x)))
#Quedandonos con el ultmo valor disponible
filas <- nrow(tcpanama)
tcpanama_credit <- tcpanama[-c(1:filas-1),]

}else{break}
# Enterprises-----------
pana <- paste(raw,"Panama_empresas.xlsx",sep="/")
if(file.exists(pana)){
  tcpana.ent <- read_excel(pana, sheet = "Page1_1")
  # Transponiendo el dataframe ---
  tcpanam <- tcpana.ent
  tcpanam <- as.data.frame(t(as.matrix(tcpanam)))
  # Renombrando variables ---
  colnames(tcpanam)
  names(tcpanam)[names(tcpanam) == "V7"] <- "year"
  names(tcpanam)[names(tcpanam) == "V8"] <- "mes"
  names(tcpanam)[names(tcpanam) == "V9"] <- "Microcredit"
  names(tcpanam)[names(tcpanam) == "V10"] <- "peq.empresa"
  names(tcpanam)[names(tcpanam) == "V11"] <- "med.empresa"
  names(tcpanam)[names(tcpanam) == "V12"] <- "CommercialCredit"
  #Quedandose con las variables importantes
  keeps <- c("year",
             "mes",
             "Microcredit",
             "peq.empresa",
             "med.empresa",
             "CommercialCredit")
  tcpanama = tcpanam[keeps]
  #Variable month
  tcpanama$month <- ""
  tcpanama$month[tcpanama$mes == "Enero"] <- c("Jan")
  tcpanama$month[tcpanama$mes == "Febrero"] <- c("Feb")
  tcpanama$month[tcpanama$mes == "Marzo"] <- c("Mar")
  tcpanama$month[tcpanama$mes == "Abril"] <- c("Apr")
  tcpanama$month[tcpanama$mes == "Mayo"] <- c("May")
  tcpanama$month[tcpanama$mes == "Junio"] <- c("Jun")
  tcpanama$month[tcpanama$mes == "Julio"] <- c("Jul")
  tcpanama$month[tcpanama$mes == "Agosto"] <- c("Aug")
  tcpanama$month[tcpanama$mes == "Septiembre"] <- c("Sep")
  tcpanama$month[tcpanama$mes == "Octubre"] <- c("Oct")
  tcpanama$month[tcpanama$mes == "Noviembre"] <- c("Nov")
  tcpanama$month[tcpanama$mes == "Diciembre"] <- c("Dec")
  tcpanama <- tcpanama %>% drop_na(mes) 
  tcpanama <- tcpanama %>% fill(year, .direction = "down")
  #La variable "date"
  tcpanama$Date <-
    lubridate::ymd(paste0(tcpanama$year, tcpanama$month, "01"))
  #Eliminando los meses que tienen problemas
  tcpanama <- tcpanama[tcpanama$Date <= paste(year2,"-",month,"-","01",sep=""), ]
  #De factor a numerico
  cols = c(3:6)
  tcpanama[, cols] = apply(tcpanama[, cols], 2, function(x)
    as.numeric(as.character(x)))
  #Agregando variables
  tcpanama$SMEs <- tcpanama$peq.empresa+tcpanama$med.empresa
  #variables de interes
  keeps6 <- c("Date", "Microcredit",
              "SMEs","CommercialCredit")
  tcpanama_enter <- tcpanama[keeps6]
  #Quedandonos con el ultmo valor disponible
  filas <- nrow(tcpanama_enter)
  tcpanama_enter <- tcpanama_enter[-c(1:filas-1),]
}else{break}
# Mortgage----------
pana <- paste(raw,"Panama_mor.xlsx",sep="/")
listsheets2 <- as.matrix(excel_sheets(pana))
if (file.exists(pana)){
  tcpana.mor <- read_excel(pana, sheet = listsheets2[length(listsheets2)]
                           , range = "C8:D100")
# Renombrando variables ---
tcpana <- tcpana.mor
colnames(tcpana)
names(tcpana)[names(tcpana) == "CREDITO HIPOTECARIO"] <- amonth4
#Encontrando el maximo valor de una columna
tcpana$id <- 1
#Encontrando el valor maximo de credito ---
tcpana$maxvar <- ave(tcpana[,2], tcpana$id, FUN = function(x) max(x,na.rm=T))
#Mantener los valores finales
#install.packages("dplyr")
tcpanama <- filter(tcpana, maxvar == tcpana[,2])
# Transponiendo el dataframe ---
tcpanama <- as.data.frame(t(as.matrix(tcpanama)))
# Rename column names
colnames(tcpanama)
names(tcpanama)[names(tcpanama) == "V1"] <- "Mortgage"
#Variable year and month
tcpanama$year <- year2
tcpanama$month <- amonth4
#Eliminando observaciones no importantes
tcpanama_Mortgage <- tcpanama[-c(1:3),]
#La variable "date"
tcpanama_Mortgage$Date <-
  lubridate::ymd(paste0(
    tcpanama_Mortgage$year,
    tcpanama_Mortgage$month,
    "01"
  ))
#Con las variables importantes
keeps2 <- c("Date",
            "Mortgage")
tcpanama_Mortgage = tcpanama_Mortgage[keeps2]
}else{break}
# Consumer Credit----------
pana <- paste(raw,"Panama_con.xlsx",sep="/")
listsheets3 <- as.matrix(excel_sheets(pana))
if (file.exists(pana)){
  tcpana.cons <- read_excel(pana, sheet = listsheets3[length(listsheets3)]
                           , range = "C6:D100")
# Renombrando variables ---
tcpana <- tcpana.cons
colnames(tcpana)
names(tcpana)[names(tcpana) == "TOTAL CONSUMO"] <- amonth4
#Encontrando el maximo valor de una columna
tcpana$id <- 1
#Encontrando el valor maximo de credito ---
tcpana$maxvar <- ave(tcpana[,2], tcpana$id, FUN = function(x) max(x,na.rm=T))
#Mantener los valores finales
#install.packages("dplyr")
tcpanama <- filter(tcpana, maxvar == tcpana[,2])
# Transponiendo el dataframe ---
tcpanama <- as.data.frame(t(as.matrix(tcpanama)))
# Rename column names
colnames(tcpanama)
names(tcpanama)[names(tcpanama) == "V1"] <- "ConsumerCredit"
#Variable year and month
tcpanama$year <- year2
tcpanama$month <- atodaym
#Eliminando observaciones no importantes
tcpanama_ConsumerCredit <- tcpanama[-c(1:3),]
#La variable "date"
tcpanama_ConsumerCredit$Date <-
  lubridate::ymd(paste0(
    tcpanama_ConsumerCredit$year,
    tcpanama_ConsumerCredit$month,
    "01"
  ))
#Con las variables importantes
keeps2 <- c("Date",
            "ConsumerCredit")
tcpanama_ConsumerCredit = tcpanama_ConsumerCredit[keeps2]
}else{break}
# Uniendo los datos----
exists1 <- exists('tcpanama_credit') && is.data.frame(get('tcpanama_credit'))
exists2 <- exists('tcpanama_ConsumerCredit') && is.data.frame(get('tcpanama_ConsumerCredit'))
exists3 <- exists('tcpanama_Mortgage') && is.data.frame(get('tcpanama_Mortgage'))
exists4 <- exists('tcpanama_enter') && is.data.frame(get('tcpanama_enter'))
if(exists1==TRUE&&exists2==TRUE&&exists3==TRUE&&exists4==TRUE){
  #cbinPad function ------------------------------
  cbindPad <- function(...) {
    args <- list(...)
    n <- sapply(args, nrow)
    mx <- max(n)
    pad <- function(x, mx) {
      if (nrow(x) < mx) {
        nms <- colnames(x)
        padTemp <- matrix(NA, mx - nrow(x), ncol(x))
        colnames(padTemp) <- nms
        if (ncol(x) == 0) {
          return(padTemp)
        } else {
          return(rbind(x, padTemp))
        }
      }
      else{
        return(x)
      }
    }
    rs <- lapply(args, pad, mx)
    return(do.call(cbind, rs))
  }
  #Uniendo los dataframe-------------------
  tcpanama <- cbindPad(tcpanama_credit,
                         tcpanama_ConsumerCredit,
                         tcpanama_Mortgage,
                         tcpanama_enter)
}else{break}
# Anhdiendo las variables que faltan para el panel----
tcpanama$PersonalCredit <- NA
#tcpanama$SMEs <- NA
tcpanama$BusinessCredit <- NA
#tcpanama$Leasing <- NA
#tcpanama$CommercialCredit <- NA
tcpanama$Government <- NA
#tcpanama$ConsumerCredit <- NA
#tcpanama$Microcredit <- NA
#tcpanama$Mortgage <- NA
#tcpanama$CreditCard <- NA
tcpanama$Pais <- "Panama"
# Total de creditos
tcpanama$Total <- rowSums(tcpanama[,c(
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard")], na.rm=TRUE)

# Con las variables de interes
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)

tcpanama <- tcpanama[keeps4]
tcpanama$year <- format(tcpanama$Date, "%Y")

# Uniendo old y latest------
old_tcpanama<- read_csv(paste(backup,"/old_tcpanama.csv",sep=""))
tcpanama <- rbind(old_tcpanama,
                     tcpanama) 
colnames(old_tcpanama)
colnames(tcpanama)
# Eliminando pasados duplicados---------
tcpanama <- tcpanama %>% distinct(Date, .keep_all = TRUE)
# If dataframe doesn't exists--------------
exists <- exists('tcpanama') && is.data.frame(get('tcpanama'))
if(exists==FALSE){ 
  tcpanama <- read_csv(paste(backup,"/old_tcpanama.csv",sep=""))   
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcpanama,paste(backup,"old_tcpanama.csv",sep="/"),
  row.names = FALSE
)



#*########## PARAGUAY ---------------------------------------
# Trabajando con el ultimo dato disponible--------------
parag <- paste(raw,"Paraguay_latest.xlsx",sep="/")
if(file.exists(parag)){
# Reading the file -----------------------
  tcparag <-
  read_excel(parag , sheet = "3", range = "A5:ZZ30")
# Renombrando variables -----------------------
  # Comerciales---------------
  Comerciales <- tcparag[grepl("Comer",tcparag$...1),]
  Desarrollo <- tcparag[grepl("De",tcparag$...1),]
  Comerciales <-rbind(Comerciales,Desarrollo)
  Comerciales <- as.data.frame(t(as.matrix(Comerciales)))
  Comerciales <- Comerciales[-c(1:1),]
  cols = c(1:4)
  Comerciales[, cols] = apply(Comerciales[, cols], 2, function(x)
    as.numeric(as.character(x)))
  Comerciales$CommercialCredit <- Comerciales$V1+Comerciales$V2+
    Comerciales$V3+Comerciales$V4
  Comerciales <-
    rownames_to_column(Comerciales, var = "Date") %>% as_tibble()
  class(Comerciales$Date)
  Comerciales$Date <- as.numeric(Comerciales$Date)
  Comerciales$Date = openxlsx::convertToDate(Comerciales$Date)
  keepc <- c("Date","CommercialCredit")
  Comerciales <- Comerciales[keepc]
  #Eliminando observaciones vacias
  Comerciales <- Comerciales %>% drop_na(Date)
  # Consumo---------------
  Consumo <- tcparag[grepl("Cons",tcparag$...1),]
  Consumo <- as.data.frame(t(as.matrix(Consumo)))
  Consumo <- Consumo[-c(1:1),]
  cols = c(1:2)
  Consumo[, cols] = apply(Consumo[, cols], 2, function(x)
    as.numeric(as.character(x)))
  Consumo$ConsumerCredit <- Consumo$V1+Consumo$V2
  Consumo <-
    rownames_to_column(Consumo, var = "Date") %>% as_tibble()
  class(Consumo$Date)
  Consumo$Date <- as.numeric(Consumo$Date)
  Consumo$Date = openxlsx::convertToDate(Consumo$Date)
  keepc <- c("Date","ConsumerCredit")
  Consumo <- Consumo[keepc]
  #Eliminando observaciones vacias
  Consumo <- Consumo %>% drop_na(Date)
  # Vivienda---------------
  Vivienda <- tcparag[grepl("Viv",tcparag$...1),]
  Vivienda <- as.data.frame(t(as.matrix(Vivienda)))
  Vivienda <- Vivienda[-c(1:1),]
  cols = c(1:2)
  Vivienda[, cols] = apply(Vivienda[, cols], 2, function(x)
    as.numeric(as.character(x)))
  Vivienda$Mortgage <- Vivienda$V1+Vivienda$V2
  Vivienda <-
    rownames_to_column(Vivienda, var = "Date") %>% as_tibble()
  class(Vivienda$Date)
  Vivienda$Date <- as.numeric(Vivienda$Date)
  Vivienda$Date = openxlsx::convertToDate(Vivienda$Date)
  keepc <- c("Date","Mortgage")
  Vivienda <- Vivienda[keepc]
  #Eliminando observaciones vacias
  Vivienda <- Vivienda %>% drop_na(Date)
  # Tarjeta Credito---------------
  Tarjetas <- tcparag[grepl("Tarj",tcparag$...1),]
  Tarjetas <- as.data.frame(t(as.matrix(Tarjetas)))
  Tarjetas <-
    rownames_to_column(Tarjetas, var = "Date") %>% as_tibble()
  class(Tarjetas$Date)
  Tarjetas$CreditCard <- Tarjetas$V1
  Tarjetas$Date <- as.numeric(Tarjetas$Date)
  Tarjetas$Date = openxlsx::convertToDate(Tarjetas$Date)
  keepc <- c("Date","CreditCard")
  Tarjetas <- Tarjetas[keepc]
  Tarjetas <- Tarjetas[-c(1:1),]
  #Eliminando observaciones vacias
  Tarjetas <- Tarjetas %>% drop_na(Date)
  #Uniendo las bases de datos----------  
  tcparaguay <- merge(Comerciales, Consumo, by = "Date", all=TRUE)
  tcparaguay1 <- merge(Tarjetas, Vivienda, by = "Date", all=TRUE)
  tcparaguay <- merge(tcparaguay, tcparaguay1, by = "Date", all=TRUE)
# Eliminando filas vacias ----------------------------------
tcparaguay <- tcparaguay %>% drop_na(Date)
# Con las variables que interesan-----------
keeps <- c("Date",
           "CommercialCredit",
           "ConsumerCredit",
           "Mortgage",
           "CreditCard")
tcparaguay = tcparaguay[keeps]
#Variable pais
tcparaguay$Pais <- "Paraguay"
# Eliminando desde cierta Date ------
tcparaguay <- tcparaguay[tcparaguay$Date >= "2018-01-01", ]
#De factor a numerico
cols = c(2:5)
tcparaguay[, cols] = apply(tcparaguay[, cols], 2, function(x)
  as.numeric(as.character(x)))
# Anhadiendo las variables que faltan para el panel--------
tcparaguay$Total <- NA
tcparaguay$PersonalCredit <- NA
tcparaguay$SMEs <- NA
tcparaguay$BusinessCredit <- NA
tcparaguay$Leasing <- NA
#tcparaguay$CommercialCredit <- NA
tcparaguay$Government <- NA
#tcparaguay$ConsumerCredit <- NA
tcparaguay$Microcredit <- NA
#tcparaguay$Mortgage <- NA
#tcparaguay$CreditCard <- NA
tcparaguay$id <- 9
#Con las variables de interes
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)

tcparaguay <- tcparaguay[keeps4]
tcparaguay$year <- format(tcparaguay$Date, "%Y")

}else{break}
# If dataframe doesn't exists--------------
exists <- exists('tcparaguay') && is.data.frame(get('tcparaguay'))
if(exists==FALSE){ 
  tcpanama <- read_csv(paste(backup,"/old_tcparaguay.csv",sep=""))   
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcparaguay,paste(backup,"old_tcparaguay.csv",sep="/"),
  row.names = FALSE
)






#*########## PERU ---------------------------------------
# Ultimos archivos descargados-------
todaym <- format(Sys.time(), "%m")
atodaym <- as.numeric(format(Sys.time(), "%m"))
todayy <- format(Sys.time(), "%Y")
year2 <- as.numeric(format(Sys.time(), "%Y"))
year <- as.numeric(todayy)%%100
atodaym <- atodaym-rezago-1 #Dos meses antes
ctodaym <- atodaym+1 #1 mes despues se reporta
len <- nchar(gsub("\\D", "", ctodaym))
ctodaym <- ifelse(len==1,paste("0",ctodaym,sep=""),ctodaym)
month <- c(atodaym)
#
amonth=ifelse(atodaym==1,"Enero",atodaym)
amonth=ifelse(atodaym==2,"Febrero",amonth)
amonth=ifelse(atodaym==3,"Marzo",amonth)
amonth=ifelse(atodaym==4,"Abril",amonth)
amonth=ifelse(atodaym==5,"Mayo",amonth)
amonth=ifelse(atodaym==6,"Junio",amonth)
amonth=ifelse(atodaym==7,"Julio",amonth)
amonth=ifelse(atodaym==8,"Agosto",amonth)
amonth=ifelse(atodaym==9,"Septiembre",amonth)
amonth=ifelse(atodaym==10,"Octubre",amonth)
amonth=ifelse(atodaym==11,"Noviembre",amonth)
amonth=ifelse(atodaym==12,"Diciembre",amonth)
amonth
amonth1=ifelse((atodaym-1)==1,"Enero",atodaym)
amonth1=ifelse((atodaym-1)==2,"Febrero",amonth1)
amonth1=ifelse((atodaym-1)==3,"Marzo",amonth1)
amonth1=ifelse((atodaym-1)==4,"Abril",amonth1)
amonth1=ifelse((atodaym-1)==5,"Mayo",amonth1)
amonth1=ifelse((atodaym-1)==6,"Junio",amonth1)
amonth1=ifelse((atodaym-1)==7,"Julio",amonth1)
amonth1=ifelse((atodaym-1)==8,"Agosto",amonth1)
amonth1=ifelse((atodaym-1)==9,"Septiembre",amonth1)
amonth1=ifelse((atodaym-1)==10,"Octubre",amonth1)
amonth1=ifelse((atodaym-1)==11,"Noviembre",amonth1)
amonth1=ifelse((atodaym-1)==12,"Diciembre",amonth1)
amonth1

amonth2=ifelse(atodaym==1,"Ene",atodaym)
amonth2=ifelse(atodaym==2,"Feb",amonth2)
amonth2=ifelse(atodaym==3,"Mar",amonth2)
amonth2=ifelse(atodaym==4,"Abr",amonth2)
amonth2=ifelse(atodaym==5,"May",amonth2)
amonth2=ifelse(atodaym==6,"Jun",amonth2)
amonth2=ifelse(atodaym==7,"Jul",amonth2)
amonth2=ifelse(atodaym==8,"Ago",amonth2)
amonth2=ifelse(atodaym==9,"Sep",amonth2)
amonth2=ifelse(atodaym==10,"Oct",amonth2)
amonth2=ifelse(atodaym==11,"Nov",amonth2)
amonth2=ifelse(atodaym==12,"Dic",amonth2)
amonth2
amonth3=ifelse(atodaym==1,"ene",atodaym)
amonth3=ifelse(atodaym==2,"feb",amonth3)
amonth3=ifelse(atodaym==3,"mar",amonth3)
amonth3=ifelse(atodaym==4,"abr",amonth3)
amonth3=ifelse(atodaym==5,"may",amonth3)
amonth3=ifelse(atodaym==6,"jun",amonth3)
amonth3=ifelse(atodaym==7,"jul",amonth3)
amonth3=ifelse(atodaym==8,"ago",amonth3)
amonth3=ifelse(atodaym==9,"sept",amonth3)
amonth3=ifelse(atodaym==10,"oct",amonth3)
amonth3=ifelse(atodaym==11,"nov",amonth3)
amonth3=ifelse(atodaym==12,"dic",amonth3)
amonth3
amonth3a=ifelse((atodaym-1)==1,"ene",atodaym)
amonth3a=ifelse((atodaym-1)==2,"feb",amonth3a)
amonth3a=ifelse((atodaym-1)==3,"mar",amonth3a)
amonth3a=ifelse((atodaym-1)==4,"abr",amonth3a)
amonth3a=ifelse((atodaym-1)==5,"may",amonth3a)
amonth3a=ifelse((atodaym-1)==6,"jun",amonth3a)
amonth3a=ifelse((atodaym-1)==7,"jul",amonth3a)
amonth3a=ifelse((atodaym-1)==8,"ago",amonth3a)
amonth3a=ifelse((atodaym-1)==9,"sept",amonth3a)
amonth3a=ifelse((atodaym-1)==10,"oct",amonth3a)
amonth3a=ifelse((atodaym-1)==11,"nov",amonth3a)
amonth3a=ifelse((atodaym-1)==12,"dic",amonth3a)
amonth3a

amonth4=ifelse(atodaym==1,"ene",atodaym)
amonth4=ifelse(atodaym==2,"feb",amonth4)
amonth4=ifelse(atodaym==3,"mar",amonth4)
amonth4=ifelse(atodaym==4,"apr",amonth4)
amonth4=ifelse(atodaym==5,"may",amonth4)
amonth4=ifelse(atodaym==6,"jun",amonth4)
amonth4=ifelse(atodaym==7,"jul",amonth4)
amonth4=ifelse(atodaym==8,"aug",amonth4)
amonth4=ifelse(atodaym==9,"sep",amonth4)
amonth4=ifelse(atodaym==10,"oct",amonth4)
amonth4=ifelse(atodaym==11,"nov",amonth4)
amonth4=ifelse(atodaym==12,"dec",amonth4)
amonth4

amonth_p=ifelse(atodaym==1,"Enero",atodaym)
amonth_p=ifelse(atodaym==2,"Febrero",amonth_p)
amonth_p=ifelse(atodaym==3,"Marzo",amonth_p)
amonth_p=ifelse(atodaym==4,"Abril",amonth_p)
amonth_p=ifelse(atodaym==5,"Mayo",amonth_p)
amonth_p=ifelse(atodaym==6,"Junio",amonth_p)
amonth_p=ifelse(atodaym==7,"Julio",amonth_p)
amonth_p=ifelse(atodaym==8,"Agosto",amonth_p)
amonth_p=ifelse(atodaym==9,"Setiembre",amonth_p)
amonth_p=ifelse(atodaym==10,"Octubre",amonth_p)
amonth_p=ifelse(atodaym==11,"Noviembre",amonth_p)
amonth_p=ifelse(atodaym==12,"Diciembre",amonth_p)
amonth_p
amonthi=ifelse(atodaym==1,"Jan",atodaym)
amonthi=ifelse(atodaym==2,"Feb",amonthi)
amonthi=ifelse(atodaym==3,"Mar",amonthi)
amonthi=ifelse(atodaym==4,"Apr",amonthi)
amonthi=ifelse(atodaym==5,"May",amonthi)
amonthi=ifelse(atodaym==6,"Jun",amonthi)
amonthi=ifelse(atodaym==7,"Jul",amonthi)
amonthi=ifelse(atodaym==8,"Aug",amonthi)
amonthi=ifelse(atodaym==9,"Sep",amonthi)
amonthi=ifelse(atodaym==10,"Oct",amonthi)
amonthi=ifelse(atodaym==11,"Nov",amonthi)
amonthi=ifelse(atodaym==12,"Dec",amonthi)
amonthi
amonthio=ifelse(atodaym==(1+1),"Jan",atodaym)
amonthio=ifelse(atodaym==2+1,"Feb",amonthio)
amonthio=ifelse(atodaym==3+1,"Mar",amonthio)
amonthio=ifelse(atodaym==4+1,"Apr",amonthio)
amonthio=ifelse(atodaym==5+1,"May",amonthio)
amonthio=ifelse(atodaym==6+1,"Jun",amonthio)
amonthio=ifelse(atodaym==7+1,"Jul",amonthio)
amonthio=ifelse(atodaym==8+1,"Aug",amonthio)
amonthio=ifelse(atodaym==9+1,"Sep",amonthio)
amonthio=ifelse(atodaym==10+1,"Oct",amonthio)
amonthio=ifelse(atodaym==11+1,"Nov",amonthio)
amonthio=ifelse(atodaym==12+1,"Dec",amonthio)
amonthio
# Trabajando con el ultimo archivo disponible-----------------
month_peru <- (substr(amonth_p, 1, 3))
year_peru <- as.numeric(year)%%100
zipdf <- unzip(paste(raw,"Peru_latest.zip",sep="/"), list = TRUE)
zipdf[1]
perus <- paste(raw,zipdf[1],sep="/")
#if(file.exists(perus)){
# Tipo de credito--------------------------------------
tcpe <-
  read_excel(perus, sheet = "Créditos x tipo", range = "A6:J17")
#Renombrando columnas
colnames(tcpe)
names(tcpe)[names(tcpe) == "Tipo de Crédito"] <- "Saldo"
names(tcpe)[names(tcpe) == "Total"] <- amonthi
keeps <- c("Saldo",
           amonthi)
tcpe = tcpe[keeps]
#Eliminando observaciones vacias ---
tcpe <- tcpe %>% drop_na(Saldo)
# Transponiendo el dataframe ---
tcpe <- as.data.frame(t(as.matrix(tcpe)))
# Rename column names
colnames(tcpe)
names(tcpe)[names(tcpe) == "V1"] <- "Corporativos"
names(tcpe)[names(tcpe) == "V2"] <- "BusinessCredit"
names(tcpe)[names(tcpe) == "V3"] <- "Mediana_emp"
names(tcpe)[names(tcpe) == "V4"] <- "Peque_emp"
names(tcpe)[names(tcpe) == "V5"] <- "Microcredit"
names(tcpe)[names(tcpe) == "V6"] <- "ConsumerCredit"
names(tcpe)[names(tcpe) == "V7"] <- "Mortgage"
names(tcpe)[names(tcpe) == "V8"] <- "Total"
#Con las variables importantes
keeps2 <- c( "Corporativos",
  "BusinessCredit",
  "Mediana_emp",
  "Peque_emp",
  "Microcredit",
  "ConsumerCredit",
  "Mortgage",
  "Total"
)
tcpe = tcpe[keeps2]
#Eliminando las filas no importantes
tcperuo <- tcpe[-c(1:1),]
#Variable year
tcperuo$year <- year2
tcperuo$month <- amonthi
#La variable "date"
tcperuo$Date <-
  format(lubridate::ymd(paste0(tcperuo$year, tcperuo$month, "01")), "%m-%Y")
# Tarjeta de credito--------------
#Reading the file
tcpe <- read_excel(perus, sheet = "Ctas SSFF", range = "A38:F39")
#Eliminando las filas no importantes
colnames(tcpe)
names(tcpe)[names(tcpe) == "...1"] <- "Saldo"
names(tcpe)[names(tcpe) == "...6"] <- amonthi
keeps <- c("Saldo",
           amonthi)
tcpe = tcpe[keeps]
#Eliminando observaciones vacias ---
tcpe <- tcpe %>% drop_na(Saldo)
# Transponiendo el dataframe ---
tcperut <- as.data.frame(t(as.matrix(tcpe)))
# Rename column names
colnames(tcperut)
names(tcperut)[names(tcperut) == "V1"] <- "CreditCard"
#Variable year
tcperut$year <- year2
tcperut$month <- amonthi
#Eliminando las filas no importantes
tcperut <- tcperut[-c(1:1),]
#La variable "date"
tcperut$Date <-
  format(lubridate::ymd(paste0(tcperut$year, tcperut$month, "01")), "%m-%Y")
# cbinPad function ------------------------------
cbindPad <- function(...) {
  args <- list(...)
  n <- sapply(args, nrow)
  mx <- max(n)
  pad <- function(x, mx) {
    if (nrow(x) < mx) {
      nms <- colnames(x)
      padTemp <- matrix(NA, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x) == 0) {
        return(padTemp)
      } else {
        return(rbind(x, padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args, pad, mx)
  return(do.call(cbind, rs))
}

# Add datasets Horinzontally-----------------------------------
tcperu <- cbindPad(tcperuo, tcperut)
# Variables importantes----------------
keeps3 <-
  c("Date",
    "BusinessCredit",
    "Microcredit",
    "Mortgage",
    "CreditCard",
    "Corporativos",
    "Mediana_emp",
    "Peque_emp",
    #"CommercialCredit",
    "ConsumerCredit",
    "Total"
  )

tcperu <- tcperu[keeps3]
class(tcperu$date)
colnames(tcperu)

# De factor a numerico-----------------
class(tcperu$CreditCard)
cols = c(2:10)
tcperu[, cols] = apply(tcperu[, cols], 2, function(x)
  as.numeric(as.character(x)))
tcperu$CreditCard <- as.numeric(as.character(tcperu$CreditCard))
# Agregando variables--------------------
tcperu$SMEs <-
  rowSums(tcperu[, c("Mediana_emp", "Peque_emp")], na.rm = TRUE)
tcperu$BusinessCredit <-
  rowSums(tcperu[, c("BusinessCredit", "Corporativos")], na.rm = TRUE)

# Anhadiendo las variables que faltan para el panel--------
tcperu$Pais <- "Peru"
tcperu$PersonalCredit <- NA
#tcperu$SMEs <- NA
#tcperu$BusinessCredit <- NA
tcperu$Leasing <- NA
tcperu$CommercialCredit <- NA
tcperu$Government <- NA
#tcperu$ConsumerCredit <- NA
#tcperu$Microcredit <- NA
#tcperu$Mortgage <- NA
#tcperu$CreditCard <- NA
#tcperu$Total <- NA
tcperu$id <- 10
#Con las variables de interes
colnames(tcperu)
tcperu$Date <- tcperu$Date3  
tcperu$Date <-
  lubridate::ymd(paste(year2,	amonthi,	01))
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)

tcperu <- tcperu[keeps4]
tcperu$year <- format(tcperu$Date, "%Y")

# Restando el credito de consumo----
tcperu$ConsumerCredit <- tcperu$ConsumerCredit-tcperu$CreditCard
# Old data and latest------------------------------
old_tcperu <- read.csv(paste(backup,"/old_tcperu.csv",sep=""))
class(old_tcperu$Date)
old_tcperu$Date <- ymd(old_tcperu$Date)
tcperu <- rbind(tcperu,old_tcperu)
# Ordenando los datos-------------------
tcperu <- tcperu[order(tcperu$Date),]
# Eliminando pasados duplicados---------
tcperu <- tcperu %>% distinct(Date, .keep_all = TRUE)
#Sorting by Data
str(tcperu)
#}else{break}
# If dataframe doesn't exists--------------
exists <- exists('tcperu') && is.data.frame(get('tcperu'))
if(exists==FALSE){ 
  tcperu <- read_csv(paste(backup,"/old_tcperu.csv",sep=""))   
}else{break}
# Exportando --------------------------------------------
write.csv(
  tcperu,paste(backup,"old_tcperu.csv",sep="/"),
  row.names = FALSE
)




#*########## VENEZUELA --------------------------------
# Ultimos archivos descargados-------
todaym <- format(Sys.time(), "%m")
atodaym <- as.numeric(format(Sys.time(), "%m"))
todayy <- format(Sys.time(), "%Y")
year2 <- as.numeric(format(Sys.time(), "%Y"))
year <- as.numeric(todayy)%%100
atodaym <- atodaym-rezago-1 #Dos meses antes
ctodaym <- atodaym+1 #1 mes despues se reporta
len <- nchar(gsub("\\D", "", ctodaym))
ctodaym <- ifelse(len==1,paste("0",ctodaym,sep=""),ctodaym)
month <- c(atodaym)
#
amonth=ifelse(atodaym==1,"Enero",atodaym)
amonth=ifelse(atodaym==2,"Febrero",amonth)
amonth=ifelse(atodaym==3,"Marzo",amonth)
amonth=ifelse(atodaym==4,"Abril",amonth)
amonth=ifelse(atodaym==5,"Mayo",amonth)
amonth=ifelse(atodaym==6,"Junio",amonth)
amonth=ifelse(atodaym==7,"Julio",amonth)
amonth=ifelse(atodaym==8,"Agosto",amonth)
amonth=ifelse(atodaym==9,"Septiembre",amonth)
amonth=ifelse(atodaym==10,"Octubre",amonth)
amonth=ifelse(atodaym==11,"Noviembre",amonth)
amonth=ifelse(atodaym==12,"Diciembre",amonth)
amonth
amonth1=ifelse((atodaym-1)==1,"Enero",atodaym)
amonth1=ifelse((atodaym-1)==2,"Febrero",amonth1)
amonth1=ifelse((atodaym-1)==3,"Marzo",amonth1)
amonth1=ifelse((atodaym-1)==4,"Abril",amonth1)
amonth1=ifelse((atodaym-1)==5,"Mayo",amonth1)
amonth1=ifelse((atodaym-1)==6,"Junio",amonth1)
amonth1=ifelse((atodaym-1)==7,"Julio",amonth1)
amonth1=ifelse((atodaym-1)==8,"Agosto",amonth1)
amonth1=ifelse((atodaym-1)==9,"Septiembre",amonth1)
amonth1=ifelse((atodaym-1)==10,"Octubre",amonth1)
amonth1=ifelse((atodaym-1)==11,"Noviembre",amonth1)
amonth1=ifelse((atodaym-1)==12,"Diciembre",amonth1)
amonth1

amonth2=ifelse(atodaym==1,"Ene",atodaym)
amonth2=ifelse(atodaym==2,"Feb",amonth2)
amonth2=ifelse(atodaym==3,"Mar",amonth2)
amonth2=ifelse(atodaym==4,"Abr",amonth2)
amonth2=ifelse(atodaym==5,"May",amonth2)
amonth2=ifelse(atodaym==6,"Jun",amonth2)
amonth2=ifelse(atodaym==7,"Jul",amonth2)
amonth2=ifelse(atodaym==8,"Ago",amonth2)
amonth2=ifelse(atodaym==9,"Sep",amonth2)
amonth2=ifelse(atodaym==10,"Oct",amonth2)
amonth2=ifelse(atodaym==11,"Nov",amonth2)
amonth2=ifelse(atodaym==12,"Dic",amonth2)
amonth2
amonth3=ifelse(atodaym==1,"ene",atodaym)
amonth3=ifelse(atodaym==2,"feb",amonth3)
amonth3=ifelse(atodaym==3,"mar",amonth3)
amonth3=ifelse(atodaym==4,"abr",amonth3)
amonth3=ifelse(atodaym==5,"may",amonth3)
amonth3=ifelse(atodaym==6,"jun",amonth3)
amonth3=ifelse(atodaym==7,"jul",amonth3)
amonth3=ifelse(atodaym==8,"ago",amonth3)
amonth3=ifelse(atodaym==9,"sept",amonth3)
amonth3=ifelse(atodaym==10,"oct",amonth3)
amonth3=ifelse(atodaym==11,"nov",amonth3)
amonth3=ifelse(atodaym==12,"dic",amonth3)
amonth3
amonth3a=ifelse((atodaym-1)==1,"ene",atodaym)
amonth3a=ifelse((atodaym-1)==2,"feb",amonth3a)
amonth3a=ifelse((atodaym-1)==3,"mar",amonth3a)
amonth3a=ifelse((atodaym-1)==4,"abr",amonth3a)
amonth3a=ifelse((atodaym-1)==5,"may",amonth3a)
amonth3a=ifelse((atodaym-1)==6,"jun",amonth3a)
amonth3a=ifelse((atodaym-1)==7,"jul",amonth3a)
amonth3a=ifelse((atodaym-1)==8,"ago",amonth3a)
amonth3a=ifelse((atodaym-1)==9,"sept",amonth3a)
amonth3a=ifelse((atodaym-1)==10,"oct",amonth3a)
amonth3a=ifelse((atodaym-1)==11,"nov",amonth3a)
amonth3a=ifelse((atodaym-1)==12,"dic",amonth3a)
amonth3a

amonth4=ifelse(atodaym==1,"ene",atodaym)
amonth4=ifelse(atodaym==2,"feb",amonth4)
amonth4=ifelse(atodaym==3,"mar",amonth4)
amonth4=ifelse(atodaym==4,"apr",amonth4)
amonth4=ifelse(atodaym==5,"may",amonth4)
amonth4=ifelse(atodaym==6,"jun",amonth4)
amonth4=ifelse(atodaym==7,"jul",amonth4)
amonth4=ifelse(atodaym==8,"aug",amonth4)
amonth4=ifelse(atodaym==9,"sep",amonth4)
amonth4=ifelse(atodaym==10,"oct",amonth4)
amonth4=ifelse(atodaym==11,"nov",amonth4)
amonth4=ifelse(atodaym==12,"dec",amonth4)
amonth4

amonth_p=ifelse(atodaym==1,"Enero",atodaym)
amonth_p=ifelse(atodaym==2,"Febrero",amonth_p)
amonth_p=ifelse(atodaym==3,"Marzo",amonth_p)
amonth_p=ifelse(atodaym==4,"Abril",amonth_p)
amonth_p=ifelse(atodaym==5,"Mayo",amonth_p)
amonth_p=ifelse(atodaym==6,"Junio",amonth_p)
amonth_p=ifelse(atodaym==7,"Julio",amonth_p)
amonth_p=ifelse(atodaym==8,"Agosto",amonth_p)
amonth_p=ifelse(atodaym==9,"Setiembre",amonth_p)
amonth_p=ifelse(atodaym==10,"Octubre",amonth_p)
amonth_p=ifelse(atodaym==11,"Noviembre",amonth_p)
amonth_p=ifelse(atodaym==12,"Diciembre",amonth_p)
amonth_p

# Trabajando el ultimo documento--------------
zipdf <- unzip(paste(raw,"Venezuela_latest.zip",sep="/"), list = TRUE)
zipdf[2,1]
venezue <- paste(raw,zipdf[2,1],sep="/")
if(file.exists(venezue)){
tcvenezuela <-
  read_excel(venezue, sheet = "Serie Dest.Créd.", range = "A5:N9000")
# Renombrando variables --------------------------------------------
colnames(tcvenezuela) # get column names

# Rename column names
names(tcvenezuela)[names(tcvenezuela) == "...1"] <- "Date"
names(tcvenezuela)[names(tcvenezuela) == "...2"] <- "CommercialCredit"
names(tcvenezuela)[names(tcvenezuela) == "...3"] <- "CreditCard"
names(tcvenezuela)[names(tcvenezuela) == "...4"] <- "ConsumoCuotas"
names(tcvenezuela)[names(tcvenezuela) == "...5"] <- "ConsumerCredit"
names(tcvenezuela)[names(tcvenezuela) == "...6"] <- "Vehiculos"
names(tcvenezuela)[names(tcvenezuela) == "...8"] <- "Mortgage"
names(tcvenezuela)[names(tcvenezuela) == "...9"] <- "Microcredit"
names(tcvenezuela)[names(tcvenezuela) == "...10"] <- "Agropecuario"
names(tcvenezuela)[names(tcvenezuela) == "...11"] <- "Turismo"
names(tcvenezuela)[names(tcvenezuela) == "...12"] <- "Industria"
names(tcvenezuela)[names(tcvenezuela) == "...13"] <- "Provision"
names(tcvenezuela)[names(tcvenezuela) == "...14"] <- "Total"
tcvenezuela

#From 
class(tcvenezuela$CommercialCredit)
tcvenezuela$CommercialCredit <- as.numeric(as.POSIXct(tcvenezuela$CommercialCredit))
tcvenezuela$CreditCard <- as.numeric(as.POSIXct(tcvenezuela$CreditCard))
tcvenezuela$ConsumerCredit <- as.numeric(as.POSIXct(tcvenezuela$ConsumerCredit))
tcvenezuela$Mortgage <- as.numeric(as.POSIXct(tcvenezuela$Mortgage))
tcvenezuela$Microcredit <- as.numeric(as.POSIXct(tcvenezuela$Microcredit))
tcvenezuela$Total <- as.numeric(as.POSIXct(tcvenezuela$Total))

# Variable mes numeral----------------------------------
tcvenezuela$id[tcvenezuela$Date == "ENERO"] <- c(1)
tcvenezuela$id[tcvenezuela$Date == "FEBRERO"] <- c(2)
tcvenezuela$id[tcvenezuela$Date == "MARZO"] <- c(3)
tcvenezuela$id[tcvenezuela$Date == "ABRIL"] <- c(4)
tcvenezuela$id[tcvenezuela$Date == "MAYO"] <- c(5)
tcvenezuela$id[tcvenezuela$Date == "JUNIO"] <- c(6)
tcvenezuela$id[tcvenezuela$Date == "JULIO"] <- c(7)
tcvenezuela$id[tcvenezuela$Date == "AGOSTO"] <- c(8)
tcvenezuela$id[tcvenezuela$Date == "SEPTIEMBRE"] <- c(9)
tcvenezuela$id[tcvenezuela$Date == "OCTUBRE"] <- c(10)
tcvenezuela$id[tcvenezuela$Date == "NOVIEMBRE"] <- c(11)
tcvenezuela$id[tcvenezuela$Date == "DICIEMBRE"] <- c(12)

tcvenezuela$month[tcvenezuela$Date == "ENERO"] <- c("Jan")
tcvenezuela$month[tcvenezuela$Date == "FEBRERO"] <- c("Feb")
tcvenezuela$month[tcvenezuela$Date == "MARZO"] <- c("Mar")
tcvenezuela$month[tcvenezuela$Date == "ABRIL"] <- c("Apr")
tcvenezuela$month[tcvenezuela$Date == "MAYO"] <- c("May")
tcvenezuela$month[tcvenezuela$Date == "JUNIO"] <- c("Jun")
tcvenezuela$month[tcvenezuela$Date == "JULIO"] <- c("Jul")
tcvenezuela$month[tcvenezuela$Date == "AGOSTO"] <- c("Aug")
tcvenezuela$month[tcvenezuela$Date == "SEPTIEMBRE"] <- c("Sep")
tcvenezuela$month[tcvenezuela$Date == "OCTUBRE"] <- c("Oct")
tcvenezuela$month[tcvenezuela$Date == "NOVIEMBRE"] <- c("Nov")
tcvenezuela$month[tcvenezuela$Date == "DICIEMBRE"] <- c("Dec")

#Variable anho
tcvenezuela$id.t <-
  with(tcvenezuela, ave(rep(1, nrow(tcvenezuela)), id, FUN = seq_along))

#Cambiando los valores del id
tcvenezuela$year <- NA
tcvenezuela$year[tcvenezuela$id.t == 1] <- c(2017)
tcvenezuela$year[tcvenezuela$id.t == 2] <- c(2018)
tcvenezuela$year[tcvenezuela$id.t == 3] <- c(2019)
tcvenezuela$year[tcvenezuela$id.t == 4] <- c(2020)
tcvenezuela$year[tcvenezuela$id.t == 5] <- c(2021)
tcvenezuela$year[tcvenezuela$id.t == 6] <- c(2022)
tcvenezuela$year[tcvenezuela$id.t == 7] <- c(2023)
tcvenezuela$year[tcvenezuela$id.t == 8] <- c(2024)
tcvenezuela$year[tcvenezuela$id.t == 9] <- c(2025)
tcvenezuela$year[tcvenezuela$id.t == 10] <- c(2026)
tcvenezuela$year[tcvenezuela$id.t == 11] <- c(2027)
tcvenezuela$year[tcvenezuela$id.t == 12] <- c(2028)
tcvenezuela$year[tcvenezuela$id.t == 13] <- c(2029)
tcvenezuela$year[tcvenezuela$id.t == 14] <- c(2030)
tcvenezuela$year[tcvenezuela$id.t == 15] <- c(2031)
tcvenezuela$year[tcvenezuela$id.t == 16] <- c(2032)
tcvenezuela$year[tcvenezuela$id.t == 17] <- c(2033)
tcvenezuela$year[tcvenezuela$id.t == 18] <- c(2034)
tcvenezuela$year[tcvenezuela$id.t == 19] <- c(2035)
tcvenezuela$year[tcvenezuela$id.t == 20] <- c(2036)
tcvenezuela$year[tcvenezuela$id.t == 21] <- c(2037)
tcvenezuela$year[tcvenezuela$id.t == 22] <- c(2038)
tcvenezuela$year[tcvenezuela$id.t == 23] <- c(2039)
tcvenezuela$year[tcvenezuela$id.t == 24] <- c(2040)
# Borrando date----------------
tcvenezuelau <- tcvenezuela %>% drop_na(Date)
tcvenezuelau <- tcvenezuela %>% drop_na(month)
tcvenezuela <- tcvenezuelau
# La variable "date"-------------
tcvenezuela$Date <-
  lubridate::ymd(paste0(tcvenezuela$year, tcvenezuela$month, "01"))
# Con las variables que interesan ---------------------------------------------
keeps <- c(
  "Date",
  "CommercialCredit",
  "CreditCard",
  "ConsumerCredit",
  "Mortgage",
  "Microcredit",
  "Total"
)
tcvenezuela = tcvenezuela[keeps]

# Variable pais----------
tcvenezuela$Pais <- "Venezuela"
class(tcvenezuela$Microcredit)
# Eliminando desde cierta Date ------
tcvenezuela <- tcvenezuela[tcvenezuela$Date >= "2018-01-01", ]
# Anhadiendo las variables que faltan para el panel--------
tcvenezuela$PersonalCredit <- NA
tcvenezuela$SMEs <- NA
tcvenezuela$BusinessCredit <- NA
tcvenezuela$Leasing <- NA
#tcvenezuela$CommercialCredit <- NA
tcvenezuela$Government <- NA
#tcvenezuela$ConsumerCredit <- NA
#tcvenezuela$Microcredit <- NA
#tcvenezuela$Mortgage <- NA
#tcvenezuela$CreditCard <- NA
tcvenezuela$id <- 11
#Con las variables de interes
keeps4 <- c(
  "Date",
  "Pais",
  "Mortgage",
  "ConsumerCredit",
  "PersonalCredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "Government",
  "Microcredit",
  "CreditCard",
  "Total"
)
tcvenezuela <- tcvenezuela[keeps4]
tcvenezuela$year <- format(tcvenezuela$Date, "%Y")
}else{break}
# If dataframe doesn't exists--------------
exists <- exists('tcvenezuela') && is.data.frame(get('tcvenezuela'))
if(exists==FALSE){ 
  tcperu <- read_csv(paste(backup,"/old_tcvenezuela.csv",sep=""))   
}else{break}

# Exportando --------------------------------------------
write.csv(
  tcvenezuela,paste(backup,"old_tcvenezuela.csv",sep="/"),
  row.names = FALSE
)




#*######################## APPEND PAISES---------------------
#*Append dataframes---------
CreditType <- rbind(
  tcargentina,
  tcbolivia,
  tcbrasil,
  tcchile,
  tccolombiam,
  tccostaricam,
  tcrepdominicana,
  tcecuador,
  tcelsalvador,
  tcguatemala,
  tchonduras,
  tcmexico,
  tcpanama,
  tcparaguay,
  tcperu,
  tcnicaragua
  #,
  #tcvenezuela
)

keeps10 <- c(
  "Date",
  "year",
  "Pais",
  "CreditCard",
  "PersonalCredit",
  "Mortgage",
  "Microcredit",
  "SMEs",
  "BusinessCredit",
  "Leasing",
  "CommercialCredit",
  "ConsumerCredit",
  "Government",
  "Total"
)
CreditType <- CreditType[keeps10]
rm(list=setdiff(ls(), "CreditType"))
# If dataframe doesn't exists--------------
exists <- exists('CreditType') && is.data.frame(get('CreditType'))
if(exists==FALSE){ 
  tcargentina <- read.csv("Data/Backup/old_tcargentina.csv") 
  tcbolivia <- read.csv("Data/Backup/old_tcbolivia.csv") 
  tcbrasil <- read.csv("Data/Backup/old_tcbrasil.csv")
  tcchile <- read.csv("Data/Backup/old_tcchile.csv")
  tccolombiam <- read.csv("Data/Backup/old_tccolombia.csv")
  tccostaricam <- read.csv("Data/Backup/old_tccostaricam.csv")
  tcrepdominicana <- read.csv("Data/Backup/old_tcrepdominicana.csv")
  tcecuador <- read.csv("Data/Backup/old_tcecuador.csv")
  tcelsalvador <- read.csv("Data/Backup/old_tcelsalvador.csv")
  tcguatemala <- read.csv("Data/Backup/old_tcguatemala.csv")
  tchonduras <- read.csv("Data/Backup/old_tchonduras.csv")
  tcmexico <- read.csv("Data/Backup/old_tcmexico.csv")
  tcnicaragua <- read.csv("Data/Backup/old_tcnicaragua.csv")
  tcpanama <- read.csv("Data/Backup/old_tcpanama.csv")
  tcparaguay <- read.csv("Data/Backup/old_tcparaguay.csv")
  tcperu <- read.csv("Data/Backup/old_tcperu.csv")
  
  CreditType <- rbind(
    tcargentina,
    tcbolivia,
    tcbrasil,
    tcchile,
    tccolombiam,
    tccostaricam,
    tcrepdominicana,
    tcecuador,
    tcelsalvador,
    tcguatemala,
    tchonduras,
    tcmexico,
    tcpanama,
    tcparaguay,
    tcperu,
    tcnicaragua
    #,
    #tcvenezuela
  )
  
  keeps10 <- c(
    "Date",
    "year",
    "Pais",
    "CreditCard",
    "PersonalCredit",
    "Mortgage",
    "Microcredit",
    "SMEs",
    "BusinessCredit",
    "Leasing",
    "CommercialCredit",
    "ConsumerCredit",
    "Government",
    "Total"
  )
  CreditType <- CreditType[keeps10]
  
}else{break}
names(CreditType)[names(CreditType) == "Pais"] <- "Country"
class(CreditType$Microcredit)
# Transformations-------------------------
cols = c(4:14);    
CreditType[,cols] = apply(CreditType[,cols], 2, function(x) as.numeric(as.character(x)));
#
CreditType$Totalem <- rowSums(CreditType[, c(9, 11)], na.rm=TRUE)
CreditType$Totalem <-
  rowSums(CreditType[, c("BusinessCredit", "CommercialCredit")], na.rm = TRUE)
CreditType <-mutate(CreditType,Totalem=ifelse(Totalem==0,NA,Totalem))

########################## Analizando resultados----------
# Verificando valores-----------
# NEW----
#Variables importantes
keepf <- c("Date" ,            "Country" ,        
           "CreditCard" ,  "Mortgage",        
           "Microcredit",      "SMEs"          , 
           "ConsumerCredit" , 
           "Totalem")
Credit.e <- CreditType[keepf]
#Date in formato date
Credit.e$Date <- as.Date(Credit.e$Date, format = "%Y-%m-%d")
#Observaciones hasta junio 2020
Credit.e <- filter(Credit.e,Date<"2020-07-01")
#De factor a numerico
cols = c(3:6);    
Credit.e[,cols] = apply(Credit.e[,cols], 2, function(x) as.numeric(as.character(x)));
#suma filas
sumrowsm <- function(df) {
  require(dplyr)
  y <- select_if(df, is_numeric)
  rowSums(y, na.rm=T)
}
Credit.e$sumas <- rowSums(Credit.e[,4:8],na.rm = T)
#suma columnas
columnss1 <- aggregate(Credit.e$sumas,
                       by=list(Country=Credit.e$Country), FUN=sum)
# OLD----
#Opening the file
tables <-  paste(getwd(),"/Outputs/Tables",sep="")
old_CreditType <- read_excel(paste(tables,"CreditType.backup.xlsx",sep="/"))
#Variables importantes
old_CreditType <- old_CreditType[keepf]
#Variable date
old_CreditType$Date <- as.Date(old_CreditType$Date, format = "%Y-%m-%d")
# Eliminando observaciones vacias
old_CreditType <- old_CreditType %>% drop_na(Date)
#Desde junio de 2020
old.CreditType.e <- filter(old_CreditType,Date<"2020-07-01")
#De factor a numerico
cols = c(3:6);    
Credit.e[,cols] = apply(Credit.e[,cols], 2, function(x) as.numeric(as.character(x)));
#suma filas
sumrowsm <- function(df) {
  require(dplyr)
  y <- select_if(df, is_numeric)
  rowSums(y, na.rm=T)
}
old.CreditType.e$sumas <- rowSums(old.CreditType.e[,4:8],na.rm = T)
#suma columnas
columnss2 <- aggregate(old.CreditType.e$sumas,
                       by=list(Country=old.CreditType.e$Country), FUN=sum)
# Analisis----
analisis <- merge(columnss1,
                  columnss2,
                  by = "Country", all=TRUE)
class(analisis$x.x)
analisis$Differences.backup <- round(analisis$x.x-analisis$x.y)
keep.s <- c("Country","x.x","Differences.backup")
analisis <- data.frame(analisis[keep.s])
# Table summarize---------
CreditType$Date <- as.Date(CreditType$Date, format = "%Y-%m-%d")
# De factor a numerico
class(CreditType$Mortgage)
cols = c(4:14)
CreditType[, cols] = apply(CreditType[, cols], 2, function(x)
  as.numeric(as.character(x)))
#Last date by country
col1 <- data.frame(old_CreditType %>%
                     group_by(Country) %>%
                     summarise(old.date = max(Date), old.obser = n()))
col2 <- data.frame(CreditType %>%
                     group_by(Country) %>%
                     summarise(new.date = max(Date), new.obser = n()))
table.summary <- merge(col1,col2,
                       by = "Country", all=TRUE)
table.summary <- merge(table.summary,
                       analisis,
                       by = "Country", all=TRUE)
table.summary$Updated.obser <- table.summary$new.obser-table.summary$old.obser
keeps9 <- c('Country',"Differences.backup","old.date","old.obser","new.date","new.obser",
            "Updated.obser")
table.summary <- table.summary[keeps9]
colnames(table.summary)[2] <- "(1)"
colnames(table.summary)[3] <- "(2)"
colnames(table.summary)[4] <- "(3)"
colnames(table.summary)[5] <- "(4)"
colnames(table.summary)[6] <- "(5)"
colnames(table.summary)[7] <- "(6)"
png("Outputs/Tables/Table.summary.png")
p <- tableGrob(table.summary)
grid.arrange(top = "Results summary", p)
grid::grid.draw(p)
dev.off()

######################### INDEX18------------------------
#INDEX18---------------------------------------------
#Para todos los paises-------------
#Identificador por pais
CreditType$year2 <- 2018
CreditType$year3 <- 2019
CreditType$year4 <- 2020
#for(i in range(CreditType$year)){
CreditType <-mutate(CreditType,id1=ifelse(year2==year,1,NA))
CreditType$id <- CreditType %>% group_indices(id1,Country)
CreditType$idc <- CreditType %>% group_indices(Country)
CreditType <-mutate(CreditType,id2=ifelse(id<=16,id,NA))
#cierre del anho 2018
CreditType$CreditCard18 <- ave(CreditType$CreditCard, CreditType$id2, FUN = last)
CreditType$PersonalCredit18 <- ave(CreditType$PersonalCredit, CreditType$id2, FUN = last)
CreditType$Mortgage18 <- ave(CreditType$Mortgage, CreditType$id2, FUN = last)
CreditType$SMEs18 <- ave(CreditType$SMEs, CreditType$id2, FUN = last)
CreditType$Microcredit18 <- ave(CreditType$Microcredit, CreditType$id2, FUN = last)
CreditType$BusinessCredit18 <- ave(CreditType$BusinessCredit, CreditType$id2, FUN = last)
CreditType$CommercialCredit18 <- ave(CreditType$CommercialCredit, CreditType$id2, FUN = last)
CreditType$Leasing18 <- ave(CreditType$Leasing, CreditType$id2, FUN = last)
CreditType$ConsumerCredit18 <- ave(CreditType$ConsumerCredit, CreditType$id2, FUN = last)
CreditType$Government18 <- ave(CreditType$Government, CreditType$id2, FUN = last)
CreditType$Total18 <- ave(CreditType$Total, CreditType$id2, FUN = last)
CreditType$Totalem18 <- ave(CreditType$Totalem, CreditType$id2, FUN = last)

# Con los valores importantes 2018
CreditType <-mutate(CreditType,CreditCard18=ifelse(id2<16,CreditCard18,NA))
CreditType <-mutate(CreditType,PersonalCredit18=ifelse(id2<16,PersonalCredit18,NA))
CreditType <-mutate(CreditType,Mortgage18=ifelse(id2<16,Mortgage18,NA))
CreditType <-mutate(CreditType,SMEs18=ifelse(id2<16,SMEs18,NA))
CreditType <-mutate(CreditType,Microcredit18=ifelse(id2<16,Microcredit18,NA))
CreditType <-mutate(CreditType,BusinessCredit18=ifelse(id2<16,BusinessCredit18,NA))
CreditType <-mutate(CreditType,Leasing18=ifelse(id2<16,Leasing18,NA))
CreditType <-mutate(CreditType,ConsumerCredit18=ifelse(id2<16,ConsumerCredit18,NA))
CreditType <-mutate(CreditType,Government18=ifelse(id2<16,Government18,NA))
CreditType <-mutate(CreditType,Total18=ifelse(id2<16,Total18,NA))
CreditType <-mutate(CreditType,CommercialCredit18=ifelse(id2<16,CommercialCredit18,NA))
CreditType <-mutate(CreditType,Totalem18=ifelse(id2<16,Totalem18,NA))

# Rellenar los valores 2018
CreditType <- CreditType %>% group_by(idc) %>% fill(CreditCard18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(PersonalCredit18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(Mortgage18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(SMEs18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(Microcredit18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(BusinessCredit18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(Leasing18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(ConsumerCredit18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(Government18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(Total18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(CommercialCredit18, .direction = "down")
CreditType <- CreditType %>% group_by(idc) %>% fill(Totalem18, .direction = "down")
#Para Honduras----------------
#Para el anho 2019
CreditType <-mutate(CreditType,idh=ifelse(year3==year,1,NA))
CreditType$id19 <- CreditType %>% group_indices(idh,Country)
CreditType$idc19 <- CreditType %>% group_indices(Country)
CreditType <-mutate(CreditType,id3=ifelse(id19<17,id19,NA))
#cierre del anho 2019
CreditType$CreditCard19 <- ave(CreditType$CreditCard, CreditType$id3, FUN = last)
CreditType$PersonalCredit19 <- ave(CreditType$PersonalCredit, CreditType$id3, FUN = last)
CreditType$Mortgage19 <- ave(CreditType$Mortgage, CreditType$id3, FUN = last)
CreditType$SMEs19 <- ave(CreditType$SMEs, CreditType$id3, FUN = last)
CreditType$Microcredit19 <- ave(CreditType$Microcredit, CreditType$id3, FUN = last)
CreditType$BusinessCredit19 <- ave(CreditType$BusinessCredit, CreditType$id3, FUN = last)
CreditType$CommercialCredit19 <- ave(CreditType$CommercialCredit, CreditType$id3, FUN = last)
CreditType$Leasing19 <- ave(CreditType$Leasing, CreditType$id3, FUN = last)
CreditType$ConsumerCredit19 <- ave(CreditType$ConsumerCredit, CreditType$id3, FUN = last)
CreditType$Government19 <- ave(CreditType$Government, CreditType$id3, FUN = last)
CreditType$Total19 <- ave(CreditType$Total, CreditType$id3, FUN = last)
CreditType$Totalem19 <- ave(CreditType$Totalem, CreditType$id3, FUN = last)

# Con los valores importantes 2019
CreditType <-mutate(CreditType,CreditCard19=ifelse(id19<17,CreditCard19,NA))
CreditType <-mutate(CreditType,PersonalCredit19=ifelse(id19<17,PersonalCredit19,NA))
CreditType <-mutate(CreditType,Mortgage19=ifelse(id19<17,Mortgage19,NA))
CreditType <-mutate(CreditType,SMEs19=ifelse(id19<17,SMEs19,NA))
CreditType <-mutate(CreditType,Microcredit19=ifelse(id19<17,Microcredit19,NA))
CreditType <-mutate(CreditType,BusinessCredit19=ifelse(id19<17,BusinessCredit19,NA))
CreditType <-mutate(CreditType,Leasing19=ifelse(id19<17,Leasing19,NA))
CreditType <-mutate(CreditType,ConsumerCredit19=ifelse(id19<17,ConsumerCredit19,NA))
CreditType <-mutate(CreditType,Government19=ifelse(id19<17,Government19,NA))
CreditType <-mutate(CreditType,Total19=ifelse(id19<17,Total19,NA))
CreditType <-mutate(CreditType,CommercialCredit19=ifelse(id19<17,CommercialCredit19,NA))
CreditType <-mutate(CreditType,Totalem19=ifelse(id19<17,Totalem19,NA))

# Rellenar los valores 2019
CreditType <- CreditType %>% group_by(idc19) %>% fill(CreditCard19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(PersonalCredit19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Mortgage19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(SMEs19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Microcredit19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(BusinessCredit19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Leasing19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(ConsumerCredit19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Government19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Total19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(CommercialCredit19, .direction = "down")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Totalem19, .direction = "down")
#
CreditType <- CreditType %>% group_by(idc19) %>% fill(CreditCard19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(PersonalCredit19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Mortgage19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(SMEs19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Microcredit19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(BusinessCredit19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Leasing19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(ConsumerCredit19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Government19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Total19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(CommercialCredit19, .direction = "up")
CreditType <- CreditType %>% group_by(idc19) %>% fill(Totalem19, .direction = "up")

# Con los valores importantes 2019
CreditType <-mutate(CreditType,CreditCard18=ifelse(Country=="Honduras",CreditCard19,CreditCard18))
CreditType <-mutate(CreditType,PersonalCredit18=ifelse(Country=="Honduras",PersonalCredit19,PersonalCredit18))
CreditType <-mutate(CreditType,Mortgage18=ifelse(Country=="Honduras",Mortgage19,Mortgage18))
CreditType <-mutate(CreditType,SMEs18=ifelse(Country=="Honduras",SMEs19,SMEs18))
CreditType <-mutate(CreditType,Microcredit18=ifelse(Country=="Honduras",Microcredit19,Microcredit18))
CreditType <-mutate(CreditType,BusinessCredit18=ifelse(Country=="Honduras",BusinessCredit19,BusinessCredit18))
CreditType <-mutate(CreditType,Leasing18=ifelse(Country=="Honduras",Leasing19,Leasing18))
CreditType <-mutate(CreditType,ConsumerCredit18=ifelse(Country=="Honduras",ConsumerCredit19,ConsumerCredit18))
CreditType <-mutate(CreditType,Government18=ifelse(Country=="Honduras",Government19,Government18))
CreditType <-mutate(CreditType,Total18=ifelse(Country=="Honduras",Total19,Total18))
CreditType <-mutate(CreditType,CommercialCredit18=ifelse(Country=="Honduras",CommercialCredit19,CommercialCredit18))
CreditType <-mutate(CreditType,Totalem18=ifelse(Country=="Honduras",Totalem19,Totalem18))

#Para PANAMA--------------------
#Para el anho 2020
CreditType <-mutate(CreditType,idh=ifelse(year4==year,1,NA))
CreditType$id20 <- CreditType %>% group_indices(idh,Country)
CreditType$idc20 <- CreditType %>% group_indices(Country)
CreditType <-mutate(CreditType,id4=ifelse(id20<17,id20,NA))
#cierre del anho 2020
CreditType$SMEs20 <- ave(CreditType$SMEs, CreditType$id20, FUN = first)
CreditType$Microcredit20 <- ave(CreditType$Microcredit, CreditType$id20, FUN = first)
CreditType$CommercialCredit20 <- ave(CreditType$CommercialCredit, CreditType$id20, FUN = first)
CreditType$Totalem20 <- ave(CreditType$Totalem, CreditType$id20, FUN = first)

# Con los valores importantes 2020
CreditType <-mutate(CreditType,SMEs20=ifelse(id20<17,SMEs20,NA))
CreditType <-mutate(CreditType,Microcredit20=ifelse(id20<17,Microcredit20,NA))
CreditType <-mutate(CreditType,CommercialCredit20=ifelse(id20<17,CommercialCredit20,NA))
CreditType <-mutate(CreditType,Totalem20=ifelse(id20<17,Totalem20,NA))

# Rellenar los valores 2020
CreditType <- CreditType %>% group_by(idc20) %>% fill(SMEs20, .direction = "up")
CreditType <- CreditType %>% group_by(idc20) %>% fill(Microcredit20, .direction = "up")
CreditType <- CreditType %>% group_by(idc20) %>% fill(CommercialCredit20, .direction = "up")
CreditType <- CreditType %>% group_by(idc20) %>% fill(Totalem20, .direction = "up")
# Con los valores importantes 2020
CreditType <-mutate(CreditType,SMEs18=ifelse(Country=="Panama",SMEs20,SMEs18))
CreditType <-mutate(CreditType,Microcredit18=ifelse(Country=="Panama",Microcredit20,Microcredit18))
CreditType <-mutate(CreditType,CommercialCredit18=ifelse(Country=="Panama",CommercialCredit20,CommercialCredit18))
CreditType <-mutate(CreditType,Totalem18=ifelse(Country=="Panama",Totalem20,Totalem18))

#El indice para todos los paises-------------
# Dividiendo cada mes entre el cierre de 2018
CreditType$iCreditCard18 <- (CreditType$CreditCard/CreditType$CreditCard18)*100
CreditType$iPersonalCredit18 <- (CreditType$PersonalCredit/CreditType$PersonalCredit18)*100
CreditType$iMortgage18 <- (CreditType$Mortgage/CreditType$Mortgage18)*100
CreditType$iSMEs18 <- (CreditType$SMEs/CreditType$SMEs18)*100
CreditType$iMicrocredit18 <- (CreditType$Microcredit/CreditType$Microcredit18)*100
CreditType$iBusinessCredit18 <- (CreditType$BusinessCredit/CreditType$BusinessCredit18)*100
CreditType$iCommercialCredit18 <- (CreditType$CommercialCredit/CreditType$CommercialCredit18)*100
CreditType$iLeasing18 <- (CreditType$Leasing/CreditType$Leasing18)*100
CreditType$iConsumerCredit18 <- (CreditType$ConsumerCredit/CreditType$ConsumerCredit18)*100
CreditType$iGovernment18 <- (CreditType$Government/CreditType$Government18)*100
CreditType$iTotal18 <- (CreditType$Total/CreditType$Total18)*100
CreditType$iTotalem18 <- (CreditType$Totalem/CreditType$Totalem18)*100



######################## INDEX18 deflactado---------------
#Trabajando el IPC---------
ipc <- read_excel("Data/Clean/IPC.xlsx")
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
#Creando un identificador para el merge------
ipc$myeark <- format(ipc$Date, "%Y-%m")
ipc$key <- paste(ipc$countryi,ipc$myeark)
colnames(ipc)
keep.ipc <- c("key","ipc","iipc18")
ipc <- ipc[keep.ipc]
#
CreditType$myeark <- format(CreditType$Date, "%Y-%m")
CreditType$key <- paste(CreditType$Country,CreditType$myeark)
#Merging data-----------
CreditType <- merge(CreditType,
                    ipc,
                    by = "key", all=TRUE)
#Indice 18ipc deflactado-------------
CreditType$iCreditCard18ipc <- CreditType$iipc18*CreditType$iCreditCard18
CreditType$iPersonalCredit18ipc <- CreditType$iipc18*CreditType$iPersonalCredit18
CreditType$iMortgage18ipc <- CreditType$iipc18*CreditType$iMortgage18
CreditType$iSMEs18ipc <- CreditType$iipc18*CreditType$iSMEs18
CreditType$iMicrocredit18ipc <- CreditType$iipc18*CreditType$iMicrocredit18
CreditType$iBusinessCredit18ipc <- CreditType$iipc18*CreditType$iBusinessCredit18
CreditType$iCommercialCredit18ipc <- CreditType$iipc18*CreditType$iCommercialCredit18
CreditType$iLeasing18ipc <- CreditType$iipc18*CreditType$iLeasing18
CreditType$iConsumerCredit18ipc <- CreditType$iipc18*CreditType$iConsumerCredit18
CreditType$iGovernment18ipc <- CreditType$iipc18*CreditType$iGovernment18
CreditType$iTotal18ipc <- CreditType$iipc18*CreditType$iTotal18
CreditType$iTotalem18ipc <- CreditType$iipc18*CreditType$iTotalem18



####################### Exportando-----------
#Filtrando las variables importantes-------
colnames(CreditType)
keep.final <- c("Date", "Country" ,              
                "CreditCard",             
                "Mortgage",               "Microcredit"  ,         
                "SMEs",                   
                "ConsumerCredit",         
                "Totalem" ,              
                "iCreditCard18",
                "iMortgage18" ,           "iSMEs18" ,              
                "iMicrocredit18" ,        
                "iConsumerCredit18"  ,    
                "iTotalem18" ,
                "iCreditCard18ipc",      
                "iMortgage18ipc",        
                "iSMEs18ipc" ,            "iMicrocredit18ipc",
                "iConsumerCredit18ipc" , 
                "iTotalem18ipc" 
)

CreditType.final <- CreditType[keep.final]
# Eliminando valores vacios
CreditType.final <- CreditType.final %>% drop_na(Date)
#Exportando el archivo final--------
write.csv(
  CreditType.final,paste("Data/Clean/CreditType.final.csv",sep="/"),
  row.names = FALSE
)
#install.packages("writexl")
write_xlsx(
  CreditType.final,paste("Data/Clean/CreditType.final.xlsx",sep="/"))


