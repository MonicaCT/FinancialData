#Project: Loading data
#Author:Monica Cueto
#Setting directory----
rm(list = ls())
loading <- paste(here::here(),"/Data/Raw",sep="")
nauto <-  paste(getwd(),"/Data/Nauto",sep="")
#election monthly lag----
rezago <- 2
#Packages-------------
pacman::p_load(dplyr,glue,here,httr,openxlsx,plyr,readxl,rvest,
               tabulizer,tidyverse,XLConnect,grid,data.table,
               ggplot2, gridExtra, Hmisc, lubridate,
               padr, readr, rio, stringr, writexl,
               XML, reshape2)
#*########## BOLIVIA ---------------------------------------
#Main url to search 
main_url <- "https://www.asoban.bo/publications-page/3"
sec_url <- "https://www.asoban.bo"
#Searching urls to download
html_url <- 
  main_url %>% 
  read_html()
#
xlsx_urls <- 
  html_nodes(html_url, 
             css = "ul, a") %>% 
  html_attr('href') %>% 
  str_subset('.pdf') %>% 
  str_subset('/Rep')
xlsx_urls %>% length()
#Downloading the files
urlsc <- paste(sec_url,xlsx_urls,sep = "")
#Non
for (i in 4) {
  download.file(urlsc[i],
                #destfile = paste0(basename(urlsc[i])),#para descargar todos los files
                destfile = paste0(nauto,"/bolivia_latest.pdf",sep=""),#para descargar el ultimo file
                mode = "wb")
}
#Extracting the table
out <- extract_tables(paste0(nauto,"/bolivia_latest.pdf",sep=""))
final <- do.call(rbind, out[length(out)])
final <- as.data.frame(final[3:nrow(final), ])
#If the file exists
Bolivia.file <- paste0(nauto,"/bolivia_latest.pdf",sep="")
if (file.exists(Bolivia.file)){
    Bolivia.updated="Done"}else{Bolivia.updated="Failed"}
Bolivia.updated<-as.data.frame(Bolivia.updated)
colnames(Bolivia.updated)[1] <- "Updated"
Bolivia.updated$Country <- "Bolivia"

#*########## BRASIL ---------------------------------------
todaym <- format(Sys.time(), "%m")
todayy <- as.numeric(format(Sys.time(), "%Y"))%%100
today <- as.numeric(paste(todayy,todaym,sep=""))-rezago
urls <- paste("https://www.bcb.gov.br/ftp/notaecon",paste("ni20",as.character(today),"pmp.zip",sep=""),sep = "/")
for (url in urls) {
  download.file(urls, destfile = paste(here::here(),"/Data/Raw/",basename(urls),sep=""))
}
# Unzip documents
# Get all the zip files - latest
creditt <- file.info(list.files(loading, full.names = T))
outDir <- rownames(creditt)[which.max(creditt$mtime)]
unzip(outDir, exdir = loading)
file.rename(("Data/Raw/Notimp2.xlsx"), paste0("Data/Raw/Brasilu.xlsx"))

#If the file exists
Brasil.file <- paste0("Data/Raw/Brasilu.xlsx")
if (file.exists(Brasil.file)){
  Brasil.updated="Done"}else{Brasil.updated="Failed"}
Brasil.updated<-as.data.frame(Brasil.updated)
colnames(Brasil.updated)[1] <- "Updated"
Brasil.updated$Country <- "Brasil"

#*########## CHILE ---------------------------------------
#rm(list = ls())
url <- "https://best-sbif-api.azurewebsites.net/CuadroExcel/?Tag=SBIF_DEUD_CCS_TRMD_MM$&Orientacion=V&TodosLosElementos=true&&nocache=1604407028355"
download.file(url, destfile = paste(loading,"/ConsumerCredit.xlsx",sep=""), mode = "wb")
url <- "https://best-sbif-api.azurewebsites.net/CuadroExcel/?Tag=SBIF_DEUD_CHV_TRMD_MM$&Orientacion=V&TodosLosElementos=true&&nocache=1604407195885"
download.file(url, destfile = paste(loading,"/Hipotecaria.xlsx",sep=""), mode = "wb")
url <- "https://best-sbif-api.azurewebsites.net/CuadroExcel/?Tag=SBIF_DEUD_CCO_TRMD_MM$&Orientacion=V&TodosLosElementos=true&&nocache=1604407304316"
download.file(url, destfile = paste(loading,"/CommercialCredit.xlsx",sep=""), mode = "wb")
url <- "https://best-sbif-api.azurewebsites.net/CuadroExcel/?Tag=SBIF_CART_CCO_TAMDEU_MM$_MONT&Orientacion=V&TodosLosElementos=true&&nocache=1607902525043"
download.file(url, destfile = paste(loading,"/EnterprisesCredit.xlsx",sep=""), mode = "wb")

#If the file exists
Chile.file1 <- paste(loading,"/ConsumerCredit.xlsx",sep="")
Chile.file2 <- paste(loading,"/Hipotecaria.xlsx",sep="")
Chile.file3 <- paste(loading,"/CommercialCredit.xlsx",sep="")
Chile.file4 <- paste(loading,"/EnterprisesCredit.xlsx",sep="")
#
if (file.exists(Chile.file1)&&file.exists(Chile.file2)
    &&file.exists(Chile.file3)&&file.exists(Chile.file4)){
  Chile.updated="Done"}else{Chile.updated="Failed"}
Chile.updated<-as.data.frame(Chile.updated)
colnames(Chile.updated)[1] <- "Updated"
Chile.updated$Country <- "Chile"

#*########## COLOMBIA ---------------------------------------
#Main url to search 
main_url <- "https://www.superfinanciera.gov.co/inicio/informes-y-cifras/cifras/establecimientos-de-credito/informacion-periodica/mensual/evolucion-cartera-de-creditos-60950"
sec_url <- "https://www.superfinanciera.gov.co"
# 2. searching urls to download -
html_url <- 
  main_url %>% 
  read_html()
xlsx_urls <- 
  html_nodes(html_url, 
             css = "ul, a") %>% 
  html_attr('href') %>% 
  str_subset('.xlsx') %>% 
  str_subset('/ca')
xlsx_urls %>% length()
#Downloading the files
urlsc <- paste(sec_url,xlsx_urls,sep = "")
#Non
for (i in 1) {
  download.file(urlsc[i],
                #destfile = paste0(basename(urlsc[i])),#para descargar todos los files
                destfile = paste0(loading,"/colombialatest.xlsx",sep=""),#para descargar el ultimo file
                mode = "wb")
}
#If the file exists
Colombia.file <- paste0(loading,"/colombialatest.xlsx",sep="")
if (file.exists(Colombia.file)){
  Colombia.updated="Done"}else{Colombia.updated="Failed"}
Colombia.updated<-as.data.frame(Colombia.updated)
colnames(Colombia.updated)[1] <- "Updated"
Colombia.updated$Country <- "Colombia"

#*########## COSTA RICA ------------------------------
todaym <- as.numeric(format(Sys.time(), "%m"))-rezago-1
year <- as.numeric(format(Sys.time(), "%Y"))
todayy <- as.numeric(format(Sys.time(), "%Y"))%%100
dayf=NA
dayf=ifelse(todaym==1,"31",dayf)
dayf=ifelse(todaym==2,"28",dayf)
dayf=ifelse(todaym==3,"31",dayf)
dayf=ifelse(todaym==4,"30",dayf)
dayf=ifelse(todaym==5,"31",dayf)
dayf=ifelse(todaym==6,"30",dayf)
dayf=ifelse(todaym==7,"31",dayf)
dayf=ifelse(todaym==8,"31",dayf)
dayf=ifelse(todaym==9,"30",dayf)
dayf=ifelse(todaym==10,"31",dayf)
dayf=ifelse(todaym==11,"30",dayf)
dayf=ifelse(todaym==12,"31",dayf)
list <- paste("https://gee.bccr.fi.cr/indicadoreseconomicos/Cuadros/frmVerCatCuadro.aspx?CodCuadro=144&Idioma=1&FecInicial=1997/12/31&FecFinal=",paste(year,todaym,dayf,sep="/"),"&Filtro=0&Exportar=True",sep = "")
for (i in 1:length(list)) {
  download.file(list[i],
                destfile = paste(loading,"/crica_latest.xls",sep=""),
                mode = "wb")
}
#If the file exists
CostaRica.file <- paste(loading,"/crica_latest.xls",sep="")
if (file.exists(CostaRica.file)){
  CostaRica.updated="Done"}else{CostaRica.updated="Failed"}
CostaRica.updated<-as.data.frame(CostaRica.updated)
colnames(CostaRica.updated)[1] <- "Updated"
CostaRica.updated$Country <- "Costa Rica"

#*########## DOMINICAN REPUBLIC ----------------------
list <-
  c(
    #"https://www.sib.gob.do/sites/default/files/nuevosdocumentos/estadisticas/seriestiempo/D-Cartera-de-Creditos_0.xlsx"
    "https://sb.gob.do/sites/default/files/nuevosdocumentos/estadisticas/seriestiempo/D-Cartera-de-Creditos.xlsx"
    )
for (i in 1:length(list)) {
  download.file(list[i],
                destfile = paste0(loading,"/RepDominicana.xlsx",sep=""),
                mode = "wb")
}
#If the file exists
DomRepublic.file <- paste0(loading,"/RepDominicana.xlsx",sep="")
if (file.exists(DomRepublic.file)){
  DomRepublic.updated="Done"}else{DomRepublic.updated="Failed"}
DomRepublic.updated<-as.data.frame(DomRepublic.updated)
colnames(DomRepublic.updated)[1] <- "Updated"
DomRepublic.updated$Country <- "Republica Dom"

#*########## ECUADOR ---------------------------------------
#Main url to search 
main_url <- "http://estadisticas.superbancos.gob.ec/portalestadistico/portalestudios/?page_id=415"
#sec_url <- "https://estadisticas.superbancos.gob.ec/portalestadistico/portalestudios/wp-content/uploads/sites/4/downloads"
# 2. searching urls to download -
html_url <- 
  main_url %>% 
  read_html()
xlsx_urls <- 
  html_nodes(html_url, 
             css = "ul, a") %>% 
  html_attr('href') %>% 
  str_subset('.zip') %>% 
  str_subset('/BOL')
#Downloading the files
xlsx_urlss =sort(xlsx_urls,decreasing = TRUE)
#
for (i in (length(xlsx_urlss[1]))) {
  download.file(xlsx_urlss[1],
                #destfile = paste0(basename(xlsx_urls[i])),#para descargar todos los files
                destfile = paste(loading,"Ecuador_latest.zip",sep="/"),#para descargar el ultimo file
                mode = "wb")
}
#If the file exists
Ecuador.file <- paste(loading,"Ecuador_latest.zip",sep="/")
if (file.exists(Ecuador.file)){
  Ecuador.updated="Done"}else{Ecuador.updated="Failed"}
Ecuador.updated<-as.data.frame(Ecuador.updated)
colnames(Ecuador.updated)[1] <- "Updated"
Ecuador.updated$Country <- "Ecuador"

#*########## EL SALVADOR ---------------------------------------
todaym <- as.numeric(format(Sys.time(), "%m"))-rezago
todayy <- as.character(as.numeric(format(Sys.time(), "%Y"))%%100)
today <- paste(todayy,todaym,sep="")
len <- nchar(gsub("\\D", "", todaym))
todaym <- ifelse(len==1,paste("0",todaym,sep=""),todaym)
main_urls <- "https://ssf.gob.sv/descargas/balances/xls"
download.file(paste(main_urls,paste("s_sa_",todaym,todayy,".xls",sep=""),sep="/"),
              destfile = paste(loading,"/ElSalvador_latest.xls",sep=""), mode = "wb")

#If the file exists
ElSalvador.file <- paste(loading,"/ElSalvador_latest.xls",sep="")
if (file.exists(ElSalvador.file)){
  ElSalvador.updated="Done"}else{ElSalvador.updated="Failed"}
ElSalvador.updated<-as.data.frame(ElSalvador.updated)
colnames(ElSalvador.updated)[1] <- "Updated"
ElSalvador.updated$Country <- "El Salvador"

#*########## NICARAGUA ---------------------------------------
list <-
  c(
   "https://www.bcn.gob.ni/estadisticas/monetario_financiero/financiero/sociedades_financieras/5-10.xls"
    )
for (i in 1:length(list)) {
  download.file(list[i],
                destfile = paste(loading,"/nicaragua_latest.xls",sep=""),
                mode = "wb")
}
#If the file exists
Nicaragua.file <- paste(loading,"/nicaragua_latest.xls",sep="")
if (file.exists(Nicaragua.file)){
  Nicaragua.updated="Done"}else{Nicaragua.updated="Failed"}
Nicaragua.updated<-as.data.frame(Nicaragua.updated)
colnames(Nicaragua.updated)[1] <- "Updated"
Nicaragua.updated$Country <- "Nicaragua"

#*########## PANAMA ---------------------------------------
#Commercial credit
todaym <- as.numeric(format(Sys.time(), "%m"))-rezago
year <- format(Sys.time(), "%Y")
date <- as.character(todaym)
len <- nchar(gsub("\\D", "", date))
date <- ifelse(len==1,paste("0",date,sep=""),date)
urls <- paste("https://www.superbancos.gob.pa/superbancos/documentos/financiera_y_estadistica/reportes_estadisticos",year,date,"cartera_sectorial_trimestral/RE-RANKING-en-RE0022.xlsx",sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Panama_com.xlsx",sep=""),
                mode = "wb")
}
#Mortgage credit
urls <- paste("https://www.superbancos.gob.pa/superbancos/documentos/financiera_y_estadistica/reportes_estadisticos",year,date,"cartera_sectorial_trimestral/RE-RANKING-en-RE0023.xlsx",sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Panama_mor.xlsx",sep=""),
                mode = "wb")
}
#Consummer credit
urls <- paste("https://www.superbancos.gob.pa/superbancos/documentos/financiera_y_estadistica/reportes_estadisticos",year,date,"cartera_sectorial_trimestral/RE-RANKING-en-RE0024.xlsx",sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Panama_con.xlsx",sep=""),
                mode = "wb")
}
#Credit card
urls <- paste("https://www.superbancos.gob.pa/superbancos/documentos/financiera_y_estadistica/reportes_estadisticos",year,date,"cartera_sectorial_trimestral/Creditos_Facilidad.xlsx",sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Panama_cred.xlsx",sep=""),
                mode = "wb")
}
#Enterprises
urls <- paste("https://www.superbancos.gob.pa/superbancos/documentos/financiera_y_estadistica/reportes_estadisticos/2020/10/cartera_sectorial/RE-CREDITO-en-RE0035.xlsx",sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Panama_empresas.xlsx",sep=""),
                mode = "wb")
}

#If the file exists
Panama.file1 <- paste(loading,"/Panama_com.xlsx",sep="")
Panama.file2 <- paste(loading,"/Panama_mor.xlsx",sep="")
Panama.file3 <- paste(loading,"/Panama_con.xlsx",sep="")
Panama.file4 <- paste(loading,"/Panama_cred.xlsx",sep="")
Panama.file5 <- paste(loading,"/Panama_empresas.xlsx",sep="")
#
if (file.exists(Panama.file1)&&file.exists(Panama.file2)
    &&file.exists(Panama.file3)&&file.exists(Panama.file4)
    &&file.exists(Panama.file5)){
  Panama.updated="Done"}else{Panama.updated="Failed"}
Panama.updated<-as.data.frame(Panama.updated)
colnames(Panama.updated)[1] <- "Updated"
Panama.updated$Country <- "Panama"

#*########## PARAGUAY ---------------------------------------
todaym <- as.numeric(format(Sys.time(), "%m"))-rezago
todayy <- as.character(as.numeric(format(Sys.time(), "%Y")))
today <- paste(todayy,todaym,sep="")
month <- todaym
month=ifelse(month=="1","enero",month)
month=ifelse(month=="2","febrero",month)
month=ifelse(month=="3","marzo",month)
month=ifelse(month=="4","abril",month)
month=ifelse(month=="5","mayo",month)
month=ifelse(month=="6","junio",month)
month=ifelse(month=="7","julio",month)
month=ifelse(month=="8","agosto",month)
month=ifelse(month=="9","septiembre",month)
month=ifelse(month=="10","octubre",month)
month=ifelse(month=="11","noviembre",month)
month=ifelse(month=="12","diciembre",month)

urls <- paste("https://www.bcp.gov.py/userfiles/files",
              paste("Ind Financ ",month," ",todayy," -Saldos-web.xlsx",sep=""),sep = "/")

for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Paraguay_latest.xlsx",sep=""),
                mode = "wb")
}

#If the file exists
Paraguay.file <- paste(loading,"/Paraguay_latest.xlsx",sep="")
if (file.exists(Paraguay.file)){
  Paraguay.updated="Done"}else{Paraguay.updated="Failed"}
Paraguay.updated<-as.data.frame(Paraguay.updated)
colnames(Paraguay.updated)[1] <- "Updated"
Paraguay.updated$Country <- "Paraguay"

#*########## PERU ---------------------------------------
todaym <- as.numeric(format(Sys.time(), "%m"))
year <- as.numeric(format(Sys.time(), "%Y"))
month <- as.numeric(as.character(todaym))-rezago-1
month2 <- as.numeric(as.character(todaym))-rezago-1
month=ifelse(month=="1","Enero",month)
month=ifelse(month=="2","Febrero",month)
month=ifelse(month=="3","Marzo",month)
month=ifelse(month=="4","Abril",month)
month=ifelse(month=="5","Mayo",month)
month=ifelse(month=="6","Junio",month)
month=ifelse(month=="7","Julio",month)
month=ifelse(month=="8","Agosto",month)
month=ifelse(month=="9","Setiembre",month)
month=ifelse(month=="10","Octubre",month)
month=ifelse(month=="11","Noviembre",month)
month=ifelse(month=="12","Diciembre",month)
#
month2=ifelse(month2=="1","en",month2)
month2=ifelse(month2=="2","fe",month2)
month2=ifelse(month2=="3","ma",month2)
month2=ifelse(month2=="4","ab",month2)
month2=ifelse(month2=="5","my",month2)
month2=ifelse(month2=="6","jn",month2)
month2=ifelse(month2=="7","jl",month2)
month2=ifelse(month2=="8","ag",month2)
month2=ifelse(month2=="9","se",month2)
month2=ifelse(month2=="10","oc",month2)
month2=ifelse(month2=="11","no",month2)
month2=ifelse(month2=="12","di",month2)
date <- paste("SF-2101-",month2,year,".ZIP",sep="")
urls <- paste("http://intranet2.sbs.gob.pe/estadistica/financiera",year,month,date,sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Peru_latest.ZIP",sep=""),
                mode = "wb")
}
month_peru <- (substr(month, 1, 3))
year_peru <- as.numeric(year)%%100
# Unzip documents
unzip(paste(loading,"Peru_latest.ZIP",sep="/"), exdir = loading)

#If the file exists
Peru.file <- paste(loading,"/Peru_latest.ZIP",sep="")
if (file.exists(Peru.file)){
  Peru.updated="Done"}else{Peru.updated="Failed"}
Peru.updated<-as.data.frame(Peru.updated)
colnames(Peru.updated)[1] <- "Updated"
Peru.updated$Country <- "Peru"

#*########## VENEZUELA ---------------------------------------
todaym <- as.numeric(format(Sys.time(), "%m"))
year <- format(Sys.time(), "%Y")
date <- as.numeric(as.character(todaym))-rezago-3
len <- nchar(gsub("\\D", "", date))
date <- ifelse(len==1,paste("0",date,sep=""),date)
date2 <- paste("SA-",year,"-",date,".zip",sep="")
urls <- paste("http://www.sudeban.gob.ve/wp-content/uploads/Estadisticas",year,date2,sep = "/")
for (i in 1:length(urls)) {
  download.file(urls[i],
                destfile = paste(loading,"/Venezuela_latest.zip",sep=""),
                mode = "wb")
}
# Unzip documents
unzip(paste(loading,"Venezuela_latest.zip",sep="/"), exdir = loading)

#If the file exists
Venezuela.file <- paste(loading,"Venezuela_latest.zip",sep="/")
if (file.exists(Venezuela.file)){
  Venezuela.updated="Done"}else{Venezuela.updated="Failed"}
Venezuela.updated<-as.data.frame(Venezuela.updated)
colnames(Venezuela.updated)[1] <- "Updated"
Venezuela.updated$Country <- "Venezuela"

##Tabla loadings------
table.loadings <- rbind(Bolivia.updated,Brasil.updated,
                        Chile.updated,Colombia.updated,CostaRica.updated,
                        DomRepublic.updated,Ecuador.updated,ElSalvador.updated,
                        Nicaragua.updated,Panama.updated,
                        Paraguay.updated,Peru.updated,Venezuela.updated)
keeps3 <- c("Country","Updated")
table.loadings <- table.loadings[keeps3]
png("Outputs/Tables/Table.loadings.png")
q <- tableGrob(table.loadings)
grid.arrange(top = "Files updated", q)
grid::grid.draw(q)
dev.off()
