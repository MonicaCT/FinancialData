rm(list = ls())
#Setting directory-----
clean <-  paste(getwd(),"/Data.ES/Clean.ES",sep="")
figures <-  paste(getwd(),"/Outputs.ES/Figures.ES/F-country",sep="")
tables <-  paste(getwd(),"/Outputs.ES/Tables",sep="")
#Packages-------------
pacman::p_load(tidyverse,dplyr,ggplot2,hrbrthemes,
               plotly,lubridate,ggpubr, readxl)
#Abriendo archivos finales-------
CreditType <- readxl::read_excel("/Final_CreditType/Data/Clean/CreditType.final.xlsx")
EconomicSector <- read_excel(paste(clean,"EconomicSector.final.xlsx",sep="/"))
#Con el archivo descargado--------
class(EconomicSector$Date)
EconomicSector$Date <- as.Date(EconomicSector$Date, format = "%Y-%m-%d")
CreditType$Date <- as.Date(CreditType$Date, format = "%Y-%m-%d")
#Total productive--------
#CreditType$T.productive.empresas <- CreditType$SMEs+CreditType$Microcredit+CreditType$Totalem
#ifelse(CreditType$T.productive.empresas==0,NA,CreditType$T.productive.empresas)
CreditType$T.productive.empresas <-
  rowSums(CreditType[, c(
    "SMEs",
    "Microcredit",
    "Totalem")], na.rm = TRUE)
#CreditType$T.productive.empresas <- transform(CreditType, T.productive.empresas = ifelse(T.productive.empresas==0,NA,T.productive.empresas))
CreditType <-mutate(CreditType,T.productive.empresas=ifelse(T.productive.empresas==0,NA,T.productive.empresas))
#
CreditType$T.productive.empresasm <-
  rowSums(CreditType[, c(
    "SMEs",
    "Totalem")], na.rm = TRUE)
CreditType <-mutate(CreditType,T.productive.empresasm=ifelse(T.productive.empresasm==0,NA,T.productive.empresasm))
CreditType <-mutate(CreditType,T.productive.empresas=
                      ifelse(Country=="Mexico",T.productive.empresasm,T.productive.empresas))

#Variables que importan------
keepsa <- c("Date","Country","T.productive")
keepsb <- c("Date","Country","T.productive.empresas")
CreditType <- CreditType[keepsb]
EconomicSector <- EconomicSector[keepsa]
#Creando un identificador para el merge------
CreditType$myeark <- format(CreditType$Date, "%Y-%m")
CreditType$key <- paste(CreditType$Country,CreditType$myeark)
#
EconomicSector$myeark <- format(EconomicSector$Date, "%Y-%m")
EconomicSector$key <- paste(EconomicSector$Country,EconomicSector$myeark)
#
keepsa <- c("key","T.productive")
keepsb <- c("key","Date","Country","T.productive.empresas")
CreditType <- CreditType[keepsb]
EconomicSector <- EconomicSector[keepsa]
#Merging data-----------
T.productive.a <- merge(EconomicSector,
                        CreditType,
                        by = "key", all=TRUE)
#Variables del analisis--------
keepsm <- c("Date","Country","T.productive","T.productive.empresas")
T.productive.a <- T.productive.a[keepsm]
#Graficos--------------
#*######By Country-----------------------------------
##Argentina:Total Productive------------------------------------
Argentina <- T.productive.a[ which(T.productive.a$Country=="Argentina"), ]
Argentina <- Argentina %>% gather(Name, Values, T.productive:T.productive.empresas)
Argentina <- Argentina %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Argentina <- Argentina[keeps1]

gi.Argentina18 <- ggplot(data = Argentina, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Argentina$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("ARGENTINA: MM $ Argentinos") +
  xlab("Date") + ylab("")
#gi.Argentina18 <- gi.Argentina18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Argentina18 <- gi.Argentina18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Argentina18 <- gi.Argentina18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Argentina18 <- gi.Argentina18+labs(colour = "Credit Type")
gi.Argentina18

ggsave(path = figures, filename = "gipc.Argentina18.png",width=9, height=5)

##Bolivia:Total Productive------------------------------------
Bolivia <- T.productive.a[ which(T.productive.a$Country=="Bolivia"), ]
Bolivia <- Bolivia %>% gather(Name, Values, T.productive:T.productive.empresas)
Bolivia <- Bolivia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Bolivia <- Bolivia[keeps1]

gi.Bolivia18 <- ggplot(data = Bolivia, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Bolivia$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Bolivia: MM US$") +
  xlab("Date") + ylab("")
#gi.Bolivia18 <- gi.Bolivia18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Bolivia18 <- gi.Bolivia18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Bolivia18 <- gi.Bolivia18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Bolivia18 <- gi.Bolivia18+labs(colour = "Credit Type")
gi.Bolivia18
ggsave(path = figures, filename = "gipc.Bolivia18.png",width=9, height=5)

##Brazil:Total Productive------------------------------------
Brazil <- T.productive.a[ which(T.productive.a$Country=="Brazil"), ]
Brazil <- Brazil %>% gather(Name, Values, T.productive:T.productive.empresas)
Brazil <- Brazil %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Brazil <- Brazil[keeps1]

gi.Brazil18 <- ggplot(data = Brazil, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Brazil$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Brazil: MM Reales") +
  xlab("Date") + ylab("")
#gi.Brazil18 <- gi.Brazil18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Brazil18 <- gi.Brazil18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Brazil18 <- gi.Brazil18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Brazil18 <- gi.Brazil18+labs(colour = "Credit Type")
gi.Brazil18
ggsave(path = figures, filename = "gipc.Brazil18.png",width=9, height=5)

##Chile:Total Productive------------------------------------
Chile <- T.productive.a[ which(T.productive.a$Country=="Chile"), ]
Chile <- Chile %>% filter(!is.na(T.productive))
Chile <- Chile %>% gather(Name, Values, T.productive:T.productive.empresas)
keeps1 <- c("Date", "Name","Values")
Chile <- Chile[keeps1]

gi.Chile18 <- ggplot(data = Chile, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Chile$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Chile: MM $ Chilenos") +
  xlab("Date") + ylab("")
#gi.Chile18 <- gi.Chile18+scale_y_continuous(limits = c(1.00e+08,1.18e+08))
gi.Chile18 <- gi.Chile18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Chile18 <- gi.Chile18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Chile18 <- gi.Chile18+labs(colour = "Credit Type")
gi.Chile18
ggsave(path = figures, filename = "gipc.Chile18.png",width=9, height=5)

##CostaRica:Total Productive------------------------------------
CostaRica <- T.productive.a[ which(T.productive.a$Country=="Costa Rica"), ]
CostaRica <- CostaRica %>% gather(Name, Values, T.productive:T.productive.empresas)
CostaRica <- CostaRica %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
CostaRica <- CostaRica[keeps1]

gi.CostaRica18 <- ggplot(data = CostaRica, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(CostaRica$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Costa Rica: MM Colones") +
  xlab("Date") + ylab("")
#gi.CostaRica18 <- gi.CostaRica18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.CostaRica18 <- gi.CostaRica18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.CostaRica18 <- gi.CostaRica18+theme(legend.key.size = unit(0.9, "lines"))
#gi.CostaRica18 <- gi.CostaRica18+labs(colour = "Credit Type")
gi.CostaRica18
ggsave(path = figures, filename = "gipc.CostaRica18.png",width=9, height=5)

##DominicanRepublic:Total Productive------------------------------------
DominicanRepublic <- T.productive.a[ which(T.productive.a$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, T.productive:T.productive.empresas)
DominicanRepublic <- DominicanRepublic %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
DominicanRepublic <- DominicanRepublic[keeps1]

gi.DominicanRepublic18 <- ggplot(data = DominicanRepublic, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(DominicanRepublic$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Dominican Republic: MM $ Dominicanos") +
  xlab("Date") + ylab("")
#gi.DominicanRepublic18 <- gi.DominicanRepublic18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.DominicanRepublic18 <- gi.DominicanRepublic18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.DominicanRepublic18 <- gi.DominicanRepublic18+theme(legend.key.size = unit(0.9, "lines"))
#gi.DominicanRepublic18 <- gi.DominicanRepublic18+labs(colour = "Credit Type")
gi.DominicanRepublic18
ggsave(path = figures, filename = "gipc.DominicanRepublic18.png",width=9, height=5)

##ElSalvador:Total Productive------------------------------------
ElSalvador <- T.productive.a[ which(T.productive.a$Country=="El Salvador"), ]
ElSalvador <- ElSalvador %>% gather(Name, Values, T.productive:T.productive.empresas)
ElSalvador <- ElSalvador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
ElSalvador <- ElSalvador[keeps1]

gi.ElSalvador18 <- ggplot(data = ElSalvador, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(ElSalvador$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("El Salvador: Miles de US$") +
  xlab("Date") + ylab("")
#gi.ElSalvador18 <- gi.ElSalvador18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.ElSalvador18 <- gi.ElSalvador18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.ElSalvador18 <- gi.ElSalvador18+theme(legend.key.size = unit(0.9, "lines"))
#gi.ElSalvador18 <- gi.ElSalvador18+labs(colour = "Credit Type")
gi.ElSalvador18
ggsave(path = figures, filename = "gipc.ElSalvador18.png",width=9, height=5)

##Guatemala:Total Productive------------------------------------
Guatemala <- T.productive.a[ which(T.productive.a$Country=="Guatemala"), ]
Guatemala$T.productive.empresas <- Guatemala$T.productive.empresas/1000
Guatemala <- Guatemala %>% gather(Name, Values, T.productive:T.productive.empresas)
Guatemala <- Guatemala %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Guatemala <- Guatemala[keeps1]

gi.Guatemala18 <- ggplot(data = Guatemala, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Guatemala$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Guatemala: Miles de Quetzales") +
  xlab("Date") + ylab("")
#gi.Guatemala18 <- gi.Guatemala18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Guatemala18 <- gi.Guatemala18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Guatemala18 <- gi.Guatemala18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Guatemala18 <- gi.Guatemala18+labs(colour = "Credit Type")
gi.Guatemala18
ggsave(path = figures, filename = "gipc.Guatemala18.png",width=9, height=5)

##Honduras:Total Productive------------------------------------
Honduras <- T.productive.a[ which(T.productive.a$Country=="Honduras"), ]
Honduras <- Honduras %>% gather(Name, Values, T.productive:T.productive.empresas)
Honduras <- Honduras %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Honduras <- Honduras[keeps1]

gi.Honduras18 <- ggplot(data = Honduras, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Honduras$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Honduras: Miles de Lempiras") +
  xlab("Date") + ylab("")
#gi.Honduras18 <- gi.Honduras18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Honduras18 <- gi.Honduras18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Honduras18 <- gi.Honduras18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Honduras18 <- gi.Honduras18+labs(colour = "Credit Type")
gi.Honduras18
ggsave(path = figures, filename = "gipc.Honduras18.png",width=9, height=5)

##Mexico:Total Productive------------------------------------
Mexico <- T.productive.a[ which(T.productive.a$Country=="Mexico"), ]
Mexico <- Mexico %>% gather(Name, Values, T.productive:T.productive.empresas)
Mexico <- Mexico %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Mexico <- Mexico[keeps1]

gi.Mexico18 <- ggplot(data = Mexico, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Mexico$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Mexico: MM $ Mexicanos") +
  xlab("Date") + ylab("")
#gi.Mexico18 <- gi.Mexico18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Mexico18 <- gi.Mexico18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Mexico18 <- gi.Mexico18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Mexico18 <- gi.Mexico18+labs(colour = "Credit Type")
gi.Mexico18
ggsave(path = figures, filename = "gipc.Mexico18.png",width=9, height=5)

##Nicaragua:Total Productive------------------------------------
Nicaragua <- T.productive.a[ which(T.productive.a$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua %>% gather(Name, Values, T.productive:T.productive.empresas)
Nicaragua <- Nicaragua %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Nicaragua <- Nicaragua[keeps1]

gi.Nicaragua18 <- ggplot(data = Nicaragua, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Nicaragua$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Nicaragua: MM Córdobas") +
  xlab("Date") + ylab("")
#gi.Nicaragua18 <- gi.Nicaragua18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Nicaragua18 <- gi.Nicaragua18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Nicaragua18 <- gi.Nicaragua18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Nicaragua18 <- gi.Nicaragua18+labs(colour = "Credit Type")
gi.Nicaragua18
ggsave(path = figures, filename = "gipc.Nicaragua18.png",width=9, height=5)

##Panama:Total Productive------------------------------------
Panama <- T.productive.a[ which(T.productive.a$Country=="Panama"), ]
Panama$T.productive <- Panama$T.productive/1000000
Panama <- Panama %>% gather(Name, Values, T.productive:T.productive.empresas)
Panama <- Panama %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Panama <- Panama[keeps1]

gi.Panama18 <- ggplot(data = Panama, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Panama$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Panama: MM Balboas") +
  xlab("Date") + ylab("")
#gi.Panama18 <- gi.Panama18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Panama18 <- gi.Panama18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Panama18 <- gi.Panama18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Panama18 <- gi.Panama18+labs(colour = "Credit Type")
gi.Panama18
ggsave(path = figures, filename = "gipc.Panama18.png",width=9, height=5)

##Paraguay:Total Productive------------------------------------
Paraguay <- T.productive.a[ which(T.productive.a$Country=="Paraguay"), ]
Paraguay <- Paraguay %>% gather(Name, Values, T.productive:T.productive.empresas)
Paraguay <- Paraguay %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Paraguay <- Paraguay[keeps1]

gi.Paraguay18 <- ggplot(data = Paraguay, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Paraguay$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Paraguay: MM Gs") +
  xlab("Date") + ylab("")
#gi.Paraguay18 <- gi.Paraguay18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Paraguay18 <- gi.Paraguay18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Paraguay18 <- gi.Paraguay18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Paraguay18 <- gi.Paraguay18+labs(colour = "Credit Type")
gi.Paraguay18
ggsave(path = figures, filename = "gipc.Paraguay18.png",width=9, height=5)

##Peru:Total Productive------------------------------------
Peru <- T.productive.a[ which(T.productive.a$Country=="Peru"), ]
Peru <- Peru %>% gather(Name, Values, T.productive:T.productive.empresas)
Peru <- Peru %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Peru <- Peru[keeps1]

gi.Peru18 <- ggplot(data = Peru, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Peru$Name)))+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-11")),
                 linetype="WHO declaration"), colour="aquamarine3") +
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-28")),
                 linetype="First COVID case"), colour="dodgerblue4") +
  scale_linetype_manual(name = "Events",
                        values = c("WHO declaration"=1,
                                   "First COVID case"=1),
                        guide = guide_legend(override.aes = list(colour = c('aquamarine3',
                                                                            'dodgerblue4'))))+
  scale_x_date(date_labels = "%b%y", date_breaks = "2 months")+
  ggtitle("Peru: Miles de soles") +
  xlab("Date") + ylab("")
#gi.Peru18 <- gi.Peru18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Peru18 <- gi.Peru18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Peru18 <- gi.Peru18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Peru18 <- gi.Peru18+labs(colour = "Credit Type")
gi.Peru18
ggsave(path = figures, filename = "gipc.Peru18.png",width=9, height=5)




