rm(list = ls())
#Setting directory-----
clean <-  paste(getwd(),"/Data.ES/Clean.ES",sep="")
figures <-  paste(getwd(),"/Outputs.ES/Figures.ES/F-country",sep="")
tables <-  paste(getwd(),"/Outputs.ES/Tables",sep="")
presentations <-  paste(getwd(),"/Outputs.ES/Presentations.ES",sep="")
#Packages-------------
#install.packages("pacman")
pacman::p_load(tidyverse,dplyr,ggplot2,hrbrthemes,plotly,lubridate,ggpubr)
#Abriendo el archivo final-------
EconomicSector <- read.csv(paste(clean,"EconomicSector.final.csv",sep="/"))
#Con el archivo descargado
class(EconomicSector$Date)
EconomicSector$Date <- as.Date(EconomicSector$Date, format = "%Y-%m-%d")
class(EconomicSector$Industry)
cols = c(3:23)
EconomicSector[, cols] = apply(EconomicSector[, cols], 2, function(x)
  as.numeric(as.character(x)))

####### GRAPHS------------------------------
#*######By Country-----------------------------------
keeps2 <- c("Date","Agricultural","Remaining","Industry",
            "Commerce","T.productive")
keeps3 <- c("Date","iAgricultural18","iRemaining18","iIndustry18",
            "iCommerce18","iT.productive18")
##Argentina:INDEX18------------------------------------
Argentina <- EconomicSector[ which(EconomicSector$Country=="Argentina"), ]
Argentina <- Argentina[keeps3]
Argentina <- Argentina %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("ARGENTINA: INDEX18") +
  xlab("Date") + ylab("")
#gi.Argentina18 <- gi.Argentina18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Argentina18 <- gi.Argentina18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Argentina18 <- gi.Argentina18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Argentina18 <- gi.Argentina18+labs(colour = "Credit Type")
gi.Argentina18

#Facets
Argentina <- EconomicSector[ which(EconomicSector$Country=="Argentina"), ]
Argentina <- Argentina[keeps3]
Argentina <- Argentina %>% gather(Name, Values, iAgricultural18:iT.productive18)
Argentina <- Argentina %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Argentina <- Argentina[keeps1]

gi.Argentina18f <- ggplot(data = Argentina, aes(x=Date, y=Values)) + 
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
  #ggtitle("ARGENTINA: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Argentina18f <- gi.Argentina18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Argentina18f <- gi.Argentina18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Argentina18f <- gi.Argentina18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Argentina18f <- gi.Argentina18f+labs(colour = "Credit Type")
gi.Argentina18f
#Arrange
ggarrange(gi.Argentina18, gi.Argentina18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Argentina18.png",width=9, height=5)
##Bolivia:INDEX18------------------------------------
Bolivia <- EconomicSector[ which(EconomicSector$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps3]
Bolivia <- Bolivia %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Bolivia: INDEX18") +
  xlab("Date") + ylab("")
#gi.Bolivia18 <- gi.Bolivia18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Bolivia18 <- gi.Bolivia18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Bolivia18 <- gi.Bolivia18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Bolivia18 <- gi.Bolivia18+labs(colour = "Credit Type")
gi.Bolivia18

#Facets
Bolivia <- EconomicSector[ which(EconomicSector$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps3]
Bolivia <- Bolivia %>% gather(Name, Values, iAgricultural18:iT.productive18)
Bolivia <- Bolivia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Bolivia <- Bolivia[keeps1]

gi.Bolivia18f <- ggplot(data = Bolivia, aes(x=Date, y=Values)) + 
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
  #ggtitle("Bolivia: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Bolivia18f <- gi.Bolivia18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Bolivia18f <- gi.Bolivia18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Bolivia18f <- gi.Bolivia18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Bolivia18f <- gi.Bolivia18f+labs(colour = "Credit Type")
gi.Bolivia18f
#Arrange
ggarrange(gi.Bolivia18, gi.Bolivia18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Bolivia18.png",width=9, height=5)
##Brazil:INDEX18------------------------------------
Brazil <- EconomicSector[ which(EconomicSector$Country=="Brazil"), ]
Brazil <- Brazil[keeps3]
Brazil <- Brazil %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Brazil: INDEX18") +
  xlab("Date") + ylab("")
#gi.Brazil18 <- gi.Brazil18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Brazil18 <- gi.Brazil18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Brazil18 <- gi.Brazil18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Brazil18 <- gi.Brazil18+labs(colour = "Credit Type")
gi.Brazil18

#Facets
Brazil <- EconomicSector[ which(EconomicSector$Country=="Brazil"), ]
Brazil <- Brazil[keeps3]
Brazil <- Brazil %>% gather(Name, Values, iAgricultural18:iT.productive18)
Brazil <- Brazil %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Brazil <- Brazil[keeps1]

gi.Brazil18f <- ggplot(data = Brazil, aes(x=Date, y=Values)) + 
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
  #ggtitle("Brazil: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Brazil18f <- gi.Brazil18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Brazil18f <- gi.Brazil18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Brazil18f <- gi.Brazil18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Brazil18f <- gi.Brazil18f+labs(colour = "Credit Type")
gi.Brazil18f
#Arrange
ggarrange(gi.Brazil18, gi.Brazil18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Brazil18.png",width=9, height=5)

##Chile:INDEX18------------------------------------
Chile <- EconomicSector[ which(EconomicSector$Country=="Chile"), ]
Chile <- Chile[keeps3]
Chile <- Chile %>% gather(Name, Values, iAgricultural18:iT.productive18)
Chile <- Chile %>% filter(!is.na(Values))
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
  ggtitle("Chile: INDEX18") +
  xlab("Date") + ylab("")
#gi.Chile18 <- gi.Chile18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Chile18 <- gi.Chile18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Chile18 <- gi.Chile18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Chile18 <- gi.Chile18+labs(colour = "Credit Type")
gi.Chile18

#Facets
Chile <- EconomicSector[ which(EconomicSector$Country=="Chile"), ]
Chile <- Chile[keeps3]
Chile <- Chile %>% gather(Name, Values, iAgricultural18:iT.productive18)
Chile <- Chile %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Chile <- Chile[keeps1]

gi.Chile18f <- ggplot(data = Chile, aes(x=Date, y=Values)) + 
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
  #ggtitle("Chile: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Chile18f <- gi.Chile18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Chile18f <- gi.Chile18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Chile18f <- gi.Chile18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Chile18f <- gi.Chile18f+labs(colour = "Credit Type")
gi.Chile18f
#Arrange
ggarrange(gi.Chile18, gi.Chile18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Chile18.png",width=9, height=5)

##CostaRica:INDEX18------------------------------------
CostaRica <- EconomicSector[ which(EconomicSector$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps3]
CostaRica <- CostaRica %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Costa Rica: INDEX18") +
  xlab("Date") + ylab("")
#gi.CostaRica18 <- gi.CostaRica18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.CostaRica18 <- gi.CostaRica18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.CostaRica18 <- gi.CostaRica18+theme(legend.key.size = unit(0.9, "lines"))
#gi.CostaRica18 <- gi.CostaRica18+labs(colour = "Credit Type")
gi.CostaRica18

#Facets
CostaRica <- EconomicSector[ which(EconomicSector$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps3]
CostaRica <- CostaRica %>% gather(Name, Values, iAgricultural18:iT.productive18)
CostaRica <- CostaRica %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
CostaRica <- CostaRica[keeps1]

gi.CostaRica18f <- ggplot(data = CostaRica, aes(x=Date, y=Values)) + 
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
  #ggtitle("CostaRica: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.CostaRica18f <- gi.CostaRica18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.CostaRica18f <- gi.CostaRica18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.CostaRica18f <- gi.CostaRica18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.CostaRica18f <- gi.CostaRica18f+labs(colour = "Credit Type")
gi.CostaRica18f
#Arrange
ggarrange(gi.CostaRica18, gi.CostaRica18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.CostaRica18.png",width=9, height=5)

##DominicanRepublic:INDEX18------------------------------------
DominicanRepublic <- EconomicSector[ which(EconomicSector$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps3]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Dominican Republic: INDEX18") +
  xlab("Date") + ylab("")
#gi.DominicanRepublic18 <- gi.DominicanRepublic18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.DominicanRepublic18 <- gi.DominicanRepublic18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.DominicanRepublic18 <- gi.DominicanRepublic18+theme(legend.key.size = unit(0.9, "lines"))
#gi.DominicanRepublic18 <- gi.DominicanRepublic18+labs(colour = "Credit Type")
gi.DominicanRepublic18

#Facets
DominicanRepublic <- EconomicSector[ which(EconomicSector$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps3]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iAgricultural18:iT.productive18)
DominicanRepublic <- DominicanRepublic %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
DominicanRepublic <- DominicanRepublic[keeps1]

gi.DominicanRepublic18f <- ggplot(data = DominicanRepublic, aes(x=Date, y=Values)) + 
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
  #ggtitle("DominicanRepublic: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.DominicanRepublic18f <- gi.DominicanRepublic18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.DominicanRepublic18f <- gi.DominicanRepublic18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.DominicanRepublic18f <- gi.DominicanRepublic18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.DominicanRepublic18f <- gi.DominicanRepublic18f+labs(colour = "Credit Type")
gi.DominicanRepublic18f
#Arrange
ggarrange(gi.DominicanRepublic18, gi.DominicanRepublic18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.DominicanRepublic18.png",width=9, height=5)

##ElSalvador:INDEX18------------------------------------
ElSalvador <- EconomicSector[ which(EconomicSector$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps3]
ElSalvador <- ElSalvador %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("El Salvador: INDEX18") +
  xlab("Date") + ylab("")
#gi.ElSalvador18 <- gi.ElSalvador18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.ElSalvador18 <- gi.ElSalvador18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.ElSalvador18 <- gi.ElSalvador18+theme(legend.key.size = unit(0.9, "lines"))
#gi.ElSalvador18 <- gi.ElSalvador18+labs(colour = "Credit Type")
gi.ElSalvador18

#Facets
ElSalvador <- EconomicSector[ which(EconomicSector$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps3]
ElSalvador <- ElSalvador %>% gather(Name, Values, iAgricultural18:iT.productive18)
ElSalvador <- ElSalvador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
ElSalvador <- ElSalvador[keeps1]

gi.ElSalvador18f <- ggplot(data = ElSalvador, aes(x=Date, y=Values)) + 
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
  #ggtitle("ElSalvador: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.ElSalvador18f <- gi.ElSalvador18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.ElSalvador18f <- gi.ElSalvador18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.ElSalvador18f <- gi.ElSalvador18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.ElSalvador18f <- gi.ElSalvador18f+labs(colour = "Credit Type")
gi.ElSalvador18f
#Arrange
ggarrange(gi.ElSalvador18, gi.ElSalvador18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.ElSalvador18.png",width=9, height=5)

##Guatemala:INDEX18------------------------------------
Guatemala <- EconomicSector[ which(EconomicSector$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps3]
Guatemala <- Guatemala %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Guatemala: INDEX18") +
  xlab("Date") + ylab("")
#gi.Guatemala18 <- gi.Guatemala18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Guatemala18 <- gi.Guatemala18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Guatemala18 <- gi.Guatemala18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Guatemala18 <- gi.Guatemala18+labs(colour = "Credit Type")
gi.Guatemala18

#Facets
Guatemala <- EconomicSector[ which(EconomicSector$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps3]
Guatemala <- Guatemala %>% gather(Name, Values, iAgricultural18:iT.productive18)
Guatemala <- Guatemala %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Guatemala <- Guatemala[keeps1]

gi.Guatemala18f <- ggplot(data = Guatemala, aes(x=Date, y=Values)) + 
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
  #ggtitle("Guatemala: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Guatemala18f <- gi.Guatemala18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Guatemala18f <- gi.Guatemala18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Guatemala18f <- gi.Guatemala18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Guatemala18f <- gi.Guatemala18f+labs(colour = "Credit Type")
gi.Guatemala18f
#Arrange
ggarrange(gi.Guatemala18, gi.Guatemala18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Guatemala18.png",width=9, height=5)

##Honduras:INDEX18------------------------------------
Honduras <- EconomicSector[ which(EconomicSector$Country=="Honduras"), ]
Honduras <- Honduras[keeps3]
Honduras <- Honduras %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Honduras: INDEX18") +
  xlab("Date") + ylab("")
#gi.Honduras18 <- gi.Honduras18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Honduras18 <- gi.Honduras18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Honduras18 <- gi.Honduras18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Honduras18 <- gi.Honduras18+labs(colour = "Credit Type")
gi.Honduras18

#Facets
Honduras <- EconomicSector[ which(EconomicSector$Country=="Honduras"), ]
Honduras <- Honduras[keeps3]
Honduras <- Honduras %>% gather(Name, Values, iAgricultural18:iT.productive18)
Honduras <- Honduras %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Honduras <- Honduras[keeps1]

gi.Honduras18f <- ggplot(data = Honduras, aes(x=Date, y=Values)) + 
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
  #ggtitle("Honduras: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Honduras18f <- gi.Honduras18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Honduras18f <- gi.Honduras18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Honduras18f <- gi.Honduras18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Honduras18f <- gi.Honduras18f+labs(colour = "Credit Type")
gi.Honduras18f
#Arrange
ggarrange(gi.Honduras18, gi.Honduras18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Honduras18.png",width=9, height=5)

##Mexico:INDEX18------------------------------------
Mexico <- EconomicSector[ which(EconomicSector$Country=="Mexico"), ]
Mexico <- Mexico[keeps3]
Mexico <- Mexico %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Mexico: INDEX18") +
  xlab("Date") + ylab("")
#gi.Mexico18 <- gi.Mexico18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Mexico18 <- gi.Mexico18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Mexico18 <- gi.Mexico18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Mexico18 <- gi.Mexico18+labs(colour = "Credit Type")
gi.Mexico18

#Facets
Mexico <- EconomicSector[ which(EconomicSector$Country=="Mexico"), ]
Mexico <- Mexico[keeps3]
Mexico <- Mexico %>% gather(Name, Values, iAgricultural18:iT.productive18)
Mexico <- Mexico %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Mexico <- Mexico[keeps1]

gi.Mexico18f <- ggplot(data = Mexico, aes(x=Date, y=Values)) + 
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
  #ggtitle("Mexico: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Mexico18f <- gi.Mexico18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Mexico18f <- gi.Mexico18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Mexico18f <- gi.Mexico18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Mexico18f <- gi.Mexico18f+labs(colour = "Credit Type")
gi.Mexico18f
#Arrange
ggarrange(gi.Mexico18, gi.Mexico18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Mexico18.png",width=9, height=5)

##Nicaragua:INDEX18------------------------------------
Nicaragua <- EconomicSector[ which(EconomicSector$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps3]
Nicaragua <- Nicaragua %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Nicaragua: INDEX18") +
  xlab("Date") + ylab("")
#gi.Nicaragua18 <- gi.Nicaragua18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Nicaragua18 <- gi.Nicaragua18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Nicaragua18 <- gi.Nicaragua18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Nicaragua18 <- gi.Nicaragua18+labs(colour = "Credit Type")
gi.Nicaragua18

#Facets
Nicaragua <- EconomicSector[ which(EconomicSector$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps3]
Nicaragua <- Nicaragua %>% gather(Name, Values, iAgricultural18:iT.productive18)
Nicaragua <- Nicaragua %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Nicaragua <- Nicaragua[keeps1]

gi.Nicaragua18f <- ggplot(data = Nicaragua, aes(x=Date, y=Values)) + 
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
  #ggtitle("Nicaragua: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Nicaragua18f <- gi.Nicaragua18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Nicaragua18f <- gi.Nicaragua18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Nicaragua18f <- gi.Nicaragua18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Nicaragua18f <- gi.Nicaragua18f+labs(colour = "Credit Type")
gi.Nicaragua18f
#Arrange
ggarrange(gi.Nicaragua18, gi.Nicaragua18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Nicaragua18.png",width=9, height=5)

##Panama:INDEX18------------------------------------
Panama <- EconomicSector[ which(EconomicSector$Country=="Panama"), ]
Panama <- Panama[keeps3]
Panama <- Panama %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Panama: INDEX18") +
  xlab("Date") + ylab("")
#gi.Panama18 <- gi.Panama18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Panama18 <- gi.Panama18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Panama18 <- gi.Panama18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Panama18 <- gi.Panama18+labs(colour = "Credit Type")
gi.Panama18

#Facets
Panama <- EconomicSector[ which(EconomicSector$Country=="Panama"), ]
Panama <- Panama[keeps3]
Panama <- Panama %>% gather(Name, Values, iAgricultural18:iT.productive18)
Panama <- Panama %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Panama <- Panama[keeps1]

gi.Panama18f <- ggplot(data = Panama, aes(x=Date, y=Values)) + 
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
  #ggtitle("Panama: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Panama18f <- gi.Panama18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Panama18f <- gi.Panama18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Panama18f <- gi.Panama18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Panama18f <- gi.Panama18f+labs(colour = "Credit Type")
gi.Panama18f
#Arrange
ggarrange(gi.Panama18, gi.Panama18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Panama18.png",width=9, height=5)

##Paraguay:INDEX18------------------------------------
Paraguay <- EconomicSector[ which(EconomicSector$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps3]
Paraguay <- Paraguay %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Paraguay: INDEX18") +
  xlab("Date") + ylab("")
#gi.Paraguay18 <- gi.Paraguay18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Paraguay18 <- gi.Paraguay18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Paraguay18 <- gi.Paraguay18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Paraguay18 <- gi.Paraguay18+labs(colour = "Credit Type")
gi.Paraguay18

#Facets
Paraguay <- EconomicSector[ which(EconomicSector$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps3]
Paraguay <- Paraguay %>% gather(Name, Values, iAgricultural18:iT.productive18)
Paraguay <- Paraguay %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Paraguay <- Paraguay[keeps1]

gi.Paraguay18f <- ggplot(data = Paraguay, aes(x=Date, y=Values)) + 
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
  #ggtitle("Paraguay: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Paraguay18f <- gi.Paraguay18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Paraguay18f <- gi.Paraguay18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Paraguay18f <- gi.Paraguay18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Paraguay18f <- gi.Paraguay18f+labs(colour = "Credit Type")
gi.Paraguay18f
#Arrange
ggarrange(gi.Paraguay18, gi.Paraguay18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Paraguay18.png",width=9, height=5)

##Peru:INDEX18------------------------------------
Peru <- EconomicSector[ which(EconomicSector$Country=="Peru"), ]
Peru <- Peru[keeps3]
Peru <- Peru %>% gather(Name, Values, iAgricultural18:iT.productive18)
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
  ggtitle("Peru: INDEX18") +
  xlab("Date") + ylab("")
#gi.Peru18 <- gi.Peru18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Peru18 <- gi.Peru18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Peru18 <- gi.Peru18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Peru18 <- gi.Peru18+labs(colour = "Credit Type")
gi.Peru18

#Facets
Peru <- EconomicSector[ which(EconomicSector$Country=="Peru"), ]
Peru <- Peru[keeps3]
Peru <- Peru %>% gather(Name, Values, iAgricultural18:iT.productive18)
Peru <- Peru %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Peru <- Peru[keeps1]

gi.Peru18f <- ggplot(data = Peru, aes(x=Date, y=Values)) + 
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
  #ggtitle("Peru: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Peru18f <- gi.Peru18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Peru18f <- gi.Peru18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Peru18f <- gi.Peru18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Peru18f <- gi.Peru18f+labs(colour = "Credit Type")
gi.Peru18f
#Arrange
ggarrange(gi.Peru18, gi.Peru18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi.Peru18.png",width=9, height=5)

####### INDEX18 deflactado------------------------------
#*######By Country-----------------------------------
keeps4 <- c("Date","iAgricultural18ipc","iRemaining18ipc","iIndustry18ipc",
            "iCommerce18ipc","iT.productive18ipc","iT.credit18ipc","iT.productive18ipc")
##Argentina:INDEX18 deflactado------------------------------------
Argentina <- EconomicSector[ which(EconomicSector$Country=="Argentina"), ]
Argentina <- Argentina[keeps4]
Argentina <- Argentina %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Argentina <- Argentina %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Argentina <- Argentina[keeps1]

gi2.Argentina18 <- ggplot(data = Argentina, aes(x=Date, y=Values)) + 
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
  ggtitle("ARGENTINA: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Argentina18 <- gi2.Argentina18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Argentina18 <- gi2.Argentina18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Argentina18 <- gi2.Argentina18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Argentina18 <- gi2.Argentina18+labs(colour = "Credit Type")
gi2.Argentina18

#Facets
Argentina <- EconomicSector[ which(EconomicSector$Country=="Argentina"), ]
Argentina <- Argentina[keeps4]
Argentina <- Argentina %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Argentina <- Argentina %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Argentina <- Argentina[keeps1]

gi2.Argentina18f <- ggplot(data = Argentina, aes(x=Date, y=Values)) + 
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
  #ggtitle("ARGENTINA: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Argentina18f <- gi2.Argentina18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Argentina18f <- gi2.Argentina18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Argentina18f <- gi2.Argentina18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Argentina18f <- gi2.Argentina18f+labs(colour = "Credit Type")
gi2.Argentina18f
#Arrange
ggarrange(gi2.Argentina18, gi2.Argentina18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Argentina18.png",width=9, height=5)

##Bolivia:INDEX18 deflactado------------------------------------
Bolivia <- EconomicSector[ which(EconomicSector$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps4]
Bolivia <- Bolivia %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Bolivia <- Bolivia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Bolivia <- Bolivia[keeps1]

gi2.Bolivia18 <- ggplot(data = Bolivia, aes(x=Date, y=Values)) + 
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
  ggtitle("Bolivia: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Bolivia18 <- gi2.Bolivia18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Bolivia18 <- gi2.Bolivia18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Bolivia18 <- gi2.Bolivia18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Bolivia18 <- gi2.Bolivia18+labs(colour = "Credit Type")
gi2.Bolivia18

#Facets
Bolivia <- EconomicSector[ which(EconomicSector$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps4]
Bolivia <- Bolivia %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Bolivia <- Bolivia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Bolivia <- Bolivia[keeps1]

gi2.Bolivia18f <- ggplot(data = Bolivia, aes(x=Date, y=Values)) + 
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
  #ggtitle("Bolivia: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Bolivia18f <- gi2.Bolivia18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Bolivia18f <- gi2.Bolivia18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Bolivia18f <- gi2.Bolivia18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Bolivia18f <- gi2.Bolivia18f+labs(colour = "Credit Type")
gi2.Bolivia18f
#Arrange
ggarrange(gi2.Bolivia18, gi2.Bolivia18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Bolivia18.png",width=9, height=5)

##Brazil:INDEX18 deflactado------------------------------------
Brazil <- EconomicSector[ which(EconomicSector$Country=="Brazil"), ]
Brazil <- Brazil[keeps4]
Brazil <- Brazil %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Brazil <- Brazil %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Brazil <- Brazil[keeps1]

gi2.Brazil18 <- ggplot(data = Brazil, aes(x=Date, y=Values)) + 
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
  ggtitle("Brazil: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Brazil18 <- gi2.Brazil18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Brazil18 <- gi2.Brazil18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Brazil18 <- gi2.Brazil18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Brazil18 <- gi2.Brazil18+labs(colour = "Credit Type")
gi2.Brazil18

#Facets
Brazil <- EconomicSector[ which(EconomicSector$Country=="Brazil"), ]
Brazil <- Brazil[keeps4]
Brazil <- Brazil %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Brazil <- Brazil %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Brazil <- Brazil[keeps1]

gi2.Brazil18f <- ggplot(data = Brazil, aes(x=Date, y=Values)) + 
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
  #ggtitle("Brazil: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Brazil18f <- gi2.Brazil18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Brazil18f <- gi2.Brazil18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Brazil18f <- gi2.Brazil18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Brazil18f <- gi2.Brazil18f+labs(colour = "Credit Type")
gi2.Brazil18f
#Arrange
ggarrange(gi2.Brazil18, gi2.Brazil18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Brazil18.png",width=9, height=5)

##Chile:INDEX18 deflactado------------------------------------
Chile <- EconomicSector[ which(EconomicSector$Country=="Chile"), ]
Chile <- Chile[keeps4]
Chile <- Chile %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Chile <- Chile %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Chile <- Chile[keeps1]

gi2.Chile18 <- ggplot(data = Chile, aes(x=Date, y=Values)) + 
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
  ggtitle("Chile: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Chile18 <- gi2.Chile18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Chile18 <- gi2.Chile18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Chile18 <- gi2.Chile18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Chile18 <- gi2.Chile18+labs(colour = "Credit Type")
gi2.Chile18

#Facets
Chile <- EconomicSector[ which(EconomicSector$Country=="Chile"), ]
Chile <- Chile[keeps4]
Chile <- Chile %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Chile <- Chile %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Chile <- Chile[keeps1]

gi2.Chile18f <- ggplot(data = Chile, aes(x=Date, y=Values)) + 
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
  #ggtitle("Chile: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Chile18f <- gi2.Chile18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Chile18f <- gi2.Chile18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Chile18f <- gi2.Chile18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Chile18f <- gi2.Chile18f+labs(colour = "Credit Type")
gi2.Chile18f
#Arrange
ggarrange(gi2.Chile18, gi2.Chile18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Chile18.png",width=9, height=5)

##CostaRica:INDEX18 deflactado------------------------------------
CostaRica <- EconomicSector[ which(EconomicSector$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps4]
CostaRica <- CostaRica %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
CostaRica <- CostaRica %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
CostaRica <- CostaRica[keeps1]

gi2.CostaRica18 <- ggplot(data = CostaRica, aes(x=Date, y=Values)) + 
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
  ggtitle("Costa Rica: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.CostaRica18 <- gi2.CostaRica18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.CostaRica18 <- gi2.CostaRica18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.CostaRica18 <- gi2.CostaRica18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.CostaRica18 <- gi2.CostaRica18+labs(colour = "Credit Type")
gi2.CostaRica18

#Facets
CostaRica <- EconomicSector[ which(EconomicSector$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps4]
CostaRica <- CostaRica %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
CostaRica <- CostaRica %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
CostaRica <- CostaRica[keeps1]

gi2.CostaRica18f <- ggplot(data = CostaRica, aes(x=Date, y=Values)) + 
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
  #ggtitle("CostaRica: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.CostaRica18f <- gi2.CostaRica18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.CostaRica18f <- gi2.CostaRica18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.CostaRica18f <- gi2.CostaRica18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.CostaRica18f <- gi2.CostaRica18f+labs(colour = "Credit Type")
gi2.CostaRica18f
#Arrange
ggarrange(gi2.CostaRica18, gi2.CostaRica18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.CostaRica18.png",width=9, height=5)

##DominicanRepublic:INDEX18 deflactado------------------------------------
DominicanRepublic <- EconomicSector[ which(EconomicSector$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps4]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
DominicanRepublic <- DominicanRepublic %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
DominicanRepublic <- DominicanRepublic[keeps1]

gi2.DominicanRepublic18 <- ggplot(data = DominicanRepublic, aes(x=Date, y=Values)) + 
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
  ggtitle("Dominican Republic: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.DominicanRepublic18 <- gi2.DominicanRepublic18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.DominicanRepublic18 <- gi2.DominicanRepublic18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.DominicanRepublic18 <- gi2.DominicanRepublic18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.DominicanRepublic18 <- gi2.DominicanRepublic18+labs(colour = "Credit Type")
gi2.DominicanRepublic18

#Facets
DominicanRepublic <- EconomicSector[ which(EconomicSector$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps4]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
DominicanRepublic <- DominicanRepublic %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
DominicanRepublic <- DominicanRepublic[keeps1]

gi2.DominicanRepublic18f <- ggplot(data = DominicanRepublic, aes(x=Date, y=Values)) + 
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
  #ggtitle("DominicanRepublic: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.DominicanRepublic18f <- gi2.DominicanRepublic18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.DominicanRepublic18f <- gi2.DominicanRepublic18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.DominicanRepublic18f <- gi2.DominicanRepublic18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.DominicanRepublic18f <- gi2.DominicanRepublic18f+labs(colour = "Credit Type")
gi2.DominicanRepublic18f
#Arrange
ggarrange(gi2.DominicanRepublic18, gi2.DominicanRepublic18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.DominicanRepublic18.png",width=9, height=5)

##ElSalvador:INDEX18 deflactado------------------------------------
ElSalvador <- EconomicSector[ which(EconomicSector$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps4]
ElSalvador <- ElSalvador %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
ElSalvador <- ElSalvador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
ElSalvador <- ElSalvador[keeps1]

gi2.ElSalvador18 <- ggplot(data = ElSalvador, aes(x=Date, y=Values)) + 
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
  ggtitle("El Salvador: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.ElSalvador18 <- gi2.ElSalvador18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.ElSalvador18 <- gi2.ElSalvador18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.ElSalvador18 <- gi2.ElSalvador18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.ElSalvador18 <- gi2.ElSalvador18+labs(colour = "Credit Type")
gi2.ElSalvador18

#Facets
ElSalvador <- EconomicSector[ which(EconomicSector$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps4]
ElSalvador <- ElSalvador %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
ElSalvador <- ElSalvador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
ElSalvador <- ElSalvador[keeps1]

gi2.ElSalvador18f <- ggplot(data = ElSalvador, aes(x=Date, y=Values)) + 
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
  #ggtitle("ElSalvador: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.ElSalvador18f <- gi2.ElSalvador18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.ElSalvador18f <- gi2.ElSalvador18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.ElSalvador18f <- gi2.ElSalvador18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.ElSalvador18f <- gi2.ElSalvador18f+labs(colour = "Credit Type")
gi2.ElSalvador18f
#Arrange
ggarrange(gi2.ElSalvador18, gi2.ElSalvador18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.ElSalvador18.png",width=9, height=5)

##Guatemala:INDEX18 deflactado------------------------------------
Guatemala <- EconomicSector[ which(EconomicSector$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps4]
Guatemala <- Guatemala %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Guatemala <- Guatemala %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Guatemala <- Guatemala[keeps1]

gi2.Guatemala18 <- ggplot(data = Guatemala, aes(x=Date, y=Values)) + 
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
  ggtitle("Guatemala: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Guatemala18 <- gi2.Guatemala18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Guatemala18 <- gi2.Guatemala18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Guatemala18 <- gi2.Guatemala18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Guatemala18 <- gi2.Guatemala18+labs(colour = "Credit Type")
gi2.Guatemala18

#Facets
Guatemala <- EconomicSector[ which(EconomicSector$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps4]
Guatemala <- Guatemala %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Guatemala <- Guatemala %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Guatemala <- Guatemala[keeps1]

gi2.Guatemala18f <- ggplot(data = Guatemala, aes(x=Date, y=Values)) + 
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
  #ggtitle("Guatemala: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Guatemala18f <- gi2.Guatemala18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Guatemala18f <- gi2.Guatemala18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Guatemala18f <- gi2.Guatemala18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Guatemala18f <- gi2.Guatemala18f+labs(colour = "Credit Type")
gi2.Guatemala18f
#Arrange
ggarrange(gi2.Guatemala18, gi2.Guatemala18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Guatemala18.png",width=9, height=5)

##Honduras:INDEX18 deflactado------------------------------------
Honduras <- EconomicSector[ which(EconomicSector$Country=="Honduras"), ]
Honduras <- Honduras[keeps4]
Honduras <- Honduras %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Honduras <- Honduras %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Honduras <- Honduras[keeps1]

gi2.Honduras18 <- ggplot(data = Honduras, aes(x=Date, y=Values)) + 
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
  ggtitle("Honduras: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Honduras18 <- gi2.Honduras18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Honduras18 <- gi2.Honduras18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Honduras18 <- gi2.Honduras18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Honduras18 <- gi2.Honduras18+labs(colour = "Credit Type")
gi2.Honduras18

#Facets
Honduras <- EconomicSector[ which(EconomicSector$Country=="Honduras"), ]
Honduras <- Honduras[keeps4]
Honduras <- Honduras %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Honduras <- Honduras %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Honduras <- Honduras[keeps1]

gi2.Honduras18f <- ggplot(data = Honduras, aes(x=Date, y=Values)) + 
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
  #ggtitle("Honduras: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Honduras18f <- gi2.Honduras18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Honduras18f <- gi2.Honduras18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Honduras18f <- gi2.Honduras18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Honduras18f <- gi2.Honduras18f+labs(colour = "Credit Type")
gi2.Honduras18f
#Arrange
ggarrange(gi2.Honduras18, gi2.Honduras18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Honduras18.png",width=9, height=5)

##Mexico:INDEX18 deflactado------------------------------------
Mexico <- EconomicSector[ which(EconomicSector$Country=="Mexico"), ]
Mexico <- Mexico[keeps4]
Mexico <- Mexico %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Mexico <- Mexico %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Mexico <- Mexico[keeps1]

gi2.Mexico18 <- ggplot(data = Mexico, aes(x=Date, y=Values)) + 
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
  ggtitle("Mexico: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Mexico18 <- gi2.Mexico18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Mexico18 <- gi2.Mexico18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Mexico18 <- gi2.Mexico18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Mexico18 <- gi2.Mexico18+labs(colour = "Credit Type")
gi2.Mexico18

#Facets
Mexico <- EconomicSector[ which(EconomicSector$Country=="Mexico"), ]
Mexico <- Mexico[keeps4]
Mexico <- Mexico %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Mexico <- Mexico %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Mexico <- Mexico[keeps1]

gi2.Mexico18f <- ggplot(data = Mexico, aes(x=Date, y=Values)) + 
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
  #ggtitle("Mexico: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Mexico18f <- gi2.Mexico18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Mexico18f <- gi2.Mexico18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Mexico18f <- gi2.Mexico18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Mexico18f <- gi2.Mexico18f+labs(colour = "Credit Type")
gi2.Mexico18f
#Arrange
ggarrange(gi2.Mexico18, gi2.Mexico18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Mexico18.png",width=9, height=5)

##Nicaragua:INDEX18 deflactado------------------------------------
Nicaragua <- EconomicSector[ which(EconomicSector$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps4]
Nicaragua <- Nicaragua %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Nicaragua <- Nicaragua %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Nicaragua <- Nicaragua[keeps1]

gi2.Nicaragua18 <- ggplot(data = Nicaragua, aes(x=Date, y=Values)) + 
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
  ggtitle("Nicaragua: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Nicaragua18 <- gi2.Nicaragua18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Nicaragua18 <- gi2.Nicaragua18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Nicaragua18 <- gi2.Nicaragua18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Nicaragua18 <- gi2.Nicaragua18+labs(colour = "Credit Type")
gi2.Nicaragua18

#Facets
Nicaragua <- EconomicSector[ which(EconomicSector$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps4]
Nicaragua <- Nicaragua %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Nicaragua <- Nicaragua %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Nicaragua <- Nicaragua[keeps1]

gi2.Nicaragua18f <- ggplot(data = Nicaragua, aes(x=Date, y=Values)) + 
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
  #ggtitle("Nicaragua: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Nicaragua18f <- gi2.Nicaragua18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Nicaragua18f <- gi2.Nicaragua18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Nicaragua18f <- gi2.Nicaragua18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Nicaragua18f <- gi2.Nicaragua18f+labs(colour = "Credit Type")
gi2.Nicaragua18f
#Arrange
ggarrange(gi2.Nicaragua18, gi2.Nicaragua18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Nicaragua18.png",width=9, height=5)

##Panama:INDEX18 deflactado------------------------------------
Panama <- EconomicSector[ which(EconomicSector$Country=="Panama"), ]
Panama <- Panama[keeps4]
Panama <- Panama %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Panama <- Panama %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Panama <- Panama[keeps1]

gi2.Panama18 <- ggplot(data = Panama, aes(x=Date, y=Values)) + 
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
  ggtitle("Panama: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Panama18 <- gi2.Panama18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Panama18 <- gi2.Panama18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Panama18 <- gi2.Panama18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Panama18 <- gi2.Panama18+labs(colour = "Credit Type")
gi2.Panama18

#Facets
Panama <- EconomicSector[ which(EconomicSector$Country=="Panama"), ]
Panama <- Panama[keeps4]
Panama <- Panama %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Panama <- Panama %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Panama <- Panama[keeps1]

gi2.Panama18f <- ggplot(data = Panama, aes(x=Date, y=Values)) + 
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
  #ggtitle("Panama: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Panama18f <- gi2.Panama18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Panama18f <- gi2.Panama18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Panama18f <- gi2.Panama18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Panama18f <- gi2.Panama18f+labs(colour = "Credit Type")
gi2.Panama18f
#Arrange
ggarrange(gi2.Panama18, gi2.Panama18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Panama18.png",width=9, height=5)



##Paraguay:INDEX18 deflactado------------------------------------
Paraguay <- EconomicSector[ which(EconomicSector$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps4]
Paraguay <- Paraguay %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Paraguay <- Paraguay %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Paraguay <- Paraguay[keeps1]

gi2.Paraguay18 <- ggplot(data = Paraguay, aes(x=Date, y=Values)) + 
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
  ggtitle("Paraguay: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Paraguay18 <- gi2.Paraguay18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Paraguay18 <- gi2.Paraguay18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Paraguay18 <- gi2.Paraguay18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Paraguay18 <- gi2.Paraguay18+labs(colour = "Credit Type")
gi2.Paraguay18

#Facets
Paraguay <- EconomicSector[ which(EconomicSector$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps4]
Paraguay <- Paraguay %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Paraguay <- Paraguay %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Paraguay <- Paraguay[keeps1]

gi2.Paraguay18f <- ggplot(data = Paraguay, aes(x=Date, y=Values)) + 
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
  #ggtitle("Paraguay: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Paraguay18f <- gi2.Paraguay18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Paraguay18f <- gi2.Paraguay18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Paraguay18f <- gi2.Paraguay18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Paraguay18f <- gi2.Paraguay18f+labs(colour = "Credit Type")
gi2.Paraguay18f
#Arrange
ggarrange(gi2.Paraguay18, gi2.Paraguay18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Paraguay18.png",width=9, height=5)



##Peru:INDEX18 deflactado------------------------------------
Peru <- EconomicSector[ which(EconomicSector$Country=="Peru"), ]
Peru <- Peru[keeps4]
Peru <- Peru %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Peru <- Peru %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Peru <- Peru[keeps1]

gi2.Peru18 <- ggplot(data = Peru, aes(x=Date, y=Values)) + 
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
  ggtitle("Peru: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi2.Peru18 <- gi2.Peru18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Peru18 <- gi2.Peru18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Peru18 <- gi2.Peru18+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Peru18 <- gi2.Peru18+labs(colour = "Credit Type")
gi2.Peru18

#Facets
Peru <- EconomicSector[ which(EconomicSector$Country=="Peru"), ]
Peru <- Peru[keeps4]
Peru <- Peru %>% gather(Name, Values, iAgricultural18ipc:iT.productive18ipc)
Peru <- Peru %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Peru <- Peru[keeps1]

gi2.Peru18f <- ggplot(data = Peru, aes(x=Date, y=Values)) + 
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
  #ggtitle("Peru: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi2.Peru18f <- gi2.Peru18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi2.Peru18f <- gi2.Peru18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi2.Peru18f <- gi2.Peru18f+theme(legend.key.size = unit(0.9, "lines"))
#gi2.Peru18f <- gi2.Peru18f+labs(colour = "Credit Type")
gi2.Peru18f
#Arrange
ggarrange(gi2.Peru18, gi2.Peru18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gi2.Peru18.png",width=9, height=5)




