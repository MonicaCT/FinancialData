rm(list = ls())
#Setting directory-----
clean <-  paste(getwd(),"/Data/Clean",sep="")
figures <-  paste(getwd(),"/Outputs/Figures/F-country",sep="")
tables <-  paste(getwd(),"/Outputs/Tables",sep="")
#Packages-------------
pacman::p_load(tidyverse,dplyr,ggplot2,hrbrthemes,plotly,lubridate,ggpubr)
#Abriendo el archivo final-------
CreditType <- read.csv(paste(clean,"CreditType.final.csv",sep="/"))
#COn el archivo descargado
class(CreditType$Date)
CreditType$Date <- as.Date(CreditType$Date, format = "%Y-%m-%d")
class(CreditType$Mortgage)
cols = c(3:20)
CreditType[, cols] = apply(CreditType[, cols], 2, function(x)
  as.numeric(as.character(x)))

####### INDEX18------------------------------
#*######By Country-----------------------------------
keeps2 <- c("Date","CreditCard","Mortgage",
            "ConsumerCredit","SMEs","Totalem","Microcredit")
keeps3 <- c("Date","iCreditCard18","iMortgage18",
            "iConsumerCredit18","iSMEs18","iTotalem18","iMicrocredit18")
##Argentina:INDEX18------------------------------------
Argentina <- CreditType[ which(CreditType$Country=="Argentina"), ]
Argentina <- Argentina[keeps3]
Argentina <- Argentina %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Argentina <- CreditType[ which(CreditType$Country=="Argentina"), ]
Argentina <- Argentina[keeps3]
Argentina <- Argentina %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Argentina18.png",width=9, height=5)

##Bolivia:INDEX18------------------------------------
Bolivia <- CreditType[ which(CreditType$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps3]
Bolivia <- Bolivia %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Bolivia <- CreditType[ which(CreditType$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps3]
Bolivia <- Bolivia %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Bolivia18.png",width=9, height=5)

##Brazil:INDEX18------------------------------------
Brazil <- CreditType[ which(CreditType$Country=="Brazil"), ]
Brazil <- Brazil[keeps3]
Brazil <- Brazil %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Brazil <- CreditType[ which(CreditType$Country=="Brazil"), ]
Brazil <- Brazil[keeps3]
Brazil <- Brazil %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Brazil18.png",width=9, height=5)

##Chile:INDEX18------------------------------------
Chile <- CreditType[ which(CreditType$Country=="Chile"), ]
Chile <- Chile[keeps3]
Chile <- Chile %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Chile <- CreditType[ which(CreditType$Country=="Chile"), ]
Chile <- Chile[keeps3]
Chile <- Chile %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Chile18.png",width=9, height=5)

##Colombia:INDEX18------------------------------------
Colombia <- CreditType[ which(CreditType$Country=="Colombia"), ]
Colombia <- Colombia[keeps3]
Colombia <- Colombia %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
Colombia <- Colombia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Colombia <- Colombia[keeps1]

gi.Colombia18 <- ggplot(data = Colombia, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Colombia$Name)))+
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
  ggtitle("Colombia: INDEX18") +
  xlab("Date") + ylab("")
#gi.Colombia18 <- gi.Colombia18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Colombia18 <- gi.Colombia18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Colombia18 <- gi.Colombia18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Colombia18 <- gi.Colombia18+labs(colour = "Credit Type")
gi.Colombia18

#Facets
Colombia <- CreditType[ which(CreditType$Country=="Colombia"), ]
Colombia <- Colombia[keeps3]
Colombia <- Colombia %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
Colombia <- Colombia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Colombia <- Colombia[keeps1]

gi.Colombia18f <- ggplot(data = Colombia, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Colombia$Name)))+
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
  #ggtitle("Colombia: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Colombia18f <- gi.Colombia18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Colombia18f <- gi.Colombia18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Colombia18f <- gi.Colombia18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Colombia18f <- gi.Colombia18f+labs(colour = "Credit Type")
gi.Colombia18f
#Arrange
ggarrange(gi.Colombia18, gi.Colombia18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gipc.Colombia18.png",width=9, height=5)

##CostaRica:INDEX18------------------------------------
CostaRica <- CreditType[ which(CreditType$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps3]
CostaRica <- CostaRica %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
CostaRica <- CreditType[ which(CreditType$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps3]
CostaRica <- CostaRica %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.CostaRica18.png",width=9, height=5)

##DominicanRepublic:INDEX18------------------------------------
DominicanRepublic <- CreditType[ which(CreditType$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps3]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
DominicanRepublic <- CreditType[ which(CreditType$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps3]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.DominicanRepublic18.png",width=9, height=5)

##Ecuador:INDEX18------------------------------------
Ecuador <- CreditType[ which(CreditType$Country=="Ecuador"), ]
Ecuador <- Ecuador[keeps3]
Ecuador <- Ecuador %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
Ecuador <- Ecuador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Ecuador <- Ecuador[keeps1]

gi.Ecuador18 <- ggplot(data = Ecuador, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Ecuador$Name)))+
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
  ggtitle("Ecuador: INDEX18") +
  xlab("Date") + ylab("")
#gi.Ecuador18 <- gi.Ecuador18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Ecuador18 <- gi.Ecuador18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Ecuador18 <- gi.Ecuador18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Ecuador18 <- gi.Ecuador18+labs(colour = "Credit Type")
gi.Ecuador18

#Facets
Ecuador <- CreditType[ which(CreditType$Country=="Ecuador"), ]
Ecuador <- Ecuador[keeps3]
Ecuador <- Ecuador %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
Ecuador <- Ecuador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Ecuador <- Ecuador[keeps1]

gi.Ecuador18f <- ggplot(data = Ecuador, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Ecuador$Name)))+
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
  #ggtitle("Ecuador: INDEX18") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Ecuador18f <- gi.Ecuador18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Ecuador18f <- gi.Ecuador18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Ecuador18f <- gi.Ecuador18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Ecuador18f <- gi.Ecuador18f+labs(colour = "Credit Type")
gi.Ecuador18f
#Arrange
ggarrange(gi.Ecuador18, gi.Ecuador18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gipc.Ecuador18.png",width=9, height=5)

##ElSalvador:INDEX18------------------------------------
ElSalvador <- CreditType[ which(CreditType$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps3]
ElSalvador <- ElSalvador %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ElSalvador <- CreditType[ which(CreditType$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps3]
ElSalvador <- ElSalvador %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.ElSalvador18.png",width=9, height=5)

##Guatemala:INDEX18------------------------------------
Guatemala <- CreditType[ which(CreditType$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps3]
Guatemala <- Guatemala %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Guatemala <- CreditType[ which(CreditType$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps3]
Guatemala <- Guatemala %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Guatemala18.png",width=9, height=5)

##Honduras:INDEX18------------------------------------
Honduras <- CreditType[ which(CreditType$Country=="Honduras"), ]
Honduras <- Honduras[keeps3]
Honduras <- Honduras %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Honduras <- CreditType[ which(CreditType$Country=="Honduras"), ]
Honduras <- Honduras[keeps3]
Honduras <- Honduras %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Honduras18.png",width=9, height=5)

##Mexico:INDEX18------------------------------------
Mexico <- CreditType[ which(CreditType$Country=="Mexico"), ]
Mexico <- Mexico[keeps3]
Mexico <- Mexico %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Mexico <- CreditType[ which(CreditType$Country=="Mexico"), ]
Mexico <- Mexico[keeps3]
Mexico <- Mexico %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Mexico18.png",width=9, height=5)

##Nicaragua:INDEX18------------------------------------
Nicaragua <- CreditType[ which(CreditType$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps3]
Nicaragua <- Nicaragua %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Nicaragua <- CreditType[ which(CreditType$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps3]
Nicaragua <- Nicaragua %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Nicaragua18.png",width=9, height=5)

##Panama:INDEX18------------------------------------
Panama <- CreditType[ which(CreditType$Country=="Panama"), ]
Panama <- Panama[keeps3]
Panama <- Panama %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Panama <- CreditType[ which(CreditType$Country=="Panama"), ]
Panama <- Panama[keeps3]
Panama <- Panama %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Panama18.png",width=9, height=5)

##Paraguay:INDEX18------------------------------------
Paraguay <- CreditType[ which(CreditType$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps3]
Paraguay <- Paraguay %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Paraguay <- CreditType[ which(CreditType$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps3]
Paraguay <- Paraguay %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Paraguay18.png",width=9, height=5)

##Peru:INDEX18------------------------------------
Peru <- CreditType[ which(CreditType$Country=="Peru"), ]
Peru <- Peru[keeps3]
Peru <- Peru %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
Peru <- CreditType[ which(CreditType$Country=="Peru"), ]
Peru <- Peru[keeps3]
Peru <- Peru %>% gather(Name, Values, iCreditCard18:iMicrocredit18)
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
ggsave(path = figures, filename = "gipc.Peru18.png",width=9, height=5)

####### INDEX18 deflactado------------------------------
#*######By Country-----------------------------------
keeps4 <- c("Date","iCreditCard18ipc","iMortgage18ipc",
            "iConsumerCredit18ipc","iSMEs18ipc","iTotalem18ipc","iMicrocredit18ipc")
##Argentina:INDEX18 deflactado------------------------------------
Argentina <- CreditType[ which(CreditType$Country=="Argentina"), ]
Argentina <- Argentina[keeps4]
Argentina <- Argentina %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("ARGENTINA: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Argentina18 <- gi.Argentina18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Argentina18 <- gi.Argentina18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Argentina18 <- gi.Argentina18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Argentina18 <- gi.Argentina18+labs(colour = "Credit Type")
gi.Argentina18

#Facets
Argentina <- CreditType[ which(CreditType$Country=="Argentina"), ]
Argentina <- Argentina[keeps4]
Argentina <- Argentina %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("ARGENTINA: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Argentina18.png",width=9, height=5)

##Bolivia:INDEX18 deflactado------------------------------------
Bolivia <- CreditType[ which(CreditType$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps4]
Bolivia <- Bolivia %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Bolivia: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Bolivia18 <- gi.Bolivia18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Bolivia18 <- gi.Bolivia18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Bolivia18 <- gi.Bolivia18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Bolivia18 <- gi.Bolivia18+labs(colour = "Credit Type")
gi.Bolivia18

#Facets
Bolivia <- CreditType[ which(CreditType$Country=="Bolivia"), ]
Bolivia <- Bolivia[keeps4]
Bolivia <- Bolivia %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Bolivia: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Bolivia18.png",width=9, height=5)

##Brazil:INDEX18 deflactado------------------------------------
Brazil <- CreditType[ which(CreditType$Country=="Brazil"), ]
Brazil <- Brazil[keeps4]
Brazil <- Brazil %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Brazil: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Brazil18 <- gi.Brazil18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Brazil18 <- gi.Brazil18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Brazil18 <- gi.Brazil18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Brazil18 <- gi.Brazil18+labs(colour = "Credit Type")
gi.Brazil18

#Facets
Brazil <- CreditType[ which(CreditType$Country=="Brazil"), ]
Brazil <- Brazil[keeps4]
Brazil <- Brazil %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Brazil: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Brazil18.png",width=9, height=5)

##Chile:INDEX18 deflactado------------------------------------
Chile <- CreditType[ which(CreditType$Country=="Chile"), ]
Chile <- Chile[keeps4]
Chile <- Chile %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Chile: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Chile18 <- gi.Chile18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Chile18 <- gi.Chile18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Chile18 <- gi.Chile18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Chile18 <- gi.Chile18+labs(colour = "Credit Type")
gi.Chile18

#Facets
Chile <- CreditType[ which(CreditType$Country=="Chile"), ]
Chile <- Chile[keeps4]
Chile <- Chile %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Chile: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Chile18.png",width=9, height=5)

##Colombia:INDEX18 deflactado------------------------------------
Colombia <- CreditType[ which(CreditType$Country=="Colombia"), ]
Colombia <- Colombia[keeps4]
Colombia <- Colombia %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
Colombia <- Colombia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Colombia <- Colombia[keeps1]

gi.Colombia18 <- ggplot(data = Colombia, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Colombia$Name)))+
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
  ggtitle("Colombia: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Colombia18 <- gi.Colombia18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Colombia18 <- gi.Colombia18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Colombia18 <- gi.Colombia18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Colombia18 <- gi.Colombia18+labs(colour = "Credit Type")
gi.Colombia18

#Facets
Colombia <- CreditType[ which(CreditType$Country=="Colombia"), ]
Colombia <- Colombia[keeps4]
Colombia <- Colombia %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
Colombia <- Colombia %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Colombia <- Colombia[keeps1]

gi.Colombia18f <- ggplot(data = Colombia, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Colombia$Name)))+
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
  #ggtitle("Colombia: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Colombia18f <- gi.Colombia18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Colombia18f <- gi.Colombia18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Colombia18f <- gi.Colombia18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Colombia18f <- gi.Colombia18f+labs(colour = "Credit Type")
gi.Colombia18f
#Arrange
ggarrange(gi.Colombia18, gi.Colombia18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gipc2Colombia18.png",width=9, height=5)

##CostaRica:INDEX18 deflactado------------------------------------
CostaRica <- CreditType[ which(CreditType$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps4]
CostaRica <- CostaRica %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Costa Rica: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.CostaRica18 <- gi.CostaRica18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.CostaRica18 <- gi.CostaRica18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.CostaRica18 <- gi.CostaRica18+theme(legend.key.size = unit(0.9, "lines"))
#gi.CostaRica18 <- gi.CostaRica18+labs(colour = "Credit Type")
gi.CostaRica18

#Facets
CostaRica <- CreditType[ which(CreditType$Country=="Costa Rica"), ]
CostaRica <- CostaRica[keeps4]
CostaRica <- CostaRica %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("CostaRica: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2CostaRica18.png",width=9, height=5)

##DominicanRepublic:INDEX18 deflactado------------------------------------
DominicanRepublic <- CreditType[ which(CreditType$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps4]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Dominican Republic: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.DominicanRepublic18 <- gi.DominicanRepublic18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.DominicanRepublic18 <- gi.DominicanRepublic18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.DominicanRepublic18 <- gi.DominicanRepublic18+theme(legend.key.size = unit(0.9, "lines"))
#gi.DominicanRepublic18 <- gi.DominicanRepublic18+labs(colour = "Credit Type")
gi.DominicanRepublic18

#Facets
DominicanRepublic <- CreditType[ which(CreditType$Country=="Dominican Republic"), ]
DominicanRepublic <- DominicanRepublic[keeps4]
DominicanRepublic <- DominicanRepublic %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("DominicanRepublic: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2DominicanRepublic18.png",width=9, height=5)

##Ecuador:INDEX18 deflactado------------------------------------
Ecuador <- CreditType[ which(CreditType$Country=="Ecuador"), ]
Ecuador <- Ecuador[keeps4]
Ecuador <- Ecuador %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
Ecuador <- Ecuador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Ecuador <- Ecuador[keeps1]

gi.Ecuador18 <- ggplot(data = Ecuador, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Ecuador$Name)))+
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
  ggtitle("Ecuador: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Ecuador18 <- gi.Ecuador18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Ecuador18 <- gi.Ecuador18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Ecuador18 <- gi.Ecuador18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Ecuador18 <- gi.Ecuador18+labs(colour = "Credit Type")
gi.Ecuador18

#Facets
Ecuador <- CreditType[ which(CreditType$Country=="Ecuador"), ]
Ecuador <- Ecuador[keeps4]
Ecuador <- Ecuador %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
Ecuador <- Ecuador %>% filter(!is.na(Values))
keeps1 <- c("Date", "Name","Values")
Ecuador <- Ecuador[keeps1]

gi.Ecuador18f <- ggplot(data = Ecuador, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Name))+
  geom_point(aes(color = Name, shape = Name)) +
  scale_shape_manual(values = 0:length(unique(Ecuador$Name)))+
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
  #ggtitle("Ecuador: INDEX18 deflactado") +
  xlab("Date") + ylab("")+
  facet_wrap(facets =  vars(Name))
#gi.Ecuador18f <- gi.Ecuador18f+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Ecuador18f <- gi.Ecuador18f+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Ecuador18f <- gi.Ecuador18f+theme(legend.key.size = unit(0.9, "lines"))
#gi.Ecuador18f <- gi.Ecuador18f+labs(colour = "Credit Type")
gi.Ecuador18f
#Arrange
ggarrange(gi.Ecuador18, gi.Ecuador18f,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "gipc2Ecuador18.png",width=9, height=5)

##ElSalvador:INDEX18 deflactado------------------------------------
ElSalvador <- CreditType[ which(CreditType$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps4]
ElSalvador <- ElSalvador %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("El Salvador: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.ElSalvador18 <- gi.ElSalvador18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.ElSalvador18 <- gi.ElSalvador18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.ElSalvador18 <- gi.ElSalvador18+theme(legend.key.size = unit(0.9, "lines"))
#gi.ElSalvador18 <- gi.ElSalvador18+labs(colour = "Credit Type")
gi.ElSalvador18

#Facets
ElSalvador <- CreditType[ which(CreditType$Country=="El Salvador"), ]
ElSalvador <- ElSalvador[keeps4]
ElSalvador <- ElSalvador %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("ElSalvador: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2ElSalvador18.png",width=9, height=5)

##Guatemala:INDEX18 deflactado------------------------------------
Guatemala <- CreditType[ which(CreditType$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps4]
Guatemala <- Guatemala %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Guatemala: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Guatemala18 <- gi.Guatemala18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Guatemala18 <- gi.Guatemala18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Guatemala18 <- gi.Guatemala18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Guatemala18 <- gi.Guatemala18+labs(colour = "Credit Type")
gi.Guatemala18

#Facets
Guatemala <- CreditType[ which(CreditType$Country=="Guatemala"), ]
Guatemala <- Guatemala[keeps4]
Guatemala <- Guatemala %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Guatemala: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Guatemala18.png",width=9, height=5)

##Honduras:INDEX18 deflactado------------------------------------
Honduras <- CreditType[ which(CreditType$Country=="Honduras"), ]
Honduras <- Honduras[keeps4]
Honduras <- Honduras %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Honduras: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Honduras18 <- gi.Honduras18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Honduras18 <- gi.Honduras18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Honduras18 <- gi.Honduras18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Honduras18 <- gi.Honduras18+labs(colour = "Credit Type")
gi.Honduras18

#Facets
Honduras <- CreditType[ which(CreditType$Country=="Honduras"), ]
Honduras <- Honduras[keeps4]
Honduras <- Honduras %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Honduras: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Honduras18.png",width=9, height=5)

##Mexico:INDEX18 deflactado------------------------------------
Mexico <- CreditType[ which(CreditType$Country=="Mexico"), ]
Mexico <- Mexico[keeps4]
Mexico <- Mexico %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Mexico: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Mexico18 <- gi.Mexico18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Mexico18 <- gi.Mexico18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Mexico18 <- gi.Mexico18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Mexico18 <- gi.Mexico18+labs(colour = "Credit Type")
gi.Mexico18

#Facets
Mexico <- CreditType[ which(CreditType$Country=="Mexico"), ]
Mexico <- Mexico[keeps4]
Mexico <- Mexico %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Mexico: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Mexico18.png",width=9, height=5)

##Nicaragua:INDEX18 deflactado------------------------------------
Nicaragua <- CreditType[ which(CreditType$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps4]
Nicaragua <- Nicaragua %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Nicaragua: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Nicaragua18 <- gi.Nicaragua18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Nicaragua18 <- gi.Nicaragua18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Nicaragua18 <- gi.Nicaragua18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Nicaragua18 <- gi.Nicaragua18+labs(colour = "Credit Type")
gi.Nicaragua18

#Facets
Nicaragua <- CreditType[ which(CreditType$Country=="Nicaragua"), ]
Nicaragua <- Nicaragua[keeps4]
Nicaragua <- Nicaragua %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Nicaragua: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Nicaragua18.png",width=9, height=5)

##Panama:INDEX18 deflactado------------------------------------
Panama <- CreditType[ which(CreditType$Country=="Panama"), ]
Panama <- Panama[keeps4]
Panama <- Panama %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Panama: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Panama18 <- gi.Panama18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Panama18 <- gi.Panama18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Panama18 <- gi.Panama18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Panama18 <- gi.Panama18+labs(colour = "Credit Type")
gi.Panama18

#Facets
Panama <- CreditType[ which(CreditType$Country=="Panama"), ]
Panama <- Panama[keeps4]
Panama <- Panama %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Panama: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Panama18.png",width=9, height=5)



##Paraguay:INDEX18 deflactado------------------------------------
Paraguay <- CreditType[ which(CreditType$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps4]
Paraguay <- Paraguay %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Paraguay: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Paraguay18 <- gi.Paraguay18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Paraguay18 <- gi.Paraguay18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Paraguay18 <- gi.Paraguay18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Paraguay18 <- gi.Paraguay18+labs(colour = "Credit Type")
gi.Paraguay18

#Facets
Paraguay <- CreditType[ which(CreditType$Country=="Paraguay"), ]
Paraguay <- Paraguay[keeps4]
Paraguay <- Paraguay %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Paraguay: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Paraguay18.png",width=9, height=5)



##Peru:INDEX18 deflactado------------------------------------
Peru <- CreditType[ which(CreditType$Country=="Peru"), ]
Peru <- Peru[keeps4]
Peru <- Peru %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  ggtitle("Peru: INDEX18 deflactado") +
  xlab("Date") + ylab("")
#gi.Peru18 <- gi.Peru18+scale_y_continuous(breaks = seq(0.7, 2, by = 0.3),limits = c(0.7,2))
gi.Peru18 <- gi.Peru18+theme(axis.text.x = element_text(angle = 60, hjust = 1))
gi.Peru18 <- gi.Peru18+theme(legend.key.size = unit(0.9, "lines"))
#gi.Peru18 <- gi.Peru18+labs(colour = "Credit Type")
gi.Peru18

#Facets
Peru <- CreditType[ which(CreditType$Country=="Peru"), ]
Peru <- Peru[keeps4]
Peru <- Peru %>% gather(Name, Values, iCreditCard18ipc:iMicrocredit18ipc)
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
  #ggtitle("Peru: INDEX18 deflactado") +
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
ggsave(path = figures, filename = "gipc2Peru18.png",width=9, height=5)




