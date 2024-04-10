rm(list = ls())
#Setting directory-----
clean <-  paste(getwd(),"/Data/Clean",sep="")
figures <-  paste(getwd(),"/Outputs/Figures/F-creditt",sep="")
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

####### Graphics------------------------------
#*******CREDIT CARD-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iCreditCard18")
iCreditCardi <- CreditType[keeps1]
#Long data
iCreditCardi <- iCreditCardi %>% gather(CreditType, Values, iCreditCard18)
iCreditCardi <- iCreditCardi %>% filter(!is.na(Values))
giIiCreditCard.r <- ggplot(data = iCreditCardi, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iCreditCardi$Country)))+
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
  ggtitle("Credit Card: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiCreditCard.r <- giIiCreditCard.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiCreditCard.r <- giIiCreditCard.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiCreditCard.r
#Reales--------------
keeps1 <- c("Date", "Country", "iCreditCard18ipc")
iCreditCard18ipci <- CreditType[keeps1]
#Long data
iCreditCard18ipci <- iCreditCard18ipci %>% gather(CreditType, Values, iCreditCard18ipc)
iCreditCard18ipci <- iCreditCard18ipci %>% filter(!is.na(Values))
giIiCreditCard18ipc <- ggplot(data = iCreditCard18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iCreditCard18ipci$Country)))+
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
  ggtitle("Credit Card: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiCreditCard18ipc <- giIiCreditCard18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiCreditCard18ipc <- giIiCreditCard18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiCreditCard18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiCreditCard.r, giIiCreditCard18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-credicard.png",width=9, height=5)

#*******MORTGAGE-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iMortgage18")
iMortgagei <- CreditType[keeps1]
#Long data
iMortgagei <- iMortgagei %>% gather(CreditType, Values, iMortgage18)
iMortgagei <- iMortgagei %>% filter(!is.na(Values))
giIiMortgage.r <- ggplot(data = iMortgagei, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iMortgagei$Country)))+
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
  ggtitle("Mortgage: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiMortgage.r <- giIiMortgage.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiMortgage.r <- giIiMortgage.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiMortgage.r
#Reales--------------
keeps1 <- c("Date", "Country", "iMortgage18ipc")
iMortgage18ipci <- CreditType[keeps1]
#Long data
iMortgage18ipci <- iMortgage18ipci %>% gather(CreditType, Values, iMortgage18ipc)
iMortgage18ipci <- iMortgage18ipci %>% filter(!is.na(Values))
giIiMortgage18ipc <- ggplot(data = iMortgage18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iMortgage18ipci$Country)))+
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
  ggtitle("Mortgage: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiMortgage18ipc <- giIiMortgage18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiMortgage18ipc <- giIiMortgage18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiMortgage18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiMortgage.r, giIiMortgage18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-mortgage.png",width=9, height=5)

#*******CONSUMER CREDIT-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iConsumerCredit18")
iConsumerCrediti <- CreditType[keeps1]
#Long data
iConsumerCrediti <- iConsumerCrediti %>% gather(CreditType, Values, iConsumerCredit18)
iConsumerCrediti <- iConsumerCrediti %>% filter(!is.na(Values))
giIiConsumerCredit.r <- ggplot(data = iConsumerCrediti, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iConsumerCrediti$Country)))+
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
  ggtitle("Consumer Credit: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiConsumerCredit.r <- giIiConsumerCredit.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiConsumerCredit.r <- giIiConsumerCredit.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiConsumerCredit.r
#Reales--------------
keeps1 <- c("Date", "Country", "iConsumerCredit18ipc")
iConsumerCredit18ipci <- CreditType[keeps1]
#Long data
iConsumerCredit18ipci <- iConsumerCredit18ipci %>% gather(CreditType, Values, iConsumerCredit18ipc)
iConsumerCredit18ipci <- iConsumerCredit18ipci %>% filter(!is.na(Values))
giIiConsumerCredit18ipc <- ggplot(data = iConsumerCredit18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iConsumerCredit18ipci$Country)))+
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
  ggtitle("Consumer Credit: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiConsumerCredit18ipc <- giIiConsumerCredit18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiConsumerCredit18ipc <- giIiConsumerCredit18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiConsumerCredit18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiConsumerCredit.r, giIiConsumerCredit18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-conscredit.png",width=9, height=5)

#*******MICROCREDIT-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iMicrocredit18")
iMicrocrediti <- CreditType[keeps1]
#Long data
iMicrocrediti <- iMicrocrediti %>% gather(CreditType, Values, iMicrocredit18)
iMicrocrediti <- iMicrocrediti %>% filter(!is.na(Values))
giIiMicrocredit.r <- ggplot(data = iMicrocrediti, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iMicrocrediti$Country)))+
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
  ggtitle("Microcredit: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiMicrocredit.r <- giIiMicrocredit.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiMicrocredit.r <- giIiMicrocredit.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiMicrocredit.r
#Reales--------------
keeps1 <- c("Date", "Country", "iMicrocredit18ipc")
iMicrocredit18ipci <- CreditType[keeps1]
#Long data
iMicrocredit18ipci <- iMicrocredit18ipci %>% gather(CreditType, Values, iMicrocredit18ipc)
iMicrocredit18ipci <- iMicrocredit18ipci %>% filter(!is.na(Values))
giIiMicrocredit18ipc <- ggplot(data = iMicrocredit18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iMicrocredit18ipci$Country)))+
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
  ggtitle("Microcredit: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiMicrocredit18ipc <- giIiMicrocredit18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiMicrocredit18ipc <- giIiMicrocredit18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiMicrocredit18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiMicrocredit.r, giIiMicrocredit18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-microcredit.png",width=9, height=5)


#*******SMEs-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iSMEs18")
iSMEsi <- CreditType[keeps1]
#Long data
iSMEsi <- iSMEsi %>% gather(CreditType, Values, iSMEs18)
iSMEsi <- iSMEsi %>% filter(!is.na(Values))
giIiSMEs.r <- ggplot(data = iSMEsi, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iSMEsi$Country)))+
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
  ggtitle("SMEs: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiSMEs.r <- giIiSMEs.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiSMEs.r <- giIiSMEs.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiSMEs.r
#Reales--------------
keeps1 <- c("Date", "Country", "iSMEs18ipc")
iSMEs18ipci <- CreditType[keeps1]
#Long data
iSMEs18ipci <- iSMEs18ipci %>% gather(CreditType, Values, iSMEs18ipc)
iSMEs18ipci <- iSMEs18ipci %>% filter(!is.na(Values))
giIiSMEs18ipc <- ggplot(data = iSMEs18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iSMEs18ipci$Country)))+
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
  ggtitle("SMEs: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiSMEs18ipc <- giIiSMEs18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiSMEs18ipc <- giIiSMEs18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiSMEs18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiSMEs.r, giIiSMEs18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-smes.png",width=9, height=5)

#*******TOTALEMP-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iTotalem18")
iTotalemi <- CreditType[keeps1]
#Long data
iTotalemi <- iTotalemi %>% gather(CreditType, Values, iTotalem18)
iTotalemi <- iTotalemi %>% filter(!is.na(Values))
giIiTotalem.r <- ggplot(data = iTotalemi, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iTotalemi$Country)))+
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
  ggtitle("Total enterprises: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiTotalem.r <- giIiTotalem.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiTotalem.r <- giIiTotalem.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiTotalem.r
#Reales--------------
keeps1 <- c("Date", "Country", "iTotalem18ipc")
iTotalem18ipci <- CreditType[keeps1]
#Long data
iTotalem18ipci <- iTotalem18ipci %>% gather(CreditType, Values, iTotalem18ipc)
iTotalem18ipci <- iTotalem18ipci %>% filter(!is.na(Values))
giIiTotalem18ipc <- ggplot(data = iTotalem18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iTotalem18ipci$Country)))+
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
  ggtitle("Total enterprises: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiTotalem18ipc <- giIiTotalem18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiTotalem18ipc <- giIiTotalem18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiTotalem18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiTotalem.r, giIiTotalem18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-totalem.png",width=9, height=5)

