rm(list = ls())
#Setting directory-----
clean <-  paste(getwd(),"/Data.ES/Clean.ES",sep="")
figures <-  paste(getwd(),"/Outputs.ES/Figures.ES/F-creditt",sep="")
tables <-  paste(getwd(),"/Outputs.ES/Tables.ES",sep="")
#Packages-------------
pacman::p_load(tidyverse,dplyr,ggplot2,hrbrthemes,plotly,lubridate,ggpubr)
#Abriendo el archivo final-------
EconomicSector <- read.csv(paste(clean,"EconomicSector.final.csv",sep="/"))
#COn el archivo descargado
class(EconomicSector$Date)
EconomicSector$Date <- as.Date(EconomicSector$Date, format = "%Y-%m-%d")
class(EconomicSector$Agricultural)
cols = c(3:20)
EconomicSector[, cols] = apply(EconomicSector[, cols], 2, function(x)
  as.numeric(as.character(x)))

####### Graphics------------------------------
#*******INDUSTRY-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iIndustry18")
iIndustryi <- EconomicSector[keeps1]
#Long data
iIndustryi <- iIndustryi %>% gather(EconomicSector, Values, iIndustry18)
iIndustryi <- iIndustryi %>% filter(!is.na(Values))
giIiIndustry.r <- ggplot(data = iIndustryi, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iIndustryi$Country)))+
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
  ggtitle("Industry: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiIndustry.r <- giIiIndustry.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiIndustry.r <- giIiIndustry.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiIndustry.r
#Reales--------------
keeps1 <- c("Date", "Country", "iIndustry18ipc")
iIndustry18ipci <- EconomicSector[keeps1]
#Long data
iIndustry18ipci <- iIndustry18ipci %>% gather(EconomicSector, Values, iIndustry18ipc)
iIndustry18ipci <- iIndustry18ipci %>% filter(!is.na(Values))
giIiIndustry18ipc <- ggplot(data = iIndustry18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iIndustry18ipci$Country)))+
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
  ggtitle("Industry: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiIndustry18ipc <- giIiIndustry18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiIndustry18ipc <- giIiIndustry18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiIndustry18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiIndustry.r, giIiIndustry18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-industry.png",width=9, height=5)

#*******Agricultural-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iAgricultural18")
iAgriculturali <- EconomicSector[keeps1]
#Long data
iAgriculturali <- iAgriculturali %>% gather(EconomicSector, Values, iAgricultural18)
iAgriculturali <- iAgriculturali %>% filter(!is.na(Values))
giIiAgricultural.r <- ggplot(data = iAgriculturali, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iAgriculturali$Country)))+
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
  ggtitle("Agricultural: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiAgricultural.r <- giIiAgricultural.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiAgricultural.r <- giIiAgricultural.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiAgricultural.r
#Reales--------------
keeps1 <- c("Date", "Country", "iAgricultural18ipc")
iAgricultural18ipci <- EconomicSector[keeps1]
#Long data
iAgricultural18ipci <- iAgricultural18ipci %>% gather(EconomicSector, Values, iAgricultural18ipc)
iAgricultural18ipci <- iAgricultural18ipci %>% filter(!is.na(Values))
giIiAgricultural18ipc <- ggplot(data = iAgricultural18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iAgricultural18ipci$Country)))+
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
  ggtitle("Agricultural: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiAgricultural18ipc <- giIiAgricultural18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiAgricultural18ipc <- giIiAgricultural18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiAgricultural18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiAgricultural.r, giIiAgricultural18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-Agricultural.png",width=9, height=5)

#*******COMMERCE-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iCommerce18")
iCommercei <- EconomicSector[keeps1]
#Long data
iCommercei <- iCommercei %>% gather(EconomicSector, Values, iCommerce18)
iCommercei <- iCommercei %>% filter(!is.na(Values))
giIiCommerce.r <- ggplot(data = iCommercei, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iCommercei$Country)))+
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
  ggtitle("Commerce: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiCommerce.r <- giIiCommerce.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiCommerce.r <- giIiCommerce.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiCommerce.r
#Reales--------------
keeps1 <- c("Date", "Country", "iCommerce18ipc")
iCommerce18ipci <- EconomicSector[keeps1]
#Long data
iCommerce18ipci <- iCommerce18ipci %>% gather(EconomicSector, Values, iCommerce18ipc)
iCommerce18ipci <- iCommerce18ipci %>% filter(!is.na(Values))
giIiCommerce18ipc <- ggplot(data = iCommerce18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iCommerce18ipci$Country)))+
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
  ggtitle("Commerce: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiCommerce18ipc <- giIiCommerce18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiCommerce18ipc <- giIiCommerce18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiCommerce18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiCommerce.r, giIiCommerce18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-commerce.png",width=9, height=5)

#*******T.productive-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iT.productive18")
iT.productivei <- EconomicSector[keeps1]
#Long data
iT.productivei <- iT.productivei %>% gather(EconomicSector, Values, iT.productive18)
iT.productivei <- iT.productivei %>% filter(!is.na(Values))
giIiT.productive.r <- ggplot(data = iT.productivei, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iT.productivei$Country)))+
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
  ggtitle("T.productive: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiT.productive.r <- giIiT.productive.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiT.productive.r <- giIiT.productive.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiT.productive.r
#Reales--------------
keeps1 <- c("Date", "Country", "iT.productive18ipc")
iT.productive18ipci <- EconomicSector[keeps1]
#Long data
iT.productive18ipci <- iT.productive18ipci %>% gather(EconomicSector, Values, iT.productive18ipc)
iT.productive18ipci <- iT.productive18ipci %>% filter(!is.na(Values))
giIiT.productive18ipc <- ggplot(data = iT.productive18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iT.productive18ipci$Country)))+
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
  ggtitle("T.productive: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiT.productive18ipc <- giIiT.productive18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiT.productive18ipc <- giIiT.productive18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiT.productive18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiT.productive.r, giIiT.productive18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-T.productive.png",width=9, height=5)


#*******Remaining-------------------------------------------
#Nominales--------------
keeps1 <- c("Date", "Country", "iRemaining18")
iT.crediti <- EconomicSector[keeps1]
#Long data
iT.crediti <- iT.crediti %>% gather(EconomicSector, Values, iRemaining18)
iT.crediti <- iT.crediti %>% filter(!is.na(Values))
giIiT.credit.r <- ggplot(data = iT.crediti, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iT.crediti$Country)))+
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
  ggtitle("Remaining sectors: INDEX-nominales") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiT.credit.r <- giIiT.credit.r+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiT.credit.r <- giIiT.credit.r+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiT.credit.r
#Reales--------------
keeps1 <- c("Date", "Country", "iRemaining18ipc")
iRemaining18ipci <- EconomicSector[keeps1]
#Long data
iRemaining18ipci <- iRemaining18ipci %>% gather(EconomicSector, Values, iRemaining18ipc)
iRemaining18ipci <- iRemaining18ipci %>% filter(!is.na(Values))
giIiRemaining18ipc <- ggplot(data = iRemaining18ipci, aes(x=Date, y=Values)) + 
  geom_line(aes(colour=Country))+
  geom_point(aes(color = Country, shape = Country)) + scale_shape_manual(values = 0:length(unique(iRemaining18ipci$Country)))+
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
  ggtitle("Remaining sectors: INDEX-deflactado") +
  xlab("Date") + ylab("")
#giICommercialCredit <- giICommercialCredit+scale_y_continuous(breaks = seq(0.75, 2.5, by = 0.25),limits = c(0.75,2.5))
giIiRemaining18ipc <- giIiRemaining18ipc+theme(axis.text.x = element_text(angle = 60, hjust = 1))
giIiRemaining18ipc <- giIiRemaining18ipc+theme(legend.key.size = unit(0.9, "lines"))
#giICommercialCredit <- giICommercialCredit+labs(colour = "Credit Type")
giIiRemaining18ipc
#Arrange-------------
library(ggpubr)
ggarrange(giIiT.credit.r, giIiRemaining18ipc,
          common.legend = TRUE,
          #labels = c("A", "B"),
          ncol = 2)
ggsave(path = figures, filename = "d-Remaining.png",width=9, height=5)

