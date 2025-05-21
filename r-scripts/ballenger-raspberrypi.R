#title: "HL ProbeTemps Full Data"
#author: "JG Ballenger"
#date: "2023-09-22"

library(readxl)
library(gt)
library(Rmisc)
library(drc)
library(emmeans)
library(cowplot)
library(lmerTest)
library(multcomp)
library(multcompView)
library(lme4)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(lubridate)
library(nlme)

#High/Low Experiment, Initial Run on 6-22-2023

setwd("~/Documents/github/ansible-raspberry-pi-paper/r-scripts/data/")


Modded.dat <- read.csv("Modified_Probes_Full.csv") %>%
  mutate(datetime = as.factor(datetime),
         ip=as.factor(ip),
         position=as.factor(position),
         lights=as.factor(lights),
         probe=as.factor(probe_id),
         temp_regime=as.factor(temp_regime),
         temperature.usb=as.numeric(temperature.usb),
         temperature.probe=as.numeric(temperature.probe),
         switch=as.factor(switch))
Modded.dat$datetime<-as.POSIXct(Modded.dat$datetime, format = "%Y-%m-%d-%H-%M")
Modded.dat$SAP <- as.integer(Modded.dat$datetime - Modded.dat$datetime[1])
Modded.dat$MAP<-as.integer(Modded.dat$SAP/60)

print(Modded.dat$datetime[1])

Modified_Low<-Modded.dat[is.element(Modded.dat$switch, c('Modified')),]
Modified_Low<-Modified_Low[is.element(Modified_Low$temp_regime, c('low')),]
Modified_LowLOn<-Modified_Low[is.element(Modified_Low$lights, c('Lights On')),]

TempModifiedLowLOn<-ggplot(Modified_LowLOn,                            
                           aes(x = datetime,
                               y = temperature.probe,
                               col = position)) +
  ggtitle("Temperature over time, modified positions lights on")+
  labs(y= "Area (Pixels)", x = "Hours After Planting")+
  scale_x_continuous(expand=c(0,0))+
  geom_line()+
  geom_point()

TempModifiedLowLOn


Modified_LowLOff<-Modified_Low[is.element(Modified_Low$lights, c('Lights Off')),]

TempModifiedLowLOff<-ggplot(Modified_LowLOff,                            
                            aes(x = datetime,
                                y = temperature.probe,
                                col = position)) +
  ggtitle("Temperature over time, modified positions, lights off")+
  labs(y= "Area (Pixels)", x = "Hours After Planting")+
  scale_x_continuous(expand=c(0,0))+
  geom_line()+
  geom_point()

TempModifiedLowLOff

Original_Low<-Modded.dat[is.element(Modded.dat$switch, c('Original')),]
Original_Low<-Original_Low[is.element(Original_Low$temp_regime, c('low')),]
Original_LowLOn<-Original_Low[is.element(Original_Low$lights, c('Lights On')),]

TempOriginalLowLOn<-ggplot(Original_LowLOn,                            
                           aes(x = datetime,
                               y = temperature.probe,
                               col = position)) +
  ggtitle("Temperature over time, original positions lights on")+
  labs(y= "Area (Pixels)", x = "Hours After Planting")+
  scale_x_continuous(expand=c(0,0))+
  geom_line()+
  geom_point()

TempOriginalLowLOn

Original_LowLOff<-Original_Low[is.element(Original_Low$lights, c('Lights Off')),]

TempOriginalLowLOff<-ggplot(Original_LowLOff,                            
                            aes(x = datetime,
                                y = temperature.probe,
                                col = position)) +
  ggtitle("Temperature over time, Original positions, lights off")+
  labs(y= "Area (Pixels)", x = "Hours After Planting")+
  scale_x_continuous(expand=c(0,0))+
  geom_line()+
  geom_point()

TempOriginalLowLOff

Malia_Temps<-plot_grid(TempOriginalLowLOn,TempOriginalLowLOff,TempModifiedLowLOn,TempModifiedLowLOff,nrow = 2)


ModdedLowLightsOn.dat<-Modded.dat%>%
  filter(lights!="Lights Off")%>%
  filter(temp_regime!="high")


MLLOn.mean<-summarySE(ModdedLowLightsOn.dat,measurevar = "temperature.probe",
                      groupvars = c("probe_id","switch"),
                      na.rm = T)

ModdedLowLightsOff.dat<-Modded.dat%>%
  filter(lights!="Lights On")%>%
  filter(temp_regime!="high")

MLLOff.mean<-summarySE(ModdedLowLightsOff.dat,measurevar = "temperature.probe",
                       groupvars = c("probe_id","switch"),
                       na.rm = T)


ModPlotLowOnPID<-ggplot(ModdedLowLightsOn.dat, aes(x=probe_id, y=temperature.probe, fill=switch))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.025, alpha=0.5, show.legend=F)+
  theme_minimal()+
  #facet_wrap()+
  labs(y= "Temperature (°C)", x = "Probe ID")+
  guides(fill=guide_legend(title="Probe"))+
  geom_hline(yintercept = 4, color="blue")+ 
  annotate("text", x=3, y=3, label="", size=5, color="black")+
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))+
  theme(plot.background = element_rect(fill = "white"))+ 
  ylim(0,6)

ModPlotLowOnPID

ModPlotLowOffPID<-ggplot(ModdedLowLightsOff.dat, aes(x=probe_id, y=temperature.probe, fill=switch))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.025, alpha=0.5, show.legend=F)+
  theme_minimal()+
  #facet_wrap()+
  labs(y= "Temperature (°C)", x = "Probe ID")+
  guides(fill=guide_legend(title="Probe"))+
  geom_hline(yintercept = 4, color="blue")+ 
  annotate("text", x=3, y=3, label="", size=5, color="black")+
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))+
  theme(plot.background = element_rect(fill = "white"))+ 
  ylim(0,6)

ModPlotLowOffPID

adjtst <- lm(temperature.probe ~ Before+After, ModdedLowLightsOff.dat)

test <- multcomp::glht(adjtst, linfct = multcomp::mcp(Before+After = "Tukey"))
hnzcld<-multcomp::cld(test)
print(hnzcld)
summary(hnzcld)

TempAOV <- lm(temperature.probe ~ Before+After, data = ModdedLowLightsOff.dat)
model1<-anova(TempAOV)
model1<-aov(temperature.probe~Before+After, data = ModdedLowLightsOff.dat)
multcomp::cld(emmeans(model1, ~temperature.probe))

TukeyHSD(model1, conf.level=.95)

#At 30°C

ModdedHighLightsOn.dat<-Modded.dat%>%
  filter(lights!="Lights Off")%>%
  filter(temp_regime!="low")

MHLOn.mean<-summarySE(ModdedHighLightsOn.dat,measurevar = "temperature.probe",
                      groupvars = c("probe_id","switch"),
                      na.rm = T)

ModdedHighLightsOff.dat<-Modded.dat%>%
  filter(lights!="Lights On")%>%
  filter(temp_regime!="low")

MHLOff.mean<-summarySE(ModdedHighLightsOff.dat,measurevar = "temperature.probe",
                       groupvars = c("probe_id","switch"),
                       na.rm = T)


ModPlotHighOnPID<-ggplot(ModdedHighLightsOn.dat, aes(x=probe_id, y=temperature.probe, fill=switch))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.025, alpha=0.5, show.legend=F)+
  theme_minimal()+
  #facet_wrap()+
  labs(y= "Temperature (°C)", x = "Probe ID")+
  guides(fill=guide_legend(title="Probe"))+
  geom_hline(yintercept = 30, color="red")+ 
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))+
  theme(plot.background = element_rect(fill = "white"))+
  ylim(25,35)

ModPlotHighOnPID

ModPlotHighOffPID<-ggplot(ModdedHighLightsOff.dat, aes(x=probe_id, y=temperature.probe, fill=switch))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.025, alpha=0.5, show.legend=F)+
  theme_minimal()+
  #facet_wrap()+
  labs(y= "Temperature (°C)", x = "Probe ID")+
  guides(fill=guide_legend(title="Probe"))+
  geom_hline(yintercept = 30, color="red")+ 
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0))+
  theme(plot.background = element_rect(fill = "white"))+ 
  ylim(25,35)

ModPlotHighOffPID



TempAOV <- lm(temperature.probe ~ Before+After, data = Modded.dat)
model1<-anova(TempAOV)
model1<-aov(temperature.probe~Before+After, data = Modded.dat)

TukeyHSD(model1, conf.level=.95)

ModIDPlot_PID<-plot_grid(ModPlotLowOnPID, ModPlotLowOffPID, ModPlotHighOnPID, ModPlotHighOffPID, labels = c('Lights On, 4°C', 'Lights Off, 4°C', 'Lights On, 30°C', 'Lights Off, 30°C'))

ModIDPlot_PID

ggsave("TemperModPositionPlot_Position_ProbeID.png", ModIDPlot_PID,
       width=12, height=8, units="in", dpi=600)