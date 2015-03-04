PlotsTrialID <- idPeak(dframe=PlotsTrial,tempvar=PlotsTrial$iButton,time=PlotsTrial$Time,PlotID=PlotsTrial$PlotID)
FieldTrialID <- idPeak(dframe=FIRETEMP_iButton_Temperatures,tempvar=FIRETEMP_iButton_Temperatures$Temperature,time=FIRETEMP_iButton_Temperatures$Time,PlotID=FIRETEMP_iButton_Temperatures$UniqueID)

FieldTrialID_TemporalSubset <- ddply(FieldTrialID,.(PlotID),subset, timestamp < (t + minutes(30)))
FieldTrialID_TemporalSubset <- ddply(FieldTrialID_TemporalSubset,.(PlotID),subset, timestamp > (t - minutes(10)))

PlotsTrialID_TemporalSubset <- ddply(PlotsTrialID,.(PlotID),subset, timestamp < (t + minutes(30)))
PlotsTrialID_TemporalSubset <- ddply(PlotsTrialID_TemporalSubset,.(PlotID),subset, timestamp > (t - minutes(10)))
PlotsTrialID$t <- PlotsTrialID$t - hours(2)

PlotsTrial_subset <- ddply(.data=PlotsTrialID_TemporalSubset,.(PlotID),subset, Temp >= quantile(Temp,0.10))
PlotsField_subset <- ddply(.data=FieldTrialID_TemporalSubset,.(PlotID),subset, Temp >= quantile(Temp,0.10))
colnames(PlotsField_subset)[1] <- "PlotID"

PlotsField_subset$Type <- "Field"
PlotsTrial_subset$Type <- "Lab"

PlotsMerge <- rbind.fill(PlotsTrial_subset,PlotsField_subset)

PlotsMerge <- ddply(PlotsMerge,
                  .(PlotID),mutate,
                  TimeRange = max(as.numeric(as.POSIXct(timestamp, tz="UTC"),tz="UTC")) - min(as.numeric(as.POSIXct(timestamp, tz="UTC"),tz="UTC")))

PlotsMergeAgg <- aggregate(TimeRange ~ PlotID + Type, PlotsMerge, FUN="mean")



comparePlot <- ggplot(PlotsMergeAgg, aes(y=TimeRange/60, x=Type, fill=Type))
comparePlot+
  geom_boxplot()+
  xlab("\nData Type\n")+
  ylab("\nTime Range (minutes)\n")+
  coord_flip()+
  myTheme


#
#
#

PlotTrialsRange <- aggregate(data=subset(PlotsMerge,Type=="Lab"),TimeRange ~ PlotID,FUN="mean")
PlotTrialsMaxThermo <- aggregate(data=PlotsTrial,Thermo ~ PlotID,FUN="max")
PlotTrialsMaxiButton <- aggregate(data=PlotsTrial,iButton ~ PlotID,FUN="max")
PlotTrialsSumiButton <- aggregate(data=subset(PlotsMerge,Type=="Lab"),Temp ~ PlotID,FUN="sum")

CompareTrial <- merge(PlotTrialsRange,PlotTrialsMaxThermo,by="PlotID")
CompareTrial <- merge(CompareTrial,PlotTrialsMaxiButton,by="PlotID")
CompareTrial <- merge(CompareTrial,PlotTrialsSumiButton,by="PlotID")


baseOne <- lm(Thermo ~ iButton, data=CompareTrial)
baseDur <- lm(Thermo ~ TimeRange, data=CompareTrial)
baseBoth <- lm(Thermo ~ iButton + TimeRange, data=CompareTrial)
baseBothInter <- lm(Thermo ~ iButton * TimeRange, data=CompareTrial)
baseMax <- lm(Thermo ~ Temp, data=CompareTrial)
