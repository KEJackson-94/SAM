library(data.table) # melt()
library(ggplot2) # ggplot()
library(countrycode)
library(tidyr) # complete()
library(zoo) # na.approx()
library(dplyr)
library(imputeTS)

path_ref_raw<-'//data_reference//data_raw//'
path_ref_score<-'//data_reference//data_score//'

##### 2021 Edition ################################################################# 
path_raw_2021<-'//data_raw//2021_ed//'
path_trans_2021<-'//data_trans//2021_ed//'
path_score_2021<-'//data_score//2021_ed//'
path_product_2021<-'//data_products//2021_ed//'

#### Social INDICATORS #########################################################
path_Soc_2021<-'//data_archive//2021_ed/Soc//'

### Crop Diversity (H Index) --------------------------------------------------

### Food Affordability (RSE) --------------------------------------------------

### Under-nourishment (UDN) ---------------------------------------------------

### Gender Gap Score (GGG) ----------------------------------------------------

### Rural Poverty (RPV) -------------------------------------------------------

### Land Rights (LRS) ---------------------------------------------------------

#### Economic INDICATORS #######################################################
path_econ_2021 <- '//data_archive//2021_ed/Econ//'

### Labor Productivity (AGDP) -------------------------------------------------
AGDP<-read.csv(paste0(getwd(),path_econ_2021,'AgValueAdded_Worker_WDI_20180918.csv')) # Agriculture forestry and fishing value added per worker (constant 2010 US$)
AGDP_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Labor_Productivity.csv'))

AGDP<-na.omit(melt(setDT(AGDP),id.vars=c("Country.Name","Country.Code","Series.Name","Series.Code"),variable.name="year"))
AGDP$year<-substr(AGDP$year,2,length(AGDP$year))
setnames(AGDP,old=colnames(AGDP),new=c("Country_Name","ISO","Series.Name","Series.Code","year","value"))
AGDP<-subset(AGDP,select=c(ISO,year,value))
AGDP_raw<-AGDP[which(AGDP$ISO=='TUR'),] # raw values for Turkey (TUR)
AGDP_raw$type<-'raw'

AGDP_ref<-AGDP_ref[which(AGDP_ref$ISO=='TUR'),] # ref values for Turkey (TUR)
AGDP_ref<-na.omit(melt(setDT(AGDP_ref),id.vars=c(),variable.name="year"))
AGDP_ref$year<-substr(AGDP_ref$year,2,length(AGDP_ref$year))
AGDP_ref$type<-'ref'
AGDP_ref<-subset(AGDP_ref,select=-c(Country_Name))

AGDP_comp<-rbind(AGDP_raw,AGDP_ref) # compare ref and raw values for Turkey (TUR)
AGDP_comp$year <- as.integer(AGDP_comp$year)
pp<-ggplot(AGDP_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="AGDP") + theme_classic()
ggsave('C://SAM_tst//AGDP.png', pp)
dev.off()

AGDP$Indicator <- 'AGDP'
AGDP<-reshape(AGDP,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
#write.csv(AGDP,paste0(getwd(),path_raw_2021,'ADGP_raw.csv'))

### Finance Access (A2F) ------------------------------------------------------

### Price Volatility (PVOL) ---------------------------------------------------

### Government Support (AEXP) -------------------------------------------------
AEXP_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Government_Support.csv'))

# calculate Ag population using Ag Value added per worker (WDI), and Ag GDP data
AGDP_capita<-read.csv(paste0(getwd(),path_econ_2021,'AgValueAdded_Worker_WDI_20180918.csv')) # Agriculture forestry and fishing value added per worker (constant 2010 US$)
AGDP_capita<-na.omit(melt(setDT(AGDP_capita),id.vars=c("Country.Name","Country.Code","Series.Name","Series.Code"),variable.name="year"))
AGDP_capita$year<-as.integer(substr(AGDP_capita$year,2,length(AGDP_capita$year)))
setnames(AGDP_capita,old=colnames(AGDP_capita),new=c("Country_Name","ISO","Series.name","Series.code",'year',"AGDP_capita"))
AGDP_capita<-subset(AGDP_capita,select=c(ISO,year,AGDP_capita))
AGDP<-read.csv(paste0(getwd(),path_econ_2021,'Ag_GDP_2010USD_WDI_08192019.csv')) # Ag GDP
AGDP<-na.omit(melt(setDT(AGDP),id.vars=c("Country.Name","Country.Code","Series.Name","Series.Code"),variable.name="year"))
AGDP$year<-as.integer(substr(AGDP$year,2,length(AGDP$year)))
setnames(AGDP,old=colnames(AGDP),new=c("Country_Name","ISO","Series.name","Series.code",'year',"AGDP"))
AGDP<-subset(AGDP,select=c(ISO,year,AGDP))

Ag_Pop<-merge(AGDP, AGDP_capita, by=c('ISO','year'),all.x=TRUE)
Ag_Pop$Ag_Pop <- Ag_Pop$AGDP/Ag_Pop$AGDP_capita

Ag_Exp<-read.csv(paste0(getwd(),path_econ_2021,'Ag_Exp_IFPRI_1980_2012.csv')) # try dividing by number of ag workers
Ag_Exp<-na.omit(melt(setDT(Ag_Exp),id.vars=c("country","ISO","FAOCODE","Unit"),variable.name="year"))
Ag_Exp$year<-as.integer(substr(Ag_Exp$year,2,length(Ag_Exp$year)))
setnames(Ag_Exp,old=colnames(Ag_Exp),new=c("Country_Name","ISO","FAOCODE","unit",'year',"Ag_Exp"))
Ag_Exp<-subset(Ag_Exp,select=c(ISO,year,Ag_Exp))
AEXP <- merge(Ag_Pop, Ag_Exp, by=c('ISO','year'),all.x=TRUE)
AEXP$value <- AEXP$Ag_Exp*1E9/AEXP$Ag_Pop
AEXP<-subset(AEXP,select=c(ISO,year,value))

AEXP_raw<-AEXP[which(AEXP$ISO=='TUR'),] # raw values for Turkey (TUR)
AEXP_raw$type<-'raw'

AEXP_ref<-AEXP_ref[which(AEXP_ref$ISO=='TUR'),] # ref values for Turkey (TUR)
AEXP_ref<-na.omit(melt(setDT(AEXP_ref),id.vars=c(),variable.name="year"))
AEXP_ref$year<-as.integer(substr(AEXP_ref$year,2,length(AEXP_ref$year)))
AEXP_ref$type<-'ref'
AEXP_ref<-subset(AEXP_ref,select=-c(Country_Name))

AEXP_comp<-rbind(AEXP_raw,AEXP_ref) # compare ref and raw values for Turkey (TUR)
pp<-ggplot(AEXP_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="AEXP") + theme_classic()
ggsave('C://SAM_tst//AEXP.png', pp)
dev.off()

AEXP$Indicator <- 'AEXP'
AEXP<-reshape(AEXP,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
#write.csv(AEXP,paste0(getwd(),path_raw_2021,'AEXP_raw.csv'))

### Trade Openness (TROP) -----------------------------------------------------

### Food Loss (FLP) -----------------------------------------------------------
FLP<-read.csv(paste0(getwd(),path_econ_2021,'Food_Loss_EIU_09182018.csv'))
FLP_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Food_Loss.csv'))

FLP<-na.omit(melt(setDT(FLP),id.vars=c("Country","FAOPos","FAOCODE"),variable.name="year"))
FLP$year<-as.integer(substr(FLP$year,2,length(FLP$year)))
setnames(FLP,old=colnames(FLP),new=c("Country_Name","FAOPos","ISO","year","value"))
FLP<-subset(FLP,select=c(ISO,year,value))
FLP_raw<-FLP[which(FLP$ISO=='TUR'),] # raw values for Turkey (TUR)
FLP_raw$type<-'raw'

FLP_ref<-FLP_ref[which(FLP_ref$ISO=='TUR'),] # ref values for Turkey (TUR)
FLP_ref<-na.omit(melt(setDT(FLP_ref),id.vars=c(),variable.name="year"))
FLP_ref$year<-as.integer(substr(FLP_ref$year,2,length(FLP_ref$year)))
FLP_ref$type<-'ref'
FLP_ref<-subset(FLP_ref,select=-c(Country_Name))

FLP_comp<-rbind(FLP_raw,FLP_ref) # compare ref and raw values for Turkey (TUR)
pp<-ggplot(FLP_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="FLP") + theme_classic()
ggsave('C://SAM_tst//FLP.png', pp)
dev.off()

FLP$Indicator <- 'FLP'
FLP<-reshape(FLP,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
#write.csv(FLP,paste0(getwd(),path_raw_2021,'FLP_raw.csv'))

#### Environmental INDICATORS ##################################################
path_Env_2021 <- '//data_archive//2021_ed/Env//'

### Water Consumption (SUSI) --------------------------------------------------
SUSI_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Water_Consumption.csv'))

TS_km3<-read.csv(paste0(getwd(),path_Env_2021,'TS_km3.csv')) #	TS_km3: water considered sustainable for agricultural use for year 2000 
setnames(TS_km3,old=colnames(TS_km3),new=c("Country.Name","TS"))
TI_km3_2015<-read.csv(paste0(getwd(),path_Env_2021,'TI_km3_2015.csv'))# TOTAL Irrigation for all 26 CROPS for year 2015
TI_km3_2015$year<-2015
setnames(TI_km3_2015,old=colnames(TI_km3_2015),new=c("Country.Name","TI","year"))
TI_km3_2000<-read.csv(paste0(getwd(),path_Env_2021,'TI_km3_2000.csv')) # TOTAL Irrigation for all 26 CROPS for year 2000
TI_km3_2000$year<-2000
setnames(TI_km3_2000,old=colnames(TI_km3_2000),new=c("Country.Name","TI","year"))
TI<-rbind(TI_km3_2000,TI_km3_2015)

AEI<-read.csv(paste0(getwd(),path_Env_2021,'FAOSTAT_data_9-10-2019_AEI.csv'))# FAOSTAT_data_9-10-2019_AEI: Area equipped for irrigation. 
AEI<-subset(AEI,select=c("Area","Year","Unit","Value"))
setnames(AEI,old=colnames(AEI),new=c("Country.Name","year","unit","AEI"))
AEI$AEI<-AEI$AEI*1000
AEI<-subset(AEI,select=-c(unit))
AEI<-AEI[which(AEI$year>1989&AEI$year<2017),]

SUSI<-merge(AEI,TI,by=c('Country.Name','year'),all.x=TRUE) # MERGE TI and AEI
SUSI$TIR<-SUSI$TI/SUSI$AEI# Compute TIR for 2000 and 2015 (when available)

# First create long format df for yrs '90-'16 in prep for spline
ISO<-unique(SUSI$Country.Name)
datalist = list()
for(i in 1:length(ISO)){
  m<-data.frame(matrix(0,ncol=2,nrow=27))
  colnames(m)<-c('Country.Name','year')
  m$Country.Name<-ISO[i]
  m$year<-1990:2016
  datalist[[i]]<-m
}
ISO_df = do.call(rbind, datalist)
SUSI<-merge(ISO_df,SUSI,by=c('Country.Name','year'), all.x=TRUE) # merge long df with SUSI

# for each iso, set pre-2000 TIR to TIR(2000), post-2015 TIR to TIR (2015) and linearly interpolate b/w 2000 and 2015
ISO<-unique(SUSI$Country.Name)
datalist2 = list()
for(i in 1:length(ISO)){
  print(ISO[i])
  df<-SUSI[which(SUSI$Country.Name==ISO[i]),]
  if (!is.na(df[which(df$year==2000),]$TIR) & !is.na(df[which(df$year==2015),]$TIR)){
    df[df$year<2000,]$TIR <- df[which(df$year==2000),]$TIR
    df[df$year>2015,]$TIR <- df[which(df$year==2015),]$TIR
    df$TIR_new<-na.spline(df$TIR)
  }
  else {
    df$TIR_new<-NA
  }
  datalist2[[i]]<-df
}
SUSI = do.call(rbind, datalist2) # SUSI now has estimate TIR and AEI for each iso and  year


SUSI<-merge(SUSI,TS_km3,by=c('Country.Name')) # Relate TS(2000) for each iso and year
SUSI$TI_new<-SUSI$TIR_new*SUSI$AEI # calculate TI for each iso and year
SUSI$value<-SUSI$TI_new/SUSI$TS # compute SUSI for each iso and year
SUSI$ISO<-countrycode(SUSI$Country.Name, origin='country.name',destination='iso3c',warn =TRUE,nomatch =NA)

SUSI<-subset(SUSI,select=c(ISO,year,value))
SUSI_raw<-SUSI[which(SUSI$ISO=='TUR'),] # raw values for Turkey (TUR)
SUSI_raw$type<-'raw'

SUSI_ref<-SUSI_ref[which(SUSI_ref$ISO=='TUR'),] # ref values for Turkey (TUR)
SUSI_ref<-na.omit(melt(setDT(SUSI_ref),id.vars=c(),variable.name="year"))
SUSI_ref$year<-as.integer(substr(SUSI_ref$year,2,length(SUSI_ref$year)))
SUSI_ref$type<-'ref'
SUSI_ref<-subset(SUSI_ref,select=-c(Country_Name))

SUSI_comp<-rbind(SUSI_raw,SUSI_ref) # compare ref and raw values for Turkey (TUR)
pp<-ggplot(SUSI_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="SUSI") + theme_classic()
ggsave('C://SAM_tst//SUSI.png', pp)
dev.off()

SUSI$Indicator <- 'SUSI'
SUSI<-reshape(SUSI,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
#write.csv(SUSI,paste0(getwd(),path_raw_2021,'SUSI_raw.csv'))

### N Surplus (Nsur) ----------------------------------------------------------

### P Surplus (Psur) ----------------------------------------------------------

### Land Use Change (LUC) -----------------------------------------------------
LCC_archive<-read.csv(paste0(getwd(),path_Env_2021,'LCC.csv'))
LCC_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Land_Use_Change.csv'))

LCC_CD<-LCC_archive[which(LCC_archive$Drivers=="Commodity-Driven Deforestation"),]
LCC_CD<-na.omit(melt(setDT(LCC_CD),id.vars=c("ISO","Drivers"),variable.name="year"))
LCC_CD$year<-as.integer(substr(LCC_CD$year,2,length(LCC_CD$year)))
setnames(LCC_CD,old=colnames(LCC_CD),new=c("ISO","Drivers","year","CD"))
LCC_CD<-subset(LCC_CD,select=c(ISO,year,CD))

LCC_SA<-LCC_archive[which(LCC_archive$Drivers=="Shifting Agriculture"),]
LCC_SA<-na.omit(melt(setDT(LCC_SA),id.vars=c("ISO","Drivers"),variable.name="year"))
LCC_SA$year<-as.integer(substr(LCC_SA$year,2,length(LCC_SA$year)))
setnames(LCC_SA,old=colnames(LCC_SA),new=c("ISO","Drivers","year","SA"))
LCC_SA<-subset(LCC_SA,select=c(ISO,year,SA))
LCC_SA_CD<-merge(LCC_SA,LCC_CD,by=c('ISO','year'),all=TRUE)

AEI<-read.csv(paste0(getwd(),path_Env_2021,'FAOSTAT_data_9-10-2019_AEI.csv'))# FAOSTAT_data_9-10-2019_AEI: Area equipped for irrigation. 
AEI<-subset(AEI,select=c("Area","Year","Unit","Value"))
setnames(AEI,old=colnames(AEI),new=c("Country_Name","year","unit","AEI"))
AEI$ISO<-countrycode(AEI$Country_Name, origin='country.name',destination='iso3c',warn =TRUE,nomatch =NA)
AEI<-subset(AEI,select=c(ISO,year,AEI))
AEI$AEI<-AEI$AEI*1000 # units are in 1000ha so convert to ha

LCC<-merge(x=AEI,y=LCC_SA_CD,by=c('ISO','year'),all = TRUE)
LCC$value<-rowSums(LCC[,c("SA", "CD")], na.rm=TRUE)/LCC$AEI
LCC<-subset(LCC,select=c(ISO,year,value))

LCC_raw<-LCC[which(LCC$ISO=='TUR'),]
LCC_raw$type<-'raw'
LCC_ref<-LCC_ref[which(LCC_ref$ISO=='TUR'),] # ref values for Turkey (TUR)
LCC_ref<-na.omit(melt(setDT(LCC_ref),id.vars=c(),variable.name="year"))
LCC_ref$year<-as.integer(substr(LCC_ref$year,2,length(LCC_ref$year)))
LCC_ref$type<-'ref'
LCC_ref<-subset(LCC_ref,select=-c(Country_Name))

LCC_comp<-rbind(LCC_raw,LCC_ref) # compare ref and raw values for Turkey (TUR)
LCC_comp$value<-replace(LCC_comp$value,LCC_comp$value==0,NA)
pp<-ggplot(LCC_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="LCC") + theme_classic()
ggsave('C://SAM_tst//LCC.png', pp)
dev.off()

##### 2023 Edition  ################################################################
path_raw_2023 <- '//data_raw//2023_ed//'
path_trans_2023 <- '//data_trans//2023_ed//'
path_score_2023 <- '//data_score//2023_ed//'

#### Social INDICATORS #########################################################

### Crop Diversity (H Index) --------------------------------------------------

### Food Affordability (RSE) --------------------------------------------------

### Under-nourishment (UDN) ---------------------------------------------------

### Gender Gap Score (GGG) ----------------------------------------------------

### Rural Poverty (RPV) -------------------------------------------------------

### Land Rights (LRS) ---------------------------------------------------------

#### Economic INDICATORS #######################################################

### Labor Productivity (AGDP) -------------------------------------------------

### Finance Access (A2F) ------------------------------------------------------

### Price Volatility (PVOL) ---------------------------------------------------

### Government Support (AEXP) -------------------------------------------------

### Trade Openness (TROP) -----------------------------------------------------

### Food Loss (FLP) -----------------------------------------------------------

#### Environmental INDICATORS ##################################################

### Water Consumption (SUSI) --------------------------------------------------

### N Surplus (Nsur) ----------------------------------------------------------

### P Surplus (Psur) ----------------------------------------------------------

### Land Use Change (LUC) -----------------------------------------------------

### Greenhouse Gas Emmissions (GHG) -------------------------------------------

### Soil Erosion (SER) --------------------------------------------------------
