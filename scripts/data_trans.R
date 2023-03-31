library(data.table) # melt()
library(ggplot2) # ggplot()

path_ref_raw <- '//data_reference//data_raw//'
path_ref_score <- '//data_reference//data_score//'

##### 2021 Edition ################################################################# 
path_raw_2021 <- '//data_raw//2021_ed//'
path_trans_2021 <- '//data_trans//2021_ed//'
path_score_2021 <- '//data_score//2021_ed//'

#### Social INDICATORS #########################################################
path_Soc_2021 <- '//data_archive//2021_ed/Soc//'

### Crop Diversity (H Index) --------------------------------------------------

### Food Affordability (RSE) --------------------------------------------------

### Under-nourishment (UDN) ---------------------------------------------------

### Gender Gap Score (GGG) ----------------------------------------------------

### Rural Poverty (RPV) -------------------------------------------------------

### Land Rights (LRS) ---------------------------------------------------------

#### Economic INDICATORS #######################################################
path_econ_2021 <- '//data_archive//2021_ed/Econ//'
PPP <- read.csv(paste0(getwd(),path_econ_2021,'Macro_GDP_PPP_Exchange_WB.csv'))
NY.GDP.PCAP.PP.KD<-PPP[which(PPP$Series.Code=='NY.GDP.PCAP.PP.KD' & PPP$Country.Code=='TUR'),] # GDP per capita  PPP (constant 2011 international $)
NY.GDP.PCAP.PP.KD<-na.omit(melt(setDT(NY.GDP.PCAP.PP.KD),id.vars=c(),variable.name="year"))
NY.GDP.PCAP.PP.KD$year<-as.integer(substr(NY.GDP.PCAP.PP.KD$year,2,length(NY.GDP.PCAP.PP.KD$year)))

NY.GDP.MKTP.PP.KD<-read.csv(paste0(getwd(),path_econ_2021,'GDP_2011PPP_WDI_10012018.csv')) # GDP PPP (constant 2011 international $)
NY.GDP.MKTP.PP.KD<-NY.GDP.MKTP.PP.KD[which(NY.GDP.MKTP.PP.KD$Country.Code=='TUR'),] # GDP PPP (constant 2011 international $)
NY.GDP.MKTP.PP.KD<-na.omit(melt(setDT(NY.GDP.MKTP.PP.KD),id.vars=c(),variable.name="year"))
NY.GDP.MKTP.PP.KD$year<-as.integer(substr(NY.GDP.MKTP.PP.KD$year,2,length(NY.GDP.MKTP.PP.KD$year)))

### Labor Productivity (AGDP) -------------------------------------------------
AGDP<-read.csv(paste0(getwd(),path_econ_2021,'AgValueAdded_Worker_WDI_20180918.csv')) # Agriculture forestry and fishing value added per worker (constant 2010 US$)
AGDP_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Labor_Productivity.csv'))

AGDP<-na.omit(melt(setDT(AGDP),id.vars=c(),variable.name="year"))
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
ggplot(AGDP_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="AGDP") + theme_classic()

AGDP$Indicator <- 'AGDP'
AGDP<-reshape(AGDP,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
write.csv(AGDP,paste0(getwd(),path_raw_2021,'ADGP_raw.csv'))

### Finance Access (A2F) ------------------------------------------------------

### Price Volatility (PVOL) ---------------------------------------------------

### Government Support (AEXP) -------------------------------------------------
AEXP_ref<-read.csv(paste0(getwd(),path_ref_raw,'Raw_Government_Support.csv'))
# compare ref and raw values for Turkey (TUR)
AGDP_ref <- AGDP_ref[which(AGDP_ref$ISO=='TUR'),]
AGDP_ref <- melt(setDT(AGDP_ref), id.vars = c(), variable.name = "year")
AGDP_ref$year <- substr(AGDP_ref$year, 2, length(AGDP_ref$year))

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

Ag_Pop<-merge(AGDP, AGDP_capita, by=c('ISO','year'), all.x=TRUE)
Ag_Pop$Ag_Pop <- Ag_Pop$AGDP/Ag_Pop$AGDP_capita

Ag_Exp<-read.csv(paste0(getwd(),path_econ_2021,'Ag_Exp_IFPRI_1980_2012.csv')) # try dividing by number of ag workers
Ag_Exp<-na.omit(melt(setDT(Ag_Exp),id.vars=c("country","ISO","FAOCODE","Unit"),variable.name="year"))
Ag_Exp$year<-as.integer(substr(Ag_Exp$year,2,length(Ag_Exp$year)))
setnames(Ag_Exp,old=colnames(Ag_Exp),new=c("Country_Name","ISO","FAOCODE","unit",'year',"Ag_Exp"))
Ag_Exp<-subset(Ag_Exp,select=c(ISO,year,Ag_Exp))
AEXP <- merge(Ag_Pop, Ag_Exp, by=c('ISO','year'), all.x=TRUE)
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
ggplot(AEXP_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="AEXP") + theme_classic()

AEXP$Indicator <- 'AEXP'
AEXP<-reshape(AEXP,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
write.csv(AEXP,paste0(getwd(),path_raw_2021,'AEXP_raw.csv'))

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
ggplot(FLP_comp,aes(x=year,y=value,colour=type)) + geom_line() + labs(title="FLP") + theme_classic()

FLP$Indicator <- 'FLP'
FLP<-reshape(FLP,idvar=c("ISO","Indicator"),timevar ="year",direction ="wide")
write.csv(FLP,paste0(getwd(),path_raw_2021,'FLP_raw.csv'))

#### Environmental INDICATORS ##################################################
path_Env_2021 <- '//data_archive//2021_ed/Env//'

### Water Consumption (SUSI) --------------------------------------------------

### N Surplus (Nsur) ----------------------------------------------------------

### P Surplus (Psur) ----------------------------------------------------------

### Land Use Change (LUC) -----------------------------------------------------

### Greenhouse Gas Emmissions (GHG) -------------------------------------------

### Soil Erosion (SER) --------------------------------------------------------

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
