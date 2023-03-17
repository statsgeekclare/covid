rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(sf)
library(ragg)
library(ggtext)
library(scales)
library(extrafont)
library(ggrepel)
library(gganimate)

#using code from Colin Angus (@VictimofMaths)
#Start with LA level cases for the whole of the UK
cases <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&format=csv"
cases <- curl_download(url=url, destfile=cases, quiet=FALSE, mode="wb")

casedata <- read.csv(cases) %>% 
  mutate(date=as.Date(date))

maxdate <- max(casedata$date)


# Using dplyr::filter to select just England
casedata <- casedata %>% filter(str_detect(areaCode, "^E"))

casedata <- casedata %>% 
  #Take the most recent 2 weeks of data
  group_by(areaName) %>% 
  arrange(date) %>% 
  slice_tail(n=8)


# select is picking the columns related to those numbers and shouldn't affect the data selection. 
# Spread would ideally need changing to pivot_wider() as this has been replaced.
casedata <- casedata %>% 
  ungroup() %>% 
  spread(date, newCasesBySpecimenDateRollingRate) %>% 
  select(c(1,2,4,11))

colnames(casedata) <- c("Lacode", "areaName", "prev", "latest")

casedata <- casedata %>% 
  mutate(abschange=latest-prev, relchange=abschange/prev)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltlacases <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(casedata, by="Lacode")

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))


plot2 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=ltlacases, aes(geometry=geom, fill=abschange), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(-1,1)*max(abs(casedata$abschange)), 
                         name="Change in cases\nper day per 100,000\nin the past week", direction=-1,
                         na.value="transparent")+
  theme_void()+
  theme(plot.title=element_markdown(face="bold", size=rel(1.5)),
        text=element_text(family="Lato"))+
  labs(title="Change in COVID cases in the past week",
       subtitle=paste0("Change in the past week in the rolling 7-day average number of cases at Lower Tier Local Authority level\nData up to ", maxdate),
       caption="Data from UKHSA, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")


agg_png("Outputs/COVIDCasesLTLAChangeCartogram.png", units="in", width=9, height=10, res=800)
plot2
dev.off()

