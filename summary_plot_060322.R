library(tidyverse)
library(sf)
library(readxl)
aw_xwalk<-read_csv("Data/AW_Xwalk_Original_062720.csv") %>%
  filter(Region=="ASW")


aw_df<-read_csv("Data/AW_mydf2.csv") %>%
  select(ParentGlobalID=globalid,
         Database=Survey123DB,
         SiteCode =  SITECODE,
         SiteType,
         CollectionDate = collection_date,
         Visit_No,
         Latitude, Longitude,
         State, Region, Stratum, Determination_Final) %>%
  mutate(Region="AW",
         CollectionDate= lubridate::mdy(CollectionDate),
         SiteType = case_when(SiteType=="Validation"~"Validation", T~"Baseline"),
         Strat2 = paste0("AW_",Stratum),
         Phase="Beta"
  ) %>%
  filter(ParentGlobalID %in% aw_xwalk$globalid) %>%
  filter(Determination_Final!="Unknown")



wm_df <- read_csv("Data/WM_main_step1.csv") %>%
  inner_join(read_csv("Data/WM_xwalk_step1.1.csv") %>%
               select(ParentGlobalID, SiteType=Set, Visit_No = VisitNo, State, Stratum, Determination_Final,
                      Latitude, Longitude)) %>%
  mutate(Region="WM") %>%
  select(ParentGlobalID,
         Database,
         SiteCode,
         SiteType,
         CollectionDate,
         Visit_No,
         Latitude ,
         Longitude,
         State,
         Region,
         Stratum,
         Determination_Final) %>%
  mutate(Strat2 = paste0("WM_",Stratum),
         Phase="Beta")

expansion_df0<-read_xlsx("Data/Western_Expansion_SDAM_v0_stakeholder_060322.xlsx", sheet="Western_Expansion_SDAM_v1_3")  %>%
  mutate(SiteCode=`Site Code`) %>%
  arrange(SiteCode, )

expansion_df0_visits<-expansion_df0 %>%
  group_by(SiteCode) %>%
  summarise(n_visits = length(SiteCode)) %>%
  ungroup() %>%
  mutate(SiteType = case_when(n_visits>1~"Baseline", T~"Validation"))

expansion_design<-read_csv("Data/Expansion_SiteListSentOut.csv") 
  
setdiff(expansion_df0$SiteCode, expansion_design$SiteCode)       



expansion_df<-expansion_df0 %>%
  left_join(expansion_design %>%
              select(SiteCode, Determination_Final=Determination_Tentative )) %>%
  transmute(Region=substr(SiteCode, 3,4),
            Region = case_when(substr(SiteCode, 1,2)=="HR"~"AW", T~Region),
            Database="Western_Expansion",
            SiteCode=SiteCode,
            SiteType="Baseline",
            CollectionDate = `Collection date`,
            Latitude = up_y, 
            Longitude=up_x,
            State=substr(SiteCode, 1,2),
            State=case_when(State=="HR"~"AZ",T~State),
            Stratum=case_when(Region == "AW" & State=="CA" ~ "CA",
                              Region == "AW" & State=="NV" ~ "NV",
                              Region == "AW" & State=="AZ" ~ "AZ",
                              Region == "AW" & State %in% c("NM", "TX") ~ "NM-TX",
                              Region == "AW" & State %in% c("CO","UT","WY", "MT") ~ "CO-WY-UT-MT",
                              Region == "WM" & State %in% c("CA", "NV") ~ "CA-NV",
                              Region == "WM" & State %in% c("AZ", "NM") ~ "Southern Rockies",
                              Region == "WM" & State %in% c("UT", "CO") ~ "Central Rockies",
                              Region == "WM" & State %in% c("MT", "WY", "SD") ~ "Northern Rockies",
                              T~"ERROR"
                              ),
            Strat2=paste0(Region,"_",Stratum),
            Determination_Final=Determination_Final,
            Phase="Expansion")





west_df<-bind_rows(aw_df, wm_df, expansion_df) %>%
  mutate(PhaseType = case_when(SiteCode %in% west_df$SiteCode[west_df$Phase=="Beta"] & SiteCode %in% west_df$SiteCode[west_df$Phase=="Expansion"] ~"Both",
                               SiteCode %in% west_df$SiteCode[west_df$Phase=="Beta"] & SiteCode %in% west_df$SiteCode[west_df$Phase!="Expansion"] ~"Beta only",
                               SiteCode %in% west_df$SiteCode[west_df$Phase!="Beta"] & SiteCode %in% west_df$SiteCode[west_df$Phase=="Expansion"] ~"Expansion only",
                               T~"Other"
                               ))

skimr::skim(west_df)

west_df$Region<-factor(west_df$Region, levels=c("AW","WM"))
west_df$Strat2<-factor(west_df$Strat2, levels=
                         c("AW_CA", "AW_NV","AW_AZ","AW_NM-TX", "AW_CO-WY-UT-MT",
                           "WM_CA-NV","WM_Southern Rockies","WM_Central Rockies", "WM_Northern Rockies"))
#Number of samples
west_df %>%
  group_by(Region, Determination_Final) %>%
  tally()
#Number of sites
west_df %>%
  select(Region, SiteCode, Determination_Final) %>%
  unique() %>%
  group_by(Region, Determination_Final) %>%
  tally()

west_df$PhaseType<-factor(west_df$PhaseType, levels=c("Beta only","Expansion only","Both"))
west_df %>%
  select(Region, SiteCode, Determination_Final, PhaseType) %>%
  unique() %>%
  group_by(Region, Determination_Final, PhaseType) %>%
  tally() %>%
  pivot_wider(names_from = PhaseType, values_from = "n")



my_projection<-3310
west_sf<-west_df %>%
  st_as_sf(coords=c("Longitude", "Latitude"),
           crs=4326,
           remove=F) %>%
  st_transform(crs=my_projection)

west_regions_sf<-st_read("NotForGit/Shapefiles/ASWandWMStudyAreas.shp") %>%
  st_transform(crs=my_projection) %>%
  mutate(Reg = case_when(Region=="USACE Arid West Region"~"AW", T~"WM"),
         Strat2 = paste0(Reg,"_",Stratum)) 
west_regions_sf$Strat2<-factor(west_regions_sf$Strat2, levels=
                         c("AW_CA", "AW_NV","AW_AZ","AW_NM-TX", "AW_CO-WY-UT-MT",
                           "WM_CA-NV","WM_Southern Rockies","WM_Central Rockies", "WM_Northern Rockies"))


west_regions_sf_bb<-st_bbox(west_regions_sf)


states_sf<-st_read("NotForGit/Shapefiles/statesp020_dissolve.shp") %>%
  st_transform(crs=my_projection)

strat_pal<-scales::viridis_pal(option="inferno", begin=.5, end=.9)(5) %>%
  c(scales::viridis_pal(option="mako", begin=.2, end=.6)(4) )

basemap<-ggplot()+
  geom_sf(data=states_sf, color="white")+
  geom_sf(data=west_regions_sf, aes(fill=Strat2), color=NA)+
  geom_sf(data=states_sf, color="white", fill=NA)+
  scale_fill_manual(values=strat_pal, name="Region_Stratum")+
  coord_sf(xlim=west_regions_sf_bb[c(1,3)],
           ylim=west_regions_sf_bb[c(2,4)])+
  theme_bw()+
  theme(axis.text = element_blank(), axis.ticks = element_blank())

basemap

basemap +
  geom_sf(data=west_sf)+
  facet_grid(Phase~Determination_Final)+
  coord_sf(xlim=west_regions_sf_bb[c(1,3)],
           ylim=west_regions_sf_bb[c(2,4)])
  

