
setwd("/Users/kyulee/Google Drive/Z Drive/Postdoc_UPitt/Covid_safegraph/covid_safegraph")
library(tidyverse)
library(zoo)
library(geosphere)
library(animation)
library(rgl)
library(igraph)
library(reshape2)
library(ggpubr)
library(magick)
library(pdftools)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

combined_north_america_data <- read_csv("inputs/combined_north_america_data.csv")

fig_path <- "/Users/kyulee/OneDrive - University of Pittsburgh/covid_safegraph/figures/figures_v2/"

# Data imports and clean (Run this only one time)
mx_ca <- read_csv(file = "inputs/MX_CA_data.csv")
covid_nyt0 <- read.csv("/Users/kyulee/Google Drive/Z Drive/Postdoc_UPitt/Covid_safegraph/rawdata/NYT/us-states.csv")
full_date_df <- data.frame(date = unique(covid_nyt0$date))
# mexico and canada
mx_ca <- mx_ca %>%
  filter(!is.na(Province_State))%>%
  filter(!(Province_State %in% c("Calgary, Alberta", "Diamond Princess","Edmonton, Alberta","Grand Princess",
                                 "London, ON","Montreal, QC","Northwest Territories","Newfoundland and Labrador","Nunavut",
                                 "Recovered","Repatriated Travellers","Toronto, ON",
                                 "Unknown")))%>%
#  filter(!(Province_State %in% c("Calgary, Alberta", "Diamond Princess","Edmonton, Alberta","Grand Princess",
#                                 "London, ON","Montreal, QC",
#                                 "Recovered","Repatriated Travellers","Toronto, ON",
#                                 "Unknown")))%>%
  mutate(deaths = Deaths - lag(Deaths), 
         deaths = ifelse(deaths<0, lead(deaths), deaths),
         deaths = ifelse(deaths<0, lead(deaths), deaths), # this is not redundant
         deaths_sm = rollmean(deaths, k=7, fill=0, align="right"), 
       deaths_sm_rate = 100000 * deaths_sm / pop2020,
       cases = ifelse(cases<0, lead(cases), cases),
       cases_sm = rollmean(cases, k=7, fill=0, align="right"),
       cases_sm_rate = 100000 * cases_sm / pop2020
  )# create data for the full period of observations for all counties/provinces

n_state = length(unique(mx_ca$Province_State))
full_date_df <- data.frame(date = as.Date(unique(covid_nyt0$date)))
for (state in unique(mx_ca$Province_State)){
  print(state)
  this_data <- mx_ca[mx_ca$Province_State==state,]
  this_data_comb <- left_join(full_date_df, this_data, by=c("date"="dt"))
  if(state=="Alberta"){
    covid_mxca = this_data_comb
  } else {
    covid_mxca = rbind(covid_mxca,this_data_comb)
  }
}
# mexico population correction
mx_pop <- read.csv("inputs/mexico_pop.csv")
mx_pop_missing <- mx_pop %>%
  filter(State%in% c("Coahuila de Zaragoza","Queretaro de Arteaga"))
mxca_state_abb = data.frame(fips= unique(covid_mxca$Province_State), state_ab = c(NA, "CAB","CBC", "CMB","CNB","CNS","CON","CPE","CQC","CSK","CYT",
                                                                                  "AGU","BCN","BCS","CAM","CHP","CHH","CMX","COA","COL","DUR","GUA","GRO","HID","JAL","MEX","MIC","MOR","NAY","NLE","OAX",
                                                                                  "PUE","QUE","ROO","SLP", "SIN","SON","TAB","TAM","TLA","VER",'YUC',"ZAC"))
mxca_state_abb <- mxca_state_abb[!is.na(mxca_state_abb$fips),]
# mexico canada final cleaning & variable creation
covid_mxca2 <- covid_mxca %>%
  mutate(day_cnt = as.numeric(as.Date(date) - as.Date("2019-01-01") + 1),
         lat = ifelse(is.na(Lat), Latitude, Lat),
         lon = ifelse(is.na(Long_), Longitude, Long_),
          Province_State = na.locf(Province_State, fromLast = TRUE),
          Country_Region = na.locf(Country_Region, fromLast = TRUE),
          lat = na.locf(lat, fromLast = TRUE),
          lon = na.locf(lon, fromLast = TRUE),
         pop2020 = na.locf(pop2020, fromLast = TRUE)) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  left_join(mxca_state_abb, by=c('Province_State' = 'fips'))%>%
  mutate(
    pop2020 = ifelse(Province_State=="Coahuila", mx_pop_missing$pop2020[mx_pop_missing$State == "Coahuila de Zaragoza"],
                     ifelse(Province_State=="Queretaro", mx_pop_missing$pop2020[mx_pop_missing$State == "Queretaro de Arteaga"],pop2020)),
    deaths_sm = ifelse(is.na(deaths_sm),0,deaths_sm),
    cases_sm = ifelse(is.na(cases_sm),0,cases_sm),
    cases_sm_rate = ifelse(Province_State %in% c("Coahuila","Queretaro"), cases_sm/pop2020, cases_sm_rate),
    deaths_sm_rate = ifelse(Province_State %in% c("Coahuila","Queretaro"), deaths_sm/pop2020, deaths_sm_rate),
    cases_sm_rate = ifelse(is.na(cases_sm_rate),0,cases_sm_rate),
    deaths_sm_rate = ifelse(is.na(deaths_sm_rate),0,deaths_sm_rate),
    phase = ifelse(day_cnt<=527, 1, ifelse(day_cnt<=619,2,ifelse(day_cnt <=790,3,4)))) %>%
    rename(state = Province_State,
          case_r = cases_sm_rate,
          death_r = deaths_sm_rate) %>%
  group_by(state) %>%
  mutate(
    lon = min(lon), # some states have different lat/lon for the same location
    lat = min(lat)
  ) %>%
  ungroup()

saveRDS(covid_mxca2, "inputs/MX_CA_clean.Rdata")


####################################################################3

plot_timeseries <- function(coviddata, country, variable){
  if (country == 'US'){
    temp <- coviddata %>%
      rename(cases_sm = case_7avg,
             deaths_sm = death_7avg)
  }else {
  temp <- coviddata %>%
    filter(Country_Region == country)%>%
    filter(date >= as.Date("2020-03-01"))
  }
  if (variable == 'case'){
    variable_lab = 'number of cases'
    value = temp$cases_sm
    fignum = '1_'
  } else if (variable == 'death'){
    variable_lab = 'number of deaths'
    value = temp$deaths_sm
    fignum = '2_'
  } else if (variable == 'caserate'){
    variable_lab = 'case rate (per 100,000)'
    value = temp$case_r
    fignum = '1_'
  } else{
    variable_lab = 'death rate (per 100,000)'
    value = temp$death_r
    fignum = '2_'
  }
  ggplot(data = temp) + 
    geom_point(aes(x = date, y = value), size=1) + 
    facet_wrap(~state, scales = "free_y") + 
    scale_x_date(breaks = c(seq(from=as.Date("2020-03-01"), to=as.Date("2021-05-03"),by="2 months")),  date_labels = "%D") + 
    ylab(variable_lab)+
    xlab("Date")+
    theme_bw()+
    theme(
      # backgrounds
      panel.background = element_blank(), axis.line=element_line(colour = "black"), legend.key = element_blank(),
      # title
      legend.title=element_blank(),
      legend.text=element_text(size = 10),
      legend.position = 'bottom',#c(0.1,0.8),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 9, angle = 60, hjust = 1), #margin = margin(t=5)),
      axis.text.y = element_text(size = 9),
      strip.text.x = element_text(size = 11))+
    scale_y_continuous(limits=c(0,NA))
  
  ggsave(paste0(fig_path, fignum, country,"_",variable,"_7day_avg.pdf"), width = 11, height = 8)
}


covid_mxca2 <- readRDS("inputs/MX_CA_clean.Rdata")
covid_us <- readRDS("nyt_case_updated.Rdata")[[1]]
covid_us_new <- read.csv("/Users/kyulee/OneDrive - University of Pittsburgh/covid_safegraph/inputs/covid_by_state.csv")
fip_dict <- read.csv("/Users/kyulee/Google Drive/Z Drive/Postdoc_UPitt/Covid_safegraph/data/fip_dict.csv")
geodata_state<- data.frame(state=state.abb, fips=unique(fip_dict$state)[-c(9,52,53,54,55)], lon=state.center$x, lat=state.center$y)
covid_us <- covid_us_new %>%
  filter(date >= as.Date("2020-03-01")) %>%
  filter(!state %in% c("Alaska","Hawaii","District of Columbia"))%>%
  left_join(geodata_state %>% select(fips, lon, lat), by=c("state"="fips"))
covid_us$date <- as.Date(covid_us$date)

##Figure 1. country-specific time series of cases, one plot each for the USA, Canada, and Mexico, showing 4 waves
plot_timeseries(covid_mxca2, 'Mexico', 'case')
plot_timeseries(covid_mxca2, 'Canada', 'case')
plot_timeseries(covid_us, 'US', 'case')
plot_timeseries(covid_mxca2, 'Mexico', 'caserate')
plot_timeseries(covid_mxca2, 'Canada', 'caserate')
plot_timeseries(covid_us, 'US', 'caserate')
##Figure 2. country-specific time series of deaths, one plot each for the USA, Canada, and Mexico, showing 4 waves
plot_timeseries(covid_mxca2, 'Mexico', 'death')
plot_timeseries(covid_mxca2, 'Canada', 'death')
plot_timeseries(covid_us, 'US', 'death')
plot_timeseries(covid_mxca2, 'Mexico', 'deathrate')
plot_timeseries(covid_mxca2, 'Canada', 'deathrate')
plot_timeseries(covid_us, 'US', 'deathrate')
## Figure 3,4. Table of cumulative case and death rate per 100,000 in three countries
cum_mxca <- covid_mxca2 %>%
  group_by(state) %>%
  mutate(
    cumcases_sm = cumsum(cases_sm),
    cumcases_sm_rate = cumsum(case_r),
    cumdeath_sm = cumsum(deaths_sm),
    cumdeath_sm_rate = cumsum(death_r)
  ) %>%
  select(date, state, Country_Region, state_ab,pop2020,cumcases_sm_rate, cumdeath_sm_rate, lon, lat) %>%
  slice_tail()
cum_us <- covid_us %>%
  group_by(state) %>%
  mutate(
     Country_Region = 'US',
     cumcases_sm = cumsum(case_7avg),
     cumcases_sm_rate = cumsum(case_r),
     cumdeath_sm = cumsum(death_7avg),
     cumdeath_sm_rate = cumsum(death_r)) %>%
  rename(pop2020 = pop_sum) %>%
  select(date, state, Country_Region, state_ab,pop2020,cumcases_sm_rate, cumdeath_sm_rate, lon, lat) %>%
  slice_tail()  

cum_all<- cum_mxca %>%
  bind_rows(cum_us) %>%
  arrange(Country_Region,state)

cum_all_bycountry <- cum_all %>%
  group_by(Country_Region) %>%
  mutate(pop_country = sum(pop2020)
         )
write.csv(cum_all,"inputs/cumulative_all.csv")

## Figure 5. Table of cumulative case rate and death rat
covid_all <- covid_us %>%
  mutate(Country_Region = 'US', date=as.Date(date)) %>%
  rename(state_ab = state_abb, case_r=case_rate, death_r=death_rate,case_7avg=case_rate_sm,death_7avg=death_rate_sm) %>%
  select(date, Country_Region, state, state_ab, lon, lat, case_r, death_r, case_7avg, death_7avg) %>%
  rename(cases_sm = case_7avg,
         deaths_sm = death_7avg) %>%
  bind_rows(covid_mxca2 %>%select(date, Country_Region, state, state_ab, lon, lat, case_r, death_r , cases_sm, deaths_sm)) %>%
  arrange(Country_Region, state, date)
covid_all$state <- factor(covid_all$state, levels = unique(covid_all$state))
covid_caserate_wide <- reshape2::dcast(covid_all, date~state, value.var = "case_r", fill=0, fun.aggregate=sum)
write.csv(covid_caserate_wide, "inputs/case_rate_bydate.csv")  
covid_deathrate_wide <- reshape2::dcast(covid_all, date~state, value.var = "death_r", fill=0, fun.aggregate=sum)
write.csv(covid_deathrate_wide, "inputs/death_rate_bydate.csv")  
saveRDS(covid_all, "inputs/covid_all_clean.Rdata")

## Figure 6. Scatter plot of cases vs deaths for all states, showing correlation of cases and deaths
scatter_cumm <- function(data, country){
  if (country %in% c('All', 'ALL',"all")){
    temp = data %>% arrange(state_ab)
    geo_tem = geo_all_color
  }else{
    temp = data %>% filter(Country_Region == country)  %>% arrange(state_ab)
    geo_temp = geo_all_color$rgb[geo_all_color$state_ab %in% temp$state_ab]
  }  
  if (country =='US'){
    gap = max(temp$cumcases_sm_rate)-max(temp$cumcases_sm_rate)*0.03
  } else{
    gap = max(temp$cumcases_sm_rate)-max(temp$cumcases_sm_rate)*0.1
  }
  this_plot <- ggplot(temp, aes(x=cumdeath_sm_rate,y=cumcases_sm_rate))+
    geom_point(alpha=0.5, color=geo_temp,size=2)+
    geom_smooth(method='lm', se=FALSE, color='black', formula = y~x)+
    geom_text(aes(label=state_ab), size = 3, hjust=-0.4, vjust=0)+
    xlab("Cumulative death rate (per 100,000)")+
    ylab("Cumulative case rate (per 100,000)")+
    stat_cor(label.x=min(temp$cumdeath_sm_rate), label.y = gap, show.legend = FALSE)+
    stat_regline_equation(label.x=min(temp$cumdeath_sm_rate), label.y=max(temp$cumcases_sm_rate))+
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(), 
      panel.grid.minor= element_blank(), 
      panel.background = element_blank(), 
      axis.line=element_line(colour = "black"), 
      legend.key = element_blank(),
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10))
#  ggsave(paste0(fig_path,"6_scatter_DeathvsCaserate(color)",country, ".pdf"), width = 15, height=9)
  
  return(this_plot)
}
# color generating function
gen_2Dcolor <- function(geo_all) {
  # geo_loc_us <- geo_all %>%
  #   filter(Country_Region == 'US')
  # 
  # geo_loc_us <- geo_loc_us%>%
  #   arrange(lon) %>%
  #   ungroup()%>%
  #   mutate (
  #     red_idx = seq(0,nrow(geo_loc_us)-1,by=1)
  #   ) %>%
  #   arrange(lat) %>%
  #   mutate(
  #     blue_idx = seq(0,nrow(geo_loc_us)-1,by=1),
  #     green_idx=0,
  #     rgb = rgb(red=red_idx,green=0,blue=blue_idx,maxColorValue = nrow(geo_loc_us)) 
  #   ) %>%
  #   arrange(state_ab)
  # 
  # geo_loc_mx_ca <- geo_all %>%
  #   filter(Country_Region == 'Mexico' | Country_Region == 'Canada') %>%
  #   mutate(red_idx = ifelse(Country_Region=='Mexico',0,1),
  #          blue_idx = 0,
  #          green_idx = ifelse(Country_Region=='Mexico',1,0),
  #          rgb = rgb(red=red_idx,green=green_idx,blue=blue_idx))
  
  # geo_all_color <- geo_loc_us %>%
  #   bind_rows(geo_loc_mx_ca) %>%
  #   arrange(state_ab)
  geo_all_color <- geo_all %>%
    mutate(red_idx = ifelse(Country_Region=='Canada',0.6,0),
           blue_idx = ifelse(Country_Region=='US',0.6,0),
           green_idx = ifelse(Country_Region=='Mexico',0.6,0),
           rgb = rgb(red=red_idx,green=green_idx,blue=blue_idx))%>%
    arrange(state_ab)
  
  
  return(geo_all_color)
}
# geo distance
geo_all <- covid_all %>%
  select(Country_Region,state,state_ab,lon,lat) %>%
  distinct() %>%
  mutate(key = paste0(Country_Region,",",state))
# color by country
geo_all_color <- gen_2Dcolor(geo_all)

scatter_cumm(cum_all, 'All')
plt_cumm_us <- scatter_cumm(cum_all, 'US')
plt_cumm_ca <-scatter_cumm(cum_all, 'Canada')
plt_cumm_mx <-scatter_cumm(cum_all, 'Mexico')
plt_cumm_mxca <- ggarrange(plt_cumm_mx, plt_cumm_ca, labels = c("B","C"), nrow=2, widths = 7, heights = 7)
plt_cumm <- ggarrange(plt_cumm_us, plt_cumm_mxca, labels = c("A",NA), ncol = 2, nrow = 1, heights = 7, widths = 14)
ggsave(paste0(fig_path, "2_scatter_cummulative.pdf"), height = 9, width = 18)

## Figure 7. Stacked state-specific times series of cases
plot_timeseries_bycountry <-function(coviddata, country, variable){
  temp <- coviddata %>%
    filter(Country_Region == country) 
 
  if (variable == 'case'){
    variable_lab = 'Number of cases'
    value = temp$cases_sm
    fignum = '7_'
  } else if (variable == 'death'){
    variable_lab = 'Number of deaths'
    value = temp$deaths_sm
    fignum = '8_'
  } else if (variable == 'caserate'){
    variable_lab = 'Case rate (per 100,000)'
    value = temp$case_r
    fignum = '7_'
  } else{
    variable_lab = 'Death rate (per 100,000)'
    value = temp$death_r
    fignum = '8_'
  }
  
  if (country == 'US'){
    num_rows = 5
  }else if(country == 'Mexico'){
    num_rows =4
  }else{
    num_rows=1
  }
  ggplot(data = temp) + 
    geom_line(aes(x = date, y = value, color= state, linetype=state), size=1) + 
    ggtitle(country)+
    scale_x_date(breaks = c(seq(from=as.Date("2020-03-01"), to=as.Date("2021-05-03"),by="1 months")),  date_labels = "%D") + 
    ylab(variable_lab)+
    xlab("Date")+
    theme_bw()+
    theme(
      # backgrounds
      panel.grid.major = element_blank(), panel.grid.minor= element_blank(), panel.background = element_blank(), axis.line=element_line(colour = "black"), legend.key = element_blank(),
      # title
      plot.title = element_text(size = 16, hjust=0.5),
      legend.title=element_blank(),
      legend.text=element_text(size = 13),
      legend.position = 'bottom',#c(0.1,0.8),
      legend.key.height=unit(0.2, 'inch'),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 13,  hjust = 1), #margin = margin(t=5)),
      axis.text.y = element_text(size = 13))+
    scale_color_manual(values=c(rep(gg_color_hue(16),3)))+
    scale_linetype_manual(values=c(rep('solid',16),rep('dotted',16),rep('dotdash',16)))+
    guides(color = guide_legend(nrow=num_rows, byrow=TRUE), linetype = guide_legend(nrow=num_rows, byrow=TRUE))
#  ggsave(paste0(fig_path, fignum, country, "_stacked_timeseries_",variable,".pdf"), width=13,height=9)
}
# this is for the combined figures with snapshot of movies
plot_timeseries_bycountry2 <-function(coviddata, country, variable){
  temp <- coviddata %>%
    filter(Country_Region == 'US') 
  
  temp1 <- temp %>% 
    filter(!(state_ab %in% c("LA","NY","AZ","FL","ND","SD","SC","MI")))
  
  temp2 <- temp %>%
    filter((state_ab %in% c("LA","NY","AZ","FL","ND","SD","SC","MI")))

  if (variable == 'case'){
    variable_lab = 'Number of cases'
    value = temp$cases_sm
    value1 = temp1
    fignum = '7_'
  } else if (variable == 'death'){
    variable_lab = 'Number of deaths'
    value = temp$deaths_sm
    fignum = '8_'
  } else if (variable == 'caserate'){
    variable_lab = 'Case rate (per 100,000)'
    value = temp$case_r
    value1 = temp1$case_r
    value2 = temp2$case_r
    fignum = '7_'
  } else{
    variable_lab = 'Death rate (per 100,000)'
    value = temp$death_r
    fignum = '8_'
  }
  
  num_rows = 5
  c_select = gg_color_hue(16)
  c_all = rep('grey',length(unique(temp$state)))
  c_all[16] = c_select[1] # LA
  c_all[30] = c_select[2] # NY
  c_all[2] = c_select[5] # AZ
  c_all[8] = c_select[6] # FL
  c_all[32] = c_select[9] # ND
  c_all[39] = c_select[10] #SD
  c_all[38] = c_select[12] #SC
  c_all[20] = c_select[16] #MI
  
  ggplot(data = temp) + 
    geom_line(data = temp1, aes(x = date, y = value1, color= state), size=1) + 
    geom_line(data = temp2, aes(x = date, y = value2, color= state), size=1) + 
    scale_x_date(breaks = c(seq(from=as.Date("2020-03-01"), to=as.Date("2021-05-03"),by="1 months")),  date_labels = "%D") + 
    ylab(variable_lab)+
    xlab("Date")+
    theme_bw()+
    theme(
      # backgrounds
      panel.grid.major = element_blank(), panel.grid.minor= element_blank(), panel.background = element_blank(), axis.line=element_line(colour = "black"), legend.key = element_blank(),
      # title
      plot.title = element_text(size = 16, hjust=0.5),
      legend.title=element_blank(),
      legend.text=element_text(size = 13),
      legend.position = 'bottom',#c(0.1,0.8),
      legend.key.height=unit(0.2, 'inch'),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 13,  hjust = 1, angle=45), #margin = margin(t=5)),
      axis.text.y = element_text(size = 13))+
    scale_color_manual(values=c_all, breaks = c("Louisiana","New York","Arizona","Florida","North Dakota","South Dakota","South Carolina","Michigan"))+
    guides(color = guide_legend(nrow=1, byrow=TRUE))
  ggsave(paste0(fig_path, fignum, country, "_stacked_timeseries(nolabel)_",variable,".pdf"), width=15,height=4)
}

plot_timeseries_bycountry(covid_all, 'US', 'case')
plot_timeseries_bycountry(covid_all, 'Mexico', 'case')
plot_timeseries_bycountry(covid_all, 'Canada', 'case')
r_case_time_US <- plot_timeseries_bycountry(covid_all, 'US', 'caserate')
r_case_time_MX <-plot_timeseries_bycountry(covid_all, 'Mexico', 'caserate')
r_case_time_CA <-plot_timeseries_bycountry(covid_all, 'Canada', 'caserate')
r_case_time_MXCA <- ggarrange(r_case_time_MX,r_case_time_CA, labels = c("B","C"),nrow=2, widths=7,heights=10)
ggarrange(r_case_time_US,r_case_time_MXCA, labels = c("A",""), ncol=2, widths=c(0.55,0.45),heights=10)
ggsave(paste0(fig_path,"7_All_stacked_timeseries_caserate.pdf"), width=28.5, height=15)
plot_timeseries_bycountry(covid_all, 'US', 'death')
plot_timeseries_bycountry(covid_all, 'Mexico', 'death')
plot_timeseries_bycountry(covid_all, 'Canada', 'death')
plot_timeseries_bycountry(covid_all, 'US', 'deathrate')
plot_timeseries_bycountry(covid_all, 'Mexico', 'deathrate')
plot_timeseries_bycountry(covid_all, 'Canada', 'deathrate')
r_death_time_US <- plot_timeseries_bycountry(covid_all, 'US', 'deathrate')
r_death_time_MX <-plot_timeseries_bycountry(covid_all, 'Mexico', 'deathrate')
r_death_time_CA <-plot_timeseries_bycountry(covid_all, 'Canada', 'deathrate')
r_death_time_MXCA <- ggarrange(r_death_time_MX,r_death_time_CA, labels = c("B","C"),nrow=2, widths=7,heights=10)
ggarrange(r_death_time_US,r_death_time_MXCA, labels = c("A",""), ncol=2, widths=c(0.55,0.45),heights=10)
ggsave(paste0(fig_path,"7_All_stacked_timeseries_deathrate.pdf"), width=27, height=15)
## 9. Scatter plot of geo distance vs case time serie sdistance for all pairs of states in three countries, showing overall correlation
# function to find state order based on variable of interest
find_stateorder <- function(direction, country){
  if(country == 'all'){
    temp = geo_all
  }else if(country %in% c('us','US','USA')){
    temp = geo_all[geo_all$Country_Region == 'US',]
  }else if (country %in% c('ca','CA','Canada')){
    temp = geo_all[geo_all$Country_Region == 'Canada',]
  }else if (country %in% c('mx',"MX","Mexico")){
    temp = geo_all[geo_all$Country_Region == 'Mexico',]
  }
  if (direction == 'latitude'){
    state_order = temp$state_ab[order(temp$lat)]
  } else {
    state_order = temp$state_ab[order(temp$lon)]
  }
  return(state_order)
}
# function to prepare for long form data
prep_df_long <- function(matDF,direction,country){
  matDF2 <- as.data.frame(cbind(state = rownames(matDF), matDF))
  DF_long <- melt(matDF2, id.vars = "state")
  colnames(DF_long) <- c("state1","state2","distance")
  DF_long$distance = as.numeric(DF_long$distance)
  state_order <- find_stateorder(direction,country)
  DF_long$state1 <- factor(DF_long$state1, levels=state_order)
  DF_long$state2 <- factor(DF_long$state2, levels=state_order)
  DF_long <- DF_long[order(DF_long$state1, DF_long$state2),]
}
# function to generate scatter plot againstcase distance 
scatter_casedist <- function(variable, filename, st){
  if (variable == 'geodist'){
    var_name = "Geographical distance (mi)"
  }
  ggscatter(comb_lg, x=variable, y="casedist", add="reg.line", alpha=0.1, size=3)+
    ggtitle(st)+
    #    ggtitle(paste0("case vs ", variable))+
#    geom_text(aes(label=states), size = 1.5, hjust=-0.4, vjust=0)+
    ylab("Case rate distance")+
    xlab(var_name)+
    stat_cor(label.x=min(comb_lg[,variable]), label.y = max(comb_lg$casedist)-0.05, size=5)+
    stat_regline_equation(label.x=min(comb_lg[,variable]), label.y=max(comb_lg$casedist), size=5)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14))
  ggsave(paste0(fig_path, "9_", filename), width = 11, height=11)
}

covid_all <- readRDS("inputs/covid_all_clean.Rdata")
# case distance
out <- reshape2::dcast(covid_all, date~state_ab, value.var = "case_r", fill=0, fun.aggregate=sum)
corout = as.matrix(cor(out[,-c(1)], method = "pearson"))
casedist = 1-corout
# geo distance
distanceDF <- distm(geo_all[,c("lon","lat")], fun = distVincentyEllipsoid)/1609
colnames(distanceDF) <- geo_all$state_ab
rownames(distanceDF) <- geo_all$state_ab
latdistDF <- distm(data.frame(lon=rep(0,nrow(geo_all)),lat=geo_all$lat), fun=distVincentyEllipsoid)/1609
colnames(latdistDF) <- geo_all$state_ab
rownames(latdistDF) <- geo_all$state_ab
londistDF <- distm(data.frame(lon=geo_all$lon,lat=rep(0,nrow(geo_all))), fun=distVincentyEllipsoid)/1609
colnames(londistDF) <- geo_all$state_ab
rownames(londistDF) <- geo_all$state_ab
# geodistance in a pair of states (long-form)
geodist_lg <- prep_df_long(distanceDF, 'latitude','all')
# latitude distance in a pair of states (long-form)
latdist_lg <- prep_df_long(latdistDF, 'latitude','all')
# longitude distance in a pair of states (long-form)
londist_lg <- prep_df_long(londistDF, 'longitude','all')
# casedistance in a pair of states (long-form)
casedist_lg <-prep_df_long(casedist, 'latitude','all')
# combine two datasets
comb_lg <- casedist_lg %>%
  filter((state1 != state2) & (as.character(state1) < as.character(state2))) %>% # remove selfpair and remove duplicated pairs
  mutate(states = paste0(state1,"/",state2))%>%
  rename(casedist = distance)%>%
  left_join(geodist_lg, by=c("state1","state2"))%>%
  rename(geodist = distance) %>%
  left_join(latdist_lg, by=c("state1","state2"))%>%
  rename(latdist = distance) %>%
  left_join(londist_lg, by=c("state1","state2")) %>%
  rename(londist = distance)

scatter_casedist("geodist","reg_case_geodist_updated.pdf","")

# Figure 12. State by state scatter plots
state_list <- geo_all$state_ab
#state_list <- c('SD','VER','CMB','CA')
comb_lg2 <- casedist_lg %>%
  filter((state1 != state2)) %>% # remove selfpair 
  mutate(states = paste0(state1,"/",state2))%>%
  rename(casedist = distance)%>%
  left_join(geodist_lg, by=c("state1","state2"))%>%
  rename(geodist = distance) %>%
  left_join(latdist_lg, by=c("state1","state2"))%>%
  rename(latdist = distance) %>%
  left_join(londist_lg, by=c("state1","state2")) %>%
  rename(londist = distance)


# a function to generate scatter plot with case distance by country
scatter_casedist_bystate <- function(variable){
  if (variable == 'geodist'){
    varlab = 'Geographical distance (mi)'
  }else if (variable == 'londist'){
    varlab = 'Longitude distance (mi)'
  } else{
    varlab = 'Latitude distance (mi)'
  }
#  pdf(paste0(fig_path,"12_reg_case_",variable,"_bystate2.pdf"), width = 13, height=9)
  plot_list <- as.list(rep(0,length(state_list)))
  for (i in 1:length(state_list)){
    print(i)
    st = state_list[i]
    print(st)
    country_key = geo_all_color$key[geo_all_color$state_ab == st]
    temp <- comb_lg2 %>%
      filter(state1 == st) %>%
      left_join(geo_all_color %>% select(state_ab, rgb), by=c("state2"="state_ab"))
    state2_color = geo_all_color$rgb[geo_all_color$state_ab %in% temp$state2]
    this_plot <-ggplot(temp, aes(x=temp[,variable],y=casedist))+
            geom_point(alpha=0.5, color=temp$rgb,size=2)+
            geom_smooth(method='lm', se=FALSE, color='black', formula = y~x)+
            geom_text(aes(label=state2), size = 3, hjust=-0.4, vjust=0)+
            ggtitle(country_key)+
            ylab("Case rate distance")+
            xlab(varlab)+
            stat_cor(label.x=min(temp[,variable]), label.y = max(temp$casedist)-0.05, show.legend = FALSE)+
            stat_regline_equation(label.x=min(temp[,variable]), label.y=max(temp$casedist))+
            theme(
              plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(), 
              panel.grid.minor= element_blank(), 
              panel.background = element_blank(), 
              axis.line=element_line(colour = "black"), 
              legend.key = element_blank())
    plot_list[[i]] <- this_plot
    
  }
  return(plot_list)
  #dev.off()
}
# scatter plot of correlation between casedist vs variable of interest in a selected state
scatter_casedist_select <- function(variable, st){
  if (variable == 'geodist'){
    varlab = 'Geographical distance (mi)'
  }else if (variable == 'londist'){
    varlab = 'Longitude distance (mi)'
  } else{
    varlab = 'Latitude distance (mi)'
  }
  if (st %in% c("SD","VER")){
    xlab_text = ""
    ylab_text = "Case rate distance"
  } else{
    xlab_text=varlab
    ylab_text = "Case rate distance"
  }
  if (st %in% c("VER","CA")){
    xlab_text = varlab
    ylab_text = ""
  }
  country_key = geo_all_color$key[geo_all_color$state_ab == st]
  temp <- comb_lg2 %>%
    filter(state1 == st) %>%
    left_join(geo_all_color %>% select(state_ab, rgb), by=c("state2"="state_ab"))
  state2_color = geo_all_color$rgb[geo_all_color$state_ab %in% temp$state2]
  this_plot <-ggplot(temp, aes(x=temp[,variable],y=casedist))+
    geom_smooth(method='lm', se=FALSE, color='light grey', formula = y~x)+
    geom_point(alpha=0.7, color=temp$rgb,size=2)+
    geom_text(aes(label=state2), size = 3, hjust=-0.4, vjust=0, color=temp$rgb)+
    ggtitle(country_key)+
    ylab(ylab_text)+
    xlab(xlab_text)+
    stat_cor(label.x=min(temp[,variable]), label.y = max(temp$casedist)-0.05, show.legend = FALSE)+
    stat_regline_equation(label.x=min(temp[,variable]), label.y=max(temp$casedist))+
    theme(
      plot.title = element_text(hjust = 0.5, size=15),
      panel.grid.major = element_blank(), 
      panel.grid.minor= element_blank(), 
      panel.background = element_blank(), 
      axis.line=element_line(colour = "black"), 
      legend.key = element_blank(),
      axis.text.x = element_text(size=11),
      axis.text.y = element_text(size=11),
      axis.title.x = element_text(size=13),
      axis.title.y = element_text(size=13)
    )
  return(this_plot)
}

# plot
casedist_geo_all <- scatter_casedist_bystate("geodist")
casedist_geo_SD <- scatter_casedist_select("geodist","SD")
casedist_geo_VER <- scatter_casedist_select("geodist","VER")
casedist_geo_CMB <- scatter_casedist_select("geodist","CMB")
casedist_geo_CA <- scatter_casedist_select("geodist","CA")
casedist_geo_select <- ggarrange(casedist_geo_SD,casedist_geo_VER,casedist_geo_CMB,casedist_geo_CA, labels = c("A","B","C","D"),nrow=2, ncol=2, widths = 15, heights=15)
ggsave(paste0(fig_path, "7_scatter_casedist_geodist.pdf"), width = 18, height=15)
scatter_casedist_bystate("londist")
scatter_casedist_bystate("latdist")


## Figure 10. 2D KK map of case time series distance for all states in three countries
gen_net <- function(ncnt, data, period, country){
  # select data for a specific phase
  if (period == 'wave')
    temp <- data %>%
      filter(phase == ncnt) %>%
      arrange(state_ab)
  else{
    temp <- data %>%
      arrange(state_ab)
  }
  # country-specific data if applicable
  if(country %in% c('all','All','ALL')){
    geo_loc = geo_all_color
  }
  else if (country == 'US'){
    geo_loc = geo_all_color[geo_all_color$Country_Region=='US',]
    temp <- temp %>% filter(Country_Region == country)
  }else if (country == 'Mexico'){
    geo_loc = geo_all_color[geo_all_color$Country_Region=='Mexico',]
    temp <- temp %>% filter(Country_Region == country)
  } else{
    geo_loc = geo_all_color[geo_all_color$Country_Region=='Canada',]
    temp <- temp %>% filter(Country_Region == country)
  }
  # state/province list included in this dataset
  state_list = unique(temp$state_ab[order(temp$state_ab)])
  # matrix
  out <- reshape2::dcast(temp, date~state_ab, value.var = "case_r", fill=0, fun.aggregate=sum)
  # calculate correlation
  corout = as.matrix(abs(cor(out[,-c(1)], method = "pearson")))
  temp_wide2 <- 1-corout 
  diag(temp_wide2) <- 0
  # generate network object
  net <- graph.adjacency(temp_wide2, mode='undirected', weighted=TRUE)
  V(net)$color = geo_loc$rgb[geo_loc$state_ab %in% state_list]
  # network layout
  set.seed(1234)
  net_ly <- layout_with_kk(net)
  
  return(list(net,net_ly))
}

# function to generate network figures by phase (for case)
gen_net_fig <- function(n_phase, covid_data, country, period, filename){
  pdf(paste0(fig_path,filename),width=15, height=15)
  for (i in 1:n_phase){
    print(i)
    net_outcome <- gen_net(i, covid_data, period , country)
    this_net <- net_outcome[[1]]
    this_net_ly <- net_outcome[[2]]
    if (period == 'wave'){
      this_phase = paste0("Wave ",i)
    }else{
      this_phase = ""
    }
    set.seed(1234)
    plot(this_net, 
         main=this_phase,
         edge.curved=0, 
         vertex.size=2,
         vertex.label.color='black', 
         vertex.label.cex=1,
         vertex.label.dist=0.5,
         edge.color='white',
         layout=this_net_ly)
  }
  dev.off()
}

gen_net_fig(1,  covid_all, 'ALL','full', "10_2Dnet_caserate_ALL_updated.pdf")
gen_net_fig(1,  covid_all, 'US','full', "10_2Dnet_caserate_US_updated.pdf")
gen_net_fig(1,  covid_all, 'Canada','full', "10_2Dnet_caserate_Canada.pdf")
gen_net_fig(1,  covid_all, 'Mexico','full', "10_2Dnet_caserate_Mexico.pdf")

## Figure 11. Link to 3D KK map
casenet_all <- gen_net(1, covid_all, 'full', 'ALL')
saveRDS(list(casenet_all[[1]]), 'inputs/casenet_usmxca.Rdata')
casenet_us <- gen_net(1, covid_all, 'full' , 'US')
saveRDS(list(casenet_us[[1]]), 'inputs/casenet_us.Rdata')
casenet_ca <- gen_net(1, covid_all, 'full' , 'Canada')
saveRDS(list(casenet_ca[[1]]), 'inputs/casenet_ca.Rdata')
casenet_mx <- gen_net(1, covid_all, 'full' , 'Mexico')
saveRDS(list(casenet_mx[[1]]), 'inputs/casenet_mx.Rdata')

net_case <- readRDS("inputs/casenet_usmxca.Rdata")[[1]]
net_case <- readRDS("inputs/casenet_us.Rdata")[[1]]
net_case <- readRDS("inputs/casenet_ca.Rdata")[[1]]
net_case <- readRDS("inputs/casenet_mx.Rdata")[[1]]
set.seed(1234)
case_kk_ly_3D<- layout_with_kk(net_case, dim=3)
par3d(cex=0.8, windowRect = c(0,0,1000,1000))
rglplot(net_case, 
        main = "full period for 40 states",
        edge.curved=0, 
        vertex.size=5,
        vertex.label.color='black', 
        cex=3,
        vertex.label.dist=0.8,
        margin=-0.05,
        edge.color='white',
        edge.width = 0.0,
        layout=case_kk_ly_3D)      

play3d(spin3d(axis=c(0,1,1),rpm=5), duration=20)
movie3d(
  movie="3Dnetwork_animation", 
  spin3d(axis = c(0, 1, 1), rpm = 7),
  duration = 10, 
  dir = ,
  type = fig_path, 
  clean = TRUE
)

rgl.snapshot(paste0(fig_path,"3Dnet_2Dsnapshot_(US)_updated", fmt='pdf'))

this_net = casenet_all[[1]]
x_coor = c(rep(0,53),geo_all_color$lon[54],rep(0,36))
y_coor = c(rep(0,53),geo_all_color$lat[54],rep(0,36))
xy_coor = cbind(x_coor, y_coor)
xy_coor[which(xy_coor == 0)] <- NA
this_net_ly = layout_with_kk(this_net,xy_coor,  dim=2)
set.seed(1234)
pdf(paste0(fig_path,"test.pdf"),width=15, height=15)
plot(this_net, 
     edge.curved=0, 
     vertex.size=2,
     vertex.label.color='black', 
     vertex.label.cex=1,
     vertex.label.dist=0.5,
     edge.color='white',
     layout=this_net_ly)
dev.off()

# combine 3 network pdf files to one 
netUS <- image_read(paste0(fig_path, "3Dnet_2Dsnapshot_(US)pdf.png"))
netCA <- image_read(paste0(fig_path, "3Dnet_2Dsnapshot_(CA)pdf.png"))
netMX <- image_read(paste0(fig_path, "3Dnet_2Dsnapshot_(MX)pdf.png"))
netUS_plt <-ggplot()+
  annotation_custom(grid::rasterGrob(netUS))
#  theme(panel.border=element_rect(color="black",size=0.5))
netMX_plt <-ggplot()+
  annotation_custom(grid::rasterGrob(netMX))
#  theme(panel.border=element_rect(color="black",size=0.5))
netCA_plt <-ggplot()+
  annotation_custom(grid::rasterGrob(netCA))
#  theme(panel.border=element_rect(color="black",size=0.5))
  
plt_netCAMX <- ggarrange(netMX_plt,netCA_plt, labels=c("B","C"),nrow=2, heights=9, widths=9)
ggarrange(netUS_plt,plt_netCAMX, labels=c("A",""),ncol=2, heights=9, widths=18)
ggsave(paste0(fig_path, "6_2Dnetwork_bycountry.pdf"), height = 20, width = 18)


# Figure 1,2. combined versions
plot_timeseries2 <- function(temp, variable){
  if (variable == 'case'){
    variable_lab = 'number of cases'
    value = temp$cases_sm
    fignum = '1_'
  } else if (variable == 'death'){
    variable_lab = 'number of deaths'
    value = temp$deaths_sm
    fignum = '2_'
  } else if (variable == 'caserate'){
    variable_lab = 'case rate (per 100,000)'
    value = temp$case_r
    fignum = '1_'
  } else{
    variable_lab = 'death rate (per 100,000)'
    value = temp$death_r
    fignum = '2_'
  }

  ggplot(data = temp,aes(x = date, y = value, color=Country_Region)) + 
    geom_point( size=1) + 
    facet_wrap(~state, scales = "free_y") + 
    scale_x_date(breaks = c(seq(from=as.Date("2020-03-01"), to=as.Date("2021-05-03"),by="2 months")),  date_labels = "%D") + 
    ylab(variable_lab)+
    xlab("Date")+
    theme_bw()+
    theme(
      # backgrounds
      panel.grid.major = element_blank(), panel.grid.minor= element_blank(), panel.background = element_blank(), axis.line=element_line(colour = "black"), legend.key = element_blank(),
      # title
      legend.title=element_blank(),
      legend.text=element_text(size = 10),
      legend.position = 'bottom',#c(0.1,0.8),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 9, angle = 60, hjust = 1), #margin = margin(t=5)),
      axis.text.y = element_text(size = 9),
      strip.text.x = element_text(size = 11))+
    scale_y_continuous(limits=c(0,NA))
#    scale_color_manual(values=c(ctry_colors[1],ctry_colors[2],ctry_colors[3]))
  
  ggsave(paste0(fig_path, fignum, "all_",variable,"_7day_avg.pdf"), width = 15, height = 11)
}
ctry_colors <- gg_color_hue(3)
covid_all2 <- covid_all %>%
  mutate(
    country_clr = ifelse(Country_Region=='US',ctry_colors[1],ifelse(Country_Region=='Mexico',ctry_colors[2],ctry_colors[3])),
  ) 
covid_all2$state = as.character(covid_all2$state)
covid_all2$state[covid_all2$state=="Prince Edward Island"]="Prince Edward Isl."
covid_all2$state[covid_all2$state=="Ciudad de Mexico"]="Mexico City"
covid_all2$state[covid_all2$state=="Baja California Sur"]="Baja Cali. Sur"
state_order_bycty <- unique(covid_all2$state[order(covid_all2$Country_Region,decreasing=TRUE)])
covid_all2$state = factor(covid_all2$state, levels=state_order_bycty)

plot_timeseries2(covid_all2,'caserate')
plot_timeseries2(covid_all2,'deathrate')

# Figure 1,2 revised (total by country)
mxca_pop <- covid_mxca2%>%
  select(state, Country_Region, pop2020)%>%
  distinct() %>%
  group_by(Country_Region) %>%
  mutate(pop_ctry = sum(pop2020)) %>%
  ungroup()%>%
  select(state, pop_ctry)
us_pop <- covid_us %>%
  select(state, pop_sum)%>%
  distinct() %>%
  ungroup() %>%
  mutate(pop_ctry = sum(pop_sum)) %>%
  select(state, pop_ctry)

pop_all <- rbind(mxca_pop, us_pop)

covid_all3 <- covid_all %>%  
  left_join(pop_all, by=c("state")) %>%
  group_by(Country_Region, date) %>%
  summarize(
    cases_sm_ctry = sum(cases_sm),
    deaths_sm_ctry = sum(deaths_sm),
    pop = pop_ctry,
    case_r = cases_sm_ctry/pop,
    death_r = deaths_sm_ctry/pop) %>%
  arrange(Country_Region,date) %>%
  mutate(
    country_clr = ifelse(Country_Region=='US',ctry_colors[1],ifelse(Country_Region=='Mexico',ctry_colors[2],ctry_colors[3])),
  ) 

plot_timeseries3 <- function(temp, variable){
  if (variable == 'case'){
    variable_lab = 'number of cases'
    value = temp$cases_sm
    fignum = '1_'
  } else if (variable == 'death'){
    variable_lab = 'number of deaths'
    value = temp$deaths_sm
    fignum = '2_'
  } else if (variable == 'caserate'){
    variable_lab = 'Case rate (per 100,000)'
    value = temp$case_r
    fignum = '1_'
    this_ylim = 84.5
  } else{
    variable_lab = 'Death rate (per 100,000)'
    value = temp$death_r
    fignum = '2_'
    this_ylim= 1.5
  }
  
  temp_plot <- 
    ggplot(data = temp,aes(x = date, y = value*100000, color=Country_Region)) + 
    geom_line( size=1) + 
    scale_x_date(breaks = c(seq(from=as.Date("2020-03-01"), to=as.Date("2021-05-03"),by="1 month")),  date_labels = "%D") + 
    ylab(variable_lab)+
    xlab("Date")+
    ylim(c(0,this_ylim))+
    theme_bw()+
    theme(
      # backgrounds
      panel.background = element_blank(), axis.line=element_line(colour = "black"), legend.key = element_blank(),
      # title
      legend.title=element_blank(),
      legend.text=element_text(size = 12),
      legend.position = 'bottom',#c(0.1,0.8),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12, angle = 60, hjust = 1), #margin = margin(t=5)),
      axis.text.y = element_text(size = 12))
    scale_y_continuous(limits=c(0,NA))
  #    scale_color_manual(values=c(ctry_colors[1],ctry_colors[2],ctry_colors[3]))
  
#  ggsave(paste0(fig_path, fignum, "all_",variable,"_7day_avg2.pdf"), width = 15, height = 11)
    return(temp_plot)
}
timeseries_case <- plot_timeseries3(covid_all3,'caserate')
timeseries_death <- plot_timeseries3(covid_all3,'deathrate')
ggarrange(timeseries_case,timeseries_death, labels=c("A","B"),nrow=2, 
                    font.label = list(size=14),
                    legend = 'bottom', common.legend=TRUE,
                    heights=15, widths=11)
ggsave(paste0(fig_path,"1_2_all_rate_7day_avg2(combined).pdf"),height=10, width=8)

# scatter plot of geo-distance vs cumulative case/death rates
# calculate difference in cumulative 
cum_us_temp <- cum_us
n_us_states <- length(cum_us$state_ab)
df_cumcase <- matrix(rep(0,n_us_states*n_us_states),nrow=n_us_states)
df_cumdeath <- matrix(rep(0,n_us_states*n_us_states),nrow=n_us_states)
for (k in c(1:n_us_states)){
  df_cumcase[,k] = abs(cum_us_temp$cumcases_sm_rate - cum_us_temp$cumcases_sm_rate[k])
  df_cumdeath[,k] = abs(cum_us_temp$cumdeath_sm_rate - cum_us_temp$cumdeath_sm_rate[k])
}
rownames(df_cumcase) <- cum_us$state_ab
rownames(df_cumdeath) <- cum_us$state_ab
colnames(df_cumcase) <- cum_us$state_ab
colnames(df_cumdeath) <- cum_us$state_ab
# prepare long-form data
cumcase_lg <- prep_df_long(df_cumcase,'latitude','US')
cumdeath_lg <- prep_df_long(df_cumdeath,'latitude','US')
# combine with other distance measures
cumcomb_lg <- cumcase_lg %>% 
  rename( cumcasedist = distance) %>%
  left_join(cumdeath_lg, by=c("state1","state2")) %>%
  rename(cumdeathdist = distance)%>%
  filter((state1 != state2) & (as.character(state1) < as.character(state2)))%>%  # remove selfpair and remove duplicated pairs
  left_join(comb_lg2, by=c("state1","state2"))
# plot
ggscatter(cumcomb_lg, x="geodist", y="cumdeathdist", add="reg.line", alpha=0.5)+
  ylab("Cumulative death rate distance")+
  xlab("Geographical distance (mi)")+
  stat_cor(label.x=min(cumcomb_lg$geodist), label.y = max(cumcomb_lg$cumdeathdist)-10, size=5)+
  stat_regline_equation(label.x=min(cumcomb_lg$geodist), label.y=max(cumcomb_lg$cumdeathdist), size=5)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
ggsave(paste0(fig_path, "4_", "reg_cumdeath_geodist.pdf"), width = 15, height=9)  

state_acrn <- covid_all %>%
  select(Country_Region, state, state_ab) %>%
  distinct()
write.csv(state_acrn,paste0(fig_path, "state_acronym.csv"))

temp <- data.frame(state =unique(covid_us$state_ab) , color = V(net_case_us)$color)
write.csv(temp,paste0(fig_path, "state_group_color.csv"))
