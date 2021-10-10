## This code is to perform network analysis based on the similarity of COVID-19 temporal trends between states.
## Date created: 10.09.2021
## Writer: Kyueun Lee

# required libraries
library(igraph)
library(ggpubr)


# A function to assign color for network nodes
gen_2Dcolor <- function(data){
  geo_all_color <- data %>%
    select(state,state_ab,lon,lat) %>%
    distinct() %>%
    mutate(red_idx = 0,
           blue_idx = 0.6,
           green_idx = 0,
           rgb = rgb(red=red_idx,green=green_idx,blue=blue_idx))%>%
    arrange(state_ab)
  
  return(geo_all_color)
}
# A function to generate network projection
gen_net <- function(ncnt, data, period){
  # select data for a specific phase
  if (period == 'wave'){
    print("wave")
    temp <- data %>%
      filter(phase <= ncnt) %>%
      arrange(state_ab)
    print(nrow(temp))
  }else{
    temp <- data %>%
      arrange(state_ab)
  }
  geo_loc = geo_all_color

  # state/province list included in this dataset
  state_list = unique(temp$state_ab[order(temp$state_ab)])
  # matrix
  out <- reshape2::dcast(temp, date~state_ab, value.var = "case_rate_sm", fill=0, fun.aggregate=sum)
  # negative correlation = no correlation
  out[out<0] <- 0
  # calculate correlation
  corout = as.matrix(cor(out[,-c(1)], method = "pearson"))
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
gen_net_fig <- function(n_phase, covid_data, period, filename){
  pdf(paste0(fig_path,filename),width=15, height=15)
    net_outcome <- gen_net(n_phase, covid_data, period )
    this_net <- net_outcome[[1]]
    this_net_ly <- net_outcome[[2]]
    if (period == 'wave'){
      this_phase = paste0("Up to Wave ",n_phase)
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
  dev.off()
}


## Main
# Figure pathway
fig_path <-"/Users/kyulee/Google Drive/Z Drive/Project_all/2021_Covid_safegraph/NetworkCOVID_US/Figure/"
# Import state-level covid data
covid_all <- read.csv("Data/covid_by_state_cln.csv")
# generate color scheme for network analysis
geo_all_color <-gen_2Dcolor(covid_all)
netfig1<-gen_net_fig(1,  covid_all, 'wave', "10_2Dnet_caserate_US_updated(phase1).pdf")
netfig2<-gen_net_fig(2,  covid_all, 'wave', "10_2Dnet_caserate_US_updated(phase1-2).pdf")
netfig3<-gen_net_fig(3,  covid_all, 'wave', "10_2Dnet_caserate_US_updated(phase1-3).pdf")
netfig4<-gen_net_fig(4,  covid_all, 'wave', "10_2Dnet_caserate_US_updated(phase1-4).pdf")
netfig5<-gen_net_fig(5,  covid_all, 'wave', "10_2Dnet_caserate_US_updated(phase1-5).pdf")

