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

# Daily covid case data by state
covid_st <- read.csv("Data/covid_by_state_cln.csv")
# A function to generate network
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

# A function to generate network figures by phase (for case)
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

