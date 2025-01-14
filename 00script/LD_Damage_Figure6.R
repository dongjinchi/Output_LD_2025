setwd("E:\\06博士论文\\002出国交流\\02cliamte_economy\\output_ld\\00script")
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
tem<-read_csv("..\\01data\\gdppc_k_tems_126_2015-2100.csv")
boots<-read_csv("..\\01data\\Bootstrap.csv")
region<-read_csv("..\\01data\\region.csv")
pop<-read_csv("..\\01data\\scenario_ssp1_pop_2015-2100.csv")
pop<-pop[,c(1:3)]
gdppc<-read_csv("..\\01data\\scenario_ssp1_gdppc_2015-2100.csv")
gdppc<-gdppc[,c(1,2,4)]

tem<-tidyr::gather(tem,model,tem, -c(region, year))

tem_loss<-tem %>%
  filter(model=="tem3" & year >=2020)
tem_loss<-tem_loss[,c(1,2)]
tem_cols <- grep("^tem", unique(tem$model), value = TRUE)
for (i in c(1:1000)) {
selc_tem <- sample(tem_cols, 1)
tem_sub<-filter(tem, model==selc_tem)

selc_boot <- boots[i, ]

#alpha = selc_boot$T_panel
#beta = selc_boot$T2_panel

alpha = selc_boot$T_ld
beta = selc_boot$T2_ld

tem_sub <-tem_sub %>%
  group_by(region) %>%
  mutate(T0=mean(tem[year >= 2015 & year <= 2019],na.rm=T)) %>%
  mutate(growth = (alpha*tem + beta*tem^2)-(alpha*T0 + beta*T0^2))
tem_sub$growth<-(tem_sub$growth+1)^(1/10)-1
tem1 <-tem_sub %>%
  filter( year %in% c(2020:2100)) %>%
  group_by(region) %>%
  mutate(damage = cumsum(growth))
tem1<-tem1[,c(1,2,7)]
tem1$damage[tem1$damage> 1]<- 1
tem1$damage[tem1$damage< -1]<- -1
colnames(tem1)[3]<-paste("damage",i,sep="")

tem_loss<-left_join(tem_loss,tem1, by=c("region","year"))

print(i)

}

save(tem_loss, file = "..\\01data\\bootstrap_ld_loss_1000.RData")
load("..\\01data\\bootstrap_ld_loss_1000.RData")



####unweighted results####
loss_year<-tem_loss %>%
  group_by(year) %>%
  summarise_all(mean,na.rm=T)
loss_year<-loss_year[,-2]
loss_year<-tidyr::gather(loss_year,damage,value, -year)
####population weighted results####
loss_year_pop <- tem_loss %>%
  left_join(pop,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, damage) %>%
  summarise(value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 
###GDPpc weighted results####
loss_year_gdp <- tem_loss %>%
  left_join(gdppc,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, damage) %>%
  summarise(value_gdp = sum(value * gdppc, na.rm = TRUE) / sum(gdppc, na.rm = TRUE)) 

loss_year<- loss_year %>%
  left_join(loss_year_pop,by=c("year","damage")) %>%
  left_join(loss_year_gdp,by=c("year","damage"))

loss_summary <- loss_year %>%
  group_by(year) %>%
  summarise(
    mean = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q10 = quantile(value, 0.10, na.rm = TRUE),
    q90 = quantile(value, 0.90, na.rm = TRUE),
    
    mean_pop = median(value_pop, na.rm = TRUE),
    sd_pop = sd(value_pop, na.rm = TRUE),
    q10_pop = quantile(value_pop, 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop, 0.90, na.rm = TRUE),
    
    mean_gdp = median(value_gdp, na.rm = TRUE),
    sd_gdp = sd(value_gdp, na.rm = TRUE),
    q10_gdp = quantile(value_gdp, 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp, 0.90, na.rm = TRUE)
  )
write_excel_csv(loss_summary,file = "..\\01data\\loss_ld_summary.csv")

for (i in c(1:3)) {
  l<-4*i-2
  h<-4*i-2+3
  loss_sub<-loss_summary[,c(1,l:h)]
  colnames(loss_sub)[2]<-"mean"
  colnames(loss_sub)[4]<-"q10"
  colnames(loss_sub)[5]<-"q90"
p<-ggplot(data = loss_sub) +
  # 绘制95%置信区间
  geom_ribbon(aes(x = year, ymin =q10, ymax =q90), 
              fill = rgb(252/255, 141/255, 98/255), alpha = 0.3) +
  # 绘制平均值的红色折线
  geom_line(aes(x = year, y = mean), 
            color = rgb(252/255, 141/255, 98/255), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_cartesian(ylim = c(-0.8, 0.4)) +
  scale_y_continuous(breaks = seq(-0.8, 0.4, 0.4)) +
  theme_classic() +
  labs(x = "", y = "Percentage change in GDP per capita") +
  theme(axis.title.y = element_text(size = 8))
print(p)
if(i==1){
  a<-p
}else if (i==3){
  b<-p
}else{
  c<-p
}
}

######
loss_region<-filter(tem_loss,year==2100)
loss_region<-loss_region[,-2]
loss_region<-tidyr::gather(loss_region,damage,value, -region)
loss_region<-loss_region %>%
  group_by(region) %>%
  summarise(mean=mean(value,na.rm=T))

library(sp)
library(sf)
area_v<-st_read("F:\\01数据库\\社会经济数据库\\01GDP数据\\gridgdp_Kummu\\world_area\\world_area.shp", 
                fid_column_name = 'FID')
area_v<-sf::st_transform(area_v,crs="+proj=longlat +datum=WGS84")

map<-left_join(area_v,loss_region, by=c("lyr_1"="region"))

library(ggplot2)
library(RColorBrewer)
a<-ggplot(data = map) +
  geom_sf(aes(fill = mean)) +  # 使用 fill 映射 loss_avg 列
  scale_fill_distiller(palette = "RdYlBu",
                       direction=1,
                       limits = c(-1, 1),                # 设置填充颜色的范围
                       oob = scales::squish,
                       name = "Percentage Change in GDP per capita" )+
  theme_minimal() +
  labs(fill="")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),# 移除主要网格线
        axis.text = element_blank(),
        legend.title = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = unit(20, "lines"),  # 设置图例的宽度
                               barheight = unit(0.5, "lines"),
                               title.position = "top",             # 标题在图例的上部
                               title.hjust = 0.5))+ 
  coord_sf(crs = "+proj=robin") # 调整图例的高度)
print(a)

library(patchwork)
library(cowplot)
combined_plot <- (a | b) 
final_plot <- combined_plot + 
  plot_annotation(tag_levels = 'a')  # 自动标注 a, b, c
print(final_plot)

ggsave(
  filename = "..\\03figure\\Figure6_2.pdf")
ggsave(
  filename = "..\\03figure\\Figure6_2.jpg",
  dpi = 500,                         # 设置分辨率为300像素
  width = 10,                        # 设置图像宽度（单位：英寸）
  height = 3                        # 设置图像高度（单位：英寸）
)


####unweighted results####
loss_year<-tem_loss %>%
  left_join(region,by=c("region")) %>%
  group_by(year,metacontinent) %>%
  summarise_all(mean,na.rm=T)
loss_year<-loss_year[,-3]
loss_year<-tidyr::gather(loss_year,damage,value, -c(metacontinent,year))
####population weighted results####
loss_year_pop <- tem_loss %>%
  left_join(region,by=c("region")) %>%
  left_join(pop,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, metacontinent,damage) %>%
  summarise(value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 
###GDPpc weighted results####
loss_year_gdp <- tem_loss %>%
  left_join(region,by=c("region")) %>%
  left_join(gdppc,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, metacontinent,damage) %>%
  summarise(value_gdp = sum(value * gdppc, na.rm = TRUE) / sum(gdppc, na.rm = TRUE)) 

loss_year<- loss_year %>%
  left_join(loss_year_pop,by=c("year","metacontinent","damage")) %>%
  left_join(loss_year_gdp,by=c("year","metacontinent","damage"))

loss_summary <- loss_year %>%
  group_by(year,metacontinent) %>%
  summarise(
    mean = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q10 = quantile(value, 0.10, na.rm = TRUE),
    q90 = quantile(value, 0.90, na.rm = TRUE),
    
    mean_pop = median(value_pop, na.rm = TRUE),
    sd_pop = sd(value_pop, na.rm = TRUE),
    q10_pop = quantile(value_pop, 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop, 0.90, na.rm = TRUE),
    
    mean_gdp = median(value_gdp, na.rm = TRUE),
    sd_gdp = sd(value_gdp, na.rm = TRUE),
    q10_gdp = quantile(value_gdp, 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp, 0.90, na.rm = TRUE)
  )
write_excel_csv(loss_summary,file = "..\\01data\\loss_ld_summary_subcont.csv")

loss_summary<-read_csv("..\\01data\\loss_panel_summary_subcont.csv")
continent<-unique(loss_summary$metacontinent)
plot<-list()
library(scales)
for (i in continent) {
  loss_sub<-filter(loss_summary, metacontinent ==i)
  p<-ggplot(data = loss_sub) +
    # 绘制95%置信区间
    geom_ribbon(aes(x = year, ymin =q10_gdp, ymax =q90_gdp), 
                fill = rgb(252/255, 141/255, 98/255), alpha = 0.3) +
    # 绘制平均值的红色折线
    geom_line(aes(x = year, y = mean_gdp), 
              color = rgb(252/255, 141/255, 98/255), linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    coord_cartesian(ylim = c(-0.90, 0.60)) +
    scale_y_continuous(breaks = seq(-0.90, 0.60, 0.30),labels = label_number(accuracy = 0.1)) +
    theme_classic() +
    labs(title = i,x = "", y = "Percentage change in GDP per capita") +
    theme(plot.title = element_text(hjust = 0.5, size = 12),axis.title.y = element_text(size = 10))
  print(p)
  plot[[i]] <- p
}
combined_plot <- (plot[[1]] | plot[[2]] | plot[[3]]) /
  (plot[[4]] | plot[[5]] | plot[[6]]) /
  (plot[[7]] | plot[[8]] | plot[[9]])
final_plot <- combined_plot + 
  plot_annotation(tag_levels = 'a')  # 自动标注 a, b, c
print(final_plot)

ggsave(
  filename = "..\\03figure\\FigureS6_2.jpg",
  dpi = 500,                         # 设置分辨率为300像素
  width = 10,                        # 设置图像宽度（单位：英寸）
  height = 10                        # 设置图像高度（单位：英寸）
)
##############
tem<-filter(tem,year %in% c(2015:2019,2100))

tem<-tem %>%
  group_by(region,model) %>%
  mutate(T0=mean(tem[year >= 2015 & year <= 2019],na.rm=T))

tem<-filter(tem, year==2100)
tem$dT <- tem$tem-tem$T0

t=2100-2019

alpha = 0.0218
beta = -0.000760

tem2 <- tem %>%
  mutate(damage = t*((alpha+2*beta*T0)*dT+beta*dT^2))
