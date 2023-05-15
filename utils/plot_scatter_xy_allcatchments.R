
rm(list = ls())
library(data.table) 
library(ggplot2)
library(dplyr)
library(forcats)
library(glue)

months_emission = c("1˚may","1˚jul", "1˚sep")

for (month_emission in months_emission) {


predictores = fread(input = "data_output/predictores/all_predictor_1981_2019_45cuencas_normalised.csv") %>% 
  subset(month_wy %in% month_emission) %>% 
  subset(var %in% c("NINO1.2","SOI","STORAGE")) %>% 
  mutate(rank = min_rank(catchment_code),  # rank the catchment codes
         block = as_factor(ntile(rank, 3)),
         var = recode(var, "NINO1.2" = "NIÑO1.2", "SOI" = "SOI", "STORAGE" = "C.H. INICIAL")) # divide the ranks into 3 equal-sized groups

plots = list()
for (block_i in as.integer(unique(predictores$block))) {
  
p = ggplot(data = predictores %>% filter( block == block_i)) +
  geom_point(aes(x = predictor_value, y = volume))+
  facet_grid(catchment_code~var,scales = "free_x")+
  geom_smooth(aes(x = predictor_value, y = volume), method = "lm", se = FALSE, color = "red") +
  labs(x = "valor del predictor normalizado []",
       y = "volumen estacional sep-mar normalizado [m3/m3]",
       caption = paste("fecha de emisión:", month_emission))

plots[[block_i]] = p

}

combined_plot = gridExtra::grid.arrange(grobs = plots, ncol = 3)
# Save the combined figure as a PNG file
ggsave(filename = glue("data_output/figuras/scatter_xy/scatter_xy_all_catchments_{month_emission}.png"),
       plot = combined_plot, height = 10, width = 9)
}
