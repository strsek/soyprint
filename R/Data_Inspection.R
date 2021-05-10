# This script generates some exploartive graphs to inspect the distribution of soy production, exports and processing across municipalities 

library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

SOY_MUN <- readRDS("intermediate_data/SOY_MUN_00.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_00.rds")
#options(scipen = 0) 

## scatter plot (x=production, y=exports)

ggplot(SOY_MUN, aes(x=prod_bean, y=exp_bean)) +
  geom_point(size=1, aes(color = nm_state))+
  scale_x_continuous(labels = scales::scientific) # +
  #geomext_repel(aes(label = co_mun), size = 2.5, hjust = 0 ,vjust = -1, color = "black", position = position_jitter(width = 0.2, height = 0, seed = 111))


# scatterotal <- ggplot(SOY_MUN, aes(x=prod_bean, y=exp_bean, label_bean = exp_bean, label_oil = exp_oil, label_cake = exp_cake)) +
#   geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
#   scale_x_continuous(labels = scales::scientific)
# ggplotly(scatterotal)

scatter_bean <- ggplot(SOY_MUN, aes(x=prod_bean, y=exp_bean)) +
geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_bean)

scatter_oil <- ggplot(SOY_MUN, aes(x=prod_bean, y=exp_oil)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_oil)

scatter_cake <- ggplot(SOY_MUN, aes(x=prod_bean, y=exp_cake)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_cake)

scatter_beanoil <- ggplot(SOY_MUN, aes(x=exp_bean, y=exp_oil)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_beanoil)

scatter_oilcake <- ggplot(SOY_MUN, aes(x=exp_oil, y=exp_cake)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_oilcake)


### barcharts

# for production

#bar_prod <- SOY_MUN %>% drop_na(prod_bean) %>% arrange(desc(prod_bean)) %>%
#  ggplot(aes(x = co_mun, y = prod_bean)) +
#  geom_bar(stat="identity", fill = 'red')+
#  coord_flip()
#ggplotly(bar_prod)

SOY_MUN %>% filter(prod_bean > 0)  %>%
  plot_ly(y= ~reorder(co_mun, prod_bean), x= ~prod_bean, type='bar', text = ~paste(nm_mun, ",", nm_state)) %>%
  layout(title = "total soy production in tons", xaxis = list(title = 'production in t'), yaxis = list(title = 'municipality code'))

# number of MUs with positive production value
SOY_MUN %>% filter(prod_bean > 0)  %>% count

# for exports
SOY_MUN %>% filter(exp_bean > 0)  %>%
  plot_ly(y= ~reorder(co_mun, exp_bean+exp_oil + exp_cake), x= ~exp_bean, type='bar', name = "bean", text = ~paste(nm_mun, ",", nm_state)) %>%
  add_trace(x = ~exp_oil, name = 'oil') %>%
  add_trace(x = ~exp_cake, name = 'cake') %>%
  layout(title ="soy exports by product type in tons", xaxis = list(title = 'exports in t'), yaxis = list(title = 'municipality code'), barmode = 'stack')

# number of MUs with positive export value
SOY_MUN %>% filter(exp_bean > 0)  %>% count
SOY_MUN %>% filter(exp_bean > 0)  %>% count
SOY_MUN %>% filter(exp_oil > 0)  %>% count
SOY_MUN %>% filter(exp_cake > 0)  %>% count


# for processing capacity
SOY_MUN %>% filter(proc_cap> 0)  %>%
  plot_ly(y= ~reorder(co_mun, proc_cap), x= ~proc_cap, type='bar', name = "processing", text = ~paste(nm_mun, ",", nm_state, '<br># processing facilities:', proc_fac, '<br># refining facilities:', ref_fac)) %>%
  add_trace(x = ~ref_cap, name = 'refining') %>%
  add_trace(x = ~bot_cap, name = 'bottling') %>%
  layout(title = "processing capacity by type", xaxis = list(title = 'capacity in tons'), yaxis = list(title = 'municipality code'), barmode = 'stack')

