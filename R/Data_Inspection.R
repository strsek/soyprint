# This script generates some exploartive graphs to inspect the distribution of soy production, exports and processing across municipalities 

library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

load("SOY_MUN.RData")
load("GEO_MUN_SOY.Rdata")
#options(scipen = 0) 

## scatter plot (x=production, y=exports)

ggplot(SOY_MUN, aes(x=prod_t, y=ex_tot_t)) +
  geom_point(size=1, aes(color = nm_state))+
  scale_x_continuous(labels = scales::scientific) # +
  #geom_text_repel(aes(label = co_mun), size = 2.5, hjust = 0 ,vjust = -1, color = "black", position = position_jitter(width = 0.2, height = 0, seed = 111))

# same with setting NAs to zero
SOY_MUN_0 <- SOY_MUN
SOY_MUN_0[is.na(SOY_MUN_0)] <- 0

scatter_total <- ggplot(SOY_MUN_0, aes(x=prod_t, y=ex_tot_t, label_bean = ex_bean_t, label_oil = ex_oil_t, label_cake = ex_cake_t)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_total)

scatter_bean <- ggplot(SOY_MUN_0, aes(x=prod_t, y=ex_bean_t)) +
geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_bean)

scatter_oil <- ggplot(SOY_MUN_0, aes(x=prod_t, y=ex_oil_t)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_oil)

scatter_cake <- ggplot(SOY_MUN_0, aes(x=prod_t, y=ex_cake_t)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_cake)

scatter_beanoil <- ggplot(SOY_MUN_0, aes(x=ex_bean_t, y=ex_oil_t)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_beanoil)

scatter_oilcake <- ggplot(SOY_MUN_0, aes(x=ex_oil_t, y=ex_cake_t)) +
  geom_point(size=1, aes(color = nm_state, label1 = co_mun, label2 = nm_mun)) +
  scale_x_continuous(labels = scales::scientific)
ggplotly(scatter_oilcake)


### barcharts

# for production

#bar_prod <- SOY_MUN %>% drop_na(prod_t) %>% arrange(desc(prod_t)) %>%
#  ggplot(aes(x = co_mun, y = prod_t)) +
#  geom_bar(stat="identity", fill = 'red')+
#  coord_flip()
#ggplotly(bar_prod)

SOY_MUN_0 %>% filter(prod_t > 0)  %>%
  plot_ly(y= ~reorder(co_mun, prod_t), x= ~prod_t, type='bar', text = ~paste(nm_mun, ",", nm_state)) %>%
  layout(title = "total soy production in tons", xaxis = list(title = 'production in t'), yaxis = list(title = 'municipality code'))

# number of MUs with positive production value
SOY_MUN_0 %>% filter(prod_t > 0)  %>% count

# for exports
SOY_MUN_0 %>% filter(ex_tot_t > 0)  %>%
  plot_ly(y= ~reorder(co_mun, ex_tot_t), x= ~ex_bean_t, type='bar', name = "bean", text = ~paste(nm_mun, ",", nm_state)) %>%
  add_trace(x = ~ex_oil_t, name = 'oil') %>%
  add_trace(x = ~ex_cake_t, name = 'cake') %>%
  layout(title ="soy exports by product type in tons", xaxis = list(title = 'exports in t'), yaxis = list(title = 'municipality code'), barmode = 'stack')

# number of MUs with positive export value
SOY_MUN_0 %>% filter(ex_tot_t > 0)  %>% count
SOY_MUN_0 %>% filter(ex_bean_t > 0)  %>% count
SOY_MUN_0 %>% filter(ex_oil_t > 0)  %>% count
SOY_MUN_0 %>% filter(ex_cake_t > 0)  %>% count

# for processing facilities 
proc <- SOY_MUN_0 %>% filter(proc_total > 0)  %>%
  plot_ly(y= ~reorder(co_mun, proc_total), x= ~proc_act, type='bar', name = "processing active", text = ~paste(nm_mun, ",", nm_state)) %>%
  add_trace(x = ~proc_inact, name = 'processing inactive') %>%
  layout(title ="number of processing facilities by status", xaxis = list(title = 'number of processing facilites',dtick=1), yaxis = list(title = 'municipality code'), barmode = 'stack')

ref <- SOY_MUN_0 %>% filter(ref_total > 0)  %>%
  plot_ly(y= ~reorder(co_mun, proc_total), x = ~ref_act, type='bar', name = "refining/bottling active", text = ~paste(nm_mun, ",", nm_state)) %>%
  add_trace(x = ~ref_inact, name = 'refining/bottling inactive') %>%
  layout(title = "number of refining/bottling facilites by status", xaxis = list(title = 'number of refining/bottling facilites',dtick=1), yaxis = list(title = 'municipality code'), barmode = 'stack')

subplot(proc,ref,shareY = T, shareX = T) 

# for processing capacity
SOY_MUN_0 %>% filter(proc_total > 0)  %>%
  plot_ly(y= ~reorder(co_mun, proc_cap), x= ~proc_cap, type='bar', name = "processing", text = ~paste(nm_mun, ",", nm_state, '<br># processing facilities:', proc_act,"/",proc_inact, '<br># refining facilities:', ref_act,"/", ref_inact)) %>%
  add_trace(x = ~ref_cap, name = 'refining') %>%
  add_trace(x = ~bot_cap, name = 'bottling') %>%
  layout(title = "processing capacity by type", xaxis = list(title = 'capacity in tons'), yaxis = list(title = 'municipality code'), barmode = 'stack')

