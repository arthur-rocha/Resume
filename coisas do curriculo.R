#### Coisas do currículo

setwd("~/Currículo")

# Gráfico habilidades -----------------------------------------------------

# infos
data=data.frame(
  individual= rev(c("R","SAS","SQL","Python","Statistics","Text-mining",
                    "Data\nvisualization",
                    "Machine\nlearning","Web\nScraping")),
  group=rev(c( rep('Tools', 4), rep('Skills', 5))) ,
  value=rev(c(100,70,70,50,100,80,80,70,70))
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(group)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar_interactive(aes(x=as.factor(id), y=value, fill=group,tooltip=individual), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add more
  
  geom_segment(data=grid_data, aes(x = (max(data$id)-2), y = 80, xend = max(data$id), yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = (max(data$id)-2), y = 60, xend = max(data$id), yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = (max(data$id)-2), y = 40, xend = max(data$id), yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = (max(data$id)-2), y = 20, xend = max(data$id), yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  # annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar_interactive(aes(x=as.factor(id), y=value, fill=group,tooltip = individual), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data,
            aes(x=id, y=value+10,
                label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=1,
            size=3.5, angle= label_data$angle, inherit.aes = FALSE )+
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0), colour = "black", alpha=0.8, size=4,
            fontface="bold", inherit.aes = FALSE)

p

## Transformar em interativo

p2 <- girafe(ggobj = p )
girafe_options(p2, opts_hover(css = "fill:#00BA38;stroke:#00BA38;transform: scale(1.5, 1.5);transition: 0.3s"),
               opts_tooltip(css = "background-color:gray;color:white;font-style:oblique;padding:10px;border-radius:5px;transition: 0.5s;") )

girafe(code = print(p))

ggsave(filename = "habilidades.png",dpi=450,height = 5.5,width = 6,bg="transparent")


# Timeline ----------------------------------------------------------------
library(dplyr)
library(ggiraph)

dt_anos = data_frame(x=2015:2019,y = 0)

data_frame(x=c(2015,2016.5,2016.6,2017.6,2018.2,2018.6),
           y = c(.12,-.12,.12,-.26,.12,-.12),
           y2=c(.22,-.22,.22,-.36,.22,-.22),
           descri= c("Moura Rocha Paraffins\n2015-2016\nAdministrative advisor",
                     "Estats Consulting Jr\n2016-2017\nMarketing advisor",
                     "State University of Maringá\n2016-2018\nProbability/inference monitor",
                     "Estats Consulting Jr\n2017-2018\nPresident",
                     "Trecsson Bussines\n2018-2018\nCommercial analyst intern",
                     "H0 consulting\n2018-2019\nStatistical intern"))%>%
ggplot(aes(x,y))+
  #ggPoints(interactive=TRUE)+
  geom_segment(aes(xend = x,y =0, yend=(y)),size=1.3,col="grey60")+
  geom_hline(yintercept = 0,size=2)+
  geom_label(data=dt_anos,aes(x=x,y,label=x),fontface=2)+
  geom_label(aes(x=x,y=y2,label=descri),fill= NA,colour = "black",
             label.size = NA, fontface=2,size=3.1)+
  geom_point_interactive(aes(data_id = descri,tooltip = descri),size=5,col='#F8766D')+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())+
  ylim(c(-.4,.4))+xlim(c(2014.5,2019.5))->linha

ggsave("Linha_teste2.png",linha,dpi=450,height = 3.6,width = 7.5,bg="transparent")

linha2 <- girafe(ggobj = linha )
final = girafe_options(linha2, opts_hover(css = "fill:#00BFC4;stroke:#00BFC4;r:10pt;transition: 0.3s"),
               opts_tooltip(css = "background-color:gray;color:white;font-style:oblique;padding:10px;border-radius:5px;transition: 0.5s;"),
               opts_sizing(rescale = T,width =  1))

htmlwidgets::saveWidget( final, "test.html" )



