library(tidyverse)
library(ggforce)
library(ggtext)
library(camcorder)
library(showtext)

# Load fonts 
font_add_google("Barlow Condensed","barlow")
font_add_google("Oswald","oswald")
font_add_google("Archivo Black","archivo")
font_add_google("Roboto Condensed","roboto")

# Automatically use {showtext} for plots
showtext_auto()

# Set fonts
ft_coun <- "oswald"

# Set colors
col_straw <- "#586f7c"
col_tit<-"#FFFFEB"

# Green to orange
pal<-c(
  '#749E35',
  '#8ABC40',
  '#F2E318',
  '#FFD400',
  '#FF9D00'
)

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*1.618, 
  units = "cm", 
  dpi = 300 
)

# Load data
data<-read_csv('https://raw.githubusercontent.com/BjnNowak/CultivatedPlanet/main/data/FAO_CropsProductivity_subset.csv')

# Prepare data
wheat<-data%>%
  filter(Item=="Wheat")%>%
  filter(
    Area=="France"|
    Area=="Germany"|
    Area=="Russian Federation"|
    Area=="United States of America"|
    Area=="India"|
    Area=="China"|
    Area=="Australia"|
    Area=="Canada"|
    Area=="Pakistan"|
    Area=="Ukraine"
  )%>%
  filter(Element=="Yield")%>%
  filter(
    Year>2008&Year<2019
  )%>%
  mutate(Area=case_when(
    Area=="United States of America"~"USA",
    Area=="Russian Federation"~"Russia",
    TRUE~Area
  ))%>%
  group_by(Area)%>%
  mutate(
    mean_period = mean(Value)/10000
  )%>%
  ungroup()%>%
  mutate(ratio=((Value/10000)/mean_period)*100)

wheat<-wheat%>%
  group_by(Area)%>%
  mutate(
    max_yield = max(Value)
  )%>%
  ungroup()

# Make ears
x_offset <- 0.11
ratio_xy_seed <- 3
max_y <- 10

hyp <- x_offset/0.25

wheat_ear<-wheat%>%
  mutate(Area=fct_reorder(Area,-mean_period))%>%
  mutate(xpos=as.numeric(as.factor(Area)))%>%
  
  mutate(xpos=case_when(
    xpos>5~xpos-4.5,
    TRUE~xpos
  ))%>%
  
  mutate(ratio_yx=max_y/max(xpos))%>%
  mutate(x_offset_relative=case_when(
    Year %% 2 == 0 ~ -x_offset,
    TRUE ~ x_offset
  ))%>%
  mutate(angle=case_when(
    Year %% 2 == 0 ~ pi/4,
    TRUE ~ -pi/4
  ))%>%
  #filter(Year==2011)%>%
  mutate(
    x = xpos-x_offset_relative,
    y = mean_period+(Year-2009)/10,
    # Radius on x-axis
    #a = Value*(x_offset/max_yield),
    a = x_offset,
    # Radius on y-axis
    b= (a/ratio_xy_seed)*ratio_yx
  )%>%
  mutate(ratio=case_when(
    ratio<85~85,
    ratio>115~115,
    TRUE~ratio
  ))


# Prepare annotations
curve_year1<-tibble(
  x=0.6,
  xend=0.8,
  y=7.1,
  yend=7.5,
  curv=-0.25,
  lab='2009'
)

curve_year2<-tibble(
  x=1.6,
  xend=1.2,
  y=8.6,
  yend=8.5,
  lab='2018'
)

curve_year3<-tibble(
  x=1.4,
  xend=1.2,
  y=8.2,
  yend=8.6,
  lab='2018'
)

curve_ch<-tibble(
  x=3.5,
  xend=3.2,
  y=6.5,
  yend=6.2,
  lab=
'China is the biggest\n
wheat producer,\n
followed by India'
)

curve_uk<-tibble(
  x=4.4,
  xend=4.1,
  y=4.9,
  yend=4.6,
  lab=
'As in China, yield\n
increases steadily\n
in Ukraine'
)

mean_fr <- wheat_ear%>%
  filter(Area=="France")%>%
  head(1)%>%
  pull(mean_period)

mean_au <- wheat_ear%>%
  filter(Area=="Australia")%>%
  head(1)%>%
  pull(mean_period)

mean_us <- wheat_ear%>%
  filter(Area=="USA")%>%
  head(1)%>%
  pull(mean_period)

mean_ch <- wheat_ear%>%
  filter(Area=="China")%>%
  head(1)%>%
  pull(mean_period)

mean_ru <- wheat_ear%>%
  filter(Area=="Russia")%>%
  head(1)%>%
  pull(mean_period)

mean_in <- wheat_ear%>%
  filter(Area=="India")%>%
  head(1)%>%
  pull(mean_period)

mean_pa <- wheat_ear%>%
  filter(Area=="Pakistan")%>%
  head(1)%>%
  pull(mean_period)

cap <- tibble(
  x=0.6,y=-2.8,label="**Data:** FAO Stats  **| Plot:** @BjnNowak"
)

# Make plot
ggplot(data=wheat_ear)+
  
  # Background
  # Soil
  annotate(
    geom = "rect",
    xmin=0.4,xmax=6,ymin=-3,ymax=0,
    fill="#562c2c")+
  # Air
  annotate(
    geom = "rect",
    xmin=0.4,xmax=6,ymin=0,ymax=9.5,
    fill="lightblue")+
  
  # Stem
  geom_segment(
    aes(x=xpos,xend=xpos,y=0,yend=mean_period),
    color=col_straw)+
  # Country names
  geom_text(
    aes(x=xpos-0.15,y=0.1,label=Area),
    hjust=0,angle=90,family=ft_coun,size=12,
    color=col_straw
  )+
  # Ear
  geom_ellipse(
    data=wheat_ear,
    aes(x0=x,y0=y,a=a,b=b,angle=angle,fill=ratio),
    color=col_straw,size=0.05
  )+
  
  # Annotations
  # Year 2009
  geom_curve(
    data = curve_year1,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = -0.25,size=.35,
    arrow = arrow(length = unit(0.02, "npc")),color="black"
  )+
  geom_text(
    data= curve_year1,
    aes(x=x+0.1,y=y-0.1,label=lab),hjust=0.5,vjust=1,
    lineheight=0.15,size=10,family='barlow',color="black",
    fontface='italic'
  )+
  # Year 2018
  geom_curve(
    data = curve_year3,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.25,size=.35,
    arrow = arrow(length = unit(0.02, "npc"))
  )+
  geom_text(
    data= curve_year3,
    aes(x=x+0.1,y=y-0.1,label=lab),hjust=0.5,vjust=1,
    lineheight=0.15,size=10,family='barlow',
    fontface='italic'
  )+
  geom_curve(
    data = curve_ch,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.25,
    arrow = arrow(length = unit(0.02, "npc"))
  )+
  geom_text(
    data= curve_ch,
    aes(x=x+0.1,y=y,label=lab),hjust=0,vjust=0.5,
    lineheight=0.15,size=10,family='barlow'
  )+
  geom_curve(
    data = curve_uk,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.25,
    arrow = arrow(length = unit(0.02, "npc"))
  )+
  geom_text(
    data= curve_uk,
    aes(x=x+0.1,y=y,label=lab),hjust=0,vjust=0.5,
    lineheight=0.15,size=10,family='barlow'
  )+
  annotate(
    geom = "segment",
    x=2,xend=2.3,y=mean_fr,yend=mean_fr,
    lty="dotted"
  )+
  annotate(
    geom = "text",
    x=2.31,y=mean_fr,label=paste("Mean yield\nper ha:\n7.0 tons"),
    family='barlow',size=10,hjust=0,vjust=0.5,color="black",lineheight=0.3
  )+
  annotate(
    geom = "text",
    x=1.54,y=mean_us-0.2,label=paste(round(mean_us,1),"t"),
    family='barlow',size=10,hjust=0,vjust=0.5,color="black",lineheight=0.3
  )+
  annotate(
    geom = "text",
    x=5.54,y=mean_au-0.2,label=paste("2.0 t"),
    family='barlow',size=10,hjust=0,vjust=0.5,color="black",lineheight=0.3
  )+
  annotate(
    geom = "text",
    x=3.04,y=mean_ch-0.2,label=paste(round(mean_ch,1),"t"),
    family='barlow',size=10,hjust=0,vjust=0.5,color="black",lineheight=0.3
  )+
  annotate(
    geom = "text",
    x=4.54,y=mean_ru-0.2,label=paste(round(mean_ru,1),"t"),
    family='barlow',size=10,hjust=0,vjust=0.5,color="black",lineheight=0.3
  )+
  annotate(
    geom = "text",
    x=3.54,y=mean_pa-0.2,label=paste(round(mean_pa,1),"t"),
    family='barlow',size=10,hjust=0,vjust=0.5,color="black",lineheight=0.3
  )+

  # Titles
  annotate(
    geom = "text",
    x=0.6,y=-0.4,label="Wheat yield for top 10 producers",
    family='archivo',size=14,hjust=0,vjust=1,color=col_tit
  )+
  annotate(
    geom = "text",
    x=0.6,y=-0.9,label=
"This graph shows wheat yields from 2009 to 2018. Size of the 
stems shows the mean yield for the period, while color of each 
grain shows the relative yield for each year (going from 2009 to 
2018 from the bottom to the top of the ear). Despite low yields,
some countries are in the top 10 due to large cultivated areas.",
    family='roboto',size=10,hjust=0,vjust=1,color=col_tit,lineheight=0.3
  )+
  geom_richtext(
    data=cap,
    aes(x=x, y=y, label = label),color=col_tit,
    hjust=0,vjust=0,size=9,family='roboto',lineheight=0.4,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  
  # Legend
  labs(
    fill='**Relative annual yield**<br>(compared to mean from 2009 to 2018)'
  )+
  guides(
    fill=guide_colourbar(
      title.position="top", title.hjust = 0.5,
      barwidth = 9,barheight = 0.5, 
      draw.ulim = F,draw.llim = F,
      frame.colour=col_tit,ticks.colour=col_tit
  ))+
  # Set scales
  scale_y_continuous(limits = c(-3,9.5))+
  scale_x_continuous(limits = c(0.4,6))+
  scale_fill_stepsn(
    colours=pal,
    breaks=seq(85, 115, by = 5),
    labels=c("","90%<","95%","100%","105%",">110%","")
  )+
  # Set theme
  theme_void()+
  theme(
    legend.position = c(0.675, 0.875),
    legend.direction="horizontal",
    legend.title =element_markdown(family = 'roboto',size=25,lineheight = 0.45, margin = margin(0,0,0,0)),
    legend.text = element_text(family='roboto',size=20),
    legend.spacing.y = unit(0.1, 'cm'),
    plot.background = element_rect(fill="white",color=NA),
    plot.margin = margin(0.1,0.1,0.1,0.1)
  )


