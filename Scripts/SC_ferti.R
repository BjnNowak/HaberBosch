library(tidyverse)
library(camcorder)
library(showtext)
library(ggtext)
library(patchwork)

# Set plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20,
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Load fonts 
font_add_google("Barlow Condensed","barlow")
font_add_google("Oswald","oswald")
font_add_google("Playfair Display","playfair")
font_add_google("Archivo Black","archivo")
font_add_google("Roboto Condensed","roboto")
font_add_google("Passion One","passion")

# Automatically use {showtext} for plots
showtext_auto()

# Set fonts
ft_nut <- "archivo"

# Set colors
col_nut <- '#548687'
wh<-"#FFFFEB"
col_exp = "#ef745c"
col_prod = "#34073d"

# Load data
ferti<-readr::read_csv('https://raw.githubusercontent.com/BjnNowak/HaberBosch/main/Data/FAO_Ferti_Full.csv')%>%
  filter(Area!="China, mainland")%>%
  mutate(Area=case_when(
    Area=="United States of America"~"USA",
    Area=="Russian Federation"~"Russia",
    TRUE~Area
  ))

use <- ferti%>%
  filter(Element=="Production")%>%
  group_by(Item,Year)%>%
  mutate(World=sum(Value))%>%
  ungroup()%>%
  mutate(ProdRat=Value/World*100)%>%
  select(Area,Item,Year,World,ProdRat)

# Exports
ex<-ferti%>%
  filter(Element=="Export Quantity")%>%
  left_join(use,by=c('Year','Area','Item'))%>%
  mutate(Ratio=Value/World*100)

# Prod
prod<-ferti%>%
  filter(Element=="Production")%>%
  left_join(use,by=c('Year','Area','Item'))%>%
  mutate(Ratio=Value/World*100)


# Nitrogen
exN<-ex%>%
  filter(Item=="Nutrient nitrogen N (total)")%>%
  filter(Year==2018)%>%
  select(Area,Export=Value,ExportRatio=Ratio)
  
prodN<-prod%>%
  filter(Item=="Nutrient nitrogen N (total)")%>%
  filter(Year==2018)%>%
  select(Area,Prod=Value,ProdRatio=Ratio)

sum(na.omit(prodN$ProdRatio))

datN <- exN%>%
  left_join(prodN,by=c('Area'))%>%
  arrange(-ProdRatio)%>%
  head(10)

# Phosphorus
exP<-ex%>%
  filter(Item=="Nutrient phosphate P2O5 (total)")%>%
  filter(Year==2018)%>%
  select(Area,Export=Value,ExportRatio=Ratio)

prodP<-prod%>%
  filter(Item=="Nutrient phosphate P2O5 (total)")%>%
  filter(Year==2018)%>%
  select(Area,Prod=Value,ProdRatio=Ratio)

datP <- exP%>%
  left_join(prodP,by=c('Area'))%>%
  arrange(-ProdRatio)%>%
  head(10)%>%
  mutate(ExportRatio=case_when(
    ExportRatio>ProdRatio~ProdRatio,
    TRUE~ExportRatio
  ))

# Potassium
exK<-ex%>%
  filter(Item=="Nutrient potash K2O (total)")%>%
  filter(Year==2018)%>%
  select(Area,Export=Value,ExportRatio=Ratio)

prodK<-prod%>%
  filter(Item=="Nutrient potash K2O (total)")%>%
  filter(Year==2018)%>%
  select(Area,Prod=Value,ProdRatio=Ratio)

datK <- exK%>%
  left_join(prodK,by=c('Area'))%>%
  arrange(-ProdRatio)%>%
  head(10)

# Prepare plots

# Nitrogen

curveN<-tibble(
  x=20,xend=15,
  y=6,yend=7,
  lab=
'Russia is the main exporter,\n
with exports equal to\n
7% of world production'
)

pN<-ggplot(data=datN,aes(x=ProdRatio,y=fct_reorder(Area,ProdRatio)))+
  geom_segment(aes(x=0,xend=ProdRatio,yend=Area),size=9,color=col_prod)+
  geom_segment(aes(x=0,xend=ExportRatio,yend=Area),size=5,color=col_exp)+
  geom_text(
    aes(x=ProdRatio+1,y=Area,label=Area),
    hjust=0,family="oswald",size=10)+
  geom_curve(
    data = curveN,
    aes(x = x, y = y, xend = xend, yend = yend),
        arrow = arrow(length = unit(0.03, "npc"))
  )+
  geom_text(
    data= curveN,
    aes(x=x,y=y-0.2,label=lab),hjust=0,vjust=1,
    lineheight=0.15,size=11,family='barlow',
    fontface='italic'
  )+
  annotate(
    "text",x=36,y=1,label="Nitrogen",
    size=20,family=ft_nut,hjust=1,vjust=0,
    fontface='bold',alpha=1,color=col_nut
  )+
  scale_x_continuous(limits=c(0,36),breaks=seq(0,30,10),labels = glue::glue("{seq(0,30,10)} %"))+
  labs(
    x="Values expressed as % of world fertilizer production"
  )+
  theme_minimal()+
  theme(
    plot.background=element_rect(fill=wh,color=NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=30,family="barlow"),
    axis.title.x = element_text(size=30,family="barlow",margin = margin(t = 5, r = 0, b = 5, l = 0))
  )

curveP<-tibble(
  xend=33,x=16,
  yend=9.3,y=6,
  lab=
'The top 5 countries\n
produce more than\n
two-thirds of all\n
phosphorus fertilizers'
)

pP<-ggplot(data=datP,aes(x=ProdRatio,y=fct_reorder(Area,ProdRatio)))+
  geom_segment(aes(x=0,xend=ProdRatio,yend=Area),size=9,color=col_prod)+
  geom_segment(aes(x=0,xend=ExportRatio,yend=Area),size=5,color=col_exp)+
  geom_text(
    aes(x=ProdRatio+1,y=Area,label=Area),
    hjust=0,family="oswald",size=10)+
  annotate(
    "text",x=36,y=1,label="Phosphorus",
    size=20,family=ft_nut,hjust=1,vjust=0,
    fontface='bold',alpha=1,color=col_nut)+
  annotate(
    "text",x=6,y=10,label="Exports",
    size=10,family='roboto',hjust=0.5,vjust=0.5,
    alpha=1,color="black")+
  annotate(
    "text",x=22,y=10,label="Production",
    size=10,family='roboto',hjust=0.5,vjust=0.5,
    alpha=1,color=wh)+
  geom_curve(
    data = curveP,
    aes(x = x, y = y, xend = xend, yend = yend),
    lty='dotted',curvature = 0.25,size=1.15
  )+
  geom_text(
    data= curveP,
    aes(x=x+10,y=y+0.5,label=lab),hjust=0,vjust=1,
    lineheight=0.15,size=11,family='barlow',
    fontface='italic'
  )+
  scale_x_continuous(limits=c(0,36),breaks=seq(0,30,10),labels = glue::glue("{seq(0,30,10)} %"))+
  labs(
    x="Values expressed as % of world fertilizer production"
  )+
  theme_minimal()+
  theme(
    plot.background=element_rect(fill=wh,color=NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=30,family="barlow"),
    axis.title.x = element_text(size=30,family="barlow",margin = margin(t = 5, r = 0, b = 5, l = 0))
  )


curveK<-tibble(
  x=32,xend=32,
  y=8,yend=9.6,
  lab=
'Canada is the main producer\n
and exporter, with about\n
30% of world production,\n
largely exported'
)

pK<-ggplot(data=datK,aes(x=ProdRatio,y=fct_reorder(Area,ProdRatio)))+
  geom_segment(aes(x=0,xend=ProdRatio,yend=Area),size=9,color=col_prod)+
  geom_segment(aes(x=0,xend=ExportRatio,yend=Area),size=5,color=col_exp)+
  geom_text(
    aes(x=ProdRatio+1,y=Area,label=Area),
    hjust=0,family="oswald",size=10)+
  annotate(
    "text",x=36,y=1,label="Potassium",
    size=20,family=ft_nut,hjust=1,vjust=0,
    fontface='bold',alpha=1,color=col_nut)+
  geom_curve(
    data = curveK,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  geom_text(
    data= curveK,
    aes(x=x-10,y=y-0.2,label=lab),hjust=0,vjust=1,
    lineheight=0.15,size=11,family='barlow',
    fontface='italic'
  )+
  scale_x_continuous(limits=c(0,36),breaks=seq(0,30,10),labels = glue::glue("{seq(0,30,10)} %"))+
  labs(
    x="Values expressed as % of world fertilizer production"
  )+
  theme_minimal()+
  theme(
    plot.background=element_rect(fill=wh,color=NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=30,family="barlow"),
    axis.title.x = element_text(size=30,family="barlow",margin = margin(t = 5, r = 0, b = 5, l = 0))
  )

# Title and sub
fake <- tibble(
  x=c(0,10),
  y=c(0,10)
)

lab<-tibble(
  x=0,
  y=7.8,
  lab="
This graph shows <span style='color:#34073d'>**the top ten fertilizer producers** </span>by nutrient,<br>
as well as <span style='color:#ef745c'>**the associated exports**</span> for these countries. Figures<br>
are expressed as percentage of world fertilizer production.<br>
<br>
Fertilizer production is concentrated in a **limited number of<br>
countries**. Since nitrogen fixation by the Haber-Bosch process<br>
is energy intensive, the main producers of these fertilizers are<br>
mainly countries with large energy resources.<br>
<br>
Phosphorus and potassium fertilizers are made from mineral<br>
resources. So the production of these fertilizers tends to be<br>
even more concentrated, based on the distribution of resources.
  "
)

cap <- tibble(
  x=0,y=0,
  label="**Data:** FAO Stats (Year 2018)  **| Plot:** @BjnNowak"
)

tit<-ggplot(data=fake,aes(x=x,y=y))+
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(0,10))+
  annotate(
    "text",x=0,y=9.5,label="The Fertilizer Monopoly",
    size=28,family="passion",hjust=0,vjust=1,lineheight=0.15,
    alpha=1,col="#101919" 
  )+
  geom_richtext(
    data=lab,
    aes(x=x, y=y, label = lab),
    size=11,family="roboto",hjust=0,vjust=1,lineheight=0.40,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  geom_richtext(
    data=cap,
    aes(x=x, y=y, label = label),
    col="grey20",
    size=10,family="roboto",hjust=0,vjust=1,lineheight=0.40,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+
  theme_void()+
  theme(
    plot.background=element_rect(fill=wh,color=NA),
    title=element_blank()
  )

tit+pN+pP+pK+
  plot_layout(ncol = 2)

