library(tidyverse)
library(ggpubr)
#Create data

simdat <- seq(0,1,by=.01) %>% #simulate all proportions at decimal level
  as.data.frame() %>% #convert to dataframe
  rename(Proportion=".") %>% #give proportions at name
  mutate(
    SUM.og = Proportion, #create the original SUM completion score
    SUM.value=if_else(Proportion>=.78,"Good","Poor"), #show which values should be considered 'good' and 'bad' based on spec
    SUM.bern = pnorm((Proportion - .78)  / (Proportion * (1-Proportion))  ), #create new SUM score using Bernoulli variance
    SUM.inter = if_else(Proportion>=.78, #create new SUM score using polynomial interpolation
                        1 - (1-Proportion) * (.5/.22), Proportion *.5/.78)) %>%
  rowwise() %>%
  mutate(SUM.new.avg = mean(c(SUM.bern,SUM.inter))) #make an average of both new approaches



#plot data

#original SUM plot
propbyog <- simdat %>%
  ggplot(aes(x=Proportion,SUM.og,color=SUM.value)) +
  geom_point() +
  # annotate("rect",xmin=.5, xmax=.78, ymin=.5, ymax=1,alpha=.2,color="green") +
  #annotate("label", x =.59, y = .95, label = "Unbiased") +
  annotate("rect",xmin=0, xmax=1, ymin=.5, ymax=1,alpha=.2,color="darkgray",fill="lightgray") +
  annotate("text", x =.2, y = .58, label = "Box should include \n'good' SUM scores") +
  ggsci::scale_color_d3() +
  ggthemes::theme_tufte(base_family = "sans") +
  theme(aspect.ratio=1,
        text = element_text(size=16))

#bernoulli plot
propbybern <- simdat %>%
  ggplot(aes(x=Proportion,SUM.bern,color=SUM.value)) +
  geom_point() +
  # annotate("rect",xmin=.5, xmax=.78, ymin=.5, ymax=1,alpha=.2,color="green") +
  #annotate("label", x =.59, y = .95, label = "Unbiased") +
  annotate("rect",xmin=0, xmax=1, ymin=.5, ymax=1,alpha=.2,color="darkgray",fill="lightgray") +
  annotate("text", x =.2, y = .58, label = "Box should include \n'good' SUM scores") +
  ggsci::scale_color_d3() +
  ggthemes::theme_tufte(base_family = "sans") +
  theme(aspect.ratio=1,
        text = element_text(size=16))

propbybern

#combine and export
p1 <- ggarrange(propbyog,propbybern,common.legend = T,labels = c("Original","Bernoulli-variance"),legend = "bottom")
p1
ggexport(p1, filename="/Users/carlpearson/Downloads/ogandbern.png",
         width=800,
         height = 450)


#create polynomial interpolation plot
propbyinter <- simdat %>%
  ggplot(aes(x=Proportion,SUM.inter,color=SUM.value)) +
  geom_point() +
  # annotate("rect",xmin=.5, xmax=.78, ymin=.5, ymax=1,alpha=.2,color="green") +
  #annotate("label", x =.59, y = .95, label = "Unbiased") +
  annotate("rect",xmin=0, xmax=1, ymin=.5, ymax=1,alpha=.2,color="darkgray",fill="lightgray") +
  annotate("text", x =.2, y = .58, label = "Box should include \n'good' SUM scores") +
  ggsci::scale_color_d3() +
  ggthemes::theme_tufte(base_family = "sans") +
  theme(aspect.ratio=1,
        text = element_text(size=16))

propbyinter

#combine and export
p2 <- ggarrange(propbyog,propbyinter,common.legend = T,labels = c("Original","Polynomial interpolation"),legend = "bottom")
p2
ggexport(p2, filename="/Users/carlpearson/Downloads/ogandinter.png",
         width=800,
         height = 450)



#Binomial variance - special exploration

propbybinom <- simdat %>%
  mutate(n=100,#simulating sample of 100 per measure
         SUM.binom = pnorm((Proportion - .78)  / (n* (Proportion * (1-Proportion)))  ) #create binomial variance SUM score
  ) %>% 
  ggplot(aes(x=Proportion,SUM.binom,color=SUM.value)) +
  geom_point() +
  # annotate("rect",xmin=.5, xmax=.78, ymin=.5, ymax=1,alpha=.2,color="green") +
  #annotate("label", x =.59, y = .95, label = "Unbiased") +
  annotate("rect",xmin=0, xmax=1, ymin=.5, ymax=1,alpha=.2,color="darkgray",fill="lightgray") +
  annotate("text", x =.2, y = .58, label = "Box should include \n'good' SUM scores") +
  ggsci::scale_color_d3() +
  ggthemes::theme_tufte(base_family = "sans") +
  theme(aspect.ratio=1,
        text = element_text(size=16))

propbybinom

#combine and plot
p3 <- ggarrange(propbyog,propbybinom,common.legend = T,labels = c("Original","Binomial variance"),legend = "bottom")
p3
ggexport(p3, filename="/Users/carlpearson/Downloads/ogandbinom.png",
         width=800,
         height = 450)

