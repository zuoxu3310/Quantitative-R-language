#################################################
####### Anna Almakaeva, Natalia Mikhailova ######
################# Visualization #################
##################### class 10 ###################

library("haven")
wvs6_1<-read_sav("/Users/Natasha/Downloads/WV6_Data_Spss_v20180912.sav") 
dim(wvs6_1)
wvs6_rus<-wvs6_1[as_factor(wvs6_1$V2)=="Russia", ]
dim(wvs6_rus)
tapply(wvs6_swru$V242, droplevels(as_factor(wvs6_swru$V2)), mean, na.rm=T)

#Table with knitr
#install.packages("knitr")
library(knitr)
# kable() creates nice tables
kable(tapply(wvs6_swru$V242, droplevels(as_factor(wvs6_swru$V2)), mean, na.rm=T))

xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
      data=wvs6_swru, drop.unused.levels = T ) #cross table from previous class
kable(xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
            data=wvs6_swru, drop.unused.levels = T )) #make it nicer

#################################
### Visualization with ggplot ###
#################################

#install.packages("ggplot2")
library(ggplot2)

# any graph with ggplot2 we start with specifying the data
ggplot(data = wvs6_rus)
# Then we add layers with graphs/ names of the axis/ legend/ coloring/ shapes/ sizes/ etc.

### DOT GRAPH ###
# is for 2 continuous variables, it's useful to assume about the associations
# V242 - age, V249 - What age did you complete your education
ggplot(data = wvs6_rus) +                        # + is to add new layer
  geom_point(mapping = aes(x = V242, y = V249))  # geom_point because we want dot graph
sum(is.na(wvs6_rus$V249)) # warning message is ok as we do have some NAs

# change color и size of dots inside geom_point()
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249), color = 'blue', size = 1.5)

# we can define any beatiful colors with HEX format (not only base ones)
# example of website with colors in hex: https://colorscheme.ru/color-converter.html
# (do not to put # before the color code from the website)
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249), color = '#f27474', size = 1.5)

# you can change the shape of dots
# see picture on our Yandex disk or to google it
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249), color = '#f27474', size = 1.5, shape=8)

# color= attribute changes the border color
# to fill the shape - use fill=
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249), color = '#f27474', 
             shape = 25, fill = 'yellow')

# change the transparency of points use alpha=
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249), alpha = 0.5)

# categorize by the third variable
# let's see which points are males and which ones are females (V240 - gender)
#let's also add as_factor so that to indentify that V240 is nominal
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240)))

# CA.
# Create a beautiful dot graph for age (V242) and year of birth (V241) for Russia and Sweden
# Add categarization by countries


### HISTOGRAM ###
#for 1 continuous variable

# by the way, we can omit "data=" and write the name of data immediately for dot graphs as well
ggplot(wvs6_rus, aes(x = V242)) +
  geom_histogram()

# change the width of the columns (binwidth)
ggplot(wvs6_rus, aes(x = V242)) +
  geom_histogram(binwidth = 1)

# shares by y-axis (..density..)
ggplot(wvs6_rus, aes(x = V242)) +
  geom_histogram(aes(y = ..density..), binwidth = 1)

# let's categorize by gender again (fill=)
ggplot(wvs6_rus, aes(x = V242, fill = as_factor(V240))) +
  geom_histogram(binwidth = 1)

# change legend labels and colors manually with scale_fill_manual
ggplot(wvs6_rus, aes(x = V242, fill = as_factor(V240))) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(name="Sex", 
                    values = c("Male"="black","Female"="red"))

# categories can overlap each other,
# therefore, we can look at the distribution sequentially (position)
ggplot(wvs6_rus, aes(x = V242, fill = as_factor(V240))) +
  geom_histogram(binwidth = 1, position = 'dodge')
#for each age there are 2 bins of males and females

ggplot(wvs6_rus, aes(x = V242, fill = as_factor(V240))) +
  geom_histogram(binwidth = 1, position = 'fill')
#for each bin there is propornional distribution of males and females

# distribution with lines geom_freqpoly()
ggplot(wvs6_rus, aes(x = V242)) +
  geom_freqpoly(binwidth = 1)

# as far as we use layers to combine them and thus to create a graph, we can assign some layers to objects
g1 <- ggplot(wvs6_rus, aes(x = V242))
g1 + geom_freqpoly(binwidth = 1)

#######################
###### Bar Chart ######
#######################
# for one discrete variable
#V57 - marital status

ggplot(wvs6_rus, aes(x = V57)) +
  geom_bar()

ggplot(wvs6_rus, aes(x = V57, fill = as_factor(V240))) +
  geom_bar()

g2 <- ggplot(wvs6_rus, aes(x = V57, fill = as_factor(V240))) +
  geom_bar(position = "fill")

g3 <-  ggplot(wvs6_rus, aes(x = V57, fill = as_factor(V240))) +
  geom_bar(position = "dodge")

#to combine graphs in one picture
library(ggpubr)
ggarrange(g2, g3)
# or to control the picture (see help for more options)
ggarrange(g2, g3, heights = c(2, 0.7),
          labels = c("dodge", "fill"),
          ncol = 2, nrow = 1)
ggsave("marstatus_combined") # to save it with a file name "marstatus_combined"

# CA.
# Create graphs for distributions of importance of family (V4) and importance of friends (V5) for Russia and Sweden
# (recode variables if the scale is reverse for better logic of visualisation)
# Add categorization by countries 



######################
####### Names  #######
######################
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  labs(title = 'Distribution of age and  age of edu completing',
       subtitle = 'by Russian sample',
       caption = 'Data: WVS, 6 wave',
       x = 'Age',
       y = 'Age when edu completed',
       color = 'Sex')

#another way of axes names is to write them independently
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  xlab('Age') +
  ylab('Age when edu completed')

# to assign break points on axes 
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  xlab('Age') +
  ylab('Age when edu completed') +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  scale_y_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45, 50)) 

# change the labels
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  xlab('Age') +
  ylab('Age when edu completed') +
  scale_x_continuous(breaks = c(25, 50, 75),
                     labels=c("25 y.o.", "50 y.o.", "75 y.o."))

# assign limits by axes (to see a piece of data)
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  xlab('Age') +
  ylab('Age when edu completed') +
  scale_x_continuous(breaks = c(25, 50, 75),
                     labels=c("25 y.o.", "50 y.o.", "75 y.o.")) +
  ylim(20, 30) +
  xlim(30, 40)

# move the legend
ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  xlab('Age') +
  ylab('Age when edu completed') +
  scale_x_continuous(breaks = c(25, 50, 75),
                     labels=c("25 y.o.", "50 y.o.", "75 y.o.")) +
  ylim(20, 30) +
  xlim(30, 40) +
  theme(legend.position = 'left')

# add some lines

## geom_hline – horizontal line
## geom_vline – vertical line
## geom_abline – a straight line with some constant value and slope

#let's calculate mean ages
mean(wvs6_rus$V242, na.rm = T)
mean(wvs6_rus$V249, na.rm = T)

ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  xlab('Age') +
  ylab('Age when edu completed') +
  geom_hline(yintercept = 20.74136, color = 'black') +
  geom_vline(xintercept = 46.0564, color = 'blue') + 
  geom_abline(intercept = 12, slope = 0.2, color = 'red')

#######################
######## THEMES #######
#######################
# theme_grey()
# theme_bw()
# theme_linedraw()
# theme_light()
# theme_dark()
# theme_minimal()
# theme_classic()
# theme_void()
# theme_test()

ggplot(data = wvs6_rus) +
  geom_point(mapping = aes(x = V242, y = V249, color = as_factor(V240))) +
  theme_minimal()

ggplot(wvs6_rus, aes(x = V57, fill = as_factor(V240))) +
  geom_bar() +
  theme_dark()

# styles
ggplot(wvs6_rus, aes(x = V57, fill = as_factor(V240))) +
  geom_bar() +
  theme_dark() +
  scale_fill_brewer(palette = "Blues")
# more styles:  https://observablehq.com/@d3/color-schemes (and other websites, just google)

#CA.
# Create a graph of distribution of happiness (V10) in Russia and Sweden on one graph
# Add all needed signs and names
# Assign nice colors 

