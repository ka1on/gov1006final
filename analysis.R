##############################
# Replication code for Article: Political Geography of Violence
# Authors: Matthew Ingram and Marcelo Marchesini da Costa  
# University at Albany, SUNY, and Insper, Sao Paulo
# contact: mingram@albany.edu
# Last updated: 20190321
# Key steps here: main analysis, including figures and tables
##############################

#####################
# time log
#####################
Sys.time()

#################################################################

#################################################################
# 
# LOAD DATA
#
#################################################################

load("./data/working/analysis20190320.RData")

#######################################
#
# MODEL IDENTIFICATIONS (2019 R&R):
# 
################
# There are 4 models.
# model1: PMDB, PSDB, and PT included; base category is all other parties
# model2: PMDB included; base category is all other parties
# model3: PSDB included; base category is all other parties
# model4: PT included; base category is all other parties

#######################################

# Model1: 3 major parties (all other parties constitute excluded category)
model1mar <- formula(
  DifHRElec ~ margin + stalign + Abstention + PMDB + PSDB + PT +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

# Model2: PMDB (all others excluded)
model2mar <- formula(
  DifHRElec ~ margin + stalign + Abstention + PMDB +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

# Model3: PSDB (all others excluded)
model3mar <- formula(
  DifHRElec ~ margin + stalign + Abstention + PSDB +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

# Model4: PT (all others excluded)
model4mar <- formula(
  DifHRElec ~ margin + stalign + Abstention + PT +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

########################################################
# OLS models and diagnostics
########################################################

# original model

#ols.ori <- lm(modelori, data=shpbra@data)
#vif(ols.ori)

# new models

ols1m <- lm(model1mar, data=shpbra@data)
ols1e <- lm(model1enc, data=shpbra@data)

ols2m <- lm(model2mar, data=shpbra@data)
ols2e <- lm(model2enc, data=shpbra@data)

ols3m <- lm(model3mar, data=shpbra@data)
ols3e <- lm(model3enc, data=shpbra@data)

ols4m <- lm(model4mar, data=shpbra@data)
ols4e <- lm(model4enc, data=shpbra@data)

ols5m <- lm(model5mar, data=shpbra@data)
ols5e <- lm(model5enc, data=shpbra@data)

# output models to file

stargazer(ols1m, ols2m, ols3m, ols4m,
          keep.stat = c("n", "rsq", "adj.rsq", "aic"),
          out = "./tables/ols.html",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          dep.var.caption = "OLS Models DV: change in homicide rate (HR Change)",
          covariate.labels = c("Margin of Victory", "Alignment", "Abstension",
                               "PMDB", "PSDB", "PT", "PopDensity",
                               "YoungMalePct", "GINI", "HDI", "SingleMotherHH",
                               "Employment", "BolsaFamilia"))

# save ols results to data file
save(ols1m, ols2m, ols3m, ols4m, file = "data.RData")


########################################################
#
# MAPS
#
########################################################

#################################################
# restructure data for ggplot

shp <- shpbra
shp@data$id = rownames(shp@data)

# if get error in fortify command below regarding gpclibPermit not being TRUE, try following:
#install.packages("gpclib", type="source")
#library(gpclib)
#gpclibPermit()
shp.points <- tidy(shp, region="id")

shp.df = join(shp.points, shp@data, by="id")


# PMDB, model 2m

plotPMDB.2m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PMDB95.2m) + 
  geom_polygon(colour="transparent", fill="white")

plotPMDB.2m <- plotPMDB.2m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="PMDB local \u03B2 (Model 2)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/mapPMDB_2m.png", height=6, width=6, units="in", res=300)
print(plotPMDB.2m)
dev.off()

#############################################
# PSDB

# PSDB, 3m

plotPSDB.3m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PSDB95.3m) + 
  geom_polygon(colour="transparent", fill="white")

plotPSDB.3m <- plotPSDB.3m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="PSDB local \u03B2 (Model 3)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/mapPSDB_3m.png", height=6, width=6, units="in", res=300)
print(plotPSDB.3m)
dev.off()


#############################################
# PT

# PT, 4m

plotPT.4m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PT95.4m) + 
  geom_polygon(colour="transparent", fill="white")

plotPT.4m <- plotPT.4m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="PT local \u03B2 (Model 4)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/mapPT_4m.png", height=6, width=6, units="in", res=300)
print(plotPT.4m)
dev.off()


#####################################################
# remaining variables with non-stationary effect
#####################################################

# Absention, model 1

plotAbstention.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=Abstention95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotAbstention.1m <- plotAbstention.1m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Abstention local \u03B2 (Model 1)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/mapAbstention1m.png", height=6, width=6, units="in", res=300)
print(plotAbstention.1m)
dev.off()

# HHsinpar, model 1

plotHHsinpar.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=HHsinpar95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotHHsinpar.1m <- plotHHsinpar.1m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="SingleParentHH local \u03B2") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, size=12), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/mapHHsinpar1m.png", height=6, width=6, units="in", res=300)
print(plotHHsinpar.1m)
dev.off()

# CoverBF, model 1

plotCoverBF.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=CoverBF95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotCoverBF.1m <- plotCoverBF.1m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="BolsaFamilia local \u03B2") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, size=12), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/mapCoverBF1m.png", height=6, width=6, units="in", res=300)
print(plotCoverBF.1m)
dev.off()

# lpopdensity95, model 1

plotlpopdensity.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=lpopdensity95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotlpopdensity.1m <- plotlpopdensity.1m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="PopDensity local \u03B2") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, size=12), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/maplpopdensity1m.png", height=6, width=6, units="in", res=300)
print(plotlpopdensity.1m)
dev.off()

# lpctpopym95, model 1

plotlpctpopym.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=lpctpopym95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotlpctpopym.1m <- plotlpctpopym.1m + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="YoungMalePct local \u03B2") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, size=12), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/maplpctpopym1m.png", height=6, width=6, units="in", res=300)
print(plotlpctpopym.1m)
dev.off()

# combined non-political variables

png(file="./figures/map_nonpoliticalvars.png", height=6.5, width=6.5, units="in", res=300)
grid.arrange(plotlpopdensity.1m, plotlpctpopym.1m,
             plotHHsinpar.1m, plotCoverBF.1m, ncol=2)
dev.off()

#####################
# time log
#####################
Sys.time()


#end