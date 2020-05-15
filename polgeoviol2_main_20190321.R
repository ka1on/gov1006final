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

#################################################################

# If already ran this main script and saved data, 
# then load RData workspace with next line:

# e.g., load("./data/working/data.RData")

# otherwise, load shapefiles with data below:

# load shp with state boundaries
# if have rgdal:
shpst <- readOGR(dsn='./shapefiles', layer="estados_2010_noislands")

# if rgdal not available, use readShapeSpatial (maptools package; fxn deprecated)
#shpst <- readShapeSpatial('./shapefiles/estados_2010_noislands.shp')

# note: sometimes dbf file will not load if extension is .DBF; if this happens, try renaming to lower case .dbf

# load muni shapefile with data for WD
# from rgdal package (preferable):
shpbra <- readOGR(dsn="./shapefiles", layer="homicidesBRA_WD_20190318")

# if rgdal not available, use readShapeSpatial from maptools (deprecated, not maintained)
#shpbra <- readShapeSpatial("./shapefiles/homicidesBRA_WD_20190214.shp")

# check shp files
#plot(shpst)
#plot(shpmun)
#plot(shpbra)

###############################################
# KEY VARIABLES
###############################################

# check vars
# vars from fullest model: DifHRElec ~ pcpop + lpctpopym + GINI + IDHM + Hhsinpar + Ocup18M + CoverBF + Abstention + turnover + stalign + PT + PSDB + DEM + PP + PTB + PR + PSB + PDT + PPS + Pc_do_B + PHS + PMN + PRP + PRTB + PSC + PSDC + PSL + PT_do_B + PTC + PTN + PV + PRB
# PMDB excluded party category

# check names as saved in shapefile
names(shpbra)

# recode names to make more intuitive
names(shpbra) <- c("COD_MUN_TSE", "COD_MUN_IBGE", "SP_ID",
                   "UF", "Sigla",
                   "Nome_Mun",
                   "GINI", "Ocup18male", "IDHM",
                   "CoverBF",
                   "lpctpopym",
                   "Area",
                   "popdensity",
                   "lpopdensity",
                   "Abstention",
                   "HHsinpar",
                   "Pop2008",
                   "DifHRElec",
                   "PT", "PSDB", "PMDB", "DEM", "PP","PTB",
                   "PR",  "PSB","PDT", "PPS","Pc_do_B", "PHS", "PMN", "PRP",    
                   "PRTB","PSC", "PSDC","PSL", "PT_do_B", "PTC", "PTN", "PV",     
                   "PRB",   "stalign", "turnover", "pcpop", 
                   "numcan",  "margin",  "majdis", "enc")

names(shpbra)

######################################################################


####################################
# MAPS OF VARIABLES OF INTEREST
####################################

# SETUP data for graphing (turn spatial file to data frame)

shpbra$PMDB.f <- as.factor(shpbra$PMDB)
shpbra$PSDB.f <- factor(shpbra$PSDB)
shpbra$PT.f <- factor(shpbra$PT)
shpbra$stalign.f <- factor(shpbra$stalign)

bra <- shpbra
bra@data$id = rownames(bra@data)
bra.points <- tidy(bra, region="id")
bra.df = join(bra.points, bra@data, by="id")

# DV

ploty <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=DifHRElec) + 
  geom_polygon(colour="transparent", fill="white")

ploty.2 <- ploty + 
  geom_polygon(aes(x = long, y = lat, group = group), data = bra.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name=name=NULL, # can replace with "\u0079" for "y", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Change in Homicide Rate") +
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

png(file="./figures/mapdv.png", height=6, width=6, units="in", res=300)
print(ploty.2)
dev.off()

# Explanatory variables

# PMDB

plot1 <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=PMDB.f 
      #col=brewer.pal(3, "Blues")
      ) + 
  geom_polygon(colour="transparent")

plot1.2 <- plot1 + 
  scale_fill_manual(name=NULL, # can replace name with legend title
                    values=c("aliceblue", "blue")) +
  labs(x="", y="", title="PMDB") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,-5,0), "mm")) + # sets margin around full plot at top, right, bottom, and left
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

# PSDB 

plot2 <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=PSDB.f 
      #col=brewer.pal(3, "Blues")
      ) + 
  geom_polygon(colour="transparent")

plot2.2 <- plot2 + 
  #scale_fill_brewer(name=NULL, # can replace with "PSDB", 
  #                                   type="qual", palette="Blues") + 
  scale_fill_manual(name=NULL, # can replace name with legend title
                    values=c("aliceblue", "blue")) +
  labs(x="", y="", title="PSDB") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,-5,0), "mm")) + # sets margin around full plot at top, right, bottom, and left
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

# PT

plot3 <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=PT.f 
      #col=brewer.pal(3, "Blues")
      ) + 
  geom_polygon(colour="transparent")

plot3.2 <- plot3 + 
  #scale_fill_brewer(name=NULL, # can replace with "PSDB", 
  #                                   type="qual", palette="Blues") + 
  scale_fill_manual(name=NULL, # can replace name with legend title
                    values=c("aliceblue", "blue")) +
  labs(x="", y="", title="PT") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,-5,0), "mm")) + # sets margin around full plot at top, right, bottom, and left
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

# Alignment

plot4 <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=stalign.f 
      #col=brewer.pal(3, "Blues")
      ) + 
  geom_polygon(colour="transparent")

plot4.2 <- plot4 + 
  #scale_fill_brewer(name=NULL, # can replace with "PSDB", 
  #                                   type="qual", palette="Blues") + 
  scale_fill_manual(name=NULL, # can replace name with legend title
                    values=c("aliceblue", "blue")) +
  labs(x="", y="", title="Alignment") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,-5,0), "mm")) + # sets margin around full plot at top, right, bottom, and left
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

# Margin of victory

plot5 <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=margin) + 
  geom_polygon(colour="transparent")

plot5.2 <- plot5 + scale_fill_gradient(name=NULL, # can replace with "margin"
                                       low="aliceblue", high="blue") + 
  labs(x="", y="", title="Margin") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,-5,0), "mm")) + # sets margin around full plot at top, right, bottom, and left
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

# Abstention

plot6 <- ggplot(bra.df) + 
  aes(long,lat,group=group,fill=Abstention) + 
  geom_polygon(colour="transparent")

plot6.2 <- plot6 + scale_fill_gradient(name = NULL, # can replace with "Abstention", 
                                       low="aliceblue", high="blue") + 
  labs(x="", y="", title="Abstention") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,-5,0), "mm")) + # sets margin around full plot at top, right, bottom, and left
  coord_equal(ratio=1) +
  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 

# This is revised Figure 2 for WD R&R (2019-03-20)
png(file="./figures/keypredictors.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
grid.arrange(plot1.2, plot2.2, plot3.2, plot4.2, plot5.2, plot6.2, ncol=2)
dev.off()

#############################################
# check distributions and correlations; not reported in paper
#############################################
#library("PerformanceAnalytics")
#corr <- shpbra@data[, c(7:11, 14:16, 18, 42:48)]

#png(file="./figures/corrplot.png")
#chart.Correlation(corr, histogram=TRUE, pch=19)
#dev.off()


#########################################################
# MODELS
#########################################################

####################################################
# original model with 3 major parties from 2017-2018
# OLD; NOTE REPORTED IN FINAL PAPER
####################################################

#modelori <- formula(
#  DifHRElec ~ turnover + stalign + Abstention + PMDB + PSDB + PT +
#    pcpop + lpctpopym + 
#    GINI + IDHM + 
#    HHsinpar + Ocup18male + 
#    CoverBF
#)

#######################################
#
# NEW MODELS (2019 R&R):
# 
################
# There are 5 models, with 2 versions of each.
# The five models are:
# model1: PMDB, PSDB, and PT included; base category is all other parties
# model2: PMDB included; base category is all other parties
# model3: PSDB included; base category is all other parties
# model4: PT included; base category is all other parties
# model5: all parties includes; base category PMDB since modal/largest party

# note: first 4 are main models; model 5 is for appendix reference

# The 2 versions of each model are:
# (i) electoral competition measured as margin of victory 
# (ii) competition measured with enc (as alternative, robustness check)

#######################################

# Model1: 3 major parties (all other parties constitute excluded category)
model1mar <- formula(
  DifHRElec ~ margin + stalign + Abstention + PMDB + PSDB + PT +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

model1enc <- formula(
  DifHRElec ~ enc + stalign + Abstention + PMDB + PSDB + PT +
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

model2enc <- formula(
  DifHRElec ~ enc + stalign + Abstention + PMDB +
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

model3enc <- formula(
  DifHRElec ~ enc + stalign + Abstention + PSDB +
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

model4enc <- formula(
  DifHRElec ~ enc + stalign + Abstention + PT +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

# Model5: all parties, PMDB is base category
model5mar <- formula(
  DifHRElec ~ margin + stalign + Abstention + 
    PT + PSDB + DEM + PP + PTB + PR + PSB + PDT + PPS + 
    Pc_do_B + PHS + PMN + PRP + PRTB + PSC + PSDC + 
    PSL + PT_do_B + PTC + PTN + PV + PRB +
    lpopdensity + lpctpopym + 
    GINI + IDHM + 
    HHsinpar + Ocup18male + 
    CoverBF
)

model5enc <- formula(
  DifHRElec ~ enc + stalign + Abstention + 
    PT + PSDB + DEM + PP + PTB + PR + PSB + PDT + PPS + 
    Pc_do_B + PHS + PMN + PRP + PRTB + PSC + PSDC + 
    PSL + PT_do_B + PTC + PTN + PV + PRB +
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

#vif(ols1m)
#vif(ols1e)
#vif(ols2m)
#vif(ols2e)
#vif(ols3m)
#vif(ols3e)
#vif(ols4m)
#vif(ols4e)
#vif(ols5m)
#vif(ols5e)

# vif ok; no concerns

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
save(ols1m, ols2m, ols3m, ols4m, file = "data.RData")

stargazer(ols1m, ols2m, ols3m, ols4m, ols5m,
          type = "html", 
          keep.stat = c("n", "rsq", "adj.rsq", "aic"),
          out = "./tables/ols-appendix.html")

#####################
# time log
#####################
Sys.time()

###########################################
# Moran Test
###########################################

moran.test(
  shpbra$DifHRElec, 
  listw = nb2listw(poly2nb(shpbra, queen=TRUE)))

# yields Moran's I of 4.478, p < 0.001

###########################################
# MC Stationarity Tests (package: GWmodel)
###########################################

# to run MC tests, uncomment next line to source R script
# note: there are 5 tests and 
# each takes about 5 hours on my Windows machine
# for a *** total about 25 hours to run ***
# to save time, run remotely or on cluster

#source("./code/polgeoviol3_mctests_20190218.R")

# note: based on MC tests for stationarity, could try mixed model

# once data re-loaded can call results of MC tests for 4 main models:
gwrmc1e
gwrmc1m
gwrmc2m
gwrmc3m
gwrmc4m

stargazer(gwrmc1m, gwrmc2m, gwrmc3m, gwrmc4m, type = "html", 
          out = "./tables/gwrmc.html")

########################################
# Alternative to MC test: Bootstrap test for non-stationarity
########################################

# Not done; ran for over 15 hours on remote lmm without completing

#tic("bootstrap1m")
#bs1m <- gwr.bootstrap(model1mar, 
#                      data=shpbra, 
#                      kernel="bisquare",
#                      approach="AIC", 
#                      R=9,
#                      adaptive=TRUE,
#                      dMat=dMat,
#                      verbose=TRUE)
#toc()


#####################
# time log
#####################
Sys.time()

########################################################
# Bandwidth seletion for models (package: spwgr)
########################################################

# coords <- coordinates(shpbra)
# using spgwr

#tic("bw1m")
#bw1m <- gwr.sel(model1mar, 
#                         data = shpbra@data,
#                         adapt = TRUE, 
#                         method = "cv", 
#                         gweight = gwr.bisquare, 
#                         coords = coords)
#bw1m
#toc()
#

# bw = 1345

#############################################################
#
# GWR models
#
#############################################################

# model with parallel package and HPC cluster

## make cluster
#ncores = 20 # use this if using batch file on cluster and set num of cores (here, e.g., 20)
#ncores <- detectCores()/2
#cl <- makeCluster(ncores, type="FORK")
#
#tic("gwr1mcl")
#gwr1mcl <- gwr(model1mar, 
#             data = shpbra@data,
#             adapt = bw1m, 
#             gweight = gwr.bisquare, 
#             coords = coords,
#             cl=cl,
#             fit.points = coords,
#             se.fit=TRUE,
#             hatmatrix=FALSE
#             hatmatrix = TRUE
#             )
#toc()

# about 10x faster using parallel package and cluster (cl) with 16 nodes on lmm
# however, using cl requires setting hatmatrix=FALSE and fit.points=coords, which 
# results in no standard errors (se), so cannot calculate significance of local coefs


#write.csv(gwr1m$SDF, "./output/gwr1m.csv") 
#write.csv(gwr1m$SDF, "./output/gwr1mb.csv") 
#summary(gwr1m$SDF)

###############################################
#
###############################################

# generate distance matrix
#dMat <- gw.dist(dp.locat=coordinates(shpbra))

#bw1orig <- bw.gwr(modelori, 
#              data=shpbra, 
#              approach="CV",
#              kernel="bisquare",
#              adaptive=TRUE,
#              dMat=dMat)

# settled on 1094
# this is about 20% of data, and smaller bw than newer models (1345, or 24% of data)



#######################################
#
# Switched to Using GWmodel
#
#######################################

# using GWmodel is much faster than spgwr, and output already include t-value
# model1m took about 31 minutes to run if se.fit=TRUE
# it only took 3 mins using parallel and 16 cores on remote lmm, but did not return SEs
# In contrast, GWmodel ran same model in 37 seconds (!), generating both SEs and t-values
# this is about 45x faster than basic gwr and 6x faster than parallel gwr!
# I mapped results of both and they generated same results, so no reason to use spgwr
# All GWR models below run using GWmodel package and command gwr.basic()


################################
# original model from 2017-2018
# NOT RUN; NOT REPORTED IN WD PAPER
################################

# generate distance matrix
#dMat <- gw.dist(dp.locat=coordinates(shpbra))

#tic("gwr1.original")
#gwr1orig <- gwr.basic(modelori, 
#                   data=shpbra, 
#                   bw=bw1orig, 
#                   kernel="bisquare",
#                   adaptive=TRUE, 
#                   dMat=dMat
#)
#toc()
#37 secs
#write.csv(gwr1m$SDF, "./output/gwr1orig.csv") 


################################
# new models
################################

# generate distance matrix
dMat <- gw.dist(dp.locat=coordinates(shpbra))

tic("gwr1m")
gwr1m <- gwr.basic(model1mar, 
             data=shpbra, 
             bw=1345,     # bw1 = 1345 from prior line 427 
             kernel="bisquare",
             adaptive=TRUE, 
             dMat=dMat
             )
toc()
#35 secs
write.csv(gwr1m$SDF, "./output/gwr1m.csv") 


# gwr1e (with enc instead of margin)
tic("gwr1e")
gwr1e <- gwr.basic(model1enc, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
write.csv(gwr1e$SDF, "./output/gwr1e.csv") 



# estimate remaining models with same bw

#####################
# gwr2m
tic("gwr2m")
gwr2m <- gwr.basic(model2mar, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
write.csv(gwr2m$SDF, "./output/gwr2m.csv") 


##########################
# gwr2e 
tic("gwr2e")
gwr2e <- gwr.basic(model2enc, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
# 33 seconds
write.csv(gwr2e$SDF, "./output/gwr2e.csv") 



#####################
# gwr3m
tic("gwr3m")
gwr3m <- gwr.basic(model3mar, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
# 32 seconds

write.csv(gwr3m$SDF, "./output/gwr3m.csv") 


##########################
# gwr3e 
tic("gwr3e")
gwr3e <- gwr.basic(model3enc, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
write.csv(gwr3e$SDF, "./output/gwr3e.csv") 



#####################
# gwr4m
tic("gwr4m")
gwr4m <- gwr.basic(model4mar, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
#33 secs
write.csv(gwr4m$SDF, "./output/gwr4m.csv") 


##########################
# gwr4e 
tic("gwr4e")
gwr4e <- gwr.basic(model4enc, 
                   data=shpbra, 
                   bw=1345, 
                   kernel="bisquare",
                   adaptive=TRUE, 
                   dMat=dMat
)
toc()
# 33 secs
write.csv(gwr4e$SDF, "./output/gwr4e.csv") 


################################################
# Robustness check; attempted mixed version of model 1m
################################################

# Not run; ran for over 17 hours on remote lmm without completing

#tic("mixed")
#gwrmix1 <- gwr.mixed(model1mar, 
#                     data=shpbra, 
#                     fixed.vars=c("margin", "stalign", "GINI", "IDHM", "Ocup18male"),
#                     intercept.fixed=TRUE, 
#                     bw=1345, 
#                     diagnostic=TRUE, 
#                     kernel="bisquare",
#                     adaptive=TRUE,
#                     dMat=dMat)
#toc()

rm(dMat) # remove this object because large memory (236 MB)

######################################
#
# LOAD DATA with MC tests all other steps to this point
#
######################################

#save.image("./data/working/analysis20190220.RData")

#load("./data/working/analysis20190220.RData")


#####################
# time log
#####################
Sys.time()


###############################################
# Generate local estimates at 95% confidence
###############################################

##########################################
# local estimates from original model

#gwr1orig$SDF$PMDB95 <- gwr1orig$SDF$PMDB
#gwr1orig$SDF$PMDB95[abs(gwr1orig$SDF$PMDB_TV)<=1.96] <- 0

#gwr1orig$SDF$PSDB95 <- gwr1orig$SDF$PSDB
#gwr1orig$SDF$PSDB95[abs(gwr1orig$SDF$PSDB_TV)<=1.96] <- 0

#gwr1orig$SDF$PT95 <- gwr1orig$SDF$PT
#gwr1orig$SDF$PT95[abs(gwr1orig$SDF$PT_TV)<=1.96] <- 0



#############################################

# local estimates from gwr1m
gwr1m$SDF$Abstention95 <- gwr1m$SDF$Abstention
gwr1m$SDF$Abstention95[abs(gwr1m$SDF$Abstention_TV)<=1.96] <- 0

gwr1m$SDF$PMDB95 <- gwr1m$SDF$PMDB
gwr1m$SDF$PMDB95[abs(gwr1m$SDF$PMDB_TV)<=1.96] <- 0

gwr1m$SDF$PSDB95 <- gwr1m$SDF$PSDB
gwr1m$SDF$PSDB95[abs(gwr1m$SDF$PSDB_TV)<=1.96] <- 0

gwr1m$SDF$PT95 <- gwr1m$SDF$PT
gwr1m$SDF$PT95[abs(gwr1m$SDF$PT_TV)<=1.96] <- 0

gwr1m$SDF$lpopdensity95 <- gwr1m$SDF$lpopdensity
gwr1m$SDF$lpopdensity95[abs(gwr1m$SDF$lpopdensity_TV)<=1.96] <- 0

gwr1m$SDF$lpctpopym95 <- gwr1m$SDF$lpctpopym
gwr1m$SDF$lpctpopym95[abs(gwr1m$SDF$lpctpopym_TV)<=1.96] <- 0

gwr1m$SDF$HHsinpar95 <- gwr1m$SDF$HHsinpar
gwr1m$SDF$HHsinpar95[abs(gwr1m$SDF$HHsinpar_TV)<=1.96] <- 0

gwr1m$SDF$CoverBF95 <- gwr1m$SDF$CoverBF
gwr1m$SDF$CoverBF95[abs(gwr1m$SDF$CoverBF_TV)<=1.96] <- 0

# gwr1e

gwr1e$SDF$Abstention95 <- gwr1e$SDF$Abstention
gwr1e$SDF$Abstention95[abs(gwr1e$SDF$Abstention_TV)<=1.96] <- 0

gwr1e$SDF$PMDB95 <- gwr1e$SDF$PMDB
gwr1e$SDF$PMDB95[abs(gwr1e$SDF$PMDB_TV)<=1.96] <- 0

gwr1e$SDF$PSDB95 <- gwr1e$SDF$PSDB
gwr1e$SDF$PSDB95[abs(gwr1e$SDF$PSDB_TV)<=1.96] <- 0

gwr1e$SDF$PT95 <- gwr1e$SDF$PT
gwr1e$SDF$PT95[abs(gwr1e$SDF$PT_TV)<=1.96] <- 0

gwr1e$SDF$lpopdensity95 <- gwr1e$SDF$lpopdensity
gwr1e$SDF$lpopdensity95[abs(gwr1e$SDF$lpopdensity_TV)<=1.96] <- 0

gwr1e$SDF$lpctpopym95 <- gwr1e$SDF$lpctpopym
gwr1e$SDF$lpctpopym95[abs(gwr1e$SDF$lpctpopym_TV)<=1.96] <- 0

gwr1e$SDF$HHsinpar95 <- gwr1e$SDF$HHsinpar
gwr1e$SDF$HHsinpar95[abs(gwr1e$SDF$HHsinpar_TV)<=1.96] <- 0

gwr1e$SDF$CoverBF95 <- gwr1e$SDF$CoverBF
gwr1e$SDF$CoverBF95[abs(gwr1e$SDF$CoverBF_TV)<=1.96] <- 0

# Local estimates from Model 2 

# gwr2m
gwr2m$SDF$Abstention95 <- gwr2m$SDF$Abstention
gwr2m$SDF$Abstention95[abs(gwr2m$SDF$Abstention_TV)<=1.96] <- 0

gwr2m$SDF$PMDB95 <- gwr2m$SDF$PMDB
gwr2m$SDF$PMDB95[abs(gwr2m$SDF$PMDB_TV)<=1.96] <- 0

gwr2m$SDF$lpopdensity95 <- gwr2m$SDF$lpopdensity
gwr2m$SDF$lpopdensity95[abs(gwr2m$SDF$lpopdensity_TV)<=1.96] <- 0

gwr2m$SDF$lpctpopym95 <- gwr2m$SDF$lpctpopym
gwr2m$SDF$lpctpopym95[abs(gwr2m$SDF$lpctpopym_TV)<=1.96] <- 0

gwr2m$SDF$HHsinpar95 <- gwr2m$SDF$HHsinpar
gwr2m$SDF$HHsinpar95[abs(gwr2m$SDF$HHsinpar_TV)<=1.96] <- 0

gwr2m$SDF$CoverBF95 <- gwr2m$SDF$CoverBF
gwr2m$SDF$CoverBF95[abs(gwr2m$SDF$CoverBF_TV)<=1.96] <- 0

# gwr2e

gwr2e$SDF$Abstention95 <- gwr2e$SDF$Abstention
gwr2e$SDF$Abstention95[abs(gwr2e$SDF$Abstention_TV)<=1.96] <- 0

gwr2e$SDF$PMDB95 <- gwr2e$SDF$PMDB
gwr2e$SDF$PMDB95[abs(gwr2e$SDF$PMDB_TV)<=1.96] <- 0

gwr2e$SDF$lpopdensity95 <- gwr2e$SDF$lpopdensity
gwr2e$SDF$lpopdensity95[abs(gwr2e$SDF$lpopdensity_TV)<=1.96] <- 0

gwr2e$SDF$lpctpopym95 <- gwr2e$SDF$lpctpopym
gwr2e$SDF$lpctpopym95[abs(gwr2e$SDF$lpctpopym_TV)<=1.96] <- 0

gwr2e$SDF$HHsinpar95 <- gwr2e$SDF$HHsinpar
gwr2e$SDF$HHsinpar95[abs(gwr2e$SDF$HHsinpar_TV)<=1.96] <- 0

gwr2e$SDF$CoverBF95 <- gwr2e$SDF$CoverBF
gwr2e$SDF$CoverBF95[abs(gwr2e$SDF$CoverBF_TV)<=1.96] <- 0



# Local estimates from Model 3 

# gwr3m
gwr3m$SDF$Abstention95 <- gwr3m$SDF$Abstention
gwr3m$SDF$Abstention95[abs(gwr3m$SDF$Abstention_TV)<=1.96] <- 0

gwr3m$SDF$PSDB95 <- gwr3m$SDF$PSDB
gwr3m$SDF$PSDB95[abs(gwr3m$SDF$PSDB_TV)<=1.96] <- 0

gwr3m$SDF$lpopdensity95 <- gwr3m$SDF$lpopdensity
gwr3m$SDF$lpopdensity95[abs(gwr3m$SDF$lpopdensity_TV)<=1.96] <- 0

gwr3m$SDF$lpctpopym95 <- gwr3m$SDF$lpctpopym
gwr3m$SDF$lpctpopym95[abs(gwr3m$SDF$lpctpopym_TV)<=1.96] <- 0

gwr3m$SDF$HHsinpar95 <- gwr3m$SDF$HHsinpar
gwr3m$SDF$HHsinpar95[abs(gwr3m$SDF$HHsinpar_TV)<=1.96] <- 0

gwr3m$SDF$CoverBF95 <- gwr3m$SDF$CoverBF
gwr3m$SDF$CoverBF95[abs(gwr3m$SDF$CoverBF_TV)<=1.96] <- 0

# gwr3e

gwr3e$SDF$Abstention95 <- gwr3e$SDF$Abstention
gwr3e$SDF$Abstention95[abs(gwr3e$SDF$Abstention_TV)<=1.96] <- 0

gwr3e$SDF$PSDB95 <- gwr3e$SDF$PSDB
gwr3e$SDF$PSDB95[abs(gwr3e$SDF$PSDB_TV)<=1.96] <- 0

gwr3e$SDF$lpopdensity95 <- gwr3e$SDF$lpopdensity
gwr3e$SDF$lpopdensity95[abs(gwr3e$SDF$lpopdensity_TV)<=1.96] <- 0

gwr3e$SDF$lpctpopym95 <- gwr3e$SDF$lpctpopym
gwr3e$SDF$lpctpopym95[abs(gwr3e$SDF$lpctpopym_TV)<=1.96] <- 0

gwr3e$SDF$HHsinpar95 <- gwr3e$SDF$HHsinpar
gwr3e$SDF$HHsinpar95[abs(gwr3e$SDF$HHsinpar_TV)<=1.96] <- 0

gwr3e$SDF$CoverBF95 <- gwr3e$SDF$CoverBF
gwr3e$SDF$CoverBF95[abs(gwr3e$SDF$CoverBF_TV)<=1.96] <- 0


# Local estimates from Model 4

# gwr4m
gwr4m$SDF$Abstention95 <- gwr4m$SDF$Abstention
gwr4m$SDF$Abstention95[abs(gwr4m$SDF$Abstention_TV)<=1.96] <- 0

gwr4m$SDF$PT95 <- gwr4m$SDF$PT
gwr4m$SDF$PT95[abs(gwr4m$SDF$PT_TV)<=1.96] <- 0

gwr4m$SDF$lpopdensity95 <- gwr4m$SDF$lpopdensity
gwr4m$SDF$lpopdensity95[abs(gwr4m$SDF$lpopdensity_TV)<=1.96] <- 0

gwr4m$SDF$lpctpopym95 <- gwr4m$SDF$lpctpopym
gwr4m$SDF$lpctpopym95[abs(gwr4m$SDF$lpctpopym_TV)<=1.96] <- 0

gwr4m$SDF$HHsinpar95 <- gwr4m$SDF$HHsinpar
gwr4m$SDF$HHsinpar95[abs(gwr4m$SDF$HHsinpar_TV)<=1.96] <- 0

gwr4m$SDF$CoverBF95 <- gwr4m$SDF$CoverBF
gwr4m$SDF$CoverBF95[abs(gwr4m$SDF$CoverBF_TV)<=1.96] <- 0

# gwr4e

gwr4e$SDF$Abstention95 <- gwr4e$SDF$Abstention
gwr4e$SDF$Abstention95[abs(gwr4e$SDF$Abstention_TV)<=1.96] <- 0

gwr4e$SDF$PT95 <- gwr4e$SDF$PT
gwr4e$SDF$PT95[abs(gwr4e$SDF$PT_TV)<=1.96] <- 0

gwr4e$SDF$lpopdensity95 <- gwr4e$SDF$lpopdensity
gwr4e$SDF$lpopdensity95[abs(gwr4e$SDF$lpopdensity_TV)<=1.96] <- 0

gwr4e$SDF$lpctpopym95 <- gwr4e$SDF$lpctpopym
gwr4e$SDF$lpctpopym95[abs(gwr4e$SDF$lpctpopym_TV)<=1.96] <- 0

gwr4e$SDF$HHsinpar95 <- gwr4e$SDF$HHsinpar
gwr4e$SDF$HHsinpar95[abs(gwr4e$SDF$HHsinpar_TV)<=1.96] <- 0

gwr4e$SDF$CoverBF95 <- gwr4e$SDF$CoverBF
gwr4e$SDF$CoverBF95[abs(gwr4e$SDF$CoverBF_TV)<=1.96] <- 0



#####################################
# add local estimates to shapefile
#####################################

##########
# add estimates from original model (2017-2018); NOT DONE

#shpbra$PMDB95.ori <- gwr1orig$SDF$PMDB95
#shpbra$PSDB95.ori <- gwr1orig$SDF$PSDB95
#shpbra$PT95.ori <- gwr1orig$SDF$PT95

######################################

# add estimates from new models

shpbra$Abstention95.1m <- gwr1m$SDF$Abstention95
shpbra$PMDB95.1m <- gwr1m$SDF$PMDB95
shpbra$PSDB95.1m <- gwr1m$SDF$PSDB95
shpbra$PT95.1m <- gwr1m$SDF$PT95
shpbra$lpopdensity95.1m <- gwr1m$SDF$lpopdensity95
shpbra$lpctpopym95.1m <- gwr1m$SDF$lpctpopym95
shpbra$HHsinpar95.1m <- gwr1m$SDF$HHsinpar95
shpbra$CoverBF95.1m <- gwr1m$SDF$CoverBF95

shpbra$Abstention95.1e <- gwr1e$SDF$Abstention95
shpbra$PMDB95.1e <- gwr1e$SDF$PMDB95
shpbra$PSDB95.1e <- gwr1e$SDF$PSDB95
shpbra$PT95.1e <- gwr1e$SDF$PT95
shpbra$lpopdensity95.1e <- gwr1e$SDF$lpopdensity95
shpbra$lpctpopym95.1e <- gwr1e$SDF$lpctpopym95
shpbra$HHsinpar95.1e <- gwr1e$SDF$HHsinpar95
shpbra$CoverBF95.1e <- gwr1e$SDF$CoverBF95

shpbra$Abstention95.2m <- gwr2m$SDF$Abstention95
shpbra$PMDB95.2m <- gwr2m$SDF$PMDB95
shpbra$lpopdensity95.2m <- gwr2m$SDF$lpopdensity95
shpbra$lpctpopym95.2m <- gwr2m$SDF$lpctpopym95
shpbra$HHsinpar95.2m <- gwr2m$SDF$HHsinpar95
shpbra$CoverBF95.2m <- gwr2m$SDF$CoverBF95

shpbra$Abstention95.2e <- gwr2e$SDF$Abstention95
shpbra$PMDB95.2e <- gwr2e$SDF$PMDB95
shpbra$lpopdensity95.2e <- gwr2e$SDF$lpopdensity95
shpbra$lpctpopym95.2e <- gwr2e$SDF$lpctpopym95
shpbra$HHsinpar95.2e <- gwr2e$SDF$HHsinpar95
shpbra$CoverBF95.2e <- gwr2e$SDF$CoverBF95

shpbra$Abstention95.3m <- gwr3m$SDF$Abstention95
shpbra$PSDB95.3m <- gwr3m$SDF$PSDB95
shpbra$lpopdensity95.3m <- gwr3m$SDF$lpopdensity95
shpbra$lpctpopym95.3m <- gwr3m$SDF$lpctpopym95
shpbra$HHsinpar95.3m <- gwr3m$SDF$HHsinpar95
shpbra$CoverBF95.3m <- gwr3m$SDF$CoverBF95

shpbra$Abstention95.3e <- gwr3e$SDF$Abstention95
shpbra$PSDB95.3e <- gwr3e$SDF$PSDB95
shpbra$lpopdensity95.3e <- gwr3e$SDF$lpopdensity95
shpbra$lpctpopym95.3e <- gwr3e$SDF$lpctpopym95
shpbra$HHsinpar95.3e <- gwr3e$SDF$HHsinpar95
shpbra$CoverBF95.3e <- gwr3e$SDF$CoverBF95

shpbra$Abstention95.4m <- gwr4m$SDF$Abstention95
shpbra$PT95.4m <- gwr4m$SDF$PT95
shpbra$lpopdensity95.4m <- gwr4m$SDF$lpopdensity95
shpbra$lpctpopym95.4m <- gwr4m$SDF$lpctpopym95
shpbra$HHsinpar95.4m <- gwr4m$SDF$HHsinpar95
shpbra$CoverBF95.4m <- gwr4m$SDF$CoverBF95

shpbra$Abstention95.4e <- gwr4e$SDF$Abstention95
shpbra$PT95.4e <- gwr4e$SDF$PT95
shpbra$lpopdensity95.4e <- gwr4e$SDF$lpopdensity95
shpbra$lpctpopym95.4e <- gwr4e$SDF$lpctpopym95
shpbra$HHsinpar95.4e <- gwr4e$SDF$HHsinpar95
shpbra$CoverBF95.4e <- gwr4e$SDF$CoverBF95

#####################################
# save (and load) workspace
#####################################

save.image("./data/working/analysis20190320.RData")

#load("./data/working/analysis20190320.RData")

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

##################################################
# PMDB, PSDB, and PT coefs from original model

# PMDB

#plotPMDB.ori <- ggplot(shp.df) + 
#  aes(long,lat,group=group,fill=PMDB95.ori) + 
#  geom_polygon(colour="transparent", fill="white")
#
#plotPMDB.ori <- plotPMDB.ori + 
#  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
#  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
#  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
#  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
#  scale_fill_gradient2(name="\u03B2", 
#                       #values=rescale(c(0, .05, .2)), 
#                       low="blue",
#                       mid="white",
#                       high="red",
#                       midpoint=0,
#                       guide="colourbar") + 
#  labs(x="", y="", title="PMDB local \u03B2 (original model, 12/2017)") +
#  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
#        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
#        axis.line.x = element_blank(), axis.line.y = element_blank(),
#        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
#        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
#        panel.spacing=unit(c(0,0,0,0), "lines"),
#        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
#  coord_equal(ratio=1) +
#  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 
#
#png(file="./figures/mapPMDB_ori.png")
#print(plotPMDB.ori)
#dev.off()
#
#
## PSDB
#
#plotPSDB.ori <- ggplot(shp.df) + 
#  aes(long,lat,group=group,fill=PSDB95.ori) + 
#  geom_polygon(colour="transparent", fill="white")
#
#plotPSDB.ori <- plotPSDB.ori + 
#  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
#  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
#  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
#  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
#  scale_fill_gradient2(name="\u03B2", 
#                       #values=rescale(c(0, .05, .2)), 
#                       low="blue",
#                       mid="white",
#                       high="red",
#                       midpoint=0,
#                       guide="colourbar") + 
#  labs(x="", y="", title="PSDB local \u03B2 (original model, 12/2017)") +
#  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
#        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
#        axis.line.x = element_blank(), axis.line.y = element_blank(),
#        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
#        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
#        panel.spacing=unit(c(0,0,0,0), "lines"),
#        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
#  coord_equal(ratio=1) +
#  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 
#
#png(file="./figures/mapPSDB_ori.png")
#print(plotPSDB.ori)
#dev.off()
#
## PT
#
#plotPT.ori <- ggplot(shp.df) + 
#  aes(long,lat,group=group,fill=PT95.ori) + 
#  geom_polygon(colour="transparent", fill="white")
#
#plotPT.ori <- plotPT.ori + 
#  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
#  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
#  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
#  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
#  scale_fill_gradient2(name="\u03B2", 
#                       #values=rescale(c(0, .05, .2)), 
#                       low="blue",
#                       mid="white",
#                       high="red",
#                       midpoint=0,
#                       guide="colourbar") + 
#  labs(x="", y="", title="PT local \u03B2 (original model, 12/2017)") +
#  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
#        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
#        axis.line.x = element_blank(), axis.line.y = element_blank(),
#        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
#        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
#        panel.spacing=unit(c(0,0,0,0), "lines"),
#        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
#  coord_equal(ratio=1) +
#  geom_polygon(data=shpst, aes(long,lat, group=group), colour="grey50", fill=NA) 
#
#png(file="./figures/mapPT_ori.png")
#print(plotPT.ori)
#dev.off()
#


#####################################################
#####################################################

# new models

#####################################################
#####################################################

# PMDB, model 1m

plotPMDB.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PMDB95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotPMDB.1m <- plotPMDB.1m + 
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
  labs(x="", y="", title="PMDB local \u03B2 (Model 1m)") +
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

png(file="./figures/mapPMDB_1m.png", height=6, width=6, units="in", res=300)
print(plotPMDB.1m)
dev.off()

# PMDB, model 1e

plotPMDB.1e <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PMDB95.1e) + 
  geom_polygon(colour="transparent", fill="white")

plotPMDB.1e <- plotPMDB.1e + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shp.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  #scale_fill_gradientn(name="LISA p", 
  #                     #values=rescale(c(0, .05, .2)), 
  #                     colours=c("darkred", "white"), 
  #                     guide="colourbar") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="PMDB local \u03B2 (Model 1e)") +
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

png(file="./figures/mapPMDB_1e.png")
print(plotPMDB.1e)
dev.off()

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

# PMDB, model 2e

plotPMDB.2e <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PMDB95.2e) + 
  geom_polygon(colour="transparent", fill="white")

plotPMDB.2e <- plotPMDB.2e + 
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
  labs(x="", y="", title="PMDB local \u03B2 (Model 2e)") +
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

png(file="./figures/mapPMDB_2e.png")
print(plotPMDB.2e)
dev.off()



#############################################
# PSDB

# PSDB, 1m

plotPSDB.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PSDB95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotPSDB.1m <- plotPSDB.1m + 
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
  labs(x="", y="", title="PSDB local \u03B2 (Model 1m)") +
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

png(file="./figures/mapPSDB_1m.png")
print(plotPSDB.1m)
dev.off()


# PSDB, 1e

plotPSDB.1e <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PSDB95.1e) + 
  geom_polygon(colour="transparent", fill="white")

plotPSDB.1e <- plotPSDB.1e + 
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
  labs(x="", y="", title="PSDB local \u03B2 (Model 1e)") +
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

png(file="./figures/mapPSDB_1e.png")
print(plotPSDB.1e)
dev.off()

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

# PSDB, 3e

plotPSDB.3e <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PSDB95.3e) + 
  geom_polygon(colour="transparent", fill="white")

plotPSDB.3e <- plotPSDB.3e + 
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
  labs(x="", y="", title="PSDB local \u03B2 (Model 3e)") +
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

png(file="./figures/mapPSDB_3e.png")
print(plotPSDB.1e)
dev.off()


#############################################
# PT

# PT, 1m

plotPT.1m <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PT95.1m) + 
  geom_polygon(colour="transparent", fill="white")

plotPT.1m <- plotPT.1m + 
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
  labs(x="", y="", title="PT local \u03B2 (Model 1m)") +
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

png(file="./figures/mapPT_1m.png")
print(plotPT.1m)
dev.off()

# PT, 1e

plotPT.1e <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PT95.1e) + 
  geom_polygon(colour="transparent", fill="white")

plotPT.1e <- plotPT.1e + 
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
  labs(x="", y="", title="PT local \u03B2 (Model 1e)") +
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

png(file="./figures/mapPT_1e.png")
print(plotPT.1e)
dev.off()

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

# PT, 4e

plotPT.4e <- ggplot(shp.df) + 
  aes(long,lat,group=group,fill=PT95.4e) + 
  geom_polygon(colour="transparent", fill="white")

plotPT.4e <- plotPT.4e + 
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
  labs(x="", y="", title="PT local \u03B2 (Model 4e)") +
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

png(file="./figures/mapPT_4e.png")
print(plotPT.4e)
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