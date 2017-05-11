##############################
####  MESH-SDG Tool V1.0  ####
####       May 2017       ####
##############################

# This tool produces graphs of modelled ecosystem service (ES) supply values under 
# user-defined scenarios and shows their relevance to attainment of the 
# Sustainable Development Goals (SDG) and targets 

# It is primarily designed for use with outputs from the MESH modelling tool 
# However it can be used with outputs from other models that produce one of 16 ES (see documentation)

# For documentation on the MESH-SDG tool and guidelines for use, 
# see https://docs.google.com/document/d/15Oc-p8iw1kw40PFjfW3a1W7GSimoMbvDeRvPlczMqCQ/edit?usp=sharing  

# It is freely available for use by anyone however we ask that 
# if used in publications, the following citation is used: 
# Jones, S.K., Wood, S.R., Johnson, J.A., DeClerck, F.A. 2017. MESH-SDG tool. 

wd <- readline() #at the prompt, copy and paste your filepath and press enter
setwd(wd)

library("ggplot2")
library("grid")
library("data.table")
library("reshape2")
library("dplyr")
library("graphics")
library("useful")

### Import results from your ES model and prepare for input to ES-SDG tool ###

# See documentation for correct format for results
Results <-read.csv("Results.csv", header = T)

# Reshape data to long format
Results <- melt(Results, "Scenario", variable.name ="ES", value.name = "PropChange")
Results$ES <- as.character(Results$ES)

# Rename ES so labels match ES-SDG lookup table and display nicely on graphs
# to do this, use the ESlabels.csv file
# If using ES model outputs from a tool other than MESH, you will need to remake the ESlabels.csv file
ESlabels <- read.csv("ESlabels.csv", header=T)
Results <- merge.data.frame(Results, ESlabels, by.x = "ES", by.y = "Output", all.x = TRUE)
Results <- Results[,c("Scenario",  "Label", "PropChange")]
names(Results)[names(Results) == "Label"] <- "ES"

#Convert proportional changes in ES to positive and negative % changes
#For water quality and erosion control, the ES increases (e.g. water qual improves) with a negative change from baseline 
#For other ES, the ES increases with a positive change from baseline
Results$PerChange <- ifelse(Results$ES == "Water Quality (N reduction)", Results$PropChange*-100,
                            ifelse(Results$ES == "Water Quality (P reduction)", Results$PropChange*-100,
                                   ifelse(Results$ES == "Erosion Control", Results$PropChange*-100, 
                                          Results$PropChange*100)))
Results <- Results[,c("Scenario", "ES", "PerChange")]

# Optional: Set scenario as factor with levels so they plot in a specific order
# NB these will need changing for each new set of scenarios
Results$Scenario <- factor(Results$Scenario, levels = c("exc", "texu", "texc"))

### Plot results to show ES change across scenarios #### 

# Set colour-blind friendly palette
# NB be sure to assign colours to all ES in your results file
cbPalette <- c("Carbon Storage" = "#009E73", "Erosion Control" = "#E69F00", 
               "Water Provision" = "#56B4E9", "Water Quality (N reduction)" = "#999999", 
               "Water Quality (P reduction)" = "#F0E442") #More if needed: "#0072B2", "#D55E00", "#CC79A7")

ES.plot <- ggplot() +
  geom_col(data=Results, aes(y = Results$PerChange, x = Results$Scenario, fill = Results$ES), position = "dodge")+
  geom_hline(yintercept=0)+ labs(x="Scenarios", y="% change in ES supply", size=12) +
  ggtitle("a)")+
  theme_bw()+ 
  theme(legend.position="none")+
  scale_fill_manual(values=cbPalette, name="Ecosystem \nServices")
ES.plot

ESstack.plot <- ggplot() +
  geom_col(data=Results, aes(y = PerChange, x = Scenario, fill = ES), position = "stack")+
  geom_hline(yintercept=0)+ labs(x="Scenarios", y="% change in ES supply", size=12) +
  ggtitle("b)")+
  theme_bw()+ 
  scale_fill_manual(values=cbPalette, name="Ecosystem \nServices")
ESstack.plot

png('Fig1a ES changes by scenario.png', width=850, height=350, res=100)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
print(ES.plot, vp = vplayout(1,1))
print(ESstack.plot, vp = vplayout(1,2))
dev.off()

#### Plot results to show ES changes mapped to the SDGs ####

# Import ES-SDG links table and make a lookup table for positively linked ES-SDG targets
# "PO" means positively linked, "UN" means link uncertain,"NE" (or missing) means not evaluated
Lookup <- as.data.table(read.csv("Lookup.csv"))
names(Lookup)
Lookup$Target <- paste("SDG", as.character(Lookup$Target_num), sep = "")
Lookup$Goal <- paste("SDG", as.character(Lookup$Goal_num), sep = "")

# Reorder columns and select expert identified linkages only
Lookup <- Lookup[,c(7,1,6,2,3,4,5)]
LookupExperts <- Lookup[,c(1:5,7)]

# Replicate water quality lookup for nitrogen and phosphorus exports
# This step is only needed if you modelled N and P effects separately (e.g. using MESH)
# Reshape to wide format
Lookup <- dcast(LookupExperts,Goal + Goal_num + Target + Target_num ~ ES, value.var="LinkExperts")
Lookup$`Water Quality (P reduction)` <- Lookup$`Water Quality`
Lookup$`Water Quality (N reduction)` <- Lookup$`Water Quality`
Lookup <- melt(Lookup, id = c("Goal","Goal_num", "Target", "Target_num"), variable.name = "ES", value.name = "Link")

# Keep only positively linked ES-SDG and remove Links column (not needed)
Lookup <- filter(Lookup, Link=="PO") 
Lookup <- subset(Lookup, select = -c(Link)) 

# Apply ES-SDG lookup table #
SDG <- merge.data.frame(Results, Lookup, by.x = "ES", by.y = "ES", all.x = TRUE)
SDG<- SDG[c(4,5,6,7,2,1,3)] #reorder columns so easy to read

# set order of variables for plotting
SDG$Goal <- ordered(SDG$Goal, levels =c("SDG1", "SDG2", "SDG3", "SDG6", 
                                        "SDG7", "SDG8", "SDG9","SDG11", 
                                        "SDG12", "SDG13", "SDG14", "SDG15"))
SDG$Goal_num <- factor(as.numeric(SDG$Goal_num))
SDG$Target <- ordered(SDG$Target, levels = SDG$Target[unique(order(SDG$Goal, SDG$Target))]) 
SDG$Target <- factor(SDG$Target)
levels(SDG$Target)
str(SDG)

# Make pivot table to sum % changes in ES linked to any target within a goal
PivotSDGES <- SDG %>%
  dcast(Goal + Scenario ~ ES, sum, value.var = "PerChange", na.rm = TRUE )
PivotSDGES <- melt(PivotSDGES, id = c("Goal", "Scenario"), variable.name = "ES", value.name = "PerChange")

PivotSDG <- SDG %>%
  dcast(Goal ~ Scenario, sum, value.var = "PerChange", na.rm = TRUE )
PivotSDG <- melt(PivotSDG, id=c("Goal"), variable.name = "Scenario", value.name = "PerChange")

#### Plot results as bar charts ####

# This plot shows % change in combined ES, summing changes across targets
png('Fig2a ES-SDG summary bar.png', width=400, height=350, res=100)
SDG.bar <- ggplot(data = PivotSDG, aes(x = Scenario, fill = Goal))+
  geom_bar(aes(y = PerChange), width = 0.7, colour = "grey20", position = "stack", stat="identity")+
  xlab("")+  ylab("% change in ES supply summed across targets")+
  scale_fill_brewer(palette= "Spectral", name="Goals")+
  theme_bw()
SDG.bar
dev.off()

# This plot shows % change in ES per goal, summing changes across targets
# Plots arranged by goal
png('Fig2b ES-SDG by goal bar.png', width=800, height=550, res=80)
grid.newpage()
SDGgoal.plot <- ggplot() +
  geom_col(data=PivotSDGES, aes(y = PerChange, x = Scenario, fill = ES), position = "stack")+
  geom_hline(yintercept=0)+ labs(x="Scenarios", y="% change in ES supply summed across targets", size=12) +
  theme_bw()+ 
  scale_fill_manual(values=cbPalette, name="Ecosystem \nServices")+
  facet_wrap(~ Goal, nrow=2,drop=TRUE)
SDGgoal.plot
dev.off()

# This plot shows % change in ES per target
SDGtarget.plot <- ggplot() +
  geom_bar(data=SDG, aes(y = PerChange, x = Scenario, fill = ES), stat="identity",
           position='stack') + geom_hline(yintercept=0)+ labs(x="Scenarios", y="% Change in ES Supply", size=12) +
  theme_bw() + scale_fill_manual(values=cbPalette, name="Ecosystem \nServices")+
  facet_wrap(~ Target, nrow=5,drop=TRUE)
SDGtarget.plot

png('Fig2c ES-SDG by target bar.png', width=800, height=800, res=110)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,1)))
print(SDGtarget.plot)
dev.off()

### Plot results as petal diagrams ###

# This plot shows % change in combined ES, summing changes across targets
png('Fig3a ES-SDG summary petal.png', width=450, height=450, res=100)
SDG.petal <- ggplot(data = PivotSDG, aes(x = Scenario, fill = Goal))+
  geom_bar(aes(y = PerChange), width = 0.7, colour = "grey20", position = "stack", stat="identity")+
  coord_polar(start = -1*pi/5, direction =1)+ #means start at x radians away from 12 o'clock and go clockwise
  xlab("")+  ylab("% change in ES supply summed across targets")+
  scale_fill_brewer(palette= "Spectral", name="Goals")+
  theme_bw()
SDG.petal
dev.off()

# This plot shows % change in ES per goal, summing changes across targets
png('Fig3b ES-SDG by goal petal.png', width=900, height=550, res=100)
SDGgoal.petal <- ggplot(data = PivotSDGES, aes(x = Scenario, fill = ES))+
  geom_bar(aes(y = PerChange), width = 0.7, colour = "grey20", position = "dodge", stat="identity")+
  coord_polar(start = -1*pi/5, direction =1)+ #means start at x radians away from 12 o'clock and go clockwise
  xlab("")+  ylab("% change in ES supply summed across targets")+
  scale_fill_manual(values = cbPalette, name="Ecosystem \nServices")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust=-2, vjust = -2,size = 8))+
  facet_wrap(~Goal, nrow=2)
SDGgoal.petal
dev.off()

# This plot shows % change in ES per target
png('Fig3c ES-SDG by target petal.png', width=980, height=850, res=100)
SDGtarget.petal <- ggplot(data = SDG, aes(x = Scenario, fill = ES))+
  geom_bar(aes(y = PerChange), width = 0.7, colour = "grey20", position = "dodge", stat="identity")+
  geom_hline(yintercept = 0, colour="red")+
  coord_polar(start = -1*pi/5, direction =1)+ #means start at x radians away from 12 o'clock and go clockwise
  xlab("")+  ylab("% change in ES supply")+
  scale_fill_manual(values = cbPalette, name="Ecosystem \nServices")+
  theme_bw()+
  theme(plot.margin = unit(c(0,1,0,1), "cm"))+
  theme(axis.text.x = element_text(hjust=-2, vjust = -2,size = 8))+
  facet_wrap(~Target, nrow=4)
SDGtarget.petal
dev.off()
