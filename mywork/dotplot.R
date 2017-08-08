library(dplyr)
library(PFRBreedR)
library(reshape2)
library(ggplot2)

#----------------------------------------------------------------------------------------------------#
#                               DOTPLOT FUNCTION                                                     #
#----------------------------------------------------------------------------------------------------#

# run this bit of code once. From here ...
EBDotplot <- function(cropId=2,
                      prpShortName =c("ahplc0","bhplc0","cohCont","mlOils0","hum","car","farn","myr","ulOils"),
                      PI=c("Moutere","Green Bullet","Nelson Sauvin"),
                      group=NULL,
                      trlName=NULL)
{
  # get data out of EBData2 and make long version
  data_D <- EBData2(crpId = cropId,PI=PI,prpShortName = prpShortName,trlName=trlName)
  melt_D <- melt(data_D,id.vars="PedigreeItem",measure.vars = prpShortName)

  # compute means
  mean_D <- melt_D %>%
    group_by(PedigreeItem,variable) %>%
    mutate(mean.ped=mean(value,na.rm=T))

  # get property labels
  prop_D <- EBProperties(crpId=2,prpShortName=prpShortName)
  final_D <- merge(mean_D,prop_D[,c("PrpShortName","PrpName")],by.x="variable",by.y="PrpShortName",all.x=T)
  prop_D <- prop_D[match(prpShortName,prop_D[,"PrpShortName"]),]

  # set order panels
  final_D$PrpName = factor(final_D$PrpName,levels=prop_D$PrpName)

  # set order PIs
  final_D$PedigreeItem = factor(final_D$PedigreeItem,levels=rev(PI))

  # create grouping factor (or not)
  if(!is.null(group))
  {
    temp_D <- data.frame(group,PI)
    final_D <- merge(final_D,temp_D,by.x="PedigreeItem", by.y="PI", all.x=TRUE)
  }
  else  {final_D$group <- final_D$PedigreeItem}

  final_D$group <- as.factor(final_D$group)
  # create graph
  p <- ggplot(data=final_D, aes(x=PedigreeItem,y=value))+
    geom_jitter(aes(y=value),size=1,color="grey",width=0.1,height=0)+
    geom_boxplot(aes(fill=group),alpha=0.2,width=0.6)+
    geom_point(aes(y=mean.ped,color=group),size=3)+
    facet_wrap(~PrpName,scales="free_x")+
    theme_bw()+
    theme(panel.grid = element_blank(),axis.title = element_blank())+
    coord_flip()

  # add or remove legend depends on grouping yes/no
  if(is.null(group))  {p <- p+ theme(legend.position="none")}

  # return graph
  return(p)
}

# ... until here

#----------------------------------------------------------------------------------------------------#
#                               Examples                                                             #
#----------------------------------------------------------------------------------------------------#

# Moutere
list <- EBProperties(prpShortName = c("ahplc0","bhplc0","mlOils0","coh","hum","car","farn","myr","ulOils"))
list[,2]
EBDotplot(prpShortName =c("ahplc0","bhplc0","mlOils0","coh","hum","car","farn","myr","ulOils"),
          PI=c("Moutere","Green Bullet","Nelson Sauvin"),
          PILabels = c("'Hort0605'","'Green Bullet'","'Nelson Sauvin'"),
          prpLabels= c("Alpha HPLC (%)", "Beta HPLC (%)", "Oils (ml/100g)", "Cohumulone (% of alpha)","Humulene (%)",    
                       "Caryophyllene (%)", "Farnesene (%)", "Myrcene (%)","Oil/alpha HPLC ratio (uL/g)"))

# Brewing 2017
ListPI <- c("04.43-37","04.99-09","06.05-31-02","06.06-30-03","06.12-33-06","L12.011-063","L12.014-131")
EBDotplot(prpShortName =c('myr','car','farn','hum','linal','limon','oxProd','flEst','citPin','otherKn', 'unkn','hcRat'),PI=ListPI)

# Smallplot 100
data_D <- EBData2(crpId = 2,trlName = "Small Plot (100) 2014" )
EBDotplot(prpShortName =c('myr','car','farn','hum','linal','limon','oxProd','flEst','citPin','otherKn',  'unkn','hcRat'),PI=data_D$PedigreeItem[order(data_D$PedigreeItem)],trlName = "Small Plot (100) 2014")

# Smallplot 200
data_D <- EBData2(crpId = 2,trlName = "Small Plot (200) 2016" )
mylevels <- levels(data_D$PedigreeItem)
mylevels <- mylevels[order(mylevels)]
mylevels <- c(mylevels[-1],mylevels[1])
EBDotplot(prpShortName =c('myr','car','farn','hum','linal','limon','oxProd','flEst','citPin','otherKn',  'unkn','hcRat'),PI=mylevels,trlName = "Small Plot (200) 2016")
