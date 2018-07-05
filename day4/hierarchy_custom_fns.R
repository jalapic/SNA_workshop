## Custom functions for hierarchy tutorial

expandrows <- function(df){
  
  library(splitstackshape)  
  library(data.table)  
  
  
  temp <- cSplit(cSplit(cbind(id = 1:nrow(df), df),
                        "Actor", ",", "long"), 
                 "Recipient", ",", "long")
  
  
  ## Convert "Actor" and "Recipient" to numeric
  SD <- c("Actor", "Recipient")
  temp[, (SD) := lapply(.SD, as.numeric), .SDcols = SD]
  
  
  ## Sort Actors and Recipients, and check for duplicates and any points where Actors equal Recipients  
  temp[, toDrop := duplicated(
    paste(pmin(Actor, Recipient), pmax(Actor, Recipient))) |
      Actor == Recipient, by = id]
  
  ## Create your "score" column
  temp[, score := ifelse(any(toDrop), 0.5, 1), by = id]
  
  ## Subset and drop the irrelevant columns
  out <- temp[!temp[, toDrop, with = TRUE]][, toDrop := NULL]
  out$id<-NULL
  return(out)
}


contests <- function(df,a,b){
  df[(df$Actor==a & df$Recipient==b)|(df$Actor==b & df$Recipient==a),]
}





### Making Matrix Plots

# takes wl matrix and order of id's and creates ggplot matrix.


matrixplot <- function(m, mylevs=NULL, lowcolor="white",highcolor="red1"){
  
  library(ggplot2)
  
  #make the df we will use for plotting
  m.dat <- reshape2::melt(m)
  m.dat <- data.frame(m.dat)
  m.dat <- m.dat[complete.cases(m.dat),] #removing NAs
  
  if(is.null(mylevs)) { mylevs = rownames(m)}
  
  #reorder the levels of the y-axis so plots properly
  m.dat$Recipient <- factor(m.dat$Recipient, levels=mylevs)
  m.dat$Actor <- factor(m.dat$Actor, levels = rev(mylevs))
  m.dat[m.dat == 0] <- NA
  
  
  #plot
  p1<-ggplot(m.dat, aes(Recipient, Actor, fill = value)) + 
    geom_tile(colour="black", 
              size=0.5, stat="identity") + 
    geom_text(data=m.dat, aes(Recipient, Actor, label = value), color="black", size=rel(3.5)) +
    scale_fill_gradient(low = lowcolor, high = highcolor, space = "Lab", na.value = "white", guide = "colourbar") +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("Loser") + 
    ylab("Winner") +
    theme(axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          axis.text = element_text(color="#3C3C3C", size=rel(1.1)),
          legend.position = "none"        
    ) 
  return(p1)
}


# Dichotomized Matrix

matrixplot0 <- function(m,mylevs=NULL,lowcolor="white",highcolor="firebrick3"){
  
  dcs <- m / (m + t(m))
  m.dat_dc <- reshape2::melt(dcs)
  colnames(m.dat_dc)[3]<-"DC"
  
  m1 <- get_di_matrix(m)
  m.dat1 <- reshape2::melt(m1)
  m.dat1 <- data.frame(m.dat1)
  m.dat1 <- merge(m.dat1, m.dat_dc)
  m.dat1$value <- ifelse(is.na(m.dat1$DC), NA, m.dat1$value)
  
  
  #reorder the levels of the y-axis so plots properly
  if(is.null(mylevs)){mylevs = rownames(m)}
  m.dat1$Recipient <- factor(m.dat1$Recipient, levels=mylevs)
  m.dat1$Actor <- factor(m.dat1$Actor, levels = rev(mylevs))
  
  m.dat1$DC[m.dat1$value == 0] <- NA
  m.dat1$value[m.dat1$value == 0] <- NA
  
  
  p2<-ggplot(m.dat1, aes(Recipient, Actor)) + 
    geom_tile(colour="black", aes(fill=DC),
              size=0.5, stat="identity") + 
    geom_text(data=m.dat1, aes(Recipient, Actor, label = value), color="black", size=rel(3.5)) +
    scale_fill_gradient(low = lowcolor, high = highcolor, space = "Lab", na.value = "white", guide = "colourbar") +
    scale_x_discrete(expand = c(0, 0), position='top') +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("Loser") + 
    ylab("Winner") +
    theme(axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          axis.text = element_text(color="#3C3C3C", size=rel(1.1)),
          legend.position = "none"        
    ) 
  return(p2)
}



plotglicko <- function(df, cval=3, mycolors=c("black", "grey", "orange", "red"), 
                       ltypes=c(1,2,3,1,2,3,1,2,3,1,2,3), thetitle="",  linewd=1, ylim1=1000,ylim2=3250,
                       ndays=1){
  
  df <- as.data.frame(df)
  
  robj <- glicko(df, cval=cval, history=T)
  
  x<-as.data.frame(unlist(robj$history))  
  z<-as.factor(df[,1])  #this is the df the glicko was conducted on
  n<-nlevels(z)
  
  x.ratings<-x[,1:n]
  x.deviations<-x[,(1+n):(n+n)]
  
  #longform the data
  x.ratingsmelt<-reshape2::melt(x.ratings)
  
  ids<-rownames(x.ratings)       #to make id column
  x.ratingsmelt$ids<-rep(ids, n)  #making id column
  
  l.ids<-length(ids)
  x.ratingsmelt$event<-rep(1:n, each=l.ids) 
  
  
  #add ranks
  xrn<-as.data.frame(x.ratings[n])
  colnames(xrn)<-c("finalrating")
  
  x.ratingsmelt$rank<-rank(-xrn$finalrating, ties.method="random")
  
  #make ids1 a factor with levels defined by rank
  x.ratingsmelt1 <- data.frame(ids=unique(x.ratingsmelt$ids),rank=unique(x.ratingsmelt$rank))
  x.ratingsmelt1 <- x.ratingsmelt1[order(x.ratingsmelt1$rank),]
  x.ratingsmelt$ids1 <- factor(x.ratingsmelt$ids,levels=x.ratingsmelt1$ids)
  
  #define color palette, multiple options (see below)
  colourCount <-length(unique(x.ratingsmelt$ids))
  getPalette = colorRampPalette(mycolors)
  
  
  ### now plot using ids1 instead of ids.
  p1<-ggplot(x.ratingsmelt, aes(x = event, y = value, col=ids1, linetype=ids1)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values=ltypes) +
    ylab("Glicko Rating") +
    xlab("Event") +
    ggtitle(thetitle)+
    ylim(ylim1,ylim2)+
    geom_line(lwd = linewd) +
    theme(plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)), 
          panel.background = element_blank(), 
          plot.background = element_blank(), 
          panel.grid.major.y = element_line(color = "gray75",linetype = 'dotted'), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor = element_blank(), 
          strip.background  = element_blank(),
          strip.text = element_text(size=rel(1.1)),
          text = element_text(color="gray20", size=10),
          axis.text = element_text(size=rel(1.0)),
          axis.text.x = element_text(color="gray20", size=rel(1.0)),
          axis.text.y = element_text(color="gray20", size=rel(1.0)),
          axis.title.x = element_text(size=rel(1.0), vjust=0),
          axis.title.y = element_text(size=rel(1.0), vjust=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  
  
  return(p1)
}


ttri_lastN <- function(df, N=3){
  
  library(dplyr)
  df %>% 
    filter(score==1) %>% 
    mutate(groupid = paste0(pmin(Actor, Recipient),pmax(Actor, Recipient))) %>%
    group_by(groupid) %>% 
    arrange(event) %>% 
    do(tail(., n=N[1]))%>%
    data.frame %>% 
    select(Actor,Recipient) %>%
    get_wl_matrix() %>%
    ttri() %>%
    .$ttri
}


### despotism
despotism <- function(x) {
  rev(sort(round(100*(rowSums(x)/sum(x)),2)))
}