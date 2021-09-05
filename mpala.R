library(dplyr)
library(lubridate)
library(ggplot2)
library(car)

## Functions
military_to_24 <- function(military) { # military is a number
  military_time <- paste0(military)
  split_military_time <- strsplit(military_time,"")[[1]]
  if (length(split_military_time) == 3) split_military_time <- c("0", split_military_time)
  split_hour24_time <- c(split_military_time[1], split_military_time[2], ":", split_military_time[3], split_military_time[4])
  if (split_hour24_time[1] == 0) split_hour24_time <- split_hour24_time[2:5]
  hour24_time <- paste(split_hour24_time, collapse = "")
  return(hour24_time)
}
find_replace <- function(vec, dictionary) {
  vec <- trimws(vec,which=c("right"))
  new_vec <- rep(NA,times=length(vec))
  colnames(dictionary) <- c("entry","replacement")
  dictionary <- rbind(dictionary,data.frame(entry=c(""),replacement=c("")))
  
  for (vid in 1:length(vec)) {
    v <- vec[vid]
    places <- match(v,dictionary$entry)
    
    if (is.na(places)) {
      cat("Warning: `",v,"` not found in supplied dictionary, returned without changing\n",sep="")
      new_vec[vid] <- v
    } else if (length(places) == 1) {
      new_vec[vid] <- dictionary[places[1],"replacement"]
    } else if (length(places) >= 2 && is.na(v)) {
        cat("Warning: `",v,"` found multiple places supplied dictionary, used first replacement\n",sep="")
        new_vec[vid] <- dictionary[places[1],"replacement"]
    } else if (length(places) >= 2 && !is.na(v) && v!="") {
      cat("Warning: `",v,"` found multiple places supplied dictionary, used first replacement\n",sep="")
      new_vec[vid] <- dictionary[places[1],"replacement"]
    }
  }
  return(new_vec)
}

is.before <- function(Date1, Date2) {
  return(time_length(interval(Date1,Date2),"day")>=0)
}
is.strictly.before <- function(Date1, Date2) {
  return(time_length(interval(Date1,Date2),"day")>0)
}
is.after <- function(Date1, Date2) {
  return(time_length(interval(Date1,Date2),"day")<=0)
}
is.strictly.after <- function(Date1, Date2) {
  return(time_length(interval(Date1,Date2),"day")<0) 
}
degrees_to_meters <- function(degrees) return(degrees*111468)
pad_zeros <- function(x,num) {
  if (num >= 26) return(NULL)
  temp <- paste0("0000000000000000000000000",x)
  return(substr(temp,nchar(temp)-1,nchar(temp)))
}




my.nova <- function(DF, Variable, Group, group_settings=list(valid=F)) {
  print(shapiro.test(DF[,Variable]))
  # cat("--- --- ---\n\n")
  # leveneTest(as.formula(paste(Variable, "~", Group)),DF)
  cat("--- --- ---\n\nOne-way ANOVA\n")
  print(summary(ANOVA <- aov(as.formula(paste(Variable, "~", Group)), data=DF)))
  cat("\n--- --- ---\n")
  print(kruskal.test(as.formula(paste(Variable, "~", Group)), data=DF))
  cat("\n--- --- ---\n\n")
  print(TukeyHSD(ANOVA))
  
  colours <- c(red, orange, yellow, green, blue)
  
  if (!group_settings$valid) {  
    ggplot(DF, aes_string(x=Group, y=Variable, fill=Group))+
      geom_violin(trim=FALSE)+
      geom_boxplot(width=0.1)+
      stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
      theme_classic()+
      scale_fill_manual(labels=unique(DF[,Group]), values=colours[1:length(unique(DF[,Group]))])+
      labs(x=Group,y=Variable)+
      theme(legend.position="none")
  } else if (group_settings$valid==T) {
    group_order <- group_settings$order
    group_limits <- group_settings$limits
    group_name <- group_settings$group_name
    variable_name <- group_settings$variable_name
    
    ggplot(DF, aes_string(x=Group, y=Variable, fill=Group))+
      geom_violin(trim=FALSE)+
      geom_boxplot(width=0.1)+
      stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
      theme_classic()+
      scale_x_discrete(limits=group_order,labels=group_limits)+
      scale_color_manual(name=group_name,values=colours[1:length(group_order)])+
      scale_fill_manual(labels=group_order, values=colours[1:length(group_order)])+
      labs(x=group_name,y=variable_name)+
      theme(legend.position="none")
  } else {
    cat("No plot rendered\n")
  }
}

duplicate.rows <- function(DF, Variable="Total.animals") {
  df.exp.1 <- matrix(nrow=1,ncol=ncol(DF))#as.data.frame(matrix(nrow=nrow(df.exp.1.temp),ncol=ncol(df.exp.1.temp)))
  for (dazzle in 1:nrow(DF)) {
    if (!is.na(DF[dazzle,Variable])) {
      for (i in 1:DF[dazzle,Variable]) {
        df.exp.1 <- rbind(df.exp.1,as.matrix(DF[dazzle,]))
      }
    }
  }
  df.exp.1 <- as.data.frame(df.exp.1)
  rownames(df.exp.1) <- NULL
  
  return(df.exp.1)
}
na.return.single <- function(x,alt) return(c(x,alt)[is.na(x)+1])
na.return <- function(vec,alt) {
  new_vec <- c()
  for (vid in 1:length(vec)) {
    new_vec[vid] <- na.return.single(vec[vid],alt)
  }
  return(new_vec)
}
nan.return.single <- function(x,alt) return(c(x,alt)[is.nan(x)+1])
nan.return <- function(vec,alt) {
  new_vec <- c()
  for (vid in 1:length(vec)) {
    new_vec[vid] <- nan.return.single(vec[vid],alt)
  }
  return(new_vec)
}
inf.return.single <- function(x,alt) return(c(x,alt)[is.infinite(x)+1])
inf.return <- function(vec,alt) {
  new_vec <- c()
  for (vid in 1:length(vec)) {
    new_vec[vid] <- inf.return.single(vec[vid],alt)
  }
  return(new_vec)
}

days_since_start <- function(date) return(as.numeric(date)-as.numeric(mdy("6/3/21"))+1)


# T.test <- function(DF, Variable, Group) {
#   DF.sample <- DF %>% sample_n(0.8*nrow(DF))
#   t.test(DF.sample[,val], c.sample[,val], paired = TRUE, alternative = "two.sided")
#   wilcox.test(z.sample[,val], c.sample[,val], paired = TRUE, alternative = "two.sided")
# }


## Names
cattle.abbr <- c("Cattle","CKC","CC","MC")
camel.abbr <- c("Camel","ZC","Comm_Camel")
zebra.abbr <- c("GZ","PZ")

cattle.names <- c("Cattle","Community Kaparo Cattle","Community Cattle","Mpala Cattle")
camel.names <- c("Camels","Zainab Camels","Community Camels")
zebra.names <- c("Grevy's zebras","Plains zebras")

bushland <- c("OG","LB","MB")
bush.names <- c("Open grassland","Light bush","Medium bush")


## Colours
red <- "#fc1303"
orange <- "#fc8403"
yellow <- "#f7d114"
green <- "#00c750"
blue <- "#033dfc" #1644db

og <- "#ffcc33"
lb <- "#a3c586"
mb <- "#5b7444"
tb <- "#162D1D"

gz <- "#a02923"
gz.2 <- "#cc3228"
pz <- "#fdfdf6"
pz.2 <- "#49a6d2"
cattle <- "#aaaaaa"
cattle.2 <- "#aaaaaa"
cattle.col <- "#aaaaaa"
camel <- "#333333"

ndvi.col <- "#4E1D41"
evi.col <- "#B7263D"

# e76f51 2a9d8f 264653 4E1D41 841F3F B7263D F7C543


before.col <- "#9FFCD6"
after.col <- "#8D867C"
while.col <- "#588A77"

phase3.abbr <- c("Before","While","After")
phase3.names <- c("Before phalanx arrival","As phalanx arrive","After phalanx arrival")

pastel.pink <- "#ffb7a1"
pastel.yellow <- "#efbc68"
pastel.sea <- "#c2d7d0"
pastel.green <- "#5f9595"
phase2.cols <- c(pastel.pink,pastel.yellow)
phase3.cols <- c(pastel.pink,pastel.yellow,pastel.green)

grazing <- "#cb997e"
resting <- "#ddbea9"
drinking <- "#b7b7a4"
walking <- "#a5a58d"
standing <- "#6b705c"

grazing.2 <- "#264653"
resting.2 <- "#2a9d8f"
drinking.2 <- "#e9c46a"
walking.2 <- "#f4a261"
standing.2 <- "#e76f51"
activity.cols.2 <- c(grazing.2,walking.2,resting.2,standing.2,drinking.2)

activity.cols.3 <- c(
  "#1f2041",
  "#4b3f72",
  "#ffc857",
  "#119da4",
  "#19647e"
)


multispecies.names <- c("Independent groups","Mixed groups")
ms0 <- "#FF8576"
ms1 <- "#9B7282"
multispecies.cols <- c(ms0,ms1)


## Variables
lat_line <- 0.409859

get_df <- function() return(read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv"))
get_ss <- function() return(read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/StatisticsWhitesheets.csv"))
get_hb <- function() return(read.csv("/Users/maxgotts/Desktop/MPALA/Maps/HabitatTIFF.csv"))
get_vi <- function() return(read.csv("/Users/maxgotts/Desktop/MPALA/Maps/VITIFF.csv"))
get_vi_l12 <- function() return(read.csv("/Users/maxgotts/Desktop/MPALA/Maps/Loops_1_2_VITIFF.csv"))


blacklist.archive <- c("GZ5","GZ78","PZ91","PZ101","PZ28","CT80","CT63",
               "CT76","CT63","CM10","CT76","CM18","CT80","GZ5",
               "PZ110","PZ28","PZ110","GZ5","PZ28")
blacklist <- c(
  "CT63", "CM10","PZ161", # Longitude >= 36.89
  "PZ28", "GZ78", "CT76", "CM18", "PZ110", "CT80", # Longitude <= 36.82
  "GZ5", "PZ91", "PZ101", "CT85", "PZ121", "GZ99",  "GZ102", "CT140", # Latitude <= 0.44
  "CT52", "CT55", "CT96", "PZ133", "PZ139", "CT130", "CT131", "CT150", "CT151", "CM41", "CT166" # Latitude >= 0.52
)

# MODIS through website
# start_date <- c('05-13','06-04')
# end_date <- c('06-04','06-15')
# vi_dates <- data.frame(start=start_date,end=end_date)

# VI start dates based on LANDSAT radical GEE processing (more temporally accurate VI data)
# start_date <- c('05-13','06-14','06-30')
# end_date <- substr(c((as.numeric(ymd(paste0("2021-",start_date[2:length(start_date)])))-1)+as.Date(0, origin="1970-01-01"),ymd("2021-12-31")),6,10)
#     # Make end dates predate next start date by 1 day
# vi_dates <- data.frame(start=start_date,end=end_date)


# VI start dates based on MODIS radical GEE processing (more temporally accurate VI data)
start_date <- c('05-09', '05-25', '06-10', '06-26')
end_date <- substr(c((as.numeric(ymd(paste0("2021-",start_date[2:length(start_date)])))-1)+as.Date(0, origin="1970-01-01"),ymd("2021-12-31")),6,10)
# Make end dates predate next start date by 1 day
vi_dates <- data.frame(start=start_date,end=end_date)


if (FALSE) {
  source('~/Desktop/MPALA/mpala.R')
}
