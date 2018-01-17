library(devtools)
install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)
View(england)
library(dplyr)

data(package="engsoccerdata")    # lists datasets currently available

head(england)

maketable <- function(df=NULL, Season=NULL, tier=NULL, pts=3){
  
  GA<-GF<-ga<-gf<-gd<-GD<-D<-L<-W<-Pts<-.<-Date<-home<-team<-visitor<-hgoal<-opp<-vgoal<-goaldif <-FT<-division<-result<-maxgoal<-mingoal<-absgoaldif<-NULL
  
  dfx <- df[(df$Season==Season & df$tier==tier),]
  
  
  temp <-rbind(
    dfx %>%
      dplyr::select(team=home, opp=visitor, GF=hgoal, GA=vgoal),
    dfx %>%
      dplyr::select(team=visitor, opp=home, GF=vgoal, GA=hgoal)
  ) %>%
    dplyr::mutate(GD = GF-GA) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(GP = sum(GD<=100),
                     W = sum(GD>0),
                     D = sum(GD==0),
                     L = sum(GD<0),
                     gf = sum(GF),
                     ga = sum(GA),
                     gd = sum(GD)
    ) %>%
    dplyr::mutate(Pts = (W*pts) + D) %>%
    dplyr::arrange(-Pts, -gd, -gf) %>%
    dplyr::mutate(Pos = rownames(.)) %>%
    as.data.frame()
  
  return(temp)
}

Season_2013 <- (maketable(df=england,Season=2013,tier=1,pts=3))
colnames(Season_2013) <- c("Team", "Games Played", "Win","Draw","Lost","Goals Scored","Goals Given","Goal Difference","Points","Position")

View(Season_2013)
#write.csv(Season_2013,file="Season_2013.csv")

list_tables <- list()
df <- as.data.frame(NULL)
df_manu <- as.data.frame(NULL)
counter <- seq(1888,2016,seq=1)


for (i in 1888:2016)
{
 list_tables[[i]] <- data.frame(maketable(df=england,Season=i,tier=1,pts=3))
 vec <- c(i,sum(list_tables[[i]]$D),sum(list_tables[[i]]$GP))
 df <- rbind(df,vec)
}

View(list_tables[[2000]])

colnames(df)<- c("Season","Draws","Games Played")


 df <- df[which(df$Draws >0),]


library(ggplot2)
plotter <- ggplot(df, aes(df$Season,df$Draws/df$`Games Played`)) + geom_point() + geom_smooth()
print(plotter+labs(title= "Proportion of Draws per Season (1888 - 2016)",
                   y="Draws", x = "Season"))


df_manu <- as.data.frame(NULL)

vec_manu <- NULL

for(j in 1888:2016)
  {
  
  
  
    
  
  
  df_trial <- maketable(df=england,Season=j,tier=1,pts=3)
  df_trial <- cbind(df_trial,rep(j,nrow(df_trial)))
  
  df_manu <- rbind(df_manu,df_trial[which(df_trial$team=="Manchester United"),])
  
}



colnames(df_manu)[11]<- "Season"

#write.csv(df_manu,file="Manchester_United.csv")
labels_manu_season <- as.vector(df_manu$Season)
labels_try <- sprintf("%04i", labels_manu_season)


cols <- c("blue", "red")[(df_manu$Season > 1986 & df_manu$Season < 2014)+1]

#cols <- c("red","blue")[(df_manu$Season < 2014)+1]
#cols

barplot((df_manu$W/df_manu$GP),names.arg=labels_try,col=cols,ylim = c(0.0,0.8),
        main="Win Percentage - Manchester United (1888-2016)", ylab = "Win %age", xlab = "Season"
        )



abline(v=107,lwd=5,col="yellow")
abline(v=74,lwd=5,col="yellow")



df_chel <- as.data.frame(NULL)

for(j in 1888:2016)
{
  
  
  
  
  
  
  df_trial <- maketable(df=england,Season=j,tier=1,pts=3)
  df_trial <- cbind(df_trial,rep(j,nrow(df_trial)))
  
  df_chel <- rbind(df_chel,df_trial[which(df_trial$team=="Chelsea"),])
  
}

colnames(df_chel)[11]<- "Season"

#write.csv(df_chel,file="Chelsea.csv")

labels_chel_season <- as.vector(df_chel$Season)
labels_try_chel <- sprintf("%04i", labels_chel_season)

cols <- c("blue", "red")[(df_chel$Season < 2003)+1]
barplot((df_chel$W/df_chel$GP),col=cols,names.arg=labels_try_chel,ylim = c(0.0,0.8),
        main="Win Percentage - Chelsea (1888-2016)", ylab = "Win %age", xlab = "Season")



abline(v=81,lwd=5,col="yellow")

df_manc <- as.data.frame(NULL)

for(j in 1888:2016)
{
  
  
  
  
  
  
  df_trial <- maketable(df=england,Season=j,tier=1,pts=3)
  df_trial <- cbind(df_trial,rep(j,nrow(df_trial)))
  
  df_manc <- rbind(df_manc,df_trial[which(df_trial$team=="Manchester City"),])
  
}

colnames(df_manc)[11]<- "Season"

#write.csv(df_manc,file="Manchester_City.csv")

labels_manc_season <- as.vector(df_manc$Season)
labels_try_manc <- sprintf("%04i", labels_manc_season)

cols <- c("deepskyblue", "red")[(df_manc$Season<2008)+1]

barplot((df_manc$W/df_manc$GP),col=cols,names.arg=labels_try_manc,ylim = c(0.0,0.8),
        main="Win Percentage - Manchester City (1888-2016)", ylab = "Win %age", xlab = "Season")

abline(v=95,lwd=5,col="yellow")


##Home Advantage Pattern

maketable_all <- function(df=NULL, Season=NULL, tier=NULL, pts=3, begin=NULL, end=NULL, type = c("both", "home", "away")){
  
  GA<-GF<-ga<-gf<-gd<-GD<-D<-L<-W<-Pts<-.<-Date<-home<-team<-visitor<-hgoal<-opp<-vgoal<-goaldif <-FT<-division<-result<-maxgoal<-mingoal<-absgoaldif<-NULL
  
  #season/tier
  if(!is.null(Season) & is.null(tier)) {
    dfx <- df[(df$Season == Season), ]
  } else if(is.null(Season) & !is.null(tier)) {
    dfx <- df[(df$tier == tier), ]
  } else if(!is.null(Season) & !is.null(tier)) {
    dfx <- df[(df$Season == Season & df$tier == tier), ]
  } else {
    dfx <- df
  }
  
  #dates
  if(!is.null(begin) & is.null(end)) {
    dfx <- dfx[(dfx$Date >= begin & dfx$Date <= end), ]
  } else if(is.null(begin) & !is.null(end)) {
    dfx <- dfx[(dfx$Date <= end), ]
  } else if(!is.null(begin) & !is.null(end)) {
    dfx <- dfx[(dfx$Date >= begin), ]
  }
  
  
  #subset only home or away fixtures, if applicable
  if(match.arg(type)=="home") {
    temp <- dplyr::select(dfx, team=home, opp=visitor, GF=hgoal, GA=vgoal)
  } else if(match.arg(type)=="away") {
    temp <- dplyr::select(dfx, team=visitor, opp=home, GF=vgoal, GA=hgoal)
  } else if(match.arg(type)=="both") {
    temp <-rbind(
      dplyr::select(dfx, team=home, opp=visitor, GF=hgoal, GA=vgoal),
      dplyr::select(dfx, team=visitor, opp=home, GF=vgoal, GA=hgoal)
    )
  }
  
  temp  %>%
    dplyr::mutate(GD = GF - GA) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(GP = sum(GD <= 100), W = sum(GD > 0), D = sum(GD == 0), L = sum(GD < 0), gf = sum(GF), ga = sum(GA), gd = sum(GD)) %>%
    dplyr::mutate(Pts = (W * pts) + D) %>%
    dplyr::arrange(-Pts, -gd, -gf) %>%
    dplyr::mutate(Pos = rownames(.)) %>%
    as.data.frame() -> temp
  
  return(temp)
}

df_all <- NULL

for(k in 1888:2016)
{
  
  
  
  
  
  
  df_trial <- maketable_all(df=england[england$tier==1,],Season=k, type="home")
  val <- c(k,sum(df_trial$GP),sum(df_trial$W))
  m1 <- matrix(val, ncol=3, byrow=TRUE)
  df_test <- as.data.frame(m1)
  
  df_all <- rbind(df_all,df_test)
  
  
  
}

colnames(df_all) <- c("Season","Games Played at Home","Games Won")

df_all$WinPer <- df_all$`Games Won`/df_all$`Games Played at Home`



df_all <- df_all[which(df_all$`Games Won` > 0),]


library(ggplot2)
plotter <- ggplot(df, aes(df_all$Season,df_all$WinPer)) + geom_point() + geom_smooth()
print(plotter+labs(title= "Proportion of Games Won At Home Ground (1888 - 2016)",
                   y="Win %age at Home Ground", x = "Season"))

