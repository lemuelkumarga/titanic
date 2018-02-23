
# Initialization ----
## ---- init

# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","randomForest","tidyr","leaflet","purrr","grDevices","pander")
load_or_install.packages(packages)

data_dir <- "data/"

# Load some helper functions
source("shared/helper.R")


## ---- end-of-init

# DATA EXPLORATION ### ----

# * Data Overview ----
## ---- data_comp
training_set <- read.csv(paste0(data_dir, "train.csv"))

cols_summary <- data_overview(training_set)

## ---- end-of-data_comp

# * Hypothesis: High Income, High Survivalhood ----
## ---- exp_hypo1

income_set <- training_set %>%
              group_by(Pclass) %>%
              summarise(CohortSize = n(),
                        SurvivalRate = sum(Survived)/n())

income_plot <-  ggplot(income_set, aes(x=SurvivalRate,
                                      y = 0,
                                      color=as.factor(Pclass),
                                      size=CohortSize)) +
                theme_lk() +
                theme(plot.margin = unit(c(0,0,0,-40),'pt')) +
                # Legends
                theme(
                  legend.position = c(0.5,0.2),
                  legend.box.just = c(0.5,0.5),
                  legend.justification = c(0.5,0.5)
                ) +
                scale_color_manual(name = "Passenger Class",
                                  labels = c("1"="1 (High-Income)","2"="2 (Medium-Income)","3"="3 (Low-Income)"),
                                  values = c("1"=as.character(get_color("green")),
                                             "2"=as.character(get_color("yellow")),
                                             "3"=as.character(get_color("red"))),
                                  guide = guide_legend(order = 1,
                                                       direction = "vertical",
                                                       override.aes=list(size=5))) +
               scale_size_continuous(name = "Cohort Size",
                                     range = c(5,20),
                                     guide = guide_legend(order = 2)) +
               # X Axis
                theme(
                  axis.line.x = element_line(colour=NA),
                  axis.ticks.x = element_line(colour=NA),
                  axis.title.x = element_text(colour=NA),
                  axis.text.x = element_text(colour=NA)
                ) +
               geom_segment(aes(x=0.2, xend=0.7, y=0, yend=0),
                            size = 0.5,
                            color=ltxt_color,
                            arrow = arrow(length = unit(10,"pt"),
                                          type = "closed")) +
               geom_text(label = "Survival Likelihood",
                         x = 0.7,
                         y = 0.02,
                         family = def_font,
                         color = ltxt_color,
                         size = 5,
                         hjust = 1) +
               # Y Axis
                theme(
                  axis.line.y = element_line(colour=NA),
                  axis.ticks.y = element_line(colour=NA),
                  axis.title.y = element_text(colour=NA),
                  axis.text.y = element_text(colour=NA)
                ) +
               scale_y_continuous(limits=c(-0.12, 0.05)) +
               # Elements
                geom_point(alpha=0.5) +
                geom_text(aes(y = -0.03, label=paste0(round(SurvivalRate*100,0),"%")),
                          size=4,
                          color=txt_color,
                          family=def_font)

income_plot
## ---- end-of-exp_hypo1

# * Hypothesis: High Fares, High Income, High Survivalhood ----
## ---- exp_hypo1_p2

appendFarePUnit <- function(dt) { dt %>%  mutate(Fare_P_Unit = Fare / (1 + SibSp + Parch)) }

fares_set <- appendFarePUnit(training_set) %>%
             select(Fare_P_Unit, Pclass, Survived)

rows_p_fare_bin <- 80
fares_pdata <- fares_set %>%
               arrange(Fare_P_Unit) %>%
               mutate(Group = as.integer((row_number() - 1) / rows_p_fare_bin)+1) %>%
               group_by(Group) %>%
               summarise(FareMax = min(Fare_P_Unit),
                         Survived = sum(Survived),
                         CohortSize = n()) %>%
               mutate(SurvivalRate = Survived / CohortSize)
fares_pdata <- rbind(fares_pdata, c(0,0,0,0,0), c(max(fares_pdata$Group)+1,max(fares_set$Fare_P_Unit),0,0,0)) %>%
               arrange(Group)

fares_plot <- ggplot(fares_pdata, aes(x = FareMax,
                                      y = SurvivalRate)) +
              theme_lk() +
              xlab("Fares / Head") +
              scale_x_continuous(labels = scales::dollar) +
              ylab("Survival Likelihood") +
              scale_y_continuous(labels = scales::percent) +
              geom_step(color = get_color(1))

fares_plot
## ---- end-of-exp_hypo1_p2

# * Hypothesis: Gender and Title ----
## ---- exp_hypo2
prefix <- function(x) { unlist(strsplit(unlist(strsplit(x, ", "))[2],"\\. "))[1] }

gender_set <- training_set %>%
              mutate(Prefix = as.character(lapply(Name, prefix))) %>%
              select(Prefix, Sex)
prefix_totals <- gender_set %>%
                 group_by(Prefix) %>%
                 summarise(total = n())
gender_sex_totals <- gender_set %>%
                     group_by(Prefix, Sex) %>%
                     summarise(val=n()) %>%
                     left_join(prefix_totals, by=c("Prefix")) %>%
                     mutate(
                       pct = ifelse(Sex == "female",1.,-1.) * val / total
                     ) %>%
                     arrange(desc(val))
gender_sex_totals$Prefix <- factor(gender_sex_totals$Prefix, levels=unique(gender_sex_totals$Prefix))

gender_plot <- ggplot(gender_sex_totals, aes(x = Prefix,
                                             y = Sex,
                                             fill = Sex,
                                             alpha = val)) +
               theme_lk() +
               scale_fill_manual(values=c("female"=as.character(get_color("red")),
                                           "male"=as.character(get_color("blue"))),
                                    guide = "none") +
               scale_alpha_continuous(limits = c(0,25),
                                       range = c(0.3,1),
                                       name = "Cohort Size",
                                       guide = guide_legend(
                                         nrow = 1,
                                         override.aes=list(fill=ltxt_color)
                                       )) +
                xlab("Title") +
                scale_x_discrete(expand = c(0, 0)) +
                scale_y_discrete(expand = c(0, 0),
                                 labels = c("female"="F","male"="M")) +
                ylab("Gender") +
                geom_tile(colour = bg_color, size = 5)

gender_plot

## ---- end-of-exp_hypo2

# * Hypothesis: Title and Survivalhood ----
## ---- exp_hypo3

appendTitle <- function(data) {
  data %>%
  mutate(Title = as.character(lapply(Name, prefix))) %>%
  mutate(Title = ifelse(Title %in% c("Mr","Mrs","Miss","Master"), Title,
                        ifelse(Sex == "male","Rare Title / M", "Rare Title / F")))
}

title_set <- appendTitle(training_set) %>%
             select(Title, Sex, Survived) %>%
             group_by(Title, Sex) %>%
             summarise(survive = sum(Survived) / n()) %>%
             arrange(desc(survive))

title_set$Title <- factor(title_set$Title, levels=c(title_set$Title))

title_plot <- ggplot(title_set, aes(x=Title,
                                    y=survive,
                                    fill=Sex)) +
              theme_lk() +
              scale_fill_manual(name = "Gender",
                                  label = c("male"="Male", "female"="Female"),
                                  values = c("male"= as.character(get_color("blue")),
                                             "female" = as.character(get_color("red"))),
                                   guide = guide_legend()) +

              xlab("Title") +
              ylab("Survival Likelihood") +
              scale_y_continuous(labels = scales::percent,
                                 # Make Sure Bar and X-Axis Stick Together
                                 expand = c(0,0)) +
              geom_bar(stat="identity",
                       width = .5) +
              geom_text(aes(y = survive - 0.08,
                            label=paste0(round(survive*100),"%")),
                        color = "#FFFFFF",
                        family=def_font,
                        size=4)

title_plot

## ---- end-of-exp_hypo3

########## HYPOTHESIS 4 : AGE AND SURVIVALHOOD ####################

## ---- exp_hypo4

age_set <- training_set %>%
           select(Age, Survived) %>%
           filter(Age > 0)

rows_p_age_bin <- 50
age_pdata <- age_set %>%
                 arrange(Age) %>%
                 mutate(Group = as.integer((row_number() - 1)/rows_p_age_bin) + 1) %>%
                 group_by(Group) %>%
                 summarise(AgeMin = min(Age),
                           Survived = sum(Survived),
                           CohortSize = n(),
                           SurviveL = Survived/CohortSize)
age_pdata <- rbind(age_pdata,
                   c(0,0,0,0,0),
                   c(max(age_pdata$Group)+1,max(age_set$Age),0,0,0)) %>%
             arrange(Group)

age_plot <- ggplot(age_pdata, aes(x=AgeMin,
                                      y=SurviveL)) +
            theme_lk() +
            xlab("Age") +
            scale_x_continuous(breaks=seq(0,max(age_pdata$AgeMin),by=10)) +
            ylab("Survival Likelihood") +
            scale_y_continuous(labels = scales::percent,
                               expand = c(0.4, -0.15)) +
                  # Actual Data
                  geom_step(color=get_color(1)) +
                  geom_vline(data = age_pdata %>%
                                    filter(row_number() %in% c(3,16)),
                             aes(xintercept = AgeMin),
                             linetype = "dashed",
                             color = ltxt_color) +
                  geom_text(data = age_pdata %>%
                                   filter(row_number() %in% c(3,16)),
                            aes(x = AgeMin + 6, label = paste(AgeMin, "Years Old")),
                            y = max(age_pdata$SurviveL),
                            color = ltxt_color,
                            size = 5,
                            family = def_font)

age_plot

## ---- end-of-exp_hypo4

########## HYPOTHESIS 5 : COMPANY AND SURVIVALHOOD ####################

## ---- exp_hypo5

company_set <- training_set %>%
                select(SibSp, Parch, Survived) %>%
                group_by(SibSp, Parch) %>%
                summarise(CohortSize = n(),
                          SurvivalRate = sum(Survived)/n())

company_plot <- ggplot(company_set, aes(x = as.factor(Parch),
                                        y = as.factor(SibSp),
                                        fill = SurvivalRate,
                                        alpha = CohortSize)) +
                theme_lk() + 
                  ########### LEGENDS ###########
                  scale_fill_gradientn(name = "Survival Likelihood",
                                       colours=c(as.character(get_color("red")),
                                                 as.character(get_color("red")),
                                                 as.character(get_color("yellow")),
                                                 as.character(get_color("green")),
                                                 as.character(get_color("green"))),
                                       values = c(0.0,0.4,0.5,0.6,1.0),
                                       guide = "none") +
                  scale_alpha_continuous(limits = c(0,25),
                                         name = "Cohort Size",
                                         guide = guide_legend(
                                           nrow = 1,
                                           override.aes=list(fill=ltxt_color)
                                         )) +
                  ########### X-AXIS ###########
                  xlab("# of Different-Age Company (e.g. Children / Parents)") +
                  scale_x_discrete(expand = c(0, 0)) +
                  ########### Y-AXIS ###########
                  ylab("# of Same-Age Company (e.g. Spouse / Siblings)") +
                  ########### ELEMENTS ###########
                geom_tile(colour = bg_color, size = 7) +
                geom_text(aes(label = paste0(round(SurvivalRate*100),"%")),
                          color = "white",
                          alpha = 1,
                          size = 5,
                          family = def_font)

company_plot

## ---- end-of-exp_hypo5

########## HYPOTHESIS 6 : COMPANY AND TICKETS ####################

## ---- exp_hypo6

cleanTickets <- function(x) { toupper(gsub("[[:punct:]]|[[:space:]]","",x)) }

com_ticket_set <- training_set %>%
                  mutate(TicketParsed = sapply(Ticket, cleanTickets))

ticket_sum_set <- com_ticket_set %>%
                  group_by(TicketParsed) %>%
                  summarise(Size = n())

com_ticket_set <- com_ticket_set %>%
                  left_join(ticket_sum_set, by=c("TicketParsed")) %>%
                  mutate( OtherCompany = SibSp + Parch,
                          OtherTickets = Size - 1) %>%
                  group_by(OtherCompany, OtherTickets) %>%
                  summarise(CohortSize = n())
com_ticket_cor <- cor(com_ticket_set$OtherCompany, com_ticket_set$OtherTickets)

com_ticket_plot <- ggplot(com_ticket_set, aes(x = OtherCompany,
                                              y = OtherTickets,
                                              alpha = CohortSize)) +
                  theme_lk() + 
                  ########### LEGENDS ###########
                  scale_alpha_continuous(name = "Cohort Size",
                                         limits = c(0,50),
                                         guide = guide_legend(
                                           nrow = 1,
                                           override.aes=list(fill=ltxt_color)
                                         )) +
                  ########### X-AXIS ###########
                  xlab("# Company") +
                  scale_x_continuous(labels = scales::comma) +
                  ########### Y-AXIS ###########
                  ylab("# Other Individuals with Same Ticket") +
                  ########### ELEMENTS ###########
                  geom_tile(fill = get_color(1)) +
                  geom_text(label = paste0("'Correlation'~rho~': ",round(com_ticket_cor*100),"%'"),
                            data = data.frame(1),
                            x = 10,y = 6, hjust = 1,
                            family = def_font, size = 5,
                            alpha = 1, color = txt_color,
                            parse=TRUE)

com_ticket_plot

## ---- end-of-exp_hypo6

########## HYPOTHESIS 7 : CABIN ####################

## ---- exp_hypo7

getCabinFloor <- function(x) {

  if (x == "") { return("NA") }

  ltr <- unique(strsplit(gsub("[0-9]","",x)," ")[[1]])
  if (length(ltr) == 1) {
    ltr
  } else {
    "Multiple"
  }

}

getCabinNumber <- function(x) {

  if (x == "") { return("NA") }

  num <- as.integer(strsplit(gsub("[A-z]","",x)," ")[[1]]) %% 2

  if (is.nan(mean(num))) {
    return("NA")
  } else if (length(unique(num)) == 1) {
    if (unique(num) == 0) { return("Even") } else { return("Odd") }
  } else {
    return("Mixed")
  }

}

getCabinCount <- function(x) {
  if (x == "") { return("NA") }
  length(strsplit(x," ")[[1]])
}

appendCabinInfo <- function(data) {
  data %>%
  mutate(
    CabinFloor = sapply(Cabin, getCabinFloor),
    CabinNumber = sapply(Cabin, getCabinNumber),
    CabinCount = sapply(Cabin, getCabinCount)
  )
}

cabin_set <- appendCabinInfo(training_set) %>%
             select(CabinFloor,CabinNumber,CabinCount, Survived)

## ---- end-of-exp_hypo7

## ---- exp_hypo7_p1

cabinLet_set <- cabin_set %>%
                group_by(CabinFloor) %>%
                summarise(CohortSize = n(),
                          SurvivalRate = sum(Survived)/n())

cabinLet_plot <- ggplot(cabinLet_set, aes(x=as.factor(CabinFloor),
                                          y=SurvivalRate,
                                          alpha = CohortSize,
                                          fill = SurvivalRate)) +
                  theme_lk() + 
                    ########### LEGENDS ###########
                  scale_alpha_continuous(name = "Cohort Size",
                                         limits = c(0,30),
                                         guide = guide_legend(
                                           nrow = 1,
                                           override.aes=list(fill=ltxt_color)
                                         )) +
                  scale_fill_gradientn(colours=c(as.character(get_color("red")),
                                                 as.character(get_color("red")),
                                                 as.character(get_color("yellow")),
                                                 as.character(get_color("green")),
                                                 as.character(get_color("green"))),
                                       values = c(0.0,0.4,0.5,0.6,1.0),
                                       guide = "none") +
                    ########### X-AXIS ###########
                    xlab("Cabin Floor") +
                    ########### Y-AXIS ###########
                    ylab("Survival Likelihood") +
                    scale_y_continuous(labels = scales::percent,
                                       # Make Sure Bar and X-Axis Stick Together
                                       expand = c(0,0),
                                       limits =c(0,1)) +
                    ########### ELEMENTS ###########
                  # Actual Data
                  geom_bar(stat="identity") +
                    geom_text(aes(y = SurvivalRate - 0.08,
                                  label=paste0(round(SurvivalRate*100),"%")),
                              color = "#FFFFFF", alpha = 1,
                              family=def_font,size=4)

cabinLet_plot

## ---- end-of-exp_hypo7_p1

## ---- exp_hypo7_p2

cabinNumber_set <- cabin_set %>%
                   group_by(CabinNumber) %>%
                    summarise(CohortSize = n(),
                              SurvivalRate = sum(Survived)/n())
cabinNumber_set$CabinNumber <- factor(cabinNumber_set$CabinNumber, levels=c("Even","Odd","Mixed","NA"))

cabinNumber_plot <- ggplot(cabinNumber_set, aes(x = SurvivalRate,
                                                y = 0,
                                                color=CabinNumber,
                                                size=CohortSize)) +
                    theme_lk() +
                    ########### LEGENDS ###########
                      theme(plot.margin = unit(c(0,0,0,-40),'pt')) +
                        # Legends
                        theme(
                          legend.position = c(0.5,0.2),
                          legend.box.just = c(0.5,0.5),
                          legend.justification = c(0.5,0.5)
                        ) +
                      scale_color_manual(name = "Room Type",
                                         values = get_color(),
                                         guide = guide_legend(order = 1,
                                                              direction = "vertical",
                                                              override.aes=list(size=5),
                                                              ncol= 2)) +
                      scale_size_continuous(name = "Cohort Size",
                                            range = c(5,20),
                                            guide = guide_legend(order = 2)) +
                      ########### X-AXIS ###########
                    theme(
                      axis.line.x = element_line(colour=NA),
                      axis.ticks.x = element_line(colour=NA),
                      axis.title.x = element_text(colour=NA),
                      axis.text.x = element_text(colour=NA)
                    ) +
                      # Add X Axis Line
                      geom_segment(aes(x=0.25, xend=0.8, y=0, yend=0),
                                   size = 0.5,
                                   color=ltxt_color,
                                   arrow = arrow(length = unit(10,"pt"),
                                                 type = "closed")) +
                      geom_text(label = "Survival Likelihood",
                                x = 0.8,
                                y = 0.02,
                                family = def_font,
                                color = ltxt_color,
                                size = 5,
                                hjust = 1) +
                      ########### Y-AXIS ###########
                    theme(
                      # Y-Axis
                      axis.line.y = element_line(colour=NA),
                      axis.ticks.y = element_line(colour=NA),
                      axis.title.y = element_text(colour=NA),
                      axis.text.y = element_text(colour=NA)) +
                      scale_y_continuous(limits=c(-0.12, 0.05)) +
                      ########### ELEMENTS ###########
                    geom_point(alpha=0.5) +
                      geom_text(aes(y = -0.03, label=paste0(round(SurvivalRate*100,0),"%")),
                                size=4,
                                color=txt_color,
                                family=def_font)


cabinNumber_plot

## ---- end-of-exp_hypo7_p2

## ---- exp_hypo7_p3
cabinCount_set <- cabin_set %>%
                  group_by(CabinCount) %>%
                  summarise(CohortSize = n(),
                            SurvivalRate = sum(Survived)/n())

cabinCount_plot <- ggplot(cabinCount_set, aes(x=as.factor(CabinCount),
                                              y=SurvivalRate,
                                              alpha = CohortSize,
                                              fill = SurvivalRate)) +
                   theme_lk() +
                    ########### LEGENDS ###########
                    scale_alpha_continuous(name = "Cohort Size",
                                           limits = c(0,50),
                                           guide = guide_legend(
                                             nrow = 1,
                                             override.aes=list(fill=ltxt_color)
                                           )) +
                    scale_fill_gradientn(colours=c(as.character(get_color("red")),
                                                   as.character(get_color("red")),
                                                   as.character(get_color("yellow")),
                                                   as.character(get_color("green")),
                                                   as.character(get_color("green"))),
                                         values = c(0.0,0.3,0.4,0.50,1.),
                                         guide = "none") +
                    ########### X-AXIS ###########
                    xlab("# Cabins Specified") +
                    ########### Y-AXIS ###########
                    ylab("Survival Likelihood") +
                    scale_y_continuous(labels = scales::percent,
                                       # Make Sure Bar and X-Axis Stick Together
                                       expand = c(0,0),
                                       limits =c(0,1)) +
                    ########### ELEMENTS ###########
                  # Actual Data
                  geom_bar(stat="identity",
                           width = 0.7) +
                  geom_text(aes(y = SurvivalRate - 0.1,
                                label=paste0("Cohort\nSize:\n",CohortSize)),
                            color = "#FFFFFF", alpha = 1,
                            family=def_font,size=4)

cabinCount_plot
## ---- end-of-exp_hypo7_p3

########## HYPOTHESIS 8 : EMBARKATION PORTS ####################

## ---- exp_hypo8

embark_set <- training_set %>%
              filter(Embarked != "") %>%
              group_by(Embarked) %>%
              summarise(CohortSize = n(),
                        SurvivalRate = sum(Survived)/n()) %>%
              mutate(
                Port = ifelse(Embarked == "C","Cherbough",
                              ifelse(Embarked == "Q","Queenstown","Southampton")),
                Lat = ifelse(Embarked == "C",49.645009,
                      ifelse(Embarked == "Q",51.851,50.9038684)),
                Lng = ifelse(Embarked == "C",-1.62444,
                      ifelse(Embarked == "Q",-8.2967,-1.4176118 ))
              )


# Create Color Palette
col_grad <- colorRamp(c(get_color("red"),get_color("yellow"), get_color("green")))
col_limits <- c(0.4,0.6)
colpal <- function(x) {
            col_list <- col_grad((min(max(x,col_limits[1]),col_limits[2])-col_limits[1])/(col_limits[2]-col_limits[1]))
            rgb(col_list[1],col_list[2],col_list[3], maxColorValue= 255)
          }
size_range <- c(10,15)

# Create Marker Icon
tit_icon <- awesomeIcons(
            icon = 'ship',
            iconColor = '#FFFFFF',
            library = 'fa',
            markerColor = 'gray'
          )

# Create Map
map <- leaflet(width = "100%") %>%
       addProviderTiles("CartoDB.Positron") %>%
        # Location of Titanic Crash
        addAwesomeMarkers(-50.2333324,41.7666636, icon=tit_icon,popup="Titanic Crash Site")

pwalk(
  list(embark_set$Port, embark_set$Lat, embark_set$Lng, embark_set$SurvivalRate, embark_set$CohortSize),
  function(port_name, lat, lng, srate, cohort) {
    map <<- map %>%
            addCircleMarkers(lng, lat,
                             popup=paste0(port_name, "<br>","Survival Likelihood: ",round(srate*100),"%"),
                             color = colpal(srate),
                             radius = max(min(cohort/20, size_range[2]),size_range[1]),
                             opacity = 0.8,
                             fillOpacity = 0.5)
  }
)

map

## ---- end-of-exp_hypo8

## ---- exp_hypo8_p2

# Assessing the correlation between Embarkation and Passenger Class

embark_pclass <- training_set %>%
                 select(Pclass, Embarked, Survived) %>%
                 filter(Embarked != "") %>%
                 group_by(Pclass, Embarked) %>%
                 summarise(CohortSize=n(),
                           SurvivalRate=sum(Survived)/n())
embark_pclass <- embark_pclass %>%
                 left_join(embark_pclass %>% group_by(Embarked) %>% summarise(TotalSize = sum(CohortSize)), by=c("Embarked")) %>%
                 mutate(PctEmbarked = CohortSize / TotalSize) %>%
                 arrange(Pclass, desc(PctEmbarked))
embark_pclass$Embarked <- factor(embark_pclass$Embarked, levels=unique(embark_pclass$Embarked))
embark_pclass$Pclass <- factor(embark_pclass$Pclass, levels=rev(unique(embark_pclass$Pclass)))

embark_pclass_plot <- ggplot(embark_pclass, aes(x=as.factor(Embarked),
                                                y=PctEmbarked,
                                                fill = as.factor(Pclass))) +
                      theme_lk() + 
                  ########### LEGENDS ###########
                  scale_fill_manual(name = "Passenger Class",
                     labels = c("1"="1 (High-Income)","2"="2 (Medium-Income)","3"="3 (Low-Income)"),
                     values = c("1"=get_color("green"),
                                "2"=alpha(get_color("yellow"),0.2),
                                "3"=alpha(get_color("red"),0.2)),
                     guide = guide_legend(reverse = TRUE)) +
                  ########### X-AXIS ###########
                  xlab("Port of Embarkation") +
                  scale_x_discrete(labels=c("S"="Southampton",
                                            "C"="Cherbough",
                                            "Q"="Queenstown")) +
                  ########### Y-AXIS ###########
                  ylab("Survival Likelihood") +
                  scale_y_continuous(labels = scales::percent,
                                     # Make Sure Bar and X-Axis Stick Together
                                     expand = c(0,0),
                                     limits =c(0,1)) +
                  ########### FLIP TO HORIZONTAL BAR CHART ###########
                  coord_flip() +
                  ########### ELEMENTS ###########
                  # Actual Data
                  geom_bar(stat="identity") +
                  geom_text(aes(label=paste0(round(PctEmbarked*100),"%"),
                                y = PctEmbarked + 0.05),
                            data = embark_pclass %>% filter(Pclass == 1),
                            color = get_color("green"),
                            family = def_font, size = 5)

embark_pclass_plot

## ---- end-of-exp_hypo8_p2

####################################################
#
# 3 - Data Preparation
#
###################################################

## ---- data_prep

### Create Age as Regression Parameters
age_data <- training_set %>%
            appendTitle() %>%
            filter(Age > 0)
age_m <- lm(Age ~ Title + SibSp + Parch + factor(Pclass), data = age_data)

clean_data <- function(data,is_training = TRUE) {
  output <- appendTitle(data) %>%
            appendCabinInfo()

  # Populate Age
  output <- output %>%
            mutate(AgePredict = predict.lm(age_m, output),
                   Age = ifelse(is.na(Age),AgePredict,Age))

  # Create Null Handler
  null_handler <- function(is_categorical = TRUE) {
    paste0('ifelse(is.na(x) | x %in% c("","NA"),', ifelse(is_categorical,'"Unknown"','-1'),',x)')

  }

  # Specify Features
  features <- c("Pclass"=FALSE,
                "Fare"=FALSE,
                "Sex"=TRUE,
                "Title"=TRUE,
                "Age"=FALSE,
                "SibSp"=FALSE,
                "Parch"=FALSE,
                "CabinFloor"=TRUE,
                "CabinNumber"=TRUE,
                "Embarked"=TRUE)
  mutate_fn <- sapply(1:length(features), function(x) {
                paste0("sapply(",names(features[x]),", function(x) { ",null_handler(features[x])," })")
              })

  output <- output %>%
            mutate_(
              .dots= setNames(mutate_fn, names(features))
            )

  # Specify factors for different features
  output$Sex <- factor(output$Sex, levels = c("male","female","Unknown"))
  output$Title <- factor(output$Title, levels = c("Mr","Mrs","Miss","Master","Rare Title / M","Rare Title / F"))
  output$CabinFloor <- factor(output$CabinFloor, levels = c("A","B","C","D","E","F","G","T","Multiple","Unknown"))
  output$CabinNumber <- factor(output$CabinNumber, levels = c("Odd","Even","Mixed","Unknown"))
  output$Embarked <- factor(output$Embarked, levels = c("S","C","Q","Unknown"))
  if (is_training) {
    output$Survived <- factor(output$Survived, levels = c(0,1))
  }

  # Specify columns to extract
  if(is_training) {
    columns <- c("Survived"=TRUE,features)
  } else {
    columns <- features
  }

  output %>% select_(.dots=names(columns))
}

cleaned_set <- clean_data(training_set)

## ---- end-of-data_prep

####################################################
#
# 4 - Train the Data using Random Forest
#
###################################################

## ---- data_model
set.seed(1)
rf_model <- randomForest(Survived ~ .,
                         data = cleaned_set,
                         importance=TRUE,
                         ntree=2000)

rf_model
importance(rf_model)

## ---- end-of-data_model

## ---- data_model_err_anal
p_y <- predict(rf_model, newdata = cleaned_set %>% select(-Survived))
err_out <-  cleaned_set %>%
            mutate(p_y = p_y) %>%
            filter(Survived != p_y)
## ---- end-of-data_model_err_anal
