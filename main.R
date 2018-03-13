
# Initialization ----
## ---- init

# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","randomForest","tidyr","leaflet","purrr","grDevices","pander")
load_or_install.packages(packages)

data_dir <- "data/"
output_dir <- "output/"

# Load some helper functions
source("shared/helper.R")


## ---- end-of-init

# DATA EXPLORATION ### ----

# * Data Overview ----
## ---- data_comp
training_set <- read.csv(paste0(data_dir, "train.csv"))

cols_summary <- data_overview(training_set)

## ---- end-of-data_comp

# * INSIGHT 1: High Income, High Survivalhood ----
## ---- exp_income

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
                                             "2"=as.character(get_color("green",0.5)),
                                             "3"=as.character(get_color("red"))),
                                  guide = guide_legend(order = 1,
                                                       direction = "vertical",
                                                       override.aes=list(size=5))) +
               scale_size_continuous(name = "Cohort Size",
                                     range = c(5,20),
                                     guide = guide_legend(order = 2,
                                                          override.aes = list(alpha=0.5))) +
               # X Axis
                theme(
                  axis.line.x = element_line(colour=NA),
                  axis.ticks.x = element_line(colour=NA),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank()
                ) +
               geom_segment(data = data.frame(1),
                            aes(x=0.2, xend=0.7, y=0, yend=0),
                            size = 0.5,
                            color=ltxt_color,
                            arrow = arrow(length = unit(10,"pt"),
                                          type = "closed")) +
               geom_text(data = data.frame(1),
                         label = "Survival Likelihood",
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
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank()
                ) +
               scale_y_continuous(limits=c(-0.12, 0.05)) +
               # Elements
                geom_point() +
                geom_text(aes(y = -0.03, label=paste0(round(SurvivalRate*100,0),"%")),
                          size=4,
                          color=txt_color,
                          family=def_font)

## ---- end-of-exp_income

## ---- exp_fare

fares_set <- training_set %>% 
             select(Fare, Pclass, Survived)

rows_p_fare_bin <- 100
fares_pdata <- fares_set %>%
               arrange(Fare) %>%
               mutate(Group = as.integer((row_number() - 1) / rows_p_fare_bin)+1) %>%
               group_by(Group) %>%
               summarise(FareMax = min(Fare),
                         Survived = sum(Survived),
                         CohortSize = n()) %>%
               mutate(SurvivalRate = Survived / CohortSize)
fares_pdata <- rbind(fares_pdata, c(0,0,0,0,0), c(max(fares_pdata$Group)+1,max(fares_set$Fare),0,0,0)) %>%
               arrange(Group)

fares_plot <- ggplot(fares_pdata, aes(x = FareMax,
                                      y = SurvivalRate)) +
              theme_lk() +
              xlab("Passenger Fares") +
              scale_x_continuous(labels = scales::dollar) +
              ylab("Survival Likelihood") +
              scale_y_continuous(labels = scales::percent) +
              geom_step(color = get_color(1))

## ---- end-of-exp_fare

# * INSIGHT 2:  Gender and Title ----
## ---- exp_titles
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
gender_sex_totals$Prefix <- substr(gender_sex_totals$Prefix,1,4)
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

## ---- end-of-exp_titles

## ---- exp_title_gender

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

## ---- end-of-exp_title_gender

# * INSIGHT 3 : AGE AND SURVIVALHOOD ----

## ---- exp_age

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
                            aes(x = AgeMin + 8.5, label = paste(AgeMin, "Years Old")),
                            y = max(age_pdata$SurviveL),
                            color = ltxt_color,
                            size = 5,
                            family = def_font)

## ---- end-of-exp_age

# * INSIGHT 4 : COMPANY AND SURVIVALHOOD ----

## ---- exp_company

appendFamilySize <- function(data) {
    data %>%
    mutate(FamilySize = Parch + SibSp + 1)
}

# Group by Family Size and calculate Survival Rates for Different Subsets of Passengers
company_set <- training_set %>%
               appendFamilySize() %>%
               mutate(Size = FamilySize,
                      isAdult = is.na(Age) | Age > 12,
                      isAdultNoChild = isAdult & Parch == 0,
                      isFemaleNoChild = isAdultNoChild & Sex == "female") %>%
               group_by(Size) %>%
               summarise(CohortSize = n(),
                         SurvivalRate = sum(Survived)/CohortSize,
                         Adults = sum(isAdult),
                         SurvivalRateAdults = sum(isAdult & Survived)/Adults,
                         AdultsNoChild = sum(isAdultNoChild),
                         SurvivalRateAdultsNoChild = sum(isAdultNoChild & Survived)/AdultsNoChild,
                         FemaleNoChild = sum(isFemaleNoChild),
                         SurvivalRateFemaleNoChild = sum(isFemaleNoChild & Survived)/FemaleNoChild)

# Normalize the survival rates with respect to baseline (Size = 0)
for (c_name in c("SurvivalRateAdults",
                 "SurvivalRateAdultsNoChild",
                 "SurvivalRateFemaleNoChild")) {
  company_set[,c_name] <- company_set[,c_name] / as.double(company_set[1,c_name]) * as.double(company_set[1,"SurvivalRate"])
}

# Calculate attribution statistics
company_set$AttrChild <- company_set$SurvivalRate - company_set$SurvivalRateAdults
company_set$AttrParents <- company_set$SurvivalRateAdults - company_set$SurvivalRateAdultsNoChild
company_set$AttrHusband <- company_set$SurvivalRateAdultsNoChild - company_set$SurvivalRateFemaleNoChild
company_set$Baseline <- company_set$SurvivalRate - company_set$AttrChild - company_set$AttrParents - company_set$AttrHusband

# Only analyze cohort size until 3 due to small sample sizes afterwards
company_stack <- company_set %>% 
                filter(Size <= 3) %>% 
                select(Size, Baseline, AttrHusband, AttrParents, AttrChild, SurvivalRate) %>%
                gather_("Attribution","Value",setdiff(names(.),c("Size","SurvivalRate"))) %>%
                mutate(Value = sapply(Value, function(x) { max(x,0) }))

# Reorder factor levels
company_stack$Attribution <- factor(company_stack$Attribution, levels = rev(unique(company_stack$Attribution)))


company_plot <- ggplot(company_stack, aes(x= Size, y= Value)) + 
                theme_lk() +
                scale_x_continuous(name="Family Size",
                                   expand = c(0.05,0),
                                   labels = function(i) { round(i) },
                                   breaks = 1:3) +
                scale_y_continuous(name="Survival Likelihood",
                                   expand = c(0,0),
                                   labels = scales::percent) +
                coord_cartesian(ylim=c(0.2,0.65)) +
                scale_fill_manual(name="Attribution", 
                                  labels=c("Baseline"="Baseline", 
                                           "AttrHusband"="Husband",
                                           "AttrParents"="Parental",
                                           "AttrChild"="Child"),
                                  values=c("Baseline"=alpha(txt_color,0.2),
                                           "AttrHusband"=get_color(1,0.8),
                                           "AttrParents"=get_color(2,0.8),
                                           "AttrChild"=get_color(3,0.8)),
                                  guide=guide_legend(reverse=T)) +
                scale_color_manual(values=c("Baseline"=alpha(txt_color,0),
                                            "AttrHusband"=get_color(1),
                                            "AttrParents"=get_color(2),
                                            "AttrChild"=get_color(3)),
                                   guide = 'none') + 
                geom_area(aes(fill = Attribution, colour=Attribution), size=2, linetype=1, position = 'stack') + 
                geom_text(data=unique(company_stack %>% select(Size, SurvivalRate)),
                          aes(x=Size, y=SurvivalRate+0.03, label=paste0(round(SurvivalRate*100),"%")),
                          color=txt_color,
                          size=5,
                          family = def_font)

## ---- end-of-exp_company

# * INSIGHT 5 : CABIN AND SURVIVALHOOD ----

## ---- exp_cabin_decompose

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

## ---- end-of-exp_cabin_decompose

## ---- exp_cabin_floors

cabinLet_set <- cabin_set %>%
                group_by(CabinFloor) %>%
                summarise(CohortSize = n(),
                          SurvivalRate = sum(Survived)/n())

cabinLet_plot <- ggplot(cabinLet_set, aes(x=as.factor(CabinFloor),
                                          y=SurvivalRate,
                                          alpha = CohortSize,
                                          fill = SurvivalRate)) +
                  theme_lk() + 
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
                    xlab("Cabin Floor") +
                    ylab("Survival Likelihood") +
                    scale_y_continuous(labels = scales::percent,
                                       # Make Sure Bar and X-Axis Stick Together
                                       expand = c(0,0),
                                       limits =c(0,1)) +
                    # Actual Data
                    geom_bar(stat="identity") +
                    geom_text(aes(y = SurvivalRate - 0.08,
                                  label=paste0(round(SurvivalRate*100),"%")),
                              color = "#FFFFFF", alpha = 1,
                              family=def_font,size=4)

## ---- end-of-exp_cabin_floors

## ---- exp_cabin_number

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
                                          guide = guide_legend(order = 2,
                                                               override.aes=list(alpha=0.5))) +
                    theme(
                      axis.line.x = element_line(colour=NA),
                      axis.ticks.x = element_line(colour=NA),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    ) +
                    # Add X Axis Line
                    geom_segment(data = data.frame(1),
                                 aes(x=0.25, xend=0.8, y=0, yend=0),
                                 size = 0.5,
                                 color=alpha(ltxt_color,0.5),
                                 arrow = arrow(length = unit(10,"pt"),
                                               type = "closed")) +
                    geom_text(data = data.frame(1),
                              label = "Survival Likelihood",
                              x = 0.8,
                              y = 0.02,
                              family = def_font,
                              color = ltxt_color,
                              size = 5,
                              hjust = 1) +
                    theme(
                      # Y-Axis
                      axis.line.y = element_line(colour=NA),
                      axis.ticks.y = element_line(colour=NA),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank()) +
                      scale_y_continuous(limits=c(-0.12, 0.05)) +
                    geom_point() +
                    geom_text(aes(y = -0.03, label=paste0(round(SurvivalRate*100,0),"%")),
                              size=4,
                              color=txt_color,
                              family=def_font)

## ---- end-of-exp_cabin_number

# * INSIGHT 6 : EMBARKATION PORTS ----

## ---- exp_port

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

## ---- end-of-exp_port

## ---- exp_port_income

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
                      scale_fill_manual(name = "Passenger Class",
                         labels = c("1"="1 (High-Income)","2"="2 (Medium-Income)","3"="3 (Low-Income)"),
                         values = c("1"=get_color("green"),
                                    "2"=alpha(get_color("green",0.5),0.2),
                                    "3"=alpha(get_color("red"),0.2)),
                         guide = guide_legend(reverse = TRUE)) +
                      xlab("Port of Embarkation") +
                      scale_x_discrete(labels=c("S"="Southampton",
                                                "C"="Cherbough",
                                                "Q"="Queenstown")) +
                      ylab("Survival Likelihood") +
                      scale_y_continuous(labels = scales::percent,
                                         # Make Sure Bar and X-Axis Stick Together
                                         expand = c(0,0),
                                         limits =c(0,1)) +
                      coord_flip() +
                      # Actual Data
                      geom_bar(stat="identity", width=0.8) +
                      geom_text(aes(label=paste0(round(PctEmbarked*100),"%"),
                                    y = PctEmbarked - 0.05),
                                data = embark_pclass %>% filter(Pclass == 1),
                                color = bg_color,
                                family = def_font, size = 5)

## ---- end-of-exp_port_income

# DATA MODELING ### ----

# * PREPARING THE DATA ----

## ---- model_age_append
appendConfirmedAdult <- function(data) {
    data %>%
    mutate(ConfirmedAdult = ifelse(Parch > 2, 1, 0))
}
## ---- end-of-model_age_append

## ---- model_age_vars
age_data <- training_set %>%
            appendTitle() %>%
            appendFamilySize() %>%
            appendConfirmedAdult() %>%
            filter(Age > 0)

age_by <- list()
for (i in c('Pclass','Title','ConfirmedAdult','FamilySize')) {
  tmp <- age_data %>% 
         group_by_at(vars(i)) %>%
         summarise(Age = mean(Age))
  age_by[[i]] <- unlist(tmp$Age)
  names(age_by[[i]]) <- unlist(tmp[,i])
}

## ---- end-of-model_age_vars

## ---- model_age_anova

age_model <- function(data) {
  
  age_data <- data %>%
              appendTitle() %>%
              appendFamilySize() %>%
              appendConfirmedAdult() %>%
              filter(Age > 0)
  
  model <- lm(Age ~ Pclass +
                    Title + 
                    ConfirmedAdult +
                    FamilySize,
              data = age_data)
  
  model
}

age_m <- age_model(training_set)
summary(age_m)

## ---- end-of-model_age_anova

## ---- model_cleanse

clean_data <- function(data, age_model, is_training = TRUE) {
  output <- appendTitle(data) %>%
            appendFamilySize() %>%
            appendConfirmedAdult()
  
  # Populate Age
  output <- output %>%
            mutate(AgePredict = predict.lm(age_model, output),
                   Age = ifelse(is.na(Age),AgePredict,Age))

  # Populate Others: Names represent columns, value is TRUE if column is categorical,
  # false otherwise
  features <- c("Pclass"=FALSE,
                "Fare"=FALSE,
                "Sex"=TRUE,
                "Title"=TRUE,
                "Age"=FALSE,
                "FamilySize"=FALSE)
  null_handler <- function(is_categorical = TRUE) {
    paste0('ifelse(is.na(x) | x %in% c("","NA"),', ifelse(is_categorical,'"Unknown"','-1'),',x)')
  }
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

cleaned_set <- clean_data(training_set, age_m)

## ---- end-of-model_cleanse

# * MODELING SURVIVAL LIKELIHOOD ----

## ---- model_randomize
set.seed(1) 
shuffled_set <- training_set[sample(nrow(training_set)),]
training_grp <- shuffled_set[1:floor(0.80 *nrow(shuffled_set)),]
testing_grp <- shuffled_set[floor(0.80 *nrow(shuffled_set) + 1):nrow(shuffled_set),]
## ---- end-of-model_randomize

## ---- model_construct
# Create age estimator using all the dataset
age_m <- age_model(rbind(training_grp, testing_grp))

set.seed(1) 
rf_model <- randomForest(Survived ~ .,
                         data = clean_data(training_grp, age_m),
                         importance=TRUE,
                         ntree=2000)

rf_model

## ---- end-of-model_construct

## ---- model_internal_test

# Compare using the testing group
p_y <- predict(rf_model, newdata = clean_data(testing_grp %>% select(-Survived), age_m, is_training=FALSE))
cmp <- cbind(testing_grp %>% select(actual=Survived), predicted=p_y) %>%
             summarise(size=n(),
                       err=sum(ifelse(actual != predicted,1,0)))


## ---- end-of-model_internal_test

## ---- model_external_test

# Read the test set
test_set <- read.csv(paste0(data_dir, "test.csv"))

# Create age estimator using both the train and test dataset
age_m <- age_model(rbind(training_set %>% select(-Survived),
                         test_set))

# Create model using all the training set
set.seed(1) 
rf_model <- randomForest(Survived ~ .,
                         data = clean_data(training_set, age_m),
                         importance=TRUE,
                         ntree=2000)

# Predict the test set
p_y <- predict(rf_model, newdata = clean_data(test_set, age_m, is_training=FALSE))
predictions <- cbind(test_set %>% select(PassengerId), Survived=p_y)
write.csv(predictions, 
          file=paste0(output_dir,"submission.csv"),
          row.names = FALSE)

# Upload the file to https://www.kaggle.com/c/titanic/submit

## ---- end-of-model_external_test

## ---- model_factors

z_scores <- (rf_model$importance[,"MeanDecreaseAccuracy"] / rf_model$importanceSD[,"MeanDecreaseAccuracy"]) %>%
            { c(.,"Baseline"=2.32) } %>%
            sort() %>%
            { data.frame("Feature"=names(.),"Zscore"=as.numeric(.)) }
z_scores$Feature <- factor(z_scores$Feature, levels = z_scores$Feature)

z_plot <- ggplot(data = z_scores, aes(x = sapply(z_scores$Zscore, function (x) {max(36,x)}),
                                      y = 0,
                                      color=Feature)) +
          theme_lk() +
          scale_color_manual(name = "Feature",
                             values = c(txt_color,get_color("palette")(7)),
                             guide = guide_legend(nrow=1,
                                                  override.aes=list(size=5))) +
          theme(
            axis.line.x = element_line(colour=NA),
            axis.ticks.x = element_line(colour=NA),
            axis.title.x = element_blank(),
            axis.text.x = element_blank()
          ) +
          # Add X Axis Line
          geom_segment(data = data.frame(1),
               aes(x=35, 
                   xend=38, 
                   y=0, 
                   yend=0),
               size = 0.5,
               color=ltxt_color) +
          geom_segment(data = data.frame(1),
                       aes(x=38, 
                           xend=40, 
                           y=0, 
                           yend=0),
                       size = 0.5,
                       color=ltxt_color,
                       linetype=2) +
          geom_segment(data = data.frame(1),
                       aes(x=40, 
                       xend=73, 
                       y=0, 
                       yend=0),
                       size = 0.5,
                       color=ltxt_color,
                       arrow = arrow(length = unit(10,"pt"),
                                     type = "closed")) +
          geom_text(data = data.frame(1),
                    label = "Score",
                    aes(x = 77,
                    y = 0),
                    family = def_font,
                    color = ltxt_color,
                    size = 5,
                    hjust = 1) +
          theme(
            # Y-Axis
            axis.line.y = element_line(colour=NA),
            axis.ticks.y = element_line(colour=NA),
            axis.title.y = element_blank(),
            axis.text.y = element_blank()) +
          scale_y_continuous(limits=c(-0.05, 0.05)) +
          geom_point(size = 15) +
          geom_text(aes(label=paste0(round(Zscore,0))),
                    family = def_font,
                    color = "#FFFFFF",
                    size = 5,
                    hjust = 0.5)


## ---- end-of-model_factors