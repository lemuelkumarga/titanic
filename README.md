## Package Requirements

### Pandoc 2.1 
- Need to reference googlefonts

## Cloning the Repository

Cloning the repository is not as straightforward due to the presence of git submodules.

Please replicate the steps below in Terminal to ensure success.

``` sh
# Clone the repo as usual
git clone https://github.com/lemuelkumarga/<project>

# Initialize submodule
cd <project>
git submodule init
git submodule update

# When cloned, submodules are detached from the HEAD. We attempt to rectify this issue to prevent problems in git
cd shared
git checkout -b tmp
git checkout master
git merge tmp
git branch -d tmp

# Return to original folder if desired
cd ../../
```

---
Estimating Survival Rate of Titanic Passengers
================
Lemuel Kumarga

## Problem Description

Set for its maiden voyage in 1912, RMS Titanic was then considered
unsinkable due to its robust structure. After picking up passengers in
Southampton, Cherbourg and Queenstown, the ship set sail towards New
York. It was on their way there that the Titanic met its calamitous
fate, crashing into an Iceberg and rendering more than 50% of the
passengers lost.

There were many questions concerning the events leading to and during
the disaster. Some of these include: Which part of the ship was most
damaged? What was the evacuation process of the passengers? Using data
containing the personal details and disaster outcomes (survival/lost) of
passengers, we will aim to answer the following set of questions.

<span class="color-1-text">FYI: This problem was posed as an
introductory challenge in
<a href="https://www.kaggle.com/c/titanic" target="_blank">Kaggle</a>.
The passengers are divided into two sets, one used for training and the
other for testing. The training sets contain both the personal details
of passengers and their outcomes, while the testing set only contain the
former. By developing a robust model through the training set, we hope
to predict the survival likelihood of passengers in the testing
set.</span>

## Summary of Results

To be completed.

## Preliminaries

First load the necessary packages for this exercise.

``` r
# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","randomForest","tidyr","leaflet","purrr","grDevices","pander")
load_or_install.packages(packages)

data_dir <- "data/"

# Load some helper functions
source("shared/helper.R")

si <- sessionInfo()
base_pkg_str <- paste0("Base Packages: ",paste(si[["basePkgs"]], collapse=", "))
attached_pkg_str <- paste0("Attached Packages: ",paste(names(si[["otherPkgs"]]), collapse=", "))
cat(paste0(base_pkg_str,"\n",attached_pkg_str))
```

    ## Base Packages: stats, graphics, grDevices, utils, datasets, base
    ## Attached Packages: purrr, leaflet, tidyr, randomForest, pander, ggplot2, dplyr, knitr

## Exploration

### About the Data

We kick off by exploring the data that was provided:

``` r
# Provides high level information about a particular data frame
# @input data: the data frame
# @input null_fn: a function that takes in a value (like those in the cells)
# and outputs true if value corresponds to the equivalent null value, false
# otherwise. For example, we may want to consider 0 as nulls on some scenarios
# but not on others.
# @output A data frame showing
# - Name of the Columns
# - Type of Variables in Each Column
# - Some Non-Null Examples of Each Column
# - % of Values that are Non-Null in Each Column (% Filled)
data_overview <- function(data,
                          null_fn = function(cname) { paste0(cname," == '' | is.na(",cname,")")}) {
                  
                  cols_summary <- data.frame(ColumnNames = colnames(data))
                  cols_summary$Type <- lapply(training_set, class) %>%
                    toupper()
                  cols_summary$Examples <- lapply(cols_summary$ColumnNames,
                                                  function(cname) {
                                                    training_set %>%
                                                      filter_(paste0("!(",null_fn(cname),")")) %>%
                                                      `[[`(cname) %>%
                                                      unique() -> filtered_set
                                                    filtered_set[1:min(5, length(filtered_set))] %>%
                                                      paste(collapse=' | ')
                                                  })
                  cols_summary$EmptyValues <- lapply(cols_summary$ColumnNames,
                                                     function(cname) {
                                                       training_set %>%
                                                         filter_(null_fn(cname)) %>%
                                                         nrow()
                                                     })
                  cols_summary$PctFilled <- lapply(cols_summary$EmptyValues,
                                                   function(x) {
                                                     ((nrow(training_set) - x) / nrow(training_set)) %>%
                                                       `*`(100) %>% round(0) %>%
                                                       paste0("%")
                                                   })
                  
                  select(cols_summary, ColumnNames, Type, Examples, PctFilled)
}
  
training_set <- read.csv(paste0(data_dir, "train.csv"))

cols_summary <- data_overview(training_set)

pander(cols_summary, caption="Titanic Passengers Data")
```

| ColumnNames | Type      | Examples                                                                                                                                                                         | PctFilled |
| :---------- | :-------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------- |
| PassengerId | INTEGER   | 1 | 2 | 3 | 4 | 5                                                                                                                                                                | 100%      |
| Survived    | INTEGER   | 0 | 1                                                                                                                                                                            | 100%      |
| Pclass      | INTEGER   | 3 | 1 | 2                                                                                                                                                                        | 100%      |
| Name        | CHARACTER | Braund, Mr. Owen Harris | Cumings, Mrs. John Bradley (Florence Briggs Thayer) | Heikkinen, Miss. Laina | Futrelle, Mrs. Jacques Heath (Lily May Peel) | Allen, Mr. William Henry | 100%      |
| Sex         | CHARACTER | male | female                                                                                                                                                                    | 100%      |
| Age         | NUMERIC   | 22 | 38 | 26 | 35 | 54                                                                                                                                                           | 80%       |
| SibSp       | INTEGER   | 1 | 0 | 3 | 4 | 2                                                                                                                                                                | 100%      |
| Parch       | INTEGER   | 0 | 1 | 2 | 5 | 3                                                                                                                                                                | 100%      |
| Ticket      | CHARACTER | A/5 21171 | PC 17599 | STON/O2. 3101282 | 113803 | 373450                                                                                                                        | 100%      |
| Fare        | NUMERIC   | 7.25 | 71.2833 | 7.925 | 53.1 | 8.05                                                                                                                                             | 100%      |
| Cabin       | CHARACTER | C85 | C123 | E46 | G6 | C103                                                                                                                                                     | 23%       |
| Embarked    | CHARACTER | S | C | Q                                                                                                                                                                        | 100%      |

Titanic Passengers Data <br>

Based on the summary above, notice the following:

  - <span class="hl color-1-text">Names</span> are aggregated in the
    following format: <span class="hl color-2-text">Last\_Name, Title
    First\_Name</span>.
  - <span class="hl color-1-text">Passenger class (Pclass) </span>
    levels are the equivalent of <span class="hl color-2-text">First
    Class (1), Business Class (2) and Economy Class (3)</span>. Hence,
    they could be used as an indicator of income level.
  - There are different types of
    <span class="hl color-1-text">Ticket</span> formats.
  - <span class="hl color-1-text">Age</span> has missing data to be
    populated.
  - <span class="hl color-1-text">Cabin</span> data is sparse, and hence
    we may want to exclude this for the prediction model.

### Preliminary Insights

#### On Income\[/Passenger Class\]

<div class="st">

Hypothesis 1: Higher income individuals are more likely to survive.

</div>

High-income individuals could pay more for seats in the lifeboats, and
hence escape the disaster.

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo1-1.png" style="display: block; margin: auto;" />
Using the passenger class as a proxy for income, we find this claim to
be
<span class="hl green-text">TRUE</span>.

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo1_p2-1.png" style="display: block; margin: auto;" />
We also noticed this phenomenon in fares, where the higher amount an
individual paid for the fares, the more likely he/she will survive the
crash.

#### On Title

<div class="st">

Hypothesis 2: Gender information is embedded under the title of the
individual.

</div>

Most titles are provided based on the gender on the individual, hence we
should expect the hypothesis to hold
true.

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo2-1.png" style="display: block; margin: auto;" />

As can be seen, almost every title has a mapping to the gender of the
individual, suggesting that every title is associated with a gender. The
only notable exception is “Dr”, which is gender neutral. Hence, the
hypothesis is <span class="hl green-text">TRUE</span>.

<div class="st">

Hypothesis 3: Females and more esteemed individuals are more likely to
survive than males.

</div>

Prior to this analysis, we will group those individuals without
“Mr”,“Mrs”,“Miss”, or “Master” title as “Rare title” individuals.
These individuals, along with females, would have higher priority in
onboarding the lifeboats, hence we assume that they are more likely to
survive the crash.

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo3-1.png" style="display: block; margin: auto;" />

As expected, females are more likely to survive than males, while
“esteemed” titles increase the survival likelihood of both genders.
Hence, the hypothesis is <span class="hl green-text">TRUE</span>.

#### On Age

<div class="st">

Hypothesis 4: Individuals who are very young and very old are more
likely to survive.

</div>

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo4-1.png" style="display: block; margin: auto;" />

With the limited data set, we can see that younger individuals are more
likely to survive. A possible explanation is probably because babies and
toddlers occupy less space (and hence easier to find seats within the
lifeboats).

However, older people, especially those more than 60 years old, are less
likely to survive. This is probably due to their lack of agility in
responding to the crash.

Hence, the premise is <span class="hl green-text">TRUE</span> for young
children and <span class="hl red-text">FALSE</span> for older people.

#### On Company

<div class="st">

Hypothesis 5: Individuals with children are more likely to survive,
while individuals with same-age company are less likely to survive.

</div>

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo5-1.png" style="display: block; margin: auto;" />

As can be seen from the chart above, individuals with 1 to 2 children
are more likely to survive than those without any children. It is
inconclusive to determine from the chart above, whether same-age company
has an impact of survivalhood. Hence, this premise is
<span class="hl yellow-text">TRUE TO A LIMITED EXTENT</span>.

<!---

<div class="st">Hypothesis 6: Groups are able to be identified based on their ticket IDs. </div>

If this is true, we should expect to see some correlation between: 

1. the number of companies with the individual, and
2. the number of other individuals with the same ticket.


```r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo6-1.png" style="display: block; margin: auto;" />

A correlation of <b>36%</b> suggests that there is in fact some correlation between ticket IDs and the identity of the group. Hence, this hypothesis is <span class="hl green-text">TRUE</span>.

-->

#### On Cabin Positions

<div class="st">

Hypothesis 6: Cabin positions should have an impact of survivalhood, but
only to a certain extent.

</div>

Cabins further away from the lifeboats will have a more difficult time
surviving, hence we should assume that where passengers stay will have
an impact on their survivalhood. However, the cabin positions can only
determine survivalhood partially, given that passengers may not be at
their cabins during the time of crash.

A more detailed deck plan can be found
<a href="https://www.encyclopedia-titanica.org/titanic-deckplans/b-deck.html" target="_blank">here</a>.
As can be seen, the letter of the cabin represents the deck (floor)
where the room is located. We also see that odd numbers correspond to
one side of the Titanic, while even numbers correspond to the other.

We will break down the cabin position into three constituents: the cabin
floor, the cabin number (odd or even), and the number of cabins
specified.

``` r
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
```

##### Cabin Floors

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo7_p1-1.png" style="display: block; margin: auto;" />

From the chart, we can deduce that <span class="hl yellow-text">Cabins B
to E</span> has a higher likelihood of survival compared to
other/unspecified cabins, which suggests that the position of cabins
play a role in determining whether a passenger survive.

##### Cabin Numbers

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo7_p2-1.png" style="display: block; margin: auto;" />

It is pretty clear that those who stay in the odd rooms are more likely
to survive than those in the even rooms.

##### Cabins Specified

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo7_p3-1.png" style="display: block; margin: auto;" />

Even though seems to be an inverse relationship between the survival
likelihood and number of cabins specified, the sample size is too small
to ensure statistical significance.

In conclusion, cabin floors and cabin numbers can determine a
passenger’s survival likelihood, while there is too little data to
deduce a relationship in the number of cabins specified. Hence, the
hypothesis is <span class="hl green-text">TRUE</span>.

#### On Embakartion Ports

<div class="st">

Hypothesis 7: Port of embarkation should have no impact on survivalhood.

</div>

``` r
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
```

<!--html_preserve-->

<div id="htmlwidget-c5ed414ac5739dec0150" class="leaflet html-widget" style="width:100%;height:288px;">

</div>

<script type="application/json" data-for="htmlwidget-c5ed414ac5739dec0150">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false}]},{"method":"addAwesomeMarkers","args":[41.7666636,-50.2333324,{"icon":"ship","markerColor":"gray","iconColor":"#FFFFFF","spin":false,"squareMarker":false,"iconRotate":0,"font":"monospace","prefix":"fa"},null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},"Titanic Crash Site",null,null,null,null,null,null]},{"method":"addCircleMarkers","args":[49.645009,-1.62444,10,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#94A162","weight":5,"opacity":0.8,"fill":true,"fillColor":"#94A162","fillOpacity":0.5,"dashArray":null},null,null,"Cherbough<br>Survival Likelihood: 55%",null,null,null,null]},{"method":"addCircleMarkers","args":[51.851,-8.2967,10,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#B55C5C","weight":5,"opacity":0.8,"fill":true,"fillColor":"#B55C5C","fillOpacity":0.5,"dashArray":null},null,null,"Queenstown<br>Survival Likelihood: 39%",null,null,null,null]},{"method":"addCircleMarkers","args":[50.9038684,-1.4176118,15,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#B55C5C","weight":5,"opacity":0.8,"fill":true,"fillColor":"#B55C5C","fillOpacity":0.5,"dashArray":null},null,null,"Southampton<br>Survival Likelihood: 34%",null,null,null,null]}],"limits":{"lat":[41.7666636,51.851],"lng":[-50.2333324,-1.4176118]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

<br> At first glance, there seems to be no plausible explanation why a
passenger’s port of embarkation will have an impact on the passenger’s
survival likelihood.

``` r
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
```

<img src="/Users/lemuel/Google Drive/Website/content/.toolkit/..//titanic/README_files/figure-gfm/exp_hypo8_p2-1.png" style="display: block; margin: auto;" />

However, by studying the demographics of the passengers who embarked at
each port, we know that a higher proportion of Cherbough are
high-income. This explains the higher survival likelihood for those who
embarked at Cherbough, and renders the hypothesis to be
<span class="hl red-text">FALSE</span>.

## Preparation

Based on the above hypotheses, we will be adding the following features
to predict the survival likelihood of an
individual.

| Feature             | Variable Type | Hypotheses Supporting the Feature | Null Handling               |
| ------------------- | ------------- | --------------------------------- | --------------------------- |
| Pclass              | Continuous    | Hypothesis 1                      | \-1                         |
| Fare                | Continuous    | Hypothesis 1                      | \-1                         |
| Sex                 | Categorical   | Hypothesis 3                      | ‘Unknown’ Category Variable |
| Title               | Categorical   | Hypothesis 3                      | ‘Unknown’ Category Variable |
| Age                 | Continuous    | Hypothesis 4                      | Regression                  |
| SibSp               | Continuous    | Hypothesis 5                      | \-1                         |
| Parch               | Continuous    | Hypothesis 5                      | \-1                         |
| CabinFloor          | Categorical   | Hypothesis 6                      | ‘Unknown’ Category Variable |
| CabinNumber         | Categorical   | Hypothesis 6                      | ‘Unknown’ Category Variable |
| Port of Embarkation | Categorical   | Hypothesis 7                      | ‘Unknown’ Category Variable |

Below is an example of the cleansed data set for model ingestion.

``` r
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

pander(head(cleaned_set))
```

| Survived | Pclass | Fare  | Sex    | Title |  Age  | SibSp | Parch |
| :------- | :----: | :---: | :----- | :---- | :---: | :---: | :---: |
| 0        |   3    | 7.25  | male   | Mr    |  22   |   1   |   0   |
| 1        |   1    | 71.28 | female | Mrs   |  38   |   1   |   0   |
| 1        |   3    | 7.925 | female | Miss  |  26   |   0   |   0   |
| 1        |   1    | 53.1  | female | Mrs   |  35   |   1   |   0   |
| 0        |   3    | 8.05  | male   | Mr    |  35   |   0   |   0   |
| 0        |   3    | 8.458 | male   | Mr    | 29.87 |   0   |   0   |

Table continues below

| CabinFloor | CabinNumber | Embarked |
| :--------- | :---------- | :------- |
| Unknown    | Unknown     | S        |
| C          | Odd         | C        |
| Unknown    | Unknown     | S        |
| C          | Odd         | S        |
| Unknown    | Unknown     | S        |
| Unknown    | Unknown     | Q        |

## The <i>Random Forest</i> Model

Listed below are the preliminary results from the initial run using the
Random Forest model.

``` r
set.seed(1)
rf_model <- randomForest(Survived ~ .,
                         data = cleaned_set,
                         importance=TRUE,
                         ntree=2000)

rf_model
```

    ## 
    ## Call:
    ##  randomForest(formula = Survived ~ ., data = cleaned_set, importance = TRUE,      ntree = 2000) 
    ##                Type of random forest: classification
    ##                      Number of trees: 2000
    ## No. of variables tried at each split: 3
    ## 
    ##         OOB estimate of  error rate: 16.84%
    ## Confusion matrix:
    ##     0   1 class.error
    ## 0 492  57   0.1038251
    ## 1  93 249   0.2719298

``` r
importance(rf_model)
```

    ##                    0          1 MeanDecreaseAccuracy MeanDecreaseGini
    ## Pclass      47.21663 47.4942132             69.39251         28.07819
    ## Fare        43.19055 43.9893907             66.67144         58.87971
    ## Sex         42.24730 36.8114966             42.57382         50.52881
    ## Title       54.59914 57.1290385             60.72891         77.20460
    ## Age         34.58641 39.4252418             53.95813         52.61366
    ## SibSp       44.09189  0.8302836             42.35741         18.13917
    ## Parch       11.04993  9.5437824             15.49590         10.00568
    ## CabinFloor  24.50409  6.9765507             27.03299         21.64158
    ## CabinNumber 17.25665 14.8254202             23.33504         12.55936
    ## Embarked    17.95344 29.6962125             35.40686         10.48846

``` r
p_y <- predict(rf_model, newdata = cleaned_set %>% select(-Survived))
err_out <-  cleaned_set %>%
            mutate(p_y = p_y) %>%
            filter(Survived != p_y)
  
pander(head(err_out))
```

| Survived | Pclass | Fare  | Sex    | Title |  Age  | SibSp | Parch |
| :------- | :----: | :---: | :----- | :---- | :---: | :---: | :---: |
| 1        |   2    |  13   | male   | Mr    | 32.99 |   0   |   0   |
| 1        |   3    | 31.39 | female | Mrs   |  38   |   1   |   5   |
| 1        |   3    | 7.229 | male   | Mr    | 29.87 |   0   |   0   |
| 0        |   2    |  21   | female | Mrs   |  27   |   1   |   0   |
| 1        |   1    | 35.5  | male   | Mr    | 41.48 |   0   |   0   |
| 1        |   3    |  9.5  | male   | Mr    |  29   |   0   |   0   |

Table continues below

| CabinFloor | CabinNumber | Embarked | p\_y |
| :--------- | :---------- | :------- | :--- |
| Unknown    | Unknown     | S        | 0    |
| Unknown    | Unknown     | S        | 0    |
| Unknown    | Unknown     | C        | 0    |
| Unknown    | Unknown     | S        | 1    |
| C          | Even        | S        | 0    |
| Unknown    | Unknown     | S        | 0    |

## Predictive Results
