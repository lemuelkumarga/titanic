## Package Requirements

### Mac/Ubuntu Operating System
- Other OS-es have not been tested, and may cause unexpected errors.

### Pandoc 2.1 
- Need to reference googlefonts

### Git LFS
- If there exists a cache folder, you may need to install Git LFS
to pull files > 100 MB. Please follow instructions [here](https://github.com/git-lfs/git-lfs/wiki/Installation).
- After Git LFS is installed, type `git lfs pull` to download the large files in the project.


## Cloning the Repository

Cloning the repository is not as straightforward due to the presence of git submodules.

Please replicate the steps below in Terminal to ensure success.

``` sh
# Clone the repo as usual
git clone https://github.com/lemuelkumarga/titanic

# Initialize submodule
cd titanic
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

There were many question marks concerning the events leading to and
during the disaster. One of these was the evacuation process of the
passengers. By analyzing personal details and disaster outcomes
(survival/lost), we aim to <b>discover the various factors affecting
passengers’ survival likelihoods.</b>

<span class="color-1-text">FYI: This problem was posed as an
introductory challenge in
<a href="https://www.kaggle.com/c/titanic" target="_blank">Kaggle</a>.
The passengers are divided into two groups, one for training and the
other for testing. The former contains both the passengers’ personal
details and outcomes, while the latter only contains personal
informaion. The aim of the project is to develop a robust model through
the training set, and predict the survival of passengers in the testing
set.</span>

To skip the methodology and proceed directly to the summary of results,
click <a href="#summary-of-results">here</a>.

## Preliminaries

First load the necessary packages for this exercise.

``` r
# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")
# Load some helper functions
source("shared/helper.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","randomForest","tidyr","leaflet","purrr","grDevices","pander")
load_or_install.packages(packages)

data_dir <- "data/"
output_dir <- "output/"

si <- sessionInfo()
base_pkg_str <- paste0("Base Packages: ",paste(si[["basePkgs"]], collapse=", "))
attached_pkg_str <- paste0("Attached Packages: ",paste(names(si[["otherPkgs"]]), collapse=", "))
cat(paste0(base_pkg_str,"\n",attached_pkg_str))
```

    ## Base Packages: stats, graphics, grDevices, utils, datasets, base
    ## Attached Packages: bindrcpp, purrr, leaflet, tidyr, randomForest, pander, ggplot2, dplyr, knitr

## Exploration

### About the Data

We kick off by exploring the data that was provided:

``` r
training_set <- read.csv(paste0(data_dir, "train.csv"))

cols_summary <- data_overview(training_set)

pander(cols_summary, caption='Titanic Passengers Data - For more info, please visit <a href="https://www.kaggle.com/c/titanic/data" target="_blank">Kaggle</a>')
```

| ColumnNames | Type      | Examples                                                                                                                                                                             | PctFilled |
| :---------- | :-------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------- |
| PassengerId | INTEGER   | 1 // 2 // 3 // 4 // 5                                                                                                                                                                | 100%      |
| Survived    | INTEGER   | 0 // 1                                                                                                                                                                               | 100%      |
| Pclass      | INTEGER   | 3 // 1 // 2                                                                                                                                                                          | 100%      |
| Name        | CHARACTER | Braund, Mr. Owen Harris // Cumings, Mrs. John Bradley (Florence Briggs Thayer) // Heikkinen, Miss. Laina // Futrelle, Mrs. Jacques Heath (Lily May Peel) // Allen, Mr. William Henry | 100%      |
| Sex         | CHARACTER | male // female                                                                                                                                                                       | 100%      |
| Age         | NUMERIC   | 22 // 38 // 26 // 35 // 54                                                                                                                                                           | 80%       |
| SibSp       | INTEGER   | 1 // 0 // 3 // 4 // 2                                                                                                                                                                | 100%      |
| Parch       | INTEGER   | 0 // 1 // 2 // 5 // 3                                                                                                                                                                | 100%      |
| Ticket      | CHARACTER | A/5 21171 // PC 17599 // STON/O2. 3101282 // 113803 // 373450                                                                                                                        | 100%      |
| Fare        | NUMERIC   | 7.25 // 71.2833 // 7.925 // 53.1 // 8.05                                                                                                                                             | 100%      |
| Cabin       | CHARACTER | C85 // C123 // E46 // G6 // C103                                                                                                                                                     | 22%       |
| Embarked    | CHARACTER | S // C // Q                                                                                                                                                                          | 99%       |

Titanic Passengers Data - For more info, please visit
<a href="https://www.kaggle.com/c/titanic/data" target="_blank">Kaggle</a>
<br>

Based on the above table, we know that:

  - <span class="hl color-1-text">Names</span> are aggregated in the
    following format: <span class="hl color-2-text">Last\_Name, Title
    First\_Name</span>.
  - <span class="hl color-1-text">Passenger class (Pclass) </span>
    levels are the equivalent of <span class="hl color-2-text">First
    Class (1), Business Class (2) and Economy Class (3)</span>.
  - <span class="hl color-1-text">Age</span> has missing data to be
    populated.
  - <span class="hl color-1-text">Cabin</span> data is sparse, and hence
    we may want to exclude this for the prediction model.

### Preliminary Insights

<div class="st">

Insight 1: Wealthy individuals are more likely to survive.

</div>

Using passenger class as a proxy for wealth, we calculated the
<a data-toggle="popover" title="Survival Likelihood" data-content="% of passengers in the group who survived
.">survival likelihood</a> of each class.

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

income_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

The chart above shows that the more premium the class, the more likely
passengers were to survive. One reason explaining this could be that 1st
class passengers were first in line to access the lifeboats, while 3rd
class passengers were left to fend for themselves.

``` r
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

fares_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

Similarly, we also noticed this phenomenon in fares. Passengers who were
paying more have a higher survival likelihood than others.

<div class="st">

Insight 2: Females and more esteemed individuals are more likely to
survive than males.

</div>

The data provides us with a list of titles under the
<span class="hl">Name</span> column. Each title is associated with a
gender (with the exception of one), as shown
below:

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

gender_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Other than <span class="hl color-1-text">Mr, Miss, Mrs and
Master</span>, all other titles are not common to the average passenger.
To navigate the small sample size, we will group all of these other
titles under <span class="hl color-2-text">Rare Title</span>.

With the following modifications, we can then assess the survival
likelihood of each title and gender:

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

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

<span class="hl">All</span> females, regardless of their titles, have
higher survival likelihoods than males. In addition, having a title
other than Mrs, Miss or Mr, elevates the survival likelihood of the
individual. One reason explaining this could be that females and
esteemed individuals were given early access to escape the Titanic.

<div class="st">

Insight 3: The younger the passenger, the more likely he/she will
survive.

</div>

The chart below shows how survival likelihood changes with age.

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
                            aes(x = AgeMin + 8.5, label = paste(AgeMin, "Years Old")),
                            y = max(age_pdata$SurviveL),
                            color = ltxt_color,
                            size = 5,
                            family = def_font)
  
age_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

Among those passengers whose age were populated, we can see that younger
individuals are more likely to survive. One possible explanation could
be because babies and toddlers occupy less space (and hence easier to
find seats within the lifeboats).

However, older people, especially those more than 60 years old, are less
likely to survive. This is probably due to their lack of agility in
responding to the crash.

<div class="st">

Insight 4: An individual is more likely to survive if he/she has other
family members with him/her.

</div>

Typically, we would expect that the more family members, the more spaces
the family unit would need in the lifeboat, hence reducing the survival
probability of each member as a whole.

However, contrary to expectations, the data shows that the larger the
<a data-toggle="popover" title="Family Size" data-content="The number of family members on board the Titanic, including the passenger. Examples of family members include spouses, siblings, parents and children.">family
size</a>, the more likely an individual will survive:

``` r
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
  
company_plot 
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

This counterintuitive relationship can be explained through 3 factors:

  - <span class="hl color-3-text">Child Privilege</span>: If you have a
    larger family size, there is an increased probability that you are a
    child. For example, a family of 5 will more likely include 2 adults
    and 3 children, rather than 5 adults. Furthermore, from Insight 3,
    we know that children have higher survival likelihoods than adults.
    Hence, by the transitive property, it follows that: <br/><br/>
    Larger Family Size -\> More Likely To Be A Child -\> More Likely to
    Survive.<br/><br/> The benefits accrued by this privilege, shown in
    <span class="hl color-3-text">red</span>, can be accounted for when
    we compare data from all passengers against data from only the
    adults (\> 15 years old)

  - <span class="hl color-2-text">Parental Privilege</span>: The larger
    the family size, the more likely you are to be a parent. Parents can
    utilize their children’s lifeboat guarantee to tag-along and get
    themselves a space. These benefits, which are represented in
    <span class="hl color-2-text">green</span>, surface when we compare
    the survival likelihoods of adults with children and adults with
    none.

  - <span class="hl color-1-text">Husband Privilege</span>: As a male,
    having a family size of 2 and no children may guarantee you better
    odds since you can tag along with your wife (see Insight 2) to
    obtain a lifeboat seat. These benefits, shown in
    <span class="hl color-1-text">blue</span>, can be accounted for when
    we compare between males and females that do not have children.

<div class="st">

Insight 5: Cabin positions should have an impact of survivalhood, but
only to a certain extent.

</div>

Passengers staying further away from the lifeboats will have more
difficult time surviving, hence we should assume that the cabin
positions will have an impact on their survivalhood. However, given that
passengers may not be at their cabins during the time of crash, this
impact may not be significant.

A more detailed deck plan can be found in the
<a href="https://www.encyclopedia-titanica.org/titanic-deckplans/b-deck.html" target="_blank">Encyclopedia
Titanica</a>. As shown from the deck plans, the letter of the cabin
represents the floor of the room, while the odd and even numbers
corresponds to the left and right side of Titanic respectively.

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
  
cabinLet_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

From the chart above, we know that <span class="hl">Cabins B to E</span>
has a higher likelihood of survival compared to other cabins. This
implies that the position of cabins play a role in determining whether a
passenger survive.

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
  
cabinNumber_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

It is pretty clear that those who stay in the odd rooms are more likely
to survive than those in the even rooms.

In conclusion, cabin floors and cabin numbers, when available, can
determine a passenger’s survival likelihood.

<div class="st">

Insight 6: Port of embarkation has an impact on survivalhood, as they
are correlated with Passenger class.

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

<div id="htmlwidget-3c00dae6adc9b698a5a1" class="leaflet html-widget" style="width:100%;height:268.8px;">

</div>

<script type="application/json" data-for="htmlwidget-3c00dae6adc9b698a5a1">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false}]},{"method":"addAwesomeMarkers","args":[41.7666636,-50.2333324,{"icon":"ship","markerColor":"gray","iconColor":"#FFFFFF","spin":false,"squareMarker":false,"iconRotate":0,"font":"monospace","prefix":"fa"},null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},"Titanic Crash Site",null,null,null,null,null,null]},{"method":"addCircleMarkers","args":[49.645009,-1.62444,10,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#94A162","weight":5,"opacity":0.8,"fill":true,"fillColor":"#94A162","fillOpacity":0.5,"dashArray":null},null,null,"Cherbough<br>Survival Likelihood: 55%",null,null,null,null]},{"method":"addCircleMarkers","args":[51.851,-8.2967,10,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#B55C5C","weight":5,"opacity":0.8,"fill":true,"fillColor":"#B55C5C","fillOpacity":0.5,"dashArray":null},null,null,"Queenstown<br>Survival Likelihood: 39%",null,null,null,null]},{"method":"addCircleMarkers","args":[50.9038684,-1.4176118,15,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#B55C5C","weight":5,"opacity":0.8,"fill":true,"fillColor":"#B55C5C","fillOpacity":0.5,"dashArray":null},null,null,"Southampton<br>Survival Likelihood: 34%",null,null,null,null]}],"limits":{"lat":[41.7666636,51.851],"lng":[-50.2333324,-1.4176118]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

<br> The map above indicates that those who embarked at Cherbough are
more likely to survive. This is a puzzling observation as where the
passenger embarks should not have an impact of what is happening on the
Titanic.

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

embark_pclass_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

However, by studying the demographics of the passengers embarking at
each port, we discovered that a higher proportion of Cherbough are 1st
class passengers. This would explain the confounding observation noted
earlier.

## Modeling Survival Likelihood

### Rationale

While the preliminary insights were useful in identifying survival
factors, we also need a way to quantify the importance of each factor.
Building a predictive model would achieve that goal.

Since this is a classification problem (for each passenger, we either
group them as “Survived” or “Died”), models such as logistic regression
and decision trees come to mind. However, for the purposes of this
study, the
<a href="https://en.wikipedia.org/wiki/Random_forest" target="_blank">Random
Forest</a> algorithm was used, as it is more robust and there already
exists a useful function <code>importance</code> that can assess the
various factors intuitively.

### Preparation

#### Variables Considered

Based on the above insights, we will be adding the following factors to
predict the survival likelihood of an individual.

| Feature    | Variable Type | Insights Supporting the Feature |
| ---------- | ------------- | ------------------------------- |
| Pclass     | Continuous    | Insight 1                       |
| Fare       | Continuous    | Insight 1                       |
| Sex        | Categorical   | Insight 2                       |
| Title      | Categorical   | Insight 2                       |
| Age        | Continuous    | Insight 3                       |
| FamilySize | Continuous    | Insight 4                       |

\*Initially, CabinFloor, CabinNumber and Port of Embarkation were used
as predictors of survival likelihood. However, the accuracy of the model
fell with the inclusion of these features due to over-fitting. This
implies that we may be over-inflating the impact of embarkation port,
while the cabin data is too sparse to yield any useful information.

#### Populating Age

Before inputting the raw data into a model, we have to ensure that the
data is in a well-defined state. One of the fixes we need to make is to
populate any unidentified <span class="hl">Age</span> values.

We will use the following variables as predictors of Age:

  - <span class="hl">Pclass</span>: 1st class members (38 years old on
    average) are generally older than 2nd (30) or 3rd (25) class
    members.

  - <span class="hl">Title</span>: Masters (5 years old on average) and
    Misses (22) tend to be younger than Mr (32) and Mrs (36).

  - <span class="hl">ConfirmedAdult</span>: This is a new category
    created just to predict age. Assuming that there are no
    intergenerational families in Titanic (i.e. an individual having
    both parents and children), we know that having more than 2
    parent-children relationships strongly implies an individual is a
    parent (since a child can have a maximum of 2 parent relationships).
    These individuals are what we call
    <span class="hl">ConfirmedAdults</span>. Such passengers are 9 years
    older on average than the rest of the passengers.

  - <span class="hl">FamilySize</span>: Based on Insight 4, we know that
    a larger family size implies a higher probability of being a child.
    In fact, the correlation between age and family size is modest at
    -30%.

<a data-toggle="popover" title="ANOVA Regression" data-content="A regression similar to the standard linear regression, with the added capability of having categorical values as independent variables.">ANOVA
regression</a> will be used to predict Age based on the factors above.

Listed below is a summary of the regression.

``` r
appendConfirmedAdult <- function(data) {
    data %>%
    mutate(ConfirmedAdult = ifelse(Parch > 2, 1, 0))
}

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
```

    ## 
    ## Call:
    ## lm(formula = Age ~ Pclass + Title + ConfirmedAdult + FamilySize, 
    ##     data = age_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -24.347  -8.213  -1.152   6.295  44.647 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          29.0606     2.8020  10.372  < 2e-16 ***
    ## Pclass               -5.8879     0.5184 -11.357  < 2e-16 ***
    ## TitleMiss             9.9067     2.2406   4.422 1.13e-05 ***
    ## TitleMr              19.9798     2.2397   8.921  < 2e-16 ***
    ## TitleMrs             22.2223     2.3357   9.514  < 2e-16 ***
    ## TitleRare Title / F  12.8385     4.7922   2.679  0.00756 ** 
    ## TitleRare Title / M  28.1837     3.3831   8.331 4.18e-16 ***
    ## ConfirmedAdult       16.2836     3.3101   4.919 1.08e-06 ***
    ## FamilySize           -2.0239     0.3600  -5.621 2.73e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.07 on 705 degrees of freedom
    ## Multiple R-squared:  0.426,  Adjusted R-squared:  0.4195 
    ## F-statistic: 65.41 on 8 and 705 DF,  p-value: < 2.2e-16

Due to our careful selection of variables, all of the coefficients are
statistically significant with a 5% significance level. This suggests
that the variables are all good predictors of Age.

#### Populating Others

Other than age, we also need some kind of sanity checks to ensure that
all data has been populated. Since the rest of columns are either too
varied, or require only a small set of missing values to be filled, we
will populate then with category “Unknown” or the number -1.

#### Putting It All Together

After resolving the missing data, we can now create a process that takes
in the raw data and output a cleansed data that is ready for model
consumption.

Below is an example of the data after cleansing:

``` r
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
  
pander(head(cleaned_set))
```

| Survived | Pclass | Fare  | Sex    | Title |  Age  | FamilySize |
| :------- | :----: | :---: | :----- | :---- | :---: | :--------: |
| 0        |   3    | 7.25  | male   | Mr    |  22   |     2      |
| 1        |   1    | 71.28 | female | Mrs   |  38   |     2      |
| 1        |   3    | 7.925 | female | Miss  |  26   |     1      |
| 1        |   1    | 53.1  | female | Mrs   |  35   |     2      |
| 0        |   3    | 8.05  | male   | Mr    |  35   |     1      |
| 0        |   3    | 8.458 | male   | Mr    | 29.35 |     1      |

### The Model

Prior to constructing the random forest model, we will split the
training set into two unequal groups, with 80% of the data for training
and the other 20% for testing purposes. The larger partition is then
used to construct a random forest model.

Listed below is a summary of the model.

``` r
set.seed(1) 
shuffled_set <- training_set[sample(nrow(training_set)),]
training_grp <- shuffled_set[1:floor(0.80 *nrow(shuffled_set)),]
testing_grp <- shuffled_set[floor(0.80 *nrow(shuffled_set) + 1):nrow(shuffled_set),]
  
# Create age estimator using all the dataset
age_m <- age_model(rbind(training_grp, testing_grp))

set.seed(1) 
rf_model <- randomForest(Survived ~ .,
                         data = clean_data(training_grp, age_m),
                         importance=TRUE,
                         ntree=2000)

rf_model
```

    ## 
    ## Call:
    ##  randomForest(formula = Survived ~ ., data = clean_data(training_grp,      age_m), importance = TRUE, ntree = 2000) 
    ##                Type of random forest: classification
    ##                      Number of trees: 2000
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 16.29%
    ## Confusion matrix:
    ##     0   1 class.error
    ## 0 392  45   0.1029748
    ## 1  71 204   0.2581818

#### Assessing Performance

We use three different indicators to test the accuracy of the model:

1.  <span class="hl">OOO</span>: The accuracy calculated internally
    within the model,
2.  <span class="hl">20% Testing Group</span>: The accuracy when used to
    predict the remaining 20% of the data not used in calibration,
3.  <span class="hl">Kaggle’s Testing Set</span>: The accuracy when used
    to predict the test set provided by
Kaggle.

<!-- end list -->

``` r
ooo_str <- paste0("Based on OOO: ",round((1 - median(rf_model$err.rate[,1]))*100,1),"% Accuracy Rate")

# Compare using the testing group
p_y <- predict(rf_model, newdata = clean_data(testing_grp %>% select(-Survived), age_m, is_training=FALSE))
cmp <- cbind(testing_grp %>% select(actual=Survived), predicted=p_y) %>%
             summarise(size=n(),
                       err=sum(ifelse(actual != predicted,1,0)))
int_str <- paste0("Based on 20% Testing Group: ",round((1 - cmp$err / cmp$size) * 100,1),"% Accuracy Rate")
    
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
ext_str <- paste0("Based on Kaggle's Testing Set: 79.4% Accuracy Rate")

cat(paste0(ooo_str,"\n",int_str,"\n",ext_str))
```

    ## Based on OOO: 83.7% Accuracy Rate
    ## Based on 20% Testing Group: 83.2% Accuracy Rate
    ## Based on Kaggle's Testing Set: 79.4% Accuracy Rate

As shown above, the model managed to predict Kaggle’s testing set
relatively well, with a score below the 18th percentile in the
<a href="https://www.kaggle.com/c/titanic/leaderboard" target="_blank">Leaderboard</a>
(at time of submission). There is also only a difference of 2-4% between
our internal (20% Testing Group) and external (Kaggle’s Testing Set)
performance, suggesting that the model is not overfitted.

#### Assessing Factors

Since the model is robust and reliable, we can now derive some knowledge
from the model. We will reference the
<a data-toggle="popover" title="Mean Decrease In Accuracy" data-content="The degree to which accuracy will fall if the feature/factor of interest is excluded from calibration. (To be more specific, the feature is not excluded, but randomized. Nevertheless, the impact to the model remains the same.) The higher the mean decrease in accuracy, the more important the factor is to the prediction.">mean
decrease in accuracy</a> score to determine the importance of each
factor.

The chart below maps the mean decrease in accuracy for each feature in
the
model:

``` r
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
  
z_plot
```

<img src="/Users/lemuel/Google Drive/Website/git/titanic/README_files/figure-gfm/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

Using a 1% significant level as the baseline, we conclude that all the
factors are statistically significant. This means that for each of the
factor, there is less than a 1% chance that the influence on survival
likelihood is a result of randomness.

Moreover, the model also provides us a picture of which factors are more
important than others. For example, characteristics associated with
wealth (<span class="hl blue-text">Fare</span> and
<span class="hl blue-text">Passenger Class</span>) play more important
roles than the passengers’ demographics
(<span class="hl purple-text">Title</span>,
<span class="hl orange-text">Age</span> and
<span class="hl yellow-text">Gender</span>).

## Summary of Results

Using data from the demographics and outcomes of titanic passengers, we
have identified a few crucial factors that determines a passenger’s
survivalhood.

These factors, ranked by importance, are:

1.  <span class="hl cyan-text">Passenger Class</span>: 1st and 2nd class
    passengers accrue additional benefits, such as better amenities and
    cabin size within the Titanic. Ironically, the benefit most crucial
    to them was probably the one not advertised: exclusive access to the
    lifeboats.
2.  <span class="hl blue-text">Fares Paid</span>: This is connected to
    the 1st factor. The higher the fares that passengers pay, the more
    benefits they accrue, hence the more premium they possess during a
    sinking disaster.
3.  <span class="hl purple-text">Title</span>: Someone with an esteemed
    title have more privileges than a lay individual on almost every
    occassion, including this life-or-death situation.
4.  <span class="hl red-text">Family Size</span>: Someone with
    dependents (whether children or a wife) has an advantage of getting
    into the lifeboats since they can “tag-along” with their dependents.
5.  <span class="hl orange-text">Age</span>: The younger the passenger,
    the less space he/she occupies, hence there are more opportunities
    for him/her to board the lifeboats.
6.  <span class="hl yellow-text">Sex</span>: During that era, females
    were prioritized over males as a form of gentlemanly behavior. This
    behavior seems to be translated during the sinking disaster, where
    females were given preference over men to board the lifeboats first.
