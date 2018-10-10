
# Initialization ----
## ---- init

# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")
# Load some helper functions
source("shared/helper.R")

options(stringsAsFactors = FALSE)
load_or_install.packages("purrr","mgcv","DMwR")

data_dir <- "data/"
output_dir <- "output/"

## ---- end-of-init

# DATA OVERVIEW ### ----

## ---- data_comp
training_set <- read.csv(paste0(data_dir, "train.csv"))

cols_summary <- data_overview(training_set)

## ---- end-of-data_comp

# DETERMINING THE FEATURES ### ----

# * Feature Extraction ----

## ---- f_title

prefix <- function(l) { as.character(sapply(l, ..(x) %:=% { unlist(strsplit(unlist(strsplit(x, ", "))[2],"\\. "))[1] })) }

title_set <- training_set %>%
             mutate(Title = prefix(Name))

# Find Titles that Have Sufficient Sample Size For Model Estimation (>10)
common_titles <- title_set %>%
                 group_by(Title) %>%
                 summarise(N=n()) %>%
                 filter(N > 10) %>%
                 select(Title) %>%
                 { c(.$Title) }

title_plot <- training_set %>%
              mutate(Title = ifelse(prefix(Name) %in% common_titles, prefix(Name), "Rare Title")) %>%
              mutate(MaleSurvivalRate = sum(Sex == "male" & Survived == 1) / sum(Sex == "male"),
                     FemaleSurvivalRate = sum(Sex == "female" & Survived == 1) / sum(Sex == "female"),
                     MixedSurvivalRate = sum(Survived) / n()) %>%
              group_by(Title) %>%
              summarise(CohortSize = n(),
                        SurvivalRate = sum(Survived)/n(),
                        Gender = sum(Sex == "male")/n(),
                        MaleSurvivalRate = MaleSurvivalRate %>% max,
                        FemaleSurvivalRate = FemaleSurvivalRate %>% max,
                        MixedSurvivalRate = MixedSurvivalRate %>% max) %>%
              mutate(Gender = ifelse(Gender == 0, "Female",
                                     ifelse(Gender == 1, "Male", "Mixed")),
                     BaselineRate = recode(Gender, Female=FemaleSurvivalRate, Male=MaleSurvivalRate, Mixed=MixedSurvivalRate)) %>%
              arrange(Gender, desc(SurvivalRate)) %>%
              mutate(Title = factor(Title, levels=Title %>% unique)) %>%
              {
                  ggplot(.,aes(x=Title, y=BaselineRate, fill=Gender)) +
                  theme_lk() +
                  geom_bar(stat="identity", 
                           alpha = 0.2,
                           width = .5, 
                           position=position_nudge(x=-0.25)) +                                       
                  geom_tile(aes(y=(BaselineRate + (SurvivalRate - BaselineRate) / 2.), 
                                height=SurvivalRate-BaselineRate), 
                            width = .5, 
                            fill = (.$SurvivalRate - .$BaselineRate >= 0) %?% `@c`(green) %:% `@c`(red),
                            position = position_nudge(x=+0.2)) + 
                  geom_text(data=subset(.,SurvivalRate - BaselineRate >= 0),
                            aes(label = "+" %|% scales::percent(SurvivalRate - BaselineRate),
                                y = SurvivalRate + 0.02),
                            nudge_x = 0.2,
                            family = `@f`,
                            color = `@c`(green)) +
                  geom_text(data=subset(.,SurvivalRate - BaselineRate < 0),
                            aes(label = scales::percent(SurvivalRate - BaselineRate),
                                y = SurvivalRate - 0.02),
                            nudge_x = 0.2,
                            family = `@f`,
                            color = `@c`(red)) +
                  scale_fill_manual(name = "Baseline Survival Rate",
                                    values = c("Male"= `@c`(blue),
                                               "Female" =`@c`(red),
                                               "Mixed" = `@c`(purple))) +
                  scale_y_continuous(name="Survival Likelihood", 
                                     labels=scales::percent,
                                     expand=c(0,0,0.02,0))
              }

feature_title <- function(Name) { ifelse(prefix(Name) %in% common_titles, 
                                         ifelse(prefix(Name) %in% c("Mr","Miss"),"Mr/Miss",prefix(Name)), "Rare Title") }

## ---- end-of-f_title

## ---- f_fam_size

feature_famsize <- function(Sibsp, Parch) { pmap(list(x=Sibsp, y=Parch), ..(x,y) %:=%  { x + y + 1 }) %>% as.integer }

famsize_plot <- training_set %>%
                mutate(FamSize = feature_famsize(SibSp, Parch)) %>%
                {
                  ggplot(., aes(x=FamSize, fill=factor(Survived))) +
                    theme_lk() +
                    theme(
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank()
                    ) +
                    geom_density(color=NA, alpha=0.5, bw="bcv") +
                    scale_x_continuous(name="Family Size", expand=c(0,0), breaks=1:5*2) + 
                    scale_y_continuous(name="Density", expand=c(0,0,0.02,0)) +
                    scale_fill_manual(name="Cohort Size", values=c(`1`=`@c`(1),`0`=`@c`(ltxt)),
                                                          labels=c(`1`="Survived",`0`="Died"))
                }
  
## ---- end-of-f_fam_size

## ---- f_cabin_deck

deck_to_index <- c("A"=1,"B"=2,"C"=3,"D"=4,"E"=5,"F"=6,"G"=7)

feature_cabin_deck <- function(Cabin) {
  
  lapply(Cabin, ..(c) %:=% {
    
    # Get All Cabins Specified
    decks <- strsplit(c %>% { gsub("[0-9]","",.) }," ") %>% unlist %>%
            # Convert Cabins to Numeric
            sapply(..(f) %:=% {
              deck_to_index[f]
            })
    
    # Assume a Leave No Man Behind Approach,
    # In other words, your chances of survival is dependent on
    # the person with the worst deck in the group
    (length(decks) == 0) %?% NA %:% (decks %>% as.integer %>% max)
                
  }) %>% unlist
}

cabin_deck_plot <- training_set %>%
                    mutate(CabinDeck = feature_cabin_deck(Cabin)) %>%
                   filter(!is.na(CabinDeck)) %>%
                  {
                    ggplot(., aes(x=CabinDeck, fill=factor(Survived))) +
                      theme_lk() +
                      theme(
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank()
                      ) +
                      geom_density(color=NA, alpha=0.5, bw="bcv") +
                      scale_x_continuous(name="Cabin Deck", expand=c(0,0), breaks=deck_to_index, labels=setNames(names(deck_to_index),deck_to_index)) + 
                      scale_y_continuous(name="Density", expand=c(0,0,0.02,0)) +
                      scale_fill_manual(name="Cohort Size", values=c(`1`=`@c`(1),`0`=`@c`(ltxt)),
                                        labels=c(`1`="Survived",`0`="Died"))
                  }
                   
## ---- end-of-f_cabin_deck

## ---- f_cabin_no

feature_cabin_no <- function(Cabin) {
  
  sapply(Cabin, ..(x) %:=% {
    if (x == "") { return(NA) }
    
    num <- as.integer(strsplit(gsub("[A-z]","",x)," ")[[1]]) %% 2
    
    if (is.nan(mean(num)) | length(which(num==0)) == length(which(num==1))) {
      return(NA)
    } else {
      # Check if there is more even cabins or odd cabins
      if (length(which(num==0)) >= length(which(num==1))) { return("Even") } else { return("Odd") }
    }
  })
}

cabin_number_plot <- training_set %>%
                      mutate(CabinNo = feature_cabin_no(Cabin),
                             CabinNo = ifelse(is.na(CabinNo),"Unspecified",CabinNo)) %>%
                      group_by(CabinNo) %>%
                      summarise(CohortSize = n(),
                                SurvivalRate = sum(Survived)/n()) %>%
                      mutate(CabinNo = factor(CabinNo, levels=c("Even","Odd","Unspecified"))) %>%
                      {
                        ggplot(., aes(x = SurvivalRate,y = 0,color=CabinNo,size=CohortSize)) +
                          theme_lk() +
                          theme(plot.margin = unit(c(0,0,0,-40),'pt'),
                                legend.position = c(1.,0.05),
                                legend.box.just = c(0.5,0.5),
                                axis.line.x = element_line(colour=NA),
                                axis.ticks.x = element_line(colour=NA),
                                axis.title.x = element_blank(),
                                axis.text.x = element_blank(),
                                axis.line.y = element_line(colour=NA),
                                axis.ticks.y = element_line(colour=NA),
                                axis.title.y = element_blank(),
                                axis.text.y = element_blank()
                          ) +
                          scale_color_manual(name = "Room Type",
                                             values = `@c`(),
                                             guide = guide_legend(order = 1,override.aes=list(size=5),
                                                                  nrow= 1)) +
                          scale_size_continuous(name = "Cohort Size",
                                                range = c(5,20),breaks=c(100,300,600),
                                                guide = guide_legend(order = 2,override.aes=list(alpha=0.5), nrow=1)) +
                          # Add X Axis Line
                          geom_segment(data = data.frame(1),aes(x=0.25, xend=0.8, y=0, yend=0),
                                       size = 0.5,color=alpha(`@c`(ltxt),0.5),
                                       arrow = arrow(length = unit(10,"pt"),type = "closed")) +
                          geom_text(data = data.frame(1),label = "Survival Likelihood",
                                    x = 0.8,y = 0.02,family = `@f`,color = `@c`(ltxt),
                                    size = 4.5,hjust = 1) +
                          scale_y_continuous(limits=c(-0.12, 0.05)) +
                          geom_point() +
                          geom_text(aes(y = c(-0.025,-0.02,-0.04), label=paste0(round(SurvivalRate*100,0),"%")),
                                    size=4,color=`@c`(txt),family=`@f`)
                      }

## ---- end-of-f_cabin_no

## ---- features

preprocess_data <- function(data) {
  data %>%
    mutate(Title=feature_title(Name),
           FamSize=feature_famsize(SibSp, Parch),
           CabinDeck=feature_cabin_deck(Cabin),
           CabinNo=feature_cabin_no(Cabin)) %>%
    select(-PassengerId, -Name, -Ticket, -Cabin) %>%
    # Substitute unknown continuous features with -1
    mutate_if(is.numeric, 
              ~ map(., ~ if (is.na(.)) { -1 } else {.}) %>% unlist) %>%
    # Substitute unknown discrete features with "-"
    mutate_if(~ !is.numeric(.),
              ~ map(., ~ if (is.na(.) | . == "") { "-"} else {.}) %>% unlist) %>%
    # Set Factors for Discrete Features
    mutate_if(~ !is.numeric(.),
              ~ factor(., levels=unique(c("-",.))))
}

features <- preprocess_data(training_set)

## ---- end-of-features

# MODELING SURVIVAL LIKELIHOOD ### ----

## ---- gam

predict.mod_gam <- function(object, newdata, type="link", threshold=0.5, ...) {
  
  # There is a Bug in GAM where if we use "by", predict will crash with the following error message:
  # newdata is a model.frame: it should contain all required variables
  # This is an attempt to fix that
  gp <- interpret.gam(object$formula)
  # if new data has no Survived column, append to fix the bug
  if (!("Survived" %in% colnames(newdata))) {
    newdata[,"Survived"] <- rep(-999,nrow(newdata))
  }
  
  newdata <- model.frame(gp$fake.formula, newdata)
  if (type == "class") {
    (predict.gam(object=object, newdata=newdata, type="response", ...) >= threshold) * 1
  } else {
    predict.gam(object=object, newdata=newdata, type=type, ...)
  }
}

mod.gam <- function(data) {
  # 1. Specify List of Features
  all_features <- data %>% select(-Survived)
  cont_vars <- all_features %>% select_if(is.numeric) %>% colnames %>% 
    map(~ "s(" %|% . %|% 
          ", k=" %|% { (length(unique(data[,.])) >= 10) %?% -1 %:% length(unique(data[,.])) } %|% 
          " ,by=(" %|% . %|%  " >= 0)*1)") %>% unlist
  disc_vars <- all_features %>% select_if(~ !is.numeric(.)) %>% colnames
  
  # Helper Function to Build Modified GAM Model
  build_gam(vars) %:=% {
    all_vars <- vars %>% paste0(collapse=" + ")
    m.gam <- gam("Survived ~ " %|% all_vars %>% as.formula, data=data, family="binomial")
    class(m.gam) <- c("mod_gam",class(m.gam))
    if (max(summary(m.gam)$p.table[,2]) >= 10) { warning("High Standard Errors.") } 
    m.gam
  }  
  
  # 2. Perform Forward Selection by comparing chi square statistic
  cur_vars <- c()
  rem_vars <- c(cont_vars,disc_vars)
  last.gam <- build_gam(c("1"))
  for (i in 1:length(rem_vars)) {
    # We attempt to find out which variables to put in next.
    # There are three extreme cases where we want to prevent the particular
    # var from being inserted
    # 1. If GAM calibration returns an error
    # 2. If GAM calibration returns a warning (often no convergence)
    # 3. If GAM calibration results in high standard errors
    chi_sq <- sapply(rem_vars, ..(v) %:=% {
      tryCatch({ 
        m.gam <- build_gam(c(cur_vars,v)); 
        (last.gam$deviance - m.gam$deviance) / ((m.gam$edf %>% sum) - (last.gam$edf %>% sum)) }, 
        warning= ..(e) %:=% { return(-Inf) },
        error= ..(e) %:=% { return(-Inf) })
      })
    
    # If all of the remaining vars cause errors, terminate forward selection
    if (max(chi_sq) == -Inf) { break }
    
    best.var <- chi_sq %>% which.max %>% names
    cur_vars <- c(cur_vars, best.var)
    rem_vars <- rem_vars[rem_vars != best.var]
    last.gam <- suppressWarnings(build_gam(cur_vars))
  }  
  
  # 3. Return Model
  lapply(1:length(c(cont_vars, disc_vars)), ..(i) %:=% {
    if (i > length(cur_vars)) { return(NULL) }
    build_gam(cur_vars[1:i])
  })
}

m.gam <- cache("m_gam", list(), ..() %:=% { mod.gam(features) })

## ---- end-of-gam

## ---- cv

# Create Cross Validation Function
cv.mod.gam <- function(formula, train, test) {
  # 1. Generate Modified GAM Models
  gams <- mod.gam(train)
  
  # 2. Get the MSE's for each GAM Model at different thresholds
  output <- list()
  cur_env <- environment()
  thresholds <- 1:19*0.05
  walk(thresholds, ..(t) %:=% {
    err_rates <- sapply(gams, ..(i) %:=% {
      if (is.null(i)) { return(Inf) }
      { predict(i, test, type="class", threshold = t) != test$Survived } %>%
      { length(which(.)) / length(.) }
    })
    names(err_rates) <- t %|% "," %|% 1:length(err_rates)
    assign("output",append(output, err_rates), envir = cur_env)
  })
  
  output
}

# Run Cross Validation With Different Folds
# 5-Fold Focuses more on Variance Error while 10-Fold Focuses more on Bias Error
res.cv <- cache("res_cv", list(), ..() %:=% { 
            crossValidation(learner("cv.mod.gam"), 
                            dataset(Survived ~ ., features),
                            cvSettings(s = 1))
          }) %>% 
          # Reformat CV Results Into a Table
          { tmp_tbl <- .@foldResults %>% as.data.frame
            tmp_tbl[,"Fold"] <- rownames(tmp_tbl)
            tmp_tbl } %>%
          gather("Params","Err_Rate",-Fold) %>%
          mutate(Err_Rate = Err_Rate %>% unlist) %>%
          group_by(Params) %>%
          summarise(MER = mean(Err_Rate),
                    SE = sd(Err_Rate)/sqrt(n()))

res.cv[,"Threshold"] <- map(res.cv$Params, ~strsplit(.,",")[[1]][1]) %>% unlist %>% as.numeric
res.cv[,"N_Params"] <- map(res.cv$Params, ~strsplit(.,",")[[1]][2]) %>% unlist %>% as.numeric

opt.row <- res.cv %>% arrange(MER) %>% { .[1,] }
opt.threshold <- opt.row$Threshold 
opt.n <- opt.row$N_Params
opt.gam <- m.gam[[opt.n]]
opt.gam$threshold <- opt.threshold

se <- opt.row$SE %>% as.numeric
res.cv[,"Min_SE"] <- res.cv[,"MER"] - opt.row$MER - se
onese.row <- res.cv %>% filter(Min_SE <= 0) %>% arrange(N_Params, (Threshold-0.5)^2) %>% { .[1,] }
onese.threshold <- onese.row$Threshold
onese.n <- onese.row$N_Params
onese.gam <- m.gam[[onese.n]]
onese.gam$threshold <- onese.threshold

cv_plot <- ggplot(res.cv %>% filter(MER != Inf & Threshold >= 0.25 & Threshold <= 0.75), 
                  aes(x=N_Params, y=Threshold)) +
            theme_lk() +
            geom_tile(aes(alpha=ifelse(Threshold == opt.threshold & N_Params == opt.n, 99,
                                       ifelse(Threshold == onese.threshold & N_Params == onese.n, 99, MER)),
                          fill=ifelse(Threshold == opt.threshold & N_Params == opt.n, "Optimal",
                                      ifelse(Threshold == onese.threshold & N_Params == onese.n, "One SE", NA)))) +
            geom_text(data=res.cv %>% subset((Threshold == opt.threshold & N_Params == opt.n) | 
                                               (Threshold == onese.threshold & N_Params == onese.n)),
                      aes(label=ifelse(Threshold == opt.threshold & N_Params == opt.n, "Optimal", "One SE")),
                      alpha = 1.,
                      color = `@c`(bg),
                      family = `@f`) +
            scale_x_continuous(name = "Number of Features", breaks = 1:4 * 2,
                               expand = c(0,0)) +
            scale_y_continuous(name = "Threshold", labels=scales::percent,
                               expand = c(0,0)) + 
            scale_alpha_continuous(name="CV Error Rate", range=c(0.05,1), 
                                   limits=c(NA,0.25), na.value=1,
                                   labels=scales::percent, 
                                   guide=guide_legend(override.aes=list(fill=`@c`(ltxt,0.8)))) +
            scale_fill_manual(values=c("Optimal"=`@c`(green), "One SE"=`@c`(blue)), na.value=`@c`(txt,0.8),
                              guide="none")

## ---- end-of-cv

## ---- p

test_set <- read.csv(data_dir %|% "test.csv")
test_features <- test_set %>% preprocess_data
save_predictions <- function(m.gam, id="opt") {
  
  p_y <- predict(m.gam, test_features, type="class", threshold=m.gam$threshold)
  predictions <- cbind(test_set %>% select(PassengerId), Survived=p_y)
  write.csv(predictions, 
            file=paste0(output_dir,id,"_submission.csv"),
            row.names = FALSE)
}

save_predictions(opt.gam)
save_predictions(onese.gam, "onese")

## ---- end-of-p

## ---- i_threshold

threshold_plot <- res.cv %>% filter(N_Params == onese.n) %>%
                  {
                    ggplot(., aes(x=Threshold, y=MER)) +
                    theme_lk() +
                    geom_point(aes(color=Threshold != onese.threshold,
                                   shape = Threshold != onese.threshold,
                                   size = Threshold != onese.threshold), 
                               show.legend=FALSE) +
                    scale_color_manual(values=c(`@c`(red),`@c`(ltxt,0.5))) +
                    scale_shape_manual(values=c(4,16)) +
                    scale_size_manual(values=c(5,3)) +
                    geom_smooth(color=`@c`(1), fill=`@c`(ltxt,0.2), method='loess') +
                    scale_x_continuous(labels=scales::percent) + 
                    scale_y_continuous(name="CV Error Rate", labels=scales::percent)
                  }

## ---- end-of-i_threshold

## ---- i_features

coeffs <- predict(onese.gam, features, type = "terms") %>% as.data.frame %>%
          mutate(ID = rownames(.)) %>%
          gather("Feature","Val",-ID) %>%
          mutate(Feature = gsub(":[A-Za-z()>=0-9\\* ]*|\\)|s\\(","",Feature))

x_vals <- features %>% mutate(ID = rownames(features)) %>%
          gather("Feature","X",-ID)

x_f_raw <- coeffs %>%
           inner_join(x_vals, by=c("ID","Feature"))

x_f_tbl <- x_f_raw %>%
           select(Feature, X, Val) %>%
           filter(X != -1) %>%
           group_by(Feature, X, Val) %>%
           summarise(CohortSize=n()) %>%
           ungroup() %>%
           arrange(Feature, X)

cont_tbl <- x_f_tbl %>%
            filter(grepl("[0-9\\.]+",X)) %>%
            mutate(X = X %>% as.numeric)

cont_plot <- ggplot(cont_tbl, aes(x=X, y=Val)) +
              geom_smooth(aes(weight=CohortSize), color=`@c`(ltxt,0.5), 
                          linetype="dotted", method="loess") +
              geom_point(aes(size=CohortSize), color=`@c`(1), alpha=0.8) +
              theme_lk() +
              scale_size_continuous(name="Cohort Size") +
              scale_y_continuous(name="f(X)") +
              facet_wrap(~toupper(Feature), ncol=2, scales="free", strip.position = "bottom")

## ---- end-of-i_features

## ---- i_features_2
discr_tbl <- x_f_tbl %>%
              filter(!grepl("[0-9\\.]+",X)) %>%
              arrange(Feature, desc(Val)) %>%
              mutate(X = factor(X, levels=unique(X)))

discr_plot <- ggplot(discr_tbl, aes(x=X, y=Val)) +
              theme_lk() +
              geom_col(fill=`@c`(1,0.5), color=`@c`(1)) +
              scale_y_continuous(name="f(X)", expand=c(0,0,0.01,0)) +
              facet_wrap(~toupper(Feature), ncol=2, scales="free_x", strip.position = "bottom")
## ---- end-of-i_features_2

## ---- i_importance

imp_tbl <- x_f_raw %>%
           group_by(Feature) %>%
           #Normalize all features so that the weighted sum of f(feature) = 0
           mutate(Val = abs(Val - sum(Val)/n())) %>%
           ungroup() %>% group_by(ID) %>%
           mutate(PctImpt = Val / sum(Val)) %>%
           group_by(Feature) %>%
           summarise(PctImpt = mean(PctImpt)) %>%
           arrange(PctImpt) %>%
           mutate(Feature = factor(Feature, levels=Feature))

imp_plot <- ggplot(imp_tbl, aes(fill=Feature, x=1, y=PctImpt)) + 
            theme_void() +
            theme_lk(fmt_x = FALSE, fmt_y = FALSE) +
            geom_col(position = position_stack(vjust = .5), 
                     width=0.7, show.legend = FALSE) +
            geom_text(aes(label=Feature, x=0.4),
                            size=3.5,
                            position = position_stack(vjust = .5),
                            family = `@f`,
                            color = `@c`(txt)) +
            geom_text(aes(label=scales::percent(round(..y..,2))),
                            position = position_stack(vjust = .5),
                            family = `@f`,
                            color = `@c`(bg)) +
            scale_fill_manual(values=`@c`(palette)(6) %>% rev) +
            lims(x = c(0.,NA)) +
            coord_flip()
  
## ---- end-of-i_importance
