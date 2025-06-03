## FAO FBS data extraction
## Time-stamp: <2021-04-28 23:39:25 valin>

## Requires 16 Go RAM memory to process the full database
## Total processing time: approx. 45 min
require(data.table)

FAO.version <- "2024jan04"
datapath <- paste("P:/GLOBIOM/Data/FAOSTAT/FAOSTAT extraction/FAOSTAT_",FAO.version,"/",sep="")
sourcepath <- "P:/GLOBIOM/Data/FAOSTAT/FAOSTAT_extraction_Sync/"
workpath <- paste("I:/ringwald/FAOSTAT/FAOSTAT_",FAO.version,"/",sep="")
dir.create("I:/ringwald/FAOSTAT/")
dir.create(workpath)

library(gdxrrw)
GAMSPath=c("C:/GAMS/35")
igdx(GAMSPath)

## Version data

data.info <- data.frame(Download_date=FAO.version,
                        Compilation_date=date(),
                        User_name=Sys.getenv("USERNAME"),
                        Computer_name=Sys.getenv("COMPUTERNAME"))
data.info.df <- data.frame(ID=colnames(data.info),Value=t(data.info))
attr(data.info.df,"symName") <- "A___Compilation_info___A"

## FAO Database listing

file.list <- data.frame(name=grep(".zip",list.files(datapath),value=TRUE))#read.table(paste0(sourcepath,"database_content.txt"))
exclude.file <- c("Investment_CountryInvestmentStatisticsProfile_E_All_Data_(Normalized).zip","CommodityBalances_(non-food)_(2010-)_E_All_Data_(Normalized).zip","CommodityBalances_(non-food)_(-2013_old_methodology)_E_All_Data_(Normalized).zip","Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip","World_Census_Agriculture_E_All_Data_(Normalized).zip","SDG_BulkDownloads_E_All_Data_(Normalized).zip")
file.list <- subset(file.list,!name %in% exclude.file)
#names(file.list) <- "name"
## one remove the two ForestryTradeFlows datasets for the moment (xls format)
file.csv <- NULL
file.csv <- data.frame(name=gsub(".zip","",file.list$name))

#file.csv <- subset(file.csv, ! name %in% exclude.file)
str(file.csv)

file.csv$name <- as.factor(file.csv$name)
##scan_only <- TRUE
scan_only <- FALSE

## Replacing files name where not matching with zip name
# a few files with suffixes not updated for csv from 'Norm' to 'Normalized'
##old.lab <- c(13)
##nonorm.lab <- c(62)

file.csv$csv.name <- levels(file.csv$name)[file.csv$name]
##file.csv$csv.name[old.lab] <- paste(substr(file.csv$csv.name[old.lab],1,nchar(file.csv$csv.name[old.lab])-7),")",sep="")
##file.csv$csv.name[nonorm.lab] <- substr(file.csv$csv.name[nonorm.lab],1,nchar(file.csv$csv.name[nonorm.lab])-13)

## match(f,file.csv$name)

## Output names without parenthesis
file.csv$out.name <- substr(levels(file.csv$name)[file.csv$name],1,nchar(levels(file.csv$name)[file.csv$name])-13)
#file.csv$out.name[file.csv$out.name=='Macro-Statistics_Key_Indicators_E_All_Data'] <- 'Macro_Statistics_Key_Indicators_E_All_Data'
file.csv$out.name <- gsub('-','',
                     gsub('\\(','',
                     gsub('\\)','',file.csv$name)))     

database.list <- list()

loop <- 1:dim(file.csv)[1]
f <- file.csv$name[1]
##loop <- 33:dim(file.csv)[1]
##loop <- c(36,37)
source("replace_all_function.R")
## Running time: 45 min
for (f in file.csv$name[loop]){
    cat("\rProcessing",f,"...")
    unzip(paste(datapath,f,".zip",sep=""),exdir=substr(workpath,1,nchar(workpath)-1))
##    system(paste("winrar e -ibck -o- ",path,f,".zip ",path,sep=""))
    f2 <- file.csv$csv.name[match(f,file.csv$name)]
    #fao.data <- if (scan_only) read.csv(paste(workpath,f2,".csv",sep=""), stringsAsFactors=FALSE)[1:2,] else read.csv(paste(workpath,f2,".csv",sep=""), stringsAsFactors=FALSE)
    fao.data <- if (scan_only) data.table::fread(paste(workpath,f2,".csv",sep=""),encoding="Latin-1")[1:2,] else fread(paste(workpath,f2,".csv",sep=""),encoding="Latin-1")
    fao.data <- as.data.frame(fao.data)
    cat(dim(fao.data))
    ncol <- length(names(fao.data))
    col.val <- match("Value",names(fao.data))
    col.set <- names(fao.data)[-col.val]
    fao.data$Flag <- as.character(fao.data$Flag)
    fao.data$Flag[is.na(fao.data$Flag)] <- ""
    fao.data$Unit <- as.character(fao.data$Unit)
    fao.data$Unit[is.na(fao.data$Unit)] <- ""
    if (!is.factor(fao.data[[col.val]])) fao.colval <- sapply(fao.data[col.val],as.numeric)
    if ( is.factor(fao.data[[col.val]])) fao.colval <- sapply(levels(fao.data[[col.val]])[fao.data[[col.val]]],as.numeric)
    fao.colset <- as.data.frame(lapply(fao.data[col.set],as.character))
    fao.colset <- as.data.frame(apply(fao.colset,2,replace_all_function))
    
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("SDG "), "", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0(" (no breakdown by urbanisation)"), "", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0(" of age "), " ", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste(c("estimates","estimated","estimate"),collapse="|"),"est.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("modeled"), "model", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Mountain Green Cover Index:"), "MGC IDX:", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Mountain Green cover area:"), "MGC area:", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0(" base period"), "=100", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Land Cover:"), "LC:", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0(", no breakdown by sex"), "", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0(", no breakdown by age"), "", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("\\b(", paste(c(" and/or","and/or"), collapse = "|"), ")\\b"), ",", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("\\b(", paste(c(" and"," or", "and", "or"), collapse = "|"), ")\\b"), ",", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("\\b(", paste(c("percent","Percent"), collapse = "|"), ")\\b"), "%", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("\\b(", paste(c("annual value"), collapse = "|"), ")\\b"), "annu.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("\\b(", paste(c("million"), collapse = "|"), ")\\b"), "mln", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("\\b(", paste(c("international $"), collapse = "|"), ")\\b"), "int $", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Number of"), "Nof", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("number of"), "nof", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Percentage of"), "Pof", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("percentage of"), "pof", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Prevalence of"), "Prev. of", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("population"), "pop.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Productivity"), "Prod.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("productivity"), "prod.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Proportion"), "Prop.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("proportion"), "prop.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Average"), "Avg.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("average"), "avg.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste(c("years old", "years of age"), collapse="|"), "y/o", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("year"), "yr", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("years"), "yrs.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("Indicator"), "Indic.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("indicator"), "indic.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("constant"), "const", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste(c("exclusive","excluding"),collapse="|"), "excl.", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("under"), "<", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("15 years old, over"), ">15 years old", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("agricultural land"), "agri. land", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste0("people"), "ppl", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste(c("Gross fixed capital formation","gross fixed capital formation"),collapse="|"), "GFCF", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub(paste(c("Gross domestic product","gross domestic product"),collapse="|"), "GDP", x, perl = TRUE)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub("\\) \\(", ",", x)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){sub("^\\d+\\.\\w+\\.\\d+\\s", "",x)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){sub("^\\d+\\.\\d+\\.\\d+\\w+\\s", "",x)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){sub("^\\d+\\d+\\.\\d+\\.\\d+\\w+\\s", "",x)}))
    # fao.colset <- as.data.frame(apply(fao.colset,2,function(x){sub("^\\d+\\.\\w+\\.\\d+\\:\\s", "",x)}))
   
     if(any(colnames(fao.colset) == c("Area"))) fao.colset$Area[which(fao.colset$Area=="Western Asia (exc. Armenia, Azerbaijan, Cyprus, Israel, Georgia)")] <- "Western Asia (exc. ARM, AZE, CYP, ISR, GEO)"

    #    fao.colset <- as.data.frame(apply(fao.colset,2,function(x){gsub("[[:space:]](?=[^()]*\\))", ",", x, perl = TRUE)}))
    fao.colset <- as.data.frame(apply(fao.colset,2,function(x){strtrim(x,63)}))
    fao.colset <- as.data.frame(lapply(fao.colset,factor))
    fao.data2 <- cbind(fao.colset,fao.colval)
    names(fao.data2)[dim(fao.data2)[2]] <- "Value"
    attr(fao.data2,"symName") <- file.csv$out.name[match(f,file.csv$name)]
    file.remove(paste(workpath,f2,".csv ",sep=""))
    cat(dim(fao.data2))
    database.list <- c(database.list,list(fao.data2))
}

## Manual adjustment of some columns

database.list.sav <- database.list

##str(database.list)
##length(database.list)

## Flags too long
## database.list[[6]]$Flag <- factor("")

## \n character detected
## for(k in 9:10){
##     nvec <- as.character(database.list[[k]]$Item)
##     nvec[nvec=="All Crops\n + (Total)"] <- "All Crops + (Total)"
##     database.list[[k]]$Item <- factor(nvec)
## }

database.list <- database.list.sav

## file.csv
## attr(database.list[[62]],"symName")
## str(database.list[[62]])
## levels(database.list[[7]]$Purpose)
## summary(database.list[[1]])
## database.list[[25]] <- database.list.sav[[25]]

## too long entries

# Elements
#t.sel <- match("Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data_(Normalized)",file.csv$name)
t.sel <- match("Emissions_crops_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Element)
nvec[nvec=="Indirect emissions (N2O that volatilises) (Synthetic fertilizers)"] <- "Indirect emissions (N2O volatilization) (Synthetic fertilizers)"
database.list[[t.sel]]$Element <- factor(nvec)

# Items
t.sel <- match("Forestry_Trade_Flows_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec[nvec=="Industrial roundwood, non-coniferous non-tropical (export/import)"] <- "Indust. roundwood, non-coniferous non-tropical (export/import)"
database.list[[t.sel]]$Item <- factor(nvec)

t.sel <- match("Forestry_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec[nvec=="Industrial roundwood, non-coniferous non-tropical (export/import)"] <- "Indust. roundwood, non-coniferous non-tropical (export/import)"
nvec[nvec=="Other industrial roundwood, all species (export/import, 1961-1989)"] <- "Other indust. roundwood, all species (export/import, 1961-1989)"
nvec[nvec=="Pulpwood, round and split, all species (export/import, 1961-1989)"] <- "Pulpwood, round, split, all species (export/import, 1961-1989)"
database.list[[t.sel]]$Item <- factor(nvec)

t.sel <- match("Environment_LandCover_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec[nvec=="Shrubs and/or herbaceous vegetation, aquatic or regularly flooded"] <- "Shrubs and/or herbaceous vegetation, aquatic or regular flooded"
nvec[nvec=="Shrubs, herbaceous vegetation, aquatic, regularly flooded"] <- "Shrubs and/or herbaceous vegetation, aquatic or regular flooded"
database.list[[t.sel]]$Item <- factor(nvec)

## For FNS data we use an external file to shorten the labels. This could be extended later even to all the database processing
t.sel <- match("Food_Security_Data_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec.subs <- read.csv(paste0(sourcepath,"FAOSTAT_FNS_Items_replacement.csv"))
for (l in 1:dim(nvec.subs)[1]) nvec[nvec==levels(nvec.subs$raw.name)[nvec.subs$raw.name[l]]] <- levels(nvec.subs$short.name)[nvec.subs$short.name[l]]
database.list[[t.sel]]$Item <- factor(nvec)
## Removing NAs
database.list[[t.sel]]$Value[is.na(database.list[[t.sel]]$Value)] <- 0

t.sel <- match("Investment_GovernmentExpenditure_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec[nvec=="Agriculture, forestry, fishing, Recurrent (Budgetary Central Government)"] <- "Agriculture, forestry, fishing, Recurrent (Budg. Cen. Govt.)"
nvec[nvec=="Agriculture, forestry, fishing, Capital (Budgetary Central Government)"] <- "Agriculture, forestry, fishing, Capital (Budg. Cen. Govt.)"
nvec[nvec=="R&D Agriculture, forestry, fishing (Budgetary Central Government)"] <- "R&D Agriculture, forestry, fishing (Budg. Cen. Govt.)"
database.list[[t.sel]]$Item <- factor(nvec)

t.sel <- match("Inputs_FertilizersProduct_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec[nvec=="Calcium ammonium nitrate (CAN) and other mixtures with calcium carbonate"] <- "Calcium ammonium nitrate (CAN) and oth calcium carbonate mix"
database.list[[t.sel]]$Item <- factor(nvec)

t.sel <- match("Investment_CapitalStock_E_All_Data_(Normalized)",file.csv$name)
nvec <- as.character(database.list[[t.sel]]$Item)
nvec[nvec=="Consumption of Fixed Capital (Agriculture, Forestry and Fishing)"] <- "Consumption of Fixed Capital (Agri, Forest, Fish)"
nvec[nvec=="Gross Fixed Capital Formation (Agriculture, Forestry and Fishing)"] <- "Gross Fixed Capital Formation (Agri, Forest, Fish)"
nvec[nvec=="Consumption of Fixed Capital (Agriculture, Forestry, Fishing)"] <- "Consumption of Fixed Capital (Agri, Forest, Fish)"
nvec[nvec=="Gross Fixed Capital Formation (Agriculture, Forestry, Fishing)"] <- "Gross Fixed Capital Formation (Agri, Forest, Fish)"
database.list[[t.sel]]$Item <- factor(nvec)

# t.sel <- match("Investment_CountryInvestmentStatisticsProfile_E_All_Data_(Normalized)",file.csv$name)
# nvec <- as.character(database.list[[t.sel]]$Item)
# nvec[nvec=="Gross Fixed Capital Formation (Agriculture, Forestry and Fishing)"] <- "Gross Fixed Capital Formation (Agri, Forest, Fish)"
# nvec[nvec=="Gross Fixed Capital Formation as a share of Value Added (Agriculture, Forestry and Fishing)"] <- "Gross Fixed Capital Formation as % VA (Agri, Forest, Fish)"
# nvec[nvec=="Gross Fixed Capital Formation (Agriculture, Forestry, Fishing)"] <- "Gross Fixed Capital Formation (Agri, Forest, Fish)"
# nvec[nvec=="Gross Fixed Capital Formation as a share of Value Added (Agriculture, Forestry, Fishing)"] <- "Gross Fixed Capital Formation as % VA (Agri, Forest, Fish)"
# database.list[[t.sel]]$Item <- factor(nvec)
  
  t.sel <- match("Macro-Statistics_Key_Indicators_E_All_Data_(Normalized)",file.csv$name)
  nvec <- as.character(database.list[[t.sel]]$Item)
  nvec[nvec=="Value Added (Manufacture of food, beverages and tobacco products)"] <- "Value Added (Manufacture food, beverages, tobacco)"
  nvec[nvec=="Value Added (Manufacture of food, beverages, tobacco products)"] <- "Value Added (Manufacture food, beverages, tobacco)"
  database.list[[t.sel]]$Item <- factor(nvec)
  nvec <- as.character(database.list[[t.sel]]$Element)
  nvec[nvec=="Ratio of Value Added (Agriculture, Forestry and Fishing) Local Currency"] <- "Ratio Value Added (Agri, Forestry, Fish) Local Currency"
  nvec[nvec=="Ratio of Value Added (Agriculture, Forestry, Fishing) Local Currency"] <- "Ratio Value Added (Agri, Forestry, Fish) Local Currency"
  database.list[[t.sel]]$Element <- factor(nvec)
  
  t.sel <- match("Employment_Indicators_Agriculture_E_All_Data_(Normalized)",file.csv$name)
  nvec <- as.character(database.list[[t.sel]]$Indicator)
  nvec[nvec=="Share of employees in agriculture, female (% of total female employees)"] <- "Share of employees in agriculture, female (% female employees)"
  nvec[nvec=="Share of employees in agriculture, male (% of total male employees)"] <- "Share of employees in agriculture, male (% male employees)"
  nvec[nvec=="Share of employment in agriculture, female (% of total female employment)"] <- "Share of employment in agriculture, female (% female employmnt)"
  nvec[nvec=="Share of employment in agriculture, male (% of total male employment)"] <- "Share of employment in agriculture, male  (% male employment)"
  nvec[nvec=="Share of female employees in agriculture (% of employees in agriculture)"] <- "Share of female employees in agriculture (% agri employees)"
  nvec[nvec=="Share of female employment in agriculture (% of employment in agriculture)"] <- "Share of female employment in agriculture (% agri employment)"
  database.list[[t.sel]]$Indicator <- factor(nvec)
  
  # Other
  
  t.sel <- match("Development_Assistance_to_Agriculture_E_All_Data_(Normalized)",file.csv$name)
  nvec <- as.character(database.list[[t.sel]]$Purpose)
  nvec[nvec=="Food and Nutrition Assistance, Agriculture and Rural Development"] <- "Food and Nutrition Assistance, Agriculture, Rural Development"
  database.list[[t.sel]]$Purpose <- factor(nvec)
  
  t.sel <- match("Indicators_from_Household_Surveys_E_All_Data_(Normalized)",file.csv$name)
  nvec <- as.character(database.list[[t.sel]]$Breakdown.Variable)
  nvec[nvec=="Gender composition: Both adult males and females in the household"] <- "Gender composition: Both adult males and females in household"
  nvec[nvec=="Education of household head: at least secondary education or other"] <- "Education of household head: at least secondary education or eq"
  nvec[nvec=="Education of adult women in household: No adult woman in the household"] <- "Education women in hh: No adult woman in household"
  nvec[nvec=="Education of adult women in household: At least 1 adult woman with primary education"] <- "Education women in hh: At least 1 adult woman w primary educ"
  nvec[nvec=="Education of adult women in household: At least 1 adult woman with secondary education"] <- "Education women in hh: At least 1 adult woman w secondary educ"
  database.list[[t.sel]]$Breakdown.Variable <- factor(nvec)
  nvec <- as.character(database.list[[t.sel]]$Indicator)
  nvec[nvec=="Share of Dietary Energy Consumption from total carbohydrates and alcohol"] <- "Share of dietary energy from total carbohydrates and alcohol"
  nvec[nvec=="Share of purchased food in total food consumption (in dietary energy)"] <- "Share of purchased food (% total dietary energy)"
  nvec[nvec=="Share of own produced food in total food consumption (in dietary energy)"] <- "Share of own produced food (% total dietary energy)"
  nvec[nvec=="Share of food from other sources in total food consumption (in dietary energy)"] <- "Share of food from other sources (% total dietary energy)"
  nvec[nvec=="Share of food consumed away from home in total food consumption (in dietary energy)"] <- "Share of food consumed away from home (% total dietary energy)"
  database.list[[t.sel]]$Indicator <- factor(nvec)
  
  ## unique(nvec)[nchar(unique(nvec))>63]
  
  ## FAO sets
  
  ## generic column names
  ##names.str <- c("Area.Code","Area","Item.Code","Item","Element.Code","Element","Year.Code","Year","Unit","Flag","Value")
  names.str <- c("Country.Code","Country","Item.Code","Item","Element.Code","Element","Year.Code","Year","Unit","Flag","Value")
  
  ## correcting wrong entries in table names
  for (k in 1:length(database.list)){
      names(database.list[[k]])[match("CountryCode",names(database.list[[k]]))] <- "Country.Code"
      names(database.list[[k]])[match("Area.Code",names(database.list[[k]]))] <- "Country.Code"
      names(database.list[[k]])[match("Area",names(database.list[[k]]))] <- "Country"
      names(database.list[[k]])[match("ItemCode",names(database.list[[k]]))] <- "Item.Code"
      names(database.list[[k]])[match("ElementGroup",names(database.list[[k]]))] <- "Element.Group"
      names(database.list[[k]])[match("ElementCode",names(database.list[[k]]))] <- "Element.Code"
      
      }
  
  ## remaining entries for Element.group
  for (k in 1:length(database.list)){
      if(!is.na(match("Element.Group",names(database.list[[k]])))){
          lab <- attr(database.list[[k]],"symName")
          database.list[[k]] <- database.list[[k]][-(match("Element.Group",names(database.list[[k]])))]
          attr(database.list[[k]],"symName") <- lab
  }}
  
  ## removing entries for Note
  for (k in 1:length(database.list)){
      if(!is.na(match("Note",names(database.list[[k]])))){
        lab <- attr(database.list[[k]],"symName")
        database.list[[k]] <- database.list[[k]][-(match("Note",names(database.list[[k]])))]
        attr(database.list[[k]],"symName") <- lab
      }
      ## jan2024: remove entries for "Area.Code..M49."
      if(!is.na(match("Area.Code..M49.",names(database.list[[k]])))){
        lab <- attr(database.list[[k]],"symName")
        database.list[[k]] <- database.list[[k]][-(match("Area.Code..M49.",names(database.list[[k]])))]
        attr(database.list[[k]],"symName") <- lab
      }
      
      ## jan2024: remove entries for "Item.Code..CPC."
      if(!is.na(match("Item.Code..CPC.",names(database.list[[k]])))){
        lab <- attr(database.list[[k]],"symName")
        database.list[[k]] <- database.list[[k]][-(match("Item.Code..CPC.",names(database.list[[k]])))]
        attr(database.list[[k]],"symName") <- lab
      }  
      
      ## jan2024: remove entries for "Item.Code..FBS."
      if(!is.na(match("Item.Code..FBS.",names(database.list[[k]])))){
        lab <- attr(database.list[[k]],"symName")
        database.list[[k]] <- database.list[[k]][-(match("Item.Code..FBS.",names(database.list[[k]])))]
        attr(database.list[[k]],"symName") <- lab
      }  
    
    ## jan2024: remove entries for "Item.Code..SDG."
    if(!is.na(match("Item.Code..SDG.",names(database.list[[k]])))){
      lab <- attr(database.list[[k]],"symName")
      database.list[[k]] <- database.list[[k]][-(match("Item.Code..SDG.",names(database.list[[k]])))]
      attr(database.list[[k]],"symName") <- lab
    }  
    
      ## jan2024: aggregate monthly values to yearly 
      if(!is.na(match("Months",names(database.list[[k]])))){
        require(tidyr)
        require(dplyr)
        lab <- attr(database.list[[k]],"symName")
        database.list[[k]] <- database.list[[k]] %>% filter(Months=="Annual value") %>% select(!any_of(c("Months","Months.Code")))#group_by(across(-c("Months","Months.Code"))) %>% reframe(Value=mean(Value)) %>% ungroup() %>% data.frame()
        attr(database.list[[k]],"symName") <- lab
      }  
    
    }
  
  ## missing entries for Year.Code
  for (k in 1:length(database.list)){
      if(is.na(match("Year.Code",names(database.list[[k]])))){
          database.list[[k]]$Year.Code <- database.list[[k]]$Year
  }}
  
  ## entries with Note column to remove
  for (k in 1:length(database.list)){
      if(!is.na(match("Note",names(database.list[[k]])))){
          lab <- attr(database.list[[k]],"symName")
          database.list[[k]] <- database.list[[k]][-(match("Note",names(database.list[[k]])))]
          attr(database.list[[k]],"symName") <- lab
  }}
  
  ## entries with Source column to remove
  for (k in 1:length(database.list)){
      if(!is.na(match("Source",names(database.list[[k]])))){
          lab <- attr(database.list[[k]],"symName")
          database.list[[k]] <- database.list[[k]][-(match("Source",names(database.list[[k]])))]
          database.list[[k]] <- database.list[[k]][-(match("Source.Code",names(database.list[[k]])))]
          attr(database.list[[k]],"symName") <- lab
  }}
  
  ## wrongly ordered column names
  for (k in 1:length(database.list)){
      if((paste(names(database.list[[k]])[order(names(database.list[[k]]))],collapse="") == paste(names.str[order(names.str)],collapse="")) &
         (paste(names(database.list[[k]]),collapse="") != paste(names.str,collapse=""))){
          lab <- attr(database.list[[k]],"symName")
          database.list[[k]] <- database.list[[k]][names.str]
          attr(database.list[[k]],"symName") <- lab
  }}
  
  ## listing of databases with different column names
  for (k in 1:length(database.list)){
        if(paste(names(database.list[[k]])[order(names(database.list[[k]]))],collapse="") != paste(names.str[order(names.str)],collapse="")){
            print(attr(database.list[[k]],"symName"))
            print(names(database.list[[k]]))
  }}
  
  ## searching in databases for given entry
  item.search <- "Share of employees in agriculture, female (% of total female employees)"
  for (k in 1:length(database.list)){
        if(!is.na(match(item.search,database.list[[k]]$Item))){
            print("Item:"); print(attr(database.list[[k]],"symName"))
  }}
  for (k in 1:length(database.list)){
    if(!is.na(match(item.search,database.list[[k]]$Element))){
      print("Element:"); print(attr(database.list[[k]],"symName"))
    }}
  for (k in 1:length(database.list)){
    if(!is.na(match(item.search,database.list[[k]]$Breakdown.Variable))){
      print("Breakdown.Variable:"); print(attr(database.list[[k]],"symName"))
    }}
  for (k in 1:length(database.list)){
    if(!is.na(match(item.search,database.list[[k]]$Indicator))){
      print("Indicator:"); print(attr(database.list[[k]],"symName"))
    }}
  
  ## Specific database with different structure from defaut (excluded from main treatment)
  
  file.exclude <- c()
  for (k in 1:length(database.list)){
      if(paste(names(database.list[[k]])[order(names(database.list[[k]]))],collapse="") != paste(names.str[order(names.str)],collapse="")){
          file.exclude <- c(file.exclude,attr(database.list[[k]],"symName"))
  }}
  
  print(file.exclude)
  ## "ConsumerPriceIndices_E_All_Data_(Normalized)"
  ## "Development_Assistance_to_Agriculture_E_All_Data_(Normalized)"
  ## "Employment_Indicators_E_All_Data_(Norm)"
  ## "Environment_Temperature_change_E_All_Data_(Normalized)"
  ## "Exchange_rate_E_All_Data_(Normalized)"
  ## "Food_Aid_Shipments_WFP_E_All_Data_(Normalized)"
  ## "Forestry_Trade_Flows_E_All_Data_(Normalized)"
  ## "Indicators_from_Household_Surveys_E_All_Data_(Normalized)"
  ## "Prices_E_All_Data_(Normalized)"
  
  ## Filters for set and mappings build up
  country.exclude <- c()
  for (k in 1:length(database.list)){
      if(is.na(match("Country.Code",names(database.list[[k]])))){
          country.exclude <- c(country.exclude,attr(database.list[[k]],"symName"))
  }}
  item.exclude <- c()
  for (k in 1:length(database.list)){
      if(is.na(match("Item.Code",names(database.list[[k]])))){
          item.exclude <- c(item.exclude,attr(database.list[[k]],"symName"))
  }}
  element.exclude <- c()
  for (k in 1:length(database.list)){
      if(is.na(match("Element.Code",names(database.list[[k]])))){
          element.exclude <- c(element.exclude,attr(database.list[[k]],"symName"))
  }}
  year.exclude <- c()
  for (k in 1:length(database.list)){
      if(is.na(match("Year.Code",names(database.list[[k]])))){
          year.exclude <- c(year.exclude,attr(database.list[[k]],"symName"))
  }}
  unit.exclude <- c()
  for (k in 1:length(database.list)){
      if(is.na(match("Unit",names(database.list[[k]])))){
          unit.exclude <- c(unit.exclude,attr(database.list[[k]],"symName"))
  }}
  
  t.excl <- match(file.exclude,file.csv$out.name)
  cty.excl <- match(country.exclude,file.csv$out.name)
  item.excl <- match(item.exclude,file.csv$out.name)
  elt.excl <- match(element.exclude,file.csv$out.name)
  yr.excl <- match(year.exclude,file.csv$out.name)
  ##unit.excl ## not needed
  
  length(t.excl)
  
  ## Regions
  region.df <- data.frame()
  n.excl <- cty.excl
  klist <- if (length(n.excl)>0) (1:length(database.list))[-n.excl] else 1:length(database.list)
  for (k in klist){
          regions <- unique(database.list[[k]][c("Country.Code","Country")])
          region.df <- unique(rbind(region.df,regions))
  }
  
  region.df$Country.Code <- as.numeric(levels(region.df$Country.Code)[region.df$Country.Code])
  country.map <- region.df[order(region.df$Country.Code),]
  country.map$Country.Code <- factor(country.map$Country.Code)
  countrycode.set <- country.map["Country.Code"]
  country.set <- as.character(country.map$Country)
  country.set <- data.frame(Country=factor(country.set[order(country.set)]))
  attr(country.map,"symName") <- "A_SETMAP_CountryCode_Country"
  attr(country.set,"symName") <- "A_SET_Country"
  attr(countrycode.set,"symName") <- "A_SET_CountryCode"
  
  ## Items
  item.df <- data.frame()
  n.excl <- item.excl
  klist <- if (length(n.excl)>0) (1:length(database.list))[-n.excl] else 1:length(database.list)
  for (k in klist){
      items <- unique(database.list[[k]][c("Item.Code","Item")])
      item.df <- unique(rbind(item.df,items))
  }
  item.df$Item.Code <- as.character(levels(item.df$Item.Code)[item.df$Item.Code])#as.numeric(levels(item.df$Item.Code)[item.df$Item.Code])
  item.map <- item.df[order(item.df$Item.Code),]
  item.map$Item.Code <- factor(item.map$Item.Code)
  itemcode.set <- item.map["Item.Code"]
  item.set <- as.character(item.map$Item)
  item.set <- data.frame(Item=factor(item.set[order(item.set)]))
  attr(item.map,"symName") <- "A_SETMAP_ItemCode_Item"
  attr(item.set,"symName") <- "A_SET_Item"
  attr(itemcode.set,"symName") <- "A_SET_ItemCode"
  
  ## Checking max number of characters
  levels(item.set$Item)[nchar(levels(item.set$Item))>63]
  
  ## Elements
  element.df <- data.frame()
  n.excl <- elt.excl
  klist <- if (length(n.excl)>0) (1:length(database.list))[-n.excl] else 1:length(database.list)
  
  for (k in klist){
  ##    elements <- unique(database.list[[k]][c("ElementGroup","ElementCode","Element")])
      elements <- unique(database.list[[k]][c("Element.Code","Element")])
      element.df <- unique(rbind(element.df,elements))
      print(k)
      print(elements$Element.Code)
      
  }
  
  element.df$Element.Code <- as.character(levels(element.df$Element.Code)[element.df$Element.Code])#as.numeric(levels(element.df$Element.Code)[element.df$Element.Code])
  element.map <- element.df[order(element.df$Element.Code),]
  element.map$Element.Code <- factor(element.map$Element.Code)
  elementcode.set <- element.map["Element.Code"]
  ## elementgroup.set <- as.numeric(levels(element.map$ElementGroup)[element.map$ElementGroup])
  ## elementgroup.set <- data.frame(ElementGroup=factor(elementgroup.set[order(elementgroup.set)]))
  element.set <- as.character(element.map$Element)
  element.set <- data.frame(Element=factor(element.set[order(element.set)]))
  ##attr(element.map,"symName") <- "A_SETMAP_ElementGroup_ElementCode_Element"
  attr(element.map,"symName") <- "A_SETMAP_ElementCode_Element"
  attr(element.set,"symName") <- "A_SET_Element"
  ##attr(elementgroup.set,"symName") <- "A_SET_ElementGroup"
  attr(elementcode.set,"symName") <- "A_SET_ElementCode"
  
  ## Checking max number of characters
  levels(element.set$Element)[nchar(levels(element.set$Element))>63]
  
  ## Years, Units, Flags
  year.df <- data.frame()
  n.excl <- yr.excl
  klist <- if (length(n.excl)>0) (1:length(database.list))[-n.excl] else 1:length(database.list)
  for (k in klist){
      years <- unique(database.list[[k]][c("Year.Code","Year")])
      year.df <- unique(rbind(year.df,years))
  }
  
  year.df$Year.Code <- as.numeric(levels(year.df$Year.Code)[year.df$Year.Code])
  year.map <- year.df[order(year.df$Year.Code),]
  year.map$Year.Code <- factor(year.map$Year.Code)
  yearcode.set <- year.map["Year.Code"]
  year.set <- as.character(year.map$Year)
  year.set <- data.frame(Year=factor(year.set[order(year.set)]))
  attr(year.set,"symName") <- "A_SET_Year"
  attr(yearcode.set,"symName") <- "A_SET_YearCode"
  attr(year.map,"symName") <- "A_SETMAP_YearCode_Year"
  
  unit.df <- data.frame()
  for (k in 1:length(database.list)){
      units <- unique(database.list[[k]][c("Unit")])
      unit.df <- unique(rbind(unit.df,units))
  }
  unit.df$Unit <- as.character(unit.df$Unit)
  unit.set <- data.frame(Unit=factor(unit.df$Unit[order(unit.df$Unit)]))
  attr(unit.set,"symName") <- "A_SET_Unit"
  
  flag.df <- data.frame()
  for (k in 1:length(database.list)){
      flags <- unique(database.list[[k]][c("Flag")])
      flag.df <- unique(rbind(flag.df,flags))
  }
  
  flag.df$Flag <- as.character(flag.df$Flag)
  flag.set <- data.frame(Flag=factor(flag.df$Flag[order(flag.df$Flag)]))
  attr(flag.set,"symName") <- "A_SET_Flag"
  
  ## Extraction in GDX
  
  ## One extraset to order numbers
  num.set <- data.frame(Num=factor(1:9999))
  attr(num.set,"symName") <- "Z___NumOrder___Z"
  
  sets.list <- c(
                list(data.info.df),
                list(num.set),
                list(country.set,countrycode.set,country.map),
                list(item.set,itemcode.set,item.map),
                list(element.set,elementcode.set,element.map),
                list(year.set,yearcode.set,year.map),
                list(unit.set,flag.set))
  
  str(sets.list)
  
  ## nsel <- c(1:length(database.list))[25]
  ## wgdx.lst("FAO_test.gdx",database.list[nsel],squeeze="n")
  
  ## Testing number of characters: max=63
  # test1 <- data.frame(A=factor(paste(rep("A",63),collapse=""))); attr(test1,"symName") <- "A"
  # test2 <- data.frame(A=paste(rep("A",64),collapse="")); attr(test,"symName") <- "A"
  # test1
  # wgdx.lst("test.gdx",test1,squeeze="n")
  sets.list[[1]][[1]] <- as.factor(sets.list[[1]][[1]])
  
  
  full.list <- c(sets.list,database.list)
  
  for(k in 1:length(full.list)){
    print(attr(full.list[[k]],"symName"))
  }
  ## Running time: 3 min
  wgdx.lst(paste("../FAO_full_",FAO.version,".gdx",sep=""),full.list,squeeze="n")