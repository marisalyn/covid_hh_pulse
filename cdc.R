library(tidyverse)
library(httr)
library(xlsx)

# fcns  ------------------------------------------------------------------------
pull_survey_data <- function(survey_num){
  url_base <- "https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/"
  dest <- "./Downloads"
  num <-  str_pad(survey_num, side = "left", width = 2, pad = "0")
  url <- paste0(url_base, "wk", survey_num, "/HPS_Week", num,"_PUF_CSV.zip")
  td <- tempdir()
  fn <- paste0("pulse2020_puf_", num, ".csv")
  dn <- paste0("pulse2020_data.dictionary_CSV_", num, ".xlsx")
  tf <- tempfile(tmpdir=td, fileext=".zip")
  download.file(url, tf)
  unzip(tf, files=, exdir=td, overwrite=TRUE)
  df <-  read.csv(file.path(td, fn), header=TRUE, row.names=NULL, stringsAsFactors=FALSE)
  return(df)
}
  
clean_survey_data <- function(df){
  df_clean <- df %>% mutate(
    EGENDER = case_when(
      EGENDER == 1 ~ "Male", 
      EGENDER == 2 ~ "Female",
      TRUE ~ as.character(EGENDER)
    ), 
    RHISPANIC = case_when(
      RHISPANIC == 1 ~ "No", 
      RHISPANIC == 2 ~ "Yes",
      TRUE ~ as.character(RHISPANIC)
    ), 
    RRACE = case_when(
      RRACE == 1 ~ "White", 
      RRACE == 2 ~ "Black", 
      RRACE == 3 ~ "Asian", 
      RRACE == 4 ~ "Other",
      TRUE ~ as.character(RRACE)
    ), 
    EEDUC = case_when(
      EEDUC == 1 ~ "Less than high school", 
      EEDUC == 2 ~ "Some high school", 
      EEDUC == 3 ~ "High school graduate or equivalent", 
      EEDUC == 4 ~ "Some college", 
      EEDUC == 5 ~ "Associate's degree", 
      EEDUC == 6 ~ "Bachelor's degree", 
      EEDUC == 7 ~ "Graduate degree",
      TRUE ~ as.character(EEDUC)
    ), 
    MS = case_when(
      MS == 1 ~ "Married", 
      MS == 2 ~ "Widowed", 
      MS == 3 ~ "Divorced", 
      MS == 4 ~ "Separated", 
      MS == 5 ~ "Never married", 
      MS == -99 ~ NA_character_, 
      MS == -88 ~ NA_character_, 
      TRUE ~ as.character(MS)
    ), 
    WRKLOSS = case_when(
      WRKLOSS == 1 ~ "Yes", 
      WRKLOSS == 2 ~ "No", 
      WRKLOSS == -99 ~ NA_character_, 
      WRKLOSS == -88 ~ NA_character_, 
      TRUE ~ as.character(WRKLOSS)
    ), 
    EXPCTLOSS = case_when(
      EXPCTLOSS == 1 ~ "Yes", 
      EXPCTLOSS == 2 ~ "No", 
      EXPCTLOSS == -99 ~ NA_character_, 
      EXPCTLOSS == -88 ~ NA_character_, 
      TRUE ~ as.character(EXPCTLOSS)
    ), 
    ANYWORK = case_when(
      ANYWORK == 1 ~ "Yes", 
      ANYWORK == 2 ~ "No", 
      ANYWORK == -99 ~ NA_character_, 
      ANYWORK == -88 ~ NA_character_, 
      TRUE ~ as.character(ANYWORK)
    ), 
    KINDWORK = case_when(
      KINDWORK == 1   ~ "Government", 
      KINDWORK == 2   ~ "Private compant", 
      KINDWORK == 3   ~ "Non-profit", 
      KINDWORK == 4   ~ "Self-employed", 
      KINDWORK == 5   ~ "Working in a family business", 
      KINDWORK == -99 ~ NA_character_, 
      KINDWORK == -88 ~ NA_character_, 
      TRUE ~ as.character(KINDWORK)
    ), 
    RSNNOWRK = case_when(
      RSNNOWRK == 1   ~ "I did not want to be employed at this time", 
      RSNNOWRK == 2   ~ "I did not work because I am/was sick with coronavirus symptoms", 
      RSNNOWRK == 3   ~ "I did not work because I am/was caring for someone with coronavirus symptoms", 
      RSNNOWRK == 4   ~ "I did not work because I am/was caring for children not in school or daycare", 
      RSNNOWRK == 5   ~ "I did not work because I am/was caring for an elderly person", 
      RSNNOWRK == 6   ~ "I am/was sick (not coronavirus related) or disabled",
      RSNNOWRK == 7   ~ "I am retired",
      RSNNOWRK == 8   ~ "I did not have work due to coronavirus pandemic related reduction in business (including furlough)",
      RSNNOWRK == 9   ~ "I am/was laid off due to coronavirus pandemic",
      RSNNOWRK == 10  ~ "My employment closed temporarily due to the coronavirus pandemic",
      RSNNOWRK == -99 ~ NA_character_, 
      RSNNOWRK == -88 ~ NA_character_, 
      TRUE ~ as.character(RSNNOWRK)
    ), 
    UNEMPPAY = case_when(
      UNEMPPAY == 1 ~ "Yes, I use paid leave", 
      UNEMPPAY == 2 ~ "Yes, I receive full pay but do not have to take leave", 
      UNEMPPAY == 3 ~ "Yes, I receive partial pay",
      UNEMPPAY == 4 ~ "No, I receive no pay", 
      UNEMPPAY == -99 ~ NA_character_, 
      UNEMPPAY == -88 ~ NA_character_, 
      TRUE ~ as.character(UNEMPPAY)
    ), 
    PRIFOODSUF = case_when(
      PRIFOODSUF == 1 ~ "Enough of the kinds of food (I/we) wanted to eat ", 
      PRIFOODSUF == 2 ~ "Enough, but not always the kinds of food (I/we) wanted to eat", 
      PRIFOODSUF == 3 ~ "Sometimes not enough to eat",
      PRIFOODSUF == 4 ~ "Often not enough to eat", 
      PRIFOODSUF == -99 ~ NA_character_, 
      PRIFOODSUF == -88 ~ NA_character_, 
      TRUE ~ as.character(PRIFOODSUF)
    ), 
    CURFOODSUF = case_when(
      CURFOODSUF == 1 ~ "Enough of the kinds of food (I/we) wanted to eat ", 
      CURFOODSUF == 2 ~ "Enough, but not always the kinds of food (I/we) wanted to eat", 
      CURFOODSUF == 3 ~ "Sometimes not enough to eat",
      CURFOODSUF == 4 ~ "Often not enough to eat", 
      CURFOODSUF == -99 ~ NA_character_, 
      CURFOODSUF == -88 ~ NA_character_, 
      TRUE ~ as.character(CURFOODSUF)
    ), 
    FOODSUFRSN2 = case_when(
      FOODSUFRSN2 == 1 ~ "Couldn't get out to buy food",
      FOODSUFRSN2 == -88 ~ NA_character_, 
      FOODSUFRSN2 == -99 ~ NA_character_,
      TRUE ~ as.character(FOODSUFRSN2)
    ), 
    FOODSUFRSN3 = case_when(
      FOODSUFRSN3 == 1 ~ "Afraid to go or didn't want to go out to buy food",
      FOODSUFRSN3 == -88 ~ NA_character_, 
      FOODSUFRSN3 == -99 ~ NA_character_,
      TRUE ~ as.character(FOODSUFRSN3)
    ), 
    FOODSUFRSN4 = case_when(
      FOODSUFRSN4 == 1 ~ "Couldn't get groceries or meals delivered to me",
      FOODSUFRSN4 == -88 ~ NA_character_, 
      FOODSUFRSN4 == -99 ~ NA_character_,
      TRUE ~ as.character(FOODSUFRSN4)
    ), 
    FOODSUFRSN5 = case_when(
      FOODSUFRSN5 == 1 ~ "The stores didn't have the food I wanted",
      FOODSUFRSN5 == -88 ~ NA_character_, 
      FOODSUFRSN5 == -99 ~ NA_character_,
      TRUE ~ as.character(FOODSUFRSN5)
    ), 
    FREEFOOD = case_when(
      FREEFOOD == 1 ~ "Yes",
      FREEFOOD == 2 ~ "No",
      FREEFOOD == -88 ~ NA_character_, 
      FREEFOOD == -99 ~ NA_character_,
      TRUE ~ as.character(FREEFOOD)
    ), 
    WHEREFREE1 = case_when(
      WHEREFREE1 == 1 ~ "Free meals through the school or other programs aimed at children",
      WHEREFREE1 == -88 ~ NA_character_, 
      WHEREFREE1 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE1)
    ), 
    WHEREFREE2 = case_when(
      WHEREFREE2 == 1 ~ "Food pantry or food bank",
      WHEREFREE2 == -88 ~ NA_character_, 
      WHEREFREE2 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE2)
    ), 
    WHEREFREE3 = case_when(
      WHEREFREE3 == 1 ~ "Home-delivered meal service like Meals on Wheels",
      WHEREFREE3 == -88 ~ NA_character_, 
      WHEREFREE3 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE3)
    ), 
    WHEREFREE4 = case_when(
      WHEREFREE4 == 1 ~ "Church, synagogue, temple, mosque or other religious organization",
      WHEREFREE4 == -88 ~ NA_character_, 
      WHEREFREE4 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE4)
    ), 
    WHEREFREE5 = case_when(
      WHEREFREE5 == 1 ~ "Shelter or soup kitchen",
      WHEREFREE5 == -88 ~ NA_character_, 
      WHEREFREE5 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE5)
    ), 
    WHEREFREE6 = case_when(
      WHEREFREE6 == 1 ~ "Other community program",
      WHEREFREE6 == -88 ~ NA_character_, 
      WHEREFREE6 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE6)
    ), 
    WHEREFREE7 = case_when(
      WHEREFREE7 == 1 ~ "Family, friends, or neighbor",
      WHEREFREE7 == -88 ~ NA_character_, 
      WHEREFREE7 == -99 ~ NA_character_,
      TRUE ~ as.character(WHEREFREE7)
    ), 
    TSPNDFOOD = case_when(
      TSPNDFOOD == -88 ~ NA_real_, 
      TSPNDFOOD == -99 ~ NA_real_,
      TRUE ~ as.numeric(TSPNDFOOD)
    ), 
    TSPNDPRPD = case_when(
      TSPNDPRPD == -88 ~ NA_real_, 
      TSPNDPRPD == -99 ~ NA_real_,
      TRUE ~ as.numeric(TSPNDPRPD)
    ), 
    FOODCONF = case_when(
      FOODCONF == 1 ~ "Not at all confident", 
      FOODCONF == 2 ~ "Somewhat confident", 
      FOODCONF == 3 ~ "Moderately confident", 
      FOODCONF == 4 ~ "Very confident", 
      FOODCONF == -88 ~ NA_character_, 
      FOODCONF == -99 ~ NA_character_,
      TRUE ~ as.character(FOODCONF)
    ), 
    HLTHSTATUS = case_when(
      HLTHSTATUS == 1 ~ "Excellent", 
      HLTHSTATUS == 2 ~ "Very good", 
      HLTHSTATUS == 3 ~ "Good", 
      HLTHSTATUS == 4 ~ "Fair", 
      HLTHSTATUS == 5 ~ "Poor", 
      HLTHSTATUS == -88 ~ NA_character_, 
      HLTHSTATUS == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHSTATUS)
    ), 
    ANXIOUS = case_when(
      ANXIOUS == 1 ~ "Not at all", 
      ANXIOUS == 2 ~ "Several days", 
      ANXIOUS == 3 ~ "More than half the days", 
      ANXIOUS == 4 ~ "Nearly every day ", 
      ANXIOUS == -88 ~ NA_character_, 
      ANXIOUS == -99 ~ NA_character_,
      TRUE ~ as.character(ANXIOUS)
    ), 
    WORRY = case_when(
      WORRY == 1 ~ "Not at all", 
      WORRY == 2 ~ "Several days", 
      WORRY == 3 ~ "More than half the days", 
      WORRY == 4 ~ "Nearly every day ", 
      WORRY == -88 ~ NA_character_, 
      WORRY == -99 ~ NA_character_,
      TRUE ~ as.character(WORRY)
    ),
    INTEREST = case_when(
      INTEREST == 1 ~ "Not at all", 
      INTEREST == 2 ~ "Several days", 
      INTEREST == 3 ~ "More than half the days", 
      INTEREST == 4 ~ "Nearly every day ", 
      INTEREST == -88 ~ NA_character_, 
      INTEREST == -99 ~ NA_character_,
      TRUE ~ as.character(INTEREST)
    ),
    DOWN = case_when(
      DOWN == 1 ~ "Not at all", 
      DOWN == 2 ~ "Several days", 
      DOWN == 3 ~ "More than half the days", 
      DOWN == 4 ~ "Nearly every day ", 
      DOWN == -88 ~ NA_character_, 
      DOWN == -99 ~ NA_character_,
      TRUE ~ as.character(DOWN)
    ),
    HLTHINS1 = case_when(
      HLTHINS1 == 1 ~ "Insurance through a current or former employer or union (through yourself or another family member)",
      HLTHINS1 == 2 ~ "No", 
      HLTHINS1 == -88 ~ NA_character_, 
      HLTHINS1 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS1)
    ),
    HLTHINS2 = case_when(
      HLTHINS2 == 1 ~ "Insurance purchased directly from an insurance company, including marketplace coverage (through yourself or another family member)",
      HLTHINS2 == 2 ~ "No", 
      HLTHINS2 == -88 ~ NA_character_, 
      HLTHINS2 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS2)
    ),
    HLTHINS3 = case_when(
      HLTHINS3 == 1 ~ "Medicare",
      HLTHINS3 == 2 ~ "No", 
      HLTHINS3 == -88 ~ NA_character_, 
      HLTHINS3 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS3)
    ),
    HLTHINS4 = case_when(
      HLTHINS4 == 1 ~ "Medicaid",
      HLTHINS4 == 2 ~ "No", 
      HLTHINS4 == -88 ~ NA_character_, 
      HLTHINS4 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS4)
    ),
    HLTHINS5 = case_when(
      HLTHINS5 == 1 ~ "TRICARE or other military health care",
      HLTHINS5 == 2 ~ "No", 
      HLTHINS5 == -88 ~ NA_character_, 
      HLTHINS5 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS5)
    ),
    HLTHINS6 = case_when(
      HLTHINS6 == 1 ~ "VA",
      HLTHINS6 == 2 ~ "No", 
      HLTHINS6 == -88 ~ NA_character_, 
      HLTHINS6 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS6)
    ),
    HLTHINS7 = case_when(
      HLTHINS7 == 1 ~ "Indian Health Service",
      HLTHINS7 == 2 ~ "No", 
      HLTHINS7 == -88 ~ NA_character_, 
      HLTHINS7 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS7)
    ),
    HLTHINS8 = case_when(
      HLTHINS8 == 1 ~ "Other",
      HLTHINS8 == 2 ~ "No", 
      HLTHINS8 == -88 ~ NA_character_, 
      HLTHINS8 == -99 ~ NA_character_,
      TRUE ~ as.character(HLTHINS8)
    ),
    DELAY = case_when(
      DELAY == 1 ~ "Yes",
      DELAY == 2 ~ "No", 
      DELAY == -88 ~ NA_character_, 
      DELAY == -99 ~ NA_character_,
      TRUE ~ as.character(DELAY)
    ),
    NOTGET = case_when(
      NOTGET == 1 ~ "Yes",
      NOTGET == 2 ~ "No", 
      NOTGET == -88 ~ NA_character_, 
      NOTGET == -99 ~ NA_character_,
      TRUE ~ as.character(NOTGET)
    ),
    TENURE = case_when(
      TENURE == 1 ~ "Owned",
      TENURE == 2 ~ "Owned with a mortgage or loan", 
      TENURE == 3 ~ "Rented", 
      TENURE == 4 ~ "Occupied without payment of rent", 
      TENURE == -88 ~ NA_character_, 
      TENURE == -99 ~ NA_character_,
      TRUE ~ as.character(TENURE)
    ),
    MORTLMTH = case_when(
      MORTLMTH == 1 ~ "Yes",
      MORTLMTH == 2 ~ "No", 
      MORTLMTH == 3 ~ "Payment was deferred", 
      MORTLMTH == -88 ~ NA_character_, 
      MORTLMTH == -99 ~ NA_character_,
      TRUE ~ as.character(MORTLMTH)
    ),
    MORTCONF = case_when(
      MORTCONF == 1 ~ "No confidence",
      MORTCONF == 2 ~ "Slight confidence", 
      MORTCONF == 3 ~ "Moderate confidence", 
      MORTCONF == 4 ~ "High confidence", 
      MORTCONF == 5 ~ "Payment is/will be deferred", 
      MORTCONF == -88 ~ NA_character_, 
      MORTCONF == -99 ~ NA_character_,
      TRUE ~ as.character(MORTCONF)
    ),
    ENROLL1 = case_when(
      ENROLL1 == 1 ~ "Yes, enrolled in a public or private school",
      ENROLL1 == 2 ~ "No", 
      ENROLL1 == -88 ~ NA_character_, 
      ENROLL1 == -99 ~ NA_character_,
      TRUE ~ as.character(ENROLL1)
    ),
    ENROLL2 = case_when(
      ENROLL2 == 1 ~ "Yes, homeschooled",
      ENROLL2 == 2 ~ "No", 
      ENROLL2 == -88 ~ NA_character_, 
      ENROLL2 == -99 ~ NA_character_,
      TRUE ~ as.character(ENROLL2)
    ),
    ENROLL3 = case_when(
      ENROLL3 == 1   ~ "No", 
      ENROLL3 == -88 ~ NA_character_, 
      ENROLL3 == -99 ~ NA_character_,
      TRUE ~ as.character(ENROLL3)
    ),
    TEACH1 = case_when(
      TEACH1 == 1 ~ "Classes normally taught in person at the school were cancelled",
      TEACH1 == -88 ~ NA_character_, 
      TEACH1 == -99 ~ NA_character_,
      TRUE ~ as.character(TEACH1)
    ),
    TEACH2 = case_when(
      TEACH2 == 1 ~ "Classes normally taught in person moved to a distance-learning format using online resources, either self-paced or in real time",
      TEACH2 == -88 ~ NA_character_, 
      TEACH2 == -99 ~ NA_character_,
      TRUE ~ as.character(TEACH2)
    ),
    TEACH3 = case_when(
      TEACH3 == 1 ~ "Classes normally taught in person moved to a distance-learning format using paper materials sent home to children",
      TEACH3 == -88 ~ NA_character_, 
      TEACH3 == -99 ~ NA_character_,
      TRUE ~ as.character(TEACH3)
    ),
    TEACH4 = case_when(
      TEACH4 == 1 ~ "Classes normally taught in person changed in some other way",
      TEACH4 == -88 ~ NA_character_, 
      TEACH4 == -99 ~ NA_character_,
      TRUE ~ as.character(TEACH4)
    ),
    TEACH5 = case_when(
      TEACH5 == 1 ~ "There was no change because schools did not close",
      TEACH5 == -88 ~ NA_character_, 
      TEACH5 == -99 ~ NA_character_,
      TRUE ~ as.character(TEACH5)
    ),
    COMPAVAIL = case_when(
      COMPAVAIL == 1 ~ "Always available",
      COMPAVAIL == 2 ~ "Usually available", 
      COMPAVAIL == 3 ~ "Sometimes available",
      COMPAVAIL == 4 ~ "Rarely available", 
      COMPAVAIL == 5 ~ "Never available",
      COMPAVAIL == -88 ~ NA_character_, 
      COMPAVAIL == -99 ~ NA_character_,
      TRUE ~ as.character(COMPAVAIL)
    ),
    COMP1 = case_when(
      COMP1 == 1 ~ "Provided by the children's school or school district to use outside of school",
      COMP1 == -88 ~ NA_character_, 
      COMP1 == -99 ~ NA_character_,
      TRUE ~ as.character(COMP1)
    ),
    COMP2 = case_when(
      COMP2 == 1 ~ "Provided by someone in the household or family, or it is the child's",
      COMP2 == -88 ~ NA_character_, 
      COMP2 == -99 ~ NA_character_,
      TRUE ~ as.character(COMP2)
    ),
    COMP3 = case_when(
      COMP3 == 1 ~ "Provided by another source",
      COMP3 == -88 ~ NA_character_, 
      COMP3 == -99 ~ NA_character_,
      TRUE ~ as.character(COMP3)
    ),
    INTRNTAVAIL = case_when(
      INTRNTAVAIL == 1 ~ "Always available",
      INTRNTAVAIL == 2 ~ "Usually available", 
      INTRNTAVAIL == 3 ~ "Sometimes available",
      INTRNTAVAIL == 4 ~ "Rarely available", 
      INTRNTAVAIL == 5 ~ "Never available",
      INTRNTAVAIL == -88 ~ NA_character_, 
      INTRNTAVAIL == -99 ~ NA_character_,
      TRUE ~ as.character(INTRNTAVAIL)
    ),
    INTRNT1 = case_when(
      INTRNT1 == 1 ~ "Paid for by the children's school or school district",
      INTRNT1 == -88 ~ NA_character_, 
      INTRNT1 == -99 ~ NA_character_,
      TRUE ~ as.character(INTRNT1)
    ),
    INTRNT2 = case_when(
      INTRNT2 == 1 ~ "Paid for by someone in the household or family",
      INTRNT2 == -88 ~ NA_character_, 
      INTRNT2 == -99 ~ NA_character_,
      TRUE ~ as.character(INTRNT2)
    ),
    INTRNT3 = case_when(
      INTRNT3 == 1 ~ "Paid for by another source",
      INTRNT3 == -88 ~ NA_character_, 
      INTRNT3 == -99 ~ NA_character_,
      TRUE ~ as.character(INTRNT3)
    ),
    INCOME = case_when(
      INCOME == 1 ~ "Less than $25,000",
      INCOME == 2 ~ "$25,000 - $34,999",
      INCOME == 3 ~ "$35,000 - $49,999",
      INCOME == 4 ~ "$50,000 - $74,999",
      INCOME == 5 ~ "$75,000 - $99,999",
      INCOME == 6 ~ "$100,000 - $149,999",
      INCOME == 7 ~ "$150,000 - $199,999",
      INCOME == 8 ~ "$200,000 and above",
      INCOME == -88 ~ NA_character_, 
      INCOME == -99 ~ NA_character_,
      TRUE ~ as.character(INCOME)
    ), 
    TTCH_HRS = case_when(
      TTCH_HRS == -88 ~ NA_real_, 
      TTCH_HRS == -99 ~ NA_real_,
      TRUE ~ as.numeric(TTCH_HRS)
    ), 
    TSCHLHRS = case_when(
      TSCHLHRS == -88 ~ NA_real_, 
      TSCHLHRS == -99 ~ NA_real_,
      TRUE 
      ~ as.numeric(TSCHLHRS)
    ) 
  ) %>% 
  mutate_at(vars(one_of("TSTDY_HRS")),  ~case_when(
    TSTDY_HRS == -88 ~ NA_real_, 
    TSTDY_HRS == -99 ~ NA_real_,
    TRUE  ~ as.numeric(TSTDY_HRS)))
              
  
  states <- read.csv("states.csv") 
  df_final <- left_join(df_clean, states, by = c("EST_ST" = "num"))
  
  return(df_final)
}

# logic -----------------------------------------------------------------------
n <- seq(1, 12, 1)
surveys_raw <- map(n, ~pull_survey_data(.))
surveys_clean <- map(surveys_raw, ~clean_survey_data(.))
surveys_all <- bind_rows(surveys_clean)

# checks -----------------------------------------------------------------------
map(surveys_all, ~unique(.))

# summarize --------------------------------------------------------------------
missing <- surveys_all %>% 
  group_by(STATE, WEEK) %>%
  summarise_all(~sum(is.na(.)))

