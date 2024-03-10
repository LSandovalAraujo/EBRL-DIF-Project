#Multidimensional Item Response Theory (MIRT) Models 
######################################
###Statsguidetree YouTube Tutorial###
#####################################

#Confirmatory MIRT Models 
#DIF with Anchors

install.packages("mirt")
library(mirt)

#Multidimensional IRT Models (i.e., more than 1 factor) are generalizations
#Traditional unidimensional IRT models (e.g., 1PL, 2PL, 3PL, GRM, etc.)
#Can each be generalized to estimate additional dimensions/factors 

#MIRT - Confirmatory (i.e., # factors predetermined) or exploratory (i.e., not known)

setwd("C:/Users/Luis/Dropbox/independent stuff/Year 1/Sam MIRT")

#Load dataset
#Clinical Impairment Scale (CIA; Bohn et al., 2008; Bohn & Fairburn, 2008; Kambanis MTurk Dataset)
#Load dataset downloaded 16 items with scale containing 4 levels 
# (0=Not at all, 1=A little, 2=Quite a bit, 3=A lot)
CIA <-read.csv("Mturk_sexatbirthCIA_M1F2_noNAcases.csv")

#Subset - sex assigned at birth(1=Male, 2=Female)
CIA <-subset (CIA, (SAB==1 | SAB==2), select = c(CIA1:CIA16, SAB))

#Set Missing Values to NA 
#Already done; ignore in this case
#CIA[CIA==()]<- NA 

#Remove all rows (i.e., cases) with missing values 
CIA <-na.omit(CIA)

#Note: 
#Can set specific IRT model by adding itemtype and stating specific model (i.e., 2PL/Graded)
#Dichot data should be 1/0, polytomous data 1 to k (k=# categories), 
#missing - NA 

#########################
#CONFIRMATORY - FOR CIA#
#########################

#First identify relationship between factors and which items load to which factor

#Specify confirmatory model. Can also do this using 
# mirt.model() function 

#let's use CIA 3-factor structure as example:
#F1 = Personal impairment 
#F2 = Social impairment 
#F3 = Cognitive impairment

fstruct3 <- 'F1 = CIA2,CIA8,CIA9,CIA11,CIA14,CIA16
F2 = CIA3,CIA7,CIA10,CIA12,CIA15
F3 = CIA1,CIA4,CIA5,CIA6,CIA13'

#Note (CIA)
#Q4 optional - if left blank, missing value imputed by calculating average of 
#total score or cognitive impairment subscale (two scores dependent on value for #4) & 
#this value added to provide total & subscale total
#we didn't include missing data here, but important for interpretation 
#coefficients for item parameters a's -slopes & d's - intercept/THRESHOLD parameters (K-1); want to see 
  #progressively decreasing values on intercept parameters 

mod1 <-mirt(CIA[,1:16], fstruct3)

coef(mod1)

summary(mod1)

##################
#####DIF TIME#####
##################

#Observed DIF between groups could be due to bias - if 
  #same standing on construct (theta), should expect similar responses to each item
  #need to describe WHY DIF is detected. Unidimensional DIF tells us that there is DIF - but need
  #to account for differences between groups due to different standing on subscales/factors - multidimensional
  #DIF can account for that. Might reasonably expect unidimensional IRT (LORDIF) to suggest more DIF items that 
  #are actually due to different standings on CIA subscales rather than bias

#set field to categorical 
CIA$SAB2<-ifelse(CIA$SAB==2, "F","M")

#########################################################
###############IMPORTANT NOTE############################
#In this example, first 3 items are set as the anchor items. 
#THIS IS VERY IMPORTANT STEP - ANCHOR ITEMS SHOULD NOT BE DIF/ BIAS ITEMS##

#To detect DIF the multigroup() function is used to conduct 
#IRT multi group analysis

#Identification of anchor items allows for the baseline to be established & subsequent DIF detection. 

#without setting anchor items DIF() function detection may also pick up differences between latent factors!!!

#anchor items are used to identify if a baseline model and 
#distribution properties of the latent factors 
#are set to be freely estimated because we do not generally expect distribution properties of factors 
#to be identical 

#Anchor selection strategies - READ THIS TO FIGURE OUT HOW TO SELECT FOR CIA
#Can have content expert provide anchor set, but several other methods
#Kopf, Zeileis, & Strobl (2015)

#THIS IS EXAMPLE - SET ANCHOR ITEMS USING METHOD ABOVE BEFORE ACTUALLY RUNNING THIS
#Pulled potentially DIF-Free items from LORDIF output - items not flagged for DIF... 
  #Anchor items from each factor in this example (not sure what to do if not)
    #Personal - 2, 16, - if needed try 11(lordif models1-2), 8 (lordif models 1-2)
    #Social - 7, 10, 12
    #Cognitive - 1

mg<-multipleGroup(CIA[,1:16],fstruct3,
               itemtype = "graded", 
               group=CIA$SAB2, 
               invariance = c("CIA1", "CIA2", "CIA7", "CIA10", "CIA12", "CIA16",
                              "free_means", "free_var"), SE = TRUE)



#CHATGPT GENERATING IDEAS FOR WARNINGS:
is.na(CIA[,1:16])
sapply(CIA[,1:16], class)
sapply(CIA[, 1:16], function(x) unique(x, incomparables = NA))

#Recommended setting equality constraints where supported by theory, trying here (the following 
#is pretty willy-nilly but wanted to document what I tried & impact on error messages): 

mg <- multipleGroup(
  CIA[, 1:16],
  fstruct3,
  itemtype = "graded",
  group = CIA$SAB2,
  invariance = c(
    "CIA2=CIA16",    # Constraint for F1 factor loading equality (.85,.87)
    "CIA7=CIA12", "CIA10",  # Constraint for F2 factor loading equality (.85,.87)
    "CIA1",
    "free_means", "free_var"
  ),
  SE = TRUE)

mg <- multipleGroup(
  CIA[, 1:16],
  fstruct3,
  itemtype = "graded",
  group = CIA$SAB2,
  invariance = c(
    "Q2 = Q16",      # Constraint for F1 factor loading equality
    "Q7 = Q12 = Q10",      # Constraint for F2 factor loading equality
    "Q1",              # Constraint for F3 factor loading equality
    "free_means", "free_var"),
  SE = TRUE)

multipleGroup(
  data,
  model = 1,
  group,
  itemtype = NULL,
  invariance = "",
  method = "EM",
  dentype = "Gaussian",
  ...
)

#BELOW IS WHERE CODE RESUMES, HAVE NOT GOTTEN PAST MULTIPLE GROUP FUNCTION ABOVE

#Next the DIF() function is used to detect DIF across slope & intercept parameters

#Item parameters freely estimated across groups (e.g.,male vs. female sex at birth)

#Equality constraint is placed on parameters
#Indicated for each of the items one by one, which will be 2,8,9 based on this example
#if model worse under equality constraints placed, then DIF is detected
#In this example, likelihood ratio approach used but can also use Wald if Wald=TRUE is added 
#Other DIF schemes available with function 
#BH is Benj-Hosh correction

#This takes a while to run, so don't freak out

dif <-DIF(mg, c('a1', 'a2', 'd1', 'd2', 'd3'),
          p.adjust = 'BH', 
          items2test=c(2,8:9), 
          seq_stat= .01)
dif

#See if anchor items are flagged for DIF (not what you want)
#look at adjusted p-values ideally 

#Can add intercept and slope parameters into DIF function to test non-uniform DIF

#General visualization by including multipleGroup() in plot() function 
#plot(multigroup())

#Produce general plot for specific items: 
#e.g., item1 - itemplot(multipleGroup(),1)