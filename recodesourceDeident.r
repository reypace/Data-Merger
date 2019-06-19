if(require(rstudioapi)==FALSE){ install.packages('rstudioapi')}
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('Source.r')



#STEP 1: identify files to use
############### Import raw data
############### 
dat_input <- '../../Schools/School Data/XX-example/Raw/'
dat_output <- '../../Schools/School Data/XX-example/Clean/'

lookup= read.table(paste0(file.path(dat_input,'codebook.csv')),row.names=NULL,header=TRUE,sep=',')

demo_raw <- read_csv(file=paste0(dat_input,'.csv'))
cbm_raw <- read_csv(file=paste0(dat_input,'.csv'))


#Step 2: Identify columns to pull (helps prevent mis-pulls)
KIDS=demo_raw$'KIDs ID'
SCHOOLN=demo_raw$"School Name"
LASTN=str_trim(demo_raw$"Last Name",side=c('both'))
FIRSTN=str_trim(demo_raw$'First Name',side=c('both'))
GENDER=demo_raw$'Gender'
DOB=demo_raw$'Birthdate'
GRADE=demo_raw$Grade
ETHN=demo_raw$Ethnicity
ELL=demo_raw$'ESOL/Biling.'
ATTRATE=demo_raw$'% Attendance'
RACE=demo_raw$'Comp Race'
LUNCH=demo_raw$'F/R Lunch'
IEP=demo_raw$'IEP'
Q504=demo_raw$'504' 

demo=cbind.data.frame(KIDS,SCHOOLN,LASTN,FIRSTN,
                      GENDER,DOB,GRADE,ETHN,ELL,
                     ATTRATE,RACE,LUNCH,IEP,Q504,stringsAsFactors = FALSE)
       


#Step 3: Recode columns using codebook

#Gender
#first we check for any values that aren't in codebook expectations
x=demo$GENDER
xaccepted=subset(lookup[,2],lookup[,1]=='gender') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

#now we can simply test to see if there are any datapoints not in the code book. 
#the number of elements in xunique should equal the number of elements in xposttest

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}


xnew=x
lookupspec=subset(lookup,lookup[,1]=='gender')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$GENDER_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies

#Grade
x=demo$GRADE
xaccepted=subset(lookup[,2],lookup[,1]=='grade') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='grade')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$GRADE_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies


#Race
x=demo$RACE
xaccepted=subset(lookup[,2],lookup[,1]=='race') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='race')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$RACE_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies


#Ethnicity
x=demo$ETHN
xaccepted=subset(lookup[,2],lookup[,1]=='ethnicity') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='ethnicity')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$ETHN_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies


#Lunch
x=demo$LUNCH
xaccepted=subset(lookup[,2],lookup[,1]=='lunch') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='lunch')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$LUNCH_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies

#504
x=demo$Q504
xaccepted=subset(lookup[,2],lookup[,1]=='504') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='504')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$Q504_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies

#IEP
x=demo$IEP
xaccepted=subset(lookup[,2],lookup[,1]=='disability') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='disability')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
demo$IEP_r=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies



#Split DOB
demo$DOB <- as.character(demo$DOB)
temp <- strsplit(demo$DOB,"/")
temp2 <- matrix(unlist(temp),ncol=3,byrow=T)
temp3 <- as.data.frame(temp2)
temp3$DOB_r <- as.Date(paste0(temp3$V3,'-',temp3$V1,'-',temp3$V2))


demo$ATTEND <- NA
demo$DAYS <- NA


demo$ATTRATE_r <- round(demo$ATTRATE/100, digits=2)

#Bind with original data set
demo2 <- cbind.data.frame(demo,temp3)

###Step 4: Checks
demo2$DUP <- duplicated(demo2)
demo2$XYEAR <- 2019


#Step 5 Pull data into final version

demo_fnl <- filter(demo2,DUP==FALSE) %>%
  select(XYEAR,SCHOOLN,KIDS,LASTN,FIRSTN,
         DOB_r,GENDER_r,RACE_r,ETHN_r,GRADE_r,LUNCH_r,ELL,Q504,IEP_r,
         ATTEND,DAYS,ATTRATE_r) 

colnames(demo_fnl) <- c('XYEAR','SCHOOLN','KIDS','LASTN','FIRSTN',
                        'DOB','GENDER','RACE','ETHN','GRADE','LUNCH','ELL','Q504','IEP',
                        'ATTEND','DAYS','ATTRATE')

######----------------------------------------------
#####          CBM clean
######----------------------------------------------

########### CBM Clean 
########### 
#wide to long for CBM type
temp=gather(cbm_raw,'XCBMM','XCBMSCORE','LNF':'R-CBM',na.rm=T)

LASTN=temp$'Last Name'
FIRSTN=temp$'First Name'
GRADE_r=temp$'Grade'
XCBMM=temp$XCBMM
XCBMSCORE=temp$XCBMSCORE
SCHOOLN=rep('Example Elementary',nrow(temp))

XYEAR=rep('2019',nrow(temp))
XWAVE=rep('SPRING2019',nrow(temp))
XCBMTYPE=rep(2,nrow(temp))

### Recode Grade
x=GRADE_r
xaccepted=subset(lookup[,2],lookup[,1]=='grade') #we're only saving input values here, so just second col
xunique=x[!duplicated(x)]

xposttest=xunique[xunique%in%xaccepted]
if(length(xposttest)!=length(xunique)){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

xnew=x
lookupspec=subset(lookup,lookup[,1]=='grade')
xnew=lookupspec$output[match(unlist(x),lookupspec$input)]
GRADE=xnew

#freq check
a=as.data.frame(table(x))
b=as.data.frame(table(xnew))
a2=a[order(match(a[,1],lookupspec[,2])),]    #here order and match are putting rows in order of lookup. #match returns the row numbers where [a,b] match eachother, then order puts it in that order
b2=b[order(match(b[,1],lookupspec[,3])),]  
sum(abs(a2[,2]-b2[,2]))  #if this is anything other than 0 then we have a mismatch of frequencies


cbm_fnl1=cbind.data.frame(XYEAR,SCHOOLN,LASTN,FIRSTN,XWAVE,GRADE,XCBMTYPE,XCBMM,XCBMSCORE,stringsAsFactors = FALSE)

##########============================= 
            # GETTING PREVIOUS CBM DATA (FALL AND WINTER - USING PREVIOUS CODING)
cbm3 <- read_csv(file.path(dat_input,'.csv'))


########### CBM Clean 
########### 


####---------------------------


# Merging FALL/Winter and Spring cbm data

colnames(cbm3)
cbm_fnl2 <- select(cbm3,XYEAR,SCHOOLN,LASTN,FIRSTN,XWAVE,GRADE,XCBMTYPE,XCBMM,XCBMSCORE)


#confirm col's of cbm_fnl and cbm_fnl2 are the same
test=colnames(cbm_fnl1)[colnames(cbm_fnl1)%in%colnames(cbm_fnl2)]
if(length(test)!=length(colnames(cbm_fnl2))){cat(red(paste('WARNING: Input data exceeds codebook entries')))}

cbm_fnl1$XCBMSCORE=as.numeric(as.character(cbm_fnl1$XCBMSCORE))   #note this will coerce the 'n/a' to be NA, which throws a warning, but is desired
cbm_fnl1.1=subset(cbm_fnl1,!is.na(cbm_fnl1$XCBMSCORE)) #removing rows where students had a score of na

cbm_fnl2$XCBMSCORE=as.numeric(as.character(cbm_fnl2$XCBMSCORE))
cbm_fnl1.1$XYEAR=as.numeric(as.character(cbm_fnl1.1$XYEAR))

cbm_fnl=dplyr::bind_rows(cbm_fnl1.1,cbm_fnl2)



###------------------------------------------
### Merge Demo and CBM
##-------------------------------------------
####################### MERGE: CBM and DEMO
#######################
cbm_fnl$SCHOOLN <- 'Example Elementary'
demo_fnl$SCHOOLN <- 'Example Elementary'
colnames(demo_fnl)
colnames(cbm_fnl)

cbm_fnlrec=cbm_fnl


colnames(cbm_fnlrec)
cbm_fnl=select(cbm_fnlrec,XYEAR,SCHOOLN,LASTN,FIRSTN,XWAVE,GRADE,XCBMTYPE,XCBMM,XCBMSCORE)

## merge data sets
cbm_fnl$XYEAR=as.numeric(as.character(cbm_fnl$XYEAR))

comp <- full_join(demo_fnl,cbm_fnl,by=c('XYEAR','SCHOOLN','LASTN','FIRSTN','GRADE'))


## Check for mismatch 
## look for cases where names might be mismatch
## beyond K-3 no demographics
check <- unique(select(comp,KIDS,LASTN,FIRSTN,GRADE)) 
check2=subset(check,is.na(check$KIDS))

#okay, I want to only see those kids who are in check2 AND who also have an entry without an NA

check3=subset(check,!is.na(check$KIDS))

#list of students who have last names in check2 AND check3
list=check2$LASTN[check2$LASTN%in%check3$LASTN]    #so, if the entry in check2 doesn't appear in check3, it will be false

#now I get the students from check who are on the list
check4=check[check$LASTN%in%list ,]   #just sort by last name, and look only at those with NA

### Recode/make variables
comp$XSCHOOLID <- recode_school(comp$SCHOOLN)
as.data.frame(table(comp$SCHOOLN))
as.data.frame(table(comp$XSCHOOLID))

### Export final 
colnames(comp)

comp_fnl <- select(comp,XYEAR,XSCHOOLID,SCHOOLN,KIDS,LASTN,FIRSTN,
                   DOB,GENDER,RACE,ETHN,GRADE,LUNCH,ELL,Q504,IEP,
                   ATTEND,DAYS,ATTRATE,
                   XWAVE,XCBMTYPE,XCBMM,XCBMSCORE)


write_csv(comp_fnl,file.path(dat_output,'Example_2018-19.csv'))


