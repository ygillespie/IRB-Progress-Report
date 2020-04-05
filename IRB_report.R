---
  title: "IRB Progress Report"
author: "Yasmine .G"
date: "Febuary 11, 2020"
output:
  html_document:
  highlight: tango
theme: spacelab
toc: yes
toc_float: yes
---
  
```{r setup, include=FALSE, warning = FALSE}
library(knitr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)
library(keyringr, warn.conflicts = F, quietly = T)
library(redcapAPI, warn.conflicts = F, quietly = T)
library(REDCapR, warn.conflicts = F, quietly = T)
library(lubridate, warn.conflicts = F, quietly = T)
#Create path to and api link to RedCaps
credential_label <- "beach_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
```

```{r variable selection, include=FALSE}
# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)
# variables
desired_fields=c("test_id","redcap_event_name",
                 "beach_part_drop_out",
                 "beachphone_pass_fail",
                 "beach_study_complete")
# participants
participant.records.v1=c("BLS001A",
                         "BLS002A",
                         "BLS003A",
                         "BLS007A",
                         "BLS008A",
                         "BLS011A",
                         "BLS012A",
                         "BLS013A",
                         "BLS014A",
                         "BLS016A",
                         "BLS019A",
                         "BLS020A",
                         "BLS023A",
                         "BLS025A",
                         "BLS027A",
                         "BLS028A",
                         "BLS030A",
                         "BLS032A",
                         "BLS033A",
                         "BLS034A",
                         "BLS036A",
                         "BLS038A",
                         "BLS040A")
# pull data
ds <- data.frame(desired_fields)
dat<- redcap_read(
  batch_size=300,
  records= participant.records.v1,
  redcap_uri = uri, 
  token      = beach_token, 
  fields     = desired_fields
)$data
head(dat)
```

```{r variable selection, include=FALSE}
dat.f=dat %>%
  select(-redcap_event_name,-redcap_repeat_instrument,-redcap_repeat_instance) %>%
  mutate(beach_part_drop_out = recode(beach_part_drop_out, 
                                      "1"="dropped","2"="not-dropped"),
         beach_study_complete    = recode(beach_study_complete, 
                                 "1"="Yes","2"="No"),
         beachphone_pass_fail    = recode(beachphone_pass_fail,
                                          "1"="Pass","2"="Fail"))%>%
  
  distinct()
```


```{r , warning=FALSE, include=FALSE}
Drop_out <- dat.f%>%
  select(test_id,beach_part_drop_out)%>%
  group_by(test_id,beach_part_drop_out)%>%
  filter(beach_part_drop_out == "dropped")
  summarise(Drop_out)
```


```{r , warning=FALSE, include=FALSE}
Active <- dat.f%>%
  select(test_id,beach_study_complete)%>%
  group_by(test_id,beach_study_complete)%>%
  filter(beach_study_complete == 2 )
  summarise(Active)
```

```{r , warning=FALSE, include=FALSE}
Passed <- dat.f%>%
  select(test_id,beachphone_pass_fail)%>%
  group_by(test_id,beachphone_pass_fail)%>%
  filter(beachphone_pass_fail == "Pass")
  summarise(Passed)
```


```{r , warning=FALSE, include=FALSE}
#An idea for the table 
#Change letters to numbers and numbers to test id
dat.f <- data.frame(Passed = sample(letters[1:3], 23, replace = TRUE), 
                   Acitve= sample(letters[1:3], 23, replace = TRUE),
                   Drop_out= sample(letters[1:3], 23, replace = TRUE))

# have a look at the data set
print.data.frame(dat.f)

```
