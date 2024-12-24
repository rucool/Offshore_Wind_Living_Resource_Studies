---
title: "RUCOOL Pulse Check 2024"
header-includes:
- \usepackage{titling}
- \pretitle{\begin{right}\LARGE\includegraphics[width=5cm]{~/Downloads/RNBCOOLSEBS/PNG/RNBCOOLSEBS_H_RED_BLACK_RGB.png}\\[\bigskipamount]}
- \posttitle{\end{center}}
output: 
  pdf_document:
    #toc: true
---
<style type="text/css">
  body{
  font-size: 14pt;
}
</style>

```{r setup, include=F, echo=F}
require(dplyr)
require(readxl)
require(ggplot2)
require(tidyverse)

# import data
pc <- read_excel("Downloads/2024_Pulse_Check_1734730134.xlsx",skip=2)
names(pc) = c("id","type","seniority","roles","ownership","communication",
              "appreciated","skills","unity","hurdles","hurdles_open",
              "who_i_manage","who_manages_me","well_organized",
              "project_dissemination","balance","goals","accomp","files","comm",
              "monday","open")

# remove summary
summary_stats = pc[length(pc$ownership),]
pc = pc[1:(length(pc$ownership)-1),]

# remove duplicate, Scott answered twice f1 and f7, use second scores and combine comments

# add unique id #
pc$id = 1:length(pc$ownership)

# averages
pc = pc %>% mutate(roles = as.numeric(roles), 
         ownership = as.numeric(ownership), 
         communication = as.numeric(communication), 
         appreciated = as.numeric(appreciated),
         skills = as.numeric(skills),
         unity = as.numeric(unity),
         well_organized = as.numeric(well_organized)) 
         
pc_sum = pc %>% 
  summarise(faculty = length(type[type %in% "Faculty"]),
            staff = length(type[type %in% "Staff"]), 
            newer = length(type[type %in% "0-4 years"]),
            older = length(type[type %in% "5+ years"]),
            av.roles = mean(roles, na.rm=TRUE), 
            sd.roles = sd(roles, na.rm=TRUE), 
            av.ownership = mean(ownership, na.rm=TRUE), 
            sd.ownership = sd(ownership, na.rm=TRUE), 
            av.communication = mean(communication, na.rm=TRUE), 
            sd.communication = sd(communication, na.rm=TRUE), 
            av.appreciated = mean(appreciated, na.rm=TRUE),
            sd.appreciated = sd(appreciated, na.rm=TRUE),
            av.skills = mean(skills, na.rm=TRUE),
            sd.skills = sd(skills, na.rm=TRUE),
            av.well_organized = mean(well_organized, na.rm=TRUE),
            sd.well_organized = sd(well_organized, na.rm=TRUE),
            av.unity = mean(unity, na.rm=TRUE),
            sd.unity = sd(unity, na.rm=TRUE))

# comm_pref
cp.sum = as.data.frame(unlist(strsplit(pc$comm, ","))) 
names(cp.sum) = c("comm")
cp.sum = cp.sum %>% 
  filter(!is.na(comm)) %>%
  mutate(comm= trimws(comm)) %>%
  group_by(comm) %>% 
  summarize(n=n()) %>% 
  mutate(n=as.numeric(n)) %>%
  arrange(n)

# file management pref
files.sum = as.data.frame(unlist(strsplit(pc$files, ","))) 
names(files.sum) = c("files")
files.sum = files.sum %>% 
  mutate(files = trimws(files)) %>%
  group_by(files) %>% 
  summarize(n=n()) %>% 
  mutate(n=as.numeric(n)) %>%
  arrange(n)

# hurdles
h.sum = as.data.frame(unlist(strsplit(pc$hurdles, ","))) 
names(h.sum) = c("hurdles")
h.sum = h.sum %>% 
  mutate(hurdles= trimws(hurdles)) %>%
  group_by(hurdles) %>% 
  summarize(n=n()) %>% 
  mutate(n=as.numeric(n)) %>%
  arrange(n) %>% 
  filter(!is.na(hurdles)) %>% 
  mutate(hurdle_perc = (n/length(pc$hurdles)*100))

## management
# who I manage
`%nin%` <- negate(`%in%`)
who.i.manage.sum = pc %>%
  filter(!is.na(who_i_manage), who_i_manage %nin% "Not Applicable") %>% 
  dplyr::group_by(who_i_manage, type) %>% 
  summarize(n=n()) %>% 
  mutate(n=as.numeric(n),
         ord = 1,
         ord = ifelse(who_i_manage %in% "Somewhat",2,ord),
         ord = ifelse(who_i_manage %in% "Yes",3,ord)) %>%
  arrange(ord)

# who manages me
who.manages.me.sum = pc %>%
  filter(!is.na(who_manages_me), who_manages_me %nin% "Not Applicable") %>% 
  group_by(who_manages_me, type) %>% 
  summarize(n=n()) %>% 
  mutate(n=as.numeric(n),
         ord = 1,
         ord = ifelse(who_manages_me %in% "Somewhat",2,ord),
         ord = ifelse(who_manages_me %in% "Yes",3,ord)) %>%
  arrange(ord)

# projects/power balance
# projects
projects.sum = pc %>%
  group_by(project_dissemination, type) %>% 
  summarize(n=n()) %>%
  mutate(projects = NA,
         projects = ifelse(project_dissemination %in% "Not applicable, I don’t work on multiple projects", "Not applicable", projects),
         projects = ifelse(project_dissemination %in% "As an employee, I’m overcommitted across the projects I work on.   As a manager, I have too many people and projects to manage, and my employees can’t take any more on.", "Overallocated", projects),
         projects = ifelse(project_dissemination %in% "As an employee, I feel no pressure from managers across the projects I work on and there is a healthy balance of my time across my projects.  As a manager, I do not have any concerns regarding balancing our employees across the projects we have right now.", "Neutral", projects),
         projects = ifelse(project_dissemination %in% "As an employee, I feel I am underutilized and would like to do more in the projects I am currently part of.  As a manager, I feel I need to give more tasks to the employees on the projects I manage to better balance my own workload and feel other managers could do the same.", "Underallocated", projects)) %>% 
  mutate(ord = 1,
         ord = ifelse(projects %in% "underallocated",2,ord),
         ord = ifelse(projects %in% "Neutral",3,ord),
         ord = ifelse(projects %in% "Overallocated",4,ord))
        
# fairness
balance.sum = pc %>%
  filter(!is.na(balance)) %>% 
  group_by(balance, type) %>% 
  summarize(n=n(),
         ord = 1,
         ord = ifelse(balance %in% "Slightly, but not a problem",2,ord),
         ord = ifelse(balance %in% "Slightly, but still a problem",3,ord),
         ord = ifelse(balance %in% "Yes",4,ord)) %>%
  arrange(ord)
```
Thank you to everyone who took the time to answer the 2024 Annual RUCOOL Pulse Check. Below are the summarized results. We had `r length(pc$id)` responses, `r pc_sum$faculty` faculty and `r pc_sum$staff` staff.

# I feel that my roles and responsibilities are well defined
Average:  `r pc_sum$av.roles` $\pm$ `r format(pc_sum$sd.roles, digits=2)`   
2023 Average = 4.18 ± 0.85.  

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot(data = pc) + geom_bar(aes(x=roles, fill=type), position = "stack") +
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("orange","cornflowerblue"))
```

# I feel a sense of ownership and accountability for the work I do. I'm proud of my accomplishments.
Average: `r pc_sum$av.ownership` $\pm$ `r format(pc_sum$sd.ownership, digits=2)`   
2023 Average: 4.36 ± 0.73. 

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + geom_bar(data = pc, aes(x=ownership, fill=type), position = "stack") + 
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue")) 
```

# Communication is clear and I feel heard.
Average: `r pc_sum$av.communication` $\pm$ `r format(pc_sum$sd.communication, digits=2)`   
2023 Average: 3.27 ± 1.24 

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + geom_bar(data = pc, aes(x=communication, fill=type), position = "stack") + 
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```
    
# I feel that my hard work is seen and appreciated.
Average:  `r pc_sum$av.appreciated` $\pm$ `r format(pc_sum$sd.appreciated, digits=2)`   
2023 Average: 3.81 ± 1.18  

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + geom_bar(data = pc, aes(x=appreciated, fill=type), position = "stack") + 
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```

# I feel my skill are utilized.
Average:  `r pc_sum$av.skills` $\pm$ `r format(pc_sum$sd.skills, digits=2)`  

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + geom_bar(data = pc, aes(x=skills, fill=type), position = "stack") + 
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```
  
# I feel part of a larger team, with a unified vision working towards a larger goal.
Average: `r pc_sum$av.unity` $\pm$ `r format(pc_sum$sd.unity, digits=2)`   
2023 Average: 3.59 ± 1  

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + geom_bar(data = pc, aes(x=unity, fill=type), position = "stack") + 
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```
  
# I view RUCOOL and its sub-groups/teams as well organized, defined, and collaborative.    

Average: `r pc_sum$av.well_organized` $\pm$ `r format(pc_sum$sd.well_organized, digits=2)`   

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + geom_bar(data = pc, aes(x=well_organized, fill=type), position = "stack") + 
  xlab("1 (disagree) - 5 (agree) stars") + ylab("Number of Times Selected")+
  xlim(0.5,5.5) + theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```

# Project dissemination: For those who work on multiple projects which statement below best describes your experience. Likewise, for those managing projects, which statement best describes the collective balance you’re experiencing within RUCOOL.  

The text in this question has been shortened in the figure below:

* *Not applicable* = "Not applicable, I don’t work on multiple projects"   
* *Underallocated* = "As an employee, I feel I am underutilized and would like to do more in the projects I am currently part of. As a manager, I feel I need to give more tasks to the employees on the projects I manage to better balance my own workload and feel other managers could do the same."   
* *Neutral* = "As an employee, I feel no pressure from managers across the projects I work on and there is a healthy balance of my time across my projects. As a manager, I do not have any concerns regarding balancing our employees across the projects we have right now."    
* *Overallocated* = "As an employee, I’m overcommitted across the projects I work on. As a manager, I have too many people and projects to manage, and my employees can’t take any more on." 

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + 
  geom_bar(data = projects.sum, aes(x=reorder(projects, ord), fill=type), 
           position = "stack") + 
  xlab("Project Dissemination") + ylab("Number of Times Selected") +
  theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```

# Does this statement hold true to your experiences at RUCOOL: I’m experiencing a tragedy of the commons or a lack of respect/fairness/coordination. For employees: I feel there is an imbalance among the team, and some managers are demanding more of my time.
   
```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + 
  geom_bar(data = balance.sum, aes(x=reorder(balance, ord), fill=type), 
           position = "stack") + 
  xlab("Imbalance") + ylab("Number of Times Selected")+
  theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))+
  theme(axis.text.x = element_text(angle = 10, hjust = 1))

```

# I feel who I manage is clear.
   
```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + 
  geom_bar(data = who.i.manage.sum, aes(x=reorder(who_i_manage, ord), fill=type), 
           position = "stack") + 
  xlab("Who I Manage is Clear") + ylab("Number of Times Selected")+
  theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```

# I feel who manages me is clear.
  
```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + 
  geom_bar(data = who.manages.me.sum, aes(x=reorder(who_manages_me, ord), fill=type), 
           position = "stack") + 
  xlab("Who Manages Me is Clear") + ylab("Number of Times Selected") +
  theme_bw() + theme(legend.position = "bottom")+ 
  scale_fill_manual(values=c("orange","cornflowerblue"))
```
  
# I prefer to communicate over...  
The survey recipient could choose two for communication preferences.   

```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + 
  geom_point(data = cp.sum, aes(x=reorder(comm, n), y=n), size = 5) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  ylab("Number of Times Selected") + xlab("Communication Preference")
```

# I prefer to save my files...  
The survey recipient could choose two.   
   
```{r echo=F, message=F, warning=F, fig.height=3}
ggplot() + 
  geom_point(data = files.sum, aes(x=reorder(files, n), y=n), size = 5) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  ylab("Number of Times Selected") + xlab("File Management Preference")
```

# What are your biggest hurdles to completing your work?
Survey recipients could choose more than one.   
```{r echo=F, message=F, warning=F, fig.height=4}
ggplot() + 
  geom_point(data = h.sum, aes(x=reorder(hurdles, n), y=hurdle_perc), size = 5) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  ylab("Percent") + xlab("Biggest Hurdles")+ 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
```
  
To expand on the other category, individuals included:  

* Steve's departure  
* Unsure of future projects/funding    

# Team goals that people would like to work on include:  

* Increase team cohesion, visibility, and coordination.   
* Getting more stable funding for operations and teaching.  

# Ending on a high note, below are what the team believes are our biggest accomplishments of 2024. 

* Caribbean hurricane research. 
* Team TOS award. 
* How the glider team manages to accomplish so many projects. 
* Great year for publications.  

  
# THANK YOU EVERYONE FOR ALL THE HARD WORK YOU DO! 

```{r echo=F, message=F, warning=F}
# # success
# # word cloud
#library(wordcloud)
#library(RColorBrewer)
#library(wordcloud2)
#require(tm)
#Create a vector containing only the text
#text <- data$text
# Create a corpus  
#docs <- Corpus(VectorSource(text))
# #matrix
#dtm <- TermDocumentMatrix(docs) 
#matrix <- as.matrix(dtm) 
# words <- sort(rowSums(matrix),decreasing=TRUE) 
# df <- data.frame(word = names(words),freq=words)
# #cloud
# wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
#           max.words=200, random.order=FALSE, rot.per=0.35,            
#           colors=brewer.pal(8, "Dark2"))
```


