
####################
##EV SALES DATA#####
####################

#####Call the necessary libraries#####
library(readxl)
library(ggplot2)
library(skimr)
library(dplyr)
library(RColorBrewer) 
library(gridExtra)

## Set working directory
setwd("C:/Users/Windows/Documents/BU ASSessments/DDM")

## Query your data
Rep <- read_excel("rep_18.xlsx")
## Get ev
Ev <- read_excel("ev_18.xlsx")


## join the data using left  join
Ev_rep <- left_join(Ev, Rep, by = "rep_id")

#explore data
skim(Ev_rep)
summary(Ev_rep)
str(Ev_rep)

  
#######Clean the data 
##Assign a name to the last column
Ev_rep <- rename(Ev_rep, training = ...6)

##filtering out the na's, wrong values & Outliers
Ev_rep <- Ev_rep %>% select(commissions,marketing,purchase,product, 
                            promotions,buyer, campaign, rep_id, jobtype,qualification, gender, experience) %>% 
  filter(promotions != "task", campaign != "slope",
         product != "film", buyer != "welt", gender != "unknown/ don't want to say",
         commissions > 0)

Ev_rep <- na.omit(Ev_rep)

#####################
####Transforming#####
#####################



## recoding "yes" to promo and "no" to regular
Ev_rep$promotions <- recode_factor(Ev_rep$promotions, 'yes' = "promo", "no" = "regular")


###create a new variable for experience 0-10 "Rookie", "Midtenure", "Veteran"
Ev_rep <- Ev_rep %>%mutate(experience1 = ifelse(experience <= 10, "Rookie 1-10", 
                                                ifelse(experience <= 20, "Midtenure 11-20", "Veteran 21-40")))




################################
#####Descriptive Statistics#####
################################
skim(Ev_rep)
summary(Ev_rep)
str(Ev_rep)

## Character Variables
table(Ev_rep$product)
table(Ev_rep$promotions)
table(Ev_rep$buyer)
table(Ev_rep$campaign)
table(Ev_rep$jobtype)
table(Ev_rep$qualification)
table(Ev_rep$gender)
table(Ev_rep$experience1)

###Numerical Variables
summary(Ev_rep$commissions)
summary(Ev_rep$marketing)
summary(Ev_rep$purchase)

table(Ev_rep$marketing)


table(Ev_rep$marketing[Ev_rep$product == "sedan"])

as.numeric(Ev_rep$marketing)

sum(Ev_rep$marketing[Ev_rep$product == "sedan"])
sum(Ev_rep$marketing[Ev_rep$product == "s"])
mean(Ev_rep$marketing)

################################
### Visualization Section ######
################################


##### Figure 1:Distribution of the KPI which is measured by purchase ########
f1 <- ggplot(Ev_rep,aes(purchase)) + geom_histogram(binwidth = 100,color = "black", 
                                                    fill = "#008754") +
  labs(x = "Revenue", y = "", title = "Revenue Distribution") +  theme_minimal()  +
  theme(axis.text.y = element_blank(),axis.line.y = element_blank(),
        text = element_text(size = 10,family = "mono",face = "bold"))






##### Figure 2:Purchase by Product  ###################

f2 <- Ev_rep %>%
  group_by(product) %>%
  summarise(sales_thousands = sum(purchase) / 10000) %>%
  ggplot(aes(x = factor(product, levels = c("sedan","suv","sport")),
             y = sales_thousands, fill = product)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(sales_thousands, 1)),
            hjust = -0.2,
            size = 3,
            fontface = "bold") +
  coord_flip() +
  labs(x = "Product", y = "Sales in Thousands",
       title = "Contribution of Product to Revenue") +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        text = element_text(size = 15, family = "mono", face = "bold")) +
  scale_fill_manual(values = c("#008754", "#CC0605", "#FF7E00"))






#####Figure 3: Average of purchase by marketing ####



f3 <- ggplot(Ev_rep, aes(x = marketing, y = purchase, color = product)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
  facet_wrap(~ product, scales = "free_y") +
  labs(
    title = "Does Spending More on Marketing Increase Sales?",
    x = "Marketing Spend",
    y = "Average Sales"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "mono", size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_color_manual(values = c("#008754", "#CC0605", "#FF7E00"))






#####Figure 4: Purchase by promotion accross the marketing channels
f4 <- ggplot(Ev_rep,aes(x=factor(campaign, levels =c("tiktok","fbook","instagram","twitter")),
                        y = purchase/ 100000, fill = campaign)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~factor(promotions,levels = c("promo","regular"))) + 
  labs(title = "Effectiveness of Marketing Channels & Promotions",y = "Sales in Thousand") +
  theme_classic()+theme(legend.position = "top",
                        legend.justification = "left",legend.title = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=0.5),
                        text = element_text(size = 15,family = "mono", face = "bold")) + 
  scale_fill_manual(values = c("#FF7E00", "#0047BB","#008754", "#CC0605"))



ggplot(Ev_rep, aes(
  x = factor(campaign, levels = c("tiktok", "fbook", "instagram", "twitter")),
  y = purchase / 10000,   # divide by 1000 to match your "Sales in Thousand" label
  fill = campaign
)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(purchase / 1000, 1)),   # show values on top of bars
            vjust = -0.3, size = 4, family = "mono", fontface = "bold" ) +
  facet_wrap(~factor(promotions, levels = c("promo", "regular"))) +
  labs(
    title = "Effectiveness of Marketing Channels & Promotions",
    y = "Sales in Thousand"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    text = element_text(size = 15, family = "mono", face = "bold")
  ) +
  scale_fill_manual(values = c("#FF7E00", "#0047BB", "#008754", "#CC0605"))







##### Figure 5: Purchase by Buyer accross marketing Channels #######
F5 <- ggplot(Ev_rep,aes(x=campaign, y = purchase/ 10000, fill = product)) + 
  geom_bar(stat = "identity") +
  labs(x = "Product", y = "Sales in Thousands",title = "Customers profile & Preferred channels") + 
  facet_wrap(~factor(buyer,levels = c("single","family", "couple")))+
  theme_classic()+theme(legend.position = "bottom",
                        legend.title = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
                        text = element_text(size = 10,family = "mono", face = "bold")) + 
  scale_fill_manual(values = c("#008754", "#CC0605","#FF7E00","#FFB6C1"))






#### Figure 6: Average of purchase by Commission ####
f6 <-  ggplot(Ev_rep, aes(x = commissions, y = purchase)) + 
  geom_jitter(stat = "summary", fun = "mean",
              aes(shape = product, color = product, fill = product), size = 1.5) + 
  geom_smooth(se = FALSE, color = "#1E90FF") + 
  labs(x = "commissions",y = "Average of Sales",
       title = "Relationship Between Sales and Commissions") + theme_minimal() +
  theme(text = element_text(family = "mono", size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("#008754", "#CC0605", "#FF7E00")) +
  scale_fill_manual(values = c("#008754", "#CC0605", "#FF7E00")) + facet_wrap(~product)



ggplot(Ev_rep, aes(x = commissions, y = purchase, color = product)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
  facet_wrap(~ product, scales = "free_y") +
  labs(
    title = "Relationship Between Sales and Commissions",
    x = "Commissions",
    y = "Average Sales"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "mono", size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_color_manual(values = c("#008754", "#CC0605", "#FF7E00"))

  


#### Figure 7: Purchase by Experience & Qualification ####
f7 <-ggplot(Ev_rep, aes(x=factor(qualification, levels = c("msc","bsc","hnd")),
                        y= purchase/ 10000, fill = qualification)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~factor(experience1, levels = c("Veteran 21-40", "Midtenure 11-20", "Rookie 1-10"))) +
  labs(title = "Sales by Experience & Qualification",y = "Sales in Thousand") +
  theme_classic()+theme(legend.position = "top",
                        legend.justification = "left",
                        legend.title = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                        text = element_text(size = 10,family = "mono", face = "bold")) +
  scale_fill_manual(values = c("#FF7E00", "#CC0605","#008754"))




##### Dashboard ####
grid.arrange(f1,f2,f4,f7)

