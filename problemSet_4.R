
library(magrittr)
library(readr)
library(tidyr)
library(dplyr)

myName <- "Yaquan Yang"
## 1
    print_order<-function(x)
    {
        a<-x[1]
        b<-x[2]
        c<-x[3]
        
        if (a <= b & a <= c){ 
            if (b <= c)  out<-c(c,b,a) else out<-c(b,c,a)
        }
        else if (b <= a & b <= c) {
            if (a <= c) out<-c(c,a,b) else out<-c(a,c,b)
        }
        else {                          
            if (a <= b) out<-c(b,a,c) else out<-c(a,b,c)
        }
        
        out	
    }
    
    ##2
    print_string<-function(n)
    {
        for(ind in 1:n) {
            out<-ind
            if(ind %% 3==0) out<-'Yes'
            if(ind %% 5==0) out<-'No'
            if(ind %% 3==0 & ind %% 5==0) out<-'Unknown'
            print(out)
        }
    }
    
## 3
    calc_sum_of_factor<-function(n)
    {
        out<-sapply(1:n,function(x) ifelse(n %% x, 0, x^2))
        sum(out)
    }
    
## 4
    find_intersect<-function(x1,x2,x3)
    {
        xs<-unique(c(x1,x2,x3))
        ins<-sapply(xs,function(x) c(x %in% x1,x %in% x2, x %in% x3))
        xs[apply(ins,2,all)]
    }
    
## 5
    factorial_base<-function(n)
    {
        tail(cumprod(1:n),1)
    }
    
## 6
    T<-function(n) n*(n+1)/2
    
    perfect_sqr<-function(x) {
        y<-sqrt(x)
        y==trunc(y)
    }
    
    
    num_tri_sqr<-function(n)
    {
        ns<-sapply(1:n,T)
        ns[sapply(ns,perfect_sqr)]
    }
    
    
    (v8<-num_tri_sqr(1500000))
    
    sum(as.numeric(v8)) ###57101607436
    
    q6_sum <- sum(num_tri_sqr(1500000))
    
    
## H-1B
    # 1
    
    h1b_2022 <- read_csv("https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv")
    
    # 3
    
    ul_md <- is.na(unlist(h1b_2022))
    na_num <- length(ul_md[ul_md == TRUE])
    
    h1b_2022a <- h1b_2022 %>% drop_na()
    h1b_2022a <- h1b_2022a[h1b_2022a$City != "-",]
    
    # h1b_2022a <- na.omit(h1b_2022)
    # h1b_2022a <- h1b_2022a[!(h1b_2022a$City=="-"),]
    
    
    # 4
    
    df_num <- h1b_2022a %>% group_by(State) %>% summarise("Conti Approval"=sum(`Continuing Approval`),
                                                          "Conti Denial"=sum(`Continuing Denial`),
                                                          "Approve"=sum(`Initial Approval`),
                                                          "Denial"=sum(`Initial Denial`)) %>%
        mutate("Init App"=Approve+Denial,
               "Conti App"=`Conti Approval`+`Conti Denial`) %>%
        select(1,6,7,4,5)
    

    
    # 5
    
    app_num <- sum(as.integer(df_num$Approve))
    den_num <- sum(as.integer(df_num$Denial))
    
    # 6
    
    city_num <- h1b_2022a %>% 
        select(3,10) %>% group_by(City) %>% 
        count(`Initial Approval`) %>% 
        arrange(City) %>% 
        transmute(Count=sum(n)) %>% 
        unique()
    
    # 7
    
    visa_num <- h1b_2022a %>%
        group_by(NAICS) %>%
        arrange(NAICS) %>%
        count(NAICS) %>%
        transmute(Number=sum(n)) %>%
        unique()
    
    visa_num$Percentage <- round(visa_num$Number *100 / sum(visa_num$Number), digits = 3)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    