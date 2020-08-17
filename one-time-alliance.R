
# print(sessionInfo())
# source("000-common.R")

if (T) { 
  options(scipen=999); #remove scientific notation
  library(data.table)
  options(datatable.print.class=TRUE)
  library(magrittr)
  library(dtplyr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  # library(plotly)
  # library(DT)
}


# 0.1 My functions ----





str <- "https://docs.google.com/spreadsheets/d/1d9uSQuRk0VhCefbim0z35V8XnwiLCrcz9iXBhmerlAc"

library(gsheet)
dtAll <- gsheet2tbl('docs.google.com/spreadsheets/d/1d9uSQuRk0VhCefbim0z35V8XnwiLCrcz9iXBhmerlAc') %>% 
  data.table(dt)

dtAll <- dtAll[!is.na(Province)] %>% select(2:11) 

prov <- "ON"

for ( prov in dtAll$Province %>% unique() ) {
  dt <- dtAll %>% filter(Province==prov)
  
  dt[, `Combined: Grn & NDP` := Grn + NDP]
  dt[, `Combined: Lib & NDP` := Lib + NDP]
  

  dt9 <- melt(dt,1:4)
  setnames(dt9, "variable", "party")
  setnames(dt9, "value", "votes")
  
  # dt9 <- dt9[order(votes)]
  
  colsParties <- c("Lib" = "red", "Con" = "blue", "Grn" = "darkgreen", "NDP" = "orange", "Other" = "gray", "Bloc" = "violet", "Combined: Grn & NDP" = "brown", "Combined: Lib & NDP"="pink")
  
  dt9$party %>% unique
  
  g <- ggplot(dt9) +
    facet_wrap(.~`Riding Name`) +
    theme_bw() +
    theme(legend.position = "bottom") +
    # coord_flip() + 
    scale_fill_manual(values = colsParties) +
    scale_colour_manual(values = colsParties) +
    geom_col(aes(x=party, y=votes, fill=party
                 # fill=party
                 # fill=reorder(party, votes),
    ),
    ) +
    labs(
      x=NULL, 
      title="Votes distribution in 2019 elections",
      subtitle=prov
    )
  print(g)
  
  ggsave(paste0("votes-", prov, ".jpg"), width=18, height=12)
  
}

dt[1]
dt[, 1:11]
dt <- dt[, 1:7]

dtAll$Province %>% unique()

for ( prov in dtAll$Province %>% unique() ) {
  
  
  
}

dt  <- dtAll[Province == prov]


dt[1:2]
dt2 <- dt %>% lazy_dt() %>% mutate_all(~replace(., is.na(.), 0)) %>% as.data.table()

# dt1 <- dt %>% mutate_all(~replace(., is.na(.), 0)) 

dt2[, All:=Bloc + Con + Grn + Lib + NDP + Other][]

cols <-names(dt)[2:7] 
cols1 <- paste0(cols, "%")
cols0 <- paste0(cols, "#")


# dt[, (cols1) := lapply(.SD, function(x){x/All}) , .SDcols=cols]
dt2[, (cols1) := .SD / All * 100, .SDcols=cols]
dt2[, (cols0) := .SD, .SDcols=cols]

melt(dt2, id=1, measure=patterns(f="*%", d="*#"))

melt (dt2, 1)


dt <- dt[, 4:10]
dt <- melt (dt, 1)

names(dt)
dt[1:2]

ggplot(dt) + facet_wrap(`Riding Name` ~ .) + 
  
  
  