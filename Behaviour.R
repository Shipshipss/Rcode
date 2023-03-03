Sys.setenv(LANGUAGE = "en")
Sys.setenv(OPENAI_API_KEY = "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")
p_load(bruceR,psych,purrr,pacman,magrittr)



library(gghighlight)
library(ggthemes)
library(ggdist)
library(esquisse) #drag & drop ggplot
library(ggtext)
library(ggsci)
library(ggrepel)



library(gridExtra)
library(sjPlot) # table 
library(interactions) #plot interaction 
library(jtools) #compare model coef
#library(PerformanceAnalytics)

library(styler) # find function and shortcut
library(gt)
library(gtsummary)

library(lattice) # xy plot

library(dotwhisker) ## coefficient plots
library(tidyfast)
library(dtplyr)
library(gtsummary)

# data import -------------------------------------------------------------
# rawdata
set.wd()
fac.vars <- c('Session','Number','Gender')

rawdata <- import('D_YH_Behavior.xlsx',sheet='rawdata',as = 'dt')[
  # if LHS has multiple col names , should add ()
  , (fac.vars):= lapply(.SD,as.factor),.SDcols = fac.vars][
    ,Gender:=fct_recode(Gender,'Male' = '1','Female'= '2')][
      .(Session = as.character(1:5) ,to = c('base','pre_Q','post_Q','pre_dance','post_dance')),
      on = 'Session',Label := i.to ]

# get present
Attendance <- rawdata[,.(Number,Attendance)] %>% na.omit()
rawdata <- merge(rawdata[,-'Attendance'],Attendance,by = 'Number',all = T)[order(Session)]
# after merge , the key column is set to 'Number' because by = 'Number'
setindex(rawdata,Label,Number)

#vars

vars <- rawdata[,SES_em : Attitude] %>% names()
Qaddvars <- c('A1','A2','A3','Attitude','ISI','IES')
Qvars = c('SEAQ','LSA','PCS','MCS','BAI','BDI')
con.vars <- c('Age','Gender','Attendance')

dancevars <- setdiff(unlist(vars), c('SEAQ','LSA','SES',
                                     #'Extraversion','Emotionality',
                                     'OCEAN','ACIPS',
                                     Qaddvars))
lut_list  <- list(
  t1 = data.table(from = as.character(1:4), 
                  to = c('base','pre_Q','post_Q','pre_dance')),
  t2 = data.table(from = as.character(2:5), 
                  to = c('pre_Q','post_Q','pre_dance','post_dance'))
)


# subject -----------------------------------------------------------------

subject <- rawdata[,.(sub = .(Number)),by = .(Label,Gender)][,.SD,Gender][
  , c("quit", "same", "newadd"):=
    transpose(map2(sub,dplyr::lag(sub),\(x,y)
                   list( setdiff(y, x), #quit
                         intersect(x, y), #same
                         setdiff(x, y)) ))] 



sub_number <- subject[ ,map(.SD,\(x) map_int(x,length)),by = .(Label,Gender)]

sub_wide <- sub_number %>% 
  dcast(Label ~ Gender,value.var = 'sub') %>% 
  .[,all := Male + Female]

#cleandata <- datanew[CJ(c(t1,t2),samesub)]

# Diff --------------------------------------------------------------------

#All diff on each adjacent time point —— diff 
diff <- setorderv(rawdata,'Session') %>% 
  .[,paste('Δ',vars,sep = ''):= map(.SD,\(x) x - shift(x) ),
    by = Number,.SDcols = vars]


# Correlation ---------------------------------------------------------------------
#corrplot variable order

# need to convert Gender variable to numeric class before calculation of corr 

  setnames(c('ΔGSES','ΔRSES','ΔSES_em','ΔExtraversion',
             'ΔEmotionality','ΔOCEAN','ΔAttitude','ΔACIPS'),
           c('ΔGSE','ΔRSE','ΔSES','ΔExt','ΔEmo','ΔB5','ΔAtt','ΔACI'))




D.pcorplot <- corrplot(D.pcor,p.mat = pcor.t$p,
                       method="square", type = 'l',
                       insig = 'blank',
                       addCoef.col = 'white',number.cex = 0.5,
                       tl.pos = 'd',tl.srt = 0,tl.col='black',tl.cex = 0.6,tl.offset = 0.1,
                       # cl.pos = 'n', # remove color bar
                       # bg = 'color1',addgrid.col = 'color2',
                       col = COL2('BrBG'))

#Simple correlation
D.cor <- Qdiff %>% 
  Corr(plot = F)
D.corplot <- corrplot(D.cor$r,p.mat = D.cor$p,
                      method="square",type="u",
                      insig = 'blank',
                      addCoef.col = 'black', number.cex = 0.5, 
                      tl.pos = 'd',tl.srt=0, tl.col='red', tl.cex = 0.6, tl.offset = 0.1,
                      col = COL2('BrBG'),
                      title = 'Simple correlation',
                      mar = c(0,0,1,0))





# Quarantine --------------------------------------------------------------

# lmer

Q.longdata <- melt(rawdata, 
                   id.vars = c("Session",'Number'), 
                   measure.vars = Qvars, 
                   variable.name = "vars") %>% setkey(Session)


Quarantine <- Fun_lme(Qdata,Qvars,c('2','3','4'))


Quarantine %$%
  {
    ggplot(Q.longdata[c('2','3','4')], aes(Session, value)) +
      pack_geom_box(
        line.mapping = aes(group = Number),
        box.alpha = 0.7, box.mapping = aes(fill = Session)
      ) +
      geom_line(predline[!"2_4"], mapping = aes(x.lab, predicted, group = 1)) +
      geom_line(predline["2_4"], mapping = aes(x.lab, predicted, group = 1)) +
      facet_wrap(~ factor(vars, levels = c("SEAQ", "PCS", "BAI", "LSA", "MCS", "BDI")),
                 scales = "free_y"
      ) +
      geom_text(siglabel, mapping = aes(x = x.m, y = pred.m, label = sig), fontface = "bold")
  } + 
  
  pack_scale(all.labels =  c('Q_Pre','Q_During','Q_Post'),
             fill.palette = 'Set2')+
  theme_bw() +
  #  labs(title = 'Significant variable PLOT on paired t-test',
  #       subtitle = 'N = 19 (F:10 / M:9)')+
  pack_theme(theme.legend.position = "none") +
  
  labs(
    title = "Linear mixed model on two time point",
    subtitle = "DV ~ Session + (1 | Subject)", fontface = "bold"
  )




# Dance -------------------------------------------------------------------
p_load(corrplot,broom,rstatix,ggpackets,broom.mixed,ggeffects,gtsummary)

# need set key on time column
# which contain t1 and t2 value
info <- data.table(
  label = c('control','quarantine','release','confirm','dance'),
  t1 = list('base','pre_Q','post_Q','pre_Q','pre_dance'),
  t2 = list('pre_Q','post_Q','pre_dance','pre_dance','post_dance')
) %>% 
  setindex(label) 

myresult <- copy(info) 
# dancevars / con.vars 
myresult[,data := map2(t1,t2,\(x,y) rawdata[c(x,y),on = 'Label'])][
  , `:=`(c("measure", "var",'cleandata','diff'), 
         {
           measure = map(data,find_measure)
           var = map(measure,~intersect(dancevars,.))
           cleandata = map2(data,var, \(x,y) 
                            x[Number %in% find_samesub(x),
                              .SD,.SDcols = c('Session','Number',con.vars,y)])
           diff = map2(cleandata,var,my_diff)
           .(measure, var, cleandata, diff) 
         })][
           ,pcor:= map2(diff,var,\(x,y)  my_pcor(x,str_c('Δ',y),con.vars))][
             
             c("control", "release", "dance"),
             `:=`(c('ttest_result','t_table'), 
                  {
                    ttest_result = pmap(list(data,var,t2),my_ttest) # t2 argument to set ref group
                    tabledata = map2(cleandata,var, \(x,y)
                                     x[,.SD,.SDcols = c('Number','Session',y)][
                                       ,Session := as.character(Session)])
                    table = map(tabledata,my_t_table)
                    .(ttest_result,table)
                  })
             ,on = 'label'][
               
               c("quarantine", "release", "confirm"),
               Quarantine := map(data,~my_lme(.,Qvars)),on = 'label'][
                 
                 'release',
                 `:=`(c('lme_result'), 
                      {
                        lme_result = map2(data,var,my_lme)
                        .(lme_result)
                      })
                 , on = 'label']



set_dtlist_name(myresult,'label')



my_boxplot <- function(label) {
  
  myresult[label,ttest_result,on = 'label'] %>% unlist(recursive = F) %$%  # magrittr:: Expose the names in lhs to the rhs expression
    {                       # %$% pass on to all layers using {}
      
      ggplot(longdata[sig.vars.fdr],aes(Label,value))+
        #Do not need D_ttest$t_data,if use %$%
        
        pack_geom_box(line.mapping = aes(group = Number),
                      box.mapping = aes(fill = Session)) %+% #ggplot:: expose
        pack_sig(text.data = stat.test[sig.vars.fdr], 
                 #dont need to D_ttest$t_test,if use %$%
                 
                 text.mapping = aes(x = 1.5,y=Inf,label = str_c('p = ',p.fdr)))+
        facet_wrap(~vars,scales = 'free_y')+
        scale_x_discrete(labels = timelabel)+
        scale_fill_brewer(palette = 'Set1',labels = timelabel)+
        # pack_scale(all.labels = c(timelabel),
        #            fill.palette = 'Set1')+
        theme_bw()+theme(axis.title = element_blank())+
    #    labs(title = 'Significant variable PLOT on paired t-test',
    #         subtitle = 'N = 19 (F:10 / M:9)')+
        pack_theme(theme.legend.position='none')
    }
  
}







# Mod/Med

Med <- PROCESS(dance_diff,y = 'ΔExtraversion',x = 'ΔSAQ',
               meds=c('ΔRSES'),
               covs = c('Age','Gender','Present'),
               ci="boot", nsim=1000, seed=1)

Mod <- PROCESS(rawdata['4'],y = 'BDI',x = 'SES',
               mods=c('RSES'),covs = c('Age','Gender','Present'))

# paper  ------------------------------------------------------------------



save_as_docx(path = "C:/Users/Ship/R")

# test --------------------------------------------------------------------

get_result <- function(result,variable) {
  # Each element may have a different length or be of a completely different type, 
  # so it is necessary to wrap them in a list.
  # then convert them to data.table and bind them together to look better
  result <-  with(myresult,get(variable)) %>% 
    map( \(x) map(x,list) %>% as.data.table()) %>% 
    rbindlist(idcol = 'label')
  # usetoplot <- result[info,on = 'label'][,timelabel:=map2(t1,t2,c)]
  
  # names = c('result','usetoplot')
  
  #return(setNames(list(result,usetoplot), names))
  return(result)
}

# list2env(.GlobalEnv)  

# append(list(vars = dancevars),after = 0) %>% 





# One time use ------------------------------------------------------------


# sigtile

adj.plot<- ggplot(tileplot,aes(x = vars,y = method,fill = sig))+
  geom_tile(color = 'black')+
  scale_fill_manual(values = c( "#C8C7C5","#002FA7"))+
  geom_text(aes(label = name),color ='white')+coord_flip()+
  theme( axis.title = element_text(size = 9),
         axis.text.x = element_text(size = 12))





