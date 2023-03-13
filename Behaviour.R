Sys.setenv(LANGUAGE = "en")
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


library(lattice) # xy plot

library(dotwhisker) ## coefficient plots
library(tidyfast)
library(dtplyr)


# data import -------------------------------------------------------------
# rawdata
set.wd()
p_load(glue,corrplot,broom,rstatix,ggpackets,broom.mixed,forcats,
       ggeffects,gtsummary,officer,flextable)
fac.vars <- c('Session','Number','Gender')

rawdata <- import('D_YH_Behavior.xlsx',sheet='rawdata',as = 'dt')[
  # if LHS has multiple col names , should add ()
  , (fac.vars):= lapply(.SD,as.factor),.SDcols = fac.vars][
    ,Gender:=fct_recode(Gender,'Male' = '1','Female'= '2')][
      .(Session = as.factor(1:5) ,to = c('base','pre_Q','post_Q','pre_dance','post_dance') %>% factor(.,.)),
      on = 'Session',Label := i.to ]

# get present
Attendance <- rawdata[, .(Number, Attendance)] %>% na.omit()
rawdata <- merge(rawdata[, -'Attendance'], Attendance, by = 'Number', all = T)[order(Session)]
# after merge , the key column is set to 'Number' because by = 'Number'
setindex(rawdata, Label, Number)

#vars

vars <- rawdata[, SES_em:Attitude] %>% names() %>% .[-'SES']
Qaddvars <- c('A1', 'A2', 'A3', 'Attitude', 'ISI', 'IES')
Qvars = c('SEAQ', 'LSA', 'PCS', 'MCS', 'BAI', 'BDI')
con.vars <- c('Age', 'Gender', 'Attendance')

dancevars <- setdiff(unlist(vars),
                     c('SEAQ', 'LSA', 'SES',
                       #'Extraversion','Emotionality',
                       'OCEAN', 'ACIPS',
                       Qaddvars))
lut_list  <- list(
  t1 = data.table(from = as.character(1:4), 
                  to = c('base','pre_Q','post_Q','pre_dance')),
  t2 = data.table(from = as.character(2:5), 
                  to = c('pre_Q','post_Q','pre_dance','post_dance'))
)


# subject -----------------------------------------------------------------

subject <- rawdata[,.(number = .(Number)),by = .(Label,Gender)][,.SD,Gender][
  , c("quit", "keep", "newadd"):=
    transpose(map2(number,dplyr::lag(number),\(x,y)
                   list( setdiff(y, x), #quit
                         intersect(x, y), #same
                         setdiff(x, y)) )),by = Gender] 


# rawdata %>% 
#   group_by(Gender, Session) %>%
#   summarise(sub = list(Number)) %>%
#   group_by(Gender) %>%
#   mutate(quit = map2(sub, dplyr::lag(sub), ~ setdiff(.y, .x)),
#          same = map2(sub, dplyr::lag(sub), ~ intersect(.x, .y)),
#          newadd = map2(sub, dplyr::lag(sub), ~ setdiff(.x, .y))) 



(sub_number <- subject[ ,map(.SD,\(x) map_int(x,length)),by = .(Label,Gender)])

sub <- sub_number %>% 
  dcast(Label ~ Gender,value.var = c('number','keep')) 

  (sub_t_wide <- sub_number %>% 
    dcast(Label ~ Gender,value.var = c('keep')) %>% 
    .[,allkeep := Male + Female] %>% 
    .[,subinfo:= pmap(.(allkeep,Male,Female), 
                      \(x,y,z) glue( 'N = {x}', 'Male = {y}', 'Female = {z}', .sep = ', '))])

#cleandata <- datanew[CJ(c(t1,t2),samesub)]

# Diff --------------------------------------------------------------------

#All diff on each adjacent time point —— diff 
(diff <- setorderv(rawdata,'Session')[,paste('Δ',vars,sep = ''):= map(.SD,\(x) x - shift(x) ),
                                      by = Number,.SDcols = vars] %>% print())
# Correlation ---------------------------------------------------------------------

#select( where(\(x) !any(is.na(x)) )) 
# need to convert Gender variable to numeric class before calculation of corr 

# setnames(c('ΔGSES','ΔRSES','ΔSES_em','ΔExtraversion',
#            'ΔEmotionality',),
#          c('ΔGSE','ΔRSE','ΔSES','ΔExt','ΔEmo'))


(cordata <- diff['pre_dance',.SD,.SDcols = patterns('^Δ'),on = 'Label',by = con.vars] %>% 
   select( where(\(x) !all(is.na(x)) )) %>% na.omit())




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



mytest[c("control", "release", "dance"),map(pcor,my_corplot),on='label']


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

# need set key on time column
# which contain t1 and t2 value
info <- data.table(
  label = c('control', 'quarantine', 'release', 'confirm', 'dance') %>% factor(., .),
  t1 = c('base', 'pre_Q', 'post_Q', 'pre_Q', 'pre_dance'),
  t2 = c('pre_Q', 'post_Q', 'pre_dance', 'pre_dance', 'post_dance'),
  colors = list(
    c('#FEFFBE', '#a3c3d9'),
    c('#a3c3d9', '#0C6291'),
    c('#0C6291', '#E8B4BC'),
    c('#a3c3d9', '#E8B4BC'),
    c('#E8B4BC', '#D282A6')
  )
)[, timelabel := map2(t1, t2, c)] %>%
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
           
           c("control", "release", "dance"),
           `:=`(c('cor_result','ttest_result'), 
                {
                  cor_result = map2(diff,var,\(x,y)  my_cor_pcor(x,str_c('Δ',y),con.vars))
                  
                  ttest_result = pmap(list(data,var,t2),my_ttest) # t2 argument to set ref group
                  
                  .(cor_result,ttest_result)
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
               , on = 'label'] %>% 
  
  set_dtlist_name('label')


getresult('cor_result')
getresult('ttest_result') 
getresult('lme_result')


ttest_result[
  ,`:=`(c('table'), 
        {
          tabledata = map2(cleandata,var, \(x,y) x[,.SD,.SDcols = c('Number','Session',y)][
            ,Session := fct_drop(Session,only = NULL)]) # delete useless levels in a factor
          table = map(tabledata, my_t_table)
          .(table)
        })][,`:=`(c('all_box','correc_box'), 
                   {
                     all = pmap(.(longdata,var,stat.test,
                                  timelabel,colors,subinfo),~my_boxplot(...,all = T))
                     
                     correc = pmap(.(longdata,sig.vars.fdr,stat.test,
                                     timelabel,colors,subinfo),~my_boxplot(...,all = F))
                     .(all,correc)
                   })]


# ttest_result$all_box 
# ttest_result$correc_box

cor_result[
  ,`:=`(c('corplot','pcorplot'),
        {
          corplot = map(cor,my_corplot)
          
          pcorplot = map2(pcor,pcor.test,my_pcorplot)
          .(corplot,pcorplot)
        })]





# Mod/Med

# Med <- PROCESS(dance_diff,y = 'ΔExtraversion',x = 'ΔSAQ',
#                meds=c('ΔRSES'),
#                covs = c('Age','Gender','Present'),
#                ci="boot", nsim=1000, seed=1)
# 
# Mod <- PROCESS(rawdata['4'],y = 'BDI',x = 'SES',
#                mods=c('RSES'),covs = c('Age','Gender','Present'))
# 



# output ------------------------------------------------------------------

ttest_result[,map2(all_box,label,\(x,y) ggsave( filename =paste( y,'_allbox.tiff'),plot = x,width = 10,height = 8.5))]



lme_result[,tbl := .(map(unlist(model,recursive = F),tbl_regression))]
lme_tbl <- lme_result[,tbl_stack(unlist(tbl,recursive = F),group_header = var[[1]])] %>% 
  add_q() %>%  bold_p(q = T) %>% add_significance_stars() %>% 
  as_flex_table() 

doc <- read_docx() %>% 
# 将表格插入到Word文档中
body_add_flextable(lme_tbl)

# 保存Word文档
print(doc, target = "lme_tbl.docx")


# One time use ------------------------------------------------------------



tiledata <- map(list(ttest_result,lme_result),\(x) x[['sig.vars.fdr']]) %>% unlist(recursive = F) %>% 
  map(\(x) dancevars %in% x) %>% as.data.table() %>% .[,var := dancevars]


(c(str_c(as.character(ttest_result$label),'_ttest'),'release_lme','var') %>% 
  setnames(tiledata,.))

tiledata %<>% 
setorderv('dance_ttest',order = -1) %>% 
   melt( id.vars = 'var',variable.name = 'method',
        value.name = 'sig')
tiledata[,name:=ifelse(sig == T,var,'')]                      

ttest_result[, map(sig.vars.fdr, \(x) dancevars %in% x) ]%>% 
  setnames(str_c(as.character(ttest_result$label),'_ttest'))

dancevars %in% lme_result$sig.vars.fdr[[1]] 





myresult[, map(diff,\(x)  testcor(x))]
testcor <- function(data) {
  Corr(data[,-'Number'])
}

# tbl merge into one doc ####


tblresult <- myresult$t_table %>% map_if(is.list,as_flex_table)

# 将表格插入到Word文档中
accumulate(tblresult,\(x) body_add_docx(doc, x))
doc <- body_add_flextable(doc, table2)

# 创建要插入的表格
table1 <- flextable(mtcars)
table2 <- flextable(iris)

# 将表格插入到Word文档中
doc <- body_add_flextable(doc, table1)
doc <- body_add_flextable(doc, table2)

# 保存Word文档
print(doc, target = "mytables.docx")


# info output ####

Atte <- Attendance[,sub:=ifelse(Number %in% c('15','36','40','46','47'),'quit','keep')][
  ,Attendance:=as.factor(Attendance)] 


sig.plot <- ggplot(tiledata,aes(x = var,y = method,fill = sig))+
  geom_tile(color = 'black')+
  scale_fill_manual(values = c( "#C8C7C5","#002FA7"))+
  geom_text(aes(label = name),color ='white')+coord_flip()+
  labs(title = 'All-period results using paired t-test and linear mixed model', 
       subtitle = "Correction method : False Discovery Rate ",
       x = "", 
       y = "") +
    theme_bruce() +
  theme(plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"), 
        axis.text = element_text(size = 10, face = "bold"), 
        axis.title = element_text(size = 10, face = "bold"))


Atte.plot <- ggplot(Atte, aes(x = Attendance, fill = sub)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", 
            position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
  labs(title = 'Attendance of Dance Intervention Participants', 
       subtitle = "Number of dance classes: 5",
       x = "Frequency of attendance", 
       y = "Headcount") +
  scale_fill_manual(values = c( "#002FA7","#C8C7C5")) + 
  theme_bruce() +
  theme(legend.text = element_text(size = 13, color = "black", face = "bold"),
    legend.title= element_blank(),
    legend.position = c(0.1, 0.9),
    legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"), 
        axis.text = element_text(size = 10, face = "bold"), 
        axis.title = element_text(size = 10, face = "bold")) +
  geom_label(x = 1.8, y = 12, label = "N = 24, keep = 19, quit = 5", 
             size = 15, fontface = "bold", 
             color = "black", fill = "white")

ggsave('Atte.tiff',Atte.plot,width = 10,height = 8)
ggsave('sig.tiff',sig.plot,width = 10,height = 8)
plot_grid(list(Atte.plot+sig.plot)) 
