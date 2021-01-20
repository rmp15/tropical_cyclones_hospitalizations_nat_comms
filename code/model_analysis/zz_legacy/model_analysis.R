# this code will take a particular cause of death
# and analyse the chosen model

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1
lag = as.numeric(args[2])

# years of analysis
years = c(1999:2014)

# expand grid of stuff (temporary)
# cod.arg = 'Congestive heart failure; nonhypertensive' ; cod.arg = gsub(" ", "_", cod.arg)

# ccs names (temporarily keeping just 10 causes of death which we thought might be interesting)
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)
# cods = ccs.names$full_name
cods.1 = c('Congestive heart failure; nonhypertensive','Pneumonia (except that caused by tuberculosis or sexually transmitted disease)',
        'Coronary atherosclerosis and other heart disease','Cardiac dysrhythmias','Chronic obstructive pulmonary disease and bronchiectasis',
        'Acute myocardial infarction','Septicemia (except in labor)','Acute cerebrovascular disease',
        'Fracture of neck of femur (hip)','Respiratory failure; insufficiency; arrest (adult)')
cods.2 = c('Fluid and electrolyte disorders','Syncope',
        'Secondary malignancies','Cancer of bronchus; lung','Cancer of colon',
        'Other connective tissue disease','Cancer of pancreas','Alcohol-related disorders',
        'Hemorrhoids','Ovarian cyst')
cods = c(cods.1,cods.2)
cod.arg = cods[seedVal] ; cod.arg = gsub(" ", "_", cod.arg)
print(cod.arg)

# output directory
# dir.output.local = paste0('~/data/morbidity/US/medicare/results/model_run/wind_events/')
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/')

# temporary to print results
for(cod.arg in cods){
    print(cod.arg)
    # load model results
    mod = readRDS(paste0(dir.output,'medicare_',gsub(" ", "_", cod.arg),'_model_output_',years[1],'_',years[length(years)],'.rds'))

    # provide summary
    # print(summary(mod))
    event.exp = exp(summary(mod)$coefficients[1])

    # print(exp(confint.default(mod)))

    print(paste0('Wind event RR is ',round(event.exp,2),' (',round(exp(confint.default(mod))[1],2),', ',round(exp(confint.default(mod))[8],2),')'))

}