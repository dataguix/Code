###################################################################
##############BOOTSTRAP############################################
###################################################################


library(gamlr)
library(distrom)

#On importe 
speaker_metadata = read.csv('metadata_cylindre.csv')
C= read.csv('C_with_metadata.csv')
"C = C[C$metadata_id %in% speaker_metadata$metadata_id,]
C = C[!duplicated(C$metadata_id),]
liste_zero = colnames(C)[colSums(C) == 0]
C = C[ , -which(names(C) %in% liste_zero)]"
liste_zero = rownames(C)[rowSums(C[,-which(names(C) %in% c('X','metadata_id','Freq.y'))]) == 0]

#######################################On créé les échantillons bootstrap
name = c(unique(as.character(speaker_metadata$CIK)))


for(boot in 1:200){
  print(boot)
  sample_boot = sample(name,size=length(name),replace = T)
  sample_count = as.data.frame(table(sample_boot))
  
  
  speaker_metadata_boot = merge(speaker_metadata,sample_count,by.x = 'CIK',by.y='sample_boot')
  metadata_id = speaker_metadata_boot[,c(17,19)]
  rownames(speaker_metadata_boot) = speaker_metadata_boot$metadata_id
  speaker_metadata_boot = speaker_metadata_boot[rep(seq_len(nrow(speaker_metadata_boot)),speaker_metadata_boot$Freq.y),]
  speaker_metadata_boot$CIK = as.factor(speaker_metadata_boot$CIK)
  speaker_metadata_boot$annee = as.factor(speaker_metadata_boot$annee)
  
  
  
  C_boot = merge(C,metadata_id,by.x = 'metadata_id',by.y = 'metadata_id')
  rownames(C_boot) = C_boot$metadata_id
  C_boot = C_boot[rep(seq_len(nrow(C_boot)),C_boot$Freq.y),]
  C_boot = C_boot[ , -which(names(C_boot) %in% c('X','metadata_id','Freq.y'))]
  liste_zero = colnames(C_boot)[colSums(C_boot) == 0]
  C_boot = C_boot[ , -which(names(C_boot) %in% liste_zero)]
  C_boot =as.matrix(C_boot)
  C_boot <- as(C_boot, "dgCMatrix")
  
  print('Exclusion des mots non prononcés')
  
  ##########################################On estime le modèle 
  
  
  
  print('Initiation du modèle')
  
  X           <- sparse.model.matrix(~  CIK +Size +annee,data = speaker_metadata_boot)
  qx          <- qr(as.matrix(X))
  X           <- X[, qx$pivot[1:qx$rank]]
  rownames(X) <- rownames(speaker_metadata_boot)
  X         <- X[rownames(C_boot), ]
  print('X!')
  
  #Nasdaq*time
  R2           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R2           <- R2 * speaker_metadata_boot$Indice_bourse
  colnames(R2) <- paste(colnames(R2), 'Nasdaq', sep = '_')
  rownames(R2) <- rownames(speaker_metadata_boot)
  R2           <- R2[rownames(C_boot), ]
  names(speaker_metadata_boot)
  
  #sector*time (ref = consumer_cyclcial, ie R7)
  R3           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R3           <- R3  * speaker_metadata_boot$Basic.Materials
  colnames(R3) <- paste(colnames(R3), 'BM', sep = '_')
  rownames(R3) <- rownames(speaker_metadata_boot)
  R3           <- R3[rownames(C_boot), ]
  
  
  R4           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R4           <- R4  * speaker_metadata_boot$Technology
  colnames(R4) <- paste(colnames(R4), 'Technology', sep = '_')
  rownames(R4) <- rownames(speaker_metadata_boot)
  R4           <- R4[rownames(C_boot), ]
  
  R5           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R5           <- R5  * speaker_metadata_boot$Healthcare
  colnames(R5) <- paste(colnames(R5), 'healthcare', sep = '_')
  rownames(R5) <- rownames(speaker_metadata_boot)
  R5           <- R5[rownames(C_boot), ]
  
  names(speaker_metadata_boot)
  R6           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R6           <- R6  * speaker_metadata_boot$Financials
  colnames(R6) <- paste(colnames(R6), 'Financials', sep = '_')
  rownames(R6) <- rownames(speaker_metadata_boot)
  R6           <- R6[rownames(C_boot), ]
  
  
  R8           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R8           <- R8  * speaker_metadata_boot$Consumer.Non.Cyclicals
  colnames(R8) <- paste(colnames(R8), 'Consumer.Non.Cyclicals', sep = '_')
  rownames(R8) <- rownames(speaker_metadata_boot)
  R8           <- R8[rownames(C_boot), ]
  
  
  R12           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R12           <- R12  * speaker_metadata_boot$Telecommunication.Services
  colnames(R12) <- paste(colnames(R12), 'Telecommunication.Services', sep = '_')
  rownames(R12) <- rownames(speaker_metadata_boot)
  R12           <- R12[rownames(C_boot), ]
  
  
  R9           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R9           <- R9  * speaker_metadata_boot$Utilities
  colnames(R9) <- paste(colnames(R9), 'Utilities', sep = '_')
  rownames(R9) <- rownames(speaker_metadata_boot)
  R9           <- R9[rownames(C_boot), ]
  
  
  R10           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R10           <- R10  * speaker_metadata_boot$Industrials
  colnames(R10) <- paste(colnames(R10), 'Industrials', sep = '_')
  rownames(R10) <- rownames(speaker_metadata_boot)
  R10           <- R10[rownames(C_boot), ]
  
  R11           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R11           <- R11  * speaker_metadata_boot$Energy
  colnames(R11) <- paste(colnames(R11), 'Energy', sep = '_')
  rownames(R11) <- rownames(speaker_metadata_boot)
  R11           <- R11[rownames(C_boot), ]
  
  
  
  #sector * nasdaq * time (ref R7)
  
  R3N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R3N           <- R3N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Basic.Materials
  colnames(R3N) <- paste(colnames(R3N), 'Nasdaq_BM', sep = '_')
  rownames(R3N) <- rownames(speaker_metadata_boot)
  R3N           <- R3N[rownames(C_boot), ]
  
  
  R4N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R4N           <- R4N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Technology
  colnames(R4N) <- paste(colnames(R4N), 'Nasdaq_Technology', sep = '_')
  rownames(R4N) <- rownames(speaker_metadata_boot)
  R4N           <- R4N[rownames(C_boot), ]
  
  R5N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R5N           <- R5N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Healthcare
  colnames(R5N) <- paste(colnames(R5N), 'Nasdaq_healthcare', sep = '_')
  rownames(R5N) <- rownames(speaker_metadata_boot)
  R5N           <- R5N[rownames(C_boot), ]
  
  R6N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R6N           <- R6N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Financials
  colnames(R6N) <- paste(colnames(R6N), 'Nasdaq_Financials', sep = '_')
  rownames(R6N) <- rownames(speaker_metadata_boot)
  R6N           <- R6N[rownames(C_boot), ]
  
  
  
  R8N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R8N           <- R8N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Consumer.Non.Cyclicals
  colnames(R8N) <- paste(colnames(R8N), 'Nasdaq_Consumer.Non.Cyclicals', sep = '_')
  rownames(R8N) <- rownames(speaker_metadata_boot)
  R8N           <- R8N[rownames(C_boot), ]
  
  
  R12N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R12N           <- R12N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Telecommunication.Services
  colnames(R12N) <- paste(colnames(R12N), 'Nasdaq_Telecommunication.Services', sep = '_')
  rownames(R12N) <- rownames(speaker_metadata_boot)
  R12N           <- R12N[rownames(C_boot), ]
  
  
  R9N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R9N           <- R9N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Utilities
  colnames(R9N) <- paste(colnames(R9N), 'Nasdaq_Utilities', sep = '_')
  rownames(R9N) <- rownames(speaker_metadata_boot)
  R9N           <- R9N[rownames(C_boot), ]
  
  
  R10N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R10N           <- R10N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Industrials
  colnames(R10N) <- paste(colnames(R10N), 'Nasdaq_Industrials', sep = '_')
  rownames(R10N) <- rownames(speaker_metadata_boot)
  R10N           <- R10N[rownames(C_boot), ]
  
  R11N           <- sparse.model.matrix(~ 0 + annee, data = speaker_metadata_boot)
  R11N           <- R11N * speaker_metadata_boot$Indice_bourse * speaker_metadata_boot$Energy
  colnames(R11N) <- paste(colnames(R11N), 'Nasdaq_Energy', sep = '_')
  rownames(R11N) <- rownames(speaker_metadata_boot)
  R11N           <- R11N[rownames(C_boot), ]
  
  #mu
  mu2 <- log(rowSums(C_boot))
  
  
  print('Estimation du modèle')
  #estimation
  start = Sys.time()
  cl <- makeCluster(15, type = ifelse(.Platform$OS.type == 'unix', 'FORK', 'PSOCK')) 
  fit <- dmr(
    cl = cl, 
    covars = cbind(X,R3,R4,R5,R6,R8,R9,R10,R11,R12,
                   R3N,R4N,R5N,R6N,R8N,R9N,R10N,R11N,R12N,
                   R2),
    counts = C_boot, 
    mu = mu2,
    free = 1:(ncol(X)+ncol(R3)+ncol(R4)+ncol(R5)+ncol(R6)+ncol(R8)+ncol(R9)+ncol(R10)+ncol(R11)+ncol(R12)+
                ncol(R3N)+ncol(R4N)+ncol(R5N)+ncol(R6N)+ncol(R8N)+ncol(R9N)+ncol(R10N)+ncol(R11N)+ncol(R12N)),
    fixedcost = 1e-2,
    lambda.start =Inf,
    lambda.min.ratio =1e-5,
    nlambda = 100,
    standardize = F
  )
  stopCluster(cl)
  ####################################################On récupère les coefficients
  print(paste("TARATTTAAAAAA...Modèle estimé en ",paste(toString(Sys.time()-start),' minutes')))
  coefs = coef(fit, k = log(nrow(X)),corrected = F)
  coefs_intercept = coefs['intercept',]
  coefs_intercept = as.matrix(coefs_intercept)
  coefs_intercept2 = t(coefs_intercept)
  
  
  coefs_X = coefs[colnames(X),]
  coefs_R2 = coefs[colnames(R2),]  
  coefs_R3 = coefs[colnames(R3),]  
  coefs_R4= coefs[colnames(R4),]  
  coefs_R5 = coefs[colnames(R5),]  
  coefs_R6 = coefs[colnames(R6),]  
  coefs_R8 = coefs[colnames(R8),]  
  coefs_R9 = coefs[colnames(R9),]  
  coefs_R10 = coefs[colnames(R10),]  
  coefs_R11 = coefs[colnames(R11),]  
  coefs_R12 = coefs[colnames(R12),]  
  
  
  coefs_R3N = coefs[colnames(R3N),]  
  coefs_R4N= coefs[colnames(R4N),]  
  coefs_R5N = coefs[colnames(R5N),]  
  coefs_R6N = coefs[colnames(R6N),]  
  coefs_R8N = coefs[colnames(R8N),]  
  coefs_R9N = coefs[colnames(R9N),]  
  coefs_R10N = coefs[colnames(R10N),]  
  coefs_R11N = coefs[colnames(R11N),]  
  coefs_R12N = coefs[colnames(R12N),]  
  
  
  #############################################On calcule l'utilité du NYSE et du NASDAQ (la vraie et la simulée)
  phi =do.call(cbind, list(R2,R3N,R4N,
                           R5N,R6N,R8N,R9N,
                           R10N,R11N,R12N)) %*% do.call(rbind,list(coefs_R2, coefs_R3N,coefs_R4N,coefs_R5N,coefs_R6N,
                                                                   coefs_R8N ,coefs_R9N , coefs_R10N , coefs_R11N , coefs_R12N))
  
  
  
  
  utility_nyse =  do.call(cbind, list(1,X,R3,R4,
                                      R5,R6,R8,R9,
                                      R10,R11,R12))%*%
    do.call(rbind, list(coefs_intercept2,coefs_X,coefs_R3,coefs_R4,
                        coefs_R5,coefs_R6,coefs_R8,coefs_R9,
                        coefs_R10,coefs_R11,coefs_R12))
  
  
  ncol_C = ncol(C_boot)
  
  nasdaq_matrix = replicate(ncol_C ,rowSums(R2))
  nasdaq_matrix_clone = (1-nasdaq_matrix)
  
  real_utility = utility_nyse + nasdaq_matrix*phi
  simulate_utility = utility_nyse + nasdaq_matrix_clone*phi
  
  
  
  ###########################################################On passe aux probabilités
  
  real_q = exp(real_utility)/rowSums(exp(real_utility))
  simulate_q = exp(simulate_utility)/rowSums(exp(simulate_utility))
  
  print('Probabilité calculées')
  
  #########################################################On mesure la distance
  matrice_diff = real_q - simulate_q
  matrice_diff = matrice_diff^2
  dc = rowSums(matrice_diff)
  id = rownames(matrice_diff)
  df = as.data.frame(cbind(id,dc))
  df$dc = as.character(df$dc)
  df$dc = as.numeric(df$dc)
  
  
  ########################################################On passe à la moyenne
  distance = c(rep(NA,14))
  distance_techno = c(rep(NA,14))
  distance_utility= c(rep(NA,14))
  distance_consumer_cyc= c(rep(NA,14))
  distance_consumer_non_cyc= c(rep(NA,14))
  distance_energy= c(rep(NA,14))
  distance_telecom= c(rep(NA,14))
  distance_bm= c(rep(NA,14))
  distance_financial= c(rep(NA,14))
  distance_health= c(rep(NA,14))
  distance_industrial= c(rep(NA,14))
  
  for (y in 2004:2017){
    liste_id_year = speaker_metadata$metadata_id[speaker_metadata$annee==y]
    dc_mean = mean(df$dc[df$id %in% liste_id_year])
    print((dc_mean))
    distance[y-2003] = dc_mean
    
    liste_id_year_techno = speaker_metadata$metadata_id[(speaker_metadata$annee==y) & speaker_metadata_boot$Technology == 1]
    dc_mean_techno = mean(df$dc[df$id %in% liste_id_year_techno])
    distance_techno[y-2003] = dc_mean_techno
    
    liste_id_year_bm = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Basic.Materials == 1]
    dc_mean_bm = mean(df$dc[df$id %in% liste_id_year_bm])
    distance_bm[y-2003]=dc_mean_bm 
    
    liste_id_year_consumer_cyc = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Consumer.Cyclicals == 1]
    dc_mean_cyc = mean(df$dc[df$id %in% liste_id_year_consumer_cyc])
    distance_consumer_cyc[y-2003]= dc_mean_cyc 
    
    liste_id_year_consumer_non_cyc = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Consumer.Non.Cyclicals == 1]
    dc_mean_non_cyc = mean(df$dc[df$id %in% liste_id_year_consumer_non_cyc])
    distance_consumer_non_cyc[y-2003] = dc_mean_non_cyc
    
    liste_id_year_health = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Healthcare== 1]
    dc_mean_health = mean(df$dc[df$id %in% liste_id_year_health])
    distance_health[y-2003] = dc_mean_health
    
    
    liste_id_year_utility = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Utilities == 1]
    dc_mean_utility = mean(df$dc[df$id %in% liste_id_year_utility])
    distance_utility[y-2003] = dc_mean_utility 
    
    liste_id_year_telecom = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Telecommunication.Services == 1]
    dc_mean_telecom = mean(df$dc[df$id %in% liste_id_year_telecom])
    distance_telecom[y-2003] =  dc_mean_telecom
    
    
    liste_id_year_energy = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Energy == 1]
    dc_mean_energy = mean(df$dc[df$id %in% liste_id_year_energy])
    distance_energy[y-2003] = dc_mean_energy
    
    liste_id_year_finance = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Financials == 1]
    dc_mean_finance = mean(df$dc[df$id %in% liste_id_year_finance ])
    distance_financial[y-2003] = dc_mean_finance
    
    liste_id_year_industry = speaker_metadata$metadata_id[speaker_metadata$annee==y & speaker_metadata_boot$Industrials == 1]
    dc_mean_industrial = mean(df$dc[df$id %in% liste_id_year_industry ])
    distance_industrial[y-2003] = dc_mean_industrial
  }
  
  
  
  
  
  if (boot == 1 ){
    distance_boot = as.data.frame(do.call("cbind",list(distance,distance_industrial,distance_health,
                                                       distance_financial,distance_consumer_cyc,
                                                       distance_consumer_non_cyc,distance_energy,
                                                       distance_utility,distance_telecom,
                                                       distance_bm,distance_techno)))
    
    colnames(distance_boot) = c('dc','dc_industrial','dc_health','dc_financial','dc_customer_cyc','dc_customer_non_cyc',
                                'dc_energy','dc_utility','dc_telecom','dc_bm','dc_techno')
  }
  
  
  if(boot != 1){
    distance_boot2 = as.data.frame(do.call("cbind",list(distance,distance_industrial,distance_health,
                                                        distance_financial,distance_consumer_cyc,
                                                        distance_consumer_non_cyc,distance_energy,
                                                        distance_utility,distance_telecom,
                                                        distance_bm,distance_techno)))
    colnames(distance_boot2) = c(paste('dc',toString(boot),sep='_'),
                                 paste('dc_industrial',toString(boot),sep='_'),
                                 paste('dc_health',toString(boot),sep='_'),
                                 paste('dc_financial',toString(boot),sep='_'),
                                 paste('dc_customer_cyc',toString(boot),sep='_'),
                                 paste('dc_customer_non_cyc',toString(boot),sep='_'),
                                 paste('dc_energy',toString(boot),sep='_'),
                                 paste('dc_utility',toString(boot),sep='_'),
                                 paste('dc_telecom',toString(boot),sep='_'),
                                 paste('dc_bm',toString(boot),sep='_'),
                                 paste('dc_techno',toString(boot),sep='_'))
    distance_boot = cbind(distance_boot,distance_boot2)
  }
  
  
  rm(C_boot)
  rm(speaker_metadata_boot)
  
}
write.csv(distance_boot,'D:/home/R550427/coef_model_panel_cylindre/bootstrap_df_final.csv')
"
distance = c(rnorm(14))
distance_industrial = c(rnorm(14))
distance_health = c(rnorm(14))
distance_financial = c(rnorm(14))
distance_consumer_cyc = c(rnorm(14))
distance_consumer_non_cyc = c(rnorm(14))
distance_energy = c(rnorm(14))
distance_utility = c(rnorm(14))
distance_telecom = c(rnorm(14))
distance_bm = c(rnorm(14))
distance_techno = c(rnorm(14))"