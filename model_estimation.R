#####################################################################
#################Distance estimation ################################
#####################################################################

metadata = read.csv('/metadata.csv') #Metadata (industry, year, etc.)
C= read.csv('/C_bdd.csv')  #dataset of words

#from dataset of words to matrix of words
C = C[C$metadata_id %in% speaker_metadata$metadata_id,]
C = C[!duplicated(C$metadata_id),]
peaker_metadata = metadata[-3893,]
metadata =metadata[metadata$metadata_id %in% C$metadata_id,]
C = C[!duplicated(C$metadata_id),]
C = C[C$metadata_id %in% metadata$metadata_id,]
metadata = metadata[!duplicated(metadata$metadata_id),]
metadata$annee = factor(metadata$annee)
metadata$CIK = factor(metadata$CIK)
metadata =metadata[metadata$metadata_id %in% C$metadata_id,]

#exclude html balise
C = C[ , -which(names(C) %in% c("windowtext","msonorm",'xfeff','nowrap','middot','font','efff',
                                'bgcolor','cell', 'tablehang', 'medium', 'upfront', 'middot',
                                'display', 'href', 'nowrap', 'class', 'cellpad', 'lsquo', 'efff',
                                'ound', 'autospac', 'wrap'))]




liste_zero = rownames(C)[rowSums(C[ , -which(names(C) %in% c("X","metadata_id"))]) == 0]
C = C[-which(rownames(C) %in% liste_zero),]
metadata =metadata[metadata$metadata_id %in% C$metadata_id,]
library(Matrix)
C =as.matrix(C)
rownames(C) = names_c
C <- as(C, "dgCMatrix")


################################################UTILITY SPECIFICATION#################################################
######################################################################################################################



#Fixed Effect
X           <- sparse.model.matrix(~  CIK +Size +annee,data = metadata)
qx          <- qr(as.matrix(X))
X           <- X[, qx$pivot[1:qx$rank]]
rownames(X) <- metadata$metadata_id
X         <- X[rownames(C), ]


#Nasdaq*time
R2           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R2           <- R2 * metadata$Indice_bourse
colnames(R2) <- paste(colnames(R2), 'Nasdaq', sep = '_')
rownames(R2) <- metadata$metadata_id
R2           <- R2[rownames(C), ]
names(metadata)
R2

#sector*time (ref = consumer_cyclical, ie R7)
R3           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R3           <- R3  * metadata$Basic.Materials
colnames(R3) <- paste(colnames(R3), 'BM', sep = '_')
rownames(R3) <- metadata$metadata_id
R3           <- R3[rownames(C), ]


R4           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R4           <- R4  * metadata$Technology
colnames(R4) <- paste(colnames(R4), 'Technology', sep = '_')
rownames(R4) <- metadata$metadata_id
R4           <- R4[rownames(C), ]

R5           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R5           <- R5  * metadata$Healthcare
colnames(R5) <- paste(colnames(R5), 'healthcare', sep = '_')
rownames(R5) <- metadata$metadata_id
R5           <- R5[rownames(C), ]

names(metadata)
R6           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R6           <- R6  * metadata$Financials
colnames(R6) <- paste(colnames(R6), 'Financials', sep = '_')
rownames(R6) <- metadata$metadata_id
R6           <- R6[rownames(C), ]
"""
R7           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R7           <- R7  * metadata$Consumer.Cyclicals
colnames(R7) <- paste(colnames(R7), 'Consumer.Cyclicals', sep = '_')
rownames(R7) <- metadata$metadata_id
R7           <- R7[rownames(C), ]
"""
R8           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R8           <- R8  * metadata$Consumer.Non.Cyclicals
colnames(R8) <- paste(colnames(R8), 'Consumer.Non.Cyclicals', sep = '_')
rownames(R8) <- metadata$metadata_id
R8           <- R8[rownames(C), ]


R12           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R12           <- R12  * metadata$Telecommunication.Services
colnames(R12) <- paste(colnames(R12), 'Telecommunication.Services', sep = '_')
rownames(R12) <- metadata$metadata_id
R12           <- R12[rownames(C), ]


R9           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R9           <- R9  * metadata$Utilities
colnames(R9) <- paste(colnames(R9), 'Utilities', sep = '_')
rownames(R9) <- metadata$metadata_id
R9           <- R9[rownames(C), ]


R10           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R10           <- R10  * metadata$Industrials
colnames(R10) <- paste(colnames(R10), 'Industrials', sep = '_')
rownames(R10) <- metadata$metadata_id
R10           <- R10[rownames(C), ]

R11           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R11           <- R11  * metadata$Energy
colnames(R11) <- paste(colnames(R11), 'Energy', sep = '_')
rownames(R11) <- metadata$metadata_id
R11           <- R11[rownames(C), ]



#sector * nasdaq * time (ref R7)

R3N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R3N           <- R3N * metadata$Indice_bourse * metadata$Basic.Materials
colnames(R3N) <- paste(colnames(R3N), 'Nasdaq_BM', sep = '_')
rownames(R3N) <- metadata$metadata_id
R3N           <- R3N[rownames(C), ]


R4N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R4N           <- R4N * metadata$Indice_bourse * metadata$Technology
colnames(R4N) <- paste(colnames(R4N), 'Nasdaq_Technology', sep = '_')
rownames(R4N) <- metadata$metadata_id
R4N           <- R4N[rownames(C), ]

R5N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R5N           <- R5N * metadata$Indice_bourse * metadata$Healthcare
colnames(R5N) <- paste(colnames(R5N), 'Nasdaq_healthcare', sep = '_')
rownames(R5N) <- metadata$metadata_id
R5N           <- R5N[rownames(C), ]

R6N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R6N           <- R6N * metadata$Indice_bourse * metadata$Financials
colnames(R6N) <- paste(colnames(R6N), 'Nasdaq_Financials', sep = '_')
rownames(R6N) <- metadata$metadata_id
R6N           <- R6N[rownames(C), ]
"""
R7           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R7           <- R7 * metadata$Indice_bourse * metadata$Consumer.Cyclicals
colnames(R7) <- paste(colnames(R7), 'Nasdaq_Consumer.Cyclicals', sep = '_')
rownames(R7) <- metadata$metadata_id
R7           <- R7[rownames(C), ]
"""
R8N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R8N           <- R8N * metadata$Indice_bourse * metadata$Consumer.Non.Cyclicals
colnames(R8N) <- paste(colnames(R8N), 'Nasdaq_Consumer.Non.Cyclicals', sep = '_')
rownames(R8N) <- metadata$metadata_id
R8N           <- R8N[rownames(C), ]


R12N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R12N           <- R12N * metadata$Indice_bourse * metadata$Telecommunication.Services
colnames(R12N) <- paste(colnames(R12N), 'Nasdaq_Telecommunication.Services', sep = '_')
rownames(R12N) <- metadata$metadata_id
R12N           <- R12N[rownames(C), ]


R9N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R9N           <- R9N * metadata$Indice_bourse * metadata$Utilities
colnames(R9N) <- paste(colnames(R9N), 'Nasdaq_Utilities', sep = '_')
rownames(R9N) <- metadata$metadata_id
R9N           <- R9N[rownames(C), ]


R10N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R10N           <- R10N * metadata$Indice_bourse * metadata$Industrials
colnames(R10N) <- paste(colnames(R10N), 'Nasdaq_Industrials', sep = '_')
rownames(R10N) <- metadata$metadata_id
R10N           <- R10N[rownames(C), ]

R11N           <- sparse.model.matrix(~ 0 + annee, data = metadata)
R11N           <- R11N * metadata$Indice_bourse * metadata$Energy
colnames(R11N) <- paste(colnames(R11N), 'Nasdaq_Energy', sep = '_')
rownames(R11N) <- metadata$metadata_id
R11N           <- R11N[rownames(C), ]



#Poisson multinomial scaling factor
mu2 <- log(rowSums(C))




############################################ESTIMATION##################################################
########################################################################################################


cl <- makeCluster(detectCores()-1, type = ifelse(.Platform$OS.type == 'unix', 'FORK', 'PSOCK')) #multi-processing
fit <- dmr(
  cl = cl, 
  covars = cbind(X,R3,R4,R5,R6,R8,R9,R10,R11,R12,
                 R3N,R4N,R5N,R6N,R8N,R9N,R10N,R11N,R12N,
                 R2),
  counts = C, 
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


#Get coefficients
coefs = coef(fit, k = log(nrow(X)),corrected = F)


coefs_intercept = coefs['intercept',]
coefs_intercept2 = as.data.frame(as.matrix(coefs_intercept))

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



coefs_X2 = as.data.frame(as.matrix(coefs_X))
coefs_R22 = as.data.frame(as.matrix(coefs_R2))
coefs_R32 = as.data.frame(as.matrix(coefs_R3))
coefs_R42 = as.data.frame(as.matrix(coefs_R4))
coefs_R52 = as.data.frame(as.matrix(coefs_R5))
coefs_R62 = as.data.frame(as.matrix(coefs_R6))
coefs_R82 = as.data.frame(as.matrix(coefs_R8))
coefs_R92 = as.data.frame(as.matrix(coefs_R9))
coefs_R102 = as.data.frame(as.matrix(coefs_R10))
coefs_R112 = as.data.frame(as.matrix(coefs_R11))
coefs_R122 = as.data.frame(as.matrix(coefs_R12))



coefs_R3N2 = as.data.frame(as.matrix(coefs_R3N))
coefs_R4N2 = as.data.frame(as.matrix(coefs_R4N))
coefs_R5N2 = as.data.frame(as.matrix(coefs_R5N))
coefs_R6N2 = as.data.frame(as.matrix(coefs_R6N))
coefs_R8N2 = as.data.frame(as.matrix(coefs_R8N))
coefs_R9N2 = as.data.frame(as.matrix(coefs_R9N))
coefs_R10N2 = as.data.frame(as.matrix(coefs_R10N))
coefs_R11N2 = as.data.frame(as.matrix(coefs_R11N))
coefs_R12N2 = as.data.frame(as.matrix(coefs_R12N))



####################################################################################################
#########################COMPUTE UTILITY############################################################

#Marginal increase of utility of being in the nasdaq
phi =do.call(cbind, list(R2,R3N,R4N,
                         R5N,R6N,R8N,R9N,
                         R10N,R11N,R12N)) %*% do.call(rbind,list(coefs_R2, coefs_R3N,coefs_R4N,coefs_R5N,coefs_R6N,
                                                                 coefs_R8N ,coefs_R9N , coefs_R10N , coefs_R11N , coefs_R12N))



#utility Nyse
utility_nyse =  do.call(cbind, list(1,X,R3,R4,
                                    R5,R6,R8,R9,
                                    R10,R11,R12))%*%
  do.call(rbind, list(coefs_intercept2,coefs_X,coefs_R3,coefs_R4,
                      coefs_R5,coefs_R6,coefs_R8,coefs_R9,
                      coefs_R10,coefs_R11,coefs_R12))


nasdaq_matrix = replicate(ncol_C ,rowSums(R2))
nasdaq_matrix_clone = (1-nasdaq_matrix)

real_utility = utility_nyse + nasdaq_matrix*phi
simulate_utility = utility_nyse + nasdaq_matrix_clone*phi



#################COMPUTE PROBABILITY####################################################################
########################################################################################################


real_q = exp(real_utility)/rowSums(exp(real_utility))
simulate_q = exp(simulate_utility)/rowSums(exp(simulate_utility))


##################COMPUTE DISTANCE######################################################################
########################################################################################################


matrice_diff = real_q - simulate_q
matrice_diff = matrice_diff^2
dc = rowSums(matrice_diff) #dc = communicational distance
id = rownames(matrice_diff)
df = as.data.frame(cbind(id,dc))
df$dc = as.character(df$dc)
df$dc = as.numeric(df$dc)


