# Checklist per il run dello script main_dineof_1.5.R.

#-------------------------------------------------------------------------------
# 1. Setting impostazioni di interpolazione
#-------------------------------------------------------------------------------

# Fissare i parametri di interesse per il run dello script.

# Tolleranza algoritmo "dineof"
# DINEOF_PRECISION <- 1e-3

# Metodo interpolativo utilizzato da mice::mice (vedi documentazione)
# MICE_METHOD <- "mean"

# Seed per mice
# MICE_SEED <- 100

# Numero massimo di iterazioni effettuate da mice
# MICE_MAXIT <- 20

#-------------------------------------------------------------------------------
# 2. Setting delle cartelle di INPUT
#-------------------------------------------------------------------------------

# Impostare i dati relativi all'input.

# INPUT

# Cartella dati .nc da interpolare
# NC_DA_INTERP <- "C:/users/michy/desktop/chr/DATA/CHL_1D_SMALL"

# Nome variabile da interpolare
# VAR_DA_INTERP <- "CHL1_mean"

# Si deve utilizzare l'informazione relativa al ghiaccio?
# USE_ICE <- TRUE

# Cartella dati .nc GHIACCIO (utile solo se USE_ICE <- TRUE)
# NC_ICE <- "C:/users/michy/desktop/chr/DATA/SIC_1D_SMALL"

# Nome variabile GHIACCIO (utile solo se USE_ICE <- TRUE)
# VAR_ICE <- "ice"

# Soglia massima ghiaccio. Pixel con ice > 0.15 sono fissati a NA a un certo punto del processo
# (utile solo se USE_ICE <- TRUE)
# ICE_THRESHOLD <- 0.15

#-------------------------------------------------------------------------------
# 3. Setting delle cartelle di OUTPUT
#-------------------------------------------------------------------------------

# Impostare i dati relativi all'output.

# Cartella dove andranno salvati i file .nc di output
# OUTPUT_DIR <- "C:/users/michy/desktop/chr/DATA/out"

# Prefisso nuovi file .nc di output
# NEW_NC_PREFIX <- "L3m_"

# Suffisso nuovi file .nc di output
# NEW_NC_SUFFIX <- "__DINEOF_CHL_1D"

# Nome della variabile salvata nei nuovi file .nc
# SAVED_INTERP_VAR_NAME <- "CHL1_intp"

#-------------------------------------------------------------------------------
# 4. Setting degli SPLIT
#-------------------------------------------------------------------------------

# Impostare i parametri con cui effettuare i due split.

# Cut-off primo split per pixel in % (mantengo solo i record con % pixel per immagine > soglia)
# PIXEL_THRESHOLD_PERCENTAGE <- 0

# Cut-off secondo split per date in % (mantengo solo i record con % pixel per data > soglia)
# DATE_THRESHOLD_PERCENTAGE <- 10

#-------------------------------------------------------------------------------
# Setting per rimozione outliers
#-------------------------------------------------------------------------------

# Impostare parametro per effettuare squish. Lo squish dei valori estremi è effettuato
# a livello di immagine.

# Percentili entro i quali "squishare" i valori estremi della variabile da interpolare
# PERCENTILE_SQUISHING_INTERVAL <- c(0.05, 0.95)

#-------------------------------------------------------------------------------
# 5. Setting per statistica descrittiva
#-------------------------------------------------------------------------------

# Impostare le statistiche descrittive che si desidera ottenere.

# Nota: Le funzioni scelte devono accettare l'argomento na.rm=T
# STAT_FUNS <- c("mean", "median", "sd", "IQR", "mad")

# Salvare le statistiche calcolate in un .csv chiamato stats.csv?
# SAVE_STATS <- TRUE

################################################################################
################################################################################
# Nota FONDAMENTALE:
################################################################################
################################################################################

# Per ice la longitudine è rimappata come lon = lon - 360
# Latitudine e longitudine devono coincidere sia per il dataframe da interpolare
# che per il dataframe del ghiaccio.

#-------------------------------------------------------------------------------
# 6. Se è tutto ok, eseguire lo script.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 7. Controlli eseguiti dallo script.
#-------------------------------------------------------------------------------

# I seguenti controlli sono effettuati solo in caso si utilizzi anche l'informazione
# relativa al ghiaccio (altrimenti sono inutili).

# I controlli eseguiti dallo script sono i seguenti:

# 1. Controllo dimensioni: i due df (quello del ghiaccio e quello da interpolare)
# devono avere le stesse dimensioni (controllo n righe e n colonne). Ossia,
# lo stesso numero di date deve essere considerato.

# 2. Latitudine e langitudine devono essere uguali nei due dataframe

# 3. Le date devono essere uguali nei due data frame. Ossia, le immagini caricate devono
# essere le stesse.

# Se anche solo un controllo da esito negativo, lo script non viene eseguito.

#-------------------------------------------------------------------------------
# 8. Procedura eseguita dallo script.
#-------------------------------------------------------------------------------

# La procedura implementata dallo script è la seguente:

# 1) Caricamento dati chl e sic se richiesto (o variabile generica e sic se richiesto).

# 2) Veririfiche sui dati indicate al punto 7 di questa "guida".
# Se è tutto ok si procede. Altrimenti STOP.

# 3) Calcolo max della variabile da interpolare.

# 4) "Squish" dei valori estremi della variabile da interpolare all'interno di 
# un intervallo scelto dall'utente in percentili. Lo squish è effettuato per immagine.

# 5) Split by pixel

# 6) Split by date

# 7) Interpolazione dineof

# 8) Se valore interpolato > valore max calcolato a punto 3. Lo fisso a NA.

# 9) Re-interpolo pixel fissati a NA nel punto 8 con MICE e algoritmo a scelta.

# 10) Se si utilizza anche l'informazione relativa al ghiaccio,
# Pixel interpolati con ice > ICE_THRESHOLD sono fissati a NA.

# 11) Calcolo statistiche descrittive

# 12) Calcolo guadagno pixel.

# 13) Calcolo modello di regressione lineare.

# 14) Salvataggio immagini interpolate in formato .nc e salvataggio immagini NON
# interpolate (senza modificarle) in formato .nc rinominando la variabile
# di interesse allo stesso modo della variabile interpolata.

# 15) Summary dataframe ottenuto e modello lineare(var interpolata ~ var da interpolare).


#-------------------------------------------------------------------------------
# 9. Caricamento in R risultati script.
#-------------------------------------------------------------------------------

# E' stato verificato il caricamento dati in R mediante la seguente procedura.

#
# # Test caricamenti dati .nc realizzati
# nc_chl_dataframe <- load_all_as_list(path = "C:/users/michy/desktop/chr/DATA/out",
#                                      variables = c("CHL1_intp"),
#                                      spare_coordinates = c("longitude", "latitude"),
#                                      coordinates = c("lon", "lat")) %>%
#     # Unisco unico df chl. Attenzione che vuole il parametro coordinates.
#     assign_id_and_melt(coordinates=c("longitude", "latitude"))
# 
# nc_chl_dataframe %>% summary()
#
