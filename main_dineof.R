# Script DINEOF V. 1.5

#-------------------------------------------------------------------------------
# Setup

# Clean env
rm(list = ls())
# Set seed for reproducibility purpose
set.seed(5520)

#--------------------------------------
# Interpolators' settings

# Tolerance for convergence of dineof method.
DINEOF_PRECISION <- 1e-5
# Mice interpolating method (check ?mice::mice).
MICE_METHOD <- "mean"
# Mice seed for reproducibility purpose.
MICE_SEED <- 100
# Maximum number of iterations for mice::mice.
MICE_MAXIT <- 10

#--------------------------------------
# INPUT

# Folder containing .nc files to be interpolated
NC_DA_INTERP <- "C:/users/michy/desktop/chr/DATA/CHL_1D_SMALL"
# Variable to be interpolated
VAR_DA_INTERP <- "CHL1_mean"

#--------------------------------------
# ICE data

# Should ICE data be used? If yes, then set to TRUE (otherwise set to FALSE).
USE_ICE <- TRUE
# Folder containing .nc files of GHIACCIO
NC_ICE <- "C:/users/michy/desktop/chr/DATA/SIC_1D_SMALL"
# Variable name for GHIACCIO
VAR_ICE <- "ice"
# Maximum GHIACCIO threshold (pixels with ice > ICE_THRESHOLD will be set to NA after interpolation).
ICE_THRESHOLD <- 0.15

#--------------------------------------
# OUTPUT

# Set OUTPUT folder
OUTPUT_DIR <- "C:/users/michy/desktop/chr/DATA/out"
# Prefix new .nc files
NEW_NC_PREFIX <- "L3m_"
# Suffx new .nc files
NEW_NC_SUFFIX <- "__DINEOF_CHL_1D"
# Name of interpolated variable to be saved in new .nc files
SAVED_INTERP_VAR_NAME <- "CHL1_intp"

#--------------------------------------
# SPLIT INFO

# Cut-off first split by pixel in % (keep only records where % of non NA pixel per image > threshold)
PIXEL_THRESHOLD_PERCENTAGE <- 5
# Cut-off second split by date in % (keep only records where % of non NA pixel per date > threshold)
DATE_THRESHOLD_PERCENTAGE <- 10

#--------------------------------------
# OUTLIERS TREATMENT

# Percentile within the variable to be interpolated is squished before doing the interpolation.
PERCENTILE_SQUISHING_INTERVAL <- c(0.05, 0.95)

#--------------------------------------
# Descriptive statistics to be calculated

# Note: selected functions MUST accept the argument na.rm = T
STAT_FUNS <- c("mean", "median", "sd", "IQR", "mad")
# Save statistics in a file called stats.csv in the output folder?
SAVE_STATS <- TRUE

#--------------------------------------
# Loading packages

# Do not change loading order of raster and dplyr otherwise namespace is messed up.
# Depends also on readr 1.1.1 and broom 0.4.3 not loaded since namespace is already polluted enough.
require(raster)         # 2.6-7
require(qchlorophyll)   # 2.1
require(dplyr)          # 0.7.4
require(lazyeval)       # 0.2.1
require(scales)         # 0.5.0
require(mice)           # 2.46.0

#-------------------------------------------------------------------------------
# START OF THE SCRIPT

# Loading files

# Load df to be interpolated
nc_chl_dataframe <- load_all_as_list(path = NC_DA_INTERP, variables = c(VAR_DA_INTERP)) %>%
    # Bind data in a single df
    assign_id_and_melt() %>%
    # Remove unused variables
    select(-id_date, -month, -year) %>%
    # Add unique id
    mutate(id_unico = row_number())

# Only load ice data if needed
if(USE_ICE)
{
    # Load ice data (sic)
    nc_sic_dataframe <- load_all_as_list(path = NC_ICE, variables = c(VAR_ICE)) %>%
        # Bind data in a single df
        assign_id_and_melt(coordinates = c("longitude", "latitude")) %>%
        # Remove unused variables
        select(-id_date, -month, -year) %>%
        # Rename
        rename(lat = latitude, lon = longitude) %>%
        # Add unique id and set longitude equal to the one in the other df
        mutate(id_unico = row_number(),
               lon = lon - 360)
}

#-------------------------------------------------------------------------------
# Checks

# Checks must be performed only if ice is used

if(USE_ICE)
{
    # Check: dfs must have the same size (check both n of rows and columns)
    checks_vector <- c(dim(nc_chl_dataframe) == dim(nc_sic_dataframe),
                       # Check: dates must be the same
                       isTRUE(all_equal(select(nc_chl_dataframe, date), select(nc_sic_dataframe, date), ignore_row_order = F)),
                       # Check: lon must be the same
                       isTRUE(all_equal(select(nc_chl_dataframe, lon), select(nc_sic_dataframe, lon), ignore_row_order = F)),
                       # Check: lat must be the same
                       isTRUE(all_equal(select(nc_chl_dataframe, lat), select(nc_sic_dataframe, lat), ignore_row_order = F)) )
    
    # Assign names to checks
    names(checks_vector) <- c("Nrows differ",
                              "Ncols differ",
                              "Dates are different",
                              "Lon is different",
                              "Lat is different")   
}else
{
    checks_vector <- TRUE
}

#-------------------------------------------------------------------------------
# If checks are ok, proceed. Otherwise show which error occurred.

if(all(checks_vector))
{
    #-------------------------------------------------------------------------------
    # Add ICE to nc_chl_dataframe (only if ice is used).
    if(USE_ICE)
    {
        print("Adding ICE to main data frame...")
        nc_chl_dataframe <- left_join(nc_chl_dataframe,
                                      nc_sic_dataframe,
                                      by=c("lat", "lon","id_pixel", "date", "id_unico"))
        
        # Clean env
        rm(nc_sic_dataframe); gc()
    }
    
    #-------------------------------------------------------------------------------
    # Calculate max of chl
    var_da_interp_max <- max(nc_chl_dataframe[[VAR_DA_INTERP]], na.rm = TRUE)
    
    #-------------------------------------------------------------------------------
    # Remove extreme values according to given threshold by squishing them into selected percentile interval
    print("Squishing extreme values in selected percentile range...")
    squish_formula <- interp( ~squish(x, quantile(x, y, na.rm=T)),
                             x = as.name(VAR_DA_INTERP),
                             y = PERCENTILE_SQUISHING_INTERVAL)
    nc_chl_dataframe <- nc_chl_dataframe %>%
        mutate_(.dots = setNames(list(interp(~identity(x), x=as.name(VAR_DA_INTERP))), "original_variable")) %>%
        group_by(date) %>%
        mutate_(.dots = setNames(list(squish_formula), VAR_DA_INTERP)) %>%
        ungroup()
    
    rm(squish_formula)
    #-------------------------------------------------------------------------------
    # 1. Split by pixel. Removes also earth pixels.
    
    print("Splitting by id_pixel...")
    pixel_split_formula <- interp( ~sum(!is.na(x))/n()*100, x = as.name(VAR_DA_INTERP))
    
    chl_dataframe <- nc_chl_dataframe %>%
        group_by(id_pixel) %>%
        mutate_(.dots = setNames(list(pixel_split_formula), "percentage_tmp")) %>%
        ungroup() %>%
        filter(percentage_tmp > PIXEL_THRESHOLD_PERCENTAGE) %>%
        select(-percentage_tmp)
    
    rm(pixel_split_formula)
    #-------------------------------------------------------------------------------
    # 2. Split by date.
    
    print("Splitting by date...")
    date_split_formula <- interp( ~sum(!is.na(x))/n()*100, x = as.name(VAR_DA_INTERP))
    
    chl_dataframe <- chl_dataframe %>%
        group_by(date) %>%
        mutate_(.dots = setNames(list(date_split_formula), "percentage_sp")) %>%
        ungroup() %>%
        filter(percentage_sp > DATE_THRESHOLD_PERCENTAGE) %>%
        select(-percentage_sp)
    
    # Collect unique dates being interpolated. These are going to be used later for saving data.
    UNIQUE_DATES_INTERPOLATED <- chl_dataframe %>%
        select(date) %>%
        distinct() %>%
        pull() %>%
        as.character()
    
    rm(date_split_formula)
    #-------------------------------------------------------------------------------
    # Dineof interpolation on chl_dataframe
    
    # From dataframe to matrix (rows = pixels, cols = dates)
    print("Preparing for dineof interpolation: matrix conversion...")
    chla <- chl_dataframe[, c("date", VAR_DA_INTERP)]
    chla <- do.call(rbind, split(chla[[VAR_DA_INTERP]], chla["date"])) %>% t() %>% log()
    
    # Interpolation
    print("Starting dineof interpolation: go take a coffe...")
    chla_dineof <- sinkr::dineof(chla, delta.rms = DINEOF_PRECISION)
    
    # Matrix out
    chla_dineof_out <- exp(chla_dineof$Xa)
    
    # Clear env
    rm(chla, chla_dineof); gc()
    
    #-------------------------------------------------------------------------------
    # Add results to chl_dataframe
    
    # Add results to chl_dataframe
    print("Interpolation ended! Adding interpolated variable to initial dataframe...")
    chl_dataframe <- chl_dataframe %>%
        arrange(date) %>%
        mutate(CHL1_intp = as.numeric(chla_dineof_out)) %>%
        arrange(id_unico)
    
    # Clean env
    rm(chla_dineof_out); gc()
    #-------------------------------------------------------------------------------
    # Remove interpolated values greater than maximum value in the data

    excessive_values_index <- chl_dataframe$CHL1_intp > var_da_interp_max 
    chl_dataframe$CHL1_intp[excessive_values_index] <- NA
    print("Interpolating again the following percentage of pixels (%):")
    # % of pixels to be re-interpolated.
    print(sum(excessive_values_index, na.rm = T)/nrow(chl_dataframe) * 100)
   
    # Only interpolate with mice if needed
    if(sum(excessive_values_index) > 0)
    {
        # Interpolation with mice
        df_to_interpolate <- chl_dataframe %>% select_(VAR_DA_INTERP, "CHL1_intp")
        mice_interpolated <- complete(mice::mice(df_to_interpolate,
                                                 m = 1,
                                                 maxit = MICE_MAXIT,
                                                 meth = MICE_METHOD,
                                                 seed = MICE_SEED),
                                      1) %>%
            as_tibble()
        
        # Replace NA with interpolated values
        chl_dataframe$CHL1_intp[excessive_values_index] <- mice_interpolated$CHL1_intp[excessive_values_index]
        # Clean env
        rm(df_to_interpolate, mice_interpolated); gc()
    }
    #--------------------------------------------------------------------------------
    # Remove pixels with ice > ICE_THRESHOLD (only if ice is used)

    if(USE_ICE)
    {
        print("Removing pixels with ICE greater than maximum admitted...")
        chl_dataframe$CHL1_intp[chl_dataframe[[VAR_ICE]] > ICE_THRESHOLD] <- NA
    }
    
    #-------------------------------------------------------------------------------
    # Add interpolated variable to main df
    
    # Riporto la nuova variabile dal df ridotto al df completo tramite un left_join per id_unico
    print("Adding interpolated variable to initial data frame...")
    nc_chl_dataframe <- chl_dataframe %>%
        select(id_unico, CHL1_intp) %>%
        left_join(nc_chl_dataframe, ., by = c("id_unico"))
    
    # Clean env
    rm(chl_dataframe); gc()
    
    #--------------------------------------------------------------------------------
    # Calculate descriptive statistics
    
    # Descriptive statistics
    stat_descriptive <- nc_chl_dataframe %>%
        summarise_at(.vars = c(VAR_DA_INTERP, "CHL1_intp"), .funs = STAT_FUNS, na.rm=T)
    
    # Number and % of dates/images interpolated
    n_interpolated_dates <- nc_chl_dataframe %>%
        distinct(date) %>%
        summarise(total_images = n()) %>%
        mutate(interpolated_images = length(UNIQUE_DATES_INTERPOLATED),
               percent_img_interpolated = interpolated_images/total_images*100)
    
    # Percentage pixel gain
    pixel_gain_stats <- nc_chl_dataframe %>%
        summarise(pixel_pre_intp = sum(!is.na(CHL1_mean)),
               pixel_post_intp = sum(!is.na(CHL1_intp)),
               percent_increase = (pixel_post_intp - pixel_pre_intp)/(pixel_pre_intp)*100)
    
    # Fitting linear model
    print("Fitting linear model")
    model <- lm(as.formula(paste("CHL1_intp ~ ", VAR_DA_INTERP, sep="")), data=nc_chl_dataframe)
    
    # Bind statistics together
    stats_out <- bind_cols(stat_descriptive, n_interpolated_dates, pixel_gain_stats)
    rm(stat_descriptive, n_interpolated_dates, pixel_gain_stats)
    
    if(SAVE_STATS)
    {
        # Save descriptive stats, linear model results.
        readr::write_csv(stats_out, file.path(OUTPUT_DIR, "stats.csv"))
        readr::write_csv(broom::tidy(model), file.path(OUTPUT_DIR, "lm_coeffs.csv"))
        readr::write_csv(broom::glance(model), file.path(OUTPUT_DIR, "lm_indeces.csv"))
    }
    
    #-------------------------------------------------------------------------------
    # Get the data back to .nc format
    
    print("Proceeding to save data: go to take your second coffe of the day...")
    
    # Select only needed columns.
    # Note: original variable is saved into "original_variable"
    nc_chl_dataframe <- nc_chl_dataframe %>%
        select_("lat", "lon", "date", "original_variable", "CHL1_intp")
    
    # Unique dates/images to be saved
    UNIQUE_DATES <- nc_chl_dataframe %>%
        select(date) %>%
        distinct() %>%
        pull() %>%
        as.character()
    
    # Saving through a classic for loop.
    for(current_date in UNIQUE_DATES)
    {
        if(current_date %in% UNIQUE_DATES_INTERPOLATED)
        {
            # If the date/image was interpolated then chose it from the df.
            var_name <- "CHL1_intp"
        }else
        {
            # If the date/image was NOT interpolated, save the original variable.
            var_name <- "original_variable"
        }
        
        # Select only current date and pull out the data to be saved
        spg <- filter(nc_chl_dataframe, date == current_date) %>%
            select_("lat", "lon", var_name)
        val <- spg %>% select_(var_name) %>% pull()
        
        # Create spatial points data frame
        coordinates(spg) <- ~ lon + lat
        # Coerce to SpatialPixelsDataFrame
        gridded(spg) <- TRUE
        # Coerce to raster
        raster_df <- raster::raster(spg)
        # Projection
        projection(raster_df) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
        # Set values
        raster_df <- setValues(raster_df, val)
        # Write raster
        writeRaster(raster_df,
                    filename = file.path(OUTPUT_DIR,
                                         paste(NEW_NC_PREFIX,
                                               gsub("-", "", current_date),
                                               NEW_NC_SUFFIX,
                                               ".nc",
                                               sep = "")),
                    varname = SAVED_INTERP_VAR_NAME,
                    format = "CDF",
                    overwrite = TRUE)
        # Print info
        print(paste("Saved ", current_date, sep=""))
    }
    
}else
{
    # Something went wrong! Print info on error.
    print(names(which(!checks_vector)))
}

# Take a look at the fitted linear model
summary(model)

# Take a look at the complete dataframe
summary(nc_chl_dataframe)
