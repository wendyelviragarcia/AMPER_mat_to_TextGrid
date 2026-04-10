library(R.matlab)
library(rPraat)
library(stringr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

##################
#   
#     VARIABLES PER INDICAR ON TENS ELS ARXIUS I ON VOLS GUARDAR
#
############################

# if you do not have your corpora "la bambina...", leave this blannk and you will get a TextGRid with C V C V C V
corpus_file <- "./corpora/corpus_AMPER_L'Alguer.txt"

#write here the path to your .mat files
folder_input <- "T:/AMPER/AMPERCAT Arxius ordenats/L'Alguer/L'Alguer home/Amper català alguer home/ficherosmat"

#write here the path to the folder where you want to save your files
folder_output <- "T:/AMPER-catala-corpus-Praat/annotacions"




##################
#### comença l'script a partir d'aquí no has de tocar res.



extract_vowel_clusters <- function(ipa) {
  
  ipa <- gsub("\\s+", "", ipa)
  
  vowels <- "aeiouɛɔɪʊəɐi̯u̯"  
  # Una o més vocals seguides, cadascuna amb possibles diacrítics
  vowel_unit <- paste0("[", vowels, "]\\p{M}*")
  vowel_cluster <- paste0("(", vowel_unit, ")+")
  
  # Estrès opcional només al principi del clúster
  pattern <- paste0("ˈ?", vowel_cluster)
  
  matches <- gregexpr(pattern, ipa, perl = TRUE)
  res <- regmatches(ipa, matches)[[1]]
  
  res[res != ""]
}



extract_consonant_clusters <- function(ipa) {
  
  ipa <- gsub("\\s+", "", ipa)
  
  vowels <- "aeiouɛɔɪʊəɐi̯u̯"  
  
  # unitat consonàntica amb diacrítics
  consonant_unit <- paste0("[^", vowels, "]", "\\p{M}*")
  #marques_accent= "ˈˈˌ"
  
  # clúster = una o més consonants seguides
  pattern <- paste0("(", consonant_unit, ")+")
  
  
  matches <- gregexpr(pattern, ipa, perl = TRUE)
  res <- regmatches(ipa, matches)[[1]]
  
  res[res != ""]
}



create_textgrid_from_mat <- function(myfile, folder_output, dict = NULL, align_text = FALSE,  fs = 16000) {
  #myfile = "wh01kwka1.mat"
  data <- readMat(myfile)
  df <- as.data.frame(data$salida)
  rm(data)
  
  
  vowels <- c("a","e","i","o","u","ɛ","ɔ","ɪ","ʊ","ə","ɐ" )
  
  colnames(df) <- c("F01","F02","F03","dur",
                    "I1","I2","I3",
                    "sample1","sample2","sample3","nsample")
  
  df$start_time <- df$sample1 / fs
  df$end_time   <- df$sample3 / fs
  
  total_time <- df$nsample[1] / fs
  
  code <- str_extract(basename(myfile), "[a-z]{4}")
  
  
    tg <- tg.createNewTextGrid(0, total_time)
  
  tg <- tg.insertNewIntervalTier(tg, 1, "sentence")
  
  
  
  
  ##################
  # aliniado
  #############
  tg <- tg.insertNewIntervalTier(tg, 2, "intervals")
  
  
  #creo el textgrid con V para las vocales
  for(i in seq_len(nrow(df))) {
    
    tStart <- df$start_time[i]
    tEnd   <- df$end_time[i]
    
    if (tStart >= tEnd) {
      warning(
        paste0(
          "MAT FILE IS WRONG: Skipping interval in file: ", myfile,
          " | row: ", i,
          " | tStart = ", tStart,
          " | tEnd = ", tEnd
        )
      )
      next
    }
    
    tg <- tg.insertInterval(
      tg,
      tierInd = 2,
      tStart = tStart,
      tEnd   = tEnd,
      label  = "V"
    )
  }
  
  # inserto los intervals del primer tier usando como límite final la última vocal
  # que ha pasado en el bucle. El inicio a veces no es la primera vocal y no tengo boundary
  
  if (align_text) {
    phrase <- dict[code]
    phrase[is.na(phrase)] <- "S"
    
    tg <- tg.insertInterval(
      tg,
      tierInd = 1,
      tStart = df$start_time[1],
      tEnd   = df$end_time[i],
      label  = phrase
    ) 
    
    }else{
      
      tg <- tg.insertInterval(
        tg,
        tierInd = 1,
        tStart = df$start_time[1],
        tEnd   = df$end_time[i],
        label  = code
      ) 
  }
  
  
  
  if(align_text){
    ipa <- dictIPA[code]
    ipa[is.na(ipa)] <- ""
    
    vseq <- extract_vowel_clusters(ipa)
    
    v_index <- 1
    
    startsbyC = 0
    endsbyC = 0
    
    for(interval in seq_along(tg$intervals$label)){
      
      lab <- tg$intervals$label[[interval]]
      
      if(lab == "V" && v_index <= length(vseq)){
        
        tg$intervals$label[[interval]] <- vseq[v_index]
        v_index <- v_index + 1
        
      } else if(lab != "V" && interval == 1){
        
        laprimera <- substr(trimws(ipa), 1, 1)
        
        if(!(laprimera %in% vowels)){
          tg$intervals$label[[interval]] <- "C"
          startsbyC = 1
        }
        
      } else if(lab != "V" && interval == length(tg$intervals$label)){
        #ultim interval
        # últim caràcter de la cadena IPA
        laultima <- substr(trimws(ipa), nchar(trimws(ipa)), nchar(trimws(ipa)))
        
        if(!(laultima %in% vowels)){
          tg$intervals$label[[interval]] <- "C"
          endsbyC = 1
        }
      } else if(lab != "V" &&
                interval > 1 &&
                interval < length(tg$intervals$label)){
        
        tg$intervals$label[[interval]] <- "C"
        
      }
      
    }
    
    
    # si empezaba por C tipo la cítara añado un interval (con tiempo fijo porque no se segmentaba)
    if(startsbyC == 1){
      # este es el interval que tiene C
      # le cambio el inicio
      startVowel = tg$intervals$t1[2]
      
      if (startVowel-0.1 > 0) {
        tg <- tg.insertBoundary(tg, "intervals", startVowel-0.1, "C")
        #esto es para la frase de arriba y no funciona
        tg$intervals$t2[1] <- startVowel-0.05
        tg$intervals$t1[2] <- startVowel-0.05
        tg$intervals$label[1] <- ""
        
        tg$sentence$t2[1] <- startVowel-0.05
        tg$sentence$t1[2] <- startVowel-0.05
        
      } else {
        tg <- tg.insertBoundary(tg, "intervals", 0.01, "C")
        tg$intervals$label[1] <- ""
        
        tg$sentence$t2[1] <- 0.01
        tg$sentence$t1[2] <- 0.01
        
      }
    }
    
    
    
    # si acababa por C tipo la cítara añado un interval (con tiempo fijo porque no se segmentaba)
    if(endsbyC == 1){
      endVowel = tg$intervals$t2[length(tg$intervals$t2)-1]
      dur = tg.getTotalDuration(tg, "intervals")
      
      if (endVowel+0.1 < dur) {
        tg <- tg.insertBoundary(tg, "intervals", endVowel+0.1)
        #esto es para la frase de arriba 
        tg$sentence$t2[2] <- endVowel+0.1
        tg$sentence$t1[3] <- endVowel+0.1
        
        
      } else {
        tg <- tg.insertBoundary(tg, "intervals", dur-0.001)
      }
      
    }
    
    
    # cambia las C por las consonantes del código
    cseq <- extract_consonant_clusters(ipa)
    c_index <- 1
    
    for(consonant in seq_along(tg$intervals$label)){
      lab <- tg$intervals$label[[consonant]]
      
      if(lab == "C" && c_index <= length(cseq)){
        tg$intervals$label[[consonant]] <- cseq[c_index]
        c_index <- c_index + 1
        
      }
  }
    
  }
  #outfile <- paste0(tools::file_path_sans_ext(myfile), ".TextGrid")
  
  #treu el segon element d'una cadena de test tipus "ja02tyta.textgrid"
  subfolder <- substr(tools::file_path_sans_ext(myfile), 1, 3)
  
    outdir <- file.path(folder_output, subfolder)
  
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  outfile <- file.path(outdir, paste0(tools::file_path_sans_ext(basename(myfile)), ".TextGrid"))
  invisible(tg.write(tg, outfile))
  
  
}




################## AQUÍ
##comença el codi per cridar funcions
#####


align_text <- FALSE

if (file.exists(corpus_file)) {
  sentences <- read.delim(corpus_file, stringsAsFactors = FALSE)
  dict <- setNames(sentences$orthography, sentences$code)
  dictIPA <- setNames(sentences$IPA, sentences$code)
  align_text <- TRUE
  
} else {
  dict <- NULL
  align_text <- FALSE
}


setwd(folder_input)
files <- list.files(pattern = "[^0]\\.mat$")
lapply(files, create_textgrid_from_mat, dict = dict, align_text = align_text, folder_output= folder_output)




