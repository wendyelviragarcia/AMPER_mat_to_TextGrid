library(R.matlab)
library(rPraat)
library(stringr)


#create_textgrid_from_mat("xp01kwka1.mat", dict)

extract_vowel_clusters <- function(ipa){
  
  chars <- strsplit(ipa, "")[[1]]
  chars <- chars[chars != " "]
  
  vowels <- c("a","e","i","o","u","ɛ","ɔ","ɪ","ʊ","ə","ɐ")
  diacritics <- "[\\u0300-\\u036F]"
  
  res <- c()
  current <- ""
  stressed <- FALSE
  
  i <- 1
  
  while(i <= length(chars)){
    
    ch <- chars[i]
    
    if(ch == "ˈ"){
      stressed <- TRUE
      i <- i + 1
      next
    }
    
    if(ch %in% vowels){
      
      if(current == ""){
        if(stressed){
          current <- "ˈ"
          stressed <- FALSE
        }
      }
      
      current <- paste0(current, ch)
      
      j <- i + 1
      
      while(j <= length(chars) && grepl(diacritics, chars[j])){
        current <- paste0(current, chars[j])
        j <- j + 1
      }
      
      i <- j
      next
    }
    
    if(nchar(current) > 0){
      res <- c(res, current)
      current <- ""
    }
    
    i <- i + 1
  }
  
  if(nchar(current) > 0)
    res <- c(res, current)
  
  res
}

extract_consonant_clusters <- function(ipa){
  
  chars <- strsplit(ipa, "")[[1]]
  chars <- chars[chars != " "]          # quitar espacios
  chars <- chars[!chars %in% c("ˈ","ˌ")] # quitar acentos
  vowels <- c("a","e","i","o","u","ɛ","ɔ","ɪ","ʊ","ə","ɐ")
  
  diacritics <- "[\\u0300-\\u036F]"
  
  res <- c()
  current <- ""
  
  i <- 1
  
  while(i <= length(chars)){
    ch <- chars[i]
    # añadir diacríticos al símbolo anterior
    if(grepl(diacritics, ch)){
      current <- paste0(current, ch)
      i <- i + 1
      next
    }
    
    if(ch %in% vowels){
      if(nchar(current) > 0){
        res <- c(res, current)
        current <- ""
      }
      
    } else {
      current <- paste0(current, ch)
    }
    
    i <- i + 1
  }
  
  if(nchar(current) > 0)
    res <- c(res, current)
  res
}

create_textgrid_from_mat <- function(myfile, dict = NULL, align_text = FALSE, fs = 16000) {
  #myfile = "wh01kwka1.mat"
  data <- readMat(myfile)
  df <- as.data.frame(data$salida)
  rm(data)
  
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
    
    tg <- tg.insertInterval(
      tg,
      tierInd = 2,
      tStart = df$start_time[i],
      tEnd   = df$end_time[i],
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
    vowels <- c("a","e","i","o","u")
    
    
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
        # último carácter de la cadena IPA
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
  
  
  folder = "T:/AMPER-catala-corpus-Praat/annotacions"
  #treu el segon element d'una cadena de test tipus "ja02tyta.textgrid"
  subfolder <- substr(tools::file_path_sans_ext(myfile), 1, 3)
  
    outdir <- file.path(folder, subfolder)
  
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  outfile <- file.path(outdir, paste0(tools::file_path_sans_ext(basename(myfile)), ".TextGrid"))
  
  invisible(tg.write(tg, outfile))
  
  
}




################## AQUÍ
####
# input el cporpus escrit
# una carpeta o arxiu on hi ha els arxius .mat

#####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
corpus_file <- "./corpora/corpus_AMPER_CAT.txt"
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



folder <- "T:\\AMPER\\AMPERCAT Arxius ordenats\\Barcelona\\Barcelona català\\Barcelona català dona\\Barcelona català corpus fix PART 2 (Programa nou, actualitzat amb els arxius refets pel Paolo)\\ficherosmat"

folder <- gsub("\\\\", "/", folder)
  
  
setwd(folder)
files <- list.files(pattern = "[^0]\\.mat$")
lapply(files, create_textgrid_from_mat, dict = dict, align_text = align_text)
