ROOT <- "C:/Users/acer/Documents/FIS VŠE/bakaláøská práce/výsledky"
setwd(ROOT)

library(maptools)
library(colorspace)
library(readxl)

#funkce pro hledani cyklu v datech
najdicyklus <- function(mat, obch){
  n = dim(mat)[2]
  RET <- c(1, which(as.logical(mat[1,]))[obch])
  
  delkacyklu=2
  pokracuj = T
  
  while(pokracuj){
    poslmesto <- RET[delkacyklu]
    dalsimesto <- which(as.logical(mat[poslmesto,]))
    RET <- c(RET, dalsimesto)
    delkacyklu <- delkacyklu+1
    pokracuj = (dalsimesto!=1)&(delkacyklu < n+1)
  }
  
  return(RET)
}

## Nacteni hranic kraju a okresu
CZ1 <- readShapePoly("CZE_adm/CZE_adm1")
CZ2 <- readShapePoly("CZE_adm/CZE_adm2")
#warning ignorujeme

# HK zupa
HKzupa <- CZ2@polygons[[23]]@Polygons[[1]]@coords[c(54:276,1:10),]
HKzupa <- rbind(HKzupa, CZ2@polygons[[26]]@Polygons[[1]]@coords[c(297:356,1:254),])
HKzupa <- rbind(HKzupa, HKzupa[1,])

# Krk zupa
Krkzupa <- CZ2@polygons[[25]]@Polygons[[1]]@coords[c(423:514,1:307),]
Krkzupa <- rbind(Krkzupa, CZ2@polygons[[27]]@Polygons[[1]]@coords[c(316:464,1:200),])
Krkzupa <- rbind(Krkzupa, Krkzupa[1,])

# Nacteni souradnic
data <- read_excel("~/FIS VŠE/bakaláøská práce/výsledky/MPL_vysledky/vystupy_x.xlsx",
                         sheet = "proR_KH_PCE")
head(data)
dim(data)
summary(data)
data$zupa <- factor(data$zupa)
      
            
pocet_obch <- 4
metody <- c("MTSP", "MTSP_KaBe", "MTSP_VRP", "MTSP_VRP_KaBe")
hezke_metody <- c("MTSP", "MTSP-KaBe", "MTSP-VRP", "MTSP-VRP-KaBe")
names(hezke_metody) <- metody
pocty_hodin <- list()
#tady vlozit pocty hodin
pocty_hodin[[metody[1]]] <- c(1, 8, 16, 24)
pocty_hodin[[metody[2]]] <- c(1, 8, 16, 24)
pocty_hodin[[metody[3]]] <- c()
pocty_hodin[[metody[4]]] <- c()
#vsechny_hodiny <- unlist(pocty_hodin)
vsechny_hodiny <- c(1, 8, 16, 24)

maK <- grepl("KaBe", metody)
names(maK) <- metody
typ_matice <- c("xx","xx", "xxx", "xxx")
names(typ_matice) <- metody

#tady doplnit hodnoty ucelovych funkci, potreba udrzet rozmery matice
Z <- matrix(c("1072,210", "1039,835", "1015,396", "999,299",
              "1009,170",  "983,509",  "967,398", "957,494",
              "3001,000", "3002,000", "3003,000", "3004,000",
              "4001,000", "4002,000", "4003,000", "4004,000"),
            byrow = T, nrow = length(metody))
rownames(Z) <- metody
colnames(Z) <- paste0(vsechny_hodiny, "h")

# Barvy
COL <- rainbow_hcl(4, c= 70, l =70, alpha = 0.5) # pozadi zup
COL2 <- c("blue", "green4", "red", "coral4") # obchodnici

lCOL2 <- list()
for(metoda in metody){
  lCOL2[[metoda]] <- list()
  for(pocethodin in colnames(Z)){
    lCOL2[[metoda]][[pocethodin]] <- c("blue", "green4", "red", "coral4")
  }
}
# ruèní úprava kvùli zachování barev
lCOL2[["MTSP"]][["24h"]] <- c("coral4", "blue", "green4", "red")
lCOL2[["MTSP_KaBe"]][["1h"]] <- c("red", "green4", "blue", "coral4")
lCOL2[["MTSP_KaBe"]][["16h"]] <- c("blue", "coral4", "green4", "red")

metoda <- "MTSP_KaBe"
pocethodin <- 1
      
for(metoda in metody){
  for(pocethodin in pocty_hodin[[metoda]]){
    matice <- read_excel("~/FIS VŠE/bakaláøská práce/výsledky/MPL_vysledky/vystupy_x.xlsx",
                         sheet = paste0(metoda,"_",pocethodin,"h"))
    
    cairo_pdf(paste0("vykresleni_cest_",metoda,"_",pocethodin,"h.pdf"), 
              width = 6, height = 6)
    {
      par(mfrow = c(1,1), mar = c(0,0,0,0))
      plot(CZ1@polygons[[5]]@Polygons[[1]]@coords, 
           type = "n", xaxt = "n", yaxt = "n", bty = "n",
           xlim = c(15,16.9), ylim = c(49.5, 50.9))
      
      # Pardubice
      polygon(CZ1@polygons[[9]]@Polygons[[1]]@coords, col = COL[2])
      
      # Orlicka zupa
      polygon(HKzupa, col = COL[1])
      
      # Jicinska zupa
      polygon(CZ2@polygons[[24]]@Polygons[[1]]@coords, col = COL[4])
      
      # Krk zupa
      polygon(Krkzupa, col = COL[3])
      
      # Dulezite jednoty s popiskem
      vyber <- !is.na(data$popisek)
      points(x = data$lon[vyber],
             y = data$lat[vyber],
             col = "black",
             pch = 16, cex = 1.3)
      text(x = data$lon[vyber]+c(-0.05,0,0,0,0,0,0,0),
           y = data$lat[vyber],
           data$popisek[vyber], pos = c(1,4,3,4,1,4,1,1))
      #pozice pos=
      #1 = dolu
      #2 = do leva
      #3 = nahoru
      #4 = do prava
      legend(x = 14.8, y = 50.1, 
             c("DKnL - Dvùr Králové",
               "             nad Labem",
               "JIC - Jièín",
               "LET - Letohrad",
               "LIT - Litomyšl",
               "NACH - Náchod",
               "OST - Ostromìø",
               "PCE - Pardubice"), 
             pch = NA, lty = 1, col = NA, bty = "n", cex = 0.8)
      legend(x = 16.35, y = 50.53, 
             c("HK - Hradec Králové"), 
             title = "Výchozí pozice",
             pch = NA, lty = 1, col = NA, bty = "n", cex = 0.8)
      
      
      
      # Cykly obchodniku
      for(obch in 1:pocet_obch){
        if(typ_matice[metoda] == "xx"){
            cyklus <- najdicyklus(matice, obch = obch)
        }else{
            cyklus <- najdicyklus(matice[seq(obch,
                                             dim(matice)[1],
                                             by=pocet_obch),], 
                                  obch = 1)
        }
        lines(data[cyklus,c("lon","lat")], 
              col = lCOL2[[metoda]][[paste0(pocethodin,"h")]][obch])
        points(data[cyklus,c("lon","lat")], 
               col = lCOL2[[metoda]][[paste0(pocethodin,"h")]][obch], pch = 16)
      }
      
      # Legendy
      mtext("n=156", 3, -2, at = 15.105, cex = 1.5)
      mtext("L=50", 3, -3.2, at = 15.08, cex = 1.5)
      
      if(maK[metoda]){
        mtext("K=2", 3, -4.4, at = 15.055, cex = 1.5)
      }
      
      mtext(paste0(hezke_metody[metoda], " (", pocethodin,"hod)"),
            3, -2, cex = 1.5)
      mtext(paste0("Z=",Z[metoda,paste0(pocethodin,"h")]),
            1, -2, cex = 1.5)
      
      legend("bottomleft", c("Orlická",
                             "Východoèeská - Pipichova ",
                             "Podkrkonošská - Jiráskova",
                             "Jièínská - Bergrova"),
              title = "Župa",
              col = "black", pch = 22, pt.bg = COL, bty = "n", pt.cex = 1.5, cex = 0.8)
      legend("topright", paste0("è. ", 1:pocet_obch), 
             title = "Obchodní cestující",
             col = COL2, 
             lty = 1, lwd = 2, pch = 16, bty = "n", cex = 0.8)
    }
    
    dev.off()
    
  }
    
}
  
    
    
