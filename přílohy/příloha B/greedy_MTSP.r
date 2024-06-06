ROOT <- "C:/Users/acer/Documents/FIS VŠE/bakaláøská práce/výsledky"
setwd(ROOT)

library(readxl)
library(maptools)
library(colorspace)

data <- read_excel("MPL_vysledky/vystupy_x.xlsx",
                          sheet = "proR_KH_PCE")

vzdalenosti <- read_excel("~/FIS VŠE/bakaláøská práce/scripty MPL/data.xlsx",
                          sheet = "KH_PCE")
#dim(vzdalenosti)
n=156
vzdalenosti <- vzdalenosti[3:(n+2),4:(n+3)]



### Greedy algoritmus - nejblizsi soused
# Kazdy z m ochodniku projede prave L cest (ten posledni o neco mene)
# start <- 1

Greedy <-function(m, start = 1, vzdalenosti){
  #kde mohu cestovat pred navratem do startu
  n <- dim(vzdalenosti)[1]
  L <- ceiling((n-1)/m)
  zbyva <- setdiff(1:n, start)
  
  celkem <- 0
  obch <- 0
  cykly <- list()
  Z <- 0
  
  while(length(zbyva) > 0){
    l <- 0
    obch <- obch + 1
    cykly[[obch]] <- start
    pozice <- start # soucasna pozice obchodnika
    while(l < L){
      l <- l+1
      #celkem <- celkem + 1
      # hledani nejkratsi cesty mezi zbyvajicimi
      vzd <- vzdalenosti[pozice, zbyva]
      Z <- Z + min(vzd)
      novapozice <- zbyva[which.min(vzd)]
      # uprava parametru
      cykly[[obch]] <- c(cykly[[obch]], novapozice)
      pozice <- novapozice
      #useknu, kam uz nemohu jit
      zbyva <- setdiff(zbyva, pozice)
      if(length(zbyva) == 0){
        break
      }
    }
    cykly[[obch]] <- c(cykly[[obch]], start)
    Z <- Z + vzdalenosti[pozice, start]
  }
  
  return(list(Z = Z, cykly = cykly))
}

m = 4
khpce <- Greedy(m = m, start = 1, vzdalenosti = vzdalenosti)
#khpce$cykly
#khpce$Z

#HNUSNA mapka

## Zakresleni do mapy
#COL <- c("blue", "green4", "red", "coral4")
#par(mfrow = c(1,1), mar = c(4,4,1,1))
#plot(0,0, xlab = "X", ylab = "Y", xlim = c(15,16.9), ylim = c(49.5, 50.9), type = "n")
#for(obch in 1:m){
#  lines(data[khpce$cykly[[obch]],3:2], col = COL[obch], type = "b")
#}

#start <- 1
#points(x = data[start,3],
#       y = data[start,2], 
#       pch = 16, col = "black", cex = 1.5)
#text(x = data[start,3],
#     y = data[start,2], 
#     "Start", pos = 4)


### HEZKA mapka

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


# Barvy
COL <- rainbow_hcl(4, c= 70, l =70, alpha = 0.5) # pozadi zup
COLorig <- c("blue", "green4", "red", "coral4") # obchodnici pro MPL
COL2 <- c("green4", "blue", "coral4", "red") # obchodnici pro greedy


#zapnuti exportu do .pdf
##############################################################################
cairo_pdf(filename = "greedy.pdf", 
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
  
################################################################################
  # Dulezite jednoty s popiskem
  vyber <- !is.na(data$popisek)
  points(x = data$lon[vyber],
         y = data$lat[vyber],
         col = "black",
         pch = 16, cex = 1.3)
  text(x = data$lon[vyber]+c(-0.05,0,0,0,0,0,0,0),
       y = data$lat[vyber],
       data$popisek[vyber], pos = c(1,4,3,4,1,4,1,1))
  
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
  
  #pozice pos=
  #1 = dolu
  #2 = do leva
  #3 = nahoru
  #4 = do prava
  
  # Cykly obchodniku
  for(obch in 1:m){
    lines(data[khpce$cykly[[obch]],c("lon","lat")], 
          col = COL2[obch])
    points(data[khpce$cykly[[obch]],c("lon","lat")], 
           col = COL2[obch], pch = 16)
  }
  
  L <- ceiling((n-1)/m)
  # Legendy
  mtext("n=156", 3, -2, at = 15.105, cex = 1.5)
  mtext(paste0("L=",L), 3, -3.2, at = 15.08, cex = 1.5)
  
  mtext("Nejbližší soused pro MTSP", 3, -2, cex = 1.5)
  
  Zf <- floor(khpce$Z)
  Zd <- round((khpce$Z - Zf)*1000)
  newZ <- paste0(Zf,",",Zd)
  
  mtext(paste0("Z=",newZ), 1, -2, cex = 1.5)
  legend("bottomleft", c("Orlická",
                         "Východoèeská - Pipichova ",
                         "Podkrkonošská - Jiráskova",
                         "Jièínská - Bergrova"),
         title = "Župa",
         col = "black", pch = 22, pt.bg = COL, bty = "n", pt.cex = 1.5, cex = 0.8)
  legend("topright", paste0("è. ", 1:m), 
         title = "Obchodní cestující",
         col = COLorig, 
         lty = 1, lwd = 2, pch = 16, bty = "n", cex = 0.8)
}

#vypnuti exportu do .pdf
dev.off()


