BiocManager::install("devtools")
library(devtools)

source_url("https://github.com/ltoker/GeneralRscripts/blob/main/generalFunc.R?raw=T")
packageF("Hmisc")


get_colorPal <- function(im, n=400, cs="RGB"){
  #print(cs) 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(dither = T, max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}

ColorList <- sapply(gsub(".jpg", "", list.files("Colors/")), function(Pallete){
  image_read(paste0("Colors/",Pallete, ".jpg"))
},simplify = F)


MoviePalettes <- lapply(ColorList, function(Pallete){
  get_colorPal(Pallete, n = 400) %>% arrange(desc(n)) %>%
    head(15) %>% arrange(hex, hue, sat,value)
})


MoviePalettes$MoonRiseKingdomColors <- c("#3A1102", "#5E1103", "#831F02", 
                                         "#C75F26", "#C7A05A","#A49469", "#868569",
                                         "#716B5D",  "#586160","#617983")

MoviePalettes$AmericanBeauty <- c("#3D0307", "#633C35", "#94736E",
                                  "#C8A2A1", "#915962", "#574751",
                                  "#9F999D", "#B5BAB6", "#E9F3E8","#7E6731")

MoviePalettes$AmericanPsycho <- c( "#181415", "#251216", "#462923",
                                   "#6F4D4B", "#BCA198", "#FBEBEB",
                                   "#CBCDDB", "#707686", "#69625D", "#312A30")

MoviePalettes$AnnieHall <- c("#050100", "#2E1C1C", "#70514F",
                             "#B28E90", "#D2ACA9", "#C8C2B6",
                             "#98958C", "#1A2215", "#231D16", "#3D1D11") 

MoviePalettes$BlueIsTheWarmestColor <- c("#191D1F", "#0C2B48", "#486A7D", 
                                         "#CDB7AA", "#5E4F4C", "#A37A7E",
                                         "#B84749", "#893C28", "#3B2D22", "#2F381E")


MoviePalettes$GrandBudapestHotelPinks <- c("#181212", "#3E1711", "#6D2A21", 
                                           "#622125", "#8D3946", "#BC7A88",
                                           "#AB7F8E", "#734E5F", "#5E5162", "#37384D")   


MoviePalettes$GreenPallete <- c("#EDE9CE", "#C2C79C", "#8B9D65",
                                "#6A5E08", "#454B1F", "#1A251C",
                                "#5E6A5C", "#5C493F", "#9A8268", "#D2C196")

MoviePalettes$HarryPoterBlues <- c("#E3F7D2", "#C8F6D2", "#81AFA5",
                                   "#507F85", "#1D4149", "#4D5B62",
                                   "#302A34", "#060814", "#01153A", "#075080")

MoviePalettes$LittleShopOfHorrors <- c("#1D1311", "#3A261F", "#A89388",
                                       "#453F21", "#8E822E", "#AE8627",
                                       "#C67E8C", "#7F5C41", "#8C463C", "#540508") 

MoviePalettes$LostInTranslationPurples <- c("#4F4448", "#291E24", "#2C1A2B",
                                            "#553C4F", "#6B617C", "#8493BD",
                                            "#616E9B", "#354350", "#1E243A", "#13111C")

MoviePalettes$MadMaxDesert <- c("#0E646B", "#133638", "#404740",
                                "#B99074", "#FFD1B4", "#FF9F00",
                                "#FC7C0C", "#5C2A09", "#311915", "#12151A")

MoviePalettes$MarryPoppins <- c("#2A2833", "#7B4A4E", "#7C6E7B",
                                "#6E70A8", "#B1B1D2", "#5C6E7B", 
                                "#588986", "#4D6C9B", "#091D36", "#1D1B25") 

MoviePalettes$SpiritedAway <- c("#65738E", "#756B90", "#795770",
                                "#523B58", "#813B3D", "#B65145",
                                "#EE8D7A", "#F9C784", "#EBA05C", "#9B5133") 


MoviePalettes$StarWarsBrowns <- c("#4F1401", "#3F1810", "#664D3C",
                                  "#D8A56D", "#FAE2B2", "#A88962",
                                  "#664C3B", "#533128", "#241A18", "#140A08")  

MoviePalettes$BugsLife <- c("#1D1D1D", "#5379AA", "#82B3C1",
                            "#E3BF71","#9D825D",  "#768156",
                            "#6B6F22", "#2B3006", "#3D2816","#272727")



saveRDS(MoviePalettes, "ColorsJPG/MoviePalettes.Rds")


