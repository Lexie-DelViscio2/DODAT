library(tidyverse)
library(oce)


#Aquadopp profiler
#Function to process raw (.prf) file to a wide-format csv. Includes variables that are not available in the software-processed csvs, including:
# Raw north, east, up velocity vectors, and mean amplitude (for backscatter)
process_adp_to_csv <- function(file) {
adp <- read.aquadoppProfiler(file)

adp <- adpConvertRawToNumeric(adp)

adp[["v"]][,,1]
dim(adp[["a"]])


# Veast <- adp[["v"]][,,2]
# Vup <- adp[["v"]][,,3]
# 
# speed <- sqrt(Vnorth^2 + Veast^2)

cells <- 1:adp@metadata[["numberOfCells"]]
dists <- round(adp[["distance"]]-.01, 2)

Vnorth <- as.data.frame(sapply(cells, function(x) { adp[["v"]][,x,1]}))
colnames(Vnorth) <- paste0("Vnorth#",cells, "(",dists,"m)")

Veast <- as.data.frame(sapply(cells, function(x) { adp[["v"]][,x,2]}))
colnames(Veast) <- paste0("Veast#",cells, "(",dists,"m)")

Vup <- as.data.frame(sapply(cells, function(x) { adp[["v"]][,x,3]}))
colnames(Vup) <- paste0("Vup#",cells, "(",dists,"m)")

amps <- as.data.frame(sapply(cells, function(x) {round(apply(adp[["a"]][,x,], c(1), mean), 3)}))
colnames(amps) <- paste0("Amp#",cells, "(",dists,"m)")

hypot <- function(x) {sqrt(x[1]^2 + x[2]^2)}
speeds <-  as.data.frame(sapply(cells, function(y) {round(apply(adp[["v"]][,y,1:2], 1, hypot),3)}))
colnames(speeds) <- paste0("Speed#",cells, "(",dists,"m)")

get_dir <- function(x) {
  d <- atan2(x[1],x[2])*(180/pi)
  d2 <- ifelse(d>=0, d, d+360)
  d2}

dirs <- as.data.frame(sapply(cells, function(y) {round(apply(adp[["v"]][,y,1:2], 1, get_dir), 3)}))
colnames(dirs) <- paste0("Dir#", cells, "(",dists,"m)")

str(as_datetime(adp[["time"]]))

df1 <- data.frame(DateTime = as_datetime(adp[["time"]]), Heading = adp[["heading"]], Pitch = adp[["pitch"]], Roll = adp[["roll"]], 
                  Pressure = adp[["pressure"]], Temperature = adp[["temperature"]])

df <- cbind(df1, speeds, dirs, amps, Vnorth, Veast, Vup)

outfile <- gsub(".PRF", "proc.csv", file)

write.csv(df, file = outfile, row.names = F)
}

files <- list.files("MDBC_Lander_Data/Aquadopp/raw/", "*.PRF", full.names = T)


lapply(files, process_adp_to_csv)
##################

path <- "MDBC_Lander_Data/Aquadopp/csv/Manually_converted/"
files_aqdp <- list.files(path, ".csv")
deps_aqdp <- substr(files_aqdp, 1,3)
aqdps <- lapply(paste0(path, files_aqdp), read_csv)

names(aqdps) <- deps_aqdp

adp <- aqdps[[1]]
adp_long <- pivot_longer(adp, cols = starts_with(c("Speed", "Amp", "Dir", "Vnorth", "Veast", "Vup")),
             names_to = c(".value", "cell", "distance"), names_pattern = "(.*)#(.*)\\((.*)m\\)") %>%
  mutate(distance = as.numeric(distance))

aqdps_long <- lapply(aqdps, function(x) {
  pivot_longer(x, cols = starts_with(c("Speed", "Amp", "Dir", "Vnorth", "Veast", "Vup")),
               names_to = c(".value", "cell", "distance"), names_pattern = "(.*)#(.*)\\((.*)m\\)") %>%
    mutate(distance = as.numeric(distance))
})

details <- read.csv("MDBC_Lander_Data/Equipment_Deployment_Master_6-26-25.csv")%>%
  filter(X.Recovered. == "Yes") %>%
  select(Deployment_ID = Short.Deployment.ID, Deployment_Date = Deployment.Time..UTC., Recovery_Date= Recovery.Time..UTC., Lat, Long, Depth_m = Depth..m.) %>%
  mutate(Deployment_Date = parse_date_time(Deployment_Date, "mdy HM"), Recovery_Date = parse_date_time(Recovery_Date, c("mdy HM")))


aqdp_long <- do.call(rbind, aqdps_long) %>%
  rownames_to_column("Deployment_ID") %>%
  mutate(Deployment_ID = gsub("\\..*","",Deployment_ID)) %>%
  left_join(details, by = "Deployment_ID") %>%
  filter(DateTime %within% interval(Deployment_Date+minutes(30), Recovery_Date-minutes(70)))


ggplot(aqdp_long) +
  geom_raster(aes(DateTime, distance, fill = Speed)) +
  ylab("Distance (m)") +
  scale_fill_viridis_c(option = "turbo") +
  facet_wrap(~Deployment_ID, scales = "free") +
  theme_classic()




