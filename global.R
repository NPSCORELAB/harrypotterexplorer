# Libraries ====================================================================
library(shiny)
library(leaflet)
library(DT)
library(visNetwork)
library(igraph)
library(shinyBS)
library(shinythemes)

# Required Data ================================================================
# ---- For Server ----
combined_geo <- readRDS(here::here("data/combined_geo2.rds"))
da_self_loop_m <- readRDS(here::here("data/da_self_loop_m_v2.rds"))
da_net_rel <- readRDS(here::here("data/da_net_rel_v2.rds"))
death_self_loop_m <- readRDS(here::here("data/death_self_loop_m.rds"))
deatheaters_rel <- readRDS(here::here("data/deatheaters_rel.rds"))
wizard_strength <- readRDS(here::here("data/wizard_strength_v2.rds"))
wizard_info <- readRDS(here::here("data/wizard_info.rds"))

combined_geo_da <- subset(combined_geo, combined_geo$Group != "DA")
combined_geo_death <- subset(combined_geo, combined_geo$Group != "Deatheaters")

# ---- For UI ----
spells_comb <- readRDS(here::here("data/spells_comb2.rds"))
spells2 <- sort(spells_comb$Spell)
spells2 <- unique(spells2)