# This file is part of ConceptMappR.
# 
# ConceptMappR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# ConceptMappR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ConceptMappR. If not, see <https://www.gnu.org/licenses/>.

# ConceptMappR is a Shiny dashboard designed to help students and teachers 
# draw, analyze and compare concept maps.
#
# Project Team: Christian Thurn (ETH Zurich), Simona Daguati (ETH Zurich), 
#               Bruno Ruetsche (PH Schwyz)
# Development: Bruno Ruetsche (PH Schwyz)
# Project Duration: 01.05.2023 - 31.05.2024
# Funding: Innovedum (ETH Zurich)

# Load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)

library(readr)
library(readxl)
library(xml2)
library(writexl)
library(rjson)

library(stringr)
library(stringdist)

library(dplyr)
library(tidyr)
library(tibble)

library(igraph)
library(visNetwork)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

library(reactable)
library(htmltools)
library(tippy)

library(conflicted)
conflicts_prefer(shinydashboard::box)
conflicts_prefer(dplyr::filter)
conflicts_prefer(shinyjs::show)
conflicts_prefer(reader::get.ext)
conflicts_prefer(dplyr::lag)

# Load language file (currently only EN is supported)
lang <- fromJSON(file = file.path("language", "en.json"))
