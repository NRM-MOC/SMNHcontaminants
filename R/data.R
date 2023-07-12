#' Database containing the Swedish national monitoring programme for contaminants in marine biota
#'
#' Contains more that 100 contaminant analysed for eight marine species: : Atlantic herring (Clupea harengus), Atlantic cod (Gadus morhua), European perch  (Perca fluviatilis), viviparous eelpout (Zoarces viviparus), blue mussel (Mytilus edulis), common guillemot (Uria aalge), common tern (Sterna hirundo), and Eurasian oystercatcher (Haematopus ostralegus).
#'
#' @format database with 41 columns
#' \describe{
#'
#'  \item{specimen_ID}{Unique identifier for each specimen or homogenate}
#'  \item{year}{Sampling year presented in the format yyyy}
#'  \item{date}{Sampling date presented in the format yyyy-mm-dd}
#'  \item{species}{Full scientific name}
#'  \item{species_EN}{Common English name of the species}
#'  \item{class}{Species class: Fish, Birds, Bivalves}
#'  \item{number_individuals}{The number of specimen in the homogenate; else is 1}
#'  \item{sample_lab_ID}{Describes the sample sent to the labs for specific analysis and is a merger between the specimen_ID, the organ and the analytical method}
#'  \item{substance_group}{Metals; PFAS; PCBs; Dioxins and furans; Pesticides; BFRs; Solvents-phenols; PAH; Organotin compounds}
#'  \item{contaminant}{Acronym name for the contaminant}
#'  \item{value}{Raw concentration reported by the lab, or threshold if value is below LOQ or LOD}
#'  \item{unit}{Unit for contaminant concentration}
#'  \item{is_censored}{TRUE or FALSE where TRUE refers to a value below LOQ or LOD}
#'  \item{LOQ_LOD}{Indicates if the value is below LOQ "<LOQ", below LOD "<LOD",  between LOD and LOQ">LOD, <LOQ"}
#'  \item{uncertainty}{The uncertainty level of the analysis}
#'  \item{contaminant_full_name}{Full name of the contaminant}
#'  \item{contaminant_Al_name}{Alternative short name of the contaminant widely used}
#'  \item{CAS_number}{The CAS or unique international identifier of the contaminant}
#'  \item{suspected_outliers}{TRUE or FALSE where TRUE refers to a suspected outlier based on median absolute deviation from linear model residual for the combination of station, species and contaminant}
#'  \item{laboratory}{Different laboratories: SU; UMU; SLU; IVL; SFA}
#'  \item{instrument}{Instruments: ICP-MS; DMA80; LC-MS-MS; GC-ECD; GC-HRMS; GC-MS; Packed-GC; Double-beam spectrophotometer; AAS; HPLC-FLC}
#'  \item{analytical_method}{Analytical methods as described in Table 1 and Supplementary File  S1: SU_Metals, SU_Hg, SU_PFAS, SU_CLC_BFR, UMU_Dioxin_PCB, SLU_Hg, SLU_Metals, IVL_PAH, SLV_CLC_BFR, SLU_PFAS}
#'  \item{sex}{Female, Male, Hermaphrodite, Immature, Mixed, Undetermined, else is NA}
#'  \item{total_length}{Specimen body length for fish, mussel or egg, average if homogenate, else is NA}
#'  \item{age}{The specimen age, average if homogenate, else is NA}
#'  \item{weight}{Specimen total weight, average if homogenate, else is NA}
#'  \item{organ}{Sampled organ for contaminant analysis: Liver or Muscle for fish, Soft tissue for mussels, and Egg_content for birds }
#'  \item{fat_percentage}{Fat percentage in the analysed organ}
#'  \item{dry_weight_percentage}{Dry weight of the analysed organ done by the laboratory with the contaminants analyses.}
#'  \item{shell_thickness}{Birds shell thickness, else is NA}
#'  \item{d13C}{Isotope ratio 13C/12C relative int standards, δ13C}
#'  \item{d15N}{Isotope ratio 15N/14N relative to air, δ15N}
#'  \item{C_dw_percentage}{Total carbon in relation to dry weight }
#'  \item{N_dw_percentage}{Total nitrogen in relation to dry weight}
#'  \item{station_name}{Stations full name in English alphabet }
#'  \item{latitude}{Latitude of the station, WGS84 projection system}
#'  \item{longitude}{Longitude of the station, WGS84 projection system}
#'  \item{HELCOM_basin}{HELCOM basin divisions to which belongs the station, see Helcom basin divisions at https://helcom.fi/}
#'  \item{ICES_basin}{ICES basin divisions to which belongs the station, see ICES basin divisions at https://www.ices.dk/}
#'  \item{season}{Sampling season spring March to mid-July or autumn mid-July to December}
#'}
#'
#' @examples data(ContaminantsDB)
"ContaminantsDB"

