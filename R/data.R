#' Training set of 250000 events from the ATLAS full-detector simulation.
#' 
#' A training dataset of 250000 events, with an ID column, 30 feature columns, a weight column and a label column.
#' @usage data(training)
#' @format A data frame with 250,000 rows and 33 variables:
#' \describe{
#'   \item{EventId}{An unique integer identifier of the event.}
#'   \item{DER_mass_MMC}{The estimated mass of the Higgs boson candidate, obtained through a probabilistic phase space integration.} 
#'   \item{DERmass_transverse_met_lep}{The transverse mass between the missing transverse energy and the lepton.}
#'   \item{DER_mass_vis}{The invariant mass of the hadronic tau and the lepton.}
#'   \item{DER_pt_h}{The modulus of the vector sum of the transverse momentum of the hadronic tau, the lepton and the missing transverse energy vector.}
#'   \item{DER_deltaeta_jet_jet}{	The absolute value of the pseudorapidity separation between the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{DER_mass_jet_jet}{The invariant mass of the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{DER_prodeta_jet_jet}{The product of the pseudorapidities of the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{DER_deltar_tau_lep}{The R separation between the hadronic tau and the lepton.}
#'   \item{DER_pt_tot}{The modulus of the vector sum of the missing transverse momenta and the transverse momenta of the hadronic tau, the lepton, the leading jet (if PRI_jet_num ≥) and the subleading jet (if PRI jet num = 2) (but not of any additional jets).}
#'   \item{DER_sum_pt}{The sum of the moduli of the transverse momenta of the hadronic tau, the lepton, the leading jet (if PRI jet num ≥ 1) and the subleading jet (if PRI jet num = 2) and the other jets (if PRI jet num = 3).}
#'   \item{DER_pt_ratio_lep_tau}{The ratio of the transverse momenta of the lepton and the hadronic tau.}
#'   \item{DER_met_phi_centrality}{The centrality of the azimuthal angle of the missing transverse energy vector w.r.t. the hadronic tau and the lepton.}
#'   \item{DER_lep_eta_centrality}{The centrality of the pseudorapidity of the lepton w.r.t. the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_tau_pt}{The transverse momentum of the hadronic tau.}
#'   \item{PRI_tau_eta}{The pseudorapidity of the hadronic tau.}
#'   \item{PRI_tau_phi}{The azimuth angle of the hadronic tau.}
#'   \item{PRI_lep_pt}{The transverse momentum of the lepton (electron or muon).}
#'   \item{PRI_lep_eta}{The pseudorapidity of the lepton.}
#'   \item{PRI_lep_phi}{The azimuth angle of the lepton.}
#'   \item{PRI_met}{The missing transverse energy.}
#'   \item{PRI_met_phi}{The azimuth angle of the mssing transverse energy.}
#'   \item{PRI_met_sumet}{The total transverse energy in the detector.}
#'   \item{PRI_jet_num}{The number of jets (integer with value of 0, 1, 2 or 3; possible larger values have been capped at 3).}
#'   \item{PRI_jet_leading_pt}{The transverse momentum of the leading jet, that is the jet with largest transverse momentum (undefined if PRI_jet_num = 0).}
#'   \item{PRI_jet_leading_eta}{The pseudorapidity of the leading jet (undefined if PRI jet num = 0).}
#'   \item{PRI_jet_leading_phi}{The azimuth angle of the leading jet (undefined if PRI jet num = 0).}
#'   \item{PRI_jet_subleading_pt}{The transverse momentum of the leading jet, that is, the jet with second largest transverse momentum (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_jet_subleading_eta}{The pseudorapidity of the subleading jet (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_jet_subleading_phi}{The azimuth angle ϕ of the subleading jet (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_jet_all_pt}{	The scalar sum of the transverse momentum of all the jets of the events.}
#'   \item{Weight}{The event weight}
#'   \item{Label}{The event label (string) (s for signal, b for background).}
#' }
#' @source \url{http://opendata.cern.ch/record/328}
#' 
"training"


#' Test set of 550000 events from the ATLAS full-detector simulation.
#'
#' A test dataset of 550000 events, with an ID column, 30 feature columns, a weight column and a label column.
#' @usage data(test)
#' @format A data frame with 550,000 rows and 33 variables:
#' \describe{
#'   \item{EventId}{An unique integer identifier of the event.}
#'   \item{DER_mass_MMC}{The estimated mass of the Higgs boson candidate, obtained through a probabilistic phase space integration.} 
#'   \item{DERmass_transverse_met_lep}{The transverse mass between the missing transverse energy and the lepton.}
#'   \item{DER_mass_vis}{The invariant mass of the hadronic tau and the lepton.}
#'   \item{DER_pt_h}{The modulus of the vector sum of the transverse momentum of the hadronic tau, the lepton and the missing transverse energy vector.}
#'   \item{DER_deltaeta_jet_jet}{	The absolute value of the pseudorapidity separation between the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{DER_mass_jet_jet}{The invariant mass of the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{DER_prodeta_jet_jet}{The product of the pseudorapidities of the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{DER_deltar_tau_lep}{The R separation between the hadronic tau and the lepton.}
#'   \item{DER_pt_tot}{The modulus of the vector sum of the missing transverse momenta and the transverse momenta of the hadronic tau, the lepton, the leading jet (if PRI_jet_num ≥) and the subleading jet (if PRI jet num = 2) (but not of any additional jets).}
#'   \item{DER_sum_pt}{The sum of the moduli of the transverse momenta of the hadronic tau, the lepton, the leading jet (if PRI jet num ≥ 1) and the subleading jet (if PRI jet num = 2) and the other jets (if PRI jet num = 3).}
#'   \item{DER_pt_ratio_lep_tau}{The ratio of the transverse momenta of the lepton and the hadronic tau.}
#'   \item{DER_met_phi_centrality}{The centrality of the azimuthal angle of the missing transverse energy vector w.r.t. the hadronic tau and the lepton.}
#'   \item{DER_lep_eta_centrality}{The centrality of the pseudorapidity of the lepton w.r.t. the two jets (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_tau_pt}{The transverse momentum of the hadronic tau.}
#'   \item{PRI_tau_eta}{The pseudorapidity of the hadronic tau.}
#'   \item{PRI_tau_phi}{The azimuth angle of the hadronic tau.}
#'   \item{PRI_lep_pt}{The transverse momentum of the lepton (electron or muon).}
#'   \item{PRI_lep_eta}{The pseudorapidity of the lepton.}
#'   \item{PRI_lep_phi}{The azimuth angle of the lepton.}
#'   \item{PRI_met}{The missing transverse energy.}
#'   \item{PRI_met_phi}{The azimuth angle of the mssing transverse energy.}
#'   \item{PRI_met_sumet}{The total transverse energy in the detector.}
#'   \item{PRI_jet_num}{The number of jets (integer with value of 0, 1, 2 or 3; possible larger values have been capped at 3).}
#'   \item{PRI_jet_leading_pt}{The transverse momentum of the leading jet, that is the jet with largest transverse momentum (undefined if PRI_jet_num = 0).}
#'   \item{PRI_jet_leading_eta}{The pseudorapidity of the leading jet (undefined if PRI jet num = 0).}
#'   \item{PRI_jet_leading_phi}{The azimuth angle of the leading jet (undefined if PRI jet num = 0).}
#'   \item{PRI_jet_subleading_pt}{The transverse momentum of the leading jet, that is, the jet with second largest transverse momentum (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_jet_subleading_eta}{The pseudorapidity of the subleading jet (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_jet_subleading_phi}{The azimuth angle ϕ of the subleading jet (undefined if PRI_jet_num ≤ 1).}
#'   \item{PRI_jet_all_pt}{	The scalar sum of the transverse momentum of all the jets of the events.}
#'   \item{Weight}{The event weight}
#'   \item{Label}{The event label (string) (s for signal, b for background).}
#' }
#' @source \url{http://opendata.cern.ch/record/328}
#'
"test"