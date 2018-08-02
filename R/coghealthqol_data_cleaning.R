
library(dplyr)
load("./inst/data/raw_data/elsa_1_8.RData")

# Wave identifiers
elsa_w1$wave <- 1; elsa_w2$wave <- 2; elsa_w3$wave <- 3; elsa_w4$wave <- 4
elsa_w5$wave <- 5; elsa_w6$wave <- 6; elsa_w7$wave <- 7; elsa_w8$wave <- 8

#

elsa_w2 %>%
  select(starts_with("scqol")) %>%
  lapply(., attr, which="label")

check_var <- function(x){
  list(
  VarLabel=attr(x, "label"),
  Values=attr(x, "labels"),
  Frequencies=table(x))
}
# check_var(elsa_w2$CfMetMT)
# scqola variables: 1 - 4 valid, negatives NA (higher better)
# hedi variables: 1-95 a health issue, 96 none, negative NA
# cfmetm: 1-5 valid, negative NA (higher worse)
# cflis variables: negative NA, whole numbers
# hef variables
# hepaa: negative NA, whole numbers (higher worse)
# heact variables: negative NA, 1-4 higher worse


# SCQOL var orders:
# Positive: a, b, d, f, h, i
# NEgative: c, e, g, j, k, l, m, n, o, p, q, r, s


# CASP-12: a, b, d, e, g, i, j, k, l, o, r, s
# Omitted: c, f, h,  m, n, p, q

qol_w1 <- elsa_w1 %>%
  select(idauniq, scqola:scqols, cfmetm, cflisd, cflisen, hedia01, hedib01, hefla,
         heflc, heji, hepain, hepaa, heska, hesmk, heacta, heactb, heactc, wave) %>%
  mutate_at(vars(starts_with("hedi")), funs(ifelse(. %in% c(96, -1) , 0, ifelse(. < -1, NA, 1)))) %>%
  mutate_at(vars(hefla, heflc, heji, hepaa, hepain, hesmk, heska), funs(ifelse(. == -1, 0, .))) %>%
  mutate_all(funs(ifelse(. < 0, NA, .))) %>%
  filter_at(vars(starts_with("hedi"), starts_with("scqol")), all_vars(!is.na(.))) %>%
  mutate_at(vars(scqolc, scqole, scqolg, scqolj, scqolk, scqoll, scqolm, scqoln, scqolo, scqolp, scqolq, scqolr, scqols),
            funs( (.*-1)+5 )) %>%
  mutate(cfmetm = (-1*cfmetm)+5) %>%
  mutate(badfall = ifelse(hefla==1 & heflc==1, 1, 0)) %>%
  mutate(joint = ifelse(heji==1, 1, 0)) %>%
  mutate(pain = ifelse(hepain==1, hepaa, 0)) %>%
  mutate(smokes = ifelse(heska==2, 0, 1)) %>%
  mutate(vig_act = (-1*heacta)+4,
            mod_act = (-1*heactb)+4,
            mild_act = (-1*heactc)+4) %>%
  mutate(qolsum = rowSums(.[grep("scqol", names(.))]) - 19) %>%
  select(id = idauniq, heart_issue = hedia01, other_issue=hedib01,
         memory_self = cfmetm, recall_delayed = cflisd, recall_immediate = cflisen,
         badfall, joint, pain, smokes, vig_act, mod_act, mild_act, qolsum, wave)

qol_w2 <- elsa_w2 %>%
  select(idauniq, scqola:scqols, cfmetm=CfMetM, cflisd=CfLisD, cflisen=CfLisEn, hedia01, hedib01, hefla=HeFla,
         heflc=HeFlc, heji=HeJi, hepain=HePain, hepaa=HePaa, hesmk=HeSmk, heska=HESka,
         heacta=HeActa, heactb=HeActb, heactc=HeActc, wave) %>%
  mutate_at(vars(hefla, heflc, heji, hepaa, hepain, hesmk, heska), funs(ifelse(. == -1, 0, .))) %>%
  mutate_at(vars(starts_with("hedi")), funs(ifelse(. %in% c(96, -1) , 0, ifelse(. < -1, NA, 1)))) %>%
  mutate_all(funs(ifelse(. < 0, NA, .))) %>%
  filter_at(vars(starts_with("hedi"), starts_with("scqol"), cfmetm), all_vars(!is.na(.))) %>%
  mutate_at(vars(scqolc, scqole, scqolg, scqolj, scqolk, scqoll, scqolm, scqoln, scqolo, scqolp, scqolq, scqolr, scqols),
            funs( (.*-1)+5 )) %>%
  mutate(cfmetm = (-1*cfmetm)+5) %>%
  mutate(badfall = ifelse(hefla==1 & heflc==1, 1, 0)) %>%
  mutate(joint = ifelse(heji==1, 1, 0)) %>%
  mutate(pain = ifelse(hepain==1, hepaa, 0)) %>%
  mutate(smokes = ifelse(heska==2, 0, 1)) %>%
  mutate(vig_act = (-1*heacta)+4,
         mod_act = (-1*heactb)+4,
         mild_act = (-1*heactc)+4) %>%
  mutate(qolsum = rowSums(.[grep("scqol", names(.))]) - 19) %>%
  select(id = idauniq, heart_issue = hedia01, other_issue=hedib01,
         memory_self = cfmetm, recall_delayed = cflisd, recall_immediate = cflisen,
         badfall, joint, pain, smokes, vig_act, mod_act, mild_act, qolsum, wave)




qol_w3 <- elsa_w3 %>%
  select(idauniq, scqola:scqols, cfmetm, cflisd, cflisen, hedia01=hedia96,
         hedib01=hedib96, hefla, heflc, heji, hepain, hepaa, hesmk, heska,
         heacta, heactb, heactc, wave) %>%
  mutate_at(vars(hefla, heflc, heji, hepaa, hepain, hesmk, heska), funs(ifelse(. == -1, 0, .))) %>%
  mutate_at(vars(starts_with("hedi")), funs(ifelse(. ==1 , 1, ifelse(. %in% c(-1, 0), 0, NA)))) %>%
  mutate_all(funs(ifelse(. < 0, NA, .))) %>%
  filter_at(vars(starts_with("hedi"), starts_with("scqol"), cfmetm), all_vars(!is.na(.))) %>%
  mutate_at(vars(scqolc, scqole, scqolg, scqolj, scqolk, scqoll, scqolm, scqoln, scqolo, scqolp, scqolq, scqolr, scqols),
            funs( (.*-1)+5 )) %>%
  mutate(cfmetm = (-1*cfmetm)+5) %>%
  mutate(badfall = ifelse(hefla==1 & heflc==1, 1, 0)) %>%
  mutate(joint = ifelse(heji==1, 1, 0)) %>%
  mutate(pain = ifelse(hepain==1, hepaa, 0)) %>%
  mutate(smokes = ifelse(heska==2, 0, 1)) %>%
  mutate(vig_act = (-1*heacta)+4,
         mod_act = (-1*heactb)+4,
         mild_act = (-1*heactc)+4) %>%
  mutate(qolsum = rowSums(.[grep("scqol", names(.))]) - 19) %>%
  select(id = idauniq, heart_issue = hedia01, other_issue=hedib01,
         memory_self = cfmetm, recall_delayed = cflisd, recall_immediate = cflisen,
         badfall, joint, pain, smokes, vig_act, mod_act, mild_act, qolsum, wave)

qol_w4 <- elsa_w4 %>%
  select(idauniq, scqola:scqols, cfmetm, cflisd, cflisen, hedia01=hedia96,
         hedib01=hedib96, hefla, heflc, heji, hepain, hepaa, hesmk, heska,
         heacta, heactb, heactc, wave) %>%
  mutate_at(vars(hefla, heflc, heji, hepaa, hepain, hesmk, heska), funs(ifelse(. == -1, 0, .))) %>%
  mutate_at(vars(starts_with("hedi")), funs(ifelse(. ==1 , 1, ifelse(. %in% c(-1, 0), 0, NA)))) %>%
  mutate_all(funs(ifelse(. < 0, NA, .))) %>%
  filter_at(vars(starts_with("hedi"), starts_with("scqol"), cfmetm), all_vars(!is.na(.))) %>%
  mutate_at(vars(scqolc, scqole, scqolg, scqolj, scqolk, scqoll, scqolm, scqoln, scqolo, scqolp, scqolq, scqolr, scqols),
            funs( (.*-1)+5 )) %>%
  mutate(cfmetm = (-1*cfmetm)+5) %>%
  mutate(badfall = ifelse(hefla==1 & heflc==1, 1, 0)) %>%
  mutate(joint = ifelse(heji==1, 1, 0)) %>%
  mutate(pain = ifelse(hepain==1, hepaa, 0)) %>%
  mutate(smokes = ifelse(heska==2, 0, 1)) %>%
  mutate(vig_act = (-1*heacta)+4,
         mod_act = (-1*heactb)+4,
         mild_act = (-1*heactc)+4) %>%
  mutate(qolsum = rowSums(.[grep("scqol", names(.))]) - 19) %>%
  select(id = idauniq, heart_issue = hedia01, other_issue=hedib01,
         memory_self = cfmetm, recall_delayed = cflisd, recall_immediate = cflisen,
         badfall, joint, pain, smokes, vig_act, mod_act, mild_act, qolsum, wave)

qol_w5 <- elsa_w5 %>%
  select(idauniq, scqola:scqols, cflisd, cflisen, hedia01=hedia96,
         hedib01=hedib96, hefla, heflc, heji, hepain, hepaa, hesmk, heska,
         heacta, heactb, heactc, wave) %>%
  mutate_at(vars(hefla, heflc, heji, hepaa, hepain, hesmk, heska), funs(ifelse(. == -1, 0, .))) %>%
  mutate_at(vars(starts_with("hedi")), funs(ifelse(. ==1 , 1, ifelse(. %in% c(-1, 0), 0, NA)))) %>%
  mutate_all(funs(ifelse(. < 0, NA, .))) %>%
  filter_at(vars(starts_with("hedi"), starts_with("scqol")), all_vars(!is.na(.))) %>%
  mutate_at(vars(scqolc, scqole, scqolg, scqolj, scqolk, scqoll, scqolm, scqoln, scqolo, scqolp, scqolq, scqolr, scqols),
            funs( (.*-1)+5 )) %>%
  mutate(badfall = ifelse(hefla==1 & heflc==1, 1, 0)) %>%
  mutate(joint = ifelse(heji==1, 1, 0)) %>%
  mutate(pain = ifelse(hepain==1, hepaa, 0)) %>%
  mutate(smokes = ifelse(heska==2, 0, 1)) %>%
  mutate(vig_act = (-1*heacta)+4,
         mod_act = (-1*heactb)+4,
         mild_act = (-1*heactc)+4) %>%
  mutate(cfmetm = NA) %>%
  mutate(qolsum = rowSums(.[grep("scqol", names(.))]) - 19) %>%
  select(id = idauniq, memory_self=cfmetm, heart_issue = hedia01, other_issue=hedib01, recall_delayed = cflisd, recall_immediate = cflisen,
         badfall, joint, pain, smokes, vig_act, mod_act, mild_act, qolsum, wave)

qol_1_5 <- rbind(qol_w1, qol_w2, qol_w3, qol_w4, qol_w5)
save(qol_1_5, file="./inst/data/derived_data/qol_1_5.RData")
