# =============================================================================
#  ECONOMETRIE – Seminar 2
#  Modelul de regresie liniară simplă: specificare, estimare prin MCMMP,
#  interpretare, testarea parametrilor, validitatea modelului, previziune.
#  CSIE, Informatică economică, anul III, 2026-2027
#
#  Fișiere necesare (în același folder cu scriptul):
#     Okun.xlsx     – 104 țări: creșterea PIB în 2023, rata șomajului 2022 și 2023
#     CEOSAL1.csv   – 209 directori generali (CEO), SUA 1990: salariu, ROE, vânzări
#     wage1.csv     – 526 salariați SUA, 1976 (de la Seminarul 1)
#
#  Firul seminarului: parcurgem o dată, complet, cele trei etape din curs
#  (specificare → estimare → inferență și previziune) pe un exemplu
#  macroeconomic (legea lui Okun), apoi reluăm rapid pașii pe exemplele din
#  curs (salariul CEO, salariu–educație) ca să se fixeze rutina.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PREGĂTIRE
# -----------------------------------------------------------------------------
# Session > Set Working Directory > To Source File Location
list.files()

instaleaza_daca_lipseste <- function(pachete) {
  lipsa <- pachete[!pachete %in% rownames(installed.packages())]
  if (length(lipsa) > 0) install.packages(lipsa)
}
instaleaza_daca_lipseste(c("readxl", "dplyr", "ggplot2", "stargazer"))

library(readxl)     # citirea fișierelor Excel
library(dplyr)
library(ggplot2)
library(stargazer)


# =============================================================================
# PARTEA I. LEGEA LUI OKUN ÎN EUROPA – un model de regresie de la cap la coadă
# =============================================================================

# -----------------------------------------------------------------------------
# 1. ETAPA 1: SPECIFICAREA MODELULUI (de la teoria economică la ecuație)
# -----------------------------------------------------------------------------
# Teoria: Okun (1962) a observat că, atunci când economia crește peste ritmul
# ei potențial, rata șomajului scade, și invers. În forma "diferențelor":
#
#     Δu = α + β · g + ε
#
#     Δu – variația ratei șomajului (puncte procentuale, 2023 față de 2022)
#     g  – creșterea reală a PIB în 2023 (%)
#     β  – coeficientul lui Okun; teoria spune β < 0 (în SUA, în jur de -0,3 … -0,5)
#     α  – variația șomajului la creștere zero; α/(-β) = creșterea "de echilibru"
#          sub care șomajul începe să urce
#     ε  – tot ce nu e în model: reforme ale pieței muncii, migrație, sezonalitate,
#          erori de măsurare, "tezaurizarea" forței de muncă de către firme etc.
#
# Ipoteza pe care o testăm: β este negativ și diferit de zero.
# Observați ordinea: ÎNTÂI teoria și semnul așteptat, APOI datele. Dacă
# estimăm întâi și inventăm teoria după, nu mai testăm nimic.

okun <- read_excel("Okun.xlsx")
str(okun)
#   `Country Code` – codul ISO al țării      cont – "EUR" pentru Europa, 0 altfel
#   GDPG2023 – creșterea PIB 2023 (%)        UR2022, UR2023 – rata șomajului (%)

# Construim variabila dependentă și păstrăm doar Europa
okun <- okun %>%
  mutate(DU = UR2023 - UR2022) %>%
  rename(tara = `Country Code`, g = GDPG2023)

europa <- okun %>% filter(cont == "EUR")
nrow(europa)          # 37 de țări
head(europa)


# -----------------------------------------------------------------------------
# 2. ETAPA 2a: EXPLORAREA DATELOR (înainte de orice regresie)
# -----------------------------------------------------------------------------
europa %>% select(g, DU) %>% as.data.frame() %>% stargazer(type = "text")
# Creșterea medie în Europa în 2023: 1,5% (de la -5,5% Irlanda la +7,5% Malta).
# Șomajul a scăzut în medie cu 0,15 pp; jumătate dintre țări sunt între -0,36 și +0,27.

# Valori extreme (outlieri). Boxplot-ul le arată ca puncte în afara "mustăților".
ggplot(europa, aes(x = "", y = g)) +
  geom_boxplot(colour = "steelblue") +
  geom_jitter(colour = "red", width = 0.05, alpha = 0.8) +
  labs(x = NULL, y = "Creșterea PIB 2023 (%)")

ggplot(europa, aes(x = "", y = DU)) +
  geom_boxplot(colour = "darkgreen") +
  geom_jitter(colour = "red", width = 0.05, alpha = 0.8) +
  labs(x = NULL, y = "Variația ratei șomajului (pp)")

europa %>% arrange(g)  %>% select(tara, g, DU) %>% head(3)   # IRL -5,5 ; EST -3,0
europa %>% arrange(-g) %>% select(tara, g, DU) %>% head(3)   # MLT +7,5 ; TUR +5,1
europa %>% arrange(DU) %>% select(tara, g, DU) %>% head(3)   # BIH -2,0 ; GRC -1,4
# Irlanda: PIB-ul este distorsionat de multinaționale (relocări de profit), nu
# reflectă activitatea reală. Un outlier nu se șterge automat; se întreabă
# ÎNTÂI de ce există. Revenim la punctul 7.

# Norul de puncte: există o legătură? E liniară? În ce sens?
ggplot(europa, aes(x = g, y = DU)) +
  geom_point(colour = "navy") +
  geom_text(aes(label = tara), size = 2.5, vjust = -0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red") +
  labs(x = "Creșterea PIB 2023 (%)", y = "Variația ratei șomajului (pp)",
       title = "Legea lui Okun, Europa, 2023")

cor(europa$g, europa$DU)      # -0,38: legătură negativă, de intensitate moderată
# Corelația este simetrică (cor(x,y) = cor(y,x)) și nu spune "cu cât". Regresia
# este asimetrică (alegem noi ce e cauză și ce e efect, pe baza teoriei) și
# răspunde la "cu cât se schimbă DU când g crește cu 1".


# -----------------------------------------------------------------------------
# 3. ETAPA 2b: ESTIMAREA PRIN METODA CELOR MAI MICI PĂTRATE (MCMMP / OLS)
# -----------------------------------------------------------------------------
# Căutăm dreapta ŷ = b0 + b1·x care face suma pătratelor distanțelor verticale
# (reziduurilor) Σ(y − ŷ)² cât mai mică. Din condițiile de minim (curs, slide-urile
# MCMMP) rezultă formulele:
#
#     b1 = Σ(x − x̄)(y − ȳ) / Σ(x − x̄)²  = cov(x, y) / var(x)
#     b0 = ȳ − b1 · x̄
#
# 3.1 "De mână", ca să vedem că nu e magie:
x <- europa$g
y <- europa$DU
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
c(b0 = b0, b1 = b1)
cov(x, y) / var(x)           # același b1, scris cu cov și var

# 3.2 Cu lm(): exact aceleași valori
model_okun <- lm(DU ~ g, data = europa)
coef(model_okun)
summary(model_okun)

# Ecuația estimată:   D̂U = 0,009 − 0,103 · g

# 3.3 Proprietăți ale estimării MCMMP (consecințe ale formulelor, nu ipoteze):
europa <- europa %>%
  mutate(DU_hat = fitted(model_okun),      # valorile ajustate ŷ
         e      = residuals(model_okun))   # reziduurile e = y − ŷ

mean(europa$e)                 # ≈ 0: reziduurile au media zero
cor(europa$e, europa$g)        # ≈ 0: reziduurile nu sunt corelate cu x
mean(europa$DU_hat); mean(europa$DU)   # media valorilor ajustate = media lui y
# Consecință: dreapta de regresie trece întotdeauna prin punctul (x̄, ȳ).

# Valori reale, valori ajustate și reziduuri pe același grafic
ggplot(europa, aes(x = g)) +
  geom_segment(aes(xend = g, y = DU, yend = DU_hat), colour = "grey60") +
  geom_point(aes(y = DU, colour = "DU observat")) +
  geom_point(aes(y = DU_hat, colour = "DU ajustat")) +
  geom_line(aes(y = DU_hat), colour = "red") +
  labs(x = "Creșterea PIB 2023 (%)", y = "Variația șomajului (pp)", colour = NULL)
# Segmentele gri sunt reziduurile. MCMMP a ales dreapta pentru care suma
# pătratelor acestor segmente este minimă.


# -----------------------------------------------------------------------------
# 4. INTERPRETAREA PARAMETRILOR
# -----------------------------------------------------------------------------
# b1 = -0,103: o creștere a PIB mai mare cu 1 punct procentual este asociată cu o
#     scădere a ratei șomajului cu 0,10 puncte procentuale, în medie.
#     Semnul este cel prevăzut de teorie. Mărimea e mai mică decât în SUA
#     (-0,3 … -0,5): piața muncii europeană reacționează mai lent la ciclu
#     (protecție mai mare a locurilor de muncă, scheme de muncă redusă).
# b0 = 0,009: la creștere economică zero, șomajul ar rămâne practic neschimbat.
#     Aici constanta are sens economic (g = 0 este în interiorul datelor).
#     NU va fi mereu așa – vezi exemplul salariu–educație, partea a III-a.
#
# Două capcane de interpretare:
#   (1) unitățile: g e în %, DU e în puncte procentuale; "1% din PIB" ≠ "1 pp";
#   (2) "asociat cu", nu "determină": regresia pe 37 de observații
#       transversale nu dovedește cauzalitate (Cursul 1 și 2).


# -----------------------------------------------------------------------------
# 5. ETAPA 3a: CÂT DE BUN ESTE MODELUL? (calitatea ajustării și testul F)
# -----------------------------------------------------------------------------
# Descompunerea variației (analiza de varianță, ANOVA):
#     SST = Σ(y − ȳ)²     variația totală a lui y
#     SSR = Σ(ŷ − ȳ)²     variația explicată de regresie
#     SSE = Σ(y − ŷ)²     variația neexplicată (reziduală)
#     SST = SSR + SSE
SST <- sum((y - mean(y))^2)
SSR <- sum((fitted(model_okun) - mean(y))^2)
SSE <- sum(residuals(model_okun)^2)
c(SST = SST, SSR = SSR, SSE = SSE, verificare = SSR + SSE)

# R² = SSR / SST = 1 − SSE / SST: ponderea variației lui y explicată de x
SSR / SST
summary(model_okun)$r.squared      # 0,142
# Modelul explică 14% din variația șomajului între țări. Puțin? Pentru date
# transversale între țări, cu o singură variabilă explicativă, nu e neobișnuit.
# R² mic nu înseamnă model inutil: b1 poate fi, în același timp, semnificativ
# și important economic. R² mare nu înseamnă model bun (vezi regresia spurioasă, C1).
# În regresia simplă, R² = cor(x, y)²:
cor(x, y)^2

# Eroarea standard a regresiei: abaterea "tipică" a reziduurilor
sigma(model_okun)                  # 0,61 pp
sqrt(SSE / (nrow(europa) - 2))     # SSE / (n − 2): pierdem 2 grade de libertate (b0 și b1)

# Testul F (validitatea modelului)
#   H0: β1 = 0  (modelul nu explică nimic)      H1: β1 ≠ 0
#   F = (SSR / 1) / (SSE / (n − 2))
anova(model_okun)
# F = 5,80, p = 0,021 < 0,05 → respingem H0: modelul este valid statistic.
# Tabelul ANOVA conține exact SSR (linia g), SSE (linia Residuals) și gradele
# de libertate de mai sus. În regresia simplă F = t², unde t e testul de la punctul 6.


# -----------------------------------------------------------------------------
# 6. ETAPA 3b: TESTAREA SEMNIFICAȚIEI PARAMETRILOR (testul t)
# -----------------------------------------------------------------------------
# Pentru fiecare parametru:   H0: β = 0     H1: β ≠ 0
#     t = b / se(b)   ~  Student cu n − 2 grade de libertate, dacă H0 e adevărată
summary(model_okun)$coefficients
#               Estimate  Std. Error   t value  Pr(>|t|)
#   (Intercept)   0,0091      0,119      0,077     0,939
#   g            -0,1034      0,043     -2,409     0,021

# 6.1 Statistica t "de mână" și valoarea critică
t_calc <- coef(model_okun)["g"] / summary(model_okun)$coefficients["g", "Std. Error"]
t_calc                                   # -2,41
t_crit <- qt(0.975, df = nrow(europa) - 2)
t_crit                                   # 2,03 (bilateral, α = 0,05, 35 gdl)
abs(t_calc) > t_crit                     # TRUE → respingem H0
2 * pt(-abs(t_calc), df = nrow(europa) - 2)   # p-value = 0,021, ca în summary

# Decizie: panta este semnificativ diferită de zero la α = 0,05 (p = 0,021).
# Constanta nu este (p = 0,94) – ceea ce aici e chiar în acord cu teoria.
# Regula rapidă: |t| > 2 ⇔ semnificativ la aproximativ 5% (pentru n > 30).

# 6.2 Intervale de încredere:  b ± t_crit · se(b)
confint(model_okun)
# β1 ∈ [-0,19; -0,02] cu 95% încredere. Intervalul nu conține 0 (aceeași
# concluzie ca testul t) și nu conține nici -0,3 (valoarea "americană").

# 6.3 Test unilateral, pentru că teoria ne dă semnul:  H0: β1 ≥ 0  vs  H1: β1 < 0
pt(t_calc, df = nrow(europa) - 2)        # p = 0,011: jumătate din p-ul bilateral


# -----------------------------------------------------------------------------
# 7. PREVIZIUNE
# -----------------------------------------------------------------------------
# 7.1 Previziune punctuală: ce variație a șomajului asociem unei creșteri de 2%?
predict(model_okun, newdata = data.frame(g = 2))
# D̂U = 0,009 − 0,103 · 2 ≈ -0,20 pp

# 7.2 Două intervale, pentru două întrebări diferite:
predict(model_okun, newdata = data.frame(g = 2), interval = "confidence")
#   interval pentru MEDIA lui DU la g = 2 → [-0,41; 0,01]: incertitudinea dreptei
predict(model_okun, newdata = data.frame(g = 2), interval = "prediction")
#   interval pentru O ȚARĂ anume cu g = 2 → [-1,46; 1,06]: + împrăștierea lui ε
# Al doilea este mult mai larg: pentru o țară individuală, modelul spune puțin.

# Grafic cu cele două benzi
noi <- data.frame(g = seq(-6, 8, by = 0.25))
benzi <- cbind(noi,
               predict(model_okun, noi, interval = "confidence")[, 2:3] %>% as.data.frame() %>% rename(ci_lwr = lwr, ci_upr = upr),
               predict(model_okun, noi, interval = "prediction")[, 2:3] %>% as.data.frame() %>% rename(pi_lwr = lwr, pi_upr = upr))
ggplot(europa, aes(x = g, y = DU)) +
  geom_ribbon(data = benzi, aes(x = g, ymin = pi_lwr, ymax = pi_upr), inherit.aes = FALSE, fill = "grey85") +
  geom_ribbon(data = benzi, aes(x = g, ymin = ci_lwr, ymax = ci_upr), inherit.aes = FALSE, fill = "grey65") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red") +
  labs(x = "Creșterea PIB 2023 (%)", y = "Variația șomajului (pp)",
       title = "Bandă de încredere (gri închis) și bandă de previziune (gri deschis)")
# Observați cum ambele benzi se lărgesc spre capete: previziunile în afara
# intervalului datelor (extrapolarea) sunt tot mai puțin de încredere.

# 7.3 Previziune inversă (ca în exemplul funcției de consum din curs):
# Un guvern vrea ca șomajul să SCADĂ cu 0,5 pp. Ce creștere economică ar fi necesară?
#     -0,5 = b0 + b1 · g   ⇒   g = (-0,5 − b0) / b1
(-0.5 - coef(model_okun)[1]) / coef(model_okun)[2]     # ≈ 4,9%
# Creșterea de echilibru (DU = 0):
-coef(model_okun)[1] / coef(model_okun)[2]             # ≈ 0,1%


# -----------------------------------------------------------------------------
# 8. CÂT DE SENSIBIL E REZULTATUL LA OUTLIERI? (analiză de robustețe)
# -----------------------------------------------------------------------------
europa_fara <- europa %>% filter(DU > -1.4, g > -5, g < 7)   # scoatem BIH, GRC, IRL, MLT
nrow(europa_fara)                                             # 33
model_okun_2 <- lm(DU ~ g, data = europa_fara)
stargazer(model_okun, model_okun_2, type = "text",
          column.labels = c("toate (37)", "fără outlieri (33)"))
# Panta devine -0,18 (p < 0,001), R² urcă la 0,38. Câteva observații schimbă
# mult rezultatul, deci în raport (și în proiect!) se prezintă AMBELE estimări
# și se explică de ce au fost excluse observațiile. Eliminarea "ca să iasă"
# nu este analiză, este manipulare.


# =============================================================================
# PARTEA II. EXEMPLUL DIN CURS: SALARIUL CEO ȘI RENTABILITATEA FIRMEI
# =============================================================================
# salary = β0 + β1·roe + ε
#   salary – salariul anual al CEO (mii USD, 1990)
#   roe    – rentabilitatea capitalului propriu, media 1988–1990 (%)
# Teorie: CEO-ul e plătit pentru performanță ⇒ β1 > 0.

ceo <- read.csv("CEOSAL1.csv")
ceo %>% select(salary, roe, sales) %>% stargazer(type = "text")
# Salariul mediu 1.281 mii USD, dar maximul 14.822: distribuție foarte asimetrică.

ggplot(ceo, aes(x = roe, y = salary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "ROE (%)", y = "Salariu (mii USD)")

model_ceo <- lm(salary ~ roe, data = ceo)
summary(model_ceo)
# salary_hat = 963,19 + 18,50 · roe        n = 209, R² = 0,013
#
# Interpretare: o rentabilitate mai mare cu 1 punct procentual este asociată cu
# un salariu mai mare cu 18,5 mii USD (18.500 USD) pe an. Pentru roe = 30:
predict(model_ceo, newdata = data.frame(roe = 30))     # ≈ 1.518 mii USD
#
# Semnificație: t = 1,66, p = 0,098. La α = 0,05 NU respingem β1 = 0;
# la α = 0,10 am respinge. Un rezultat "la limită" se raportează ca atare.
# R² = 1,3%: ROE explică aproape nimic din variația salariilor între CEO.
# Mesajul: un coeficient poate avea sensul "corect" și o magnitudine
# economică mare și totuși modelul să fie slab. Semn, mărime, semnificație și
# R² sunt patru lucruri diferite și se comentează separat.

# Verificarea proprietăților din curs (slide "valorile reale, estimate și reziduurile")
ceo <- ceo %>% mutate(salary_hat = fitted(model_ceo), e = residuals(model_ceo))
ceo %>% select(salary, roe, salary_hat, e) %>% head(10)
c(media_salary = mean(ceo$salary), media_salary_hat = mean(ceo$salary_hat), media_e = mean(ceo$e))
# 1281 = 1281, iar media reziduurilor este 0 – exact ca în curs.

# Graficul reziduurilor în funcție de x: ar trebui să fie un nor fără structură
ggplot(ceo, aes(x = roe, y = e)) +
  geom_point(alpha = 0.6) + geom_hline(yintercept = 0, colour = "red") +
  labs(x = "ROE (%)", y = "Reziduuri")
# Câteva reziduuri uriașe (salarii de peste 5.000) domină estimarea. La C7 vom
# vedea că log(salary) rezolvă în mare parte problema.


# =============================================================================
# PARTEA III. SALARIU ȘI EDUCAȚIE (wage1) – când constanta nu are sens
# =============================================================================
wage1 <- read.csv("wage1.csv")
model_wage <- lm(wage ~ educ, data = wage1)
summary(model_wage)
# ŵage = -0,90 + 0,54 · educ      n = 526, R² = 0,165
#
# b1 = 0,54: un an de școală în plus este asociat cu +0,54 USD/oră. t = 10,2,
#     p < 0,001: puternic semnificativ (cu 526 de observații, erorile standard sunt mici).
# b0 = -0,90: salariul "prezis" pentru o persoană cu 0 ani de școală este
#     negativ. Absurd, dar nu e o greșeală: în eșantion aproape nimeni nu are
#     sub 8 ani de școală, deci constanta este o extrapolare în afara datelor.
#     Constanta se interpretează doar dacă x = 0 este o valoare plauzibilă.
table(wage1$educ)
predict(model_wage, newdata = data.frame(educ = c(8, 12, 16)))
# 3,43 / 5,59 / 7,76 USD/oră pentru 8, 12, 16 ani de școală.

anova(model_wage)      # F = 103,4 = t² = 10,17²; aceeași concluzie ca testul t

# O previzualizare (detalii la C7): dacă logaritmăm salariul,
model_lwage <- lm(lwage ~ educ, data = wage1)
coef(model_lwage)
# panta 0,083 se citește "un an de școală în plus ⇒ salariu cu ≈ 8,3% mai mare".
# Este forma folosită în Cursul 1 (acolo 9,2%, cu exper și tenure în model).
# De ce procente și nu dolari? Pentru că un an de școală valorează mai mult
# pentru cineva care deja câștigă mult – efectul e proporțional, nu aditiv.


# -----------------------------------------------------------------------------
# SINTEZĂ: ce raportăm pentru un model de regresie simplă
# -----------------------------------------------------------------------------
#   1. teoria și semnul așteptat al lui β1
#   2. statistici descriptive, grafic, outlieri
#   3. ecuația estimată, cu erorile standard sub coeficienți, n și R²
#   4. interpretarea lui b1 (unități!) și, dacă are sens, a lui b0
#   5. testul t (p-value) și intervalul de încredere pentru β1; testul F
#   6. previziuni cu interval și limitele lor (extrapolare, cauzalitate)


# -----------------------------------------------------------------------------
# EXERCIȚII
# -----------------------------------------------------------------------------
# 1. Temă (din cerința seminarului): reluați pașii 1–7 pentru țările
#    NON-europene (cont != "EUR"). Atenție la valorile lipsă – folosiți
#    filter(!is.na(g), !is.na(DU)) și verificați n. Se confirmă legea lui Okun?
#    Comparați coeficientul cu cel european și explicați diferența.
# 2. Pentru modelul european: calculați R² ca 1 − SSE/SST și verificați
#    egalitatea F = t² din output.
# 3. În CEOSAL1 estimați salary ~ sales. Interpretați panta ținând cont de
#    unități (sales este în milioane USD). Este constanta interpretabilă?
# 4. În wage1 estimați wage ~ exper. Semnul este cel așteptat? Comparați R² cu
#    modelul wage ~ educ. Ce ar însemna să le punem pe amândouă în model
#    (seminarul următor)?
# 5. Pentru proiect: pe setul de date ales de echipă, scrieți modelul de
#    regresie simplă pe care l-ați estima, semnul așteptat al pantei și de ce.
