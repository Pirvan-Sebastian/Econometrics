# =============================================================================
#  ECONOMETRIE – Seminar 1
#  Introducere în R. Recapitulare de statistică. Tipuri de date economice.
#  CSIE, Informatică economică, anul III, 2026-2027
#
#  Fișiere necesare (în același folder cu scriptul):
#     intro_auto.csv   – 26 autoturisme: preț, consum, greutate, lungime, origine
#     wage1.csv        – 526 salariați SUA, 1976 (Wooldridge, WAGE1)
#     prminwge.csv     – salariul minim în Puerto Rico, 1950–1987 (PRMINWGE)
#     hprice3.csv      – prețuri ale locuințelor, 1978 și 1981 (HPRICE3)
#     wagepan.csv      – 545 bărbați urmăriți 8 ani, 1980–1987 (WAGEPAN)
#
#  Cum se lucrează: rulați scriptul linie cu linie (Ctrl+Enter pe linia curentă
#  sau pe selecție). Citiți comentariile înainte de a rula – ele explică ce
#  face fiecare comandă și cum se citește rezultatul.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PREGĂTIREA MEDIULUI DE LUCRU
# -----------------------------------------------------------------------------

# R lucrează întotdeauna într-un "director de lucru" (working directory).
# De acolo citește fișierele și acolo salvează rezultatele.
getwd()          # unde suntem acum?

# Varianta recomandată: în RStudio, Session > Set Working Directory >
# To Source File Location. Astfel directorul de lucru devine folderul în
# care se află scriptul, iar fișierele CSV pot fi citite doar cu numele lor.
#
# Varianta manuală: setwd("C:/.../Seminar 1")
# Atenție: în R calea se scrie cu "/" (sau "\\"), nu cu "\" ca în Windows.

list.files()     # ce fișiere există în directorul de lucru? Trebuie să vedem cele 5 CSV-uri.

# Pachetele sunt colecții de funcții scrise de alți utilizatori. Se instalează
# O SINGURĂ DATĂ (install.packages) și se încarcă la FIECARE sesiune (library).
# Funcția de mai jos instalează un pachet doar dacă lipsește, ca să nu
# reinstalăm de fiecare dată când rulăm scriptul.
instaleaza_daca_lipseste <- function(pachete) {
  lipsa <- pachete[!pachete %in% rownames(installed.packages())]
  if (length(lipsa) > 0) install.packages(lipsa)
}
instaleaza_daca_lipseste(c("dplyr", "ggplot2", "stargazer", "moments"))

library(dplyr)      # manipularea datelor: select, filter, mutate, group_by
library(ggplot2)    # grafice
library(stargazer)  # tabele frumoase pentru statistici descriptive și regresii
library(moments)    # asimetrie (skewness) și boltire (kurtosis)


# -----------------------------------------------------------------------------
# 1. R DE BAZĂ: OBIECTE, VECTORI, DATA FRAME
# -----------------------------------------------------------------------------

# În R totul este un obiect. Atribuirea se face cu "<-" (sau "=").
x <- 5
y <- c(2, 4, 6, 8)        # c() = "combine": construiește un vector
x * y                     # operațiile se fac element cu element
mean(y); sd(y); length(y)

# Ajutorul este la un "?" distanță. Orice funcție are documentație cu
# argumentele ei, valorile implicite și exemple. Obișnuiți-vă să o consultați.
?mean
?cor

# Citirea unui fișier CSV într-un data frame (tabel cu linii = observații,
# coloane = variabile).
auto <- read.csv("intro_auto.csv")

View(auto)        # deschide tabelul într-un tab separat (doar în RStudio)
str(auto)         # STRucture: nr. de observații, nr. de variabile, tipul fiecăreia
names(auto)       # numele variabilelor
dim(auto)         # dimensiuni: linii, coloane
head(auto)        # primele 6 linii
head(auto, 10)    # primele 10 linii

# Accesarea datelor:
auto$mpg          # o variabilă, cu operatorul $
auto[1:10, ]      # liniile 1–10, toate coloanele  →  [linii, coloane]
auto[, c("make", "price")]        # toate liniile, doar două coloane
auto[auto$foreign == 1, ]         # doar mașinile din import (condiție logică)

# Variabilele din acest set:
#   price   – prețul (USD)              mpg     – consum, mile pe galon (mai mare = mai economic)
#   repairs – nr. reparații             weight  – greutate (livre)
#   length  – lungime (inch)            foreign – 1 = import, 0 = producție internă
# foreign este o variabilă binară ("dummy"): le vom folosi intens la C7.


# -----------------------------------------------------------------------------
# 2. RECAPITULARE STATISTICĂ DESCRIPTIVĂ
# -----------------------------------------------------------------------------

# 2.1 O singură variabilă numerică
summary(auto$mpg)      # minim, cuartile, mediană, medie, maxim
sd(auto$mpg)           # abaterea standard
var(auto$mpg)          # varianța (= sd^2)
quantile(auto$mpg, probs = c(0.1, 0.9))   # decilele 1 și 9
skewness(auto$mpg)     # asimetrie: > 0 coadă la dreapta, < 0 coadă la stânga, 0 simetrică
kurtosis(auto$mpg)     # boltire: 3 pentru normală (valoarea de referință în pachetul moments)

hist(auto$mpg, main = "Distribuția consumului (mpg)", xlab = "mpg", col = "grey85")
boxplot(auto$price, main = "Prețul autoturismelor", ylab = "USD")

# De ce contează: multe variabile economice (salarii, prețuri, venituri) sunt
# puternic asimetrice la dreapta. Vom vedea la C7 că logaritmarea le "simetrizează",
# ceea ce ajută mult modelul de regresie.

# 2.2 O variabilă calitativă (categorială)
table(auto$make)                 # tabel de frecvențe
table(auto$make, auto$foreign)   # tabel de contingență (două variabile)
prop.table(table(auto$foreign))  # frecvențe relative: 27% din mașini sunt import

# 2.3 Legătura dintre două variabile numerice
plot(auto$weight, auto$mpg,
     xlab = "Greutate (livre)", ylab = "Consum (mpg)",
     main = "Mașinile mai grele consumă mai mult")

cor(auto$price, auto$mpg)                       # Pearson (implicit): -0.44
cor(auto$price, auto$mpg, method = "spearman")  # pe ranguri, robust la valori extreme
cor(auto$price, auto$mpg, method = "kendall")

# Citire: coeficientul de corelație ia valori în [-1, 1]. Semnul dă direcția
# legăturii, modulul dă intensitatea. Pearson măsoară doar legătura LINIARĂ.
# Corelația NU spune nimic despre cauzalitate (vezi C1: cauzalitate vs. corelație).

# Matricea de corelații pentru toate variabilele numerice:
round(cor(auto[, c("price", "mpg", "weight", "length")]), 2)


# -----------------------------------------------------------------------------
# 3. RECAPITULARE INFERENȚĂ STATISTICĂ
# -----------------------------------------------------------------------------
# Logica oricărui test, pe care o vom folosi tot semestrul:
#   1. formulăm H0 (ipoteza nulă) și H1 (alternativa);
#   2. calculăm o statistică de test din eșantion;
#   3. citim p-value = probabilitatea de a obține o statistică cel puțin la fel
#      de extremă ca cea observată, DACĂ H0 ar fi adevărată;
#   4. dacă p-value < nivelul de semnificație α (uzual 0,05), respingem H0.
# Respingerea lui H0 înseamnă "datele sunt incompatibile cu H0", nu "H1 e dovedită".
# Nerespingerea lui H0 înseamnă "nu avem suficiente dovezi", nu "H0 e adevărată".

# 3.1 Testul t pentru media unei populații
#   H0: media populației μ = 20        H1: μ ≠ 20
t.test(auto$mpg, mu = 20)
# Citire: t = 0,99, p-value = 0,33 > 0,05 → nu respingem H0. Media de eșantion
# (20,9) nu este semnificativ diferită de 20. Intervalul de încredere 95%
# [19,0; 22,8] conține valoarea 20 – aceeași concluzie, exprimată altfel.

# 3.2 Testul t pentru compararea a două medii (grupuri independente)
#   H0: μ_import = μ_intern        H1: μ_import ≠ μ_intern
t.test(mpg ~ foreign, data = auto)
# Sintaxa "y ~ x" se citește "y în funcție de x" și este aceeași ca la regresie.
# Mașinile din import au în medie 24 mpg, cele interne 19,8 mpg; p ≈ 0,10.
# La α = 0,05 nu respingem H0; la α = 0,10 am respinge-o. Nivelul α se fixează
# ÎNAINTE de a vedea rezultatul, nu după.
# Observație: eșantionul e mic (7 mașini din import), deci testul are putere redusă.

# 3.3 ANOVA – compararea mediilor pe mai multe grupuri
#   H0: toate mediile sunt egale       H1: cel puțin una diferă
# Aici avem doar două grupuri, deci ANOVA e echivalentă cu testul t cu varianțe
# egale. O folosim ca să vedem legătura cu regresia: ANOVA este un caz
# particular al modelului de regresie cu variabile dummy.
anova(lm(mpg ~ factor(foreign), data = auto))
# F = 4,58, p = 0,043 < 0,05 → respingem H0: originea explică o parte a
# variației consumului. (Testul F îl reîntâlnim la C4, pentru validitatea modelului.)


# -----------------------------------------------------------------------------
# 4. PRIMUL MODEL DE REGRESIE
# -----------------------------------------------------------------------------
# Întrebare: cum depinde consumul (mpg) de greutate, lungime și origine?
# Model:  mpg = β0 + β1·weight + β2·length + β3·foreign + ε
#
# lm() = "linear model". Se scrie formula "y ~ x1 + x2 + x3" și se indică
# setul de date; NU scriem auto$mpg ~ auto$weight (merge, dar face previziunea
# și tabelele mult mai greoaie).

model1 <- lm(mpg ~ weight + length + foreign, data = auto)
summary(model1)

# Cum se citește rezultatul (detaliem la C2–C4, aici doar orientarea):
#   Estimate    – coeficientul estimat b. weight: -0,0050 → la o creștere a
#                 greutății cu 1 livră, consumul scade cu 0,005 mpg, ceilalți
#                 factori fiind constanți (ceteris paribus). Sau: +1.000 livre → -5 mpg.
#   Std. Error  – eroarea standard a estimației (cât de "sigur" e coeficientul).
#   t value     – Estimate / Std. Error; statistica testului H0: β = 0.
#   Pr(>|t|)    – p-value-ul testului. weight: 0,032 < 0,05 → semnificativ;
#                 length (0,58) și foreign (0,45) → nesemnificative.
#   R-squared   – 0,67: modelul explică 67% din variația consumului.
#   F-statistic – 14,8, p ≈ 0,00002 → cel puțin un coeficient este diferit de 0;
#                 modelul este valid statistic.
#
# Observație: length pare irelevant, dar length și weight sunt puternic
# corelate (0,91 în matricea de mai sus). Este un prim exemplu de
# multicoliniaritate – C6.

coef(model1)                  # doar coeficienții
confint(model1)               # intervale de încredere 95% pentru coeficienți
head(fitted(model1))          # valorile ajustate ŷ
head(residuals(model1))       # reziduurile e = y − ŷ

# Regresia simplă și dreapta de regresie pe grafic
model_simplu <- lm(mpg ~ weight, data = auto)
plot(mpg ~ weight, data = auto, xlab = "Greutate (livre)", ylab = "Consum (mpg)")
abline(model_simplu, col = "red", lwd = 2)

# Previziune: ce consum estimăm pentru o mașină de 3.000 de livre?
predict(model_simplu, newdata = data.frame(weight = 3000))
# 38,07 − 0,0055 · 3000 ≈ 21,5 mpg

# Curățăm spațiul de lucru înainte de partea a doua.
rm(list = ls())


# -----------------------------------------------------------------------------
# 5. MANIPULAREA DATELOR CU dplyr
# -----------------------------------------------------------------------------
# Setul WAGE1: 526 de salariați din SUA (1976). Este setul din Cursul 1
# (slide "Primul model estimat") și îl vom folosi în multe seminarii.
#   wage   – salariul orar (USD)      educ    – ani de școală
#   exper  – experiență (ani)         tenure  – vechime la angajatorul curent (ani)
#   female – 1 = femeie               married – 1 = căsătorit(ă)
#   lwage  – log(wage)                expersq – exper^2

wage1 <- read.csv("wage1.csv")
str(wage1)

# Operatorul pipe "%>%" (sau "|>", varianta din R de bază) trimite obiectul
# din stânga ca prim argument al funcției din dreapta:
#   wage1 %>% select(wage, educ)   este identic cu   select(wage1, wage, educ)
# Avantajul: pașii se înlănțuie și se citesc de sus în jos, ca o rețetă.

# select – alege coloane
wage1 %>% select(wage, educ, exper) %>% head(10)

# filter – alege linii după o condiție
wage1 %>% filter(female == 1) %>% nrow()          # câte femei? 252
wage1 %>% filter(educ >= 16, exper < 5) %>% head() # absolvenți de facultate, la început de carieră

# mutate – creează variabile noi
wage1 <- wage1 %>%
  mutate(lwage_calc = log(wage),          # verificăm că lwage din fișier e log(wage)
         educsq     = educ^2,
         high_educ  = as.integer(educ >= 16))
all.equal(wage1$lwage, wage1$lwage_calc)  # TRUE (până la rotunjire)

# group_by + summarise – statistici pe grupuri
wage1 %>%
  group_by(female) %>%
  summarise(n          = n(),
            salariu_m  = mean(wage),
            educ_m     = mean(educ),
            exper_m    = mean(exper))
# Femeile câștigă în medie 4,59 USD/oră, bărbații 7,10, la educație aproape egală.
# Este o diferență cauzală (discriminare) sau reflectă alți factori
# (experiență, ocupație, ore lucrate)? Exact genul de întrebare pe care
# regresia multiplă ne ajută să o abordăm (C3) – și pe care nu o închide (C1).

# Statistici descriptive într-un tabel curat, cu stargazer
wage1 %>% select(wage, lwage, educ, exper, tenure, female) %>%
  stargazer(type = "text")

# Un grafic cu ggplot2: distribuția salariului, brut și logaritmat
ggplot(wage1, aes(x = wage)) +
  geom_histogram(bins = 30, fill = "grey70", colour = "white") +
  labs(title = "Salariul orar are o coadă lungă la dreapta", x = "USD/oră", y = "Frecvență")

ggplot(wage1, aes(x = lwage)) +
  geom_histogram(bins = 30, fill = "grey70", colour = "white") +
  labs(title = "log(salariu) este aproape simetric", x = "log(USD/oră)", y = "Frecvență")

skewness(wage1$wage); skewness(wage1$lwage)   # 2,01 vs. 0,39

# Estimăm două modele și le punem unul lângă altul
m_simplu   <- lm(wage ~ educ, data = wage1)
m_multiplu <- lm(lwage ~ educ + exper + tenure, data = wage1)

stargazer(m_simplu, m_multiplu, type = "text")

# Citire:
#   m_simplu:   un an de școală în plus este asociat cu +0,54 USD/oră. R² = 0,16.
#   m_multiplu: modelul din Cursul 1. Coeficientul educ = 0,092: un an de școală
#               în plus este asociat cu un salariu cu ≈ 9,2% mai mare, la
#               experiență și vechime constante. (Interpretarea în procente
#               vine din logaritm – detalii la C7.)
# Reținem întrebarea din curs: este 9,2% un efect cauzal? Persoanele cu mai
# multă școală pot avea și abilități mai mari, care ar fi crescut salariul oricum.

# Export pentru proiect: tabelul se salvează ca fișier și se deschide în Word
stargazer(m_simplu, m_multiplu, type = "html", out = "tabel_regresie.html")
# Fișierul apare în directorul de lucru. În Word: File > Open > tabel_regresie.html,
# apoi copiați tabelul în document. Așa veți raporta rezultatele în proiect,
# nu prin capturi de ecran din consolă.

rm(list = ls())


# -----------------------------------------------------------------------------
# 6. TIPURI DE DATE ECONOMICE (legătura cu Cursul 1)
# -----------------------------------------------------------------------------
# Tipul datelor determină metoda. Recunoașteți-le după STRUCTURA tabelului:
# ce reprezintă o linie și ce identificator are.

# 6.1 Date cross-section (secțiune transversală)
# O linie = o unitate (persoană, firmă, țară) la un moment dat. Nu există
# dimensiune temporală. Ordinea liniilor nu contează.
wage1 <- read.csv("wage1.csv") %>% select(wage, lwage, educ, exper, tenure, married, female)
head(wage1, 5)
stargazer(wage1, type = "text")
# Identificator: individul i (implicit, numărul liniei).   Metode: C2–C7, C9–C11.

# 6.2 Serii de timp
# O linie = un moment de timp (an, trimestru, lună). Ordinea CONTEAZĂ, iar
# observațiile vecine sunt de regulă corelate între ele (autocorelare, C6).
prminwge <- read.csv("prminwge.csv") %>% select(year, avgmin, avgcov, prunemp, prgnp)
#   avgmin – salariul minim mediu     avgcov – gradul de acoperire al legii
#   prunemp – rata șomajului           prgnp  – PNB, Puerto Rico
head(prminwge, 5)
range(prminwge$year)             # 1950–1987, 38 de ani
stargazer(prminwge, type = "text")

ggplot(prminwge, aes(x = year, y = avgmin)) +
  geom_line() + geom_point() +
  labs(title = "Salariul minim în Puerto Rico, 1950–1987", x = NULL, y = "USD/oră")
# Trendul crescător este evident. Două serii cu trend vor fi corelate chiar
# dacă nu au nicio legătură: "regresia spurioasă" din Cursul 1.

# 6.3 Pooled cross sections (secțiuni transversale reunite)
# Mai multe eșantioane cross-section, din momente diferite, cu UNITĂȚI DIFERITE.
# Aici: locuințe vândute în 1978 și ALTE locuințe vândute în 1981 (Kiel și
# McClain, 1995 – efectul unui incinerator asupra prețurilor).
hprice3 <- read.csv("hprice3.csv") %>% select(year, y81, price, rooms, baths, dist)
#   y81 – 1 dacă vânzarea e din 1981   dist – distanța până la incinerator (picioare)
head(hprice3, 5)
table(hprice3$year)              # 179 locuințe în 1978, 142 în 1981

hprice3 %>%
  group_by(year) %>%
  summarise(n = n(), pret_mediu = mean(price), camere_medii = mean(rooms))
# Prețul mediu a crescut de la ≈ 76.600 la ≈ 120.600 USD. Nu putem urmări aceeași
# locuință în timp, dar putem compara grupuri "înainte / după" – baza metodei
# diferenței în diferențe (DiD) din Cursul 1.

# 6.4 Date panel (longitudinale)
# ACELEAȘI unități observate în mai multe perioade. Fiecare linie are DOI
# identificatori: unitatea (nr) și timpul (year).
wagepan <- read.csv("wagepan.csv") %>% select(nr, year, lwage, educ, exper, hours, union, married)
head(wagepan, 10)                # observați: primele 8 linii sunt individul 13, 1980–1987
length(unique(wagepan$nr))       # 545 de indivizi
table(wagepan$year)              # 8 ani × 545 = 4.360 de observații: panel echilibrat

# Traiectoriile individuale ale salariului, plus media pe fiecare an
ggplot(wagepan, aes(x = year, y = lwage, group = nr)) +
  geom_line(alpha = 0.08) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", colour = "red", linewidth = 1.2) +
  labs(title = "545 de traiectorii salariale, 1980–1987 (roșu = media anuală)",
       x = NULL, y = "log(salariu)")
# Avantajul datelor panel: putem compara fiecare individ cu el însuși în timp,
# eliminând caracteristicile fixe neobservate (abilitate, motivație) – C12–C13.

# Sinteză:
#   tip de date        o linie =                   identificator(i)     exemplu
#   cross-section      o unitate, un moment        i                    wage1
#   serie de timp      un moment                   t                    prminwge
#   pooled cross-sec.  o unitate, un moment;       i, t (unități         hprice3
#                      unități diferite în timp    diferite la t diferit)
#   panel              o unitate, un moment;       i și t                wagepan
#                      aceleași unități în timp


# -----------------------------------------------------------------------------
# 7. EXERCIȚII (de rezolvat în seminar sau acasă)
# -----------------------------------------------------------------------------
# 1. Din wage1, calculați salariul mediu pe patru grupuri: femei/bărbați ×
#    căsătorit/necăsătorit (group_by cu două variabile). Comentați.
# 2. Testați dacă salariul mediu al femeilor diferă de cel al bărbaților
#    (t.test(wage ~ female, ...)). Scrieți H0 și H1 înainte de a rula testul.
# 3. Estimați lwage ~ educ + exper + tenure + female. Cu cât diferă, în procente,
#    salariul femeilor de cel al bărbaților, la educație, experiență și vechime
#    egale? Comparați cu diferența brută de la exercițiul 2.
# 4. Din hprice3, calculați prețul mediu în 1978 și 1981 separat pentru
#    locuințele aflate la mai puțin de 15.000 de picioare de incinerator
#    (dist < 15000) și pentru celelalte. Ce observați?
# 5. Pentru proiect: alegeți o temă și identificați un set de date real
#    (INS TEMPO, Eurostat, Banca Mondială, BNR). Stabiliți ce tip de date sunt
#    și ce variabilă va fi cea dependentă. Aduceți-l la seminarul următor.
