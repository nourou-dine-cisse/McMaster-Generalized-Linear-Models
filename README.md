# Generalized Linear Models (GLM): Theory & Applied Statistical Inference in R

> **Cadre académique** : McMaster University (Canada) — Advanced Statistical Inference & Generalized Linear Models (GLM)  
> **Auteur** : Mohamed Nourou Dine CISSE  
> **Domaine** : `Applied Statistics, Exponential Dispersion Models & Biostatistical Inference`  
> **Stack Technique** : `R` `R Markdown` `Famille Exponentielle` `Régression Logistique` `Régression de Poisson` `Quasi-Vraisemblance` `Déviance`

---

## 📌 Présentation Réelle du Projet (Honest Overview)

Ce dépôt rassemble les travaux théoriques et empiriques réalisés dans le cadre du cours de **Modèles Linéaires Généralisés (GLM)** à **McMaster University** (Canada). 

Les modèles linéaires gaussiens classiques s'avèrent inadaptés lorsque les variables réponses sont binaires (succès/échec), de comptage (nombres entiers positifs discrets) ou continues fortement asymétriques à variance dépendante de la moyenne. Le cadre des GLM unifie ces situations au sein de la **famille exponentielle** en introduisant une fonction de lien monotone $g(\mu_i) = \eta_i = X_i^\top \beta$.

Le répertoire compile 5 devoirs approfondis (*Assignments*) résolus sous R Markdown ainsi que les scripts de cours illustrant les diagnostics sur des jeux de données réels (Pima Indians Diabetes, O-ring Space Shuttle, Admissions universitaires, Fruitfly lifespan, etc.).

---

## 🏗️ Structure du Répertoire

```text
13_McMaster_Generalized_Linear_Models/
├── README.md                      # Documentation complète
├── assignments_and_reports/       # Devoirs académiques et rapports de modélisation
│   ├── Assignment 1/              # Régression linéaire multiple et diagnostics de résidus
│   │   ├── question1.Rmd / .pdf
│   │   └── question3.Rmd / .pdf
│   ├── Assignment 2/              # Régression logistique binomiale (données Pima & Fastener)
│   │   ├── Q1.Rmd, Q2.Rmd, Q3.Rmd
│   │   └── Pima.csv, Fastener.csv, Shuttle.csv
│   ├── Assignment 3/              # Analyse de la déviance, sélection AIC et liens probit / cloglog
│   │   ├── Question1.Rmd, question2.Rmd
│   │   └── Admissions.csv
│   ├── Assignment 4/              # Données de comptage : régression de Poisson et surdispersion
│   │   ├── Question1.Rmd / .pdf
│   │   ├── Question.2.Rmd / .pdf
│   │   ├── Question3.Rmd / .pdf
│   │   └── Quine.csv
│   └── Assignment 5/              # Quasi-vraisemblance et modèles Gamma (longévité Fruitfly)
│       ├── question1.Rmd / .pdf
│       ├── question2.Rmd / .pdf
│       ├── Fruitfly.csv
│       └── Happiness.csv
└── src/                           # Scripts de cours et datasets d'illustration
    ├── Jan05.R, Jan28.R, Feb11.R, Mar02.R, etc. # Implémentations R pas-à-pas
    ├── Cars.csv                   # Données automobiles
    ├── Shuttle.csv                # Données incidents joints toriques Challenger
    └── Teens.csv                  # Données sociologiques
```

---

## ⚙️ Fondements Théoriques Couverts

1. **Famille Exponentielle & Fonctions de Variance** :
   $$f(y; \theta, \phi) = \exp\left(\frac{y\theta - b(\theta)}{a(\phi)} + c(y, \phi)\right)$$
   Moyenne $\mathbb{E}[Y] = b'(\theta) = \mu$, Variance $\text{Var}(Y) = b''(\theta) a(\phi) = V(\mu) a(\phi)$.
2. **Fonctions de Lien Canoniques & Alternatives** :
   - Binomiale : Logit ($\ln \frac{\mu}{1-\mu}$), Probit ($\Phi^{-1}(\mu)$), Complementary Log-Log ($\ln(-\ln(1-\mu))$).
   - Poisson : Log ($\ln \mu$).
   - Gamma : Inverse ($-1/\mu$).
3. **Inférence & Diagnostic** :
   - Algorithme de scoring de Fisher / Moindres Carrés Pondérés Itératifs (IRLS).
   - Test du rapport de vraisemblance basé sur la déviance résiduelle $D(y; \hat{\mu})$.
   - Modélisation de la surdispersion via des modèles quasi-Poisson ou binomiaux négatifs.

---

## 🚀 Guide d'Exécution sous R

Pour tricoter l'un des devoirs (ex. Devoir 2) :
```R
# Installation des packages recommandés
install.packages(c("MASS", "ggplot2", "knitr", "rmarkdown", "car"))

# Compilation du document
rmarkdown::render("assignments_and_reports/Assignment 2/Q1.Rmd", output_format = "pdf_document")
```

---

## 📬 Profil de l'Auteur
- **Mohamed Nourou Dine CISSE**
- **ENSIIE** & **Université Paris-Saclay** — Double diplôme, Master 2 Intelligence Artificielle (M2IA)
- Échange académique : **McMaster University** (Canada)
- **GitHub** : [@nourou-dine-cisse](https://github.com/nourou-dine-cisse)
