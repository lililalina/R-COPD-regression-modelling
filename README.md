# COPD Regression Modelling in R

This project is part of the **"Linear Regression in R for Public Health"** course by Imperial College London on Coursera. The analysis focuses on identifying factors associated with **Quality of Life (SGRQ score)** in patients with **Chronic Obstructive Pulmonary Disease (COPD)** using linear regression models in R.

---

## 📊 Objective

To evaluate which clinical and demographic factors are associated with variations in the **St. George’s Respiratory Questionnaire (SGRQ)** scores among COPD patients.

---

## 🧪 Dataset

- **Source:** Provided in course materials
- **File:** `COPD_student_dataset.csv`
- **Description:** Contains variables such as age, smoking status, lung function measures, exercise capacity, and SGRQ scores.

---

## 🛠 Tools & Technologies

- **Language:** R
- **Libraries:** `ggplot2`, `dplyr`, `car`, `psych`, `stargazer`
- **Reporting:** R Markdown (`.Rmd`) to PDF (`.pdf`)
- **Documentation:** Word report included (`analysis-report.docx`)

---

## 📈 Analysis Summary

- Performed **univariate** and **multivariate** linear regression.
- Evaluated assumptions: linearity, normality, multicollinearity, residual plots.
- Key findings:
  - Age, smoking status, and exercise tolerance were significant predictors of lower quality of life.
  - Model diagnostics confirmed the appropriateness of the linear regression assumptions.

---

## 📂 Project Files

| File | Description |
|------|-------------|
| `COPD_student_dataset.csv` | Main dataset used for analysis |
| `copd-multivariate-modelling.Rmd` | R Markdown script for full analysis |
| `copd-multivariate-modelling.pdf` | Rendered PDF report from `.Rmd` |
| `analysis-report.docx` | Summary of analysis in Word format |
| `copd-12-467.pdf`, `10.1177_1479972317694622.pdf` | Reference articles for background |
| `.RData`, `.Rhistory` | Session data (optional to use) |

---

## 📊 Example Output

Below is an example of a regression summary table and diagnostic plot used in the analysis:

https://github.com/lililalina/R-COPD-regression-modelling/blob/main/pics/table%20for%20model%20mlr.png?raw=true
https://github.com/lililalina/R-COPD-regression-modelling/blob/main/pics/resvsfit-mlr6.png?raw=true
https://github.com/lililalina/R-COPD-regression-modelling/blob/main/pics/11-mlr6.png?raw=true
https://github.com/lililalina/R-COPD-regression-modelling/blob/main/pics/scale-mlr6.png?raw=true
https://github.com/lililalina/R-COPD-regression-modelling/blob/main/pics/scale-mlr6.png?raw=true

---

## 🚀 How to Reproduce

1. Clone the repository:
   ```bash
   git clone https://github.com/lililalina/R-COPD-regression-modelling.git
