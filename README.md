# Volatility Spillovers: COMEX to Pakistani Precious Metal Markets
### DCC-GARCH Analysis | Gold & Silver | 2015–2024

---

## Research Question
Do volatility shocks in US COMEX gold and silver futures markets spill over into Pakistani precious metal markets (PMEX), and is this transmission unidirectional or bidirectional?

## Motivation
Pakistan has deep cultural and financial exposure to precious metals, with PMEX offering standardised gold and silver futures denominated in PKR. Despite the intuitive linkage between COMEX benchmarks and PMEX pricing, no published study has directly estimated bilateral volatility transmission between these two markets for both gold and silver jointly. This project addresses that confirmed research gap.

---

## Methodology
- **Model:** Dynamic Conditional Correlation GARCH — DCC-GARCH (Engle, 2002)
- **First step:** Univariate GARCH(1,1) with Student-t errors for each return series
- **Second step:** Joint DCC estimation with multivariate Student-t innovations
- **Spillover direction:** Granger causality tests on squared returns (5 lags)
- **Software:** R 4.6.0 — packages: quantmod, rugarch, rmgarch, FinTS, tseries, lmtest

---

## Data
| Series | Source | Ticker | Unit |
|---|---|---|---|
| COMEX Gold | Yahoo Finance | GC=F | USD/troy oz |
| COMEX Silver | Yahoo Finance | SI=F | USD/troy oz |
| USD/PKR Exchange Rate | Yahoo Finance | PKR=X | PKR per USD |
| Pakistan Gold (PKR) | Constructed | — | PKR (= GC=F × PKR=X) |
| Pakistan Silver (PKR) | Constructed | — | PKR (= SI=F × PKR=X) |

**Sample period:** January 2, 2015 – December 31, 2024
**Observations:** 2,210 daily returns (after removing 2 corrupted PKR=X data points)

---

## Key Results

### Mean Dynamic Conditional Correlations
| Market Pair | Mean DCC |
|---|---|
| COMEX Gold — Pakistan Gold | 0.7468 |
| COMEX Silver — Pakistan Silver | 0.8813 |
| COMEX Gold — COMEX Silver | 0.7960 |
| Pakistan Gold — Pakistan Silver | 0.8202 |

### Granger Causality (Squared Returns, 5 Lags)
| Direction | F-stat | p-value | Result |
|---|---|---|---|
| COMEX Gold → Pakistan Gold | 1.742 | 0.122 | Not significant |
| Pakistan Gold → COMEX Gold | 1.817 | 0.106 | Not significant |
| COMEX Silver → Pakistan Silver | 4.628 | < 0.001 | Significant *** |
| Pakistan Silver → COMEX Silver | 2.580 | 0.025 | Significant * |

### DCC Parameters
| Parameter | Estimate | p-value |
|---|---|---|
| dcca1 (shock) | 0.0348 | < 0.001 |
| dccb1 (persistence) | 0.9591 | < 0.001 |
| a + b | 0.9939 | — |

---

## Main Findings
1. **High integration:** Mean DCC of 0.75 (gold) and 0.88 (silver) confirm strong US-to-Pakistan transmission in both metals
2. **Gold transmission is contemporaneous** — no significant lead-lag in either direction at daily frequency
3. **Silver transmission is bidirectional** — COMEX silver strongly predicts Pakistani silver volatility (p < 0.001), with a significant reverse channel (p < 0.05)
4. **Time-varying correlations** decline sharply during the 2018 and COVID-19 (2020) disruption periods before recovering to near-peak levels post-2022

---

---

## References
- Engle, R. F. (2002). Dynamic conditional correlation. *Journal of Business and Economic Statistics*, 20(3), 339–350.
- Bollerslev, T. (1986). Generalised autoregressive conditional heteroskedasticity. *Journal of Econometrics*, 31(3), 307–327.

---

*Author: Ahmer Anwaar | GitHub: ahmer-econ | July 2026*

## Repository Structure
