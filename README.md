# rhapsody

A domain-agnostic ODE and difference-equation simulator with an interactive Shiny GUI. rhapsody is designed to facilitate writing a model in plain text, exploring it interactively with sliders, and export results, without writing boilerplate solver code.

## Features

- **Live editor** — write ODEs, difference equations, initial conditions, and parameters in a minimal plain-text syntax; the model is re-parsed on every keystroke
- **Interactive sliders** — parameter values update the plot in real time (Live mode) or on demand
- **Events** — time-based (`at(t == 50): X = X + 30`) and state-based (`at(X > 80): r = r * 0.5`) discontinuities
- **Auxiliary variables** — derived quantities plotted alongside state variables
- **Solver panel** — override time settings (`t0`, `tmax`, `dt`) and choose solver method (`lsoda`, `rk4`, etc.)
- **Parameter scan** — sweep one parameter across a range; display as overlaid curves or a summary statistic plot (max, min, mean, final)
- **FFT analysis** — spectral analysis with Hann, Hamming, and Blackman windows and transient trimming
- **Steady-state finder** — run-to-SS and Newton methods via rootSolve; one-click "Use as Initial Conditions"
- **Import** — load `.rhy` session files or Berkeley Madonna (`.mmd`) models
- **Export** — save as `.rhy`, deSolve R script (`.R`), Berkeley Madonna (`.mmd`), or CSV simulation output
- **Reports** — generate HTML or Word reports with model source, parameter table, time-series plot, and simulation table; download a standalone deSolve R script

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("kestrel99/rhapsody")
```

## Usage

```r
library(rhapsody)
rhapsody::run_app()
```

The app opens in your browser with a Lotka-Volterra predator-prey model loaded. Edit the model, adjust sliders, and click **Run**.

## Model Syntax

Models are written in a plain-text format:

```
# One-compartment PK with oral absorption and Emax PD
dA/dt = -ka * A
dC/dt = ka * A / Vd - ke * C

A[0] = 100
C[0] = 0

ka   = 1.0   # [0.1, 5]
ke   = 0.2   # [0.05, 1]
Vd   = 20    # [5, 100]
Emax = 10    # [1, 20]
EC50 = 2     # [0.1, 10]

E = Emax * C / (EC50 + C)

t0   = 0
tmax = 24
dt   = 0.1
```

| Syntax | Meaning |
|--------|---------|
| `dX/dt = expr` | ODE for state variable X |
| `X[t+1] = expr` | Difference equation (uses `X[t]` for current value) |
| `X[0] = value` | Initial condition |
| `name = value` | Parameter |
| `name = expr` | Auxiliary variable (if it references state variables) |
| `at(t == value): var = expr` | Time-based event |
| `at(var > value): var = expr` | State-based event |
| `t0`, `tmax`, `dt` | Simulation time settings |
| `# comment` | Line comment |

## Dependencies

| Package | Role |
|---------|------|
| shiny, bslib, shinyAce | GUI framework |
| deSolve | ODE integration |
| rootSolve | Steady-state finding |
| plotly | Interactive plots |
| rmarkdown, knitr, ggplot2 | Report generation |
| jsonlite | Session file format |
| golem | App packaging |

