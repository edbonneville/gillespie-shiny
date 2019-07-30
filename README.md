## Simulating stochastic epidemics

This R Shiny app is an addendum to my [master's thesis](https://www.universiteitleiden.nl/binaries/content/assets/science/mi/scripties/statscience/2018-2019/thesis_bonneville_s1914944.pdf), which was on the statistical forecasting of infectious disease epidemics using the time between successive infections (infection interevent times).

The app allows the user to simulate and explore susceptible-infectious (SI), susceptible-infectious-susceptible (SIS) and susceptible-infectious-recovered (SIR) type epidemics stochastically with the [Gillespie algorithm](https://en.wikipedia.org/wiki/Gillespie_algorithm). All source code is available on this [Github repository](https://github.com/edbonneville/gillespie_shiny).

<img src="app_screenshot.png" width="800">

### Running it locally

To run the app locally on your personal machine, you need to first install the packages `shiny`, `shinydashboard`, `plotly`, `shinyWidgets`, `tidyverse`, and `deSolve`.

You can then run the following chunk of code:

``` r
library(shiny)
shiny::runGitHub("gillespie_shiny", "edbonneville")
```

### Features and usage

The panel on the left allows you to select the:

* Epidemic type: SI, SIS or SIR (without demographics).
* Infection parameter &beta; scaled per year.
* Average infectious period 1/&gamma; in days, in the case of SIS or SIR.
* Shape parameter &alpha; of the Gamma distribution of interevent times. By default this is exponential (&alpha; = 1), but it can be modified.
* Initial population conditions: number of susceptible *S*, infectious *I* and total population size *N*. Note that *N = S + I*, and for computational reasons we restrict *N* &le; 1000. 
* Time frame *t*<sub>end</sub>, i.e. how long we should simulate for. For SI epidemics, the susceptible pool may be depleted before *t*<sub>end</sub>. Analogously for SIR epidemics, the entire population may have recovered before *t*<sub>end</sub>. We restrict *t*<sub>end</sub> to a maximum of 365 days.

On the right panel, in addition to visualising the evolution of the number of susceptible, infectious and recovered individuals over time, you can also: 

* Overlay the deterministic (ODE solutions) curves, or view them on their own.
* Visualise the rates of the different events (infections or recoveries) over time.
* View the distribution of infection interevent times.
* Plot infection interevent times against the number of infectious.

The last two features are directly relevant to the thesis. Finally, you can also download a copy of the simulated dataset as a .csv.

**Note**: in the SIS and SIR case, it is possible after pressing 'Run' that the epidemic will end prematurely. This may be due to a) your chosen parameterisation (e.g. &beta; < &gamma;), or b) due to the initial conditions. If you start with *I* = 1, the first drawn event (by chance) may be a recovery, leading to extinction. For the latter case, you can either run the simulation again, of increase your initial *I* to ensure the epidemic takes off.

### Downloading data and running a Gamma GLM with inverse link

If you would like to work further along the lines of the thesis, you can model the infection interevent times using a Gamma Generalised Linear Model (GLM) with an inverse link function. 

With current app, let us assume you simulate an epidemic under an SIS model and download the data as "SIS_sim". You can then run the following code:

``` r
library(tidyverse)

dat <- read.csv("SIS_sim.csv") 

dat_glm <- dat %>% 
  
  # Subset only infections
  filter(transition == "infection") %>% 
  
  # Compute interevent times
  mutate(interev = t - c(0, t[-length(t)])) %>% 
  
  # Remove first row (interev = 0, since first event)
  slice(-1) %>% 
  
  # Add explanatory variables
  mutate(x1 = I, x2 = I^2)

# Run gamma glm with inverse link
glm(interev ~ x1 + x2, family = Gamma(link = "inverse"), data = dat_glm)
```

### Acknowledgements and extra notes

The layout of the app was in part inspired by the ["Explore ID Model"](http://www.seabbs.co.uk/shiny/exploreidmodels/) Shiny app developed by [Sam Abbott](https://www.samabbott.co.uk/) at the University of Bristol.

Feel free to use the app and source code for educational and/or personal purposes, and if you have any questions or issues to report feel free to contact me on 
<e.f.bonneville@lumc.nl>.

Edouard Bonneville
