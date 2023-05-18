### "Don't Procrastinate, Vaccinate"
#### An Experiment to Understand the Causal Effects of Raising Awareness on Preventive Healthcare
##### W241: Experiments and Causality, Spring 2020
##### Alex Dessouky, Matt Kawa, Sonali Serro

###### Project Structure
```
.
├── Makefile
├── README.md
├── W241_Final_Project_Presentation.pdf
├── data
│   ├── analysis
│   │   └── w241_Final_Project_Analysis.csv
│   ├── images
│   │   ├── consort_diagram.png
│   │   ├── flu_vaccination_coverage_2010-2019.jpeg
│   │   ├── outcome_measurement.png
│   │   └── power_curve.jpeg
│   └── raw
│       ├── W241_Final_Project_Pilot_Raw.csv
│       └── W241_Final_Project_Raw.csv
├── src
│   ├── analysis.R
│   ├── analysis_pilot.R
│   ├── clean_data.R
│   ├── common.R
│   ├── get_data.R
│   ├── power_analysis.R
│   └── run_report.R
├── w241_Final_Project_Paper.Rmd
└── w241_Final_Project_Paper.pdf
```

###### Project Report Pipeline
| make command      | What it does
|-------------------|----------------------------------|
| make clean        | Delete all local data files      |
| make get_data     | Grab data from an online version |
| make clean_data   | Pre-process the data files       |
| make run_report   | Generate the Report              |
| make run_pipeline | Run above steps in succession    |
