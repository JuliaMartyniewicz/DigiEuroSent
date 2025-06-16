# Media portrayals of the digital euro: a sentiment analysis of media coverage

## Master's Thesis - Computational Social Science - Universidad Carlos III de Madrid

## Introduction
This repository contains the code for a Master's thesis that aims to provide analysis of sentiment and emotional tone in traditional media in the context of the digital euro. This project aims to fill a significant research gap regarding the role of media narratives in shaping public perception and acceptance of this key initiative of the European Central Bank.

## Research Objective
The main objective of the thesis was to investigate how the digital euro is portrayed in traditional media in terms of sentiment. Additionally, it analyses the evolution of sentiment over time, differences between countries and dominant emotions in media coverage. A Computational Social Science approach was used, drawing on the theories of Agenda Setting, Framing Theory and Technology Acceptance Model, as well as the interaction between the "supply side" (media) and the "demand side" (public opinion).

## Data
The data used in the analysis comes from the INFOMEDIA platform and includes headlines and summaries of newspaper, TV and radio articles from 2022-2025. **It should be emphasized that due to the agreement with the European Central Bank and the nature of the data, the raw database cannot be made available in this repository.** However, the scripts included in the repository show the full methodological and analytical process, which allows for replication of the study with access to analogous datasets.

## Repository Structure
The repository contains three main R scripts, reflecting the subsequent stages of the research process:
* `data_cleaning.R`: Script responsible for the process of cleaning, transforming and pre-processing the data, preparing it for further sentiment analysis.
* `headlines_analysis.R`: Script containing code to perform detailed sentiment analysis of media headlines, including identification of time trends, differences between countries, media types, media sources and ideological groups.
* `summaries_analysis.R`: Script dedicated to the analysis of sentiment and emotional tone in article summaries, including the same tools for analysis as in the case of headlines.

## Key Results
* Overall sentiment towards the digital euro in traditional media is **neutral with a slight positive trend**, showing **significant fluctuations over time**.
* There is **significant regional variation**, with media in countries such as Croatia, Lithuania and Romania showing more positive sentiment than, for example, the UK, Czechia or Hungary.
* Analysis of media types shows that **headlines often have more mixed sentiment (including negative sentiment), while summaries are consistently more positive and balanced**, indicating a headline-grabbing strategy and a media focus on credibility in the content.
* The analysis confirms that the media discourse is governed by a strong emotional conflict: on the one hand, **trust and anticipation** dominate, but on the other, **fear** is almost as strong, which indicates a deeply polarized narrative, torn between faith in the project and the media focus on potential risks.

## How to run the code (Note)
To run the R scripts included in this repository, you must have access to the source data that matches the structure used in the study. Due to the aforementioned privacy restrictions, the data files are not publicly available. However, the scripts are fully commented and illustrate the data analysis methodology used.

### Acknowledgements
I would like to express my gratitude to the European Central Bank for providing access to the INFOMEDIA database, which was the basis for this Master's thesis. Their support enabled me to conduct a detailed analysis of the media sentiment of the digital euro.
Special thanks to my supervisor, Francisco Villamil, for his valuable advice and inspiring discussions at every stage of the work.
