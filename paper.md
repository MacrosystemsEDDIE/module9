---
title: 'Macrosystems EDDIE Module 9: Using High-Frequency Data to Manage Water Quality'
tags:
  - environmental science
  - resource management
  - lake
  - reservoir
  - drinking water
authors:
  - name: Mary E. Lofton
    orcid: 0000-0003-3270-1330
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
  - name: Rosa-Lee Cooke
    affiliation: 3
  - name: Cayelan C. Carey
    orcid: 0000-0001-8835-4476
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
affiliations:
 - name: Center for Ecosystem Forecasting, Virginia Tech
   index: 1
 - name: Department of Biological Sciences, Virginia Tech
   index: 2
 - name: Department of Industrial Technology, Mountain Empire Community College
   index: 3
date: 23 August 2024
bibliography: paper.bib
---

# Summary

'Macrosystems EDDIE Module 9: Using High-Frequency Data to Manage Water Quality' was developed as part of a virtual, asynchronous curriculum for community college students training to become drinking water treatment plant operators. In recent decades, there have been substantial improvements in our ability to monitor water quality in real time using sensors that measure variables at a high frequency (every few minutes). In this module, students explore data collected using high-frequency sensors and learn how to interpret these data to inform water quality management in a drinking water treatment plant. The focal question that students work to answer during the module is: How can we use high-frequency data to improve water quality? The module is designed to be fully accessible for both in-person, hybrid, and virtual, asynchronous courses. For in-person or synchronous courses, this entire module can be completed in one 2 to 3-hour lab period, two 75-minute lecture periods, or three 1-hour lecture periods. Students complete module activities using an R Shiny web application, and can answer questions either by typing them into a student handout, which can be downloaded from the app, or by using a Canvas quiz, which can be imported into Canvas from the Canvas commons. Module materials include source code and datasets for the R Shiny web application, instructional videos and slide decks, a student handout with question prompts, and an instructor manual with detailed instructions for module delivery in diverse course contexts as well as an answer key for student questions. Open-source versions of all module materials are available in the module GitHub repository. 

# Statement of Need


...

# Instructional design

This module was developed as part of the Macrosystems EDDIE program, which introduces students to quantitative skills, such as ecological modeling and forecasting, through use of real-world environmental datasets [@carey_macrosystems_2020; @hounshell_macrosystems_2021]. Macrosystems EDDIE modules are developed using the 5E (Engagement, Exploration, Explanation, Expansion, Evaluation) instructional model [@bybee_bscs_2006]. Each module comprises three activities (A, B, and C), which are scaffolded such that Activity A Engages students and asks them to Explore the module topic; Activity B Explains more about the topic to Expand on Activity A; and Activity C Evaluates students' understanding of the topic [@oconnell_project_2024]. The three-part module structure also allows instructors to choose whether to complete just Activity A, Activities A and B, or all three Activities, maximizing the adaptibility of modules to various course contexts. 

Module activities and questions are designed to enhance student engagement [one paragraph about student engagement through real-world problem-solving and place-based learning]

# Learning Objectives

By the end of this module, students will be able to:

1. Define key measures of surface freshwater quality (water temperature, dissolved oxygen, and turbidity). 
2. Explain how water temperature changes over the course of a year in a temperate reservoir and how these changes affect water quality. 
3. Interpret high-frequency water quality data to make decisions about water extraction depth for a drinking water reservoir. 
4. Evaluate water quality data and forecasts to make decisions about drinking water treatment. 

# Module Activities

This module comprises three activities (A, B, C) which guide students towards answering the focal question: **How can we use high-frequency data to improve water quality?** Below is a brief overview of the module activities.

## Activity A 

In Activity A, students access and explore high-frequency water quality data from a drinking water reservoir in southwest Virginia. Students may choose to explore data from one of two drinking water supply reservoirs in southwest Virginia, USA: Falling Creek Reservoir (abbreviated as FCR) or Beaverdam Reservoir (BVR). Both waterbodies are located in Vinton, VA and are owned and operated by the Western Virginia Water Authority. FCR and BVR are monitored regularly as part of a Long-Term Research in Environmental Biology (LTREB) program which is funded by the National Science Foundation (NSF). As a result, both reservoirs have at least six years of high-frequency water temperature, dissolved oxygen, and turbidity data which students can visualize and interpret to complete module activities. Activity A includes two objectives. In Objective 1, students learn about their focal reservoir site, and answer questions about its size, location, use, and average water quality conditions. In Objective 2, students view recent (within the past two years) environmental data from their site, and are asked to interpret these data in the context of drinking water quality. For example, students view a short video explaining how high turbidity (indicating low water clarity) can complicate drinking water treatment due to high concentrations of particulate matter on treatment plant filters. Then, they plot turbidity data from their focal reservoir and are asked to identify times during the year when turbidity might be high enough to cause water quality concerns.

## Activity B

In Activity B, students use high-frequency water quality data to explore how water quality changes and make decisions about water withdrawal depth over the course of a year. They are also introduced to the concept of water quality forecasting, which can provide predictions of future water quality to managers to enable pre-emptive management action to avoid water quality impairment. Activity B includes two objectives. In Objective 3, students view environmental data from three different seasons (summer, fall, winter) at their chosen reservoir data. They are asked to assess how water quality differs among seasons, and use this information to decide from which depth in the reservoir to extract water for treatment at a drinking water treatment plant. Ultimately, students should choose the depth which maximizes the quality of the water flowing from the reservoir into the drinking water treatment plant, as this water will be the easiest and most cost-efficient to treat to meet drinking water quality standards. In Objective 4, students are asked to interpret a fall turnover forecast. Fall turnover is a seasonal event in many inland waterbodies located in temperate regions. During fall turnover, water temperatures change very quickly, and this can lead to rapid changes in the chemical and biological attributes of water quality, potentially complicating drinking water treatment. A forecast of fall turnover could allow drinking water treatment plant managers to anticipate water quality impairment and prepare for additional treatment measures, thus ensuring they are able to meet drinking water quality standards during the period of fall turnover.

## Activity C

In Activity C, students make management recommendations for water treatment using water quality forecasts. Activity C includes one objective. In Objective 5, students are given a drinking water treatment plant operation scenario and asked to decide whether to enact additional treatment measures to ensure that they meet regulatory standards for water clarity. Student are asked to make decisions using both "real-time" water quality data as well as water quality forecasts over time. At the end of the objective, students are given the opportunity to evaluate the quality of their decisions after the fact, as well as evaluate the utility of both the "real-time" data and the water quality forecasts to their decision-making process.

# Module Materials

# Context for Use

include audience here

# Module Development Background

Tell us the “story” of the project: how did it come to be?

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this: ![Example figure.](mod9_conceptual_figure.png)

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References