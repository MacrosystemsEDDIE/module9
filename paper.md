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

'Macrosystems EDDIE Module 9: Using High-Frequency Data to Manage Water Quality' is an educational module in which students explore real-world data collected using high-frequency sensors in drinking water reservoirs and interpret these data to inform water quality management. The focal question that students address during the module is: How can we use high-frequency data to improve water quality? This module was developed as part of a curriculum for community college students training to become drinking water treatment plant operators, and could also be taught in high school or introductory undergraduate environmental science and natural resource management courses. The one- to three-hour module is designed to be flexible for use in-person, hybrid, and virtual, asynchronous course formats. Students complete module activities using an R Shiny web application which can be accessed from an internet browser on a computer. Module materials include source code and datasets for the R Shiny web application, instructional videos and slide decks, a student handout with question prompts, and an instructor manual. Open-source versions of all module materials are available in the module GitHub repository and published in the Environmental Data Initiative repository. 

# Statement of Need

In recent decades, there have been substantial improvements in our ability to monitor water quality in real time using sensors that measure variables at a high frequency [e.g., every few minutes rather than once a week or once a month; @bieroza_advances_2023]. These high-frequency data have tremendous potential to inform drinking water management [@marce_automatic_2016; @seifert-dahnn_costs_2021; @bertone_probabilistic_2023], and use of real-time high-frequency water quality data to inform water resource management is increasing [e.g., @ruberg_societal_2008; @tran_khac_automatic_2018; @zhan_high-frequency_2022]. In some large drinking water treatment plants, water treatment workflows using high-frequency data are completely automated [@worm_integration_2010]. However, many drinking water treatment plants are small, with manual operation procedures, even in well-resourced nations [e.g., @stein_performance_2017], necessitating drinking water treatment operators to use their own judgment and experience to make decisions about water treatment based on past, current, and expected future water quality.

In addition to directly informing water management decision-making, collection of high-frequency water quality data has enabled recent development of water quality forecasts, or predictions of the future water quality conditions with uncertainty [@lofton_progress_2023]. Often, water quality forecasts are developed with the goal of informing and improving water treatment and management [e.g., @carey_advancing_2022]. However, few managers currently incorporate water quality forecasts into their decision-making workflows [reviewed by @lofton_progress_2023; but see @baracchini_meteolakes_2020].

To teach water treatment students how to use high-frequency data and forecasts to improve water quality, we developed a short (one- to three-hour) module for the Mountain Empire Community College (MECC) Water program (Big Stony Gap, VA, USA). The MECC Water program is the only one of its kind in the southern Appalachian region of the United States, a region which receives a high proportion of its drinking water from reservoirs in small, mountainous catchments with mall treatment plants. The module teaches key skills in visualizing and interpreting high-frequency data and forecast that are applied to drinking water treatment scenarios.

# Instructional design

This module was developed as part of the Macrosystems EDDIE (Environmental Data-Driven Inquiry and Exploration) educational program, which introduces students to quantitative skills, such as data visualization, interpretation, modeling, and forecasting, through analysis of real-world environmental datasets [@carey_macrosystems_2020; @hounshell_macrosystems_2021; @lofton_modular_2024]. Macrosystems EDDIE modules are developed using the 5E (Engagement, Exploration, Explanation, Expansion, Evaluation) instructional model [@bybee_bscs_2006]. Each module comprises three activities (A, B, and C), which are scaffolded to sequentially build on each other. Activity A Engages students and asks them to Explore the module topic; Activity B Explains more about the topic to Expand on Activity A; and Activity C Evaluates students' understanding of the topic [@oconnell_project_2024]. The three-part module structure also provides instructors the flexibility to choose whether to complete just Activity A, Activities A and B, or all three Activities, maximizing the adaptibility of modules to various course contexts. 

All module activities and questions are designed to enhance student engagement through real-world problem-solving [@villarroel_authentic_2018] and place-based learning [@semken_sense_2008]. To engage students in real-world problem-solving, we developed realistic decision-making scenarios for students to complete that mirror the past experiences and water treatment challenges of operators working at the same drinking water reservoirs that are included in the module (see descriptions of Activity B and C in Module Activities for further details). To engage students in place-based learning, we specifically chose drinking water reservoir sites located in the same region as the community college where we taught the module (southwest Virginia, United States). 

# Learning Objectives

By the end of this module, students will be able to:

1. Define key measures of freshwater quality (water temperature, dissolved oxygen, and turbidity). 
2. Explain how water temperature changes over the course of a year in a temperate reservoir and how these changes affect water quality. 
3. Interpret high-frequency water quality data to make decisions about water extraction depth for a drinking water reservoir. 
4. Evaluate water quality data and forecasts to make decisions about drinking water treatment. 

# Module Activities

This module comprises an introduction and three activities (A, B, C) which guide students towards answering the focal question: **How can we use high-frequency data to improve water quality?** Below is a brief overview of the module activities:

- **Introduction**: Students are introduced to key concepts covered in the module and select a focal drinking water reservoir site to work with during the module.
- **Activity A**: Students access and explore high-frequency water quality data from a drinking water reservoir in southwest Virginia. 
- **Activity B**: Students use high-frequency water quality data to explore how water quality changes over the course of a year and make decisions about water withdrawal depth (i.e., the depth from which water will be extracted for treatment at a drinking water treatment plant).  
- **Activity C**: Students make management recommendations for water treatment to meet regulatory standards for turbidity (water clarity) using high-frequency data and water quality forecasts. 

# Context for Use

This module was originally developed as part of a virtual, asynchronous curriculum for community college students training to become drinking water treatment plant operators. While the module is targeted for introductory environmental science and natural resources community college and undergraduate students in Appalachia, it can be broadly adapted for classrooms across the USA and in other countries, with some small modifications (e.g., changes in water quality thresholds, unit conversions).

All module materials are designed to be fully accessible for in-person, hybrid, and virtual, asynchronous courses. For in-person or synchronous courses, this entire module can be completed in one 3-hour lab period, two 75-minute lecture periods, or three 1-hour lecture periods. Students complete module activities using an R Shiny web application, and can answer questions either by typing them into a student handout which can be downloaded from the app, or by using a Canvas quiz which can be imported from the Canvas commons (Canvas commons link). Module materials include: source code and datasets for the R Shiny web application; instructional videos and slide presentations; a student handout with question prompts; and an instructor manual. The instructor manual includes both detailed instructions for module delivery in diverse course contexts as well as an answer key for student questions. Open-source versions of all module materials are available in the module GitHub repository (https://github.com/MacrosystemsEDDIE/module9) and all module code is published on Zenodo (Zenodo DOI). Editable and open-source versions of all instructional materials (videos, slide presentations, student handout and instructor manual) are also hosted online through the Science Education Resource Center at Carleton College (SERC; https://serc.carleton.edu/eddie/teaching_materials/modules/module9.html) and are published in the Environmental Data Initiative repository (EDI DOI). 

This module is currently included in the curricula for ENV 211: Sanitary Biology and Chemistry in the Environmental Science Water/Wastewater Specialization and **Course YYY in the Program ZZZ** at Mountain Empire Community College, Big Stone Gap, VA, USA.

# Module Development Background

This module was developed through a partnership between Rosa-Lee Cooke, lead instructor for environmental sciences and water/wastewater treatment at Mountain Empire Community College, and Virginia Tech researchers Mary E. Lofton and Cayelan C. Carey, who lead Macrosystems EDDIE and have previously developed undergraduate educational materials related to freshwater science and ecological forecasting [@carey_macrosystems_2020; @hounshell_macrosystems_2021; @lofton_modular_2024]. Module development was supported by the National Science Foundation (EF-2318861).  

# Acknowledgements

We thank Jamie Morris of the Western Virginia Water Authority for guidance regarding regulatory standards for turbidity and water treatment operations at Falling Creek Reservoir. We also thank Michael Kricheldorf, Sierra Tannheiser, and Tallory Wendall for their feedback on early versions of the module.

# References