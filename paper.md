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

'Macrosystems EDDIE Module 9: Using High-Frequency Data to Manage Water Quality' is a one- to three-hour educational module that asks students to explore real-world data collected using high-frequency sensors in drinking water reservoirs and interpret these data to inform water quality management in a drinking water treatment plant. The focal question that students work to answer during the module is: How can we use high-frequency data to improve water quality? This module was developed as part of a virtual, asynchronous curriculum for community college students training to become drinking water treatment plant operators, and could also be taught in high school or introductory undergraduate environmental science and natural resource management courses. The module is designed to be flexible for use in-person, hybrid, and virtual, asynchronous course formats. Students complete module activities using an R Shiny web application which can be accessed from an internet browser on a computer. Module materials include source code and datasets for the R Shiny web application, instructional videos and slide decks, a student handout with question prompts, and an instructor manual. Open-source versions of all module materials are available in the module GitHub repository. 

# Statement of Need

In recent decades, there have been substantial improvements in our ability to monitor water quality in real time using sensors that measure variables at a high frequency [e.g., every few minutes; @bieroza_advances_2023]. These high-frequency data have tremendous potential to inform drinking water management [@marce_automatic_2016; @seifert-dahnn_costs_2021; @bertone_probabilistic_2023], and use of real-time high-frequency water quality data to inform water resource management is increasing [e.g., @ruberg_societal_2008; @tran_khac_automatic_2018; @zhan_high-frequency_2022]. In some large drinking water treatment plants, water treatment workflows using high-frequency data are completely automated [@worm_integration_2010]. However, the majority of drinking water treatment plants may be small, with manual operation procedures, even in well-resourced nations [e.g., @stein_performance_2017], necessitating that drinking water treatment operators use their own judgment and experience to make decisions about water treatment based on current and expected future water quality.

In addition to directly informing water management decision-making, collection of high-frequency water quality data has enabled recent development of water quality forecasts, or predictions of the future water quality conditions with uncertainty [@lofton_progress_2023]. Often, water quality forecasts are developed with the goal of informing and improving water treatment and management [e.g., @carey_advancing_2022]. However, few managers currently incorporate water quality forecasts into their decision-making workflows [@lofton_progress_2023; but see @baracchini_meteolakes_2020]. Possible reasons for the lack of uptake of water quality forecasts into management decision-making workflows include the difficulty of interpreting the uncertainty associated with forecasts [@berthet_operational_2016; @mulder_understanding_2023], as well as a perceived and/or real deficit in forecast skill [@rayner_weather_2005; @jackson-blake_opportunities_2022].

Within the United States, the southern Appalachian region receives a high proportion of its drinking water from reservoirs [RoL PD REF #27]. Many of these reservoirs are located in small, high-elevation catchments with correspondingly small treatment plants in high-poverty communities [e.g., maybe Appalachian Voice ref?]. Currently, the only community college water treatment training program in the region is located at Mountain Empire Community College (MECC) in far southwestern Virginia. The MECC Water program graduates ~75 students/year, many of whom enroll in MECC's virtual curriculum from the larger surrounding region, including from out-of-state. Consequently, embedding visualization and interpretation of high-frequency water quality data and forecasts into the MECC Water curriculum enables training of dozens of water treatment operators per year on the utility of high-frequency data for informing water treatment decision-making, many of whom will subsequently be employed in the southern Appalachian region. 

To address the need for curriculum to introduce water treatment students to use of high-frequency data and forecasts to improve water quality, we developed a short (one- to three-hour) module for the MECC Water program. The module develops key skills in high-frequency data and forecast visualization and interpretation that can be applied across a variety of natural resource management topics, and the module materials and activities are designed to be easily adaptable for multiple course formats by the instructor.

# Instructional design

This module was developed as part of the Macrosystems EDDIE (Environmental Data-Driven Inquiry and Exploration) program, which introduces students to quantitative skills, such as data visualization, interpretation, modeling, and forecasting, through use of real-world environmental datasets [@carey_macrosystems_2020; @hounshell_macrosystems_2021; @lofton_modular_2024]. Macrosystems EDDIE modules are developed using the 5E (Engagement, Exploration, Explanation, Expansion, Evaluation) instructional model [@bybee_bscs_2006]. Each module comprises three activities (A, B, and C), which are scaffolded to sequentially build on each other. Activity A Engages students and asks them to Explore the module topic; Activity B Explains more about the topic to Expand on Activity A; and Activity C Evaluates students' understanding of the topic [@oconnell_project_2024]. The three-part module structure also allows instructors to choose whether to complete just Activity A, Activities A and B, or all three Activities, maximizing the adaptibility of modules to various course contexts. 

All module activities and questions are designed to enhance student engagement through real-world problem-solving [@villarroel_authentic_2018] and place-based learning [@semken_sense_2008]. High student engagement has been shown to improve student learning outcomes [@carini_student_2006], particularly for underrepresented groups [@theobald_active_2020]. To engage students in real-world problem-solving, we developed decision-making scenarios for students to complete that are informed by the past experiences and water treatment challenges of operators working at the same drinking water reservoirs that are included in the module (see descriptions of Activity B and C in Module Activities for further details). To engage students in place-based learning, we specifically chose drinking water reservoir sites located in the same region as the community college where we taught the module (southwest Virginia, United States), so that students would be more likely able to relate to the geographic location of the reservoirs. In addition, the drinking water reservoirs presented in the module are more likely to exhibit similar water treatment challenges as other reservoirs located throughout the region, where the drinking water operations students who complete the module may work in the future.

# Learning Objectives

By the end of this module, students will be able to:

1. Define key measures of surface freshwater quality (water temperature, dissolved oxygen, and turbidity). 
2. Explain how water temperature changes over the course of a year in a temperate reservoir and how these changes affect water quality. 
3. Interpret high-frequency water quality data to make decisions about water extraction depth for a drinking water reservoir. 
4. Evaluate water quality data and forecasts to make decisions about drinking water treatment. 

# Module Activities

This module comprises an introduction and three activities (A, B, C) which guide students towards answering the focal question: **How can we use high-frequency data to improve water quality?** Below is a brief overview of the module activities.

## Introduction

In the Introduction, students watch a brief (~ 7 min.) video introducing key concepts related to water quality and high-frequency environmental sensors, as well as the drinking water reservoir sites where data used in the module are collected. Students also view a brief (~5 min.) video orientation to the module webpage and its interactive features. Finally, students select a focal drinking water reservoir site to work with for the rest of the module. Students may choose to explore data from one of two drinking water supply reservoirs in southwest Virginia, USA: Falling Creek Reservoir (abbreviated as FCR) or Beaverdam Reservoir (BVR). Both waterbodies are located in Vinton, VA and are owned and operated by the Western Virginia Water Authority. FCR and BVR are monitored regularly as part of a Long-Term Research in Environmental Biology (LTREB) program which is funded by the National Science Foundation (NSF). As a result, both reservoirs have at least six years of high-frequency water temperature, dissolved oxygen, and turbidity data which students can visualize and interpret to complete module activities.

## Activity A 

In Activity A, students access and explore high-frequency water quality data from a drinking water reservoir in southwest Virginia. Activity A includes two objectives. In Objective 1, students learn about their focal reservoir site, and answer questions about its size, location, use, and average water quality conditions. In Objective 2, students view recent (within the past two years) environmental data from their site, and are asked to interpret these data in the context of drinking water quality. For example, students view a short video explaining how high turbidity (indicating low water clarity) can complicate drinking water treatment due to high concentrations of particulate matter on treatment plant filters. Then, they plot turbidity data from their focal reservoir and are asked to identify times during the year when turbidity might be high enough to cause water quality concerns.

## Activity B

In Activity B, students use high-frequency water quality data to explore how water quality changes over the course of a year and make decisions about water withdrawal depth (i.e., the depth from which water will be extracted for treatment at a drinking water treatment plant). They are also introduced to the concept of water quality forecasting, which can provide predictions of future water quality to managers to enable pre-emptive management action to avoid water quality impairment. Activity B includes two objectives. In Objective 3, students view environmental data from three different seasons (summer, fall, winter) at their chosen reservoir data. They are asked to assess how water quality differs among seasons, and use this information to decide from which depth in the reservoir to extract water for treatment at a drinking water treatment plant. Ultimately, students should choose the depth which maximizes the quality of the water flowing from the reservoir into the drinking water treatment plant, as this water will be the easiest and most cost-efficient to treat to meet drinking water quality standards. In Objective 4, students are asked to read and interpret a fall turnover forecast. Fall turnover is a seasonal event which occurs in many inland waterbodies located in temperate regions. During fall turnover, water temperatures change very quickly, and this can lead to rapid changes in the chemical and biological attributes of water quality, potentially complicating drinking water treatment. A forecast of fall turnover could allow drinking water treatment plant managers to anticipate water quality impairment and prepare for additional treatment measures, thus ensuring they are able to meet drinking water quality standards during the period of fall turnover.

## Activity C

In Activity C, students make management recommendations for water treatment using water quality forecasts. Activity C includes one objective. In Objective 5, students are given a drinking water treatment plant operation scenario and asked to decide whether to enact additional treatment measures to ensure that they meet regulatory standards for turbidity (water clarity). Student are asked to make decisions using both "real-time" water quality data as well as water quality forecasts over time. At the end of the objective, students are given the opportunity to evaluate the quality of their decisions after the fact, as well as evaluate the utility of both the "real-time" data and the water quality forecasts to their decision-making process.

# Context for Use

This module was originally developed as part of a virtual, asynchronous curriculum for community college students training to become drinking water treatment plant operators, and could also be taught in high school and introductory undergraduate environmental science and natural resource management courses.

All module materials are designed to be fully accessible for in-person, hybrid, and virtual, asynchronous courses. For in-person or synchronous courses, this entire module can be completed in one 2 to 3-hour lab period, two 75-minute lecture periods, or three 1-hour lecture periods. Students complete module activities using an R Shiny web application, and can answer questions either by typing them into a student handout which can be downloaded from the app, or by using a Canvas quiz which can be imported from the Canvas commons. Module materials include: source code and datasets for the R Shiny web application; instructional videos and slide decks; a student handout with question prompts; and an instructor manual. The instructor manual includes both detailed instructions for module delivery in diverse course contexts as well as an answer key for student questions. Open-source versions of all module materials are available in the module GitHub repository (https://github.com/MacrosystemsEDDIE/module9) and all module code is published on Zenodo (Zenodo DOI). Editable and open-source versions of all instructional materials (videos, slide decks, student handout and instructor manual) are also hosted online through the Science Education Resource Center at Carleton College (SERC; https://serc.carleton.edu/eddie/teaching_materials/modules/module9.html) and are published in the Environmental Data Initiative repository (EDI DOI). 

This module is currently included in the curricula for ENV 211: Sanitary Biology and Chemistry in the Environmental Science Water/Wastewater Specialization and **Course YYY in the Program ZZZ** at Mountain Empire Community College, Big Stone Gap, VA, USA.

# Module Development Background

This module was developed through a partnership between Rosa-Lee Cooke, lead instructor for environmental sciences and water/wastewater treatment at Mountain Empire Community College, and Virginia Tech researchers Mary E. Lofton and Cayelan C. Carey, who have previously developed educational materials for undergraduates related to freshwater science and ecological forecasting [@carey_macrosystems_2020; @hounshell_macrosystems_2021; @lofton_modular_2024]. Our aim in developing this module was to provide drinking water operations students with experience in visualizing and interpreting real-world high-frequency water quality data, as well as introduce students to the potential of water quality forecasting to inform water resource management. Module development was primarily supported by the National Science Foundation (EF-2318861).  

# Acknowledgements

We thank Jamie Morris of the Western Virginia Water Authority for guidance regarding regulatory standards for turbidity and water treatment operations at Falling Creek Reservoir, which informed module activities. We also thank Michael Kricheldorf, Sierra Tannheiser, and Tallory Wendall for their feedback on early versions of the module.

# References