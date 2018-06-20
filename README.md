# ZAPMM
Zimbabwean Agro-Pastoral Management Model: Musimboti wevanhu, zvipfuo nezvirimwa. An Agent-Based Model in NetLogo created with researcher-farmers.

This model has been created with and for the researcher-farmers of the Muonde Trust (http://www.muonde.org/), a registered Zimbabwean non-governmental organization dedicated to fostering indigenous innovation. Model behaviors and parameters (*mashandiro nemisiyano nedzimwe model*) derive from a combination of literature review and the collected datasets from Muonde’s long-term (over 30 years) community-based research. The goals of this model are three-fold (*muzvikamu zvitatu*):

A) To represent three components of a Zimbabwean agro-pastoral system (crops, woodland grazing area, and livestock) along with their key interactions and feedbacks and some of the human management decisions that may affect these components and their interactions.

B) To assess how climate variation (implemented in several different ways) and human management may affect the sustainability of the system as measured by the continued provisioning of crops, livestock, and woodland grazing area.

C) To provide a discussion tool for the community and local leaders to explore different management strategies for the agro-pastoral system (*hwaro/nzira yekudyidzana kwavanhu, zvipfuo nezvirimwa*), particularly in the face of climate change.

If you use this code, please cite the model:

Eitzel Solera, Mv, Neves, Kleber Tulio, Solera, Jon, Wilson, Kenneth B, Mawere Ndlovu, Abraham, Fisher, Aaron C, Veski, André, Omoju, Oluwasola E, Mhike Hove, Emmanuel (2018, June 19). “Zimbabwe Agro-Pastoral Management Model (ZAPMM): Musimboti wevanhu, zvipfuo nezvirimwa.” (Version 1.0.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/323685ea-3a71-44c1-9b95-a472a703e8cc/releases/1.0.0/


This repository includes the model code in nlogo format, the Objectives, Design, and Details (ODD) document describing the model, in .doc format.  This version of the model includes timers which track setup time and run time.  It also includes all the files required to run multiple BehaviorSpace Experiments (model parameter combinations) on a high-performance cloud computing cluster (using qsub).  See the ODD for explanations of the behaviorspaces, which are split up for purposes of parallel computing. Finally, the repository includes the outputs of running those experiments on SabalCore Computing's cluster (http://www.sabalcore.com/) and R code analyzing the output, all of which is summarized in the ODD.

The version of the model without the timers, and the pdf version of the ODD, are here for completeness, and can also be found at CoMSES.net 

~~~~~~~~~~~~~~~~~~~~~

Model File and Description:
---------------------------
ZAPMM-2018-06-15_WithTimers.nlogo         The version of the model with timers (used to generate results, below)
ZAPMM_ABM_ODD_2018_Final.doc              Final version of the model description document
Notes_on_Model_testing.txt                Checklist of model development/checking steps

Files for running on cluster:
-----------------------------
RunSoftwareTests.pbs             Runs the behaviorspace with the software unit tests to confirm that they pass
ShortRunForTesting.pbs           Runs a small 8-model combination to confirm that the model works on the cluster
MT_nosub.pbs                     Runs models with Muonde thresholds and no subsidy
MT_sub.pbs                       Runs models with Muonde thresholds and feed/transport subsidy for cows
OT_feed_vary_extr.pbs            Runs models with minimal thresholds, feed subsidy, and extreme rainfall models
OT_feed_vary.pbs                 Runs models with minimal thresholds, feed subsidy, and historical rainfall models
OT_nosub_const.pbs               Runs models with minimal thresholds, no subsidy/constant rainfall
OT_nosub_vary.pbs                Runs models with minimal thresholds, no subsidy, and all varying rainfall models
OT_tran_vary_extr.pbs            Runs models with minimal thresholds, transport subsidy, and extreme rainfall models
OT_tran_vary.pbs                 Runs models with minimal thresholds, transport subsidy, and historical rainfall models
email_data.sh                    Script that emails the data to the researcher
cleanup.sh                       Script that cleans up the folder after a run of the behaviorspace experiments
computer_cluster_run_notes.txt   Notes on how what was needed to set NetLogo up on SabalCore's cluster


Output data and R analysis code:
--------------------------------
ZAPMM_BehaviorTesting_2018-05-13.xls  A spreadsheet summarizing model real-time behavior checks
RunTimeSummary2018Jun15.xls           Summary spreadsheet of internal model run times and cluster run times
Exp_2018_06_15_Analysis_forGitHub.R   R script for assembling and analyzing the model output
ModelList.txt                         List of model names for input into R analysis
COWS.o* files (10)                    Output and run times for each behaviorspace run
*.dat files (10)                      Raw output from NetLogo for each behaviorspace run
Exp_2018_05_15.csv                    Results of all behaviorspaces compiled in R


Files found on CoMSES.net:
---------------------------
ZAPMM-2018-06-16.nlogo
ZAPMM_ABM_ODD_2018_Final.pdf
