# ZAPMM
Zimbabwean Agro-Pastoral Management Model: an Agent-Based Model in NetLogo created with farmer-researchers

If you use this code, please cite the model:
M. V. Eitzel, Kleber Tulio Neves, Jon Solera, K. B. Wilson, Abraham Mawere Ndlovu, Aaron C. Fisher, André Veski, Oluwasola E. Omoju, Emmanuel Mhike Hove. Zimbabwe Agro-Pastoral Management Model (ZAPMM): Musimboti wevanhu, zvipfuo nezvirimwa. Available at CoMSES.net ***DOI will appear here***


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
MT_nosub.pbs                     Behaviorspace with Muonde thresholds and no subsidy
MT_sub.pbs                       Behaviorspace with Muonde thresholds and feed/transport subsidy for cows
OT_feed_vary_extr.pbs            Behaviorspace with minimal thresholds, feed subsidy, and extreme rainfall models
OT_feed_vary.pbs                 Behaviorspace with minimal thresholds, feed subsidy, and historical rainfall models
OT_nosub_const.pbs               Behaviorspace with minimal thresholds, no subsidy/constant rainfall
OT_nosub_vary.pbs                Behaviorspace with minimal thresholds, no subsidy, and all varying rainfall models
OT_tran_vary_extr.pbs            Behaviorspace with minimal thresholds, transport subsidy, and extreme rainfall models
OT_tran_vary.pbs                 Behaviorspace with minimal thresholds, transport subsidy, and historical rainfall models
email_data.sh                    Script that emails the data to the researcher
cleanup.sh                       Script that cleans up the folder after a run of the behaviorspace experiments
computer_cluster_run_notes.txt   Notes on how what was needed to set NetLogo up on SabalCore's cluster

Output data and R analysis code:
--------------------------------
ZAPMM_BehaviorTesting_2018-05-13.xls      A spreadsheet summarizing model real-time behavior checks
RunTimeSummary2018Jun15.xls           Summary spreadsheet of internal model run times and cluster run times
Exp_2018_06_15_Analysis_forGitHub.R   R script for assembling and analyzing the model output
ModelList.txt                         List of model names for input into R analysis
COWS.o* files                         Output and run times for each model
RunSoftwareTests.dat                  Raw output from NetLogo for behaviorspace run
ShortRunForTesting.dat                Raw output from NetLogo for behaviorspace run
MT_nosub.dat                          Raw output from NetLogo for behaviorspace run
MT_sub.dat                            Raw output from NetLogo for behaviorspace run
OT_feed_vary.dat                      Raw output from NetLogo for behaviorspace run
OT_feed_vary_extr.dat                 Raw output from NetLogo for behaviorspace run
OT_nosub_const.dat                    Raw output from NetLogo for behaviorspace run
OT_nosub_vary.dat                     Raw output from NetLogo for behaviorspace run
OT_tran_vary.dat                      Raw output from NetLogo for behaviorspace run
OT_tran_vary_extr.dat                 Raw output from NetLogo for behaviorspace run
Exp_2018_05_15.csv                    Results of all behaviorspaces compiled in R


Files found on CoMSES.net:
---------------------------
ZAPMM-2018-06-16.nlogo
ZAPMM_ABM_ODD_2018_Final.pdf