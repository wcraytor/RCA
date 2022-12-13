# RCA
 
#================================   R Studio MARS Code ===================================
Main R Script code is below.  This is what is pulled into R Studio and executed.  

RCA/MarketAreaModel/Atherton/09012022/1_RPackage/Run/Mkt01/Mkt01.R

And this file will source (or include) the *.R scripts in RCA/MarketAreaModel/Atherton/09012022/1_RPackage/R/.

The many configuration options are in this Excel spreadsheet:

RCA/MarketAreaModel/Atherton/09012022/2_Earth/Mls/Appraisal_2.xlsx

Note that the sheet MlsData contains the data downloaded from Pro.MlsListings.com - the primary MLS for the SF Bay Area.  However members cannot distribute the data or in particular certain portions, such  as the MLS numbers.  To be safe all property identifying information was removed.  That is unfortunate - but I think what I have uploaded is better than nothing.

If you have experience with R Studio and R Script, then you will probably be able to make good use of the code.  Although, I haven't had much time to go through and comment it or clean it up.  I have used it for creating all of the City Price Models for San Mateo County on https://pacificvista.net/price_models.  For each city, you will have to change some of the information in the headers and perhaps the Appraisal_2.xlsx spreadsheet - as at times different cities require different parameters.  Usually I just work out of R Studio in executing R Script code - rather than build some kind of separate UI.

I will eventually add more comments and clean up the code a bit.

#========

I have not uploaded the actual code I use for my Alamode Appraisal reports.  That code fully automates moving the price model into the Alamode Sales Grid.  Eventually that will be added as a separate set of code.

#====================================  Prolog Code  ======================================

RCA/Prolog/protocol.pl contains Prolog code that serves as a template for describing the complete process I go through to do appraisal per the Residual Constraint Approach.

You might ask why I do it in this way.  

1.  It can be executed as a a program.  So, it can be used as a template to control an automaton, if such a thing existed, to do everything necessary in the right order.  It also closely documents the steps needed.  It's essentially an exercise in diligence.  

2.  The program can be run from SWI-Prolog which is open source and can be downloaded.  It really just writes of a description of each step, the time taken and that's it.  Sounds simple.  But it is also easy to go out and keep adding more complexity and real work to each code step.  From the Prolog code we can call C#, C++, R Script and other languages to do real work.  

