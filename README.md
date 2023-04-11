# RCA
 
#================================   R Studio MARS Code ===================================

The latest code changes simplify the Excel configuration sheets in RCA/RCA_URAR/2_Data/Mls/Appraisal_2.xlsx.
 - The OneWayAggregation sheet data which specified how regression field adjustments are to be merged into URAR fields was merged into the RegressionFields sheet.
 - Corresponding code changes were made as well.

The purpose of this particular set of code is to create a spreadsheet that can be copied and paste into an Alamode Worksheet for the URAR form.  Using this you can generate a worksheet for as many comparables as you want - and the adjusted values will exactly match for each one.
 
The recommended way to run this, especially for begineers is to cut and paste the code into the R Console.   

You can run all code by focusing RCA/RCA_URAR/1_Code/Run/RunModule1.R
  -  Copy all code down to Stage_1() just to run the setup and Stage_1().  That will include the earth run.  Get that to work first,then copy the code in RunModule1.R to run Stage_2() and Stage_3().
      - The runs will create Excel output files.  The final major file after running Stage_2() is Stage Appraisal_2_StageIIb.xlsx.  This will contain all of the intermediate fields, plus the URA fields and in the final column the Adjusted Sale Price as URAR_SalePrice
        in the last column.  That will be your concluded value for that particular sales grid run.
      - When you run Stage_3() it creates an additional file called UrarPrepData.xlsx.  This will serve as the input to RunModul2.R - which creates your input file for uploading
        to the Alamode Worksheet (for URAR).
   
 
#====================================  Prolog Code  ======================================

RCA/Prolog/protocol.pl contains Prolog code that serves as a template for describing the complete process I go through to do appraisal per the Residual Constraint Approach.

You might ask why I do it in this way.  

1.  It can be executed as a a program.  So, it can be used as a template to control an automaton, if such a thing existed, to do everything necessary in the right order.  It also 
closely documents the steps needed.  It's essentially an exercise in diligence.  

2.  The program can be run from SWI-Prolog which is open source and can be downloaded.  It really just writes of a description of each step, the time taken and that's it.  
Sounds simple.  But it is also easy to go out and keep adding more complexity and real work to each code step.  From the Prolog code we can call C#, C++, R Script and other languages to do real work.  

