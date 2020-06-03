
package GEM.LTE.Primitives.Solution is

   -- Processing thread starter
   procedure Start (N_Tides, N_Modulations : in Integer;
                    Number_of_Threads : Positive := 1);

   -- For a monitoring thread to indicate the current status
   -- Will block until the optmizer is incremented
   function Status return String;

   -- can call w/o multiprocessing, set ID=0
   procedure Dipole_Model (N_Tides, N_Modulations : in Integer;
                           ID : in Integer := 0;
                           File_Name : in String := "nino34_soi.txt";
                           Split_Training : in BOOLEAN := FALSE);


   -- Should also add interfaces such as
   -- 1. Load default parameter set, using streaming format
   -- 2. Save parameter set
   -- 3. Save resultant model to file, via climate index (time, value) pairs
   -- 4.

end GEM.LTE.Primitives.Solution;
