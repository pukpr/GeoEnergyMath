
package GEM.LTE.Primitives is

   type Pair is  -- A pair of values, such as in monthly temperature time-series
      record
         Date, Value : Long_Float;
      end record;

   type Data_Pairs is array (Positive range <>) of Pair;

   -- corresponding to a climate index such as ENSO
   function Make_Data (Name : in String) return Data_Pairs;

   -- Save results
   procedure Save (Model, Data, Forcing : in Data_Pairs;
                   File_Name : in String := "lte_results.csv");

   --
   -- Main algorithms
   --

   -- Infinite Impulse Response -- integrator
   function IIR (Raw : in Data_Pairs;
                 lagA, lagB, lagC : in Long_Float;
                 iA, iB, iC : in Long_Float := 0.0;
                 Start : in Long_Float := Long_Float'First) return  Data_Pairs;

   -- Finite Impulse Response -- smoother
   function FIR (Raw : in Data_Pairs;
                 Behind, Current, Ahead : in Long_Float) return  Data_Pairs;

   -- Impulse modulation
   generic
      with function Impulse (Time : in Long_Float) return Long_Float;
   function Amplify (Raw : in Data_Pairs;
                     Offset, Ramp, Start : in Long_Float) return Data_Pairs;

   -- LTE models = Superposition of tides + Laplace's Tidal Eqn modulation

   function Tide_Sum (Template : in Data_Pairs;
                      Constituents : in Long_Periods;
                      Periods : in Long_Periods_Frequency;
                      Ref_Time : in Long_Float; -- remove the millenial offset
                      Scaling : in Long_Float;
                      Order2, Order3 : in Long_Float) return Data_Pairs;


   function LTE (Forcing : in Data_Pairs;
                 Wave_Numbers : in Modulations;
                 Amp_Phase : in Modulations_Amp_Phase;
                 Offset, K0, Trend : in Long_Float := 0.0) return Data_Pairs;

   -- Query to determine if a Tidal Constituent value should not be changed
   -- This uses a float comparison and is really only used for tidal periods
   function Is_Fixed (Value : in Long_Float) return Boolean;

   --
   -- Utility procedures
   --

   -- Correlation coefficient
   function CC (X, Y : in Data_Pairs) return Long_Float;

   -- Zero-crossing metric
   function Xing (X, Y : in Data_Pairs) return Long_Float;

   -- RMS
   function RMS (X, Y : in Data_Pairs;
                 Ref, Offset : in Long_Float) return Long_Float;

   -- Dumps to stdIO all the data up to time corresponding to run_time
   procedure Dump (Model, Data : in Data_Pairs;
                   Run_Time : Long_Float := 200.0);

   -- Halts the running threads
   procedure Stop;
   function Halted return Boolean;

   -- 3 point median
   function Median (Raw : in Data_Pairs) return Data_Pairs;

   -- rectangular window of width = 2*Lobe_Width+1
   function Window (Raw : in Data_Pairs;
                    Lobe_Width : in Positive) return Data_Pairs;

end GEM.LTE.Primitives;
