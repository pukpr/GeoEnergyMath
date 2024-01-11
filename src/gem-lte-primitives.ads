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
                      Constituents : in Long_Periods_Amp_Phase;
                      Periods : in Long_Periods;
                      Ref_Time : in Long_Float := 0.0;
                      Scaling : in Long_Float := 1.0;
                      Cos_Phase : in Boolean := True;
                      Year_Len : in Long_Float := Year_Length;
                      Integ: in Long_Float := 0.0
                      ) return Data_Pairs;


   function LTE (Forcing : in Data_Pairs;
                 Wave_Numbers : in Modulations;
                 Amp_Phase : in Modulations_Amp_Phase;
                 Offset, K0, Trend, Accel : in Long_Float := 0.0;
                 NonLin : in Long_Float := 1.0) return Data_Pairs;

   -- Query to determine if a Tidal Constituent value should not be changed
   -- This uses a float comparison and is really only used for tidal periods
   function Is_Fixed (Value : in Long_Float) return Boolean;


   procedure Regression_Factors (Data_Records : in Data_Pairs;  -- Time series
                                 --First, Last,  -- Training Interval
                                 NM : in Positive; -- # modulations
                                 Forcing : in Data_Pairs;  -- Value @ Time
                                 -- Factors_Matrix : in out Matrix;
                                 DBLT : in Periods;
                                 DALTAP : out Amp_Phases;
                                 DALEVEL : out Long_Float;
                                 DAK0 : out Long_Float;
                                 Secular_Trend : in out Long_Float;
                                 Accel : out Long_Float;
                                 Singular : out Boolean);

   --
   -- Utility procedures
   --

   -- Correlation coefficient
   function CC (X, Y : in Data_Pairs) return Long_Float;

   -- Zero-crossing metric
   function Xing (X, Y : in Data_Pairs) return Long_Float;

   -- Dynamic Time Warp, Sakoe_Chiba_Optimized
   function DTW_Distance(X, Y: in Data_Pairs; Window_Size: Positive) return Long_Float;

   -- RMS
   function RMS (X, Y : in Data_Pairs;
                 Ref, Offset : in Long_Float) return Long_Float;

   -- Correlation coefficient on spectrum
   function FT_CC (Model, Data, Forcing : in Data_Pairs) return Long_Float;

   -- Minimum Entropy
   function Min_Entropy_Power_Spectrum (X, Y : in Data_Pairs) return Long_Float;

   function Is_Minimum_Entropy return Boolean;

   -- Dumps to stdIO all the data up to time corresponding to run_time
   procedure Dump (Model, Data : in Data_Pairs;
                   Run_Time : Long_Float := 200.0);

   -- Halts the running threads
   procedure Stop;
   function Halted return Boolean;
   procedure Continue;

   -- 3 point median
   function Median (Raw : in Data_Pairs) return Data_Pairs;

   -- rectangular window of width = 2*Lobe_Width+1
   function Window (Raw : in Data_Pairs;
                    Lobe_Width : in Positive) return Data_Pairs;

   NL : constant Boolean := True;
   procedure Put (Value : in Long_Float;
                  Text : in String := "";
                  New_Line : in Boolean := False);

   -- 9 point centered filter
   function Filter9Point (Raw : in Data_Pairs) return  Data_Pairs;


end GEM.LTE.Primitives;
