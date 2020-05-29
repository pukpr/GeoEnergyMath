
with Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Long_Float_Text_IO;
with GEM.Random_Descent;
with GNAT.Ctrl_C;
with System.Task_Info;
with GEM.LTE.Primitives.Shared;

package body GEM.LTE.Primitives.Solution is

   -- Map task threads to multicore processors if available
   task type Thread(CPU : System.Task_Info.CPU_Number) is
     pragma Task_Info (new System.Task_Info.Thread_Attributes'(CPU=>CPU));
   end Thread;
   type Thread_Access is access Thread;

   -- Start the task threads with a remote handler to cleanly stopping mechanism
   procedure Start (Number_of_Threads : Positive := 1) is
      TA : Thread_Access;
      use System.Task_Info;
   begin
     GNAT.Ctrl_C.Install_Handler (Handler => GEM.LTE.Primitives.Stop'Access);
     for I in 0..Number_of_Threads-1 loop
         TA := new Thread (CPU_Number(I));
         delay 1.0; -- let them gradually start up
      end loop;
   end Start;

   --
   -- This is the passive monitoring object with mutual exclusve access
   -- to the best metric provided by the executing threads
   --
   protected Monitor is
      procedure Check (Metric     : in Long_Float;
                       Client     : in Integer;
                       Count      : in Long_Integer;
                       Best       : out Boolean;  -- accessing thread deemed best
                       Percentage : out Integer); -- % of best metric if not best
      procedure Client (ID : out Integer); -- registering a thread ID, called once
      function Best_Value return Long_Float;
      entry Status (Metric : out Long_Float;   -- used by a monitoring thread, i.e. main
                    Client : out Integer;      -- returns client thread w/ best metric
                    Cycle : out Long_Integer); -- and the cycle count it is on
      procedure Stop;
   private
      --  Value of current best metric stored internally
      Best_Metric  : Long_Float := 0.0;
      Client_Index : Integer := 1;
      Waiting : Boolean := TRUE; -- triggers status only if value changes
      Best_Client : Integer := -1;
      Best_Count : Long_Integer := 0;
   end Monitor;

   protected body Monitor is
      procedure Check (Metric     : in Long_Float;
                       Client     : in Integer;
                       Count      : in Long_Integer;
                       Best       : out Boolean;
                       Percentage : out Integer) is
      begin
         if Metric >= Best_Metric then
            Waiting := not (Metric > Best_Metric);
            Best_Metric := Metric;
            Best := True;
            Best_Client := Client;
            Best_Count := Count;
         else
            Best := False;
         end if;
         if Best_Metric > 0.0 then
            Percentage := Integer(100.0*Metric/Best_Metric);
         else
            Percentage := 0;
         end if;
      end Check;

      procedure Client (ID : out Integer) is
      begin
         ID := Client_Index;
         Client_Index := Client_Index + 1;
      end Client;

      function Best_Value return Long_Float is
      begin
         return Best_Metric;
      end Best_Value;

      entry Status (Metric : out Long_Float;
                    Client : out Integer;
                    Cycle : out Long_Integer) when not Waiting is
      begin
         Waiting := TRUE;
         Metric := Best_Metric;
         Client := Best_Client;
         Cycle := Best_Count;
      end Status;

      procedure Stop is
      begin
         Waiting := FALSE; -- necesary to allow a clean exit when program halted
      end Stop;
   end Monitor;

   function Status return String is
      Metric : Long_Float;
      Client : Integer;
      Cycle : Long_Integer;
   begin
      -- Blocking call to monitor, will only return when state changes
      Monitor.Status(Metric, Client, Cycle);
      return "Status: " & Client'Img & Cycle'Img & Metric'Img;
   end Status;

   task body Thread is
      ID : Integer;
   begin
      Monitor.Client(ID);
      Text_IO.Put_Line("Thread #" & ID'Img);
      -- Assume default for File_Name
      Dipole_Model(ID => ID, Split_Training => TRUE);
   end Thread;

   --
   -- This is the main computation for modeling a dipole, configured
   -- with default parameters that fit to an ENSO NINO34/SOI data set
   -- at a CC of > 0.83.
   --
   procedure Dipole_Model (ID : in Integer := 0;
                           File_Name : in String := "nino34_soi.txt";
                           Split_Training : in BOOLEAN := FALSE) is
      package LEF renames Ada.Numerics.Long_Elementary_Functions;

      Data : Text_IO.File_Type;
      Data_Records : Data_Pairs := Make_Data(File_Name);

      function Impulse (Time : Long_Float) return Long_Float;

      function Impulse_Amplify is new Amplify(Impulse => Impulse);

      Start_Time : Long_Float := 1880.0;
      Run_Time   : Long_Float := 136.6; -- used to truncate the file output
      Model      : Data_Pairs := Data_Records;
      -- Best_Model : Data_Pairs := Data_Records; -- perhaps needed if
      Best  : Boolean := False;
      Percentage : Integer;

      D : Shared.Param_S :=
        (NLP    => LP'Length,
         NLT    => LT'Length,
         LP     => LP,
         LT     => LT,
         Offset => -0.002770852,
         bg     => 0.037589404,
         ImpA   => 13.57577452, -- 0.0,
         ImpB   => 7.056004563, -- 1.22,
         ImpC   => 20.25515663, -- 1.399,
         ImpD   => 1.659027049, -- 1.2149,
         mA     => 0.080599014,
         mP     => -0.008405309,
         mD     => -0.0021993,
         fB     => -0.022004419, -- 10.761,
         fC     => 1.469665629, -- 11.865,
         fA     => 0.600532903, -- 7.55161
         shiftT => 0.000001 );

      D0 : constant Shared.Param_S := D;  -- reference

      function Impulse (Time : Long_Float) return Long_Float is
         Value : Long_Float;
         Trunc : Integer := Integer((Time - Long_Float'Floor(Time))*1000.0);
      begin
         if Trunc = 775 then     -- ~October
            Value := D.ImpA;
         elsif Trunc = 858 then  -- ~November
            Value := D.ImpB;
         elsif Trunc = 942 then  -- ~December
            Value := D.ImpC;
         elsif Trunc = 25 then   -- ~January
            Value := D.ImpD;
         else
            Value := 0.0;
         end if;
         return Value + D.bg;
      end Impulse;

      procedure Put_CC (Val1, Val2 : in Long_Float;
                        Counter : in Long_Integer;
                        Thread  : in Integer) is
      begin
         Text_IO.Put( "CC=" );
         Ada.Long_Float_Text_IO.Put(Val1, Fore=>4, Aft=>8, Exp=>0);
         Ada.Long_Float_Text_IO.Put(Val2, Fore=>4, Aft=>8, Exp=>0);
         Text_IO.Put_Line("  " & Thread'Img & Counter'Img);
      end Put_CC;

      procedure Zero (Model : in out Long_Periods;
                      Amplitude : in Long_Float ) is
      begin
         for I in Model'Range loop
             Model(i).Amplitude := Amplitude;
         end loop;
      end Zero;

      der : Long_Float := 1.0 + D.mD - D.mA - D.mP; -- keeps the inegrator stable
      CorrCoeff, Old_CC : Long_Float := 0.0;
      CorrCoeffTest, Old_CCTest : Long_Float := 0.0;
      Progress_Cycle, Spread : Long_Float;
      Counter : Long_Integer := 0;

      ------------------------------------------------------------------------
      -- This is close to a violtion of encapsulation, as we need a way
      -- to modify all float parameters without having knowledge of the abstract
      -- objects, so the data record D is overlaid with a plain array Set
      ------------------------------------------------------------------------
      Size_Shared : Positive := D'Size/Long_Float'Size - 1; -- subtract header=2 ints

      package Walker is new GEM.Random_Descent (Fixed => Is_Fixed,
                                                Set_Range => Size_Shared);

      Set, Keep : Walker.LF_Array (1 .. Size_Shared);
      for Set'Address use D.Offset'Address; -- This may require Ada rec rep clauses
      ------------------------------------------------------------------------

   begin
      Old_CC := 0.0;
      -- Zero(D.LP, 0.0001); -- ------------------!!
      Walker.Reset;

      loop
         Counter := Counter + 1;
         delay 0.0; -- context switching point if multi-processing not avilable
         Progress_Cycle := Long_Float(Counter);
         -- This slowly oscillates to change the size of the step to hopefully
         -- help it escape local minima, every 1000 cycles
         Spread := 0.000_000_1 + 0.1*(1.0-LEF.Cos(Progress_Cycle/1000.0));
         Walker.Markov(Set, Keep, Spread); -- 0.00000001

         -- Tidal constituents summed, amplified by impulse, and LTE modulated
         if Split_Training and Best then
          delay 0.001; -- don't do anything if in the lead, let others catch up.
         else
          Model := LTE(
           Forcing => IIR(
             Raw  => Impulse_Amplify(
               Raw    => FIR(
                 Raw     => Tide_Sum(Template     => Data_Records,
                                        Constituents => D.LP,
                                        Ref_Time     => Start_Time + D.ShiftT,
                                        Scaling      => 0.5),
                 Behind  => D.fB,  -- -0.022004419,
                 Current => D.fC,  -- 1.469665629,
                 Ahead   => D.fA), -- 0.600532903,
               Offset => D.Offset),
                           lagA => der, lagB => D.mA, lagC => D.mP, lagD => D.mD,
                          iA => -0.0063, iB => 0.02, iC => 0.0068, iD => -0.00026),
            Wave_Numbers => D.LT, Offset => 0.389, K0 => 0.169);

            -- extra filtering, 2 equal-weighted 3-point box windows creating triangle
            Model := FIR(FIR(Model,0.333,0.333,0.333), 0.333, 0.333, 0.333);
         end if;

         -- pragma Debug ( Dump(Model, Data_Records, Run_Time) );

         CorrCoeff := CC( Model(20..840), Data_Records(20..840));
         CorrCoeffTest := CC( Model(840..1640), Data_Records(840..1640));
                 -- Xing( Model(20..1630), Data_Records(20..1630));

         -- Register the results with a monitor
         if Split_Training then
            Monitor.Check(CorrCoeffTest, ID, Counter, Best, Percentage);
         else
            Monitor.Check(CorrCoeff, ID, Counter, Best, Percentage);
         end if;

         if CorrCoeff > Old_CC then
            if Best then
               Put_CC(CorrCoeff, CorrCoeffTest, Counter, ID);
               -- delay 0.25;
               -- Put_CC(CorrCoeffTest, 0, ID);
            end if;
            Old_CC := CorrCoeff;
            Old_CCTest := CorrCoeffTest;
         else
            Set := Keep;
         end if;

         if (not Best) and Counter > 100_000 and Percentage < 99 then
            Text_IO.Put_Line("Resetting" & ID'Img & Percentage'Img & "%");
            D := D0; -- load back reference model parameters
            -- Zero(D.LP, 0.0001); ----------------!!!!!!!!!!!!!
            Counter := 0;
            Old_CC := 0.0; --- I FORGOT THIS EARLIER
         end if;

         exit when Halted;

      end loop;
      Monitor.Stop;
      if Best then
         Walker.Dump(Keep); -- Print results of last best evaluation,
         -- Should also save to file
         if Split_Training then
            Put_CC(CorrCoeff, CorrCoeffTest, Counter, ID);
         else
            Put_CC(Old_CC, Old_CCTest, Counter, ID);
         end if;
      end if;

   end Dipole_Model;

end GEM.LTE.Primitives.Solution;
