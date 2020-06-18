with Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Long_Float_Text_IO;
with GEM.Random_Descent;
with GNAT.Ctrl_C;
with System.Task_Info;
with GEM.LTE.Primitives.Shared;
with Ada.Exceptions;
with Gnat.Traceback.Symbolic;

package body GEM.LTE.Primitives.Solution is

   -- Map task threads to multicore processors if available
   task type Thread(CPU : System.Task_Info.CPU_Number;
                    N_Tides, N_Modulations : Integer) is
     pragma Task_Info (new System.Task_Info.Thread_Attributes'(CPU=>CPU));
   end Thread;
   type Thread_Access is access Thread;

   -- Start the task threads with a remote handler to cleanly stopping mechanism
   procedure Start (N_Tides, N_Modulations : in Integer;
                    Number_of_Threads : Positive := 1) is
      TA : Thread_Access;
      use System.Task_Info;
   begin
     GNAT.Ctrl_C.Install_Handler (Handler => GEM.LTE.Primitives.Stop'Access);
     for I in 0..Number_of_Threads-1 loop
         TA := new Thread (CPU=>CPU_Number(I), N_Tides=>N_Tides, N_Modulations=>N_Modulations);
         delay 1.0; -- let them gradually start up
      end loop;
   end Start;

   Worst_Case : constant LONG_FLOAT := GEM.Getenv("STARTING_METRIC", 0.001);

   --
   -- This is the passive monitoring object with mutual exclusve access
   -- to the best metric provided by the executing threads
   --
   protected Monitor is
      procedure Check (Metric     : in Long_Float;
                       Client     : in Integer;
                       Count      : in Long_Integer;
                       Best       : out Boolean;  -- accessing thread deemed best
                       BestClient : out Integer;
                       Percentage : out Integer); -- % of best metric if not best
      procedure Client (ID : out Integer); -- registering a thread ID, called once
      function Best_Value return Long_Float;
      entry Status (Metric : out Long_Float;   -- used by a monitoring thread, i.e. main
                    Client : out Integer;      -- returns client thread w/ best metric
                    Cycle : out Long_Integer); -- and the cycle count it is on
      procedure Stop;
   private
      --  Value of current best metric stored internally
      Best_Metric  : Long_Float := Worst_Case; -- so doesn't cause overflow for %
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
                       BestClient : out Integer;
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
         if Best_Metric > Worst_Case then
            Percentage := Integer(100.0*Metric/Best_Metric);
         else
            Percentage := 0;
         end if;
         BestClient := Best_Client;
      exception
         when Constraint_Error =>
            Best := False;
            BestClient := Best_Client;
            Percentage := 0;
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

   -----------------------------------


   task body Thread is
      ID : Integer;
      Name : String := GEM.Getenv(Name => "CLIMATE_INDEX",
                                  Default => "nino34_soi.txt");
      Split : Boolean := not (GEM.Getenv(Name => "SPLIT_TRAINING",
                                         Default => "") = "");
   begin
      Monitor.Client(ID);
      Text_IO.Put_Line(Name & " for Thread #" & ID'Img & Thread.CPU'Img);
      -- Assume default for File_Name
      Dipole_Model(N_Tides=>Thread.N_Tides,
                   N_Modulations=>Thread.N_Modulations,
                   ID => ID, File_Name=>Name, Split_Training => Split);
   end Thread;

   --
   -- This is the main computation for modeling a dipole, configured
   -- with default parameters that fit to an ENSO NINO34/SOI data set
   -- at a CC of > 0.83.
   --
   procedure Dipole_Model (N_Tides, N_Modulations : in Integer;
                           ID : in Integer := 0;
                           File_Name : in String := "nino34_soi.txt";
                           Split_Training : in BOOLEAN := FALSE) is
      package LEF renames Ada.Numerics.Long_Elementary_Functions;

      Data : Text_IO.File_Type;
      Data_Records : Data_Pairs := Make_Data(File_Name);

      function Impulse (Time : Long_Float) return Long_Float;
      function Impulse_Sin (Time : Long_Float) return Long_Float;

      function Impulse_Amplify is new Amplify(Impulse => Impulse_Sin);

      Start_Time : Long_Float := Data_Records(Data_Records'First).Date;
      Model      : Data_Pairs := Data_Records;
      Best  : Boolean := False;
      Percentage : Integer;
      Best_Client : Integer;

      D : Shared.Param_S(N_Tides, N_Modulations) := GEM.LTE.Primitives.Shared.Get(N_Tides, N_Modulations);
      D0 : constant Shared.Param_S := D;  -- reference

      ImpA : constant Integer := GEM.Getenv("IMPA", 9); -- Defaults for ENSO
      ImpB : constant Integer := GEM.Getenv("IMPB", 10);
      ImpC : constant Integer := GEM.Getenv("IMPC", 11);
      ImpD : constant Integer := GEM.Getenv("IMPD", 0);

      Maximum_Loops : constant Long_Integer := GEM.Getenv("MAXLOOPS", 100_000);
      Threshold : constant Integer := GEM.Getenv("THRESHOLD", 99);
      Scaling : constant Long_Float := GEM.Getenv("SCALING", 0.5);
      Spread_Min : constant Long_Float := GEM.Getenv("SPREAD_MIN", 0.000_000_1);
      Spread_Max : constant Long_Float := GEM.Getenv("SPREAD_MAX", 0.1);
      Spread_Cycle : constant Long_Float := GEM.Getenv("SPREAD_CYCLE", 1000.0);
      Catchup : constant Boolean := GEM.Getenv("THRESHOLD_ACTION", "RESTART") = "CATCHUP";
      --K0 : constant Long_Float := GEM.Getenv("K0", 0.169);
      RMS_Metric : constant Boolean := GEM.Getenv("METRIC", "CC") = "RMS";
      Sin_Impulse : constant Boolean := GEM.Getenv("IMPULSE", "DELTA") = "SIN";
      Sampling_Per_Year : constant Long_Float := GEM.Getenv("SAMPLING", 12.0);

      function Metric (X, Y : in Data_Pairs) return Long_Float is
      begin
         if RMS_Metric then
            return RMS(X,Y)*CC(X,Y);
         else
            return CC(X,Y); -- TEMP
         end if;
      end Metric;

      function Impulse (Time : Long_Float) return Long_Float is
         Value : Long_Float;
         -- Impulses will occur on a month for monthly data
         Trunc : Integer := Integer((Time - Long_Float'Floor(Time))*Sampling_Per_Year);
      begin
         if Trunc = ImpA then     --775 ~October
            Value := D.ImpA;
         elsif Trunc = ImpB then  --858 ~November
            Value := D.ImpB;
         elsif Trunc = ImpC then  --942 ~December
            Value := D.ImpC;
         elsif Trunc = ImpD then   --25 ~January
            Value := D.ImpD;
         else
            Value := 0.0;
         end if;
         return Value + D.bg;
      end Impulse;

      function Impulse_Sin (Time : Long_Float) return Long_Float is
         Pi : Long_Float := Ada.Numerics.Pi;
         Value : Long_Float;
         use Ada.Numerics.Long_Elementary_Functions;
         -- scale*(COS(2*PI*(E2+ip)))^2+D17*(COS(2*PI*(E2+ip)))^3
      begin
         if not Sin_Impulse then
            return Impulse(Time);
         end if;
         -- uisng the impA & impB env vars as odd & even powers, since they won't be used for impulse
         Value := D.ImpA*(COS(2.0*Pi*(Time+D.ImpB)))**ImpA+D.ImpC*(COS(2.0*Pi*(Time+D.ImpB)))**ImpB;
         return Value + D.bg;
      end Impulse_Sin;

      procedure Put_CC (Val1, Val2 : in Long_Float;
                        Counter : in Long_Integer;
                        Thread  : in Integer) is
      begin
         Text_IO.Put( "CC=" );
         Ada.Long_Float_Text_IO.Put(Val1, Fore=>4, Aft=>10, Exp=>0);
         Ada.Long_Float_Text_IO.Put(Val2, Fore=>4, Aft=>10, Exp=>0);
         Text_IO.Put_Line("  " & Thread'Img & Counter'Img);
      end Put_CC;

      procedure Zero (Model : in out Long_Periods;
                      Amplitude : in Long_Float ) is
      begin
         for I in Model'Range loop
             Model(i).Amplitude := Amplitude;
         end loop;
      end Zero;

      procedure Zero (Model : in out Modulations;
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
      First : Integer := Data_Records'First+12;
      Last : Integer := Data_Records'Last-12;
      Mid : Integer := (First+Last)/2;
      TS : Long_Float := GEM.Getenv("TRAIN_START", Data_Records(First).Date);
      TE : Long_Float := GEM.Getenv("TRAIN_END", Data_Records(Last).Date);

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
      for I in Data_Records'Range loop
         exit when Data_Records(I).Date > TS; -- Finding indices to start
         First := I;
      end loop;
      for I in Data_Records'Range loop
         exit when Data_Records(I).Date > TE; -- and end
         Last := I;
      end loop;
      Start_Time := Long_Float(Integer(Start_Time)); -- 1880.0;
      Old_CC := 0.0;
      Walker.Reset;
      Text_IO.Put_Line("Catchup mode enabled:" & Boolean'Image(Catchup) );

      loop
         Counter := Counter + 1;
         delay 0.0; -- context switching point if multi-processing not avilable
         Progress_Cycle := Long_Float(Counter);
         -- This slowly oscillates to change the size of the step to hopefully
         -- help it escape local minima, every N cycles
         Spread := Spread_Min + Spread_Max*(1.0-LEF.Cos(Progress_Cycle/Spread_Cycle));
         Walker.Markov(Set, Keep, Spread);

         -- Tidal constituents summed, amplified by impulse, and LTE modulated
         if Split_Training and Best then
            Save(Model, Data_Records);
            delay 1.0; -- don't do anything if in the lead, let others catch up.
         else -- if Scaling > 0.0 then
            Model := LTE(Forcing => IIR(
                         Raw  => Impulse_Amplify(
                           Raw    => FIR(
                             Raw     => Tide_Sum(Template     => Data_Records,
                                                 Constituents => D.LP,
                                                 Ref_Time     => Start_Time + D.ShiftT,
                                                 Scaling      => Scaling,
                                                 Order2       => D.Order2,
                                                 Order3       => D.Order3
                                                ),
                             Behind  => D.fB,
                             Current => D.fC,
                             Ahead   => D.fA),
                           Offset => D.Offset),
                         lagA => der, lagB => D.mA, lagC => D.mP, lagD => D.mD,
                         iA => D.init, iB => 0.0, iC => 0.0, iD => 0.0),
                         Wave_Numbers => D.LT, Offset => D.level, K0 => D.K0);

            -- extra filtering, 2 equal-weighted 3-point box windows creating triangle
            Model := FIR(FIR(Model,0.333,0.333,0.333), 0.333, 0.333, 0.333);
         --  else
         --     Model := LTE(Forcing => IIR(
         --                  Raw  => Impulse_Amplify(
         --                    Raw    => FIR(
         --                      Raw     => GravityM(Template     => Data_Records,
         --                                          Constituents => D.LP,
         --                                          Ref_Time     => Start_Time + D.ShiftT,
         --                                          Scaling      => D.Level),
         --                      Behind  => D.fB,
         --                      Current => D.fC,
         --                      Ahead   => D.fA),
         --                    Offset => D.Offset),
         --                  lagA => der, lagB => D.mA, lagC => D.mP, lagD => D.mD,
         --                  iA => D.init, iB => 0.0, iC => 0.0, iD => 0.0),
         --                  Wave_Numbers => D.LT, Offset => Scaling, K0 => D.K0);
         --
         --     -- extra filtering, 2 equal-weighted 3-point box windows creating triangle
         --     Model := FIR(FIR(Model,0.333,0.333,0.333), 0.333, 0.333, 0.333);
         end if;

         -- pragma Debug ( Dump(Model, Data_Records, Run_Time) );

         if Split_Training then
            CorrCoeff := Metric( Model(First..Mid), Data_Records(First..Mid));
            CorrCoeffTest := Metric( Model(Mid..Last), Data_Records(Mid..Last));
         else
            CorrCoeff := Metric( Model(First..Last), Data_Records(First..Last));
         end if;

         -- Register the results with a monitor
         if Split_Training then
            Monitor.Check(CorrCoeffTest, ID, Counter, Best, Best_Client, Percentage);
         else
            Monitor.Check(CorrCoeff, ID, Counter, Best, Best_Client, Percentage);
         end if;

         if ID = Best_Client then
            Counter := 1; -- no use penalizing thread in the lead
         end if;

         if CorrCoeff > Old_CC then
            --if Best then
            --   Put_CC(CorrCoeff, CorrCoeffTest, Counter, ID);
            --end if;
            Old_CC := CorrCoeff;
            Old_CCTest := CorrCoeffTest;
            Keep := Set;
            if CatchUp then  -- save it for other threads to reset from
               GEM.LTE.Primitives.Shared.Put(D);
            end if;
         else
            Set := Keep;
         end if;

         if (not Best) and Counter > Maximum_Loops and Percentage < Threshold then
            Text_IO.Put_Line("Resetting" & ID'Img & Percentage'Img & "%");
            if CatchUp then
               D := GEM.LTE.Primitives.Shared.Get(N_Tides, N_Modulations);
            else -- Restart
               D := D0; -- load back reference model parameters
            end if;
            Counter := 0;
            Old_CC := 0.0;
         end if;

         exit when Halted;

      end loop;
      Monitor.Stop;
      if Best_Client = ID then
         Set := Keep;
         D := D;
         GEM.LTE.Primitives.Shared.Save(D);
         Walker.Dump(Keep); -- Print results of last best evaluation,
         Save(Model, Data_Records);    -- Should also save to file
         if Split_Training then
            Put_CC(CorrCoeff, CorrCoeffTest, Counter, ID);
         else
            Put_CC(Old_CC, Old_CCTest, Counter, ID);
         end if;
      else
         Text_IO.Put_Line("Exited " & ID'Img);
      end if;


   exception
      when E : others =>
         Text_IO.Put_Line ("Solution err: " & Ada.Exceptions.Exception_Information(E));
         -- The following may need a debug-specifi compiler switch to activate
         Text_IO.Put_Line (Gnat.Traceback.Symbolic.Symbolic_Traceback(E));

   end Dipole_Model;

end GEM.LTE.Primitives.Solution;
