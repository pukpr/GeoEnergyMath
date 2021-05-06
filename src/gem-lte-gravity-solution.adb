with Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Long_Float_Text_IO;
with GEM.Random_Descent;
with GNAT.Ctrl_C;
with System.Task_Info;
with GEM.LTE.Gravity.Shared;
with Ada.Exceptions;
with Gnat.Traceback.Symbolic;
with Gem.LTE.Primitives;

package body Gem.LTE.Gravity.Solution is

    -- Map task threads to multicore processors if available
   task type Thread(CPU : System.Task_Info.CPU_Number;
                    N_Modulations : Integer) is
     pragma Task_Info (new System.Task_Info.Thread_Attributes'(CPU=>CPU));
   end Thread;
   type Thread_Access is access Thread;

   -- Start the task threads with a remote handler to cleanly stopping mechanism
   procedure Start (N_Modulations : in Integer;
                    Number_of_Threads : Positive := 1) is
      TA : Thread_Access;
      use System.Task_Info;
   begin
     GNAT.Ctrl_C.Install_Handler (Handler => GEM.LTE.Primitives.Stop'Access);
     for I in 0..Number_of_Threads-1 loop
         TA := new Thread (CPU=>CPU_Number(I), N_Modulations=>N_Modulations);
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
      Gravity_Model(N_Modulations=>Thread.N_Modulations,
                   ID => ID, File_Name=>Name, Split_Training => Split);
   exception
      when E : others =>
         Text_IO.Put_Line ("Thread err: " & Ada.Exceptions.Exception_Information(E));
         -- The following may need a debug-specifi compiler switch to activate
         Text_IO.Put_Line (Gnat.Traceback.Symbolic.Symbolic_Traceback(E));
   end Thread;

   --
   -- This is the main computation for modeling a dipole, configured
   -- with default parameters that fit to an ENSO NINO34/SOI data set
   -- at a CC of > 0.83.
   --
   procedure Gravity_Model (N_Modulations : in Integer;
                           ID : in Integer := 0;
                           File_Name : in String := "nino34_soi.txt";
                           Split_Training : in BOOLEAN := FALSE) is
      package LEF renames Ada.Numerics.Long_Elementary_Functions;
      use Gem.LTE.Primitives;

      Data_Records : Data_Pairs := Make_Data(File_Name);

      Ref_Time : Long_Float := GEM.Getenv("REF_TIME", Data_Records(Data_Records'First).Date);
      Forcing  : Data_Pairs := Data_Records;
      Model    : Data_Pairs := Data_Records;
      KeepModel: Data_Pairs := Data_Records;

      Best  : Boolean := False;
      Percentage : Integer;
      Best_Client : Integer;

      D : Shared.Param_S(N_Modulations) := GEM.LTE.Gravity.Shared.Get(N_Modulations);
      DKeep : Shared.Param_S(N_Modulations) := D;
      D0 : constant Shared.Param_S := D;  -- reference

      Maximum_Loops : constant Long_Integer := GEM.Getenv("MAXLOOPS", 100_000);
      Threshold : constant Integer := GEM.Getenv("THRESHOLD", 99);
      Scaling : constant Long_Float := GEM.Getenv("SCALING", 0.5);
      Spread_Min : constant Long_Float := GEM.Getenv("SPREAD_MIN", 0.000_000_1);
      Spread_Max : constant Long_Float := GEM.Getenv("SPREAD_MAX", 0.1);
      Spread_Cycle : constant Long_Float := GEM.Getenv("SPREAD_CYCLE", 1000.0);
      Catchup : constant Boolean := GEM.Getenv("THRESHOLD_ACTION", "RESTART") = "CATCHUP";
      RMS_Metric : constant Boolean := GEM.Getenv("METRIC", "CC") = "RMS";
      Sampling_Per_Year : constant Long_Float := GEM.Getenv("SAMPLING", 12.0);
      Filter : constant Long_Float := GEM.Getenv("FILTER", 0.33333333);
      Forcing_Only : constant Boolean := GEM.Getenv("FORCING", FALSE);
      Calibrate : constant Boolean :=  GEM.Getenv("CAL", FALSE);
      RMS_Data : Long_Float := 0.0;

      function Metric (X, Y : in Data_Pairs) return Long_Float is
      begin
         if RMS_Metric then
            return (RMS(X,Y,RMS_Data, 0.0) + CC(X,Y))/2.0;
         else
            return CC(X,Y);
         end if;
      end Metric;


      procedure Put_CC (Val1, Val2 : in Long_Float;
                        Counter : in Long_Integer;
                        Thread  : in Integer) is
      begin
         Text_IO.Put( "CC=" );
         Ada.Long_Float_Text_IO.Put(Val1, Fore=>4, Aft=>10, Exp=>0);
         Ada.Long_Float_Text_IO.Put(Val2, Fore=>4, Aft=>10, Exp=>0);
         Text_IO.Put_Line("  " & Thread'Img & Counter'Img);
      end Put_CC;

      CorrCoeff, Old_CC : Long_Float := 0.0;
      CorrCoeffTest, Old_CCTest : Long_Float := 0.0;
      Progress_Cycle, Spread : Long_Float;
      Counter : Long_Integer := -1;
      Init_Keep : Long_Float;

      function Find_Index (Time : in Long_Float) return Integer is
         Index : Integer;
      begin
         for I in Data_Records'Range loop
            exit when Data_Records(I).Date > Time; -- Finding indices at time
            Index := I;
         end loop;
         return Index;
      end Find_Index;

      TS : Long_Float := GEM.Getenv("TRAIN_START", Data_Records(Data_Records'First).Date);
      TE : Long_Float := GEM.Getenv("TRAIN_END", Data_Records(Data_Records'Last).Date);

      First : Integer := Find_Index (TS); -- Data_Records'First+12;
      Last : Integer := Find_Index (TE); -- Data_Records'Last-12;
      Mid : Integer := (First+Last)/2;

      ------------------------------------------------------------------------
      -- This is close to a violtion of encapsulation, as we need a way
      -- to modify all float parameters without having knowledge of the abstract
      -- objects, so the data record D is overlaid with a plain array Set
      ------------------------------------------------------------------------
      Size_Shared : Positive := D'Size/Long_Float'Size - 1; -- subtract header=2 ints

      package Walker is new GEM.Random_Descent (Fixed => Is_Fixed,
                                                Set_Range => Size_Shared);

      Set, Keep : Walker.LF_Array (1 .. Size_Shared);
      for Set'Address use D.P'Address; -- This may require Ada rec rep clauses
      ------------------------------------------------------------------------

      use Ada.Numerics.Long_Elementary_Functions;
      GM : Gem.LTE.Gravity.Model_Series;

   begin
      for I in First .. Last loop
         RMS_Data := RMS_Data + Data_Records(I).Value * Data_Records(I).Value;
      end loop;
      RMS_Data := Ada.Numerics.Long_Elementary_Functions.Sqrt(RMS_Data);
      Old_CC := 0.0;
      Walker.Reset;
      Text_IO.Put_Line("Catchup mode enabled:" & Boolean'Image(Catchup) );
      Keep := Set;

      loop
         Counter := Counter + 1;
         delay 0.0; -- context switching point if multi-processing not avilable

         Annual_Impulse_Response (GM, D.P);
         for I in Model'Range loop
            Forcing(I).Value := GM(I).Avg;
         end loop;


         --  Forcing := IIR(
         --                  Raw  => Impulse_Amplify(
         --                    Raw    => FIR(
         --                        Raw     => Tide_Sum(Template     => Data_Records,
         --                                            Constituents => D.LP,
         --                                            Ref_Time     => Ref_Time + D.ShiftT,
         --                                            Scaling      => Scaling,
         --                                            Order2       => D.Order2,
         --                                            Order3       => D.Order3
         --                                           ),
         --                      Behind  => D.fB,
         --                      Current => D.fC,
         --                      Ahead   => D.fA),
         --                    Offset => D.Offset),
         --                  lagA => der, lagB => D.mA, lagC => D.mP, lagD => D.mD,
         --                  iA => D.init, iB => 0.0, iC => 0.0, iD => 0.0, Start => 0.0*(TS-(Ref_Time + D.ShiftT)));

         if Forcing_Only then
            Model := Forcing;
         else
            -- Forcing := Median(Forcing);
            Model := Gem.LTE.Primitives.LTE
                        (Forcing => Forcing,
                         Wave_Numbers => D.Lt, Offset => D.P.level, K0 => D.P.K0);  -- D.LT GEM.LTE.LT0

            -- extra filtering, 2 equal-weighted 3-point box windows creating triangle
            Model := FIR(FIR(Model,Filter,1.0-2.0*Filter,Filter), Filter, 1.0-2.0*Filter, Filter);
         end if;

     --    end if;

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
            KeepModel := Model;
            DKeep := D;
            if CatchUp then  -- save it for other threads to reset from
               GEM.LTE.Gravity.Shared.Put(D);
            end if;
         else
            Set := Keep;
         end if;

         if (not Best) and Counter > Maximum_Loops and Percentage < Threshold then
            Text_IO.Put_Line("Resetting" & ID'Img & Percentage'Img & "%");
            if CatchUp then
               D := GEM.LTE.Gravity.Shared.Get(N_Modulations);
            else -- Restart
               D := D0; -- load back reference model parameters
            end if;
            Counter := 1;
            Old_CC := 0.0;
         end if;

         exit when Halted;

         Progress_Cycle := Long_Float(Counter);
         -- This slowly oscillates to change the size of the step to hopefully
         -- help it escape local minima, every N cycles
         if Counter = 0 then
            Spread := 0.0;
         else
            Spread := Spread_Min + Spread_Max*(1.0-LEF.Cos(Progress_Cycle/Spread_Cycle));
         end if;
         if Calibrate then
            Walker.Markov(D.P.Time_Shift, Init_Keep, Spread);
         else
            Walker.Markov(Set, Keep, Spread);
         end if;

      end loop;
      Monitor.Stop;
      if Best_Client = ID then
         GEM.LTE.Gravity.Shared.Save(DKeep);
         Walker.Dump(Keep); -- Print results of last best evaluation,
         Save(KeepModel, Data_Records, Forcing);    -- Should also save to file
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

   end Gravity_Model;


end Gem.LTE.Gravity.Solution;
