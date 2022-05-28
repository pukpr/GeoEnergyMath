with Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Long_Float_Text_IO;
with GEM.Random_Descent;
with GNAT.Ctrl_C;
with System.Task_Info;
with GEM.LTE.Primitives.Shared;
with Ada.Exceptions;
with Gnat.Traceback.Symbolic;
--with GEM.Matrices;
with Ada.Numerics.Generic_Real_Arrays;

package body GEM.LTE.Primitives.Solution is

   -- Multiple Linear Regression types
--G   package MLR is new Gem.Matrices (
--G      Element_Type => Long_Float,
--G      Zero => 0.0,
--G      One => 1.0);
--G   subtype Vector is MLR.Vector;
--G   subtype Matrix is MLR.Matrix;
   package MLR is new Ada.Numerics.Generic_Real_Arrays (Real => Long_Float);
   subtype Vector is MLR.Real_Vector;
   subtype Matrix is MLR.Real_Matrix;

   function To_Matrix
     (Source        : Vector;
      Column_Vector : Boolean := True)
      return          Matrix
   is
      Result : Matrix (1 .. 1, Source'Range);
   begin
      for Column in Source'Range loop
         Result (1, Column) := Source (Column);
      end loop;
      if Column_Vector then
         return MLR.Transpose (Result);
      else
         return Result;
      end if;
   end To_Matrix;

   function To_Row_Vector
     (Source : Matrix;
      Column : Positive := 1)
      return   Vector
   is
      Result : Vector (Source'Range (1));
   begin
      for Row in Result'Range loop
         Result (Row) := Source (Row, Column);
      end loop;
      return Result;
   end To_Row_Vector;

   function Regression_Coefficients
     (Source     : Vector;
      Regressors : Matrix)
      return       Vector
   is
      Result : Matrix (Regressors'Range (2), 1 .. 1);
      Nil : Vector(1..0);
   begin
      if Source'Length /= Regressors'Length (1) then
         raise Constraint_Error;
      end if;
      declare
         Regressors_T : constant Matrix := MLR.Transpose (Regressors);
         use MLR;
      begin
         Result := MLR.Inverse (Regressors_T * Regressors) *
                   Regressors_T *
                   To_Matrix (Source);
      end;
      return To_Row_Vector (Source => Result);
   exception
      when Constraint_Error => -- Singular
         Text_IO.Put_Line("Singular result, converging?");
         return Nil; -- Source;  -- Doesn't matter, will give a junk result
   end Regression_Coefficients;

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

      Data_Records : Data_Pairs := Make_Data(File_Name);

      function Impulse (Time : Long_Float) return Long_Float;
      function Impulse_Sin (Time : Long_Float) return Long_Float;

      function Impulse_Amplify is new Amplify(Impulse => Impulse_Sin);

      Ref_Time : Long_Float := GEM.Getenv("REF_TIME", Data_Records(Data_Records'First).Date);
      Impulses : Data_Pairs := Data_Records;
      Forcing  : Data_Pairs := Data_Records;
      Model    : Data_Pairs := Data_Records;
      KeepModel: Data_Pairs := Data_Records;

      Best  : Boolean := False;
      Percentage : Integer;
      Best_Client : Integer;

      D : Shared.Param_S(N_Tides, N_Modulations) := GEM.LTE.Primitives.Shared.Get(N_Tides, N_Modulations);
      DKeep : Shared.Param_S(N_Tides, N_Modulations) := D;
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
      RMS_Metric : constant Boolean := GEM.Getenv("METRIC", "CC") = "RMS";
      Sin_Impulse : constant Boolean := GEM.Getenv("IMPULSE", "DELTA") = "SIN";
      Sampling_Per_Year : constant Long_Float := GEM.Getenv("SAMPLING", 12.0);
      Filter : constant Long_Float := GEM.Getenv("FILTER", 0.33333333);
      MLR_On : constant Boolean := GEM.Getenv("MLR", FALSE);
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

      function Impulse (Time : Long_Float) return Long_Float is
         Value : Long_Float;
         -- Impulses will occur on a month for monthly data
         Trunc : Integer := Integer((Time - Long_Float'Floor(Time))*Sampling_Per_Year);
      begin
         if Trunc = ImpA then
            Value := D.B.ImpA;
         elsif Trunc = ImpB then
            Value := -D.B.ImpA + D.B.ImpB; -- delta
         elsif Trunc = ImpC then
            Value := 0.0; -- D.B.ImpC;
         elsif Trunc = ImpD then
            Value := 0.0; -- D.B.ImpD;
         else
            Value := 0.0;
         end if;
         return Value; -- + D.B.bg;
      end Impulse;

      function Impulse_Power (Time : Long_Float) return Long_Float is
         Pi : Long_Float := Ada.Numerics.Pi;
         Value : Long_Float := 0.0;
         use Ada.Numerics.Long_Elementary_Functions;
      begin
         -- Optimize the impulse power, this will create an even and odd impulse
         --Value := D.B.ImpA*(abs(COS(2.0*Pi*(Time+D.B.ImpB))))**D.B.ImpD
         --        +D.B.ImpC*(abs(COS(2.0*Pi*(Time+D.B.ImpB))))**D.B.ImpD * COS(2.0*Pi*(Time+D.B.ImpB));
         return Value; -- + D.B.bg;
      end Impulse_Power;


      function Impulse_Sin (Time : Long_Float) return Long_Float is
         Pi : Long_Float := Ada.Numerics.Pi;
         Value : Long_Float := 0.0;
         use Ada.Numerics.Long_Elementary_Functions;
         -- scale*(COS(2*PI*(E2+ip)))^2+D17*(COS(2*PI*(E2+ip)))^3
      begin
         if not Sin_Impulse then
            return Impulse(Time);
         end if;
         if ImpA > 0 then
            -- using the impA & impB env vars as odd & even powers, since they won't be used for impulse
            null; -- Value := D.B.ImpA*(COS(2.0*Pi*(Time+D.B.ImpB)))**ImpA+D.B.ImpC*(COS(2.0*Pi*(Time+D.B.ImpB)))**ImpB + D.B.ImpD*COS(2.0*Pi*(Time+D.B.ImpB));
         else
            return Impulse_Power(Time);
         end if;
         return Value; -- + D.B.bg;
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

      der : Long_Float;
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
      Size_Shared : Positive := D.B'Size/Long_Float'Size - 1; -- subtract header=2 ints

      package Walker is new GEM.Random_Descent (Fixed => Is_Fixed,
                                                Set_Range => Size_Shared);

      Set, Keep : Walker.LF_Array (1 .. Size_Shared);
      for Set'Address use D.B.Offset'Address; -- This may require Ada rec rep clauses
      ------------------------------------------------------------------------

      RData : Vector (1 .. Last-First+1);
      NM : Integer := GEM.Getenv("NM", N_Modulations);
      Num_Coefficients : constant Integer := 2 + NM*2 + 1; -- !!! sin + cos mod
      Factors_Matrix : Matrix (1 .. Last-First+1, 1 .. Num_Coefficients);
      use Ada.Numerics.Long_Elementary_Functions;
      Pi : Long_Float := Ada.Numerics.Pi;
      Secular_Trend : Long_Float := 0.0;
      --!
      Impulse_Residual : Long_Float := GEM.Getenv("IR", 0.0);
   begin
      for I in First .. Last loop
         RData (I-First+1) := Data_Records(I).Value;
         Factors_Matrix (I-First+1, 1) := 1.0;  -- DC offset
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

         -- Tidal constituents summed, amplified by impulse, and LTE modulated

         der := 1.0 - D.B.mA - D.B.mP; -- keeps the integrator stable
         Impulses := Impulse_Amplify(
                            Raw     => Tide_Sum(Template     => Data_Records,
                                                Constituents => D.B.LP,
                                                Periods      => D.A.LPF,
                                                Ref_Time     => Ref_Time + D.B.ShiftT,
                                                Scaling      => Scaling,
                                                Order2       => 0.0, --D.B.Order2,
                                                Order3       => 0.0  --D.B.Order3
                                               ),
                          Offset => D.B.Offset, Ramp => D.B.bg, Start => Data_Records(Data_Records'First).Date);

         Forcing := IIR( Raw => Impulses,
                         lagA => der, lagB => D.B.mA, lagC => D.B.mP,
                         iA => D.B.init, iB => 0.0, iC => 0.0, Start => 0.0*(TS-(Ref_Time + D.B.ShiftT)));

         --  add a portion of the impulse back in
         for I in First .. Last loop
            Forcing(I).Value := Forcing(I).Value + Impulse_Residual*Impulses(I).Value;
         end loop;

         if not Forcing_Only then -- MLR_On
            for I in First .. Last loop
                Factors_Matrix(I-First+1, 2) := Forcing(I).Value;
                for K in D.B.LT'First .. NM loop  -- D.B.LT'First = 1
                    Factors_Matrix(I-First+1, 3+(K-1)*2) := Sin(2.0*Pi*D.B.LT(K) * Forcing(I).Value);
                    Factors_Matrix(I-First+1, 4+(K-1)*2) := Cos(2.0*Pi*D.B.LT(K) * Forcing(I).Value);
                end loop;
                --!
                --Factors_Matrix(I-First+1, Num_Coefficients-1) := Impulses(I).Value;
                Factors_Matrix(I-First+1, Num_Coefficients) := Forcing(I).Date;
            end loop;

            declare
               Coefficients : constant Vector := -- MLR.
                              Regression_Coefficients
                                 (Source     => RData,
                                  Regressors => Factors_Matrix);
               K : Integer := 4;
            begin
               if Coefficients'Length /= 0 then
                  D.A.Level := Coefficients(1);
                  D.A.K0 := Coefficients(2);
                  for I in 1 .. NM loop  -- if odd
                     D.A.LTAP(K/2-1).Amplitude := Sqrt(Coefficients(K-1)*Coefficients(K-1) + Coefficients(K)*Coefficients(K));
                     D.A.LTAP(K/2-1).Phase := Arctan(Coefficients(K), Coefficients(K-1));
                     K := K+2;
                  end loop;
                  --!
                  --Impulse_Residual := Coefficients(Num_Coefficients-1); --!!!
                  Secular_Trend := Coefficients(Num_Coefficients); --!!!
               end if;
            end;
         end if;

         if Forcing_Only then
            Model := Forcing;
         else
            -- Forcing := Median(Forcing);

            Model := LTE(Forcing => Forcing,
                         Wave_Numbers => D.B.Lt(1..NM),
                         Amp_Phase => D.A.LTAP,
                         Offset => D.A.level,
                         K0 => D.A.K0,
                         Trend => Secular_Trend);  -- D.LT GEM.LTE.LT0

            -- extra filtering, 2 equal-weighted 3-point box windows creating triangle
            Model := FIR(FIR(Model,Filter,1.0-2.0*Filter,Filter), Filter, 1.0-2.0*Filter, Filter);
            --Model := Window(Window(Model, 5),5);
            --Model := FIR(FIR(Model,Filter,1.0-2.0*Filter,Filter), Filter, 1.0-2.0*Filter, Filter);
            --Model := FIR(FIR(Model,Filter,1.0-2.0*Filter,Filter), Filter, 1.0-2.0*Filter, Filter);
            --Model := FIR(FIR(Model,Filter,1.0-2.0*Filter,Filter), Filter, 1.0-2.0*Filter, Filter);
            --Model := FIR(FIR(Model,Filter,1.0-2.0*Filter,Filter), Filter, 1.0-2.0*Filter, Filter);
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
            Walker.Markov(D.B.Init, Init_Keep, Spread);
            Walker.Markov(D.B.shiftT, Init_Keep, Spread);
         else
            Walker.Markov(Set, Keep, Spread);
         end if;

      end loop;
      Monitor.Stop;
      if Best_Client = ID then
         GEM.LTE.Primitives.Shared.Save(DKeep);
         -- Walker.Dump(Keep); -- Print results of last best evaluation,
         Ada.Long_Float_Text_IO.Put(D.B.Offset, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :offset:");
         Ada.Long_Float_Text_IO.Put(D.B.Bg, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :bg:");
         Ada.Long_Float_Text_IO.Put(D.B.ImpA, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :impA:");
         Ada.Long_Float_Text_IO.Put(D.B.ImpB, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :impB:");
         Ada.Long_Float_Text_IO.Put(D.B.mA, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :mA:");
         Ada.Long_Float_Text_IO.Put(D.B.mP, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :mP:");
         Ada.Long_Float_Text_IO.Put(D.B.shiftT, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :shiftT:");
         Ada.Long_Float_Text_IO.Put(D.B.init, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :init:");
         Text_IO.Put_Line("---- LTE");
         Ada.Long_Float_Text_IO.Put(D.A.K0, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :K0:");
         Ada.Long_Float_Text_IO.Put(D.A.Level, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :level:");
         for I in D.B.LT'Range loop
            Ada.Long_Float_Text_IO.Put(D.B.LT(I), Fore=>4, Aft=>11, Exp=>0);
            Text_IO.Put(", ");
            Ada.Long_Float_Text_IO.Put(D.A.LTAP (I).Amplitude, Fore=>4, Aft=>11, Exp=>0);
            Text_IO.Put(", ");
            Ada.Long_Float_Text_IO.Put(D.A.LTAP(I).Phase, Fore=>4, Aft=>11, Exp=>0);
            Text_IO.New_Line;
         end loop;
         Text_IO.Put_Line("---- Tidal");
         for I in D.B.LP'Range loop
            Ada.Long_Float_Text_IO.Put(D.A.LPF(I), Fore=>4, Aft=>11, Exp=>0);
            Text_IO.Put(", ");
            Ada.Long_Float_Text_IO.Put(D.B.LP(I).Amplitude, Fore=>4, Aft=>11, Exp=>0);
            Text_IO.Put(", ");
            Ada.Long_Float_Text_IO.Put(D.B.LP(I).Phase, Fore=>4, Aft=>11, Exp=>0);
            Text_IO.New_Line;
         end loop;
         Ada.Long_Float_Text_IO.Put(Impulse_Residual, Fore=>4, Aft=>11, Exp=>0); Text_IO.Put_Line(" :ir:");

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

   end Dipole_Model;


end GEM.LTE.Primitives.Solution;
