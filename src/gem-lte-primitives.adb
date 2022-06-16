with Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO;
with Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Exceptions;
with Ada.Numerics.Generic_Real_Arrays;
with Gnat.Os_Lib;
--with GEM.Matrices;

package body GEM.LTE.Primitives is
   Aliased_Period : constant Boolean := GEM.Getenv("ALIAS", FALSE);
   Min_Entropy : constant Boolean := GEM.Getenv("METRIC", "") = "ME";
   -- Trend : constant Boolean := GEM.Getenv("TREND", TRUE);
   Linear_Step : constant Boolean := GEM.Getenv("STEP", FALSE);


   function Is_Minimum_Entropy return Boolean is
   begin
      return Min_Entropy;
   end Is_Minimum_Entropy;

   function File_Lines (Name : String) return Integer is
      Data  : Text_IO.File_Type;
      Count : Integer :=0;
   begin
      Text_IO.Open(File => Data,
                   Mode => Text_IO.In_File,
                   Name => Name);
      loop
         Text_IO.Skip_Line(Data);
         Count := Count + 1;
      end loop;
   exception
      when Text_IO.End_Error =>
         Text_IO.Close(File => Data);
         return Count;
      when E : Others =>
         Text_IO.Put_Line(Ada.Exceptions.Exception_Information(E));
         return 0;
   end File_Lines;

   function Make_Data (Name : String) return Data_Pairs is
      Lines                 : Integer := File_Lines(Name);
      Data                  : Text_IO.File_Type;
      Date, Value           : Long_Float;
      Arr                   : Data_Pairs(1..Lines);
   begin
      Text_IO.Open(File => Data,
                   Mode => Text_IO.In_File,
                   Name => Name);
      for I in 1..Lines loop
         Ada.Long_Float_Text_IO.Get(File => Data, Item => Date);
         Ada.Long_Float_Text_IO.Get(File => Data, Item => Value);
         Text_IO.Skip_Line(Data);
         -- Text_IO.Put_Line(Date'Img & " " & Value'Img);
         Arr(I) := (Date, Value);
      end loop;
      Text_IO.Close(File => Data);
      return Arr;
   exception
      when E : Others =>
         Text_IO.Put_Line(Ada.Exceptions.Exception_Information(E));
         Gnat.Os_Lib.Os_Exit(0);
         return Arr;
   end Make_Data;

   -- a running mean filter, used to damp the inpulse response
   function IIR (Raw : in Data_Pairs;
                 lagA, lagB, lagC : in Long_Float;
                 iA, iB, iC : in Long_Float := 0.0;
                 Start : in Long_Float := Long_Float'First) return  Data_Pairs is
      Res : Data_Pairs := Raw;
      Start_Index : Integer;
   begin
      for I in Raw'Range loop
         Start_Index := I;
         exit when Raw(I).Date > Start;
      end loop;
      --Text_IO.Put_Line(Raw(Start_Index).Date'Img & " " & Start_Index'Img);

      -- The first few values are sensitive to prior information so can adjust
      -- these for better fits in the early part of the time series.
      Res(Start_Index + 1).Value := iC;
      Res(Start_Index + 2).Value := iB;
      Res(Start_Index + 3).Value := iA;
      for I in Start_Index + 4 .. Raw'Last loop
         Res(I).Value := Raw(I-1).Value
           + lagA*Res(I-1).Value
           + lagB*Res(I-2).Value
           + lagC*Res(I-3).Value;
      end loop;
      -- Backwards integration
      --  for I in reverse Raw'First .. Start_Index-1 loop
      --     Res(I).Value := (Res(I+3).Value - Raw(I+1).Value
      --       - lagA*Res(I+2).Value
      --       - lagB*Res(I+1).Value)/lagC;
      --  end loop;
      for I in Raw'First+4 .. Start_Index-1 loop
         Res(I).Value := Raw(I-1).Value
           + lagA*Res(I-1).Value
           + lagB*Res(I-2).Value
           + lagC*Res(I-3).Value;
      end loop;
      return Res;
   end IIR;

   -- also called a boxcar filter, a moving average w/ window of 3 points
   function FIR (Raw : in Data_Pairs;
                 Behind, Current, Ahead : in Long_Float) return  Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      for I in Raw'First + 1 .. Raw'Last - 1 loop
         Res(I).Value := Behind * Raw(I-1).Value +
                         Current * Raw(I).Value +
                         Ahead * Raw(I+1).Value;
      end loop;
      return Res;
   end;

   -- Amplifies a tidal time series with an impulse array (i.e. Dirac comb)
   function Amplify (Raw : in Data_Pairs;
                     Offset, Ramp, Start : in Long_Float) return  Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      for I in Raw'Range loop
         Res(I).Value := Offset + Ramp*(Raw(I).Date-Start) + Raw(I).Value * Impulse(Raw(I).Date);
      end loop;
      return Res;
   end;

   -- Conventional tidal series summation or superposition of cycles
   function Tide_Sum (Template : in Data_Pairs;
                      Constituents : in Long_Periods_Amp_Phase;
                      Periods : in Long_Periods;
                      Ref_Time : in Long_Float := 0.0;
                      Scaling : in Long_Float := 1.0;
                      Cos_Phase : in Boolean := True;
                      Year_Len : in Long_Float := Year_Length
                      ) return Data_Pairs is
      Pi : Long_Float := Ada.Numerics.Pi;
      Time : Long_Float;
      Res : Data_Pairs := Template;
   begin
      for I in Template'Range loop
         Time := Template(I).Date - Ref_Time;
         declare
            TF : Long_Float := 0.0;
            use Ada.Numerics.Long_Elementary_Functions;
         begin
            for J in Constituents'Range loop
               declare
                  L : Amp_Phase renames Constituents(J);
                  Period : Long_Float := Periods(J);
               begin
                  if Aliased_Period then
                     if Period >= Year_Len/3.01 then
                        Period := Period;
                     else
                        Period := 1.0/(Year_Len/Period - Long_Float(INTEGER(Year_Len/Period)));
                     end if;
                  end if;
                  if Cos_Phase then
                     TF := TF + L.Amplitude*Cos(2.0*Pi*Year_Len/Period*Time + L.Phase);
                  else
                     TF := TF + L.Amplitude*Sin(2.0*Pi*Year_Len/Period*Time + L.Phase);
                  end if;
               end;
            end loop;
            Res(I) := (Time, Scaling * TF);
         end;
      end loop;
      return Res;
   end Tide_Sum;


   -- Stands for Laplace's Tidal Equation solution
   -- see Mathematical Geoenergy (2018), Ch.12
   function LTE (Forcing : in Data_Pairs;
                 Wave_Numbers : in Modulations;
                 Amp_Phase : in Modulations_Amp_Phase;
                 Offset, K0, Trend : in Long_Float := 0.0) return Data_Pairs is
      Res : Data_Pairs := Forcing;
   begin
      for I in Forcing'Range loop
         declare
            LF : Long_Float := 0.0;
            use Ada.Numerics.Long_Elementary_Functions;
            Pi : Long_Float := Ada.Numerics.Pi;
         begin
            for J in Wave_Numbers'Range loop
               declare
                  M : GEM.LTE.Amp_Phase renames Amp_Phase(J);
               begin
                  LF := LF + M.Amplitude*Sin(2.0*Pi*Wave_Numbers(J)*Res(I).Value + M.Phase);
               exception
                  when Constraint_Error =>
                     null;
               end;
            end loop;
            LF := LF + Offset + K0*Res(I).Value + Trend*Res(I).Date; -- K0 is wavenumber=0 solution
            Res(I).Value := LF;
         end;
      end loop;
      return Res;
   end LTE;

   -- some of the values may not be modifiesd so this is used to identify them
   function Is_Fixed (Value : in Long_Float) return Boolean is
   begin
      return False;
   end Is_Fixed;

   --
   -- The following are utility functions for simulation
   --

   -- Pearson's Correlation Coefficient
   function CC (X, Y : in Data_Pairs) return Long_Float is
      N : Integer := X'Length;
      sum_X, sum_Y, sum_XY, squareSum_X, squareSum_Y : Long_Float := 0.0;
      use Ada.Numerics.Long_Elementary_Functions;
      J : Integer := Y'First;
      Denominator : Long_Float;
   begin
      for I in X'Range loop
        -- sum of elements of array X.
        sum_X := sum_X + X(i).Value;

        -- sum of elements of array Y.
        sum_Y := sum_Y + Y(j).Value;

        -- sum of X[i] * Y[i].
        sum_XY := sum_XY + X(i).Value * Y(j).Value;

        -- sum of square of array elements.
        squareSum_X := squareSum_X + X(i).Value * X(i).Value;
        squareSum_Y := squareSum_Y + Y(j).Value * Y(j).Value;

        J := J + 1;
      end loop;
      -- assert the denominator that it doesn't hit zero
      Denominator := (Long_Float(n) * squareSum_X - sum_X * sum_X)
                    * (Long_Float(n) * squareSum_Y - sum_Y * sum_Y);
      if Denominator <= 0.0 then
         return 0.0;
      else
         return (Long_Float(n) * sum_XY - sum_X * sum_Y) / Sqrt(Denominator);
      end if;
   exception
      when Constraint_Error =>
         return 0.0;
   end CC;

   -- A zero-crossing metric that is faster than CC which can be used to
   -- improve fits to time-series with many zero crossings, as the precise
   -- amplitude is not critical
   function Xing (X, Y : in Data_Pairs) return Long_Float is
      N : Integer := X'Length;
      sum_absXY, sum_XY : Long_Float := 0.0;
      use Ada.Numerics.Long_Elementary_Functions;
      J : Integer := Y'First;
   begin
      for I in X'Range loop
        -- sum of X[i] * Y[i].
        sum_XY := sum_XY + X(i).Value * Y(j).Value;
        sum_absXY := sum_absXY + abs(X(j).Value)*abs(Y(j).Value);

        J := J + 1;
      end loop;
      return sum_XY / sum_absXY;
   end Xing;

  function RMS (X, Y : in Data_Pairs;
                Ref, Offset : in Long_Float) return Long_Float is
      N : Integer := X'Length;
      sum_XY : Long_Float := 0.0;
      use Ada.Numerics.Long_Elementary_Functions;
      J : Integer := Y'First;
   begin
      for I in X'Range loop
        -- sum of (X[i] - Y[i])^2
        sum_XY := sum_XY + (X(i).Value - Y(j).Value + Offset)**2;
        J := J + 1;
      end loop;
      -- return 1.0 - sqrt(sum_XY)/Ref;
      return 1.0/(1.0+sqrt(sum_XY)/Ref);
   end RMS;

   Pi : constant Long_Float := Ada.Numerics.Pi;
   Mult : constant Long_Float := 1.003;  -- 1.006 1.012 1.02 --1.05
   Step : constant Long_Float := 0.02;
   F_Start : constant Long_Float := 0.3; -- 0.01; --1.0
   F_End : constant Long_Float := 1000.0;

   function Min_Entropy_RMS (X, Y : in Data_Pairs) return Long_Float is
      use Ada.Numerics.Long_Elementary_Functions;
      Value, Sum : Long_Float := 0.0;
      S, C : Long_Float; -- cumulative
      F : Long_Float := F_Start;
      N : Integer := 1;
   begin
      loop
         S := 0.0;
         C := 0.0;
         for I in X'Range loop
            S := S + Sin(2.0*Pi*F*X(I).Value)*Y(I).Value;
            C := C + Cos(2.0*Pi*F*X(I).Value)*Y(I).Value;
         end loop;
         Value := Value + (S*S + C*C);
         Sum := Sum + Sqrt((S*S + C*C));
         if Linear_Step then
            F := F + Step;
         else
            F := F*Mult;
         end if;
         exit when F > F_End;
         N := N + 1;
      end loop;
      Sum := Sum*Sum/Long_Float(N*N) ;
      Value := (Value-Sum)/Sum;
      --Text_IO.Put_Line(Value'Img & Sum'Img);
      return Value;
   end Min_Entropy_RMS;


   procedure LTE_Power_Spectrum (Forcing, Model, Data : in Data_Pairs;
                             Model_Spectrum, Data_Spectrum : out Data_Pairs) is
     use Ada.Numerics.Long_Elementary_Functions;
     Model_S : Data_Pairs(Model'Range);
     Data_S : Data_Pairs(Data'Range);
     Value, Sum : Long_Float := 0.0;
     F : Long_Float := F_Start;
     S, C : Long_Float; -- cumulative
     N : Integer := 1;
   begin
      for J in Data'Range loop
         S := 0.0;
         C := 0.0;
         for I in Data'First+8 .. Data'Last loop  -- remove Init value
            S := S + Sin(2.0*Pi*F*Forcing(I).Value)*Data(I).Value;
            C := C + Cos(2.0*Pi*F*Forcing(I).Value)*Data(I).Value;
         end loop;
         Data_S(J).Date := F;
         Data_S(J).Value := (S*S + C*C);
         if Linear_Step then
            F := F + Step;
         else
            F := F*Mult;
         end if;
      end loop;
      F := F_Start;
      for J in Model'Range loop
         S := 0.0;
         C := 0.0;
         for I in Model'First+8 .. Model'Last loop  -- remove Init value
            S := S + Sin(2.0*Pi*F*Forcing(I).Value)*Model(I).Value;
            C := C + Cos(2.0*Pi*F*Forcing(I).Value)*Model(I).Value;
         end loop;
         Model_S(J).Date := F;
         Model_S(J).Value := (S*S + C*C);
         if Linear_Step then
            F := F + Step;
         else
            F := F*Mult;
         end if;
      end loop;
      Model_Spectrum := Model_S;
      Data_Spectrum := Data_S;
   end LTE_Power_Spectrum;

   procedure ME_Power_Spectrum (Forcing, Model, Data : in Data_Pairs;
                             Model_Spectrum, Data_Spectrum : out Data_Pairs) is
     use Ada.Numerics.Long_Elementary_Functions;
     Model_S : Data_Pairs(Model'Range);
     Data_S : Data_Pairs(Data'Range);
     Value, Sum : Long_Float := 0.0;
     F : Long_Float := F_Start;
     S, C : Long_Float; -- cumulative
     N : Integer := 1;
   begin
      for J in Data'Range loop
         S := 0.0;
         C := 0.0;
         for I in Data'First+8 .. Data'Last loop  -- remove Init value
            S := S + Sin(2.0*Pi*F*Forcing(I).Value)*Data(I).Value;
            C := C + Cos(2.0*Pi*F*Forcing(I).Value)*Data(I).Value;
         end loop;
         Data_S(J).Date := F;
         Data_S(J).Value := (S*S + C*C);
         if Linear_Step then
            F := F + Step;
         else
            F := F*Mult;
         end if;
      end loop;
      F := F_Start;
      for J in Model'Range loop
         S := 0.0;
         C := 0.0;
         for I in Model'First+8 .. Model'Last loop  -- remove Init value
            S := S + Sin(2.0*Pi*F*Forcing(I).Value)*Model(I).Value;
            C := C + Cos(2.0*Pi*F*Forcing(I).Value)*Model(I).Value;
         end loop;
         Model_S(J).Date := F;
         Model_S(J).Value := (S*S + C*C);
         if Linear_Step then
            F := F + Step;
         else
            F := F*Mult;
         end if;
      end loop;
      Model_Spectrum := Model_S;
      Data_Spectrum := Data_S;
   end ME_Power_Spectrum;

   function Min_Entropy_Power_Spectrum (X, Y : in Data_Pairs) return Long_Float is
      First : Positive := X'First;
      Last : Positive := X'Last;
      Mid : Positive := (First+Last)/2;
      FD : Data_Pairs := Y(First..Mid);
      LD : Data_Pairs := Y(Mid..Last);
   begin
      if GEM.Getenv("MERMS", False) then
         return Min_Entropy_RMS (X, Y);
      else
         ME_Power_Spectrum (X, FD, LD, FD, LD);
         return CC(Filter9Point(FD), Filter9Point(LD));
         -- return CC(Window(FD,2), Window(LD,2));
      end if;
   end Min_Entropy_Power_Spectrum;


   --
   procedure Dump (Model, Data : in Data_Pairs;
                  Run_Time : Long_Float := 200.0) is
   begin
      for I in Data'Range loop
         Text_IO.Put_Line(Data(I).Date'Img & " " & Model(I).Value'Img &
                                              " " & Data(I).Value'Img);
         exit when Model(I).Date > Run_Time;
      end loop;
   end Dump;

   Running : Boolean := True;

   procedure Stop is
   begin
      Running := False;
   end Stop;

   function Halted return Boolean is
   begin
      return not Running;
   end Halted;

   --
   -- protect the file from reentrancy
   --

   protected Safe is
      procedure Save (Model, Data, Forcing : in Data_Pairs;
                      File_Name : in String);
   end Safe;

   protected body Safe is
      procedure Save (Model, Data, Forcing : in Data_Pairs;
                      File_Name : in String) is
         FT : Text_IO.File_Type;
         Model_S : Data_Pairs := Model;
         Data_S : Data_Pairs := Data;
      begin
         Text_IO.Create(File => FT, Name=>File_Name, Mode=>Text_IO.Out_File);
         if Is_Minimum_Entropy then
            ME_Power_Spectrum (Forcing=>Model, Model=>Forcing, Data=>Data,
                               Model_Spectrum=>Model_S, Data_Spectrum=>Data_S);
         else
            ME_Power_Spectrum (Forcing=>Forcing, Model=>Model, Data=>Data,
                               Model_Spectrum=>Model_S, Data_Spectrum=>Data_S);
            Model_S := Window(Model_S,2);
            Data_S := Window(Data_S,2);
         end if;
         for I in Data'Range loop
            Text_IO.Put_Line(FT, Data(I).Date'Img & ", " & Model(I).Value'Img &
                               ", " & Data(I).Value'Img & ", " & Forcing(I).Value'Img &
                               ", " & Data_S(I).Date'Img & ", " & Model_S(I).Value'Img &
                               ", " & Data_S(I).Value'Img );
         end loop;
         Text_IO.Close(FT);
      end Save;
   end Safe;

   procedure Save (Model, Data, Forcing : in Data_Pairs;
                   File_Name : in String := "lte_results.csv") is
   begin
      Safe.Save(Model,Data, Forcing, File_Name);
   end Save;

   -- 3 point median
   function Median (Raw : in Data_Pairs) return Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      for I in Raw'First + 1 .. Raw'Last - 1 loop
         if Raw(I-1).Value < Raw(I).Value then
            if Raw(I-1).Value >= Raw(I+1).Value then
               Res(I).Value := Raw(I-1).Value;
            elsif Raw(I).Value < Raw(I+1).Value then
               Res(I).Value := Raw(I).Value;
            else
               Res(I).Value := Raw(I+1).Value;
            end if;
         else
            if Raw(I-1).Value < Raw(I+1).Value then
               Res(I).Value := Raw(I-1).Value;
            elsif Raw(I).Value >= Raw(I+1).Value then
               Res(I).Value := Raw(I).Value;
            else
               Res(I).Value := Raw(I+1).Value;
            end if;
         end if;
      end loop;
      return Res;
   end Median;

   function Window (Raw : in Data_Pairs;
                    Lobe_Width : in Positive) return Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      for I in Raw'First + Lobe_Width .. Raw'Last - Lobe_Width loop
         Res(I).Value := 0.0;
         for J in I - Lobe_Width .. I + Lobe_Width loop
            Res(I).Value := Res(I).Value +  Raw(J).Value;
         end loop;
         Res(I).Value := Res(I).Value / Long_Float(2*Lobe_Width+1);
      end loop;
      return Res;
   end Window;


   -- Regression procedures
--   package MLR is new Gem.Matrices (
--      Element_Type => Long_Float,
--      Zero => 0.0,
--      One => 1.0);
--   subtype Vector is MLR.Vector;
--   subtype Matrix is MLR.Matrix;


   package MLR is new Ada.Numerics.Generic_Real_Arrays (Real => Long_Float);
   subtype Vector is MLR.Real_Vector;
   subtype Matrix is MLR.Real_Matrix;

   -- Multiple Linear Regression types

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
         delay 1.0;
         return Nil; -- Source;  -- Doesn't matter, will give a junk result
   end Regression_Coefficients;


   procedure Regression_Factors (Data_Records : in Data_Pairs;
                                 First, Last,  -- Training Interval
                                 NM : in Positive; -- # modulations
                                 Forcing : in Data_Pairs;  -- Value @ Time
                                 -- Factors_Matrix : in out Matrix;
                                 DBLT : in Periods;
                                 DALTAP : out Amp_Phases;
                                 DALEVEL : out Long_Float;
                                 DAK0 : out Long_Float;
                                 Secular_Trend : in out Long_Float;
                                 Singular : out Boolean) is

      use Ada.Numerics.Long_Elementary_Functions;
      Pi : Long_Float := Ada.Numerics.Pi;
      Trend : Boolean := Secular_Trend > 0.0;
      -- Add_Trend : Integer := Integer(Secular_Trend);
      Add_Trend : Integer := Boolean'Pos(Trend);
      Num_Coefficients : constant Integer := 2 + NM*2 + Add_Trend; -- !!! sin + cos mod
      RData : Vector (1 .. Last-First+1);
      Factors_Matrix : Matrix (1 .. Last-First+1, 1 .. Num_Coefficients);
   begin
      for I in First .. Last loop
         RData(I-First+1) := Data_Records(I).Value;
         Factors_Matrix(I-First+1, 1) := 1.0;  -- DC offset
         Factors_Matrix(I-First+1, 2) := Forcing(I).Value;
         for K in DBLT'First .. NM loop  -- D.B.LT'First = 1
            Factors_Matrix(I-First+1, 3+(K-1)*2) := Sin(2.0*Pi*DBLT(K) * Forcing(I).Value);
            Factors_Matrix(I-First+1, 4+(K-1)*2) := Cos(2.0*Pi*DBLT(K) * Forcing(I).Value);
         end loop;
         if Trend then
            Factors_Matrix(I-First+1, Num_Coefficients) := Forcing(I).Date;
         end if;
      end loop;

      declare
         Coefficients : constant Vector := -- MLR.
                        Regression_Coefficients
                           (Source     => RData,
                            Regressors => Factors_Matrix);
         K : Integer := 4;
      begin
         if Coefficients'Length = 0 then
            Singular := True;
         else
            DALevel := Coefficients(1);
            DAK0 := Coefficients(2);
            for I in 1 .. NM loop  -- if odd
               DALTAP(K/2-1).Amplitude := Sqrt(Coefficients(K-1)*Coefficients(K-1) + Coefficients(K)*Coefficients(K));
               DALTAP(K/2-1).Phase := Arctan(Coefficients(K), Coefficients(K-1));
               K := K+2;
            end loop;
            if Trend then
               Secular_Trend := Coefficients(Num_Coefficients); --!!!
            else
               Secular_Trend := 0.0;
            end if;
            Singular := False;
         end if;
      end;
   end Regression_Factors;

   procedure Put (Value : in Long_Float;
                  Text : in String := "";
                  New_Line : in Boolean := False) is
   begin
      Ada.Long_Float_Text_IO.Put(Value, Fore=>4, Aft=>11, Exp=>0);
      Text_IO.Put(Text);
      if New_Line then
         Text_IO.New_Line;
      end if;
   end Put;

   function Filter9Point (Raw : in Data_Pairs) return  Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      for I in Raw'First + 4 .. Raw'Last - 4 loop
         Res(I).Value :=
           1.0/9.0 * ( 0.25*(Raw(I-4).Value + Raw(I-3).Value + Raw(I-2).Value)+
                       0.5*(Raw(I-1).Value + Raw(I).Value + Raw(I+1).Value)+
                       0.25*(Raw(I+2).Value + Raw(I+3).Value + Raw(I+4).Value) );
      end loop;
      return Res;
   end Filter9Point;


end GEM.LTE.Primitives;
