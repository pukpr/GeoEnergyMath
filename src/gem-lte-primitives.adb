with Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Exceptions;
with Ada.Numerics.Generic_Real_Arrays;

package body GEM.LTE.Primitives is
   Aliased_Period : constant Boolean := GEM.Getenv("ALIAS", FALSE);
   Min_Entropy : constant Boolean := GEM.Getenv("METRIC", "") = "ME";

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
                      Ref_Time : in Long_Float;
                      Scaling : in Long_Float;
                      Order2, Order3 : in Long_Float) return Data_Pairs is
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
                     if Period >= Year_Length/3.01 then
                        Period := Period;
                     else
                        Period := 1.0/(Year_Length/Period - Long_Float(INTEGER(Year_Length/Period)));
                     end if;
                  end if;
                  TF := TF + L.Amplitude*Cos(2.0*Pi*Year_Length/Period*Time + L.Phase); --!
               end;
            end loop;
            Res(I) := (Time, Scaling * (TF + Order2*TF*TF + Order3*TF*TF*TF) );
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
      return 1.0 - sqrt(sum_XY)/Ref;
   end RMS;

   Pi : constant Long_Float := Ada.Numerics.Pi;
   Mult : constant Long_Float := 1.02;  --1.05
   F_Start : constant Long_Float := 1.0; --0.1
   F_End : constant Long_Float := 1000.0;

   function Min_Entropy_Power_Spectrum (X, Y : in Data_Pairs) return Long_Float is
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
         Sum := Sum + Sqrt(S*S + C*C);
         F := F*Mult;
         exit when F > F_End;
         N := N + 1;
      end loop;
      Sum := Sum*Sum/Long_Float(N*N) ;
      Value := (Value-Sum)/Sum;
      --Text_IO.Put_Line(Value'Img & Sum'Img);
      return Value;
   end Min_Entropy_Power_Spectrum;

   procedure ME_Power_Spectrum (Forcing, Model, Data : in Data_Pairs;
                             Model_Spectrum, Data_Spectrum : out Data_Pairs) is
      use Ada.Numerics.Long_Elementary_Functions;
      Model_S : Data_Pairs(Model'Range);
      Data_S : Data_Pairs(Model'Range);
      Value, Sum : Long_Float := 0.0;
      F : Long_Float := F_Start;
      S, C, SM, CM : Long_Float; -- cumulative
      N : Integer := 1;
   begin
      for J in Data'Range loop
         S := 0.0;
         C := 0.0;
         SM := 0.0;
         CM := 0.0;
         for I in Data'Range loop
            S := S + Sin(2.0*Pi*F*Forcing(I).Value)*Data(I).Value;
            C := C + Cos(2.0*Pi*F*Forcing(I).Value)*Data(I).Value;
            SM := SM + Sin(2.0*Pi*F*Forcing(I).Value)*Model(I).Value;
            CM := CM + Cos(2.0*Pi*F*Forcing(I).Value)*Model(I).Value;
         end loop;
         Data_S(J).Date := F;
         Data_S(J).Value := (S*S + C*C);
         Model_S(J).Date := F;
         Model_S(J).Value := (SM*SM + CM*CM);
         F := F*Mult;
      end loop;
      Model_Spectrum := Model_S;
      Data_Spectrum := Data_S;
   end ME_Power_Spectrum;


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
         end if;
         for I in Data'Range loop
            Text_IO.Put_Line(FT, Data_S(I).Date'Img & ", " & Model_S(I).Value'Img &
                               ", " & Data_S(I).Value'Img & ", " & Forcing(I).Value'Img);
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


   procedure Regression_Factors (Data_Records : in Data_Pairs;
                                 First, Last,  -- Training Interval
                                 NM : in Positive; -- # modulations
                                 Forcing : in Data_Pairs;  -- Value @ Time
                                 -- Factors_Matrix : in out Matrix;
                                 DBLT : in Periods;
                                 DALTAP : out Amp_Phases;
                                 DALEVEL : out Long_Float;
                                 DAK0 : out Long_Float;
                                 Secular_Trend : out Long_Float) is

      use Ada.Numerics.Long_Elementary_Functions;
      Pi : Long_Float := Ada.Numerics.Pi;
      Num_Coefficients : constant Integer := 2 + NM*2 + 1; -- !!! sin + cos mod
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
             Factors_Matrix(I-First+1, Num_Coefficients) := Forcing(I).Date;
         end loop;

         declare
            Coefficients : constant Vector :=
                           Regression_Coefficients
                              (Source     => RData,
                               Regressors => Factors_Matrix);
            K : Integer := 4;
         begin
            if Coefficients'Length /= 0 then
               DALevel := Coefficients(1);
               DAK0 := Coefficients(2);
               for I in 1 .. NM loop  -- if odd
                  DALTAP(K/2-1).Amplitude := Sqrt(Coefficients(K-1)*Coefficients(K-1) + Coefficients(K)*Coefficients(K));
                  DALTAP(K/2-1).Phase := Arctan(Coefficients(K), Coefficients(K-1));
                  K := K+2;
               end loop;
               Secular_Trend := Coefficients(Num_Coefficients); --!!!
            end if;
         end;
   end Regression_Factors;

end GEM.LTE.Primitives;
