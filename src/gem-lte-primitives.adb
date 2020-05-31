with Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.Long_Elementary_Functions;

package body GEM.LTE.Primitives is

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
   end Make_Data;

   -- a running mean filter, used to damp the inpulse response
   function IIR (Raw : in Data_Pairs;
                 lagA, lagB, lagC, lagD : in Long_Float;
                 iA, iB, iC, iD : in Long_Float := 0.0) return  Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      -- The first few values are sensitive to prior information so can adjust
      -- these for better fits in the early part of the time series.
      -- The long range skip value = iD goes back 12 months
      Res(Raw'First + 1).Value := iD; -- -0.000257937;
      Res(Raw'First + 9).Value := iC; -- 0.006775218;
      Res(Raw'First + 10).Value := iB; --0.020083924;
      Res(Raw'First + 11).Value := iA; ---0.006329159;
      for I in Raw'First + 12 .. Raw'Last loop
         Res(I).Value := Raw(I-2).Value  -- this should be I-1 for prior step (will fix)
           + lagA*Res(I-1).Value
           + lagB*Res(I-2).Value
           + lagC*Res(I-3).Value
           + lagD*Res(I-11).Value;
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
                     Offset : in Long_Float) return  Data_Pairs is
      Res : Data_Pairs := Raw;
   begin
      for I in Raw'Range loop
         Res(I).Value := Offset + Raw(I).Value * Impulse(Raw(I).Date);
      end loop;
      return Res;
   end;

   -- Conventional tidal series summation or superposition of cycles
   function Tide_Sum (Template : in Data_Pairs;
                      Constituents : in Long_Periods;
                      Ref_Time : in Long_Float;
                      Scaling : in Long_Float) return Data_Pairs is
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
                  L : Long_Period renames Constituents(J);
               begin
                  TF := TF + L.Amplitude*Cos(2.0*Pi*Year/L.Period*Time + L.Phase);
               end;
            end loop;
            Res(I) := (Time, Scaling * TF);
            -- Text_IO.Put_Line(Res(I).Date'Img & " " & Res(I).Value'Img);
         end;
      end loop;
      return Res;
   end Tide_Sum;

   -- Stands for Laplace's Tidal Equation solution
   -- see Mathematical Geoenergy (2018), Ch.12
   function LTE (Forcing : in Data_Pairs;
                 Wave_Numbers : in Modulations;
                 Offset, K0 : in Long_Float := 0.0) return Data_Pairs is
      Res : Data_Pairs := Forcing;
   begin
      for I in Forcing'Range loop
         declare
            LF : Long_Float := 0.0;
            use Ada.Numerics.Long_Elementary_Functions;
         begin
            for J in Wave_Numbers'Range loop
               declare
                  M : Modulation renames Wave_Numbers(J);
               begin
                  LF := LF + M.Amplitude*Sin(M.Wavenumber*Res(I).Value + M.Phase);
               end;
            end loop;
            LF := LF + Offset + K0*Res(I).Value; -- K0 is wavenumber=0 solution
            Res(I).Value := LF;
         end;
      end loop;
      return Res;
   end LTE;

   -- some of the values may not be modifiesd so this is used to identify them
   function Is_Fixed (Value : in Long_Float) return Boolean is
   begin
      -- If a value happens to have the same value as a tidal period,
      -- do not modify it and leave it as a fixed value.
      for I in LP'Range loop
         if Value = LP(I).Period then
            return True;
         end if;
      end loop;
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
      return (Long_Float(n) * sum_XY - sum_X * sum_Y)
                  / sqrt((Long_Float(n) * squareSum_X - sum_X * sum_X)
                      * (Long_Float(n) * squareSum_Y - sum_Y * sum_Y));
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


   -- protect the file from reentrancy

   protected Safe is
      procedure Save (Model, Data : in Data_Pairs;
                      File_Name : in String);
   end Safe;

   protected body Safe is
      procedure Save (Model, Data : in Data_Pairs;
                      File_Name : in String) is
         FT : Text_IO.File_Type;
      begin
         Text_IO.Create(File => FT, Name=>File_Name, Mode=>Text_IO.Out_File);
         for I in Data'Range loop
            Text_IO.Put_Line(FT, Data(I).Date'Img & ", " & Model(I).Value'Img &
                               ", " & Data(I).Value'Img);
         end loop;
         Text_IO.Close(FT);
      end Save;
   end Safe;


   procedure Save (Model, Data : in Data_Pairs;
                   File_Name : in String := "lte_results.csv") is
   begin
      Safe.Save(Model,Data, File_Name);
   end Save;

end GEM.LTE.Primitives;
