with Ada.Numerics.Long_Elementary_Functions;
with Text_IO;
with Gem.Ephemeris;

package body Gem.LTE.Gravity is

   use Ada.Numerics.Long_Elementary_Functions;
   Pi : constant := Ada.Numerics.pi;

   R0 : constant := 0.962464666;
   D0 : constant := 0.255162759;
   T16pt9 : constant := 1.0/0.059224542;
   Year : constant := 365.25;
   D1962 : constant := 2437665.0 - 82.0*Year;

   procedure Annual_Impulse_Response (D : in out Model_Series;
                                      P : in Parms) is
      Length : Integer := D'Last;
      Declin, Rang, V : Long_Float;
      TJD, GST, R, alpha, delt : Long_Float;
      Imp : Long_Float;

   begin

      GST := 0.0;

   for I in 1 .. Length loop -- 10000

      TJD := P.Time_Shift + D1962 + (Long_Float(I)/12.0 * Year); -- I

      Gem.Ephemeris.MOONPOS(TJD, GST, R, alpha, delt);
      R := R/400_000.0;

      TJD := 1880.0 + (TJD-D1962)/Year; -- 1962.0

      Imp := P.ImpAmp*(ABS SIN(pi*(TJD-P.ImpPhase)))**P.ImpExp +  P.ImpBase;

      D(I) := (TJD, R, Delt, alpha, Imp, 0.0, 0.0, 0.0, 0.0);
      -- Text_IO.Put_Line(FT, TJD'Img & " " & R'Img & " " & Delt'Img & " " & alpha'Img);
   end loop;
   -- Text_IO.Close(FT);

   for I in 2 .. Length-1 loop
      Rang := (D(I-1).R-R0)*P.Range1+ (D(I).R-R0)*P.Range2+ (D(I+1).R-R0)*P.Range3;
      Declin :=  (ABS SIN(D(I-1).Delt)-D0)*P.AbsSin1 +
                 (ABS SIN(D(I).Delt)-D0)*P.AbsSin2 +
                 (ABS SIN(D(I+1).Delt)-D0)*P.AbsSin3 +
            COS(D(I-1).Delt)*P.DCos1 + COS(D(I).Delt)*P.DCos2 + COS(D(I+1).Delt)*P.DCos3 +
            COS(2.0*D(I-1).Delt)*P.D2Cos1 + COS(2.0*D(I).Delt)*P.D2Cos2 + COS(2.0*D(I+1).Delt)*P.D2Cos3 +
            SIN(D(I-1).Delt)*P.DSin1 + SIN(D(I).Delt)*P.DSin2 + SIN(D(I+1).Delt)*P.DSin3;

--      if I < 15 then
--         Text_IO.Put_Line(Rang'Img & "  " & Declin'Img);
--      end if;

      D(I).Force := 3.8136387153462*Rang + (-0.56143373945034)*Declin + P.DR*Declin*Rang + P.DR2*Declin*Rang*Rang+
        P.R2*Rang*Rang + P.DR3*Declin*Rang*Rang*Rang + P.R3*Rang*Rang*Rang +
          P.T16pt9Amp*Cos(2.0*Pi/T16pt9*D(I).T + P.T16pt9Phase);

      D(I).Force := (D(I).Force+P.Shift)*D(I).Impulse + P.Offset;
   end loop;

   D(1).DE := P.IC1;
   D(2).DE := P.IC2;
   D(3).DE := P.IC3;
   for I in 4 .. Length-1 loop
      D(I).DE := (1.0-P.Lag1-P.Lag2)*D(I-1).DE + P.Lag1*D(I-2).DE + P.Lag2*D(I-3).DE
        + P.FMinus*D(I-1).Force + P.F*D(I).Force + P.FPlus*D(I+1).Force;
   end loop;


   for I in 1 .. Length loop
      V := D(I).DE;
      D(I).M := 5.114879841*sin(6.350525071*V-0.827999352) +
        2.517216186*sin(56.67139318*V+1.543293722)
        -3.877257728*sin(170.6009574*V-0.272696364) +
         1.225659486*V -1.442939628;
   end loop;

   for I in 8 .. Length-8 loop
      D(I).Avg := 0.25*(D(I-4).M +D(I-3).M +D(I-2).M)+
                   0.5*(D(I-1).M +D(I).M +D(I+1).M)+
                   0.5*(D(I+2).M +D(I+3).M +D(I+4).M);
      -- Text_IO.Put_Line(FT, D(I).T'Img & " " & D(I).Avg'Img);
   end loop;


   end;


end Gem.LTE.Gravity;
