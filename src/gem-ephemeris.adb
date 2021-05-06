with Ada.Numerics.Long_Elementary_Functions;

package body Gem.Ephemeris is

   use Ada.Numerics.Long_Elementary_Functions;

   -------------
   -- MOONPOS --
   -------------

   procedure MOONPOS
     (TJD, GST : in Long_Float; R, alpha, delt : out Long_Float)
   is
      -- https://www.sciencedirect.com/science/article/pii/B9780128205136000229
      -- calculates the geocentric position of the Moon
      -- as range R, right ascension & declination at given epoch
      -- (apparent position correction is not considered.)

      pi,irad,T,lambda,B : Long_Float;

      L,M,Mm,D,F,eo,A1,A2,A3,C1,C2,C3,C4 : Long_Float;

      -- TJD : Julian date
      -- T : Julian century from 2000 Jan 0.5 ET
      -- R : Range to the moon (in kilometer)
      -- alpha : right ascension of the moon - GST
      -- delta : declination of the moon
      -- L : mean longitude of the moon
      -- M : mean anomaly of the sun
      -- Mm : mean anomaly of the moon
      -- D : mean elongation of the moon
      -- F : mean angle of the moon from its ascending node
      -- eo : obliquity of the ecliptic
      -- (after Meeus - Astronomical Algorithms 1998)

   begin

      pi := 3.141592653589793;

      irad := pi/180.0;

      T := ( TJD - 2451545.0)/36525.0;

      L := 218.3164477 + (481267.88123421+(-0.0015786
                          +(1.85584E-6 - 1.53388E-8*T)*T)*T)*T;

      L := Long_Float'Remainder(L, 360.0);

      M := 357.5291092 + (35999.0502909+(-0.0001536
                          -4.0833E-8*T)*T)*T;

      M := Long_Float'Remainder(M, 360.0);

      Mm := 134.9633964 + (477198.8675055+(0.0087414
                           + (1.43741E-6 - 6.7972E-8*T)*T)*T)*T;

      Mm := Long_Float'Remainder(Mm, 360.0);

      D := 297.8501921 + (445267.1114034+(-1.8819E-3
                          + (1.831945E-6 - 8.84447E-9*T)*T)*T)*T;

      D := Long_Float'Remainder(D, 360.0);

      F := 93.272095 + (483202.0175233+(-0.0036539
                        + (-2.8361E-7 + 1.15833E-9*T)*T)*T)*T;

      F := Long_Float'Remainder(F, 360.0);

      A1 := 119.75 + 131.849*T;

      A2 := 53.09 + 479264.290*T;

      A3 := 313.45 + 481266.484*T;

      A1 := Long_Float'Remainder(A1, 360.0);

      A2 := Long_Float'Remainder(A2, 360.0);

      A3 := Long_Float'Remainder(A3, 360.0);

      --!!! ce := 1.0 + (-0.002516-0.0000074*T)*T;

      -- unit conversion(degree -> radian)

      M := M*irad ; Mm := Mm*irad ; D := D*irad ; F := F*irad;

      A1 := A1*irad ; A2 := A2*irad ; A3 := A3*irad;

      -- unit conversion end
      -- lambda, B, px are still expressed in degrees!

      lambda := L + 6.288774*sin(Mm)
        + 1.274027*sin(2.0*D-Mm)
        + 0.658314*sin(2.0*D)
        + 0.213618*sin(2.0*Mm)
        - 0.185116*sin(M)
        - 0.114332*sin(2.0*F)
        + 0.058793*sin(2.0*(D-Mm))
        + 0.057066*sin(2.0*D-M-Mm)
        + 0.053322*sin(2.0*D+Mm)
        + 0.045758*sin(2.0*D-M)
        - 0.040923*sin(M-Mm)
        - 0.034720*sin(D)
        - 0.030383*sin(M+Mm)
        + 0.015327*sin(2.0*D-2.0*F)
        - 0.012528*sin(Mm+2.0*F)
        + 0.010980*sin(Mm-2.0*F)
        + 0.010675*sin(4.0*D-Mm)
        + 0.010034*sin(3.0*Mm)
        + 0.008548*sin(4.0*D-2.0*Mm)
        - 0.007888*sin(2.0*D+M-Mm)
        - 0.006766*sin(2.0*D+M)
        - 0.005163*sin(D-Mm)
        + 0.004987*sin(D+M)
        + 0.004036*sin(2.0*D-M+Mm)
        + 0.003994*sin(2.0*D+2.0*Mm)
        + 0.003841*sin(4.0*D)
        + 0.003665*sin(2.0*D-3.0*Mm)
        - 0.002689*sin(M-2.0*Mm)
        - 0.002602*sin(2.0*D-Mm+2.0*F)
        + 0.002390*sin(2.0*D-M-2.0*Mm)
        - 0.002348*sin(D+Mm)
        + 0.002236*sin(2.0*D-2.0*M)
        - 0.002120*sin(M+2.0*Mm)
        - 0.002069*sin(2.0*M)
        + 0.002048*sin(2.0*D-2.0*M-Mm)
        - 0.001773*sin(2.0*D+Mm-2.0*F)
        - 0.001595*sin(2.0*D+2.0*F)
        + 0.001215*sin(4.0*D-M-Mm)
        - 0.001110*sin(2.0*Mm+2.0*F)
        - 0.000892*sin(3.0*D-Mm)
        - 0.000810*sin(2.0*D+M+Mm)
        + 0.000759*sin(4.0*D-M-2.0*Mm)
        - 0.000713*sin(2.0*M-Mm)
        - 0.000700*sin(2.0*D+2.0*M-Mm)
        + 0.000691*sin(2.0*D+M+2.0*Mm)
        + 0.000596*sin(2.0*D-M-2.0*F)
        + 0.000549*sin(4.0*D+Mm)
        + 0.000537*sin(4.0*Mm)
        + 0.000520*sin(4.0*D-M)
        - 0.000487*sin(D-2.0*Mm)
        - 0.000399*sin(2.0*D+M-2.0*F)
        - 0.000381*sin(2.0*Mm-2.0*F)
        + 0.000351*sin(D+M+Mm)
        - 0.000340*sin(3.0*D-2.0*Mm)
        + 0.000330*sin(4.0*D-3.0*Mm)
        + 0.000327*sin(2.0*D-M-2.0*Mm)
        - 0.000323*sin(2.0*M+Mm)
        + 0.000299*sin(D+M-Mm)
        + 0.000294*sin(2.0*D+3.0*Mm)
        + 0.003958*sin(A1)
        + 0.001962*sin(L-F)
        + 0.000318*sin(A2);

      B := 5.128122*sin(F)
        + 0.280602*sin(Mm+F)
        + 0.277693*sin(Mm-F)
        + 0.173237*sin(2.0*D-F)
        + 0.055413*sin(2.0*D+F-Mm)
        + 0.046271*sin(2.0*D-F-Mm)
        + 0.032573*sin(2.0*D+F)
        + 0.017198*sin(2.0*Mm+F)
        + 0.009266*sin(2.0*D+Mm-F)
        + 0.008822*sin(2.0*Mm-F)
        + 0.008216*sin(2.0*D-M-F)
        + 0.004324*sin(2.0*D-F-2.0*Mm)
        + 0.004200*sin(2.0*D+F+Mm)
        - 0.003359*sin(2.0*D+M-F)
        + 0.002463*sin(2.0*D-M-Mm-F)
        + 0.002211*sin(2.0*D-M+F)
        + 0.002065*sin(2.0*D-M-Mm-F)
        - 0.001870*sin(M-Mm-F)
        + 0.001828*sin(4.0*D-Mm-F)
        - 0.001794*sin(M+F)
        - 0.001749*sin(3.0*F)
        - 0.001565*sin(M-Mm+F)
        - 0.001491*sin(D+F)
        - 0.001475*sin(M+Mm+F)
        - 0.001410*sin(M+Mm-F)
        - 0.001344*sin(M-F)
        - 0.001335*sin(D-F)
        + 0.001107*sin(3.0*Mm+F)
        + 0.001021*sin(4.0*D-F)
        + 0.000833*sin(4.0*D-Mm+F)
        + 0.000777*sin(Mm-3.0*F)
        + 0.000671*sin(4.0*D-2.0*Mm+F)
        + 0.000607*sin(2.0*D-3.0*F)
        + 0.000596*sin(2.0*D+2.0*Mm-F)
        + 0.000491*sin(2.0*D-M+Mm-F)
        - 0.000451*sin(2.0*D-2.0*Mm+F)
        + 0.000439*sin(3.0*Mm-F)
        + 0.000422*sin(2.0*D+2.0*Mm+F)
        + 0.000421*sin(2.0*D-3.0*Mm-F)
        - 0.000366*sin(2.0*D+M-Mm+F)
        - 0.000351*sin(2.0*D+M+F)
        + 0.000331*sin(4.0*D+F)
        + 0.000315*sin(2.0*D-M+Mm+F)
        + 0.000302*sin(2.0*D-2.0*M-F)
        - 0.000283*sin(Mm+3.0*F)
        - 0.000229*sin(2.0*D+M+Mm-F)
        + 0.000223*sin(D+M-F)
        + 0.000223*sin(D+M+F)
        - 0.000220*sin(M-2.0*Mm-F)
        - 0.000220*sin(2.0*D+M-Mm-F)
        - 0.000185*sin(D+Mm+F)
        + 0.000181*sin(2.0*D-M-2.0*Mm-F)
        - 0.000177*sin(M+2.0*Mm+F)
        + 0.000176*sin(4.0*D-2.0*Mm-F)
        + 0.000166*sin(4.0*D-M-Mm-F)
        - 0.000164*sin(D+Mm-F)
        + 0.000132*sin(4.0*D+Mm-F)
        - 0.000119*sin(D-Mm-F)
        + 0.000115*sin(4.0*D-M-F)
        + 0.000107*sin(2.0*D-2.0*M+F)
        - 0.002235*sin(L)
        + 0.000382*sin(A3)
        + 0.000175*sin(A1-F)
        + 0.000175*sin(A1+F)
        + 0.000127*sin(L-Mm)
        - 0.000115*sin(L+Mm);

      R := 385000.56
        - 20905.355*cos(Mm)
        - 3699.111*cos(2.0*D-Mm)
        - 2955.968*cos(2.0*D)
        - 569.925*cos(2.0*Mm)
        + 48.888*cos(M)
        - 3.149*cos(2.0*F)
        + 246.158*cos(2.0*(D-Mm))
        - 152.138*cos(2.0*D-M-Mm)
        - 170.733*cos(2.0*D+Mm)
        - 204.586*cos(2.0*D-M)
        - 129.620*cos(M-Mm)
        + 108.743*cos(D)
        + 104.755*cos(M+Mm)
        + 10.321*cos(2.0*D-2.0*F)
        + 79.661*cos(Mm-2.0*F)
        - 34.782*cos(4.0*D-Mm)
        - 23.210*cos(3.0*Mm)
        - 21.636*cos(4.0*D-2.0*Mm)
        + 24.208*cos(2.0*D+M-Mm)
        + 30.824*cos(2.0*D+M)
        - 8.379*cos(D-Mm)
        - 16.675*cos(D+M)
        - 12.831*cos(2.0*D-M+Mm)
        - 10.445*cos(2.0*D+2.0*Mm)
        - 11.650*cos(4.0*D)
        + 14.403*cos(2.0*D-3.0*Mm)
        - 7.003*cos(M-2.0*Mm)
        + 10.056*cos(2.0*D-M-2.0*Mm)
        + 6.322*cos(D+Mm)
        - 9.884*cos(2.0*D-2.0*M)
        + 5.751*cos(M+2.0*Mm)
        - 4.950*cos(2.0*D-2.0*M-Mm)
        + 4.130*cos(2.0*D+Mm-2.0*F)
        - 3.958*cos(4.0*D-M-Mm)
        + 3.258*cos(3.0*D-Mm)
        + 2.616*cos(2.0*D+M+Mm)
        - 1.897*cos(4.0*D-M-2.0*Mm)
        - 2.117*cos(2.0*M-Mm)
        + 2.354*cos(2.0*D+2.0*M-Mm)
        - 1.423*cos(4.0*D+Mm)
        - 1.117*cos(4.0*Mm)
        - 1.571*cos(4.0*D-M)
        - 1.739*cos(D-2.0*Mm)
        - 4.421*cos(2.0*Mm-2.0*F)
        + 1.165*cos(2.0*M+Mm)
        + 8.752*cos(2.0*D-Mm-2.0*F);

      eo := 23.43929111 +
        (-0.0130125 + (-1.300416667E-2 +( -1.6388889E-7
         + 5.0361111E-7*T)*T)*T)*T;

      C1 := cos(eo*pi/180.0)*sin(lambda*pi/180.0)
        - sin(eo*pi/180.0)*tan(B*pi/180.0);
      C2 := cos(lambda*pi/180.0);
      alpha := arctan( C1, C2) - GST;
      C3 := sin(B*pi/180.0)*cos(eo*pi/180.0);
      C4 := cos(B*pi/180.0)*sin(eo*pi/180.0)*sin(lambda*pi/180.0);
      delt := arcsin(C3+C4);

   end MOONPOS;

end Gem.Ephemeris;
