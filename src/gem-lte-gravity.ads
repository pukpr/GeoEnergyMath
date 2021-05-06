package Gem.LTE.Gravity is


   Length : constant := 2048;

   type Model is
      record
         T, R, Delt, Alpha, Impulse, Force, DE, M, Avg : Long_Float;
      end record;
   type Model_Series is array (1..Length) of Model;

   -- D : array (1..Length) of Model;

   type Parms is
      record
         Time_Shift : Long_Float;
         ImpAmp, ImpPhase, ImpExp, ImpBase : Long_Float;
         Range1, Range2, Range3 : Long_Float;
         AbsSin1, AbsSin2, AbsSin3 : Long_Float;
         DCos1, DCos2, DCos3 : Long_Float;
         D2Cos1, D2Cos2, D2Cos3 : Long_Float;
         DSin1, DSin2, DSin3 : Long_Float;
         T16pt9Amp, T16pt9Phase : Long_Float;
         DR, DR2, R2, DR3, R3 : Long_Float;
         Shift, Offset : Long_Float;
         IC1, IC2, IC3 : Long_Float;
         Lag1, Lag2 : Long_Float;
         F, Fminus, Fplus : Long_Float;
         Level, K0 : Long_Float;
      end record;


   P0: Parms := (Time_Shift => 0.00001,
                 ImpAmp => 1.00512483456406,
                 ImpPhase => 0.367651152767032,
                 ImpExp => 221.053938152006,
                 ImpBase => -0.0000393674293892007,
                 Range1 => -16.52633854, -- *3.8136387153462,
                 Range2 => 26.28893212, -- *3.8136387153462,
                 Range3 => -14.98490087, -- *3.8136387153462,
                 AbsSin1 => 0.449521234, -- *(-0.56143373945034),
                 AbsSin2 => 7.252886725, -- *(-0.56143373945034),
                 AbsSin3 => 3.513177378, -- *(-0.56143373945034),
                 DCos1 => -0.1530402, -- *(-0.56143373945034),
                 DCos2 => -0.27393033, -- *(-0.56143373945034),
                 DCos3 => -9.034107661, -- *(-0.56143373945034),
                 D2Cos1 => -0.009033088, -- *(-0.56143373945034),
                 D2Cos2 => 2.656219564, -- *(-0.56143373945034),
                 D2Cos3 => 10.19190331, -- *(-0.56143373945034),
                 DSin1 => 1.257965348, -- *(-0.56143373945034),
                 DSin2 => -1.616009622, -- *(-0.56143373945034),
                 DSin3 => 0.379125065, -- *(-0.56143373945034),
                 T16pt9Amp => 0.037319398,
                 T16pt9Phase => -0.767148096,
                 DR => -0.456802409396286,
                 DR2 => 3.51379918883421,
                 R2 => 0.170539891372677,
                 DR3 => 18.6606512760497,
                 R3 => -10.6060312997259,
                 -- Offset => 1.77746059856432,  -- neg offset sign
                 Shift => -0.845876749066495+1.77746059856432,
                 Offset =>  -0.00005576278667772,
                 IC1 => 4.65997446594183,
                 IC2 => 0.840638432372657,
                 IC3 => -0.0395887334934722,
                 Lag1 => -0.925353713233002,
                 Lag2 => 0.196054855977508,
                 F => 0.532489391961045,
                 Fminus => 2.98471490041099,
                 Fplus => 0.172307592422057,
                 Level => -1.442939628,
                 K0 => 1.225659486
                );

   LTM : constant Modulations := (
    (6.350525071,	5.114879841,	-0.827999352),
   (56.67139318, 	2.517216186,	+1.543293722),
   (170.6009574, 	-3.877257728,	-0.272696364),
   (6000.0, 	0.01,	1.0),
   (16000.0, 	0.01,	1.0)

                                 );
   procedure Annual_Impulse_Response (D : in out Model_Series;
                                      P : in Parms);

end Gem.LTE.Gravity;
