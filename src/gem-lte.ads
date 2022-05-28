package GEM.LTE is
   -- Laplace's Tidal Equations model for equatorial waves -- Chap 11, Chap 12

   -- Top level holds the main Tidal Constituents and LTE modulation parameters

   pragma Elaborate_Body;

   -- The year value is close to the Tropical year of 365.24219 days
   -- which accumulates to ~1/2 day error over 100 years
   Year : constant Long_Float := 365.2412377; -- 365.246462;
   Draconic : constant Long_Float := 27.21222082;
   Tropical : constant Long_Float := 27.32163232; -- 27.32166155;
   Anomalistic : constant Long_Float := 27.55454988;

   N : constant := 1.0/(1.0/Draconic - 1.0/Tropical);
   p : constant := 1.0/(1.0/Tropical - 1.0/Anomalistic);
   Year_Length : constant := 365.241237718675000;

   type Doodson_Argument is
      record
         s, h, p, N : Integer;
         Period : Long_Float; --
      end record;

   Doodson_Args : array (Positive range <>) of Doodson_Argument :=
     ( (3, 0,-1, 2, 0.0),
       (1,-1, 0, 0, 0.0),
       (0, 0, 1,-1, 0.0),
       (0, 0, 1, 1, 0.0),
       (1, 0, 1, 0, 0.0),
       (4, 0, 0, 1, 0.0),
       (4, 0,-2, 1, 0.0),
       (2, 0, 0, 2, 0.0),
       (2, 0, 0, 0, 0.0),
       (0, 0, 0, 2, 0.0),
       (3, 0,-1, 1, 0.0),
       (2, 0,-2, 0, 0.0),
       (0, 0, 1, 0, 0.0),
       (0, 0, 0, 1, 0.0),
       (1, 0,-1, 0, 0.0),
       (1, 0,-1,-1, 0.0),
       (1, 0,-1, 1, 0.0),
       (0, 0, 2, 0, 0.0),
       (4, 0,-2, 0, 0.0),
       (4, 0, 0, 0, 0.0),
       (3,-2, 1, 0, 0.0),
       (3, 0,-1, 0, 0.0),
       (1, 0, 0, 0, 0.0),
       (1, 0, 0, 1, 0.0),
       (3, 0,-3, 0, 0.0),
       (2,-2, 0, 0, 0.0),
       (1,-2, 1, 0, 0.0),
       (3, 0, 1, 1, 0.0),
       (2, 0, 0, 1, 0.0),
       (1, 0, 1, 1, 0.0) );



   type Long_Period is
      record
         Amplitude, Phase : Long_Float;
      end record;
   subtype Period is Long_Float;

   type Long_Periods is array (Positive range <>) of Long_Period;
   type Long_Periods_Frequency is array (Positive range <>) of Period;

   type Harmonics is array (Positive range <>) of Long_Float;

   type Modulation is
      record
         Amplitude, Phase : Long_Float;
      end record;
   subtype Wavenumber is Long_Float;

   type Modulations is array (Integer range <>) of Wavenumber;
   type Modulations_Amp_Phase is array (Integer range <>) of Modulation;

   -- Default values that produce a fit to NINO34 of cc = ~0.83
   -- The period values can be derived from the constants above
   -- but are left as values for easy identification.
   -- Also, the phases are not reduced to within (-PI .. PI)  boundaries

   --  SemiAnnual : constant := 182.623231; -- days
   --  Annual : constant := 365.246462;
   --  ThirdAnnual : constant := 121.748821;
   --  Msm : constant := 31.81209136;
   --  Mm_1 : constant := 27.66676713;
   --  Mm_f : constant := 13.77727494;  -- _f for fortnightly
   --  Nodical_f : constant := 13.60611041;
   --  Mf_f : constant := 13.66083077;
   --  Nodical : constant := 27.21222082;
   --  Mm : constant := 27.55454988;
   --  Mf : constant := 27.32166155;
   --  Mf_prime : constant := 13.63341568;
   --  Msqm : constant := 7.095810615; -- aka Msq
   --  Mqm : constant := 6.859402884;  -- Mq
   --  Mstm : constant := 9.556887401;
   --  Mfm : constant := 9.132950781;  -- aka Mt
   --  Mfm_1 : constant := 9.124566355;
   --  Msf_f : constant := 14.76532679;
   --  Msm_1 : constant := 27.09267692;
   --  Msf : constant := 29.53065358;
   --  Perigee_half : constant := SemiAnnual*8.85;
   --  Perigee : constant := Annual*8.85;
   --  Nodal_half : constant := SemiAnnual*18.6;
   --  Nodal : constant := Annual*18.6;
   --  Mf_Mfp : constant := 6.82355473;
   --  Mf_Mf : constant := 6.83041539;
   --  Mf_Nodical : constant := 6.81670784;
   --  Msp : constant := 5.643; -- not used
   --  M357 : constant := 1.0/(1.0/Nodal+1.0/Perigee_half);
   --  MTDD : constant := 1.0/(1.0/Mf+2.0/Nodical);
   --

   LP : Long_Periods (Doodson_Args'Range);

   LPF : Long_Periods_Frequency (Doodson_Args'Range);


   LTM : constant Modulations := (
    2.763168894,
   42.38703079,
   18.68216048,
   100.0,
   10.0,
   200.0,
   600.0,
   70.0,
   150.0,
   400.0,
   0.5
                                 );

   LTAP : constant Modulations_Amp_Phase := (
    (4.782646953,	-1.531748603),
   ( 4.206881588,	-5.741140747),
   ( 3.581731279,	-4.376886727),
   (0.01,	1.0),
   (0.01,	1.0),
   (0.01,	1.0),
   (0.01,	1.0),
   (0.01,	1.0),
   (0.01,	1.0),
   (0.01,	1.0),
   (0.01,	1.0)
                                 );


   LT0 :  Modulations (1..0);


   LTM_NINO34 : constant Modulations := (
   5.418249691,
1.388909654,
20.01744803,
3.621585351,
10.19436977,
34.49566978,
154.5224978,
80.35700362,
328.3065452,
648.4298787,
5161.28896 );

   LTAP_NINO34 : constant Modulations_Amp_Phase := (
(0.153249734	,	1.740430381),
(0.096147614	,	1.377232535),
(0.200301197	,	-0.435775052),
(0.099510785	,	-2.027516506),
(0.111454193	,	0.109971307),
(-0.18224337   ,	0.562560365),
(-0.407984091	,	0.929897973),
(0.132155984	,	4.712143396),
(0.058143978	,	3.2642847),
(-0.234278036	,	0.054185875),
(0.29980264	,	3.641119323)
                                   );

end GEM.LTE;

