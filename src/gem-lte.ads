package GEM.LTE is
   -- Laplace's Tidal Equations model for equatorial waves -- Chap 11, Chap 12

   -- Top level holds the main Tidal Constituents and LTE modulation parameters

   pragma Elaborate_Body;

   -- The year value is close to the Tropical year of 365.24219 days
   -- which accumulates to ~1/2 day error over 100 years
   -- Year : constant Long_Float := 365.2412384; -- 365.246462;
   Draconic : constant Long_Float := 27.21222082;
   Tropical : constant Long_Float := 27.32163237; -- 27.32166155;
   Anomalistic : constant Long_Float := 27.55454988;

   N : constant := 1.0/(1.0/Draconic - 1.0/Tropical);
   p : constant := 1.0/(1.0/Tropical - 1.0/Anomalistic);
   --Year_Length : constant := 365.2412384;  -- 365.241237718675000;

   function Year_Length return Long_Float;


   type Doodson_Argument is
      record
         s, h, p, N : Integer;
         Period : Long_Float; --
      end record;

   type Doodson_List is array (Positive range <>) of Doodson_Argument;

   Doodson_Args : Doodson_List :=
     ( (3, 0,-1, 2, 0.0),
       (1,-1, 0, 0, 0.0),
 --      (0, 0, 1,-1, 0.0),
 --      (0, 0, 1, 1, 0.0),
       (1, 0, 1, 0, 0.0),
       (4, -2, 0, 1, 0.0),
       (4, 0,-2, 1, 0.0),
       (2, 0, 0, 2, 0.0),
       (2, 0, 0, 0, 0.0),
       (0, 0, 0, 2, 0.0),
       (3, 0,-1, 1, 0.0),
       (2, 0,-2, 0, 0.0),
 --      (0, 0, 1, 0, 0.0),
       (0, 0, 0, 1, 0.0),
       (1, 0,-1, 0, 0.0),
       (1, 0,-1,-1, 0.0),
       (1, 0,-1, 1, 0.0),
       (0, 0, 2, 0, 0.0),
       (4, 0,-2, 0, 0.0),
       (4, -2, 0, 0, 0.0),
       (3,-2, 1, 0, 0.0),
       (3, 0,-1, 0, 0.0),
       (1, 0, 0, 0, 0.0),
       (1, 0, 0, 1, 0.0),
       (3, 0,-3, 0, 0.0),
       (2,-2, 0, 0, 0.0),
       (1,-2, 1, 0, 0.0),
       (3, -2, 1, 1, 0.0),
       (2, 0, 0, 1, 0.0),
       (1, 0, 1, 1, 0.0)

       --,(3, -2,-1, 0, 0.0),
       --(5, -2, -1, 0, 0.0),
       --(1,  2,  1, -1, 0.0),
       --(1,  2,  -1, 0, 0.0)
      );

  QBO_Args : Doodson_List :=
     ( (0, 1, 0, 0, 0.0),
       (0, 2, 0, 0, 0.0),
       (1, 0, 0, 0, 0.0),
       (2, 0, 0, 0, 0.0),
       (1, 0, 0, 1, 0.0),
       (2, 0,0, 1, 0.0),
       (2, 0, 0, 2, 0.0)

 );

   type Amp_Phase is
      record
         Amplitude, Phase : Long_Float;
      end record;
   subtype Period is Long_Float;

   type Amp_Phases is array (Positive range <>) of Amp_Phase;
   type Periods is array (Positive range <>) of Period;

   -- Tidal Factor types
   subtype Long_Periods_Amp_Phase is Amp_Phases;
   subtype Long_Periods is Periods;

   -- LTE modulation types
   subtype Modulations_Amp_Phase is Amp_Phases;
   subtype Modulations is Periods;

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

   LPAP : Long_Periods_Amp_Phase (Doodson_Args'Range);
   LP : Long_Periods (Doodson_Args'Range);

   LPRef : Long_Periods_Amp_Phase (Doodson_Args'Range);

   QBOAP : Long_Periods_Amp_Phase (QBO_Args'Range);
   QBO : Long_Periods (QBO_Args'Range);


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
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0),
     (0.0, 0.0));


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

   LTAP_NINO34 : constant Modulations_Amp_Phase := LTAP;

   LTM_IOD : constant Modulations := (
   10.47794403,
28.70477254,
46.96926187,
82.58877522,
166.5979348,
597.5106739,
51.95085745,
41.46872024,
120.7676296,
4.536710575,
4235.964024 );

   procedure Year_Adjustment (Value : in Long_Float;
                              List : in out Periods);


end GEM.LTE;

