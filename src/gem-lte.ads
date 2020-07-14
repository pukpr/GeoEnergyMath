package GEM.LTE is
   -- Laplace's Tidal Equations model for equatorial waves -- Chap 11, Chap 12

   -- Top level holds the main Tidal Constituents and LTE modulation parameters

   -- The year value is close to the Tropical year of 365.24219 days
   -- which accumulates to ~1/2 day error over 100 years
   Year : constant Long_Float := 365.246462;
   Draconic : constant Long_Float := 27.21222082;
   Tropical : constant Long_Float := 27.32166155;
   Anomalistic : constant Long_Float := 27.55454988;

   type Long_Period is
      record
         Period, Amplitude, Phase : Long_Float;
      end record;

   type Long_Periods is array (Positive range <>) of Long_Period;

   type Modulation is
      record
         Wavenumber, Amplitude, Phase : Long_Float;
      end record;

   type Modulations is array (Integer range <>) of Modulation;

   -- Default values that produce a fit to NINO34 of cc = ~0.83
   -- The period values can be derived from the constants above
   -- but are left as values for easy identification.
   -- Also, the phases are not reduced to within (-PI .. PI)  boundaries

   SemiAnnual : constant := 182.623231; -- days
   Annual : constant := 365.246462;
   ThirdAnnual : constant := 121.748821;
   Msm : constant := 31.81209136;
   Mm_1 : constant := 27.66676713;
   Mm_f : constant := 13.77727494;  -- _f for fortnightly
   Nodical_f : constant := 13.60611041;
   Mf_f : constant := 13.66083077;
   Nodical : constant := 27.21222082;
   Mm : constant := 27.55454988;
   Mf : constant := 27.32166155;
   Mf_prime : constant := 13.63341568;
   Msqm : constant := 7.095810615;
   Mqm : constant := 6.859402884;
   Mstm : constant := 9.556887401;
   Mfm : constant := 9.132950781;
   Mfm_1 : constant := 9.124566355;
   Msf_f : constant := 14.76532679;
   Msm_1 : constant := 27.09267692;
   Msf : constant := 29.53065358;
   Perigee_half : constant := SemiAnnual*8.85;
   Perigee : constant := Annual*8.85;
   Nodal_half : constant := SemiAnnual*18.6;
   Nodal : constant := Annual*18.6;
   Mf_Mfp : constant := 6.82355473;
   Mf_Mf : constant := 6.83041539;
   Mf_Nodical : constant := 6.81670784;

   LP0 : constant Long_Periods := (
                                  (Mf_f, 0.189949387, 	-2.835798758), -- tropical fortnightly
                                  (Mm, -0.046842425,	-1.377131938), -- anomalistic monthly
                                  (Mf_prime, 0.061011159, 	-1.990250035), -- tropical/draconic nonlinear
                                  (Msqm, -0.047886633,	-3.405720189) -- Msqm
                                 );

   LP : constant Long_Periods := (
                                  (SemiAnnual,	0.018080271, 	-2.077486706), -- semi-annual
                                  (Annual,  0.018078892, 	1.575074188),  -- annual
                                  (Msm, -0.000354105,	-0.097518929), -- lunar evection
                                  (Mm_1, 0.020074647, 	3.666651532),  --
                                  (Mm_f, 0.020116683, 	0.022561926),  -- anomalistic fortnightly
                                  (Nodical_f, -0.028703042,	-1.318646215), -- draconic/nodal fortnightly
                                  (Mf_f, 0.189949387, 	-2.835798758), -- tropical fortnightly
                                  (Nodical, 0.00125881,  	0.412311989),  -- draconic/nodal monthly
                                  (Mm, -0.046842425,	-1.377131938), -- anomalistic monthly
                                  (Mf, 0.000746357, 	0.466946899),  -- tropical monthly
                                  (Mf_prime, 0.061011159, 	-1.990250035), -- tropical/draconic nonlinear
                                  (Msqm, -0.047886633,	-3.405720189), -- Msqm
                                  (Mqm, -0.02248555, 	-17.7200619),  -- Mqm
                                  (Mstm, 0.014018594, 	-7.453951926), -- Mstm
                                  (Mfm, -0.006125104,	-0.03308375),  -- Mfm
                                  (Mfm_1, 0.008314442, 	-0.000163231),
                                  (Msf_f, -0.017749988,	-2.474938943), -- synodic fortnightly
                                  (Msm_1, 0.013887908, 	7.163035366),
                                  (Msf, 0.00000001, 0.9),
                                  (Perigee_half,	  0.0001, 1.0), -- Mf_Mfp Perigee_half
                                  (Perigee,  0.0001, 1.0),
                                  (Nodal_half,	  0.0001, 1.0), -- Mf_Nodical Nodal_half
                                  (Nodal,  0.0001, 1.0),
                                  (Mf_Mfp,  0.0001, 1.0),
                                  (Mf_Mf,  0.0001, 1.0)
                                 );


   LTM : constant Modulations := (
    (2.763168894,	4.782646953,	-1.531748603),
   (42.38703079, 	4.206881588,	-5.741140747),
   (18.68216048, 	3.581731279,	-4.376886727),
   (6000.0, 	0.01,	1.0),
   (16000.0, 	0.01,	1.0)
                               );

   --
   -- Conventional Tidal -- short periods, no modulation
   --

   M2 : constant := 12.4206012/24.0;
   S2 : constant := 12.0000001/24.0; -- not exactly 12 for reason, see Is_Fixed
   N2 : constant := 12.65834751/24.0;
   K1 : constant := 23.93447213/24.0;
   O1 : constant := 25.81933871/24.0;

   SP : constant Long_Periods :=
     (-- SP=Short periods for conventional tidal analysis
      (M2,  0.01, 0.9),   -- Principal lunar semidiurnal M2
      (S2,  0.01, 0.9),   -- Principal solar semidiurnal S2
      (N2, 0.01, 0.9),    -- Larger lunar elliptic semidiurnal N2
      (K1, 0.01, 0.9),    -- Lunar diurnal K1
      (O1, 0.01, 0.9)     -- Lunar diurnal O1
     );

   LT0 :  Modulations (1..0);

end GEM.LTE;

