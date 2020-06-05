
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

   LP : constant Long_Periods := (
(182.623231,	0.018080271, 	-2.077486706), -- semi-annual
(365.246462,  0.018078892, 	1.575074188),  -- annual
(31.81209136, -0.000354105,	-0.097518929), -- lunar evection
(27.66676713, 0.020074647, 	3.666651532),  --
(13.77727494, 0.020116683, 	0.022561926),  -- anomalistic fortnightly
(13.60611041, -0.028703042,	-1.318646215), -- draconic/nodal fortnightly
(13.66083077, 0.189949387, 	-2.835798758), -- tropical fortnightly
(27.21222082, 0.00125881,  	0.412311989),  -- draconic/nodal monthly
(27.55454988, -0.046842425,	-1.377131938), -- anomalistic monthly
(27.32166155, 0.000746357, 	0.466946899),  -- tropical monthly
(13.63341568, 0.061011159, 	-1.990250035), -- tropical/draconic nonlinear
(7.095810615, -0.047886633,	-3.405720189), -- Msqm
(6.859402884, -0.02248555, 	-17.7200619),  -- Mqm
(9.556887401, 0.014018594, 	-7.453951926), -- Mstm
(9.132950781, -0.006125104,	-0.03308375),  -- Mfm
(9.124566355, 0.008314442, 	-0.000163231),
(14.76532679, -0.017749988,	-2.474938943), -- synodic fortnightly
(27.09267692, 0.013887908, 	7.163035366),
(182.623231*8.85,	  0.0001, 1.0),
(365.246462*8.85,  0.0001, 1.0),
(182.623231*18.6,	  0.0001, 1.0),
(365.246462*18.6,  0.0001, 1.0)
                                 );

   LTM : constant Modulations := (
   (2.763168894,	4.782646953,	-1.531748603),
   (42.38703079, 	4.206881588,	-5.741140747),
   (18.68216048, 	3.581731279,	-4.376886727),
   (6000.0, 	0.01,	1.0)
                               );

   --
   -- Defaults for an index such as QBO
   -- The modulation is very weak
   --
   LPQ : constant Long_Periods := (
  (182.623231,	0.018080271, 	-2.077486706),
  (365.246462,  	0.018078892, 	1.575074188),
  (13.60611041, 	-0.000003042,	-1.318646215),
  (27.21222082, 	0.012588100,  	0.412311989)
                                  );

   LTQ :  constant Modulations :=
     ( 1 => (0.1, 0.9,	0.9) );

   --
   -- Conventional Tidal -- short periods, no modulation
   --

   SP : constant Long_Periods :=
     (-- SP=Short periods for conventional tidal analysis
        (12.4206012/24.0,  0.01, 0.9), -- Principal lunar semidiurnal M2
      (12.0000001/24.0,  0.01, 0.9),   -- Principal solar semidiurnal S2  -- not exactly 12 for reason, see Is_Fixed
      (12.65834751/24.0, 0.01, 0.9),   -- Larger lunar elliptic semidiurnal N2
      (23.93447213/24.0, 0.01, 0.9),   -- Lunar diurnal K1
      (25.81933871/24.0, 0.01, 0.9)    -- Lunar diurnal O1
     );

   LT0 :  Modulations (1..0);


end GEM.LTE;
