
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
  (182.623231,	0.018080271, 	-2.077486706),
(365.246462,  	0.018078892, 	1.575074188),
(31.81209136, 	-0.000354105,	-0.097518929),
(27.66676713, 	0.020074647, 	3.666651532),
(13.77727494, 	0.020116683, 	0.022561926),
(13.60611041, 	-0.028703042,	-1.318646215),
(13.66083077, 	0.189949387, 	-2.835798758),
(27.21222082, 	0.00125881,  	0.412311989),
(27.55454988, 	-0.046842425,	-1.377131938),
(27.32166155, 	0.000746357, 	0.466946899),
(13.63341568, 	0.061011159, 	-1.990250035),
(7.095810615, 	-0.047886633,	-3.405720189),
(6.859402884, 	-0.02248555, 	-17.7200619),
(9.556887401, 	0.014018594, 	-7.453951926),
(9.132950781, 	-0.006125104,	-0.03308375),
(9.124566355, 	0.008314442, 	-0.000163231),
(14.76532679, 	-0.017749988,	-2.474938943),
(27.09267692, 	0.013887908, 	7.163035366)
                                 );

   LT : constant Modulations := (
   (2.763168894,	4.782646953,	-1.531748603),
   (42.38703079, 	4.206881588,	-5.741140747),
   (18.68216048, 	3.581731279,	-4.376886727)
                                );

   --
   -- Defaults for an index such as QBO
   -- The modulation is very weak
   --
   LPQ : constant Long_Periods := (
  (182.623231,	0.018080271, 	-2.077486706),
  (365.246462,  	0.018078892, 	1.575074188),
  (13.60611041, 	-0.000003042,	-1.318646215),
--  (13.66083077, 	0.000087, 	-2.835798758),
  (27.21222082, 	0.012588100,  	0.412311989)
--  (27.32166155, 	0.000046357, 	0.466946899)
                                  );

   LTQ :  constant Modulations :=
     ( 1 => (0.1,	1.0,	0.0) );

   --
   -- Conventional Tidal -- no modulation
   --
   LT0 :  Modulations (1..0);


   --
   -- NAO
   --
    LPN : constant Long_Periods := (
(182.62323100,   0.03296847,  -4.94930984),
 (365.24646200,   0.01327353,   0.20501780),
  (31.81209136,  -0.00041340,  -0.38565011),
  (27.66676713,   0.00045340,   5.06050095),
  (13.77727494,   0.00191165,   2.09515727),
  (13.60611041,  -0.00489421,  -1.63755198),
  (13.66083077,   0.01038169,  -3.23157316),
  (27.21222082,   0.00011583,   0.00000006),
  (27.55454988,  -0.00548761,  -0.00000000),
  (27.32166155,   0.01207559,   3.82456894),
  (13.63341568,   0.00099947,  -4.58387415),
   (7.09581062,  -0.00089667,  -6.53681250),
   (6.85940288,  -0.00242939, -14.16653513),
   (9.55688740,   0.00157442,  -8.43712749),
   (9.13295078,  -0.01360592,  -1.09026283),
   (9.12456636,   0.00000039,  -4.10242765),
  (14.76532679,  -0.01088327,  -4.99878948),
  (27.09267692,   0.00163049,   6.84466396)
);

end GEM.LTE;
