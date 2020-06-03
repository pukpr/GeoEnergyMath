
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
(365.246462,  0.018078892, 	1.575074188),
(31.81209136, -0.000354105,	-0.097518929),
(27.66676713, 0.020074647, 	3.666651532),
(13.77727494, 0.020116683, 	0.022561926),
(13.60611041, -0.028703042,	-1.318646215),
(13.66083077, 0.189949387, 	-2.835798758),
(27.21222082, 0.00125881,  	0.412311989),
(27.55454988, -0.046842425,	-1.377131938),
(27.32166155, 0.000746357, 	0.466946899),
(13.63341568, 0.061011159, 	-1.990250035),
(7.095810615, -0.047886633,	-3.405720189),
(6.859402884, -0.02248555, 	-17.7200619),
(9.556887401, 0.014018594, 	-7.453951926),
(9.132950781, -0.006125104,	-0.03308375),
(9.124566355, 0.008314442, 	-0.000163231),
(14.76532679, -0.017749988,	-2.474938943),
(27.09267692, 0.013887908, 	7.163035366),
(182.623231*8.85,	  0.0001, 1.0),
(365.246462*8.85,  0.0001, 1.0),
(182.623231*18.6,	  0.0001, 1.0),
(365.246462*18.6,  0.0001, 1.0)
                                 );

   LT : constant Modulations := (
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


   First : constant Positive := LP'First;

   --
   -- NAO experiment
   --
    LPN : constant Long_Periods := (
 (LP(First).Period,   0.00915747709,  -2.83945640438),
 (LP(First+1).Period,   0.00012250798,   0.00308393439),
 (LP(First+2).Period,   -0.01233237927,  -1.14529191154),
 (LP(First+3).Period,   0.00124951939,   5.14995418188),
 (LP(First+4).Period,   0.00128992615,   0.24438321562),
 (LP(First+5).Period,  -0.00032507431,  -4.63472387220),
 (LP(First+6).Period,   0.05745789323,  -8.29792010034),
 (LP(First+7).Period,   0.00000158520,   0.27546630446),
 (LP(First+8).Period,  -0.00193220712,  -0.97630333500),
 (LP(First+9).Period,   0.01056641292,   1.27034778410),
 (LP(First+10).Period,   0.00244277479,  -2.94084843658),
 (LP(First+11).Period,  -0.00099212326,  -1.29879573606),
 (LP(First+12).Period,  -0.00119971471, -11.01936096507),
 (LP(First+13).Period,   0.00236374917,  -5.76963403033),
 (LP(First+14).Period,  -0.00012410934,  -0.04432388197),
 (LP(First+15).Period,   0.00002383758,  -0.00147260273),
 (LP(First+16).Period,  -0.05636002768,  -4.72692144932),
 (LP(First+17).Period,   0.01295288631,   1.57534615162)
                                    );

   LTN : constant Modulations := (
  (   8.37399498,   1.33178242,  -4.97431640),
  (  38.21430538,   6.55202324,  -6.50350933),
  (  16.74561480,   6.82660631,  -6.40596227)
                              );

end GEM.LTE;
