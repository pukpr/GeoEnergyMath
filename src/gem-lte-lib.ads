package GEM.LTE.Lib is

   --
   -- Defaults for an index such as QBO
   -- The modulation is very weak
   --
   LPQ : constant Long_Periods := (
                                   (SemiAnnual,	0.018080271, 	-2.077486706),
                                   (Annual,  	0.018078892, 	1.575074188),
                                   (Nodical_f, 	-0.000003042,	-1.318646215),
                                   (Nodical, 	0.012588100,  	0.412311989)
                                  );

   LTQ :  constant Modulations :=
     ( 1 => (0.00182064774,  28.61799849553,  -0.01179420997),
       2 => (0.01, 10.0, 0.1)
      );

  -- Alternate modulations for QBO, set for daily ERA5 modeling

   LPQ1 : constant Long_Periods := (
                                   (Annual,  5.38914453296,  12.27460339790),    -- annual
                                   (Nodical, 32.86439106365,   0.03622545527),    -- draconic/nodal monthly
                                   (Mm, 4.91575066920,   1.82718437704),                   -- anomalistic monthly
                                   (Mf, 7.22125189178,   0.11104598982),        -- tropical monthly 10 instead of 0
                                   (Msf, 12.05903196333,   0.12548331357),         -- synodic monthly
                                   (SemiAnnual,  47.23327206089,   4.09952648378), -- annual
                                   (Nodical_f, 4.38893056301,  -1.06849741951),   -- draconic/nodal monthly
                                   (Mm_f, 23.63453760757,   1.28503095988),        -- anomalistic fort
                                   (Mf_f, 14.67244146176,  22.57891440266),        -- tropical fort
                                   (Msf_f, 5.31629018224,   4.30446264621),       -- synodic fort
                                   (Mf_prime, 2.78528192281,   1.59318255115),     --
                                   (Msm, 4.14153718440,   0.12492043961), -- lunar evection
                                   (Mm_1, 5.09699816737,   4.64300489955),  --
                                   (Msqm, -6.01783866671,  -1.66953361526), -- Msqm
                                   (Mqm, -3.25723131127,  -2.03772037551),  -- Mqm
                                   (Mstm, 6.40431363824,  -0.99977709636), -- Mstm
                                   (Mfm, 0.81827228838,   1.18351723575)  -- Mfm
                                  );

  LTQ1 :  constant Modulations :=
     ( 1 => (0.01966683511,  47.44813677719,  -1.74167525086),
       2 => (0.00993702255,  79.00974357738,  -5.16070449792)
      );

  LPQ2 : constant Long_Periods := (
                                   (SemiAnnual,	0.027120929,	2.219388416),
                                   (Annual,  	-0.032300026,	0.683206442),
                                   (ThirdAnnual, 	0.000331951,	-2.12715963)
                                  );
  LTQ2a :  constant Modulations :=
     ( 1 => (0.001452932,	31.8677021,	-6.023750083),
       2 => (11.55616601,	1.743978697,	74.31224775)
      );

  LTQ2 :  constant Modulations :=
     ( 1 => (0.001477322,	31.98339244,	-6.026207311),
       2 => (-11.79958459,	0.829999795,	-1.482957814),
       3 => (-0.00118548,	73.13096397,	28.19403845),
       4 => (13.25722155,	4.749369169,	-9.229680387),
       5 => (13.24844574,	4.999396502,	-1.645886744),
       6 => (100.0,	0.000,	0.779555181),
       7 => (200.0,	0.000,	0.779555181),
       8 => (300.0,	0.000,	0.779555181)
      );


   -- Gravity model
   LPG : constant Long_Periods := (
                                   (Annual,  5.54427521924,  12.35194529914),    -- annual
                                   (Nodical, 12.17763556185,  -0.22066080786),    -- draconic/nodal monthly
                                   (Mm, 2.87390248408,   0.35851201969),                   -- anomalistic monthly
                                   (Mf, 4.92855742959, -16.74164563990),        -- tropical monthly 10 instead of 0
                                   (Msf, 6.86614512980,   0.84389777184),         -- synodic monthly
                                   (SemiAnnual,  46.31029148751,   3.76660141114), -- annual
                                   (Nodical_f, 1.55697134893,   5.49053652018),   -- draconic/nodal monthly
                                   (Mm_f, 17.74289667390,   1.87540789664),        -- anomalistic fort
                                   (Mf_f, 6.04927802462,  37.98367681257),        -- tropical fort
                                   (Msf_f, 3.15095931186,   1.95652733500),       -- synodic fort
                                   (Mf_prime, 3.21220955529,  -3.07447357086),     --
                                   (Msm, -0.000354105,	-0.097518929), -- lunar evection
                                   (Mm_1, 0.020074647, 	3.666651532),  --
                                   (Msqm, -0.047886633,	-3.405720189), -- Msqm
                                   (Mqm, -0.02248555, 	-17.7200619),  -- Mqm
                                   (Mstm, 0.014018594, 	-7.453951926), -- Mstm
                                   (Mfm, -0.006125104,	-0.03308375),  -- Mfm
                                   (Msm_1, 0.013887908, 	7.163035366)
                                  );

   -- Length-of-Day model
   LPLOD : constant Long_Periods := (
                                     (SemiAnnual,	0.03306673307,  -4.42404121940), -- semi-annual
                                     (Annual,  0.02564149359,  -2.07767532533),    -- annual
                                     (Msm, -0.07372495100,   3.73829776660),       -- lunar evection
                                     (Mm_1, 0.08215366481,  -8.43758734288),       --
                                     (Mm_f, -0.19176989356,   0.67477824951),      -- anomalistic fortnightly
                                     (Nodical_f, -0.05048948879,  -3.52178002451), -- draconic/nodal fortnightly
                                     (Mf_f, 1.23174598617,  -2.44697674050),       -- tropical fortnightly
                                     (Nodical, 0.03145346075,  -0.97682514345),    -- draconic/nodal monthly
                                     (Mm, 0.94821924902,  -1.34959519405),         -- anomalistic monthly
                                     (Mf, 0.05541937874,  -1.31043032866),         -- tropical monthly
                                     (Mf_prime, 0.21395115311,   0.82297369254),   -- tropical/draconic nonlinear
                                     (Msqm, -0.01824006752,  -1.12422153364),      -- Msqm
                                     (Mqm, -0.06014753048,  -1.90656942351),       -- Mqm
                                     (Mstm, 0.00945530822,  -7.48152894521),       -- Mstm
                                     (Mfm, -0.17499436241,  -0.31939028303),       -- Mfm
                                     (Mfm_1, -0.00044538891,  -1.49288592141),
                                     (Msf_f, -0.09615621986,   2.98376867267),     -- synodic fortnightly
                                     (Msm_1, 0.43071830671,   4.96354301328),
                                     (Msf, 0.00264160736,   1.96584394926),
                                     (Perigee_half,	  0.10622977316,  -0.01203902517),
                                     (Perigee,  0.00637273325,  -1.18876217715),
                                     (Nodal_half,	  0.05669232627,   1.79187935574),
                                     (Nodal,  -0.15028223979,  -1.75184181549)
                                    );

   LTGM : constant Modulations := (
 1 => (  1.0,   2.38946919615,   6.20845799035)
                                    );

 -- Sunspot model

   Sun : constant Long_Periods :=
     (
      (13.475,  9.8, 0.9),
      (26.698,  14.3, 1.0),
      (26.914,  21.6, 0.8),
      (27.554,  9.9, 1.3),
      (13.849,  12.0, 0.9),
      (365.0*24.0,  5.0, 1.0),
      (14.089,  7.8, 1.1),
      (27.334,  9.5, 0.9),
      (27.194,  14.2, 1.0),
      (26.959,  9.7, 1.4),
      (13.864,  9.7, 0.8),
      (365.0*22.0, 5.0, 1.0)
     );

   LSun : constant Modulations := (
    (9.75,	 2.9,	-0.73),
   (76.8, 	4.16,	-3.8),
   (-0.03, 6.43,	-5.8),
   (6000.0, 	1.01,	1.0),
   (9000.0, 	1.01,	1.0)
                               );

end GEM.LTE.Lib;