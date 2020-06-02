with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
procedure NAO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   LT : constant GEM.LTE.Modulations := (
   (2.763168894,	4.782646953,	-1.531748603),
   (42.38703079, 	4.206881588,	-5.741140747),
   (18.68216048, 	3.581731279,	-4.376886727),
   (144.0, 0.001,	1.0),
   (320.0, 0.001,	1.0)
                                );

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.LP'Length,
         NLT    => LT'Length,
         LP     => GEM.LTE.LP,
         LT     => LT,
         Offset => -0.002770852,
         bg     => 0.037589404,
         ImpA   => 13.57577452, -- 0.0,
         ImpB   => -7.056004563, -- 1.22,
         ImpC   => 20.25515663, -- 1.399,
         ImpD   => -16.59027049, -- 1.2149,
         mA     => 0.080599014,
         mP     => -0.008405309,
         mD     => -0.0021993,
         shiftT => 0.000001,
         fB     => -0.022004419, -- 10.761,
         fC     => 1.469665629, -- 11.865,
         fA     => 0.600532903, -- 7.55161
         k0     => 0.169,
         level  => 0.0,
         init   => 0.0063);
        --  (NLP    => GEM.LTE.LP'Length,
        --   NLT    => GEM.LTE.LT'Length,
        --   LP     => GEM.LTE.LP,
        --   LT     => GEM.LTE.LT,
        --   Offset => -0.01245515355,
        --   bg     => 0.66703017403,
        --   ImpA   => 0.89412846982,
        --   ImpB   => 1.11205504650,
        --   ImpC   => 47.39935490397,
        --   ImpD   => 0.59752729609,
        --   mA     => 0.08854726554,
        --   mP     => -0.00614911270,
        --   mD     => -0.00843314079,
        --   shiftT => 0.00034708490,
        --   fB     => -0.00010642898,
        --   fC     => 0.96136838504,
        --   fA     => 0.04672216434,
        --   k0     => 0.169,
        --   level  => 0.0,
        --   init   => 0.0
        --  );

  begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.Setenv("CLIMATE_INDEX", "nao.txt");
   GEM.Setenv("IMPA", "9");  -- these are 6-months earlier than 9 & 10
   GEM.Setenv("IMPB", "6");
   GEM.Setenv("IMPC", "3");  -- these are 6-months earlier than 9 & 10
   GEM.Setenv("IMPD", "0");
   GEM.LTE.Primitives.Shared.Load(D); -- if available
   GEM.LTE.Primitives.Shared.Put(D);
   GEM.LTE.Primitives.Solution.Start(N);
   for I in 1..Integer'Last loop
      -- delay 1.0;
      -- The call to Status is blocking
      Text_IO.Put_Line(GEM.LTE.Primitives.Solution.Status & " #" & I'Img);
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 5.0;

end NAO_Opt;

--   -0.01245515355,
--     0.66703017403,
--     0.89412846982,
--     1.11205504650,
--    47.39935490397,
--     0.59752729609,
--     0.08854726554,
--    -0.00614911270,
--    -0.00843314079,
--     0.00034708490,
--    -0.00010642898,
--     0.96136838504,
--     0.04672216434,
--   182.62323100000,   0.00915747709,  -2.83945640438,
--   365.24646200000,   0.00012250798,   0.00308393439,
--    31.81209136000,  -0.01233237927,  -1.14529191154,
--    27.66676713000,   0.00124951939,   5.14995418188,
--    13.77727494000,   0.00128992615,   0.24438321562,
--    13.60611041000,  -0.00032507431,  -4.63472387220,
--    13.66083077000,   0.05745789323,  -8.29792010034,
--    27.21222082000,   0.00000158520,   0.27546630446,
--    27.55454988000,  -0.00193220712,  -0.97630333500,
--    27.32166155000,   0.01056641292,   1.27034778410,
--    13.63341568000,   0.00244277479,  -2.94084843658,
--     7.09581061500,  -0.00099212326,  -1.29879573606,
--     6.85940288400,  -0.00119971471, -11.01936096507,
--     9.55688740100,   0.00236374917,  -5.76963403033,
--     9.13295078100,  -0.00012410934,  -0.04432388197,
--     9.12456635500,   0.00002383758,  -0.00147260273,
--    14.76532679000,  -0.05636002768,  -4.72692144932,
--    27.09267692000,   0.01295288631,   1.57534615162,
--  CC=   0.88554022   0.00000000   3 1
