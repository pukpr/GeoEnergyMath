
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
procedure QBO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.LPQ'Length,
         NLT    => GEM.LTE.LTQ'Length,
         LP     => GEM.LTE.LPQ,
         LT     => GEM.LTE.LTQ,
         Offset => -0.002770852,
         bg     => 0.037589404,
         ImpA   => 13.57577452, -- 0.0,
         ImpB   => 7.056004563, -- 1.22,
         ImpC   => -20.25515663, -- 1.399,
         ImpD   => -1.659027049, -- 1.2149,
         mA     => 0.080599014,
         mP     => -0.008405309,
         mD     => -0.0021993,
         fB     => -0.022004419, -- 10.761,
         fC     => 1.469665629, -- 11.865,
         fA     => 0.600532903, -- 7.55161
         shiftT => 0.000001 );
begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.Setenv("CLIMATE_INDEX", "qbo_30hPa.txt");
   GEM.Setenv("IMPC", "3");
   GEM.Setenv("IMPD", "4");
   GEM.LTE.Primitives.Shared.Put(D);
   GEM.LTE.Primitives.Solution.Start(N);
   for I in 1..Integer'Last loop
      -- delay 1.0;
      -- The call to Status is blocking
      Text_IO.Put_Line(GEM.LTE.Primitives.Solution.Status & " #" & I'Img);
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 3.0;

end QBO_Opt;
