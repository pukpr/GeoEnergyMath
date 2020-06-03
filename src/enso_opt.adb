
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
procedure ENSO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.LP'Length,
         NLT    => GEM.LTE.LT'Length,
         LP     => GEM.LTE.LP,
         LT     => GEM.LTE.LT,
         Offset => -0.002770852,
         bg     => 0.037589404,
         ImpA   => 13.57577452, -- 0.0,
         ImpB   => 7.056004563, -- 1.22,
         ImpC   => 20.25515663, -- 1.399,
         ImpD   => 1.659027049, -- 1.2149,
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
begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.LTE.Primitives.Shared.Load(D); -- if available
   GEM.LTE.Primitives.Shared.Put(D);
   GEM.LTE.Primitives.Solution.Start(D.NLP,D.NLT,N);
   for I in 1..Integer'Last loop
      -- delay 1.0;
      -- The call to Status is blocking
      declare
         S : String := GEM.LTE.Primitives.Solution.Status;
      begin
         if I mod 100 = 0 then
            Text_IO.Put_Line(S & " #" & I'Img);
         end if;
      end;
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 5.0;

end ENSO_Opt;
