
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
procedure Tidal_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.SP'Length,
         NLT    => GEM.LTE.LT0'Length,
         LP     => GEM.LTE.SP,
         LT     => GEM.LTE.LT0,
         Offset => 0.001,
         bg     => 0.9,
         ImpA   => 0.0, -- 0.0,
         ImpB   => 0.0, -- 1.22,
         ImpC   => 0.0, -- 1.399,
         ImpD   => 0.0, -- 1.2149,
         mA     => 0.49,
         mP     => 0.0,
         mD     => 0.0,
         shiftT => 0.000001,
         fB     => 0.0, -- 10.761,
         fC     => 0.8, -- 11.865,
         fA     => 0.0, -- 7.55161
         k0     => 0.9,
         level  => 0.0,
         init   => 0.01);
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

end Tidal_Opt;
