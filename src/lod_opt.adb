
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
procedure LOD_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.LP'Length,
         NLT    => GEM.LTE.LT0'Length,
         LP     => GEM.LTE.LP,
         LT     => GEM.LTE.LT0,
         Offset => 0.00014,
         bg     => 1.29,
         ImpA   => 0.0,
         ImpB   => 0.0,
         ImpC   => 0.0,
         ImpD   => 0.0,
         mA     => 0.0344,
         mP     => -0.00585,
         mD     => -0.00805,
         shiftT => 0.0000006,
         fB     => 0.107,
         fC     => 1.326,
         fA     => 0.077,
         k0     => 1.44,
         level  => 0.0,
         init   => 0.0286);
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

end LOD_Opt;
