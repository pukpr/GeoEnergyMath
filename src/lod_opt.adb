
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with GEM.LTE.Lib;
with Text_IO;
procedure LOD_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.Lib.LPLOD'Length,
         NLT    => GEM.LTE.LT0'Length,
         LP     => GEM.LTE.Lib.LPLOD,
         LT     => GEM.LTE.LT0,
         Offset => -0.00000007634,
         bg     => 0.00000007547,
         ImpA   => 0.0,
         ImpB   => 0.0,
         ImpC   => 0.0,
         ImpD   => 0.0,
         mA     => 0.06089251365,
         mP     => -0.49760174462,
         mD     => 0.03232811484,
         shiftT => 0.00037080416,
         fB     => 0.48071797435,
         fC     => 0.48449848886,
         fA     => -0.28415079854,
         k0     => 8782.72455399216,
         level  => 0.26187396614,
         init   => -0.00000005124,
         order2 => 0.000,
         order3 => 0.000);
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
         if I mod 10 = 0 then
            Text_IO.Put_Line(S & " #" & I'Img);
         end if;
      end;
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 5.0;

end LOD_Opt;
