with Ada.Command_Line;
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;

procedure ENSO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;
   Ch : Character;
   Avail : Boolean;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.LP'Length,
         NLT    => GEM.LTE.LTM'Length,
         LP     => GEM.LTE.LP,
         LT     => GEM.LTE.LTM,
         Offset => -0.002770852,
         bg     => 0.037589404,
         ImpA   => 13.57577452,
         ImpB   => 7.056004563,
         ImpC   => 20.25515663,
         ImpD   => 1.659027049,
         mA     => 0.080599014,
         mP     => -0.008405309,
         mD     => -0.0021993,
         shiftT => 0.000001,
         fB     => -0.022004419,
         fC     => 1.469665629,
         fA     => 0.600532903,
         k0     => 0.169,
         level  => 0.0,
         init   => 0.0063,
         order2 => 0.000001,
         order3 => 0.000001
         -- LH => GEM.LTE.Harm
        );

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
      Text_IO.Get_Immediate(Ch, Avail);
      if Avail then
         if Ch = 'q' then
            GEM.LTE.Primitives.Stop;
         end if;
      end if;
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 5.0;

end ENSO_Opt;
