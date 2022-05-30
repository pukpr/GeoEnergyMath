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
     (NLP => GEM.LTE.LP'Length,
      NLT => GEM.LTE.LTM'Length,
      A =>
        (k0     => 0.0,
         level  => 0.0,
         NLP    => GEM.LTE.LP'Length,
         NLT    => GEM.LTE.LTM'Length,
         LPF    => GEM.LTE.LPF,
         LTAP   => GEM.LTE.LTAP),
      B =>
        (NLP    => GEM.LTE.LP'Length,
         NLT    => GEM.LTE.LTM'Length,
         LP     => GEM.LTE.LP,
         LT     => GEM.LTE.LTM,
         Offset => 0.0,
         bg     => 0.0,
         ImpA   => 9.0,
         ImpB   => 0.0, ---9.0,
         mA     => 0.0,
         mP     => 0.0,
         shiftT => 0.00000,
         init   => 0.0063)
        );

begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.LTE.Primitives.Shared.Load(D); -- if available
   --D.B.LP(8).Amplitude := 0.02;
   --D.B.LP(9).Amplitude := 0.001;
   --D.B.LT(1) := 0.1;
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
