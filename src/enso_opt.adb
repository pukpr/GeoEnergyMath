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
         --ImpC   => 20.25515663,
         --ImpD   => 1.659027049,
         mA     => 0.0,
         mP     => 0.0,
         --mD     => -0.0021993,
         shiftT => 0.00000,
         --fB     => -0.022004419,
         --fC     => 1.469665629,
         --fA     => 0.600532903,
         --k0     => 0.169,
         --level  => 0.0,
         init   => 0.0063)
         --order2 => 0.000001,
         --order3 => 0.000001)
         -- LH => GEM.LTE.Harm
        );

begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.LTE.Primitives.Shared.Load(D); -- if available
   --D.B.LT := GEM.LTE.LTM_NINO34;
   --D.B.LT(8) := 80.28896;
   D.B.LT(9) := 252.0;

--   D.B.ImpA := 20.0;
--D.B.ImpB := -17.0;
--   D.B.bg := 0.00000;
--   D.B.Offset := 0.000001;



--   D.B.ImpB := -1.0;
--   D.B.ImpC := 0.0;
--   D.B.ImpD := 0.0;
   --D.B.mA := 0.00001;
   --D.B.mP := 0.0000;
--   D.B.mD := 0.00000000;
--   D.B.fB := 0.0;
--   D.B.fA := 0.0;
--   D.B.order2 := 0.0;
--   D.B.order3 := 0.0;
     --   D.B.Init := 0.0001;
     --   D.B.Offset := 0.0001;
   --
   --for I in D.A.LPF'Range loop
   --   D.A.LPF(I) := GEM.LTE.Doodson_Args(I).Period;
   --   D.B.LP(I).Amplitude := 0.01;
   --   D.B.LP(I).Phase := 1.0;
   --end loop;
   --  D.B.LT(D.B.LT'First) := 65.83486399;
   --  D.B.LT(D.B.LT'First+1) := 180.3574051;
   --  D.B.LT(D.B.LT'First+2) := 1046.765696;
   --  D.B.LT(D.B.LT'First+3) := 295.1165761;
   --  D.B.LT(D.B.LT'First+4) := 518.920579;
   --  D.B.LT(D.B.LT'First+5) := 758.8053957;
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
