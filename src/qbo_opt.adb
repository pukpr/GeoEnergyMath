with Ada.Command_Line;
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
with GEM.dLOD;

procedure QBO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;
   Ch : Character;
   Avail : Boolean;

   D : GEM.LTE.Primitives.Shared.Param_S :=
     (NLP => GEM.LTE.QBO'Length,
      NLT => GEM.LTE.LTM'Length,
      A =>
        (k0     => 0.0,
         level  => 0.0,
         NLP    => GEM.LTE.QBO'Length,
         NLT    => GEM.LTE.LTM'Length,
         LP     => GEM.LTE.QBO,
         LTAP   => GEM.LTE.LTAP),
      B =>
        (NLP    => GEM.LTE.QBO'Length,
         NLT    => GEM.LTE.LTM'Length,
         LPAP   => GEM.LTE.QBOAP,
         LT     => GEM.LTE.LTM,
         Offset => 0.0,
         bg     => 0.0,
         ImpA   => 1.0, -- 9.0
         ImpB   => 0.0, ---9.0,
         mA     => 0.0,
         mP     => 0.0,
         shiftT => 0.00000,
         init   => 0.0063)
        );

   -- Ref : GEM.LTE.Long_Periods_Amp_Phase := GEM.LTE.LPAP;
begin
   declare
      -- AP : Gem.LTE.Long_Periods_Amp_Phase  :=  GEM.dLOD("../dlod3.dat");
   begin

      Text_IO.Put_Line(N'Img & " processors available");
      GEM.LTE.Primitives.Shared.Load(D); -- if available

      D.B.LT(1) := GEM.Getenv("LT1", D.B.LT(1));
      D.B.Offset := GEM.Getenv("OFFSET", D.B.Offset);
      D.B.bg :=    GEM.Getenv("BG", D.B.bg);
      D.B.shiftT := GEM.Getenv("SHIFTT", D.B.shiftT);
      D.B.ImpA := GEM.Getenv("IMPAVALUE", D.B.ImpA);
      D.B.ImpB := GEM.Getenv("IMPBVALUE", D.B.ImpB);
      D.B.mA   := GEM.Getenv("MA",        D.B.mA);
      D.B.mP   := GEM.Getenv("MP",        D.B.mP);
      D.B.Init := GEM.Getenv("INIT",      D.B.Init);

      GEM.LTE.Primitives.Shared.Put(D);

   end;

   GEM.LTE.Primitives.Solution.Start(D.NLP,D.NLT,N);

   for I in 1..Integer'Last loop
      -- delay 1.0;
      -- The call to Status is blocking
      declare
         S : String := GEM.LTE.Primitives.Solution.Status;
      begin
         if I mod GEM.LTE.Primitives.Solution.Check_Every_N_Loops = 0 then
         --if I mod 100 = 0 then
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

 end QBO_Opt;
