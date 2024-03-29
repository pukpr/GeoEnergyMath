with Ada.Command_Line;
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
with GEM.dLOD;

procedure All_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;
   Ch : Character;
   Avail : Boolean;
   use GEM.LTE;
   Y : constant Long_Float := Gem.LTE.Year_Length;

   DBLT : Gem.LTE.Long_Periods := Gem.LTE.LP & (Y, Y/2.0, Y/3.0);
   DBLTAP : Gem.LTE.Long_Periods_Amp_Phase := Gem.LTE.LPAP &
         Gem.LTE.Long_Periods_Amp_Phase'((0.0,0.0), (0.0,0.0), (0.0,0.0));

   D : GEM.LTE.Primitives.Shared.Param_S :=
     (NLP => DBLT'Length,
      NLT => GEM.LTE.LTM'Length,
      A =>
        (k0     => 0.0,
         level  => 0.0,
         NLP    => DBLT'Length,
         NLT    => GEM.LTE.LTM'Length,
         LP     => DBLT,
         LTAP   => GEM.LTE.LTAP),
      B =>
        (NLP    => DBLT'Length,
         NLT    => GEM.LTE.LTM'Length,
         LPAP   => DBLTAP,
         LT     => GEM.LTE.LTM,
         Offset => 0.0,
         bg     => 0.0,
         ImpA   => 1.0, -- 9.0
         ImpB   => 0.0, ---9.0,
         impC  =>  0.0,
         delA  =>  0.0,
         delB  =>  0.0,
         asym  =>  0.0,
         ann1  =>  0.0,
         ann2  =>  0.0,
         sem1  =>  0.0,
         sem2  =>  0.0,
         year  =>  0.0,
         ir    =>  0.0,
         mA     => 0.0,
         mP     => 0.0,
         shiftT => 0.00000,
         init   => 0.0063),
        C => (others => 0)
     );


   -- Ref : GEM.LTE.Long_Periods_Amp_Phase := GEM.LTE.LPAP;
begin
   declare
      AP : Gem.LTE.Long_Periods_Amp_Phase  :=  GEM.dLOD("../dlod3.dat");
   begin

      Text_IO.Put_Line(N'Img & " processors available");
      GEM.LTE.Primitives.Shared.Load(D); -- if available

      D.B.LT(1) := GEM.Getenv("LT1", D.B.LT(1));
      D.B.LT(2) := GEM.Getenv("LT2", D.B.LT(2));
      D.B.LT(3) := GEM.Getenv("LT3", D.B.LT(3));
      D.B.LT(4) := GEM.Getenv("LT4", D.B.LT(4));
      D.B.LT(5) := GEM.Getenv("LT5", D.B.LT(5));
      D.B.LT(6) := GEM.Getenv("LT6", D.B.LT(6));
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

 end All_Opt;
