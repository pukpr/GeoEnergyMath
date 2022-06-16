with Ada.Command_Line;
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
with GEM.dLOD;

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
         LP     => GEM.LTE.LP,
         LTAP   => GEM.LTE.LTAP),
      B =>
        (NLP    => GEM.LTE.LP'Length,
         NLT    => GEM.LTE.LTM'Length,
         LPAP   => GEM.LTE.LPAP,
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
      AP : Gem.LTE.Long_Periods_Amp_Phase  :=  GEM.dLOD("../dlod3.dat");
   begin

      Text_IO.Put_Line(N'Img & " processors available");
      GEM.LTE.Primitives.Shared.Load(D); -- if available

      if GEM.Getenv("DLOD_REF", FALSE) then
         Text_IO.Put_Line("Referencing dLOD");
         for I in D.B.LPAP'Range loop
           GEM.LTE.LPRef(I).Amplitude := Ap(I).Amplitude;
           GEM.LTE.LPRef(I).Phase := Ap(I).Phase;
         end loop;
      else -- update
         Text_IO.Put_Line("Loading dLOD");
         for I in D.B.LPAP'Range loop
           GEM.LTE.LPRef(I).Amplitude := Ap(I).Amplitude;
           GEM.LTE.LPRef(I).Phase := Ap(I).Phase;
           D.B.LPAP(I).Amplitude := Ap(I).Amplitude;
           D.B.LPAP(I).Phase := Ap(I).Phase;
         end loop;
      end if;
      D.B.LT(1) := GEM.Getenv("LT1", D.B.LT(1));
      D.B.LT(2) := GEM.Getenv("LT2", D.B.LT(2));
      D.B.LT(3) := GEM.Getenv("LT3", D.B.LT(3));
      D.B.LT(4) := GEM.Getenv("LT4", D.B.LT(4));
      D.B.Offset := GEM.Getenv("OFFSET", D.B.Offset);
      D.B.bg := 0.0;
      D.B.mA := 0.0;
      D.B.shiftT := 0.0;
      D.B.ImpA := GEM.Getenv("IMPaVALUE", D.B.ImpA);
      D.B.ImpB := GEM.Getenv("IMPbVALUE", D.B.ImpB);
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

 end ENSO_Opt;
