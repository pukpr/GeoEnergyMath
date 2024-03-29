with Ada.Command_Line;
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with Text_IO;
with GEM.dLOD;
with GNAT.OS_Lib;
with Ada.Calendar;

procedure QBO_Opt is
   N :  Positive := Gem.Getenv("NUMBER_OF_PROCESSORS",  System.Task_Info.Number_Of_Processors);
   --N :  Positive := System.Task_Info.Number_Of_Processors;
   Ch : Character;
   Avail : Boolean;
   T : Ada.Calendar.Time;
   Cycle : Duration := Duration(Gem.Getenv("TIMEOUT",  LONG_FLOAT(Duration'Last)/2.0));
   use type Ada.Calendar.Time;

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
   --declare
   --   AP : Gem.LTE.Long_Periods_Amp_Phase  :=  GEM.dLOD("../dlod3.dat");
   --begin

      Text_IO.Put_Line(N'Img & " processors available, timeout=" & Cycle'Img);
      GEM.LTE.Primitives.Shared.Load(D); -- if available
      --  if GEM.Command_Line_Option_Exists("r") or not GEM.Getenv("DLOD_REF", FALSE) then
      --     Text_IO.Put_Line("Loading dLOD");
      --     for I in D.B.LPAP'Range loop
      --       GEM.LTE.LPRef(I).Amplitude := Ap(I).Amplitude;
      --       GEM.LTE.LPRef(I).Phase := Ap(I).Phase;
      --       D.B.LPAP(I).Amplitude := Ap(I).Amplitude;
      --       D.B.LPAP(I).Phase := Ap(I).Phase;
      --     end loop;
      --  else -- update
      --     Text_IO.Put_Line("Referencing dLOD");
      --     for I in D.B.LPAP'Range loop
      --       GEM.LTE.LPRef(I).Amplitude := Ap(I).Amplitude;
      --       GEM.LTE.LPRef(I).Phase := Ap(I).Phase;
      --     end loop;
      --  end if;
      GEM.LTE.Primitives.Shared.Put(D);

   --end;

   T := Ada.Calendar.Clock;

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
         if Ch = 'q' or Ch = 's' then
            GEM.LTE.Primitives.Stop;
         elsif Ch = 'x' then
            Text_IO.Put_Line("Exiting, no save");
            Gnat.Os_Lib.Os_Exit(0);
         end if;
      end if;
      if Ch = 'q' then
         exit when GEM.LTE.Primitives.Halted;
      end if;
      if Ada.Calendar.Clock > T + Cycle then
         GEM.LTE.Primitives.Stop;
         T := Ada.Calendar.Clock;
         exit when GEM.LTE.Primitives.Halted; -- new
      end if;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");


   delay 5.0;

 end QBO_Opt;
