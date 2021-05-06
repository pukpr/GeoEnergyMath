with Ada.Command_Line;
with System.Task_Info;
with GEM.LTE.Gravity.Solution;
with GEM.LTE.Gravity.Shared;
with GEM.LTE.Primitives;
with Text_IO;

procedure ENSO_Gravity is
   N :  Positive := System.Task_Info.Number_Of_Processors;
   Ch : Character;
   Avail : Boolean;

   D : GEM.LTE.Gravity.Shared.Param_S :=
        (NLT => GEM.LTE.Gravity.LTM'Length,
         LT  => GEM.LTE.Gravity.LTM,
         P   => GEM.LTE.Gravity.P0
        );

begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.LTE.Gravity.Shared.Load(D); -- if available
   GEM.LTE.Gravity.Shared.Put(D);
   GEM.LTE.Gravity.Solution.Start(D.NLT,N);
   for I in 1..Integer'Last loop
      -- delay 1.0;
      -- The call to Status is blocking
      declare
         S : String := GEM.LTE.Gravity.Solution.Status;
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

end ENSO_Gravity;
