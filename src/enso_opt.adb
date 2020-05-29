
with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with Text_IO;
procedure ENSO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;
begin
   Text_IO.Put_Line(N'Img & " processors available");
   GEM.LTE.Primitives.Solution.Start(N);
   for I in 1..Integer'Last loop
      -- delay 1.0;
      -- The call to Status is blocking
      Text_IO.Put_Line(GEM.LTE.Primitives.Solution.Status & " #" & I'Img);
      exit when GEM.LTE.Primitives.Halted;
   end loop;

end ENSO_Opt;
