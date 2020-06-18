with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with GEM.LTE.Lib;
with Text_IO;
procedure QBO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;

   D : GEM.LTE.Primitives.Shared.Param_S :=
        (NLP    => GEM.LTE.Lib.LPQ'Length,
         NLT    => GEM.LTE.Lib.LTQ'Length,
         LP     => GEM.LTE.Lib.LPQ,
         LT     => GEM.LTE.Lib.LTQ,
         Offset => -0.002770852,
         bg     => 0.037589404,
         ImpA   => 13.57577452,  -- This is the positive impulse per year
         ImpB   => 7.056004563,  --  A & B
         ImpC   => -20.25515663, -- This is the negative impulse per year
         ImpD   => -1.659027049, --  C & D
         mA     => 0.080599014,
         mP     => -0.008405309,
         mD     => -0.0021993,
         shiftT => 0.000001,
         fB     => -0.022004419,
         fC     => 1.469665629,
         fA     => 0.600532903,
         k0     => 2.169,
         level  => 0.5,
         init   => 0.01,
         order2 => 0.000,
         order3 => 0.000 );  -- 1.0
begin
   Text_IO.Put_Line(N'Img & " processors available");
   -- GEM.Setenv("CLIMATE_INDEX", "qbo_30hPa.txt");
   GEM.Setenv("IMPC", "3");  -- these are 6-months earlier than 9 & 10
   GEM.Setenv("IMPD", "4");
   GEM.LTE.Primitives.Shared.Load(D);
   GEM.LTE.Primitives.Shared.Put(D);
   GEM.LTE.Primitives.Solution.Start(D.NLP, D.NLT, N);
   for I in 1..Integer'Last loop
      -- The call to Status is blocking
      declare
         S : String := GEM.LTE.Primitives.Solution.Status;
      begin
         if I mod 100 = 0 then
            Text_IO.Put_Line(S & " #" & I'Img);
         end if;
      end;
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 5.0;

end QBO_Opt;
