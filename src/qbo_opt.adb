with System.Task_Info;
with GEM.LTE.Primitives.Solution;
with GEM.LTE.Primitives.Shared;
with GEM.LTE.Lib;
with Text_IO;
procedure QBO_Opt is
   N :  Positive := System.Task_Info.Number_Of_Processors;
   Ch : Character;
   Avail : Boolean;

   D0 : GEM.LTE.Primitives.Shared.Param_S :=
          (NLP    => GEM.LTE.Lib.LPQ'Length,
           NLT    => GEM.LTE.Lib.LTQ'Length,
           LP     => GEM.LTE.Lib.LPQ,
           LT     => GEM.LTE.Lib.LTQ,
           Offset => -1.22684949919,
           bg     => -0.26441696199,
           ImpA   => 7.66247924185/100.0,  -- This is the positive impulse per year
           ImpB   => 8.49994380699/100.0,  --  A & B
           ImpC   => -20.96168643883/100.0, -- This is the negative impulse per year
           ImpD   => 0.17939053101/100.0, --  C & D
           mA     => 0.09260091281,
           mP     => -0.00395224056,
           mD     => -0.00127336342,
           shiftT => -0.02929520736,
           fB     => 0.44472381369,
           fC     => 0.28870587768,
           fA     => 2.24535989268,
           k0     => 0.05046021975,
           level  => 0.0001,
           init   => -10.56157375803,
           order2 => 0.00029866729,
           order3 => 0.00000765837 );  -- 1.0


   D : GEM.LTE.Primitives.Shared.Param_S := -- for daily ERA5 fit
        (NLP    => GEM.LTE.Lib.LPQ1'Length,
         NLT    => GEM.LTE.Lib.LTQ1'Length,
         LP     => GEM.LTE.Lib.LPQ1,
         LT     => GEM.LTE.Lib.LTQ1,
         Offset => -0.95651603190,
         bg     => 0.12351789235,
         ImpA   => 7.46470432968,  -- This is the positive impulse per year
         ImpB   => 7.18466213550,  --  A & B
         ImpC   => -21.57715429423, -- This is the negative impulse per year
         ImpD   => 0.09880862485, --  C & D
         mA     => 0.08911175163,
         mP     => -0.00388527436,
         mD     => -0.00129985028,
         shiftT => -0.02966607495,
         fB     => 0.42697103505,
         fC     => 0.24893650487,
         fA     => 2.22334099814,
         k0     => 0.01537371720,
         level  => 0.00092988260,
         init   => -10.0, -- 3581.90766387663,
         order2 => 0.00064474107,
         order3 => 0.00000189297 );  -- 1.0
begin
   Text_IO.Put_Line(N'Img & " processors available");
   -- GEM.Setenv("CLIMATE_INDEX", "qbo_30hPa.txt");
   GEM.Setenv("IMPC", "3");  -- these are 6-months earlier than 9 & 10
   GEM.Setenv("IMPD", "4");
   GEM.LTE.Primitives.Shared.Load(D);
--   for I in D.LP'Range loop
--      D.LP(I).Period := D.LP(I).Period + 0.0000001;
--   end loop;
   --D.LT(1).Amplitude := 0.01;
   --D.LT(2).Amplitude := 0.01;
   --D.K0 := 0.00001;
   -- D.Init := -0.1;
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
      Text_IO.Get_Immediate(Ch, Avail);
      if Avail and then Ch = 'q' then
          GEM.LTE.Primitives.Stop;
      end if;
      exit when GEM.LTE.Primitives.Halted;
   end loop;
   Text_IO.Put_Line("Main exiting, flushing other tasks");
   delay 5.0;

end QBO_Opt;


