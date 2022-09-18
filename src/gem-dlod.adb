with Text_IO;
with GEM.LTE.Primitives;

function GEM.dLOD (File_Name : in String) return Gem.LTE.Long_Periods_Amp_Phase is
   use GEM.LTE, GEM.LTE.Primitives;
   D : Data_Pairs := Make_Data(File_Name);
   Y : constant Long_Float := Year_Length;
   First, Last : Integer;
   Singular : Boolean;
   Forcing : Data_Pairs := D;
   Model : Data_Pairs := D;
   DBLT : Gem.LTE.Long_Periods := Gem.LTE.LP & (Y, Y/2.0, Y/3.0);
   DBLTAP : Gem.LTE.Long_Periods_Amp_Phase := Gem.LTE.LPAP &
         Gem.LTE.Long_Periods_Amp_Phase'((0.0,0.0), (0.0,0.0), (0.0,0.0));
   Level, K0, Trend : Long_Float;
   Last_Time : Long_Float;
begin
   --DBLT(4) :=  Gem.LTE.LP(9);
   First := D'First;
   Last := D'Last;
   Text_IO.Put_Line("records from " & First'Img & " ... " & Last'Img);
   Text_IO.Put_Line("factors from " & DBLT'First'Img & " ... " & DBLT'Last'Img);
   for I in Forcing'Range loop
      Forcing(I).Value := Forcing(I).Date;
      Last_Time := Forcing(I).Value;
   end loop;
   Text_IO.Put("updated forcing  ");
   Put(Last_Time);
   Text_IO.New_Line;
   for I in DBLT'Range loop -- to frequency
      DBLT(I) := GEM.LTE.Year_Length/DBLT(I);
   end loop;

   Trend := 0.0;
   Regression_Factors (Data_Records => D, -- Time series
                       --First => First,
                       --Last => Last,  -- Training Interval
                       Forcing => Forcing,  -- Value @ Time
                       NM => DBLT'Last, -- # modulations
                       DBLT => DBLT, --D.B.LT,
                       DALTAP => DBLTAP, --D.A.LTAP,
                       DALEVEL => LEVEL,
                       DAK0 => K0,
                       Secular_Trend => Trend,
                       Singular => Singular
                       );

   Text_IO.Put_Line("Singular? " & Singular'Img);
   for I in DBLT'Range loop
      Put(DBLT(I)); Text_IO.Put("   ");
      Put(DBLTAP(I).Amplitude); Text_IO.Put("   ");
      Put(DBLTAP(I).Phase); Text_IO.Put("   ");
      Text_IO.New_Line;
   end loop;

   Model := Gem.LTE.Primitives.LTE(Forcing => Forcing,
                Wave_Numbers => DBLT,
                Amp_Phase => DBLTAP,
                Offset => Level,
                K0 => K0,
                Trend => 0.0);
   --  for I in DBLT'Range loop -- to frequency
   --     DBLT(I) := GEM.LTE.Year_Length/DBLT(I);
   --  end loop;
   --  Model := Tide_Sum(Template => D,
   --                 Constituents => DBLTAP,
   --                 Periods => DBLT,
   --                 Ref_Time => 0.0,
   --                 Scaling => 1.0,
   --                 Cos_Phase => False);
   Put(CC(D,Model), "=CC " );
   Put(Year_Length, "=Yr", True);

--   for I in Model'Range loop
--         Text_IO.Put_Line(Model(I).Date'Img & " " & Model(I).Value'Img & " " & D(I).Value'Img);
--   end loop;

   return DBLTAP;

end;
