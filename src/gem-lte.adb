package body GEM.LTE is

begin

   for I in Doodson_Args'Range loop
      Doodson_Args(I).Period :=  1.0/
        (Long_Float(Doodson_Args(I).s) / Tropical +
         Long_Float(Doodson_Args(I).h) / Year_Length +
         Long_Float(Doodson_Args(I).p) / p +
           Long_Float(Doodson_Args(I).N) / N );
      LPF(I) := Doodson_Args(I).Period;
      LP(I) := (0.01, 1.0);
   end loop;

end GEM.LTE;
