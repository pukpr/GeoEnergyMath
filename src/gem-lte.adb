package body GEM.LTE is

   function Doodson (I : in Integer;
                     D : in Doodson_List) return Long_Float is
   begin
      return 1.0/
        (Long_Float(D(I).s) / Tropical +
         Long_Float(D(I).h) / Year_Length +
         Long_Float(D(I).p) / p +
           Long_Float(D(I).N) / N );
   end Doodson;

begin

   for I in Doodson_Args'Range loop
      Doodson_Args(I).Period := Doodson(I, Doodson_Args);
      LP(I) := Doodson_Args(I).Period;
      LPAP(I) := (0.01, 1.0);
   end loop;

   for I in QBO_Args'Range loop
      QBO_Args(I).Period := Doodson(I, QBO_Args);
      QBO(I) := QBO_Args(I).Period;
      QBOAP(I) := (0.01, 1.0);
   end loop;

end GEM.LTE;
