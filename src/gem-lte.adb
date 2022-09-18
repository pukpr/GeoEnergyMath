with Ada.Text_IO;
with GNAT.OS_Lib;

package body GEM.LTE is

   Year_in_Days : constant := 365.2422484;  -- 365.241237718675000;
   Year_Correction : Long_Float := GEM.Getenv("YEAR", 0.0);
   Year_Dynamic_Correction  : Long_Float := 0.0;

   function Year_Length return Long_Float is
   begin
      return Year_in_Days + Year_Correction + Year_Dynamic_Correction;
   end Year_Length;


   function Doodson (I : in Integer;
                     D : in Doodson_List) return Long_Float is
   begin
      return 1.0/
        (Long_Float(D(I).s) / Tropical +
         Long_Float(D(I).h) / Year_Length +
         Long_Float(D(I).p) / p +
           Long_Float(D(I).N) / N );
   end Doodson;

   procedure Year_Adjustment (Value : in Long_Float;
                              List : in out Periods) is
   begin
      if Value /= 0.0 then
         --Year_Dynamic_Correction := Value;
         Year_Correction := Value;
         for I in List'Range loop
            List(I) := Doodson(I, Doodson_Args);
         end loop;
      end if;
   end Year_Adjustment;

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
