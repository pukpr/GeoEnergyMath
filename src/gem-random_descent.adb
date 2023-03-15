
with Ada.Numerics.Float_Random;
with Ada.Numerics.Long_Elementary_Functions;
with Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.Discrete_Random;

package body GEM.Random_Descent is
   package FR renames Ada.Numerics.Float_Random;
   package LEF renames Ada.Numerics.Long_Elementary_Functions;

   subtype Set_Index is Positive range 1..Set_Range;
   package DR is new Ada.Numerics.Discrete_Random(Set_Index);
   Flip_Value : constant Long_Float := GEM.Getenv("FLIP", 0.0);
   Fix_Harm : constant BOOLEAN := GEM.Getenv("FIX", FALSE);
   Log_Harm : constant BOOLEAN := GEM.Getenv("LOGH", FALSE);
   Relative : constant BOOLEAN := GEM.Getenv("REL", TRUE);

   subtype Harmonic_Index is Positive range 2..Harmonic_Range;
   package HR is new Ada.Numerics.Discrete_Random(Harmonic_Index);

   D : DR.Generator; -- Discrete for selecting from a set of params
   G : FR.Generator; -- Floating point for values
   H : HR.Generator; -- Floating point for values


   procedure Markov (Set : in out LF_Array;
                     Ref : out LF_Array;
                     Spread : in Long_Float;
                     Cal : in LF_Array) is
      Ran : Long_Float := Long_Float(FR.Random(G));
      Sign : Long_Float := Long_Float(FR.Random(G) - 0.5); -- Is step + or - ?
      Adjust : Long_Float;
      I : Set_Index := DR.Random(D);
   begin
      Ref := Set; -- keep in case of error?
      --for I in Set'Range loop -- a real gradient descent would do all at once
      if Set(I) = 0.0 then
         if Flip_Value < 0.0 then
            Set(I) := Spread * Long_Float'Copy_Sign(LEF.Log(Ran), Sign);
         else
            Markov(Set, Ref, Spread, Cal);  -- recurse, pick another
         end if;
      elsif Fixed(Set(I)) then
         Markov(Set, Ref, Spread, Cal);  -- recurse, pick another
      else
         if Relative then
            Adjust := 1.0 + Spread * Long_Float'Copy_Sign(LEF.Log(Ran), Sign);
            Set(I) := Set(I) * Adjust;
            if Ran < Flip_Value then
               Set(I) := -Set(I);
            end if;
         else
            Adjust := Spread * Cal(I) * Long_Float'Copy_Sign(Ran, Sign);
            Set(I) := Cal(I) + Adjust;
         end if;
      end if;
      --end loop;
   end Markov;

   procedure Markov (Value : in out Long_Float;
                     Ref : out Long_Float;
                     Spread : in Long_Float;
                     Cal : in Long_Float) is
      Ran : Long_Float := Long_Float(FR.Random(G));
      Sign : Long_Float := Long_Float(FR.Random(G) - 0.5); -- Is step + or - ?
      Adjust : Long_Float;
   begin
      Ref := Value;
      if Relative then
         Adjust := 1.0 + Spread * Long_Float'Copy_Sign(LEF.Log(Ran), Sign);
         Value := Value * Adjust;
         if Ran < Flip_Value then
            Value := -Value;
         end if;
      else
         Adjust := Spread * Cal * Long_Float'Copy_Sign(Ran, Sign);
         Value := Cal + Adjust;
      end if;
   end Markov;

   procedure Dump(Set : in LF_Array) is
   begin
      for I in Set'Range loop
         Ada.Long_Float_Text_IO.Put(Set(I), Fore=>4, Aft=>11, Exp=>0);
         Text_IO.Put_Line(",");
      end loop;
   end Dump;

   procedure Random_Harmonic(Index : in out Positive;
                             Ref : out Positive) is
      Ran : Long_Float := Long_Float(FR.Random(G));
      FV : Long_Float := abs Flip_Value;
   begin
      Ref := Index;
      if Fix_Harm then
         null;
      elsif Ran < FV then
         if Log_Harm then
            Index := 2 - INTEGER(Long_Float(Harmonic_Range)
                                 * LEF.Log(Long_Float(FR.Random(G))));
         else
            Index := HR.Random(H);
         end if;
      end if;
   end Random_Harmonic;


   procedure Random_Harmonic(Index : in out NS;
                             Ref : out NS) is
      Test : Positive;
      Reference : NS := Index;
   begin
      for I in Index'Range loop
         Random_Harmonic (Index(I), Test);
         for J in Reference'Range loop
            -- Make sure no duplicates in Harmonics otherwise singularity in sol'n
            if Index(I) = Reference(J) then
               Index(I) := Test;  -- keep the old value
               exit;
            end if;
         end loop;
      end loop;
      Ref := Reference;
   end Random_Harmonic;

   procedure Reset is
   begin
      DR.Reset(D);
      FR.Reset(G);
      HR.Reset(H);
   end;
begin
   if GEM.Getenv("RESET", FALSE) then
      Reset;
   end if;
end GEM.Random_Descent;
