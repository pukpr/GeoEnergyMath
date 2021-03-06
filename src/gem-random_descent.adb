
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

   D : DR.Generator; -- Discrete for selecting from a set of params
   G : FR.Generator; -- Floating point for values

   procedure Markov (Set : in out LF_Array;
                     Ref : out LF_Array;
                     Spread : in Long_Float) is
      Ran : Long_Float := Long_Float(FR.Random(G));
      Sign : Long_Float := Long_Float(FR.Random(G) - 0.5); -- Is step + or - ?
      Adjust : Long_Float;
      I : Set_Index := DR.Random(D);
   begin
      Ref := Set; -- keep in case of error?
      --for I in Set'Range loop -- a real gradient descent would do all at once
      if Set(I) = 0.0 or Fixed(Set(I)) then
         Markov(Set, Ref, Spread);  -- recurse, pick another
      else
         Adjust := 1.0 + Spread * Long_Float'Copy_Sign(LEF.Log(Ran), Sign);
         Set(I) := Set(I) * Adjust;
         if Ran < Flip_Value then
            Set(I) := -Set(I);
         end if;
      end if;
      --end loop;
   end Markov;

   procedure Markov (Value : in out Long_Float;
                     Ref : out Long_Float;
                     Spread : in Long_Float) is
      Ran : Long_Float := Long_Float(FR.Random(G));
      Sign : Long_Float := Long_Float(FR.Random(G) - 0.5); -- Is step + or - ?
      Adjust : Long_Float;
   begin
      Ref := Value;
      Adjust := 1.0 + Spread * Long_Float'Copy_Sign(LEF.Log(Ran), Sign);
      Value := Value * Adjust;
      if Ran < Flip_Value then
         Value := -Value;
      end if;
   end Markov;

   procedure Dump(Set : in LF_Array) is
   begin
      for I in Set'Range loop
         Ada.Long_Float_Text_IO.Put(Set(I), Fore=>4, Aft=>11, Exp=>0);
         Text_IO.Put_Line(",");
      end loop;
   end Dump;

   procedure Reset is
   begin
      DR.Reset(D);
      FR.Reset(G);
   end;

end GEM.Random_Descent;
