
generic
   with function Fixed (Value : in Long_Float) return Boolean is <>;
   Set_Range : in Positive;
   Harmonic_Range : in Positive;
package GEM.Random_Descent is

   -- Instantiated with an array, i.e set of floats
   -- any values in Fixed are not modified

   type LF_Array is array (Positive range <>) of Long_Float; -- this could be in generic formal

   -- This is essentially a gradient descent probe, with a step length that has
   -- a damped exponential PDF, i.e Markov. So it does +/- random steps that
   -- are scaled by the Spread parameter.
   procedure Markov (Set : in out LF_Array;  -- Randomly selects from param set
                     Ref : out LF_Array;     -- returns the unchanged set
                     Spread : in Long_Float;
                     Cal : in LF_Array);     -- Calibrated starting point

   procedure Markov (Value : in out Long_Float;  -- Randomly selects from param set
                     Ref : out Long_Float;     -- returns the unchanged set
                     Spread : in Long_Float;
                     Cal : in Long_Float);

   procedure Dump(Set : in LF_Array);

   procedure Random_Harmonic(Index : in out Positive;
                             Ref : out Positive);
   procedure Random_Harmonic(Index : in out Ns;
                             Ref : out Ns);

   -- Random number generator reset, otherwise it will start from fixed seed
   procedure Reset;

end GEM.Random_Descent;
