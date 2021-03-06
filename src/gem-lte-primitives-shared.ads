
package GEM.LTE.Primitives.Shared is

   -- Record structure used for optimization sharing
   -- This could use the benefit of Ada record representation clauses,
   -- but as it is with GNAT compiler, there is no dope information
   -- and so is essentially a linear array of long_floats

   type Param_S (NLP, NLT : Integer) is
      record
         Offset : Long_Float;
         bg     : Long_Float;
         ImpA   : Long_Float;
         ImpB   : Long_Float;
         ImpC   : Long_Float;
         ImpD   : Long_Float;

         mA     : Long_Float;
         mP     : Long_Float;
         mD     : Long_Float;
         shiftT : Long_Float;
         fB     : Long_Float;
         fC     : Long_Float;
         fA     : Long_Float;
         k0     : Long_Float;
         level  : Long_Float;
         init   : Long_Float;
         order2 : Long_Float;
         order3 : Long_Float;
         -- LH     : Harmonics(1..126); --36 126

         LP     : Long_Periods(1..NLP);
         LT     : Modulations(1..NLT);

      end record;

   -- don't think this is necessary but watch for cases where the
   -- compiler may optimize away reads and treat the data as unused
   -- pragma Volatile(Param_S);

   --
   -- This is the approach for delivering the input parameters to the
   -- processing threads. The call to Put will release the Get per thread
   --
   procedure Put (P : in Param_S);

   function Get(N_Tides, N_Modulations : in Integer) return Param_S;

   --
   -- from a file -- name of file executable
   --
   procedure Save (P : in Param_S);

   procedure Load (P : in out Param_S);


end GEM.LTE.Primitives.Shared;
