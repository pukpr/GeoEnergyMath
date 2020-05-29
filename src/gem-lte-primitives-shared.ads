
package GEM.LTE.Primitives.Shared is

   -- Record structure used for optimization sharing
   -- This could use the benefit of Ada record representation clauses,
   -- but as it is with GNAT compiler, there is no dope information
   -- and so is essentially a linear array of long_floats

   type Param_S (NLP, NLT : Positive) is
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

         LP     : Long_Periods(1..NLP);
         LT     : Modulations(1..NLT);

      end record;

   -- don't think this is necessary but watch for cases where the
   -- compiler may optimize away reads and treat the data as unused
   -- pragma Volatile(Param_S);

end GEM.LTE.Primitives.Shared;
