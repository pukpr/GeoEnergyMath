
package GEM.LTE.Primitives.Shared is

   Max_Harmonics : constant := 20;

   type Param_A (NLP, NLT : Integer) is
      record
         k0     : Long_Float;
         level  : Long_Float;
         LP     : Long_Periods(1..NLP);  -- Fixed
         LTAP   : Modulations_Amp_Phase(1..NLT); -- Calculated
      end record;

   type Param_B (NLP, NLT : Integer) is
      record                    -- Random walk values
         Offset : Long_Float;   -- Integrated trend on forcing
         bg     : Long_Float;   -- Background of impulse
         ImpA   : Long_Float;   -- Amplitude of sin impulse
         ImpB   : Long_Float;   -- Phase of sin impulse
         ImpC   : Long_Float;   -- Power of sin impulse
         DelA   : Long_Float;   -- Amp of delta impulse
         DelB   : Long_Float;   -- Phase of delta impulse
         Asym   : Long_Float;   -- Asymmetry of semi-annual impulse

         Ann1   : Long_Float;   -- Amplitude annual
         Ann2   : Long_Float;   -- Phase annual
         Sem1   : Long_Float;   -- Amplitude semi-annual
         Sem2   : Long_Float;   -- Phase semi-annual
         IR     : Long_Float;   -- Impulse pass-through
         Year   : Long_Float;   -- Year correction (in days)

         mA     : Long_Float;   -- 1st order response
         mP     : Long_Float;   -- 2nd order response
         shiftT : Long_Float;   -- Starting time correction
         init   : Long_Float;   -- Initial value

         LPAP   : Long_Periods_Amp_Phase(1..NLP);
         LT     : Modulations(1..NLT);
      end record;

   -- Record structure used for optimization sharing
   -- This could use the benefit of Ada record representation clauses,
   -- but as it is with GNAT compiler, there is no dope information
   -- and so is essentially a linear array of long_floats

   type Param_S (NLP, NLT : Integer) is
      record
         A : Param_A(NLP, NLT);
         B : Param_B(NLP, NLT);
         C : NS(1..Max_Harmonics) := (others => 0);
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

   procedure Dump (D : in Param_S);


end GEM.LTE.Primitives.Shared;
