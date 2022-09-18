with Text_IO;
with GEM.LTE;
with GEM.dLOD;
with Ada.Command_Line;

procedure dLOD_Call is
begin

   declare
      Name : constant String := Ada.Command_Line.Argument(1);
      -- "dlod3.dat"
      DBLTAP : Gem.LTE.Long_Periods_Amp_Phase  :=  GEM.dLOD(Name);
   begin
      null;
   end;

end;

-- 0.99064132219=CC  365.24123840000=Yr
-- 0.99064647703=CC  365.24223840000=Yr
-- 0.99064997996=CC  365.24323840000=Yr
-- 0.99065181974=CC  365.24423840000=Yr
-- 0.99065198518=CC  365.24523840000=Yr  ***
-- 0.99065046507=CC  365.24623840000=Yr
-- 0.99064724829=CC  365.24723840000=Yr

