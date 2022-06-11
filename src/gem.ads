package GEM is
   -- Top level directory for all GeoEnergyMath software routines
   -- Adapted from Mathematical GeoEnergy (Wiley/2009)
   -- All GEM child libraries inherit from this adaptation

   function Getenv (Name : in String; Default : in String) return String;
   function Getenv (Name : in String; Default : in Integer) return Integer;
   function Getenv (Name : in String; Default : in Long_Integer) return Long_Integer;
   function Getenv (Name : in String; Default : in Float) return Float;
   function Getenv (Name : in String; Default : in Long_Float) return Long_Float;
   function Getenv (Name : in String; Default : in Boolean) return Boolean;

   procedure Setenv (Name : in String; Value : in String);
   procedure Clear (Name : in String);

   -- List of integers from "1 2 4 6" etc
   type Ns is array(Positive range <> ) of Positive;
   function S_to_I (S : in string) return Ns;


end GEM;
