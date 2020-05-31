with Ada.Environment_Variables;

package body GEM is

   function Getenv (Name : in String; Default : in String) return String renames
     Ada.Environment_Variables.Value;

   function Getenv (Name : in String; Default : in Integer) return Integer is
   begin
      return Integer'Value (Getenv (Name, Integer'Image (Default)));
   end Getenv;

   function Getenv (Name : in String; Default : in Float) return Float is
   begin
      return Float'Value (Getenv (Name, Float'Image (Default)));
   end Getenv;

   function Getenv (Name : in String; Default : in Long_Integer) return Long_Integer is
   begin
      return Long_Integer'Value (Getenv (Name, Long_Integer'Image (Default)));
   end Getenv;

   function Getenv (Name : in String; Default : in Long_Float) return Long_Float is
   begin
      return Long_Float'Value (Getenv (Name, Long_Float'Image (Default)));
   end Getenv;

   procedure Setenv (Name : in String; Value : in String) is
   begin
      Ada.Environment_Variables.Set(Name, Value);
   end Setenv;

   procedure Clear (Name : in String) is
   begin
      Ada.Environment_Variables.Clear(Name);
   end Clear;

end GEM;
