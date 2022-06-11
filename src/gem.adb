with Ada.Environment_Variables;
with Ada.Command_Line;
with Text_IO;
with Ada.Integer_Text_IO;

package body GEM is

   --function Getenv (Name : in String; Default : in String) return String renames
   --  Ada.Environment_Variables.Value; -- works but can't debug

   function Getenv (Name : in String; Default : in String) return String is
      Str : constant String := Ada.Environment_Variables.Value(Name,Default);
   begin
      if Ada.Command_Line.Argument_Count > 0 then
         Text_IO.Put(":" & Name & "=" & Str & ":");
      end if;
      return Str;
   end Getenv;

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

   function Getenv (Name : in String; Default : in Boolean) return Boolean is
   begin
      return Boolean'Value (Getenv (Name, Boolean'Image (Default)));
   end Getenv;

   procedure Setenv (Name : in String; Value : in String) is
   begin
      Ada.Environment_Variables.Set(Name, Value);
   end Setenv;

   procedure Clear (Name : in String) is
   begin
      Ada.Environment_Variables.Clear(Name);
   end Clear;

   -- String to list of integers
   function S_to_I (S : in string) return Ns is
     use Ada.Integer_Text_IO;
     List : Ns(1..100); -- magic number
     N, L : Integer := 0;
     Index : Integer := 1;
   begin
     loop
        get(S(L+1..S'Last), N, L);
        List(Index) := N;
        Index := Index+1;
     end loop;
   exception
     when others  =>
        return List(1..Index-1);
   end;



end GEM;
