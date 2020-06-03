with Ada.Direct_IO;
with Ada.Command_Line;
with Ada.Text_IO;

package body GEM.LTE.Primitives.Shared is

   -- Can only access the info via a pointer
   type Param_P is access all Param_S;

   protected Server is
      procedure Put (P : in Param_S);
      entry Get (P : in out Param_S);
   private
      Available : Boolean := False;
      Params : Param_P;
   end Server;

   ------------
   -- Server --
   ------------

   protected body Server is

      ---------
      -- Put --
      ---------

      procedure Put (P : in Param_S) is
      begin
         if not Available then -- only do once to allocate
            Params := new Param_S'(P);
         else  -- use allocated space
            Params.all := P;
         end if;
         Available := True;
      end Put;

      ---------
      -- Get --
      ---------

      entry Get (P : in out Param_S) when Available is
      begin
         P := Params.all;
      end Get;

   end Server;


   procedure Put (P : in Param_S) is
   begin
      Server.Put (P);
   end Put;

   function Get (N_Tides, N_Modulations : in Integer) return Param_S is
      P : Param_S(N_Tides, N_Modulations);
   begin
      Server.Get (P);
      return P;
   end Get;

   procedure Save (P : in Param_S) is
      subtype PS is Param_S(P.NLP,P.NLT);
      package DIO is new Ada.Direct_IO(PS);
      FN : constant String := Ada.Command_Line.Command_Name & ".parms";
      use DIO;
      FT : File_Type;
   begin
      Create(FT, Out_File, FN);
      Write(FT, P);
      Close(FT);
      -- Server.Put (P);
   end Save;

   procedure Load (P : in out Param_S) is
      subtype PS is Param_S(P.NLP,P.NLT);
      package DIO is new Ada.Direct_IO(PS);
      FN : constant String := Ada.Command_Line.Command_Name & ".parms";
      use DIO;
      FT : File_Type;
   begin
      Open(FT, In_File, FN);
      Read (FT, P);
      Close (FT);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Error:" & FN);
         if Is_Open(FT) then
            Close(FT);
         end if;
   end Load;

end GEM.LTE.Primitives.Shared;
