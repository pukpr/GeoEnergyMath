with Ada.Direct_IO;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.OS_Lib;

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
      if GEM.Command_Line_Option_Exists("p") then
         Dump(P);
         GNAT.OS_Lib.Os_Exit(0);
      end if;
      Close (FT);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Error:" & FN);
         if Is_Open(FT) then
            Close(FT);
         end if;
   end Load;

   procedure Dump (D : in Param_S) is
   begin
         Put(D.B.Offset, " :offset:", NL);
         Put(D.B.Bg,     " :bg:", NL);
         Put(D.B.ImpA,   " :impA:", NL);
         Put(D.B.ImpB,   " :impB:", NL);
         Put(D.B.mA,     " :mA:", NL);
         Put(D.B.mP,     " :mP:", NL);
         Put(D.B.shiftT, " :shiftT:", NL);
         Put(D.B.init,   " :init:", NL);
         Ada.Text_IO.Put_Line("---- Tidal ----");
         for I in D.B.LPAP'Range loop
            Put(D.A.LP(I), ", ");
            Put(D.B.LPAP(I).Amplitude, ", ");
            Put(D.B.LPAP(I).Phase, I'Img, NL);
         end loop;
   end Dump;

end GEM.LTE.Primitives.Shared;
