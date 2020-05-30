package body GEM.LTE.Primitives.Shared is

   -- Can only access the info via a pointer
   type Param_P is access Param_S;

   protected Server is
      procedure Put (P : in Param_S);
      entry Get (P : out Param_P);
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
         if not Available then -- only do once
            Params := new Param_S'(P);
         end if;
         Available := True;
      end Put;

      ---------
      -- Get --
      ---------

      entry Get (P : out Param_P) when Available is
      begin
         P := Params;
      end Get;

   end Server;


   procedure Put (P : in Param_S) is
   begin
      Server.Put (P);
   end Put;

   function Get return Param_S is
      P_pointer : Param_P;
   begin
      Server.Get (P_Pointer);
      return P_Pointer.all;
   end Get;

end GEM.LTE.Primitives.Shared;
