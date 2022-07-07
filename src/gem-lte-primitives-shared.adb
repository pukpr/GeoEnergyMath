with Ada.Direct_IO;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO;

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

   procedure Put (FT : in Ada.Text_IO.File_Type;
                  Text : in String;
                  Value : in Long_Float) is
   begin
      Ada.Text_IO.Put(FT, Text & " ");
      Ada.Long_Float_Text_IO.Put(FT, Value, Fore=>4, Aft=>11, Exp=>0);
      Ada.Text_IO.New_Line(FT);
   end;

   procedure Put (FT : in Ada.Text_IO.File_Type;
                  Text : in String;
                  Value : in Integer) is
   begin
      Ada.Text_IO.Put(FT, Text & " " & Integer'Image(Value));
      Ada.Text_IO.New_Line(FT);
   end;

   procedure Put (FT : Ada.Text_IO.File_Type;
                  V1,V2,V3 : in Long_Float) is
   begin
      Ada.Long_Float_Text_IO.Put(FT, V1, Fore=>4, Aft=>11, Exp=>0);
      Ada.Long_Float_Text_IO.Put(FT, V2, Fore=>4, Aft=>11, Exp=>0);
      Ada.Long_Float_Text_IO.Put(FT, V3, Fore=>4, Aft=>11, Exp=>0);
      Ada.Text_IO.New_Line(FT);
   end;

   procedure Write (D : in Param_S) is
      FN : constant String := Ada.Command_Line.Command_Name & ".par";
      FT : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create(FT, Ada.Text_IO.Out_File, FN);
      Put(FT, "offs", D.B.Offset);
      Put(FT, "bg  ", D.B.Bg);
      Put(FT, "impA", D.B.ImpA);
      Put(FT, "impB", D.B.ImpB);
      Put(FT, "impC", D.B.ImpC);
      Put(FT, "delA", D.B.DelA);
      Put(FT, "delB", D.B.DelB);
      Put(FT, "asym", D.B.Asym);
      Put(FT, "ann1", D.B.Ann1);
      Put(FT, "ann2", D.B.Ann2);
      Put(FT, "sem1", D.B.Sem1);
      Put(FT, "sem2", D.B.Sem2);
      Put(FT, "year", D.B.Year);
      Put(FT, "IR  ", D.B.IR);
      Put(FT, "ma  ", D.B.mA);
      Put(FT, "mp  ", D.B.mP);
      Put(FT, "shfT", D.B.shiftT);
      Put(FT, "init", D.B.init);
      for I in D.B.LPAP'Range loop
         Put(FT,
             D.A.LP(I),
             D.B.LPAP(I).Amplitude,
             D.B.LPAP(I).Phase);
      end loop;
      for I in D.B.LT'Range loop
         Put(FT, "ltep", D.B.LT(I));
      end loop;
      for I in D.C'Range loop
         Put(FT, "harm", D.C(I));
         exit when D.C(I) = 0;
      end loop;
      Ada.Text_IO.Close(FT);
   end Write;

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
      if GEM.Command_Line_Option_Exists("r") then
         Write(P);
      end if;
      -- Server.Put (P);
   end Save;

   procedure Read (FT : in Ada.Text_IO.File_Type;
                   Text : in String;
                   Value : in out Long_Float) is
      Str : String(1..100);
      N : Integer;
   begin
      Ada.Text_IO.Get_Line(FT, Str, N);
      if Text = Str(1..4) then
         Ada.Long_Float_Text_IO.Get(Str(5..N), Value, N);
      else
         Ada.Text_IO.Put_Line(Text & " mismatches " & Str(1..N));
         GNAT.OS_Lib.Os_Exit(0);
      end if;
   end;

    procedure Read (FT : in Ada.Text_IO.File_Type;
                    Text : in String;
                    Value : in out Integer) is
      Str : String(1..100);
      N : Integer;
   begin
      Ada.Text_IO.Get_Line(FT, Str, N);
      if Text = Str(1..4) then
         Ada.Integer_Text_IO.Get(Str(5..N), Value, N);
      else
         Ada.Text_IO.Put_Line(Text & " mismatches " & Str(1..N));
         GNAT.OS_Lib.Os_Exit(0);
      end if;
   end;

   procedure Read (FT : in Ada.Text_IO.File_Type;
                  V1,V2,V3 : out Long_Float) is
      Str : String(1..100);
      N : Integer;
      Floats : Fs(1..3);
      Index : Integer := 1;
   begin
      Ada.Text_IO.Get_Line(FT, Str, N);
      Floats := S_to_LF (Str(1..N));
      V1 := Floats(1);
      V2 := Floats(2);
      V3 := Floats(3);
   exception
      when others =>
         Ada.Text_IO.Put_Line("Finished " & Str(1..N));
   end;


   procedure Read (D : in out Param_S) is
      FN : constant String := Ada.Command_Line.Command_Name & ".par";
      FT : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open(FT, Ada.Text_IO.In_File, FN);
      Read(FT, "offs", D.B.Offset);
      Read(FT, "bg  ", D.B.Bg);
      Read(FT, "impA", D.B.ImpA);
      Read(FT, "impB", D.B.ImpB);
      Read(FT, "impC", D.B.ImpC);
      Read(FT, "delA", D.B.DelA);
      Read(FT, "delB", D.B.DelB);
      Read(FT, "asym", D.B.Asym);
      Read(FT, "ann1", D.B.Ann1);
      Read(FT, "ann2", D.B.Ann2);
      Read(FT, "sem1", D.B.Sem1);
      Read(FT, "sem2", D.B.Sem2);
      Read(FT, "year", D.B.Year);
      Read(FT, "IR  ", D.B.IR);
      Read(FT, "ma  ", D.B.mA);
      Read(FT, "mp  ", D.B.mP);
      Read(FT, "shfT", D.B.shiftT);
      Read(FT, "init", D.B.init);
      for I in D.B.LPAP'Range loop
         Read(FT,
             D.A.LP(I),
             D.B.LPAP(I).Amplitude,
             D.B.LPAP(I).Phase);
      end loop;
      for I in D.B.LT'Range loop
         Read(FT, "ltep", D.B.LT(I));
      end loop;
      for I in D.C'Range loop
         Read(FT, "harm", D.C(I));
      end loop;
      Ada.Text_IO.Close(FT);
   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Put_Line ("Closing" & FN);
         Ada.Text_IO.Close(FT);
   end Read;

   procedure Load (P : in out Param_S) is
      subtype PS is Param_S(P.NLP,P.NLT);
      package DIO is new Ada.Direct_IO(PS);
      FN : constant String := Ada.Command_Line.Command_Name & ".parms";
      use DIO;
      FT : File_Type;
   begin
      begin
         Open(FT, In_File, FN);
         Read (FT, P);
         Close (FT);
      exception
         when Name_Error =>
            Ada.Text_IO.Put_Line ("No PARMS file: " & FN);
         when others =>
            Ada.Text_IO.Put_Line ("Error:" & FN);
            if Is_Open(FT) then
               Close(FT);
            end if;
      end;
      if GEM.Command_Line_Option_Exists("p") then
         Dump(P);
         GNAT.OS_Lib.Os_Exit(0);
      elsif GEM.Command_Line_Option_Exists("w") then
         Write(P);
         GNAT.OS_Lib.Os_Exit(0);
      elsif GEM.Command_Line_Option_Exists("r") then
         Read(P);
      end if;
   end Load;

   procedure Dump (D : in Param_S) is
   begin
         Put(D.B.Offset, " :offset:", NL);
         Put(D.B.Bg,     " :bg:", NL);
         Put(D.B.ImpA,   " :impA:", NL);
         Put(D.B.ImpB,   " :impB:", NL);
         Put(D.B.ImpC,   " :impC:", NL);
         Put(D.B.DelA, ":delA:", NL);
         Put(D.B.DelB, ":delB:", NL);
         Put(D.B.Asym, ":asym:", NL);
         Put(D.B.Ann1, ":ann1:", NL);
         Put(D.B.Ann2, ":ann2:", NL);
         Put(D.B.Sem1, ":sem1:", NL);
         Put(D.B.Sem2, ":sem2:", NL);
         Put(D.B.Year, ":year:", NL);
         Put( D.B.IR,  ":IR:",   NL);
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
         Ada.Text_IO.Put_Line("---- LTE static ----");
         for I in D.B.LT'Range loop
            Put(D.B.LT(I), "", NL);
         end loop;
   end Dump;

end GEM.LTE.Primitives.Shared;
