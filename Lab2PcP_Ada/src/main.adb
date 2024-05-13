with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Main is

   task type Min_Element_Search_Task is
      pragma Storage_Size (10_000_000);
   end Min_Element_Search_Task;

   task body Min_Element_Search_Task is

      Num_Of_Threads  : constant Integer := 4;
      Num_Of_Elements : constant Integer := 1_000_000;

      Array_Of_Elements : array (1 .. Num_Of_Elements) of Integer;
      type Index_Range is range 1 .. Num_Of_Elements;

      protected Update_Handler is
         procedure Set_Minimum_Index (Found_Min_Index : in Integer);
         entry Get_Minimum_Index (Index : out Integer);
      private
         Min_Index     : Integer := Array_Of_Elements'First;
         Num_Of_Tasks   : Integer := 0;
      end Update_Handler;

      task type Element_Searcher is
         entry Start (Start_Index, End_Index : in Integer);
      end Element_Searcher;

      function Find_Minimum_Index (Start_Index, End_Index : in Integer) return Integer
      is
         Current_Min_Index : Integer := Start_Index;
      begin
         for Index in Start_Index .. End_Index loop
            if Array_Of_Elements(Index) < Array_Of_Elements(Current_Min_Index) then
               Current_Min_Index := Index;
            end if;
         end loop;
         return Current_Min_Index;
      end Find_Minimum_Index;

      task body Element_Searcher is
         Start_Index, End_Index : Integer;
         Found_Min_Index : Integer := 0;
      begin
         accept Start (Start_Index, End_Index : in Integer) do
            Element_Searcher.Start_Index := Start_Index;
            Element_Searcher.End_Index := End_Index;
         end Start;
         Found_Min_Index :=
           Find_Minimum_Index (Start_Index => Start_Index, End_Index => End_Index);
         Update_Handler.Set_Minimum_Index (Found_Min_Index);
      end Element_Searcher;

      type Element_Searcher_Array is array (Integer range <>) of Element_Searcher;

      procedure Initialize_Array is
      begin
         for Index in 1 .. Num_Of_Elements loop
            Array_Of_Elements (Index) := Index;
         end loop;
      end Initialize_Array;

      procedure Set_Random_Minimum is
         package Random_Integers is new Ada.Numerics.Discrete_Random (Index_Range);
         use Random_Integers;
         Random_Index : Integer;
         Random_Gen   : Generator;
      begin
         Reset (Random_Gen);
         Random_Index := Integer (Random (Random_Gen));
         Array_Of_Elements (Random_Index) := -111;
         Put_Line("New minimum set - index:" & Random_Index'Img & " number:" &
            Array_Of_Elements (Random_Index)'Img);
      end Set_Random_Minimum;

      protected body Update_Handler is
         procedure Set_Minimum_Index (Found_Min_Index : in Integer) is
         begin
            if Array_Of_Elements (Found_Min_Index) < Array_Of_Elements (Min_Index) then
               Min_Index := Found_Min_Index;
            end if;
            Num_Of_Tasks := Num_Of_Tasks + 1;
         end Set_Minimum_Index;

         entry Get_Minimum_Index (Index : out Integer) when Num_Of_Tasks = Num_Of_Threads is
         begin
            Index := Min_Index;
         end Get_Minimum_Index;

      end Update_Handler;

      procedure Parallel_Search is
         Step         : Integer := Array_Of_Elements'Length / Num_Of_Threads;
         Threads      : Element_Searcher_Array (1 .. Num_Of_Threads);
         Boundary     : Integer := Array_Of_Elements'First;
         Result_Min_Index : Integer := 0;
      begin
         for I in 1 .. (Num_Of_Threads - 1) loop
            Put_Line(I'Img & " part have bounds: Left:" & Integer'Image (Boundary) & " Right:" &
                 Integer'Image (Boundary + Step));

            Threads (I).Start (Boundary, Boundary + Step);
            Boundary := Boundary + Step;
         end loop;
         Put_Line("The last part have bounds: Left:" & Integer'Image (Boundary) & " Right:" &
              Integer'Image (Num_Of_Elements));

         Threads (Threads'Last).Start (Boundary, Num_Of_Elements);

         Update_Handler.Get_Minimum_Index (Result_Min_Index);

         Put_Line ("Minimal element in array: " & Array_Of_Elements (Result_Min_Index)'Img);
         Put_Line ("Index of minimal element in array: " & Result_Min_Index'Img);
      end Parallel_Search;

   begin
      Initialize_Array;
      Set_Random_Minimum;
      Parallel_Search;

   end Min_Element_Search_Task;

   Find_Minimum : Min_Element_Search_Task;
begin
   null;
end Main;