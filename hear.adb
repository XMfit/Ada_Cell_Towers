with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;
Use Ada.Strings.Unbounded;

with Graph;

procedure hear is

    -- Input line with set length, and var to track length
    Input : String (1 .. 150);
    Last : Natural;

    -- Subtype to index chars in the string
    Subtype Index is Integer range 0 .. 150;

    -- Index position of each tower
    Tower_One_Start : Integer;
    Tower_One_End : Integer;
    Tower_Two_Start : Integer;
    Tower_Two_End : Integer;
    Command : Character;

    -- Tower names
    Tower_One_Name : Unbounded_String;
    Tower_Two_Name : Unbounded_String;

    -- Get tower name using starting, and stopping index
    function Get_Name(Input : String; Start_Pos : Index; End_Pos : Index) return String is
        Name : String(1..(End_Pos - Start_Pos) + 1);
    begin
        Name := Input(Start_Pos .. End_Pos);
        return Name;
    end Get_Name;

    -- Find starting index of a name
    function Tower_Start(Input : String; Start_Pos : Index) return Integer is 
    begin 
        for I in Start_Pos .. 150 loop
            if Input(I) /= ' ' then        -- parse through spaces untit char found
                    return I;
            end if;
        end loop;
    return 0;
    end Tower_Start;

    -- Find ending index of a name
    function Tower_End(Input : String; Start_Pos : Index) return Integer is
    begin
        for I in Start_Pos .. 150 loop
            -- end parsing when finding a space, or command
            if Input(I) = ' ' then
                return (I - 1);
            elsif Input(I) = '.' then
                return (I - 1);
            elsif Input(I) = '?' then
                return (I - 1);
            elsif Input(I) = '#' then
                return (I - 1);
            end if;
        end loop;
    return 0;
    end Tower_End; 

    function Return_Command(Input : String; Start_Pos : Index) return Character is
    begin
        for I in Start_Pos .. 150 loop
            -- End parsing when a command is found
            if Input(I) = '.' then
                return Input(I);
            elsif Input(I) = '?' then
                return Input(I);
            elsif Input(I) = '#' then
                return Input(I);
            end if;
        end loop;
    return 'a';
    end Return_Command;

   procedure Execute_Command(Tower_One : Unbounded_String; Tower_Two : Unbounded_String; Command : Character) is 

   begin
        if command = '.' then
            -- Check if towers exists, and add if need be
            if not (Graph.In_Master_List(Tower_One)) then
                Graph.Add_To_Master_List(Tower_One);
            end if;

            if not (Graph.In_Master_List(Tower_Two)) then
                Graph.Add_To_Master_List(Tower_Two);
            end if;

            Graph.Link_Nodes (Tower_One, Tower_Two);
            
        elsif command = '#' then
            Graph.Delete_Link(Tower_One, Tower_Two);

        elsif command = '?' then
            -- If either tower isn't in master list than the connection doesn't exist.
            if not (Graph.In_Master_List(Tower_One) and Graph.In_Master_List(Tower_Two)) then
                Put_Line("- " & To_String(Tower_One) & " => " & To_String(Tower_Two));
            else
                if Graph.Query_Link(Tower_One, Tower_Two) then
                    Put_Line("+ " & To_String(Tower_One) & " => " & To_String(Tower_Two));
                else
                    Put_Line("- " & To_String(Tower_One) & " => " & To_String(Tower_Two));
                end if;
            end if;
            
        end if;
        

   end Execute_Command;

begin

    while not (End_Of_File) loop
        -- Gets Input
        Get_Line(Item => Input, Last => Last);

        -- Get start and stop index of towers
        Tower_One_Start := Tower_Start(Input, 1);
        Tower_One_End := Tower_End(Input, Tower_One_Start);
        Tower_Two_Start := Tower_Start(Input, Tower_One_End + 1);
        Tower_Two_End := Tower_End(Input, Tower_Two_Start);

        -- Get command
        Command := Return_Command(Input, Tower_Two_End);

        -- Debugging stuff
        --Put_Line(Character'Image(Command));

        --Put_Line(Index'Image(Tower_One_Start) & Index'Image(Tower_One_End));
        --Put_Line(Index'Image(Tower_Two_Start) & Index'Image(Tower_Two_End));

        -- Stores names
        Tower_One_name := To_Unbounded_String(Get_Name(Input, Tower_One_Start, Tower_One_End));
        Tower_Two_Name := To_Unbounded_String(Get_Name(Input, Tower_Two_Start, Tower_Two_End));

        -- Execute Command
        Execute_Command(Tower_One_Name, Tower_Two_Name, Command);
    end loop;

    

end hear;
