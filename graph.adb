with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
Use Ada.Strings.Unbounded;


package body graph is

    -- See if a node exists in master list
    function In_Master_List (Node_Item_Name : Unbounded_String) return Boolean is
        Node_Item : Node_Pointer;        -- Pointer to nodes in master list
        Element_Found : Boolean := false;
    begin
        -- While cursor is on an element
        for C in Master_List.Iterate loop
            Node_Item := Element(C);               -- Store elem in Node_Item
            if Node_Item.ID = Node_Item_Name then  -- Check if Node_item.ID matches
                Element_Found := true;             -- If yes exit return true
                exit;                              -- leave loop
            end if;
        end loop;
        return Element_Found;
    end In_Master_List;

    -- Gets node from master list
    function Get_From_Master_List (Node_Item_Name : Unbounded_String) return Node_Pointer is
        Node_Item : Node_Pointer := Null;
    begin
        for C in Master_List.Iterate loop
            Node_Item := Element(C);
            if Node_Item.ID = Node_Item_Name then
                return Node_Item;
            end if;
        end loop;
        return Node_Item;
    end Get_From_Master_List;

    -- Adds node to master list
    procedure Add_To_Master_List (Node_Name : Unbounded_String) is
        Node_To_Add : Node_Pointer := New Node;  -- Pointer to new node item
    begin
        Node_To_Add.ID := Node_Name;
        Master_List.Append(Node_To_Add);        -- Append new node item
    end Add_To_Master_List;

    -- Link two nodes together
    procedure Link_Nodes (Node_One : Unbounded_String; Node_Two : Unbounded_String) is
        Node_One_Access : Node_Pointer := Get_From_Master_List(Node_One);
        Node_Two_Access : Node_Pointer := Get_From_Master_List(Node_Two);
    begin
        Node_One_Access.Links.Append(Node_Two_Access);

        -- Debug stuff
        -- Put_Line("Linking Nodes: " & To_String(Node_One) &" " &To_String(Node_Two));
        -- Print_Links_Of_Node(Node_One_Access);
    end Link_Nodes;


    procedure Delete_Link (Node_One : Unbounded_String; Node_Two : Unbounded_String) is
        Node_One_Access : Node_Pointer := Get_From_Master_List(Node_One);
        Node_Two_Access : Node_Pointer := Get_From_Master_List(Node_Two);
        Node_Item : Node_Pointer;       -- Points to nodes we iterate through
        -- Need a cursor to use delete as 'for C in...' syntax doesn't work and I tried casting it
        Cursos_Pos : Cursor := First(Node_One_Access.Links); 
    begin
        -- Find where node two is, and delete its link
        while Cursos_Pos /= No_Element loop
            Node_item := Element(Cursos_Pos);
            if Node_Item.ID = Node_Two then
                Delete(Node_One_Access.Links, Cursos_Pos);
                exit;
            end if;
            Next(Cursos_Pos);
        end loop;
        -- Debug Stuff
        -- Put_Line("Deleting Link: " & To_String(Node_One) &" " &To_String(Node_Two));
        -- Print_Links_Of_Node(Node_One_Access);
    end Delete_Link;

    function Query_Link (Node_One : Unbounded_String; Node_Two : Unbounded_String) return Boolean is
        Node_One_Access : Node_Pointer := Get_From_Master_List(Node_One);   -- Node one pointer
        Node_Two_Access : Node_Pointer := Get_From_Master_List(Node_Two);   -- Node two pointer

        DFS_Val : Boolean;          -- Return bool from dfs result

        Visited : List;         -- List of visited nodes

    begin


        -- Call dfs with current pos of our init node, init node, target node, and visited nodes
        DFS_Val := DFS(Node_One_Access, Node_Two_Access, Visited);
        return DFS_Val;
    end Query_Link;



    function DFS (Node : Node_Pointer; 
                 Target : Node_Pointer; Visited : in out List) return Boolean is

        In_Visit : Boolean := False;
        Dummy_Node : Node_Pointer;

    begin
        -- If current node equals target node return true
        if (Node.ID = Target.ID) then
            -- Put_Line ("Found: " & To_String(Target.ID));
            return true;
        end if;

        -- Check if our current node is in visited
        for C in Visited.Iterate loop
            Dummy_Node := Element(C);
            if Dummy_Node.ID = Node.ID then
                In_Visit := true;
            end if;
        end loop;

        -- Add current Node to visited, if it isnt
        if not In_Visit then
            Visited.Append(Node);
            -- Put_Line("Checking Neighbors of " & To_String(Node.ID));
            -- Check neighbors of our current node

            for C in Node.Links.Iterate loop
                if DFS(Element(C), Target, Visited) then
                    return true;
                end if;
            end loop;

        end if;
        return false;
    end DFS;


    procedure Print_Links_Of_Node (Node : Node_Pointer) is
        Node_Item : Node_Pointer := Null;
    begin
        Put_Line ("All nodes linked to " & To_String(Node.ID));

        for C in Node.Links.Iterate loop
            Node_Item := Element(C);
            Put_Line(To_String(Node_Item.ID) & " ");
        end loop;
    end Print_Links_Of_Node;

end graph;