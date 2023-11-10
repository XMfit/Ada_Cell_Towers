with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
Use Ada.Strings.Unbounded;


package body graph is

    function In_Master_List (Node_Item_Name : Unbounded_String) return Boolean is

        -- Cursor!!!
        Cursor_Pos : Cursor := First(Master_List);
        Node_Item : Node_Access;        -- Pointer to nodes in master list
        Element_Found : Boolean := false;
        
    begin
        -- While cursor is on an element
        while Cursor_Pos /= No_Element loop
            Node_Item := Element(Cursor_Pos);      -- Store elem in Node_Item
            if Node_Item.ID = Node_Item_Name then  -- Check if Node_item.ID matches
                Element_Found := true;             -- If yes exit return true
                exit;       -- leave loop
            end if;
            Next(Cursor_Pos);                      -- Iterate
        end loop;
        return Element_Found;
    end In_Master_List;

    procedure Add_To_Master_List (Node_Name : Unbounded_String) is
        Node_To_Add : Node_Access := New Node;  -- Pointer to new node item
    begin
        Node_To_Add.ID := Node_Name;
        Master_List.Append(Node_To_Add);        -- Append new node item
    end Add_To_Master_List;


end graph;