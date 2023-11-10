--   Author: Thomas Johnson, thomasjohnso2020@my.fit.edu
--   Author: Lamine Djibo,
--   Course: CSE 4250, Fall 2023
--   Project: Project #3, can you HEAR me now?
--   Implementation: GNATMAKE 12.3.0

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;


package body Graph is

   --   See if a node exists in master list
   function In_Master_List (Node_Item_Name : Unbounded_String)
                            return Boolean is
      Node_Item : Node_Pointer;        -- Pointer to nodes in master list
      Element_Found : Boolean := False;
   begin
      --   While cursor is on an element
      for C in Master_List.Iterate loop
         Node_Item := Element (C);
         if Node_Item.ID = Node_Item_Name then
            Element_Found := True;
            exit;
         end if;
      end loop;
      return (Element_Found);
   end In_Master_List;

   --   Gets node from master list
   function Get_From_Master_List (Node_Item_Name : Unbounded_String)
                                 return Node_Pointer is
      Node_Item : Node_Pointer := null;
   begin
      for C in Master_List.Iterate loop
         Node_Item := Element (C);
         if Node_Item.ID = Node_Item_Name then
            return Node_Item;
         end if;
      end loop;
      return Node_Item;
   end Get_From_Master_List;

   --   Adds node to master list
   procedure Add_To_Master_List (Node_Name : Unbounded_String) is
      Node_To_Add : Node_Pointer := new Node;  --   Pointer to new node item
   begin
      Node_To_Add.ID := Node_Name;
      Master_List.Append (Node_To_Add);  --   Append new node item
   end Add_To_Master_List;

   --   Link two nodes together
   procedure Link_Nodes (Node_One : Unbounded_String;
                         Node_Two : Unbounded_String) is
      Node_One_Access : Node_Pointer := Get_From_Master_List (Node_One);
      Node_Two_Access : Node_Pointer := Get_From_Master_List (Node_Two);
   begin
      Node_One_Access.Links.Append (Node_Two_Access);
   end Link_Nodes;

   function Direct_Link (Node_One : Unbounded_String;
                         Node_Two : Unbounded_String) return Boolean is
      Node_One_Access : Node_Pointer := Get_From_Master_List (Node_One);
      Node_Two_Access : Node_Pointer := Get_From_Master_List (Node_Two);
      Node_Item : Node_Pointer := null;
   begin
      for C in Node_One_Access.Links.Iterate loop
         Node_Item := Element (C);
         if Node_Item.ID = Node_Two_Access.ID then
            return True;
         end if;
      end loop;
      return False;
   end Direct_Link;

   procedure Delete_Link (Node_One : Unbounded_String;
                          Node_Two : Unbounded_String) is
      Node_One_Access : Node_Pointer := Get_From_Master_List (Node_One);
      Node_Two_Access : Node_Pointer := Get_From_Master_List (Node_Two);
      Node_Item : Node_Pointer;
      Cursor_Pos : Cursor := First (Node_One_Access.Links);
   begin
      --   Find where node two is, and delete its link
      while Cursor_Pos /= No_Element loop
         Node_Item := Element (Cursor_Pos);
         if Node_Item.ID = Node_Two then
            Delete (Node_One_Access.Links, Cursor_Pos);
            exit;
         end if;
         Next (Cursor_Pos);
      end loop;
   end Delete_Link;

   function Query_Link (Node_One : Unbounded_String;
                        Node_Two : Unbounded_String) return Boolean is
      Node_One_Access : Node_Pointer := Get_From_Master_List (Node_One);
      Node_Two_Access : Node_Pointer := Get_From_Master_List (Node_Two);

      DFS_Val : Boolean;     --   Return bool from dfs result

      Visited : List;        --   List of visited nodes

   begin
      --   Call dfs with init node, target node, and visited nodes
      DFS_Val := DFS (Node_One_Access, Node_Two_Access, Visited);
      return DFS_Val;
   end Query_Link;

   function DFS (Node : Node_Pointer;
                 Target : Node_Pointer; Visited : in out List)
                 return Boolean is

      In_Visit : Boolean := False;
      Dummy_Node : Node_Pointer;

   begin
      --   If current node equals target node return true
      if (Node.ID = Target.ID) then
         return True;
      end if;

      --   Check if our current node is in visited
      for C in Visited.Iterate loop
         Dummy_Node := Element (C);
         if Dummy_Node.ID = Node.ID then
            In_Visit := True;
         end if;
      end loop;

      --   Add current Node to visited, if it isnt
      if not In_Visit then
         Visited.Append (Node);
         --   Put_Line("Checking Neighbors of " & To_String(Node.ID));
         --   Check neighbors of our current node

         for C in Node.Links.Iterate loop
            if DFS (Element (C), Target, Visited) then
               return True;
            end if;
         end loop;

      end if;
      return False;
   end DFS;

   procedure Print_Links_Of_Node (Node : Node_Pointer) is
      Node_Item : Node_Pointer := null;
   begin
      for C in Node.Links.Iterate loop
         Node_Item := Element (C);
         Put_Line (To_String (Node_Item.ID) & " ");
      end loop;
   end Print_Links_Of_Node;

end Graph;