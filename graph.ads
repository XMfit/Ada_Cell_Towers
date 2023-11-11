--   Author: Thomas Johnson, thomasjohnso2020@my.fit.edu
--   Author: Lamine Djibo, ldjibo2016@my.fit.edu
--   Course: CSE 4250, Fall 2023
--   Project: Project #3, can you HEAR me now?
--   Implementation: GNATMAKE 12.3.0

--   with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Graph is

   type Node;      --   Thing storing info
   type Node_Pointer is access Node;    --   Pointer to node

   --   package definine a doubly linked list of nodes called Node_Lists
   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists
                             (Element_Type => Node_Pointer);
   use Node_Lists;

   --   Node storing id, visit tag for dfs, and its links
   type Node is record
      ID : Unbounded_String;
      Links : List;
   end record;

   --   List of every Node apart of the graph
   Master_List : List;

   function In_Master_List (Node_Item_Name : Unbounded_String) return Boolean;

   function Get_From_Master_List (Node_Item_Name : Unbounded_String)
                                  return Node_Pointer;

   procedure Add_To_Master_List (Node_Name : Unbounded_String);

   procedure Link_Nodes (Node_One : Unbounded_String;
                         Node_Two : Unbounded_String);

   function Direct_Link (Node_One : Unbounded_String;
                         Node_Two : Unbounded_String) return Boolean;

   procedure Delete_Link (Node_One : Unbounded_String;
                          Node_Two : Unbounded_String);

   function Query_Link (Node_One : Unbounded_String;
                        Node_Two : Unbounded_String) return Boolean;

   function DFS (Node : Node_Pointer;
                Target : Node_Pointer; Visited : in out List) return Boolean;

   procedure Print_Links_Of_Node (Node : Node_Pointer);

end Graph;