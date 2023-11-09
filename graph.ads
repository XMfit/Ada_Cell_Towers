-- graph.ads ada specification file (for string data type)

-- Import the Ada.Containers.Double_Linked_Lists package to work with double linked lists.
with Ada.Containers.Doubly_Linked_Lists;
-- Import the Ada.Strings.Unbounded package for strings
with Ada.Strings.Unbounded;

-- Declare the package "Graph," which contains the graph data structure.
package Graph is
   -- Declare the Node_Type and Node_Access types for nodes in the graph.
   type Node_Type is private;
   type Node_Access is access Node_Type;

   -- Declare the main data structure, the graph, as a tagged record.
   type Graph is tagged record
      Nodes : Ada.Containers.Doubly_Linked_Lists;  -- A list of nodes in the graph.
   end record;

   -- Initialize the graph.
   procedure Initialize(G : in out Doubly_Linked_Lists);

   -- Check if a node with the given data exists in the graph.
   function Node_Exists(G : in out Doubly_Linked_Lists; Data : in Node_Type) return Node_Access;

   -- Add a new node to the graph.
   procedure Add_Node(G : in out Doubly_Linked_Lists; Data : in Node_Type);

   -- Add an edge between two nodes if they both exist.
   procedure Add_Edge(G : in out Doubly_Linked_Lists; From : in Node_Type; To : in Node_Type);

   -- Delete an edge between two nodes.
   procedure Delete_Edge(G : in out Doubly_Linked_Lists; From : in Node_Type; To : in Node_Type);

   -- Check if a path exists between two nodes in the graph.
   function Path_Exists(G : in Doubly_Linked_Lists; From : in Node_Type; To : in Node_Type) return Boolean;

   -- Print the contents of the graph.
   procedure Print_Graph(G : in Doubly_Linked_Lists);

private
   -- Import the Ada.Containers.Double_Linked_Lists package with a renaming to "DLL."
   package DLL is new Ada.Containers.Doubly_Linked_Lists;

   -- Declare the internal representation of a node in the graph as a tagged record.
   type Node_Type is tagged record
      Data : Ada.Strings.Unbounded.Unbounded_String;  -- Data associated with the node.
      Edges : DLL.List;  -- List of edges connecting to other nodes.
   end record;
end Graph;
