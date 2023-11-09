-- graph.adb data structure implementation (body) for string data type

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

package body Graph is

  -- Use the DLL package for double linked lists from standart library
   package DLL is new Ada.Containers.Doubly_Linked_Lists;

  -- constructor for graph
   procedure Initialize(G : in out Doubly_Linked_Lists) is
   begin
      -- Clear the list of nodes to initialize the graph.
      DLL.Clear(G.Nodes);
   end Initialize;


  -- Checks if a node with the given data exists in the graph and returns it
   function Node_Exists(G : in out Doubly_Linked_Lists; Data : in Node_Type) return Node_Access is
   Current_Node : Node_Access;
   Current_Cursor : DLL.Cursor;
    begin
      -- Initialize the cursor to the first element in the list.
      Current_Cursor := DLL.First(G.Nodes);

      -- Iterate through the list to find the node.
      while Current_Cursor /= DLL.No_Element loop
          -- Get the node from the cursor.
          Current_Node := DLL.Element(Current_Cursor);

          -- Compare the data of the node with the target data.
          if Ada.Strings.Unbounded.To_String(Data.Data) = Ada.Strings.Unbounded.To_String(Current_Node.Data.Data) then
            return Current_Node; -- Return the node if found.
          end if;

          -- Move to the next element in the list.
          Current_Cursor := DLL.Next(Current_Cursor);
      end loop;

      -- If the loop completes and the node is not found, return null.
      return null; -- Return null if not found.
    end Node_Exists;

    

  -- Adds a node to the graph without checking if it exists
   procedure Add_Node(G : in out Doubly_Linked_Lists; Data : in Node_Type) is
      New_Node : Node_Access;
   begin
      New_Node := new Node_Type'(Data);
      DLL.Append(G.Nodes, New_Node);
   end Add_Node;


  -- Adds an edge between two nodes if they both exist.
   procedure Add_Edge(G : in out Doubly_Linked_Lists; From : in Node_Type; To : in Node_Type) is
   From_Node, To_Node : Node_Access;
   Edge_Exists : Boolean := False;
   From_Iter, To_Iter : DLL.Cursor;
    begin
      From_Node := Node_Exists(G, From);
      To_Node := Node_Exists(G, To);

      if From_Node /= null and To_Node /= null then
          -- Check if the edge already exists from "From" to "To."
          From_Iter := DLL.Find(From_Node.Data.Edges, To_Node);
          Edge_Exists := From_Iter /= DLL.No_Element;

          -- If the edge doesn't exist, add it.
          if not Edge_Exists then
            DLL.Append(From_Node.Data.Edges, To_Node);
          end if;
      else
          Ada.Text_IO.Put_Line("Edge cannot be established because one or two of the nodes do not exist.");
      end if;
    end Add_Edge;



   -- Deletes an edge between two nodes.
   procedure Delete_Edge(G : in out Doubly_Linked_Lists; From : in Node_Type; To : in Node_Type) is
   From_Node, To_Node : Node_Access;
   From_Iter, To_Iter : DLL.Cursor;
    begin
      From_Node := Node_Exists(G, From);
      To_Node := Node_Exists(G, To);

      if From_Node /= null and To_Node /= null then
          -- Find the iterators for the "From" and "To" nodes.
          From_Iter := DLL.Find(From_Node.Data.Edges, To_Node);
          To_Iter := DLL.Find(To_Node.Data.Edges, From_Node);

          if From_Iter /= DLL.No_Element and To_Iter /= DLL.No_Element then
            -- Delete the elements using the iterators.
            DLL.Delete(From_Node.Data.Edges, From_Iter);
            DLL.Delete(To_Node.Data.Edges, To_Iter);

            -- Check if the "From" node has no more edges, and if so, delete it using an iterator.
            if DLL.Is_Empty(From_Node.Data.Edges) then
                for Node_Iter in DLL.Iterate(G.Nodes) loop
                  if DLL.Element(Node_Iter) = From_Node then
                      DLL.Delete(G.Nodes, Node_Iter);
                      exit; -- Exit the loop once the node is deleted.
                  end if;
                end loop;
            end if;

            -- Check if the "To" node has no more edges, and if so, delete it using an iterator.
            if DLL.Is_Empty(To_Node.Data.Edges) then
                for Node_Iter in DLL.Iterate(G.Nodes) loop
                  if DLL.Element(Node_Iter) = To_Node then
                      DLL.Delete(G.Nodes, Node_Iter);
                      exit; -- Exit the loop once the node is deleted.
                  end if;
                end loop;
            end if;
          else
            Ada.Text_IO.Put_Line("Nodes still have edges");
          end if;
      else
          Ada.Text_IO.Put_Line("Edge cannot be established because one or two of the nodes do not exist.");
      end if;
    end Delete_Edge;



  -- check if path exists between 2 nodes in graph
  function Path_Exists(G : in Doubly_Linked_Lists; From : in Node_Type; To : in Node_Type) return Boolean is
    function DFS(Current : Node_Access; Visited : DLL.List) return Boolean is
    begin
        -- Check if the "To" node has been reached.
        if Current = To then
          return True;  -- Path exists.
        end if;

        -- Mark the current node as visited.
        DLL.Append(Visited, Current);

        -- Recursively explore unvisited neighbor nodes.
        for E of DLL.Iterate(Current.Data.Edges) loop
          if not DLL.In(Visited, E) then
              return DFS(E, Visited);
          end if;
        end loop;

        return False;  -- No path found.
    end DFS;

    -- Use the modified Node_Exists function to check if both "From" and "To" nodes exist.
    From_Node := Node_Exists(G, From);
    To_Node := Node_Exists(G, To);

    if From_Node /= null and To_Node /= null then
        return DFS(From_Node, DLL.Empty_List);
    else
        -- Print a message if one or both of the nodes do not exist.
        Ada.Text_IO.Put_Line("Path cannot be established because one or two of the nodes do not exist.");
        return False;
    end if;
   end Path_Exists;



   procedure Print_Graph(G : in Graph) is
      Current_Node, Current_Edge : Node_Access;
   begin
      Ada.Text_IO.Put_Line("Graph:");
      for N of DLL.Iterate(G.Nodes) loop
         Ada.Text_IO.Put("Node with Data: ");
         Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(N.Data.Data));
         if not DLL.Is_Empty(N.Data.Edges) then
            Ada.Text_IO.Put("  Edges to: ");
            for E of DLL.Iterate(N.Data.Edges) loop
               Ada.Text_IO.Put(Ada.Strings.Unbounded.To_String(E.Data.Data) & " ");
            end loop;
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Print_Graph;

end Graph;