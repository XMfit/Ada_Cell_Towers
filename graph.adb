-- graph.adb data structure implementation (body) for string data type

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Double_Linked_Lists;

package body Graph is

  -- Use the DLL package for double linked lists from standart library
   package DLL is new Ada.Containers.Double_Linked_Lists;

  -- constructor for graph
   procedure Initialize(G : in out Graph) is
   begin
      -- Clear the list of nodes to initialize the graph.
      DLL.Clear(G.Nodes);
   end Initialize;


  -- Checks if a node with the given data exists in the graph and returns it
   function Node_Exists(G : in out Graph; Data : in Node_Type) return Node_Access is
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
   procedure Add_Node(G : in out Graph; Data : in Node_Type) is
      New_Node : Node_Access;
   begin
      New_Node := new Node_Type'(Data);
      DLL.Append(G.Nodes, New_Node);
   end Add_Node;


  -- Adds an edge between two nodes if they both exist.
   procedure Add_Edge(G : in out Graph; From : in Node_Type; To : in Node_Type) is
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
   procedure Delete_Edge(G : in out Graph; From : in Node_Type; To : in Node_Type) is
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
  function Path_Exists(G : in Graph; From : in Node_Type; To : in Node_Type) return Boolean is
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




















--  -- Generic_Doubly_Linked_List_Graph.adb
--  with Ada.Text_IO;
--  with Generic_Package_Containers.Doubly_Linked_Lists;  -- Include the Doubly_Linked_Lists package.

--  package body Generic_Doubly_Linked_List_Graph is
--     use Ada.Text_IO;

--     function Find_Node(G : Graph; Data : Node_Type) return Node_Access is
--     begin
--        for Node_Ptr in G.Nodes loop
--           declare
--              Node_A : Node_Access := Node_Type_Access(Node_Ptr.Element);
--           begin
--              if Node_A.Data = Data then
--                 return Node_A;
--              end if;
--           end;
--        end loop;
--        return null;
--     end Find_Node;

--     function DFS_Path_Exists(Start_Node : Node_Access; Target_Node : Node_Access; Visited : Node_Lists.List) return Boolean is
--     begin
--        if Start_Node = Target_Node then
--           return True;
--        elsif Start_Node /= null then
--           for Adj_Node_Ptr in Start_Node.Adjacent_Nodes loop
--              declare
--                 Adj_Node : Node_Access := Node_Type_Access(Adj_Node_Ptr.Element);
--              begin
--                 if not Adj_Node in Visited then
--                    Visited.Append(Adj_Node);
--                    if DFS_Path_Exists(Adj_Node, Target_Node, Visited) then
--                       return True;
--                    end if;
--                 end if;
--              end;
--           end loop;
--        end if;
--        return False;
--     end DFS_Path_Exists;

--     procedure Initialize(G : in out Graph) is
--     begin
--        G.Nodes := Node_Lists.New_List;
--     end Initialize;

--     function Node_Exists(G : in out Graph; Data : in Node_Type) return Boolean is
--     begin
--        declare
--           Node_To_Find : Node_Access := Find_Node(G, Data);
--        begin
--           if Node_To_Find = null then
--              -- If the node doesn't exist, add it
--              Add_Node(G, Data);
--              return False;
--           else
--              return True;
--           end if;
--        end;
--     end Node_Exists;

--     procedure Add_Node(G : in out Graph; Data : in Node_Type) is
--     begin
--        declare
--           New_Node : Node_Access := new Node;
--        begin
--           New_Node.Data := Data;
--           New_Node.Adjacent_Nodes := Node_Lists.New_List;
--           G.Nodes.Append(New_Node);
--        end;
--     end Add_Node;

--     procedure Add_Edge(G : in out Graph; From : in Node_Type; To : in Node_Type) is
--        From_Node : Node_Access := Find_Node(G, From);
--        To_Node : Node_Access := Find_Node(G, To);
--     begin
--        if From_Node /= null and To_Node /= null then
--           From_Node.Adjacent_Nodes.Append(To_Node);
--        else
--           Put_Line("One or both nodes not found. Edge not added.");
--        end if;
--     end Add_Edge;

--     procedure Delete_Edge(G : in out Graph; From : in Node_Type; To : in Node_Type) is
--        From_Node : Node_Access := Find_Node(G, From);
--        To_Node : Node_Access := Find_Node(G, To);
--     begin
--        if From_Node /= null and To_Node /= null then
--           From_Node.Adjacent_Nodes.Delete(To_Node);

--           -- Check if From_Node has no more adjacent nodes, delete it
--           if From_Node.Adjacent_Nodes.Is_Empty then
--              G.Nodes.Delete(From_Node);
--           end if;

--           -- Check if To_Node has no more adjacent nodes, delete it
--           if To_Node.Adjacent_Nodes.Is_Empty then
--              G.Nodes.Delete(To_Node);
--           end if;
--        else
--           Put_Line("Edge not found.");
--        end if;
--     end Delete_Edge;

--     function Path_Exists(G : in Graph; From : in Node_Type; To : in Node_Type) return Boolean is
--        From_Node : Node_Access := Find_Node(G, From);
--        To_Node : Node_Access := Find_Node(G, To);
--        Visited_Nodes : Node_Lists.List := Node_Lists.New_List;
--     begin
--        if From_Node /= null and To_Node /= null then
--           return DFS_Path_Exists(From_Node, To_Node, Visited_Nodes);
--        else
--           Put_Line("Nodes not found.");
--           return False;
--        end if;
--     end Path_Exists;

--     procedure Print_Graph(G : in Graph) is
--     begin
--        for Node_Ptr in G.Nodes loop
--           declare
--              Node_A : Node_Access := Node_Type_Access(Node_Ptr.Element);
--           begin
--              Put("Node: " & Node_A.Data & " -> Adjacent Nodes: ");
--              for Adj_Node_Ptr in Node_A.Adjacent_Nodes loop
--                 Put(Node_Type_Access(Adj_Node_Ptr.Element).Data & " ");
--              end loop;
--              New_Line;
--           end;
--        end loop;
--     end Print_Graph;

--  end Generic_Doubly_Linked_List_Graph;








--  -- call Text_IO package
--  with Ada.Text_IO;
--  package body GenGraph is
--     use Ada.Text_IO;

--    -- go through the list of nodes and find if the data belongs to an existing node
--     function Find_Node(G : Graph; Data : Node_Type) return Node_Access is
--     begin
--        -- loop through nodes
--        for Node_Ptr in G.Nodes loop
--           declare
--              Node_A : Node_Access := Node_Type_Access(Node_Ptr.Element);
--           begin
--              -- if data exist return node
--              if Node_A.Data = Data then
--                 return Node_A;
--              end if;
--           end;
--        end loop;
--        return null;
--     end Find_Node;
  
--    -- execute Depth First Search to find out if a path exist between existing nodes
--     function DFS_Path_Exists(Start_Node : Node_Access; Target_Node : Node_Access; Visited : Node_List) return Boolean is
--     begin
--        -- if start node is end node then path exist (from A to A)
--        if Start_Node = Target_Node then
--           return True;
--        -- otherwise look at adjacent node and go down that path 
--        -- while keeping track of the visited node to remember path
--        -- make recursive call until path has been defined
--        elsif Start_Node /= null then
--           for Adj_Node_Ptr in Start_Node.Adjacent_Nodes loop
--              declare
--                 Adj_Node : Node_Access := Node_Type_Access(Adj_Node_Ptr.Element);
--              begin
--                 if not Adj_Node in Visited then
--                    Visited.Append(Adj_Node);
--                    if DFS_Path_Exists(Adj_Node, Target_Node, Visited) then
--                       return True;
--                    end if;
--                 end if;
--              end;
--           end loop;
--        end if;
--        return False;
--     end DFS_Path_Exists;

--    -- constructor for the graph data structure
--     procedure Initialize(G : in out Graph) is
--     begin
--        G.Nodes := new Node_List;
--     end Initialize;

--    -- create a node and initialize list of adjacent nodes if it does not exist 
--     procedure Add_Node(G : in out Graph; Data : in Node_Type) is
--      Existing_Node : Node_Access := Find_Node(G, Data);
--     begin
--        if Existing_Node = null then
--            declare
--              New_Node : Node_Access := new Node;
--            begin
--              New_Node.Data := Data;
--              New_Node.Adjacent_Nodes := new Node_List;
--              G.Nodes.Append(New_Node);
--            end;
--        else
--            Put_Line("Node " & Data & " already exists in the graph.");
--        end if;
--     end Add_Node;

--    -- create a link between 2 nodes if they both exist
--     procedure Add_Edge(G : in out Graph; From : in Node_Type; To : in Node_Type) is
--        From_Node : Node_Access := Find_Node(G, From);
--        To_Node : Node_Access := Find_Node(G, To);
--     begin
--        if From_Node /= null and To_Node /= null then
--           From_Node.Adjacent_Nodes.Append(To_Node);
--        else
--           Put_Line("One or both nodes not found. Edge not added.");
--        end if;
--     end Add_Edge;

--    -- check if both nodes exist and if they do delete the edge
--    -- then check if any adjacent link still exist if not delete the nodes
--     procedure Delete_Edge(G : in out Graph; From : in Node_Type; To : in Node_Type) is
--     From_Node : Node_Access := Find_Node(G, From);
--     To_Node : Node_Access := Find_Node(G, To);
--     begin
--      if From_Node /= null and To_Node /= null then
--          From_Node.Adjacent_Nodes.Delete(To_Node);
        
--          -- Check if From_Node has no more adjacent nodes, delete it
--          if From_Node.Adjacent_Nodes.Is_Empty then
--            G.Nodes.Delete(From_Node);
--          end if;
        
--          -- Check if To_Node has no more adjacent nodes, delete it
--          if To_Node.Adjacent_Nodes.Is_Empty then
--            G.Nodes.Delete(To_Node);
--          end if;
--      else
--          Put_Line("Edge not found.");
--      end if;
--     end Delete_Edge;

--    -- check if both nodes exists then perform DFS between the two node to find if a path exists
--     function Path_Exists(G : in Graph; From : in Node_Type; To : in Node_Type) return Boolean is
--        From_Node : Node_Access := Find_Node(G, From);
--        To_Node : Node_Access := Find_Node(G, To);
--        Visited_Nodes : Node_List := new Node_List;
--     begin
--        if From_Node /= null and To_Node /= null then
--           return DFS_Path_Exists(From_Node, To_Node, Visited_Nodes);
--        else
--           Put_Line("Nodes not found.");
--           return False;
--        end if;
--     end Path_Exists;

--     procedure Print_Graph(G : in Graph) is
--     begin
--        for Node_Ptr in G.Nodes loop
--           declare
--              Node_A : Node_Access := Node_Type_Access(Node_Ptr.Element);
--           begin
--              Put("Node: " & Node_A.Data & " -> Adjacent Nodes: ");
--              for Adj_Node_Ptr in Node_A.Adjacent_Nodes loop
--                 Put(Node_Type_Access(Adj_Node_Ptr.Element).Data & " ");
--              end loop;
--              New_Line;
--           end;
--        end loop;
--     end Print_Graph;

--  end GenGraph;
