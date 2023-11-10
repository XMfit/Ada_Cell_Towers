# Project 3 of PLC 
A cell phone company is continually building and leasing communication towers. Each tower has one-way, direct communication links to other towers. The task is find if there is a communication link from one tower to another, possibly through other towers.

The input to the program is essentially a list of pairs of tower names. Each pair will appear on a single line and will contain distinct names. Some pairs represent one-way communication links between towers, and other pairs represent queries. Links and queries may be interspersed. Links, if they exist, may be removed.

All the communication links of the system are added one link at a time. Here is an example of establishing a link:

`Tower_A Tower_B.`

The establishment of the link is indicated by a period. There can be only one link between two towers. Here is an example of deleting a link:

`Tower_A Tower_B#`

From now on there is no link between the two towers, though a link could be reestablished at a later time.

A query asks if a communication channel is possible at the moment from one tower to another by any combination of one-way links. If another link is added or removed to the system later in the input, then the answer to the same query may be different. In the output, a plus sign (+) represents an affirmative answer; a minus sign (-) says there is no such channel.

Queries are distinguished from inserstions and deletions by ending in a question mark.

`Tower_A Tower_B?`

You are required to create your own graph package graph.ads graph.adb and name it Graph. The package can be generic or non-generic. The graph is to be a list of adjacency lists. You are required to use the generic, doubly-linked list package from the standard library. 

# Input / Output

 For the following input:
```
Tower_A   Tower_B.
 Tower_B Tower_C .
Tower_A Tower_C?     -- A query
Tower_B Tower_D.
 Tower_A  Tower_D ?  -- Another query
Tower_F Tower_E.
 Tower_D  Tower_B ?  -- A third query
xxxx  yyyy?          -- Unknown tower names
 Tower_D  Tower_B .
 Tower_D  Tower_B ?  -- Now there is a link
 Tower_D  Tower_B #
```
the output should be

```
+ Tower_A => Tower_C
+ Tower_A => Tower_D
- Tower_D => Tower_B
- xxxx => yyyy
+ Tower_D => Tower_B
```