with Ada.Strings.Unbounded;

generic
   type Argument_Names is (<>);
package Generic_Command_Line_Types is

   subtype Argument_Range is Argument_Names;
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   type Boolean_Array is array (Argument_Names) of Boolean;
   type Natural_Array is array (Argument_Names) of Natural;
   type Help_Array    is array (Argument_Names) of Unbounded_String;

end Generic_Command_Line_Types;
