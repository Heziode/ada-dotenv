with Ada.Environment_Variables;
with Print_Variable;
with Load_Environment_Variables;
with Ada.Text_IO;

procedure Example_3 is

begin
   Ada.Text_IO.Put_Line ("Start main");
   Ada.Environment_Variables.Iterate (Print_Variable'Access);
end Example_3;
