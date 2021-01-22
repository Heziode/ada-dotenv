with Ada.Environment_Variables;
with Dotenv;
with Print_Variable;

procedure Example_1 is
begin
   Dotenv.Config;

   Ada.Environment_Variables.Iterate (Print_Variable'Access);
end Example_1;
