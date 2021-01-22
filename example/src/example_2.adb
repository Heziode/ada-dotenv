with Ada.Environment_Variables;
with Dotenv;
with Print_Variable;

procedure Example_2 is
begin
   Dotenv.Config (Path        => "bin/.env.interpolation",
                  File_Form   => "wcem=8",
                  Overwrite   => True,
                  Debug       => True,
                  Interpolate => True);

   Ada.Environment_Variables.Iterate (Print_Variable'Access);
end Example_2;
