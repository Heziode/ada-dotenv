with Ada.Text_IO;
with Dotenv;

package body Load_Environment_Variables is
begin
   Ada.Text_IO.Put_Line ("Load Environment Variables");
   Dotenv.Config;
end Load_Environment_Variables;
