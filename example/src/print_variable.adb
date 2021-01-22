with Ada.Text_IO;

procedure Print_Variable (Name, Value : String) is
begin
   Ada.Text_IO.Put_Line (Name & ": " & Value);
end Print_Variable;
