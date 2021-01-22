pragma Ada_2012;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;
with GNAT.Regpat;

package body Dotenv is

   RE_INI_KEY_VAL       : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("^\s*([\w.-]+)\s*=\s*(.*)?\s*$");
   RE_EXPANDED_VAL      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("(.?\$(?:{(?:[a-zA-Z0-9_]+)?}|(?:[a-zA-Z0-9_]+)?))");
   RE_EXPANDED_PART_VAL : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("(.?)\${?([a-zA-Z0-9_]+)?}?");

   --------------------------------------------------------------------------------------------------------------------
   --                                               Inner sub-programs                                               --
   --------------------------------------------------------------------------------------------------------------------

   --  Print a messange on the standard output.
   --  Used for debug purpose.
   --  @param Message The message to print
   procedure Log (Message : String);

   --  Open a file. By default, if file does not exist, it create it.
   --  @param File The file container
   --  @param Mode Mode to open the file
   --  @param Path Location of the file
   --  @param File_Form "Form" to pass to the file
   --  @param Auto True if auto create the file if it does not exist, false otherwise
   procedure Open_File (File      : in out File_Type;
                        Mode      : File_Mode;
                        Path      : String;
                        File_Form : String  := DEFAULT_FILE_FORM_VALUE;
                        Auto      : Boolean := False);

   function Get_Boolean_Environment_Variable (Name : String; Default_Value : Boolean) return Boolean;
   pragma Inline (Get_Boolean_Environment_Variable);

   --  Get a Key and Value from a Map, and set in Environment Variable.
   --  @param C Cursor to a map
   procedure Add_Environment_Variable (C : Cursor; Debug, Overwrite : Boolean);

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Put_Line ("[dotenv][DEBUG] " & Message);
   end Log;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File (File      : in out File_Type;
                        Mode      : File_Mode;
                        Path      : String;
                        File_Form : String  := DEFAULT_FILE_FORM_VALUE;
                        Auto      : Boolean := False)
   is
      use Ada.Directories;
   begin
      if Exists (Path) then
         Open (File, Mode, Path, File_Form);
      else
         if Auto then
            Create (File, Mode, Path, File_Form);
         else
            raise Ada.Directories.Name_Error;
         end if;
      end if;
   end Open_File;

   --------------------------------------
   -- Get_Boolean_Environment_Variable --
   --------------------------------------

   function Get_Boolean_Environment_Variable (Name : String; Default_Value : Boolean) return Boolean is
   begin
      if not Ada.Environment_Variables.Exists (Name) then
         return Default_Value;
      end if;

      begin
         return Boolean'Value (Ada.Environment_Variables.Value (Name));
      exception
         when Constraint_Error =>
            return Default_Value;
      end;
   end Get_Boolean_Environment_Variable;

   ------------------------------
   -- Add_Environment_Variable --
   ------------------------------

   procedure Add_Environment_Variable (C : Cursor; Debug, Overwrite : Boolean) is
      Key   : constant String := To_String (Environment_Variable_Map.Key (C));
      Value : constant String := To_String (Element (C));
   begin
      if not Ada.Environment_Variables.Exists (Key) or Overwrite then
         Ada.Environment_Variables.Set (Key, Value);
      elsif Debug then
         Log (Key & " is already defined in environment variable and will not be overwritten.");
      end if;
   end Add_Environment_Variable;

   --------------------------------------------------------------------------------------------------------------------
   --                                               Outer sub-programs                                               --
   --------------------------------------------------------------------------------------------------------------------

   -----------
   -- Parse --
   -----------

   function Parse (File : File_Type; Debug : Boolean := DEFAULT_DEBUG_VALUE) return Map is
      use GNAT.Regpat;

      --  Given a Line, match the Key=Value pattern.
      --  @param Search_In Line to analyze
      --  @param Key Output key found
      --  @param Value Output value found
      --  @param Found True if the pattern is found, False otherwise
      procedure Search_For_Pattern (Search_In  : String;
                                    Key, Value : out Unbounded_String;
                                    Found      : out Boolean);

      --  Replace a Pattern by Replacement in S.
      --  @param S String to process
      --  @param Pattern Pattern to replace
      --  @param Replacement New value
      --
      --  example: if S is "Mary had a XX lamb", then String_Replace(S, "X", "little");
      --          will turn S into "Mary had a littlelittle lamb"
      --          and String_Replace(S, "Y", "small"); will not change S
      procedure String_Replace (S : in out Unbounded_String; Pattern, Replacement : String);

      ------------------------
      -- Search_For_Pattern --
      ------------------------

      procedure Search_For_Pattern (Search_In  : String;
                                    Key, Value : out Unbounded_String;
                                    Found      : out Boolean)
      is
         Result : Match_Array (0 .. 2);
      begin
         Match (RE_INI_KEY_VAL, Search_In, Result);
         Found := not (Result (1) = No_Match);
         if Found then
            Key   := To_Unbounded_String (Search_In (Result (1).First .. Result (1).Last));
            if not (Result (2) = No_Match) then
               Value := To_Unbounded_String (Search_In (Result (2).First .. Result (2).Last));
            else
               Value := To_Unbounded_String ("");
            end if;
         end if;
      end Search_For_Pattern;

      --------------------
      -- String_Replace --
      --------------------

      procedure String_Replace (S : in out Unbounded_String; Pattern, Replacement : String) is
         Index : Natural;
      begin
         loop
            Index := Ada.Strings.Unbounded.Index (Source => S, Pattern => Pattern);
            exit when Index = 0;
            Replace_Slice (Source => S,
                           Low    => Index,
                           High   => Index + Pattern'Length - 1,
                           By     => Replacement);
         end loop;
      end String_Replace;

      Line   : Unbounded_String := Null_Unbounded_String;
      Result : Map              := Empty_Map;
      Idx    : Natural          := 0;
   begin --  Parse
      loop
         Idx := Idx + 1;
         exit when End_Of_File (File);

         if End_Of_Line (File) then
            Skip_Line (File);
         elsif End_Of_Page (File) then
            Skip_Page (File);
         else
            Line := To_Unbounded_String (Get_Line (File));
         end if;

         declare
            Key   : Unbounded_String := Null_Unbounded_String;
            Value : Unbounded_String := Null_Unbounded_String;
            Found : Boolean          := False;
            Is_Double_Quoted : Boolean := False;
            Is_Single_Quoted : Boolean := False;
         begin
            Search_For_Pattern (Search_In           => To_String (Line),
                                Key                 => Key,
                                Value               => Value,
                                Found               => Found);
            if not Found then
               if Debug then
                  Log ("did not match key and value when parsing line "
                       & To_String (Trim (To_Unbounded_String (Natural'Image (Idx)), Ada.Strings.Both))
                       & ": " & To_String (Line));
               end if;
               goto CONTINUE_NEXT_LINE;
            end if;

            if Length (Value) > 0 then
               Is_Double_Quoted := Element (Value, 1) = '"' and Element (Value, Length (Value)) = '"';
               Is_Single_Quoted := Element (Value, 1) = ''' and Element (Value, Length (Value)) = ''';

               --- If single or double quoted, remove quotes
               if Is_Double_Quoted or Is_Single_Quoted then
                  Value := Unbounded_Slice (Value, 2, Length (Value) - 1);

                  if Is_Double_Quoted then
                     String_Replace (S           => Value,
                                     Pattern     => "\n",
                                     Replacement => ASCII.LF & "");
                  end if;
               else
                  Value := Trim (Value, Ada.Strings.Both);
               end if;
            end if;

            if Result.Contains (Key) then
               Result.Replace (Key, Value);
            else
               Result.Insert (Key, Value);
            end if;
         end;

         <<CONTINUE_NEXT_LINE>>
      end loop;
      return Result;
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse (Path : String; Debug : Boolean := DEFAULT_DEBUG_VALUE) return Map is
      File   : File_Type;
      Result : Map := Empty_Map;
   begin
      Open_File (File => File,
                 Mode => In_File,
                 Path => Path);
      Result := Parse (File, Debug);
      Close (File);
      return Result;
   end Parse;

   -------------------------
   -- Interpolate_Element --
   -------------------------

   function Interpolate_Element (Env_Var : Map; Value : String; Overwrite : Boolean) return String is
      use GNAT.Regpat;

      --  Return the value corresponding to the Key. It looking in Env_Var and in Environment Variable.
      --    Example: if we have the following environment variable:
      --    - FIRSTNAME=John
      --    Env_Var content:
      --    - FIRSTNAME=Jane
      --    If Overwrite is True, then the return will be: Jane
      --    If Overwrite is False, then the return will be: John
      --  @param Key Key of the paradize
      --  @return Return the value corresponding to the Key or an empty String if Key is not found in Env_Var nor in
      --    environments variables.
      function Get_Value_Of (Key : String) return String;

      ------------------
      -- Get_Value_Of --
      ------------------

      function Get_Value_Of (Key : String) return String is
      begin
         if Env_Var.Contains (To_Unbounded_String (Key)) then
            if Overwrite and Ada.Environment_Variables.Exists (Key)  then
               return Ada.Environment_Variables.Value (Key);
            else
               return To_String (Env_Var.Element (To_Unbounded_String (Key)));
            end if;
         elsif Ada.Environment_Variables.Exists (Key) then
            return Ada.Environment_Variables.Value (Key);
         end if;
         return "";
      end Get_Value_Of;

      Found                   : Boolean          := False;
      Match, Str              : Unbounded_String := Null_Unbounded_String;
      Last_Index, First, Last : Integer          := 0;
      Result_Array            : Match_Array (0 .. 1);
   begin --  Interpolate_Element
      if Value'Length = 0 then
         return Value;
      end if;

      Str := To_Unbounded_String (Value);

      Loop_Over_Values : loop
         GNAT.Regpat.Match (RE_EXPANDED_VAL, To_String (Str), Result_Array, Last_Index, Length (Str));
         Found := not (Result_Array (1) = No_Match);

         if Found then
            Match := Unbounded_Slice (Source => Str,
                                      Low    => Result_Array (1).First,
                                      High   => Result_Array (1).Last);
            First      := Result_Array (1).First;
            Last       := Result_Array (1).Last;
            Last_Index := Result_Array (1).Last;
         end if;

         exit Loop_Over_Values when not Found or Last_Index >= Length (Str);

         Match_Parts : declare
            Part_Result       : Unbounded_String := Null_Unbounded_String;
            Part_Search_In    : constant String  := To_String (Match);
            Part_Result_Array : Match_Array (0 .. 2);
            Prefix            : Unbounded_String := Null_Unbounded_String;
            Origin_Size       : Natural;
         begin
            GNAT.Regpat.Match (RE_EXPANDED_PART_VAL, Part_Search_In, Part_Result_Array);
            Prefix := To_Unbounded_String (Part_Search_In (Part_Result_Array (1).First .. Part_Result_Array (1).Last));

            if Prefix = "\"
            then
               Replace_Slice (Source => Str,
                              Low    => First + Part_Result_Array (1).First - 1,
                              High   => First + Part_Result_Array (1).Last - 1,
                              By     => "");
            else
               Part_Result := To_Unbounded_String
                 (Get_Value_Of (Part_Search_In (Part_Result_Array (2).First .. Part_Result_Array (2).Last)));
               Origin_Size := Length (Part_Result);
               Part_Result := To_Unbounded_String (Interpolate_Element (Env_Var, To_String (Part_Result), Overwrite));

               Replace_Slice (Source => Str,
                              Low    => First + Length (Prefix),
                              High   => Last,
                              By     => To_String (Part_Result));

               Last_Index := Length (Part_Result) - Origin_Size;
            end if;
         end Match_Parts;

      end loop Loop_Over_Values;

      return To_String (Str);
   end Interpolate_Element;

   -----------------
   -- Interpolate --
   -----------------

   procedure Interpolate (Overwrite : Boolean;
                          Debug     : Boolean;
                          Env_Var   : in out Map)
   is
      --  Expand a value.
      --  For example, if we have the following environment variables:
      --  - FIRSTNAME=John
      --  - LASTNAME=Doe
      --  - HELLO=Hello ${FIRSTNAME} $LASTNAME
      --  Then the value will be: Hello John Doe
      --  @param C Cursor to a map
      procedure Interpolation (C : Cursor);

      -------------------
      -- Interpolation --
      -------------------

      procedure Interpolation (C : Cursor) is
      begin
         Env_Var.Replace (Key      => Environment_Variable_Map.Key (C),
                          New_Item => To_Unbounded_String
                            (Interpolate_Element (Env_Var, To_String (Element (C)), Overwrite)));
         Add_Environment_Variable (C         => C,
                                   Debug     => Debug,
                                   Overwrite => Overwrite);
      end Interpolation;
   begin --  Interpolate
      Env_Var.Iterate (Interpolation'Access);
   end Interpolate;

   ------------
   -- Config --
   ------------

   procedure Config (Overwrite   : Boolean;
                     Debug       : Boolean;
                     Interpolate : Boolean;
                     Path        : String  := "";
                     File_Form   : String  := "")
   is
      use Ada.Command_Line, Ada.Directories;

      Result : Map := Empty_Map;

      --  Get a Key and Value from a Map, and set in Environment Variable.
      --  @param C Cursor to a map
      procedure Add_Environment_Variable (C : Cursor);

      ------------------------------
      -- Add_Environment_Variable --
      ------------------------------

      procedure Add_Environment_Variable (C : Cursor) is
      begin
         Add_Environment_Variable (C         => C,
                                   Debug     => Debug,
                                   Overwrite => Overwrite);
      end Add_Environment_Variable;

      File_Path : Unbounded_String := Null_Unbounded_String;
      Form      : Unbounded_String := To_Unbounded_String (File_Form);
      File      : File_Type;
   begin --  Config
      --  Get the config file that will be used
      if Path'Length > 0 then
         if Exists (Path) then
            File_Path := To_Unbounded_String (Path);
         else
            raise Ada.Directories.Name_Error with "The following environment variable file does not exists: " & Path;
         end if;
      elsif Ada.Environment_Variables.Exists (DOTENV_CONFIG_PATH)
        and then Ada.Environment_Variables.Value (DOTENV_CONFIG_PATH)'Length > 0
      then
         if Exists (Ada.Environment_Variables.Value (DOTENV_CONFIG_PATH)) then
            File_Path := To_Unbounded_String (Ada.Environment_Variables.Value (DOTENV_CONFIG_PATH));
         else
            raise Ada.Directories.Name_Error with DOTENV_CONFIG_PATH & " environment variable provide a path to '"
              & Ada.Environment_Variables.Value (DOTENV_CONFIG_PATH) & "' but there is no file at this location.";
         end if;
      elsif Exists (Compose (Current_Directory, DEFAULT_FILENAME)) then
         File_Path := To_Unbounded_String (Compose (Current_Directory, DEFAULT_FILENAME));
      elsif Exists (Compose (Containing_Directory (Command_Name), DEFAULT_FILENAME)) then
         File_Path := To_Unbounded_String (Compose (Containing_Directory (Command_Name), DEFAULT_FILENAME));
      else
         --  No '.env' file found
         raise Ada.Directories.Name_Error with "No '.env' file provived and there is no '.env' file in '" &
           Current_Directory & "' nor in '" & Containing_Directory (Command_Name) & "'";
      end if;

      --  Get configuration via environment variable

      --  File Form
      if File_Form'Length > 0 then
         Form := To_Unbounded_String (File_Form);
      elsif Ada.Environment_Variables.Exists (DOTENV_CONFIG_FILE_FORM) then
         Form := To_Unbounded_String (Ada.Environment_Variables.Value (DOTENV_CONFIG_FILE_FORM));
      else
         Form := To_Unbounded_String (DEFAULT_FILE_FORM_VALUE);
      end if;

      --  Load file and parse content
      Open_File (File      => File,
                 Mode      => In_File,
                 Path      => To_String (File_Path),
                 File_Form => To_String (Form));
      Result := Parse (File, Debug);
      Close (File);

      --  Then, set environment variable
      if Interpolate then
         Dotenv.Interpolate (Overwrite => Overwrite,
                             Debug     => Debug,
                             Env_Var   => Result);
      else
         Result.Iterate (Add_Environment_Variable'Access);
      end if;
   end Config;

   ------------
   -- Config --
   ------------

   procedure Config (Path      : String  := "";
                     File_Form : String  := "")
   is
   begin
      Config (Overwrite   => Get_Boolean_Environment_Variable (DOTENV_CONFIG_OVERWRITE, DEFAULT_OVERWRITE_VALUE),
              Debug       => Get_Boolean_Environment_Variable (DOTENV_CONFIG_DEBUG, DEFAULT_DEBUG_VALUE),
              Interpolate => Get_Boolean_Environment_Variable (DOTENV_CONFIG_INTERPOLATE, DEFAULT_DEBUG_VALUE),
              Path        => Path,
              File_Form   => File_Form);
   end Config;

   ----------------------
   -- Config_Overwrite --
   ----------------------

   procedure Config_Overwrite (Overwrite : Boolean;
                               Path      : String  := "";
                               File_Form : String  := "")
   is
   begin
      Config (Overwrite   => Overwrite,
              Debug       => Get_Boolean_Environment_Variable (DOTENV_CONFIG_DEBUG, DEFAULT_DEBUG_VALUE),
              Interpolate => Get_Boolean_Environment_Variable (DOTENV_CONFIG_INTERPOLATE, DEFAULT_DEBUG_VALUE),
              Path        => Path,
              File_Form   => File_Form);
   end Config_Overwrite;

   ------------------
   -- Config_Debug --
   ------------------

   procedure Config_Debug (Debug     : Boolean;
                           Path      : String  := "";
                           File_Form : String  := "")
   is
   begin
      Config (Overwrite   => Get_Boolean_Environment_Variable (DOTENV_CONFIG_OVERWRITE, DEFAULT_OVERWRITE_VALUE),
              Debug       => Debug,
              Interpolate => Get_Boolean_Environment_Variable (DOTENV_CONFIG_INTERPOLATE, DEFAULT_DEBUG_VALUE),
              Path        => Path,
              File_Form   => File_Form);
   end Config_Debug;

   ------------------
   -- Config_Debug --
   ------------------

   procedure Config_Interpolate (Interpolate : Boolean;
                                 Path        : String  := "";
                                 File_Form   : String  := "")
   is
   begin
      Config (Overwrite   => Get_Boolean_Environment_Variable (DOTENV_CONFIG_OVERWRITE, DEFAULT_OVERWRITE_VALUE),
              Debug       => Get_Boolean_Environment_Variable (DOTENV_CONFIG_DEBUG, DEFAULT_DEBUG_VALUE),
              Interpolate => Interpolate,
              Path        => Path,
              File_Form   => File_Form);
   end Config_Interpolate;

end Dotenv;
