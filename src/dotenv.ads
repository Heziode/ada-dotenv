with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

--  @description
--  This package provide a set of sub-programs to parse and configure environment variable from a file.
--
--  Environment variables in file shall have the following format: Key=Value
--  The value can be single quoted or doubly quoted. If this is the case, they are removed in the result value.
--  Values that contains "\n" (in string format) are converted to Line Feed if it surronded by double quoted.
package Dotenv is
   use Ada.Strings.Unbounded, Ada.Text_IO;

   function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type is (Ada.Strings.Hash (To_String (Key)));

   function Equivalent_Key (Left, Right : Unbounded_String) return Boolean is (Left = Right);

   package Environment_Variable_Map is new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                                                       Element_Type    => Unbounded_String,
                                                                       Hash            => Hash,
                                                                       Equivalent_Keys => Equivalent_Key);
   use Environment_Variable_Map;

   DEFAULT_DEBUG_VALUE     : constant Boolean := False;

   --  The parameter "wcem=8" indicates that we want to open a file in
   --  UTF-8 format (which can therefore contain accents)
   DEFAULT_FILE_FORM_VALUE : constant String  := "wcem=8";

   DEFAULT_FILENAME        : constant String  := ".env";

   DEFAULT_OVERWRITE_VALUE : constant Boolean := False;

   --  For example, if we have the following environment variables:
   --  - FIRSTNAME=John
   --  - LASTNAME=Doe
   --  - HELLO=Hello ${FIRSTNAME} $LASTNAME
   --  Then the final value of HELLO will be "Hello John Doe" if Interpolation is True
   DEFAULT_INTERPOLATE_VALUE : constant Boolean := False;

   DOTENV_CONFIG_DEBUG       : constant String  := "DOTENV_CONFIG_DEBUG";
   DOTENV_CONFIG_FILE_FORM   : constant String  := "DOTENV_CONFIG_FILE_FORM";
   DOTENV_CONFIG_INTERPOLATE : constant String  := "DOTENV_CONFIG_INTERPOLATE";
   DOTENV_CONFIG_OVERWRITE   : constant String  := "DOTENV_CONFIG_OVERWRITE";
   DOTENV_CONFIG_PATH        : constant String  := "DOTENV_CONFIG_PATH";

   --  Read a file to find environment variable.
   --  @param File File to read
   --  @param Debug True to show debug information, False otherwise. Default: DEFAULT_DEBUG_VALUE
   --  @return Return a map that contains in key, the environment name, and in value the environment value.
   function Parse (File : File_Type; Debug : Boolean := DEFAULT_DEBUG_VALUE) return Map;

   --  Read a file to find environment variable.
   --  @param Path File to read
   --  @param Debug True to show debug information, False otherwise. Default to: DEFAULT_DEBUG_VALUE
   --  @return Return a map that contains in key, the environment name, and in value the environment value.
   function Parse (Path : String; Debug : Boolean := DEFAULT_DEBUG_VALUE) return Map;

   --  Interpolate Value using elements provided by Env_Var and Environment Variable.
   --  For example, if we have the following environment variables:
   --  - FIRSTNAME=John
   --  - LASTNAME=Doe
   --  - HELLO=Hello ${FIRSTNAME} $LASTNAME
   --  Then the final value of HELLO will be: Hello John Doe
   --  @param Env_Var Contains element that can be used in interpolation
   --  @param Value Value to interpolate
   --  @param Overwrite True to overwrite existing environment variable, False otherwise.
   --    Default: DEFAULT_OVERWRITE_VALUE
   --    Example: if we have the following environment variable:
   --    - GREETING=Hello
   --    - FIRSTNAME=John
   --    Env_Var content:
   --    - FIRSTNAME=Jane
   --    - LASTNAME=Doe
   --    And the Value: $GREETING ${FIRSTNAME} $LASTNAME
   --    If Overwrite is True, then the return will be: Hello Jane Doe
   --    If Overwrite is False, then the return will be: Hello John Doe
   --  @return Retuns the interpolated value
   function Interpolate_Element (Env_Var : Map; Value : String; Overwrite : Boolean) return String;

   --  Interpolate all values of the given map
   --  @param Overwrite True to overwrite existing environment variable, False otherwise.
   --    Default: DEFAULT_OVERWRITE_VALUE
   --    Example: if we have the following environment variable:
   --    - GREETING=Hello
   --    - FIRSTNAME=John
   --    Env_Var content:
   --    - FIRSTNAME=Jane
   --    - LASTNAME=Doe
   --    And the Value: $GREETING ${FIRSTNAME} $LASTNAME
   --    If Overwrite is True, then the return will be: Hello Jane Doe
   --    If Overwrite is False, then the return will be: Hello John Doe
   procedure Interpolate (Overwrite : Boolean;
                          Debug     : Boolean;
                          Env_Var   : in out Map);

   --  Configure environment variables.
   --  @param Overwrite True to overwrite existing environment variable, False otherwise.
   --    Default: DEFAULT_OVERWRITE_VALUE
   --  @param Debug True to show debug information, False otherwise. Default: DEFAULT_DEBUG_VALUE
   --  @param Path path to the environment file. If no Path provided, this procedure try to solve the name in the
   --    following way:
   --    - If DOTENV_CONFIG_PATH environment variable is set, it use it
   --    - Find a '.env' file in the current working directory
   --    - Find a '.env' file in the execution directory
   --  @param File_Form Set Form to file, like encoding, etc. Default: DEFAULT_FILE_FORM_VALUE
   --  @exception Ada.Directories.Name_Error Throwed when no valid path found for environment variable file.
   procedure Config (Overwrite   : Boolean;
                     Debug       : Boolean;
                     Interpolate : Boolean;
                     Path        : String  := "";
                     File_Form   : String  := "");

   --  Configure environment variables.
   --  This procedure check if DOTENV_CONFIG_OVERWRITE is set, otherwise it uses DEFAULT_OVERWRITE_VALUE.
   --  If the value of DOTENV_CONFIG_OVERWRITE is invalid, it uses ;
   --  This procedure check if DOTENV_CONFIG_DEBUG is set, otherwise it uses DEFAULT_DEBUG_VALUE.
   --  If the value of DEFAULT_DEBUG_VALUE is invalid, it uses DEFAULT_DEBUG_VALUE;
   --  @param Path path to the environment file. If no Path provided, this procedure try to solve the name in the
   --    following way:
   --    - If DOTENV_CONFIG_PATH environment variable is set, it use it
   --    - Find a '.env' file in the current working directory
   --    - Find a '.env' file in the execution directory
   --  @param File_Form Set Form to file, like encoding, etc. Default: DEFAULT_FILE_FORM_VALUE
   --  @exception Ada.Directories.Name_Error Throwed when no valid path found for environment variable file.
   procedure Config (Path      : String  := "";
                     File_Form : String  := "");

   --  Configure environment variables.
   --  This procedure check if DOTENV_CONFIG_DEBUG is set, otherwise it uses DEFAULT_DEBUG_VALUE.
   --  If the value of DEFAULT_DEBUG_VALUE is invalid, it uses DEFAULT_DEBUG_VALUE;
   --  @param Overwrite True to overwrite existing environment variable, False otherwise.
   --    Default: DEFAULT_OVERWRITE_VALUE
   --  @param Path path to the environment file. If no Path provided, this procedure try to solve the name in the
   --    following way:
   --    - If DOTENV_CONFIG_PATH environment variable is set, it use it
   --    - Find a '.env' file in the current working directory
   --    - Find a '.env' file in the execution directory
   --  @param File_Form Set Form to file, like encoding, etc. Default: DEFAULT_FILE_FORM_VALUE
   --  @exception Ada.Directories.Name_Error Throwed when no valid path found for environment variable file.
   procedure Config_Overwrite (Overwrite : Boolean;
                               Path      : String  := "";
                               File_Form : String  := "");

   --  Configure environment variables.
   --  This procedure check if DOTENV_CONFIG_OVERWRITE is set, otherwise it uses DEFAULT_OVERWRITE_VALUE.
   --  If the value of DOTENV_CONFIG_OVERWRITE is invalid, it uses DEFAULT_OVERWRITE_VALUE;
   --  @param Debug True to show debug information, False otherwise. Default: DEFAULT_DEBUG_VALUE
   --  @param Path path to the environment file. If no Path provided, this procedure try to solve the name in the
   --    following way:
   --    - If DOTENV_CONFIG_PATH environment variable is set, it use it
   --    - Find a '.env' file in the current working directory
   --    - Find a '.env' file in the execution directory
   --  @param File_Form Set Form to file, like encoding, etc. Default: DEFAULT_FILE_FORM_VALUE
   --  @exception Ada.Directories.Name_Error Throwed when no valid path found for environment variable file.
   procedure Config_Debug (Debug     : Boolean;
                           Path      : String  := "";
                           File_Form : String  := "");

   --  Configure environment variables.
   --  This procedure check if DOTENV_CONFIG_OVERWRITE is set, otherwise it uses DEFAULT_OVERWRITE_VALUE.
   --  If the value of DOTENV_CONFIG_OVERWRITE is invalid, it uses DEFAULT_OVERWRITE_VALUE;
   --  @param Interpolate True to interpolate values, False otherwise. Default: DEFAULT_INTERPOLATE_VALUE
   --    For example, if we have the following environment variables:
   --    - FIRSTNAME=John
   --    - LASTNAME=Doe
   --    - HELLO=Hello ${FIRSTNAME} $LASTNAME
   --    Then the value of final HELLO will be: Hello John Doe
   --  @param Path path to the environment file. If no Path provided, this procedure try to solve the name in the
   --    following way:
   --    - If DOTENV_CONFIG_PATH environment variable is set, it use it
   --    - Find a '.env' file in the current working directory
   --    - Find a '.env' file in the execution directory
   --  @param File_Form Set Form to file, like encoding, etc. Default: DEFAULT_FILE_FORM_VALUE
   --  @exception Ada.Directories.Name_Error Throwed when no valid path found for environment variable file.
   procedure Config_Interpolate (Interpolate : Boolean;
                                 Path        : String  := "";
                                 File_Form   : String  := "");
end Dotenv;
