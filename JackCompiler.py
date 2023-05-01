import sys
from enum import Enum, auto
import os
import unicodedata

class TokenKind(Enum):
    """ Enum of possible Tokens in Jack """
    KEYWORD = auto()
    IDENTIFIER = auto()
    SYMBOL = auto()
    INT_CONST = auto()
    STR_CONST = auto()
    KEYWORD_CONST = auto()

class Token():
    """ Data class for tokens in Jack """
    def __init__(self, tokenKind, value):
        # Tokens are a TokenKind and their associated value.
        # EG KEYWORD, let
        self.tokenKind = tokenKind
        self.value = value
    
    def __str__(self):
        final_str = ""
        if self.tokenKind == TokenKind.KEYWORD:
            final_str = f"KEYWORD: {self.value}"
        elif self.tokenKind == TokenKind.IDENTIFIER:
            final_str = f"IDENTIFIER: {self.value}"
        elif self.tokenKind == TokenKind.INT_CONST:
            final_str = f"INTEGER: {self.value}"
        elif self.tokenKind == TokenKind.SYMBOL:
            final_str = f"SYMBOL: {self.value}"
        elif self.tokenKind == TokenKind.STR_CONST:
            final_str = f"STRING: {self.value}"
        else:
            final_str = f"SOME OTHER VALUE??? {self.value}"
        return final_str


class Tokenizer:
    """ Go over an input file, breaking into tokens and generating nicely
    formatted XML as well.
    let x = 3; becomes 
    <tokens> 
        <keyword> let </keyword> 
        <identifier> x </identifier>
        <symbol> = </symbol>
        <integerConstant> 3 </integerConstant>
        <symbol> ; </symbol>
    </tokens>"""
    def __init__(self, filepath, dir=False):
        self.filepath = filepath
        self.filename = os.path.basename(filepath)
        self.contents = open(filepath, 'r')
        self.output = [] # List of tokens
        self.SYMBOL_LIST = list("+-*&|~<>/=;.,{[}]()")
        self.KEYWORDS = ["class", "constructor", "function", "method", "field",
                        "static", "var", "int", "char", "boolean", "void",
                        "let", "do", "if", "else",
                        "while", "return"]
        self.KEYWORD_CONSTANTS = ['true', 'false', 'this', 'null']
    
    def tokenize(self):
        """ Tokenize the input, character by character, line by line. 
        Special consideration must be taken for comments:
        // denotes a single line comment
        /* */ denote a multiline comment

        For each character, check if the character represents the end of
        a token.
        if it does: 
            Create a Token object from all the characters found so far.
            Append the Token to the output list.
            Then reset the characters found so far, and move along.
        if it doesn't:
            Append the character onto the characters found so far
            Move along
          """
        block_comment = False
        for line in self.contents:
            consumed_chars = []
            sanitized_line = remove_control_characters(line)
            if "/*" in line and "*/" not in line:
                # Block comment, skip everything until "*/" is found
                # FIXME: This assumes that /* ... */ does not occur on the same
                # line as something we need.
                block_comment: bool = True
                continue
            elif "/*" in line and "*/" in line:
                # Single-line block comment ala /* test */
                continue
            
            if block_comment:
                if "*/" in line:
                    block_comment = False
                continue

            if "//" in sanitized_line:
                # Line comment; take everything up to that point
                sanitized_line = sanitized_line.partition("//")[0]

            for char in list(sanitized_line):
                if "\"" in consumed_chars:
                    # Since anything at all can appear inside of a string,
                    # keep appending characters until we reach another " char
                    # and at that point, empty consumed_chars and make a new token
                    if char == '"':
                        consumed_chars.append('"')
                        current_token = self.create_token(consumed_chars)
                        self.output.append(current_token)
                        consumed_chars = []
                    else:
                        consumed_chars.append(char)
                else:
                    if char == " ":
                        # There are no tokens that contain whitespace
                        # that are not also String Constants; but '"' is not inside
                        # the list of tokens we've seen so far, so this token is not
                        # a String.
                        if len(consumed_chars) != 0:
                            # Don't assume the last input is something useful
                            # Prevents "SPC SPC SPC SPC if" from creating four
                            # empty tokens.
                            current_token = self.create_token(consumed_chars)
                            self.output.append(current_token)
                        # Reset consumed_chars as we've found the end of a token
                        consumed_chars = []
                    elif char in self.SYMBOL_LIST:
                        # Character is some kind of symbol
                        # NOTE: / is division but // is also a comment
                        if len(consumed_chars) != 0:
                            # Whatever we had before may also be a token
                            # As in the case of getName(myArray[index])
                            # getName, (, myArray, [, index, ], ) are tokens
                            current_token = self.create_token(consumed_chars)
                            self.output.append(current_token)
                        current_token = self.create_token(char)
                        self.output.append(current_token)
                        consumed_chars = []
                    else:
                        consumed_chars.append(char)
    
    def create_token(self, char_list):
        """ Take a list of characters and return a Token """
        token_value = ''.join(char_list)

        if token_value in self.KEYWORDS:
            token = Token(TokenKind.KEYWORD, token_value)
        elif token_value in self.SYMBOL_LIST:
            token = Token(TokenKind.SYMBOL, token_value)
        elif token_value in self.KEYWORD_CONSTANTS:
            token = Token(TokenKind.KEYWORD_CONST, token_value)
        elif '"' in token_value:
            token = Token(TokenKind.STR_CONST, token_value.strip('"'))
        elif token_value[0].isdigit():
            try:
                token = Token(TokenKind.INT_CONST, int(token_value))
            except ValueError:
                print(f"Tried to tokenize something starting with a digit that was not a number: {token_value}")
        else:
            token = Token(TokenKind.IDENTIFIER, token_value)

        return token

    def outputXml(self):
        """ Write the tokens as a nicely formatted XML tree. """
        outputfile = open(f"{os.path.dirname(self.filepath)}/{os.path.splitext(self.filename)[0]}-tokenizer.xml", 'w')
        # These characters are used by XML and thus cannot be directly written.
        non_writable_chars = dict([(">", "&gt;"), ("<", "&lt;"), ("&", "&amp;")])
        output_str = "<tokens>\n"
        for token in self.output:
            if token.tokenKind == TokenKind.IDENTIFIER:
                output_str = output_str + f"\t<identifier>{token.value}</identifier>\n"
            elif token.tokenKind == TokenKind.KEYWORD:
                output_str = output_str + f"\t<keyword>{token.value}</keyword>\n"
            elif token.tokenKind == TokenKind.SYMBOL:
                # Check if we need to handle the character separately or not.
                if token.value not in non_writable_chars.keys():
                    output_str = output_str + f"\t<symbol>{token.value}</symbol>\n"
                else:
                    output_str = output_str + f"\t<symbol>{non_writable_chars[token.value]}</symbol>\n"
            elif token.tokenKind == TokenKind.STR_CONST:
                output_str = output_str + f"\t<stringConstant>{token.value}</stringConstant>\n"
            else:
                output_str = output_str + f"\t<integerConstant>{token.value}</integerConstant>\n"
        output_str = output_str + "</tokens>"
        outputfile.write(output_str)
        outputfile.close()


def remove_control_characters(string):
    """ Remove any control characters from the string. 
    The Unicode Consortium defines what counts as a control character; they are
    all prefixed with C, so filter those matching strings out. """
    return "".join(ch for ch in string if unicodedata.category(ch)[0] != "C")

class Parser:
    """ Parse the tokens into the output format; for part one, the output is XML
    In part two, the output is Jack VM Code."""
    def __init__(self, filename, tokens):
        self.fullpath = filename
        self.filename = os.path.basename(filename)
        self.tokens: list = tokens
        self.symbol_table = SymbolTable()
        self.classname = "" # Keep track for functions
        self.subroutineName = "" # Keep track for Label generation
        self.num_whiles = 0 # Keep track for label generation
        self.num_if = 0 # Keep track for label generation
        self.writer = VMWriter(filename)
        self.output = self.compile_class()
        
    
    def write_output(self):
        #outputfile = open(f"{os.path.dirname(self.fullpath)}/{os.path.splitext(self.filename)[0]}.xml", 'w')
        #outputfile.write(self.output)
        #outputfile.close()
        self.writer.close_file()
    
    def eat_token(self):
        """ Consume the next token, returning it and removing it from the list
        of tokens in the program."""
        token = self.tokens[0]
        self.tokens = self.tokens[1:]
        return token
    
    def indent_str(self, string, level):
        """ Indent the string to the correct level by inserting level * '\t' 
        to the left-side of the string """
        return string.rjust(len(string) + level, '\t')

    def compile_class(self):
        """ Start compilation with the top-most item, which is always a Class. """
        # Eat the first token, which should be "class"
        current_token = self.eat_token()
        #output_string = "<class>\n"
        #output_string += f"\t<keyword>{current_token.value}</keyword>\n"
        # After "class" is always ClassName
        class_name = self.eat_token()
        self.classname = class_name.value
        #output_string += f"\t<className>\n"
        #output_string = f"{output_string}\t<identifier>{class_name.value}</identifier>\n"
        #output_string += f"\t</className>\n"
        symbol = self.eat_token()
        #output_string += f"\t<symbol>{symbol.value}</symbol>\n"
        # Token is either static | field for classVarDec, or
        # constructor | function | method for subroutineDec
        current_token = self.eat_token()
        while current_token.value in ["field", "static"]:
            # Write zero or more Class Variables
            #output_string = self.compile_classVarDec(output_string, current_token)
            self.compile_classVarDec(current_token)
            current_token = self.eat_token()
        while current_token.value in ["function", "method", "constructor"]:
            # Write function/constructor/method declarations
            #output_string = self.compile_subroutineDec(output_string, current_token)
            self.compile_subroutineDec(current_token)
            current_token = self.eat_token()
        if current_token.value == "}":
            pass
            #output_string = output_string + "\t<symbol>}</symbol>\n"
        else:
            pass
        #output_string += "</class>"
        #return output_string
    
    def compile_identifier(self, identifier: Token, kind: str, caller: str, indent_level: int):
        """ Handle identifiers in a more sophisticated way.
        Output the identifier's category, the running index if it exists, and if
        the variable is being declared or used.
        """
        #string_so_far += self.indent_str(f"<identifier>{identifier.value}\n", indent_level)
        if caller in ["classVarDec", "varDec", "subroutineDec"]:
            use_dec = "declaration"
        elif caller in ["expression", "subroutineCall", "argument", "statement"]:
            use_dec = "use"
        else:
            use_dec = f"NOT FOUND? SOMETHING WENT WRONG: Caller => {caller}"

        #string_so_far += self.indent_str(f"<useDec>{use_dec}</useDec>\n", indent_level + 1)

        if kind in ["static", "field", "var", "arg"]:
            pass
            #string_so_far += self.indent_str(f"<kind>{kind} :: {self.symbol_table.index_of(identifier.value)}</kind>\n", indent_level + 1)
        else:
            pass
            #string_so_far += self.indent_str(f"<kind>{kind}</kind>\n", indent_level + 1)
        #string_so_far += self.indent_str("</identifier>\n", indent_level)
        
        #return string_so_far
    
    def compile_classVarDec(self, static_field: Token):
        """ Compile class fields and variables, in the form
        static Int i;
        field Array arr;
        static Int a, b, c, d, e;
        Generates no VM Code.
        """
        #string_so_far += "\t<classVarDec>\n"
        #string_so_far += f"\t\t<keyword>{static_field.value}</keyword>\n"
        type_token = self.eat_token()
        if type_token.tokenKind == TokenKind.KEYWORD:
            pass
            #string_so_far += f"\t\t<keyword>{type_token.value}</keyword>\n"
        elif type_token.tokenKind == TokenKind.KEYWORD_CONST:
            pass
            #string_so_far += f"\t\t<keywordConstant>{type_token.value}</keyword>\n"
        else:
            pass
            #string_so_far = self.compile_identifier(string_so_far, type_token, "class", "classVarDec", 2)
            #string_so_far += self.indent_str(f"<identifier>{type_token.value}</identifier>\n", 2)
        var_name = self.eat_token()
        # Provide more information on identifiers
        #string_so_far = self.compile_identifier(string_so_far, var_name, {static_field.value}, "classVarDec", 2)
        self.symbol_table.define(var_name.value, type_token.value, static_field.value)
        #string_so_far += f"\t\t<identifier>{var_name.value}</identifier>\n"

        # There are zero or more other var_names
        # So self.eat_token may return either ',' or ';' at this stage
        current_token = self.eat_token()
        while current_token.value != ";":
            if current_token.value == ',':
                pass
                #string_so_far += f"\t\t<symbol>{current_token.value}</symbol>\n"
            else:
                # We can use the same static_field.value for kind as this is still
                # within the same variable declaration. Same for type_token.
                self.symbol_table.define(current_token.value, type_token.value, static_field.value)
                #string_so_far = self.compile_identifier(string_so_far, current_token, static_field.value, "classVarDec", 2)
                #string_so_far += f"\t\t<identifier>{current_token.value}</identifier>\n"
            current_token = self.eat_token()
        #string_so_far += f"\t\t<symbol>{current_token.value}</symbol>\n"
        #string_so_far += "\t</classVarDec>\n"
        return #string_so_far
    
    def compile_subroutineDec(self, function_method_constructor):
        """ Compile each sub-statment inside the class definition
        Form:
        (constructor | function | method) (void | type) 
        subroutineName (parameterList) subroutineBody"""
        void_or_type = self.eat_token()
        if void_or_type.tokenKind == TokenKind.KEYWORD:
            pass
        elif void_or_type.tokenKind == TokenKind.KEYWORD_CONST:
            pass
        else:
            pass
        current_token = self.eat_token()
        self.subroutineName = f"{self.classname}.{current_token.value}"
        current_token = self.eat_token()
        current_token = self.eat_token()
        
        # Tell the symbol table we're now inside of a new subroutine
        self.symbol_table.start_subroutine()
        if function_method_constructor.value == "method":
            # If we're a method, we need the "this" segment to be included
            # as an argument in order to access fields. Specifically, it must be
            # argument 0 in order to work.
            self.symbol_table.define("this", self.classname, "arg")
        self.compile_parameterList(current_token)
        
        n_args = self.symbol_table.var_count("arg")
        n_fields = self.symbol_table.var_count("field")

        print(f"Function Declaration with {n_args} arguments")
        
        # compile_parameterList will have eaten all the tokens up to and including )
        # so we just append it here
        # NOTE: self.eat_token() will now return whatever is the token after ')', NOT ')'
        self.compile_subroutineBody(function_method_constructor, n_fields)
        return
    
    def compile_parameterList(self, param1):
        """ Compile the list of function parameters.
        Comes in form (type varName (, type varName)* )?
        SIDE-EFFECT: Store the arguments in the Symbol table"""
        if param1.value != ')':
            # We have parameters!
            current_token = self.eat_token()
            self.symbol_table.define(current_token.value, param1.value, "arg")
            current_token = self.eat_token()
            while current_token.value != ")":
                # More args to parse -- starts with ',', then type, then Varname
                current_token = self.eat_token()
                arg_type = current_token.value
                current_token = self.eat_token()
                self.symbol_table.define(current_token.value, arg_type, "arg")
                current_token = self.eat_token()
        return


    def compile_subroutineBody(self, subroutineType, n_args):
        """ Subroutine body has the following form:
        { varDec* statements }
        SubroutineType is function | constructor | method
        n_args is the number of arguments the function requires."""
        # Should be '{' by now
        current_token = self.eat_token()
        current_token = self.eat_token()
        # zero or more var dec, so loop through them:
        while current_token.value == 'var':
            # Variable declarations at the top of the body, in form:
            # var Type varName (, Type varName)* ;
            self.compile_varDec(current_token)
            current_token = self.eat_token()
        # In the above loop we ate the ';' token and are now onto the statements
        # We write the "function functionName nlocals" here because we need to know
        # the size of the local segment (the number of vars declared)
        nvars = self.symbol_table.var_count("var")
        self.writer.write_function(self.subroutineName, nvars)
        """ The compiler, during the above compile_parameterList call, will have
        created a symbol table consisting of the arguments for this function.
        This is useful especially in the case of a constructor, where we need to
        allocate space for the arguments as we will presumably want to assign to
        them at a later date (inside the constructor body, usually).
        Memory.alloc(n) will allocate n contiguous free memory addresses and
        will return the base address.
        """
        if subroutineType.value == "constructor":
            self.writer.write_push("constant", n_args)
            self.writer.write_call("Memory.alloc", "1")
            # Anchor the base address returned to the "this" segment
            self.writer.write_pop("pointer", 0)
        elif subroutineType.value == "method":
            # Set "this" to the current object
            self.writer.write_push("arg", 0)
            self.writer.write_pop("pointer", 0)

        while current_token.value != '}':
            self.compile_statements(current_token)
            current_token = self.eat_token()
        return

    def compile_varDec(self, keyword_token):
        """ Compile variable declarations, which take the form: 
        var Type varName (, Type varName)* ;
        corrosponding to "var Int a, beta, c;"
        """
        #string_so_far += "\t\t<varDec>\n"
        #string_so_far += f"\t\t\t<keyword>{keyword_token.value}</keyword>\n"
        current_token = self.eat_token()
        var_type = current_token.value
        if current_token.tokenKind == TokenKind.KEYWORD:
            pass
            #string_so_far += f"\t\t\t<keyword>{current_token.value}</keyword>\n"
        elif current_token.tokenKind == TokenKind.KEYWORD_CONST:
            pass
            #string_so_far += f"\t\t\t<keywordConstant>{current_token.value}</keywordConstant>\n"
        else:
            # Type that was defined by user (eg a class as in var /Array/ arr) {name} 
            pass
            #string_so_far = self.compile_identifier(string_so_far, current_token, "type", "varDec", 3)
        current_token = self.eat_token()
        var_name = current_token.value
        # We know we're a variable, as that's all we can be in compile_varDec
        self.symbol_table.define(var_name, var_type, "var")
        #string_so_far = self.compile_identifier(string_so_far, current_token, "var", "varDec", 3)
        #string_so_far += f"\t\t\t<identifier>{current_token.value}</identifier>\n"
        # Now the token may either be ',' or ';'
        current_token = self.eat_token()
        while current_token.value == ',':
            # , -> varName -> , | ;
            #string_so_far += f"\t\t\t<symbol>{current_token.value}</symbol>\n"
            current_token = self.eat_token()
            var_name = current_token.value
            self.symbol_table.define(var_name, var_type, "var")
            #string_so_far = self.compile_identifier(string_so_far, current_token, "var", "varDec", 3)
            #string_so_far += f"\t\t\t<identifier>{current_token.value}</identifier>\n"
            current_token = self.eat_token()
        #string_so_far += f"\t\t\t<symbol>{current_token.value}</symbol>\n"
        #string_so_far += "\t\t</varDec>\n"
        return #string_so_far
    
    def compile_statements(self, triggering_token):
        """ Compile the statements in the code. Statements come if several forms:
        letStatement | doStatement | ifStatement | whileStatement | returnStatement
        """
        # Current token will be one of the five options above less the "Statement" part
        #string_so_far += "\t\t<statements>\n"
        current_token = triggering_token
        if current_token.value == "if":
            #string_so_far = self.compile_ifStatement(string_so_far)
            self.num_if += 1
            self.compile_ifStatement()
        elif current_token.value == 'do':
            #string_so_far = self.compile_doStatement(string_so_far, 2)
            self.compile_doStatement()
        elif current_token.value == 'let':
            self.compile_letStatement()
            #string_so_far = self.compile_letStatement(string_so_far, 2)
        elif current_token.value == 'while':
            self.num_whiles += 1
            self.compile_whileStatement()
            #string_so_far = self.compile_whileStatement(string_so_far, 2)
        elif current_token.value == 'return':
            self.compile_returnStatement()
            #string_so_far = self.compile_returnStatement(string_so_far, 2)
        else:
            print(f"Compile_Statements found a non-allowable value: {current_token}")
            raise NotImplementedError
        #string_so_far += "\t\t</statements>\n"
        return #string_so_far
    
    def compile_ifStatement(self):
        """ Compile an if statement, which is defined as the following:
        if (expression) {statements} (else {statements})? 
        Code algorithm:
        compile expression
        not
        if-goto L1
        compile statements1
        goto L2
        
        label L1
        compile statements2
        
        label L2
        (rest of program)"""
        label1 = f"{self.subroutineName}$IF_ELSE{self.num_if}"
        label2 = f"{self.subroutineName}$IF_END{self.num_if}"
        current_token = self.eat_token()
        self.compile_expression()
        self.writer.write_arithmetic("NOT")
        self.writer.write_if(label1) 
        # Above will have eaten all upto ')'
        current_token = self.eat_token()
        current_token = self.eat_token()
        # Should be '{'
        # Now aught to be the start of a statement
        current_token = self.eat_token()
        while current_token.value != '}':
            self.compile_statements(current_token)
            current_token = self.eat_token()
        # At this point, token should be '}'
        self.writer.write_goto(label2)
        self.writer.write_label(label1)
        # Else doesn't have to exist, the current token is '}'
        # Either 'else' or any other token
        if self.tokens[0].value == 'else':
            # Else clause: else { statements }
            current_token = self.eat_token()
            current_token = self.eat_token()
            current_token = self.eat_token()
            while current_token.value != '}':
                self.compile_statements(current_token)
                current_token = self.eat_token()
            # Token should now be '}'
        self.writer.write_label(label2)
        return
    
    def compile_letStatement(self):
        """ Compile a let statement in the form:
        let varName( [ expression ])? = expression ;
        Code Algorithm:
        (expression should have put something on the stack)
        pop varName
        where varName will be the segment it's on and the index.
        """
        # Should now be an identifier
        current_token = self.eat_token()
        var_name = current_token.value
        var_kind = self.symbol_table.kind_of(var_name)
        var_index = self.symbol_table.index_of(var_name)
        array_access = False
        if var_kind != None:
            pass
            if var_kind == "var":
                var_kind = "local"
        else:
            print("Tried to assign to undeclared variable:", var_name)

        # Token is now either [ or =
        current_token = self.eat_token()
        if current_token.value == '[':
            # Something like "let arr[expression 1] = expression 2"
            # Method: push arr
            #         compile_expression 1
            #         add
            #       (the below is handled outside this if statement)
            #         compile_expression 2
            #         pop temp 0
            #         pop pointer 1
            #         push temp 0
            #         pop that 0
            array_access = True
            self.writer.write_push(var_kind, var_index)
            while current_token.value != ']':
                self.compile_expression()
                current_token = self.eat_token()
            # The "push N" should have been handled by the above loop
            self.writer.write_arithmetic("ADD")
            current_token = self.eat_token()
        # Next token will now be some expression's first token.
        # compile_expression has to have that token as its beginning, so don't eat it yet.
        self.compile_expression()
        # Expression should have put something on the stack
        if not array_access:
            self.writer.write_pop(var_kind, var_index)
        else:
            self.writer.write_pop("temp", 0)
            self.writer.write_pop("pointer", 1)
            self.writer.write_push("temp", 0)
            self.writer.write_pop("that", 0)
        # Compile_expression should only eat its tokens, not the ';' token.
        current_token = self.eat_token()
        return
    
    def compile_doStatement(self):
        """ Do statements come in the follow form:
        do subroutineCall ;
        NOTE: Functions must always return a value. In a do statement, the
        return type is void; therefore, the return value must be handled by
        popping it off the stack and placing it into /temp 0/
         """
        #string_so_far += self.indent_str("<doStatement>\n", indent_level)
        #string_so_far += self.indent_str("<keyword>do</keyword>\n", indent_level + 1)
        current_token = self.eat_token()
        #string_so_far = self.compile_subroutineCall(string_so_far, indent_level + 2, current_token)
        self.compile_subroutineCall(current_token)
        current_token = self.eat_token()

        # Pop the return value from the above function into temp 0
        self.writer.write_pop("temp", 0)
        #string_so_far += self.indent_str(f"<symbol>{current_token.value}</symbol>\n", indent_level + 1)
        #string_so_far += self.indent_str("</doStatement>\n", indent_level)
        return #string_so_far
    
    def compile_whileStatement(self):
        """ While Statements have the form: 
        while ( expression ) { statements }
        Code algorithm:
        label L1:
            compile expression
            not
            if-goto L2
            compile statements
            goto L1
        label L2
        (rest of program)
        """
        current_token = self.eat_token()
        label1 = f"{self.subroutineName}$WHILE{self.num_whiles}"
        label2 = f"{self.subroutineName}$WHILE_END{self.num_whiles}"
        self.writer.write_label(label1)
        self.compile_expression()
        self.writer.write_arithmetic("NOT")
        self.writer.write_if(label2)
        # Token should now be ')'
        current_token = self.eat_token()
        current_token = self.eat_token()
        # Should be the start of the statement body
        current_token = self.eat_token()
        # Statements, now
        while current_token.value != '}':
            self.compile_statements(current_token)
            current_token = self.eat_token()
        self.writer.write_goto(label1)
        self.writer.write_label(label2)
        return
    
    def compile_returnStatement(self):
        """ Return statements are of the form:
        return expression? ; """
        #string_so_far += self.indent_str("<returnStatement>\n", indent_level)
        #string_so_far += self.indent_str("<keyword>return</keyword>\n", indent_level + 1)
        current_token = self.eat_token()
        expr_exists = False
        if current_token.value != ';':
            # Here the above token will likely be the identifier piece of the above
            # eg in return x + 6; the current_token the first time will be 'x'
            # but compile_expression requires that to be the next token to be eaten
            # So we'll put it back
            expr_exists = True
            self.tokens.insert(0, current_token)
            #string_so_far = self.compile_expression(string_so_far, indent_level + 1)
            self.compile_expression()
            current_token = self.eat_token()
        #string_so_far += self.indent_str(f"<symbol>{current_token.value}</symbol>\n", indent_level + 1)
        #string_so_far += self.indent_str(f"</returnStatement>\n", indent_level)
        if not expr_exists:
            # No expression; we're not returning anything so we have to "return" 0 
            self.writer.write_push("constant", 0)
        self.writer.write_return()
        return #string_so_far
    
    def compile_expression(self):
        """ Compile expressions, which come in various forms:
        term (op term)* where
        term: integerConstant | stringConstant | keywordConstant | varName |
                varName '[' expression ']' | subroutineCall | { expression } | unaryOp term
        
        The following algorithm is used for code generation:
        if exp is a number /n/:
            output "push /n/"
        elif exp is a variable /var/:
            output "push /var/"
        elif exp is /exp1 op exp2/:
            compile_term(exp1)
            compile_term(exp2)
            output "op"
        elif exp is /op exp/:
            compile_term(exp)
            output "op"
        elif exp is /f(exp1, exp2, ...)/:
            compile_expression(exp1)
            compile_expression(exp2)
            ...
            output "call f"
        """
        #npc = dict([(">", "&gt;"), ("<", "&lt;"), ("&", "&amp;")])
        #string_so_far += self.indent_str("<expression>\n", indent_level)
        # We want to handle terms separate from a whole expression
        #string_so_far = self.compile_term(string_so_far, indent_level)
        self.compile_term()
        
        # Now that we've handled term, check if we need to handle (op term)*
        current_token = self.eat_token()
        while current_token.value in list("+-*/&|<>="):
            """ if current_token.value in npc.keys():
                string_so_far += self.indent_str(f"<symbol>{npc[current_token.value]}</symbol>\n", indent_level + 1)
            else:
                string_so_far += self.indent_str(f"<symbol>{current_token.value}</symbol>\n", indent_level + 1) """
            #string_so_far = self.compile_term(string_so_far, indent_level)
            self.compile_term()
            op = current_token.value
            if op == "+":
                self.writer.write_arithmetic("ADD")
            elif op == "-":
                # Not a unary op, we're a subtraction
                self.writer.write_arithmetic("SUB")
            elif op == "*":
                # Math.multiply is the OS's multiplication method
                # It always takes two arguments
                self.writer.write_call("Math.multiply", 2)
            elif op == "/":
                self.writer.write_call("Math.divide", 2)
            elif op == "&":
                self.writer.write_arithmetic("AND")
            elif op == "|":
                self.writer.write_arithmetic("OR")
            elif op == "<":
                self.writer.write_arithmetic("LT")
            elif op == ">":
                self.writer.write_arithmetic("GT")
            else:
                self.writer.write_arithmetic("EQ")
            current_token = self.eat_token()
        else:
            # Whoops, we've eaten a token that isn't ours to begin with
            # Quick, put it back on the queue before anybody notices
            # And get us out of here
            self.tokens.insert(0, current_token)
        #string_so_far += self.indent_str("</expression>\n", indent_level)
        return #string_so_far
    
    def compile_term(self):
        """ Compile a term, within an expression.
        Terms are
        integerConstant | stringConstant | keywordConstant | varName |
        varName '[' expression ']' | subroutineCall | ( expression ) | unaryOp term
        """
        #string_so_far += self.indent_str("<term>\n", indent_level)
        current_token = self.eat_token()
        if current_token.tokenKind in [TokenKind.INT_CONST, TokenKind.STR_CONST]:
            if current_token.tokenKind == TokenKind.INT_CONST:
                # Output "push constant /n/"
                self.writer.write_push("constant", current_token.value)
                #string_so_far += self.indent_str(f"<integerConstant>{current_token.value}</integerConstant>\n", indent_level + 1)
            else:
                # TODO: string building (have to call String.appendChar on each character)
                str_len = len(current_token.value)
                self.writer.write_push("constant", f"{str_len}")
                self.writer.write_call("String.new", "1")
                for ch in current_token.value:
                    self.writer.write_push("constant", f"{ord(ch)}")
                    self.writer.write_call("String.appendChar", 2)
                #string_so_far += self.indent_str(f"<stringConstant>{current_token.value}</stringConstant>\n", indent_level + 1)
        elif current_token.value in ["this", "that", "true", "false", "null"]:
            # TODO: false, null in VM code are 0, true is -1; "this" is the current object, "that" is an array
            segname = current_token.value
            if segname == "this":
                self.writer.write_push("pointer", 0)
            elif segname == "that":
                self.writer.write_push("pointer", 1)
            elif segname == "true":
                self.writer.write_push("constant", 1)
                self.writer.write_arithmetic("NEG")
            elif segname == "false" or segname == 'null':
                self.writer.write_push("constant", 0)
            else:
                print("Somehow we're in the else branch of term [this, that, true, false, null]")
        elif current_token.tokenKind == TokenKind.IDENTIFIER:
            # varName or varname[expr] 
            # NOTE: subroutineCall might fall into this cage as well
            # Do a cheeky lookahead and see if the next token is '[' or '{' without popping it
            # TODO: This is an array access, meaning we need to use pointer 1 (that)
            if self.tokens[0].value == '[':
                arr_seg = self.symbol_table.kind_of(current_token.value)
                arr_seg_index = self.symbol_table.index_of(current_token.value)
                current_token = self.eat_token()
                self.writer.write_push(arr_seg, arr_seg_index)
                self.compile_expression()
                # Above should have pushed an integer to the stack.
                # That integer will be the array offset
                self.writer.write_arithmetic("ADD")
                # We now want to push the value stored there onto the stack
                self.writer.write_pop("pointer", 1)
                self.writer.write_push("that", 0)
                # Should be ] now
                current_token = self.eat_token()
            elif self.tokens[0].value == '(' or self.tokens[0].value == '.':
                # SubroutineCall
                # Meaning we've eaten the name of the subroutine, so we have to pass it on
                self.compile_subroutineCall(current_token)
            else:
                # Varname, no index lookup
                # Output to /push seg i/
                segment = self.symbol_table.kind_of(current_token.value)
                if segment == "var":
                    segment = "local"
                if segment == None:
                    print("Looked up a variable in the table inside compileExpression, but didn't find it!", current_token.value)
                index = self.symbol_table.index_of(current_token.value)
                self.writer.write_push(segment, index)

        elif current_token.value == '(':
            # ( expression ) here
            self.compile_expression()
            # Aught to be }
            current_token = self.eat_token()
        elif current_token.value in ['~', '-']:
            # UnaryOp term
            self.compile_term()
            # Hopefully, the above should have written something NOT/NEG can operate on
            if current_token.value == '~':
                self.writer.write_arithmetic("NOT")
            else:
                self.writer.write_arithmetic("NEG")
        else:
            print(f"Expression: Some non-valid expression in term: -> {current_token}")
        
        return
    
    def compile_subroutineCall(self, subroutineName):
        """ Subroutine Calls are defined as: 
        subroutineName ( expressionList ) | 
        (className | varName) . subroutineName ( expressionList )
        Calling requires knowledge of the number of arguments in the subroutine; that is:
            call Math.mult 2 requires knowing that Math.mult takes two arguments.
        
        """
        print(f"compile_subroutineCall given {subroutineName.value} as subroutineName")
        # Kind will be either static, field, var, arg if subroutineName is a variable name
        # None otherwise (class or subroutine)
        # If Kind is a var or arg, we need to push it to the stack /before/ pushing the arguments
        # Then we can call the method. This assumes the called method knows what to do with the
        # supplied base address of the object. The compiler will inject the nessesary code to handle
        # such things when it detects a "method" declaration.
        # Example:
        # Jack: myObj.foo(x1, x2)
        # VM:   push myObj --> push local someNum
        #       push x1    --> push local otherNum
        #       push x2    --> push local someOtherNum
        #       call foo   --> call ObjType.foo
        # For "call foo", we need to know the type. 

        current_token = self.eat_token()
        if current_token.value == '(':
            # do foo(a, b, c)
            # This is always a method call! Function calls will be prefixed with
            # the Class name, as in MyMath.mod(a, b)
            # ExpressionList is (expression (, expression)* )?
            # Meaning expressionList might just be empty, but oh well.
            self.writer.write_push("pointer", 0)
            n_args = self.compile_expressionList(current_token)
            n_args += 1 # Methods always have the object as an argument
            name = f"{self.classname}.{subroutineName.value}"
            
            self.writer.write_call(name, n_args)
            # Should have taken everything up to ')'
            current_token = self.eat_token()
        else:
            # Option two: (className | varName) . subroutineName( expressionList )
            kind = self.symbol_table.kind_of(subroutineName.value)
            current_token = self.eat_token()
            name = current_token.value
            if kind == 'var' or kind == 'static' or kind == 'field' or kind == "local":
                # We're likely calling a method on some object. 
                index = self.symbol_table.index_of(subroutineName.value)
                self.writer.write_push(kind, index)
                # Just want to call the method for its effect
                # Assumes the caller's code knows what to do with the implicit argument
                call_name = f"{self.symbol_table.type_of(subroutineName.value)}.{name}"
                # The object being called upon is an argument to the function but
                # is not explicitly declared in the expression list.
                n_args = 1
            else:
                # We're a "className", so we should be called as className.functionName
                n_args = 0
                call_name = f"{subroutineName.value}.{name}"
            current_token = self.eat_token()
            n_args += self.compile_expressionList(current_token)
           

            self.writer.write_call(call_name, n_args)
            current_token = self.eat_token()
        return
    
    def compile_expressionList(self, starting_token):
        """Expression lists are defined as:
        (expression (, expression)* )?
        So there may be no expressions or several.
        Returns the number of arguments in the expressionList"""
        #string_so_far += self.indent_str("<expressionList>\n", indent_level)
        # current_token = self.eat_token()
        current_token = starting_token
        n_args = 0
        if self.tokens[0].value != ')':
            # There are some expressions here
            while current_token.value != ')':
                n_args += 1
                if current_token.value == ',':
                    pass
                    #string_so_far += self.indent_str(f"<symbol>{current_token.value}</symbol>\n", indent_level + 1)
                #string_so_far = self.compile_expression(string_so_far, indent_level + 1)
                self.compile_expression()
                current_token = self.eat_token()

        if current_token.value == ')':
            # We have eaten the ), so put it back like a good function
            self.tokens.insert(0, current_token)
        #string_so_far += self.indent_str("</expressionList>\n", indent_level)
        return n_args

class SymbolTableEntry():
    """ An entry for the symbol table, consisting of the following fields:
    index, the index of the entry on its segment
    segment, the virtual segment of the entry
    type, the type of the entry
    """
    def __init__(self, entry_type, index, kind):
        self.type = entry_type
        self.index = index
        self.kind = kind


class SymbolTable():
    """ Class for entries on a symbol table. Tracks the segment, index, name,
    and type of the entry."""
    def __init__(self):
        """ Creates the empty symbol tables. """
        # Keys are the variable name (meaning each variable must be unique)
        # There's only two scopes (class and subroutine) so no need to worry
        # about things like "shadowing"
        self.class_table = dict()
        self.subroutine_table = dict()
        # Dictionaries for tracking the running indexes of each kind of identifier
        self.class_index = dict([("static", 0), ("field", 0)])
        self.subroutine_index = dict([("var", 0), ("arg", 0)])
    
    def define(self, name: str, type: str, kind: str):
        """ Create an entry in the symbol table.
        For each entry of Kind, give a running index to that kind
        eg var Int num; becomes the entry (num, Int, var, 0) in the table.
        var Int x; becomes (x, Int, var, 1) as there is already one Var type."""
        if kind in ["static", "field"]:
            seg_index = self.class_index[kind]
            self.class_table[name] = SymbolTableEntry(type, seg_index, kind)
            self.class_index[kind] = self.class_index[kind] + 1
        else:
            seg_index = self.subroutine_index[kind]
            self.subroutine_table[name] = SymbolTableEntry(type, seg_index, kind)
            self.subroutine_index[kind] = self.subroutine_index[kind] + 1
    
    def start_subroutine(self):
        """ Reset the subroutine symbol table at the start of a new subroutine.
        Also resets the index of each segment that can be found in a subroutine."""
        self.subroutine_table.clear()
        self.subroutine_index["var"] = 0
        self.subroutine_index["arg"] = 0
    
    def kind_of(self, name: str):
        """ Returns the kind of the named identifier; if it is not available in
        the current scope, returns None"""
        if name in self.subroutine_table.keys():
            return self.subroutine_table[name].kind
        elif name in self.class_table.keys():
            return self.class_table[name].kind
        else:
            return None

    def var_count(self, kind: str):
        """ Returns the number of variables of the given Kind in the current scope."""
        count = 0
        if kind in ["field", "static"]:
            """ for value in self.class_table.values():
                if value[0] == kind:
                    count += 1 """
            count = self.class_index[kind]
        else:
            """ for value in self.subroutine_table.values():
                if value[0] == kind:
                    count += 1 """
            count = self.subroutine_index[kind]
        return count
    
    def type_of(self, name: str):
        """ Return the type of the named identifier in the current scope. """
        if name in self.subroutine_table.keys():
            return self.subroutine_table[name].type
        elif name in self.class_table.keys():
            return self.class_table[name].type
        else:
            print(f"type_of: entry not fFound in scope: {name}")
            return f"NO TYPE FOUND FOR ENTRY {name}"

    def index_of(self, name: str):
        """ Return the index of the named identifier in the current scope. """
        if name in self.subroutine_table.keys():
            return self.subroutine_table[name].index
        elif name in self.class_table.keys():
            return self.class_table[name].index
        else:
            print(f"index_of: entry not found in scope {name}")
            return f"NO INDEX FOUND FOR ENTRY {name}"
        
class VMWriter():
    """ Class for writing the VM code of the appropriate location. """
    def __init__(self, path):
        self.file = path
        self.filename = os.path.basename(path)
        self.output = ""
    
    def write_push(self, seg, index):
        """ Push some item from virtual segment /seg/ at index to the stack
        Seg is one of "static" "local" "temp" "constant" "arg" "this" "that" "pointer" """
        if seg == "var":
            seg = "local"
        elif seg == "field":
            seg = "this"
        elif seg == "arg":
            seg = "argument"
        else:
            pass
        self.output += (f"push {seg} {index}\n")
        

    def write_pop(self, seg, index):
        """ Pop some item from the stack into the virtual segment /seg i/
        Seg is one of "static" "local" "pointer" "temp" "arg" "this" "that" """
        if seg == "var":
            seg = "local"
        elif seg == "field":
            seg = "this"
        elif seg == "arg":
            seg = "argument"
        else:
            pass
        self.output += (f"pop {seg} {index}\n")

    def write_arithmetic(self, command: str):
        """ Write some arithmetic command. Valid commands are:
        ADD, SUB, NEG, EQ, GT, LT, AND, OR, NOT """
        cmd = command.lower()
        if cmd in ["add", "sub", "neg", "not", "eq", "gt", "lt", "and", "or", "not"]:
            self.output += f"{cmd}\n"
        else:
            print(f"Some incorrect command passed to write_arithmetic: {cmd}")

    def write_label(self, label):
        """ Write a label, eg label LOOP"""
        self.output += f"label {label}\n"

    def write_goto(self, label):
        """ Write a GOTO statement, for control flow """
        self.output += f"goto {label}\n"

    def write_if(self, label):
        """ Writes a VM IF-GOTO command """
        self.output += f"if-goto {label}\n"

    def write_call(self, name, n_args):
        """ Write the VM version of a function call 
        NOTE: Some value is ALWAYS returned after a call, even if return type
        is /void/. This value must be removed from the stack, at some point. """
        self.output += f"call {name} {n_args}\n"

    def write_function(self, name, n_locals):
        """ Write a function setup """
        self.output += f"function {name} {n_locals}\n"

    def write_return(self):
        """ Writes a VM return command """
        self.output += "return\n"

    def close_file(self):
        """ Writes the output then closes the file """
        outputfile = open(f"{os.path.dirname(self.file)}/{os.path.splitext(self.filename)[0]}.vm", 'w')
        outputfile.write(self.output)
        outputfile.close()

def tokenize_dir(path):
    # Go through each file in the dir and tokenize it.
    tokens = []
    for item in os.listdir(path):
        if os.path.splitext(item)[1] == ".jack":
            tokenizer = Tokenizer(f"{path}/{item}")
            tokenizer.tokenize()
            tokens.append((f"{path}/{item}", tokenizer.output))
            tokenizer.outputXml()
    return tokens

def parse_dir(filename_tokens_pair):
    """ Go through each (filename, tokens) pair and parse each them """
    for filename, tokens in filename_tokens_pair:
        print(f"Parsing file {filename}")
        parser = Parser(filename, tokens)
        parser.write_output()

def main():
    input = os.path.abspath(sys.argv[1])
    if os.path.isfile(input):
        # Create a new tokenizezr for it
        tokenizer = Tokenizer(input)
        tokenizer.tokenize()
        tokenizer.outputXml()
        tokens = tokenizer.output
        parser = Parser(input, tokens)
        parser.write_output()
    elif os.path.isdir(input):
        tokens_filename_pair = tokenize_dir(input)
        parse_dir(tokens_filename_pair)
    else:
        raise NameError

main()