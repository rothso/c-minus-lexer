open Lexer;

let tokenToString = (token: token): string =>
  switch (token) {
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Divide => "/"
  | NotEqual => "!="
  | Equal => "=="
  | LessThan => "<"
  | LessThanEql => "<="
  | GreaterThan => ">"
  | GreaterThanEql => ">="
  | Assignment => "="
  | Semicolon => ";"
  | Comma => ","
  | LParen => "("
  | RParen => ")"
  | LBrace => "{"
  | RBrace => "}"
  | Ident(string) => "id:\t" ++ string
  | Integer(int) => "num:\t" ++ string_of_int(int)
  | FloatingPoint(float) => "float:\t" ++ string_of_float(float)
  | Keyword(keyword) =>
    "kw:\t"
    ++ (
      switch (keyword) {
      | If => "if"
      | Else => "else"
      | Int => "int"
      | Float => "float"
      | Void => "void"
      | While => "while"
      | Return => "return"
      }
    )
  | Invalid(string) => "error:\t" ++ string
  };

/* The user must provide the name of the C- file */
let file =
  try (Sys.argv[2]) {
  | _ =>
    Js.log("Error: no file specified in args");
    exit(1);
  };

/* Read the input C- file and tokenize it line-by-line */
Node.Fs.readFileAsUtf8Sync(file)
|> Js.String.split("\n")
|> Array.fold_left(
     (prevState, line) => {
       /* Print the current line */
       Js.log("\027[36m>>>> " ++ line ++ "\027[0m");
       /* Tokenize it, resuming from the previous state */
       let (tokens, nextState) = tokenize(~state=?prevState, line);
       /* Print the tokens */
       tokens |> List.map(tokenToString) |> List.iter(Js.log);
       /* The next line needs to know if we are in a comment state */
       switch (nextState) {
       | Some(Comment(_)) => nextState
       | _ => None
       };
     },
     None
   );
