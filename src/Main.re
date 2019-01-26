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

let file =
  try (Sys.argv[2]) {
  | _ =>
    Js.log("Error: no file specified in args");
    exit(1);
  };

let () =
  Node.Fs.readFileAsUtf8Sync(file)
  |> tokenize
  |> List.map(tokenToString)
  |> List.iter(Js.log);
