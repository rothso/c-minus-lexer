let explode = (input: string): list(char) => {
  let rec exp = (i, chars) =>
    i >= 0 ? exp(i - 1, [input.[i], ...chars]) : chars;
  exp(String.length(input) - 1, []);
};

type token =
  | Plus /* + */
  | Minus /* - */
  | Times /* * */
  | Divide /* / */
  | NotEqual /* != */
  | Equal /* == */
  | LessThan /* < */
  | LessThanEql /* <= */
  | GreaterThan /* > */
  | GreaterThanEql /* >= */
  | Assignment /* = */
  | Semicolon /* ; */
  | Comma /* , */
  | LParen /* ( */
  | RParen /* ) */
  | LBrace /* { */
  | RBrace /* } */
  | Ident
  | Integer(string) /* TODO cast to int */
  | Float(string)
  | Keyword
  | Invalid(string);

type state =
  | Number(string)
  | Float(string)
  | String(string);

let lexer = (input: string) => {
  let rec tok = (input, buffer, tokens) => {
    switch (input) {
    /* No characters left; reverse the tokens because we've been prepending tokens */
    | [] => List.rev(tokens)
    | _ =>
      /* We're gonna process the head now and recursively process the remainder (tail) */
      let head = List.hd(input);
      let tail = List.tl(input);
      let next = tok(tail);

      let lookAheadEql = (withEq: token, plain: token) =>
        switch (tail) {
        | ['=', ...rem] => tok(rem, None, [withEq, ...tokens])
        | _ => next(None, [plain, ...tokens])
        };

      /* We're using a state machine to capture multi-character tokens like identifiers */
      switch (head, buffer, tokens) {
      /* Special characters: + - * / < <= > >= == != = ; , ( ) [ ] /* */ */

      /* Parentheses and braces */
      | ('(', None, t) => next(None, [LParen, ...t])
      | (')', None, t) => next(None, [RParen, ...t])
      | ('{', None, t) => next(None, [LBrace, ...t])
      | ('}', None, t) => next(None, [RBrace, ...t])
      /* Addition and subtraction */
      | ('+', None, t) => next(None, [Plus, ...t])
      | ('-', None, t) => next(None, [Minus, ...t])
      /* Multiplication and division */
      | ('*', None, t) => next(None, [Times, ...t])
      | ('/', None, t) => next(None, [Divide, ...t])
      /* Equality operators */
      | ('>', None, _) => lookAheadEql(GreaterThan, GreaterThanEql)
      | ('<', None, _) => lookAheadEql(LessThan, LessThanEql)
      | ('!', None, _) => lookAheadEql(NotEqual, Invalid("!"))
      | ('=', None, _) => lookAheadEql(Equal, Assignment)
      /* Delimiters */
      | (';', None, t) => next(None, [Semicolon, ...t])
      | (',', None, t) => next(None, [Comma, ...t])
      /* State: Numbers */
      | ('0'..'9' as i, None, t) =>
        next(Some(Number(String.make(1, i))), t)
      | ('0'..'9' as i, Some(Number(n)), t) =>
        let num = n ++ String.make(1, i);
        switch (tail) {
        | [] => next(None, [Integer(num), ...t]) /* 1 2 3 EOF */
        | ['.', ...rem] =>
          switch (rem) {
          | ['0'..'9' as j, ...r] =>
            tok(r, Some(Float(num ++ "." ++ String.make(1, j))), t)
          | _ => tok(rem, None, [Invalid("."), Integer(num), ...t]) /* 1 2 3 . EOF */
          }
        | _ => next(Some(Number(num)), t)
        };
      | ('0'..'9' as i, Some(Float(n)), t) =>
        let num = n ++ String.make(1, i)
        switch (tail) {
        | [] => next(None, [Float(num), ...t])
        | _ => next(Some(Float(num)), t)
        }
      /* Invalid token */
      | (a, _, t) => next(None, [Invalid(String.make(1, a)), ...t])
      };
    };
  };
  tok(explode(input), None, []);
};

let rec printTokens = (tokens: list(token)) => {
  let head = List.hd(tokens);
  let tail = List.tl(tokens);
  Js.log(
    switch (head) {
    | Integer(n) => "integer: " ++ n
    | Float(n) => "float: " ++ n
    | Invalid(n) => "invalid: " ++ n
    | _ => ""
    },
  );
  if (tail != []) {
    printTokens(tail);
  };
};

printTokens(lexer("123.4567"));

let xs = explode("123.4567");
let rec wtf = xs => {
  if (xs != []) {
    Js.log(String.make(1, List.hd(xs)));
    wtf(List.tl(xs));
  };
};
wtf(xs);