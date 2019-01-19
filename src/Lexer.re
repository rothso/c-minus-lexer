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
  | Ident(string)
  | Integer(int)
  | Float(float)
  | Keyword
  | Invalid(string);

type state =
  | Number(string)
  | PartialF(string)
  | Fractional(string)
  | String(string);

exception Todo(string);

let ($^) = (s, c) => s ++ String.make(1, c);
let atoi = int_of_string;
let atof = float_of_string;

let lexer = (input: string) => {
  let rec tok = (input, buffer, tokens) => {
    switch (input) {
    /* No characters left; empty the buffer and reverse the tokens because we've been prepending */
    | [] =>
      List.rev(
        switch (buffer) {
        | Some(Number(s)) => [Integer(atoi(s)), ...tokens]
        | Some(PartialF(s)) => [Invalid("."), Integer(atoi(s)), ...tokens]
        | Some(Fractional(s)) => [Float(atof(s)), ...tokens]
        | Some(String(s)) => [Ident(s), ...tokens]
        | None => tokens
        },
      )
    | _ =>
      /* We're gonna process the head now and recursively process the remainder (tail) */
      let head = List.hd(input);
      let tail = List.tl(input);
      let next = tok(tail);
      let curr = tok(input);

      let lookAhead = (char, found: token, notFound: token) =>
        switch (tail) {
        | [c, ...rem] when c == char => tok(rem, None, [found, ...tokens])
        | _ => next(None, [notFound, ...tokens])
        };

      /* We're using a state machine to capture multi-character tokens like identifiers */
      switch (buffer) {
      | None =>
        switch (head, tokens) {
        /* Parentheses and braces */
        | ('(', t) => next(None, [LParen, ...t])
        | (')', t) => next(None, [RParen, ...t])
        | ('{', t) => next(None, [LBrace, ...t])
        | ('}', t) => next(None, [RBrace, ...t])
        /* Addition and subtraction */
        | ('+', t) => next(None, [Plus, ...t])
        | ('-', t) => next(None, [Minus, ...t])
        /* Multiplication and division */
        | ('*', t) => next(None, [Times, ...t])
        | ('/', t) => next(None, [Divide, ...t])
        /* Equality operators */
        | ('>', _) => lookAhead('=', GreaterThan, GreaterThanEql)
        | ('<', _) => lookAhead('=', LessThan, LessThanEql)
        | ('!', _) => lookAhead('=', NotEqual, Invalid("!"))
        | ('=', _) => lookAhead('=', Equal, Assignment)
        /* Delimiters */
        | (';', t) => next(None, [Semicolon, ...t])
        | (',', t) => next(None, [Comma, ...t])
        /* Builders */
        | ('0'..'9' as i, t) => next(Some(Number(String.make(1, i))), t)
        | ('a'..'z' as a, t) => next(Some(String(String.make(1, a))), t)
        /* Error */
        | (a, t) => next(None, [Invalid(String.make(1, a)), ...t])
        }
      /* State: Valid numbers */
      | Some(Number(n)) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Number(n $^ i)), t)
        | ('.', t) => next(Some(PartialF(n)), t)
        | (_, t) => curr(None, [Integer(atoi(n)), ...t])
        }
      /* State: Numbers ending with a decimal */
      | Some(PartialF(n)) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Fractional(n ++ "." $^ i)), t)
        | (_, t) => curr(None, [Invalid("."), Integer(atoi(n)), ...t])
        }
      /* State: Valid floats */
      | Some(Fractional(f)) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Fractional(f $^ i)), t)
        | (_, t) => curr(None, [Float(atof(f)), ...t])
        }
      | Some(String(s)) =>
        switch (head, tokens) {
        | ('a'..'z' as i, t) => next(Some(String(s $^ i)), t)
        | (_, t) => curr(None, [Ident(s), ...t])
        }
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
    | Integer(n) => "integer: " ++ string_of_int(n)
    | Float(n) => "float: " ++ string_of_float(n)
    | Invalid(n) => "invalid: " ++ n
    | _ => ""
    },
  );
  if (tail != []) {
    printTokens(tail);
  };
};

/* printTokens(lexer("123.4567")); */

/* let xs = explode("123.4567");
   let rec wtf = xs =>
     if (xs != []) {
       Js.log(String.make(1, List.hd(xs)));
       wtf(List.tl(xs));
     }; */