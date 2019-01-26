type keyword =
  | If
  | Else
  | Int
  | Float
  | Void
  | While
  | Return;

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
  | FloatingPoint(float)
  | Keyword(keyword)
  | Invalid(string);

type state =
  | Number(string)
  | Partial(string)
  | Fractional(string)
  | String(string)
  | Comment(int)
  | LineComment;

exception Todo(string);

/* Helper functions (to keep the lines short) */
let ($^) = (s, c) => s ++ String.make(1, c);
let atoi = int_of_string;
let atof = float_of_string;

let explode = (input: string): list(char) => {
  let rec exp = (i, chars) =>
    i >= 0 ? exp(i - 1, [input.[i], ...chars]) : chars;
  exp(String.length(input) - 1, []);
};

module StringMap = Map.Make(String);

let keywords =
  StringMap.(
    empty
    |> add("if", If)
    |> add("else", Else)
    |> add("int", Int)
    |> add("float", Float)
    |> add("void", Void)
    |> add("while", While)
    |> add("return", Return)
  );

let identify = string =>
  switch (StringMap.find(string, keywords)) {
  | exception Not_found => Ident(string)
  | keyword => Keyword(keyword)
  };

let tokenize = (input: string) => {
  let rec tok = (input, buffer, tokens) => {
    switch (input) {
    /* No characters left; empty the buffer and reverse the tokens because we've been prepending */
    | [] =>
      List.rev(
        switch (buffer) {
        | Some(Number(s)) => [Integer(atoi(s)), ...tokens]
        | Some(Partial(s)) => [Invalid("."), Integer(atoi(s)), ...tokens]
        | Some(Fractional(s)) => [FloatingPoint(atof(s)), ...tokens]
        | Some(String(s)) => [identify(s), ...tokens]
        | Some(Comment(_) | LineComment) => tokens /* TODO throw exception */
        | None => tokens
        },
      )
    | _ =>
      /* We're gonna process the head now and recursively process the remainder (tail) */
      let head = List.hd(input);
      let tail = List.tl(input);
      let next = tok(tail);
      let curr = tok(input);

      let lookAheadEq = (found: token, notFound: token) =>
        switch (tail) {
        | ['=', ...rem] => tok(rem, None, [found, ...tokens])
        | _ => next(None, [notFound, ...tokens])
        };

      let lookAheadS = (char: char, state: option(state)) =>
        switch (tail) {
        | [c, ...rem] when c == char => tok(rem, state, tokens)
        | _ => next(buffer, tokens) /* stay in the same state */
        };

      /* We're using a state machine to capture multi-character tokens like identifiers */
      switch (buffer) {
      | None =>
        switch (head, tokens) {
        | (' ' | '\t' | '\n' | '\r', t) => next(None, t)
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
        | ('/', t) =>
          switch (tail) {
          | ['*', ...rem] => tok(rem, Some(Comment(1)), t)
          | ['/', ...rem] => tok(rem, Some(LineComment), t)
          | _ => next(None, [Divide, ...tokens])
          }
        /* Equality operators */
        | ('>', _) => lookAheadEq(GreaterThanEql, GreaterThan)
        | ('<', _) => lookAheadEq(LessThanEql, LessThan)
        | ('!', _) => lookAheadEq(NotEqual, Invalid("!"))
        | ('=', _) => lookAheadEq(Equal, Assignment)
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
        | ('.', t) => next(Some(Partial(n)), t)
        | (_, t) => curr(None, [Integer(atoi(n)), ...t])
        }
      /* State: Numbers ending with a decimal */
      | Some(Partial(n)) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Fractional(n ++ "." $^ i)), t)
        | (_, t) => curr(None, [Invalid("."), Integer(atoi(n)), ...t])
        }
      /* State: Valid floats */
      | Some(Fractional(f)) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Fractional(f $^ i)), t)
        | (_, t) => curr(None, [FloatingPoint(atof(f)), ...t])
        }
      /* State: Identifiers */
      | Some(String(s)) =>
        switch (head, tokens) {
        | ('a'..'z' as i, t) => next(Some(String(s $^ i)), t)
        | (_, t) => curr(None, [identify(s), ...t])
        }
      /* State: Block comments (can be nested) */
      | Some(Comment(i)) as state =>
        switch (head, tokens) {
        | ('/', _) => lookAheadS('*', Some(Comment(i + 1)))
        | ('*', _) => lookAheadS('/', i == 1 ? None : Some(Comment(i - 1)))
        | (_, t) => next(state, t)
        }
      /* State: Line comments */
      | Some(LineComment) =>
        switch (head, tokens) {
        | ('\n', t) => next(None, t)
        | (_, t) => next(Some(LineComment), t)
        }
      };
    };
  };
  tok(explode(input), None, []);
};
