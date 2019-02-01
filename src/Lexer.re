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
  | LBracket /* [ */
  | RBracket /* ] */
  | Ident(string)
  | Integer(int)
  | FloatingPoint(float)
  | Keyword(keyword)
  | Invalid(string);

type state =
  | Number(string)
  | Partial(string)
  | Fractional(string)
  | Partial2(string)
  | Partial3(string, char)
  | Scientific(string)
  | String(string)
  | Comment(int)
  | LineComment;

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

let rec dump = (state: state) =>
  switch (state) {
  | Number(s) => [Integer(int_of_string(s))]
  | Partial(s) => [Invalid("."), ...dump(Number(s))]
  | Fractional(s) => [FloatingPoint(float_of_string(s))]
  | Partial2(s) => [Invalid("E"), ...dump(Fractional(s))]
  | Partial3(s, c) => [Invalid(String.make(1, c)), ...dump(Partial2(s))]
  | Scientific(s) => [FloatingPoint(float_of_string(s))]
  | String(s) => [
      switch (StringMap.find(s, keywords)) {
      | exception Not_found => Ident(s)
      | keyword => Keyword(keyword)
      },
    ]
  | Comment(_) => []
  | LineComment => []
  };

/* Helper function (to keep the lines short) */
let ($^) = (s, c) => s ++ String.make(1, c);

let explode = (input: string): list(char) => {
  let rec exp = (i, chars) =>
    i >= 0 ? exp(i - 1, [input.[i], ...chars]) : chars;
  exp(String.length(input) - 1, []);
};

let tokenize = (~state=?, input: string) => {
  let rec tok = (input, buffer, tokens) => {
    switch (input) {
    /* No characters left; empty the buffer and reverse the tokens because we've been prepending */
    | [] => (
        List.rev(
          switch (buffer) {
          | Some(state) => dump(state) @ tokens
          | None => tokens
          },
        ),
        buffer,
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
        /* Parentheses, braces and brackets */
        | ('(', t) => next(None, [LParen, ...t])
        | (')', t) => next(None, [RParen, ...t])
        | ('{', t) => next(None, [LBrace, ...t])
        | ('}', t) => next(None, [RBrace, ...t])
        | ('[', t) => next(None, [LBracket, ...t])
        | (']', t) => next(None, [RBracket, ...t])
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
        | (('a'..'z' | 'A'..'Z') as a, t) => next(Some(String("" $^ a)), t)
        /* Error */
        | (a, t) => next(None, [Invalid(String.make(1, a)), ...t])
        }
      /* State: Valid numbers */
      | Some(Number(n) as state) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Number(n $^ i)), t)
        | ('.', t) => next(Some(Partial(n)), t)
        | ('E', t) => next(Some(Partial2(n)), t)
        | (_, t) => curr(None, dump(state) @ t)
        }
      /* State: Numbers ending with a decimal */
      | Some(Partial(n) as state) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Fractional(n ++ "." $^ i)), t)
        | (_, t) => curr(None, dump(state) @ t)
        }
      /* State: Valid floats */
      | Some(Fractional(f) as state) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Fractional(f $^ i)), t)
        | ('E', t) => next(Some(Partial2(f)), t)
        | (_, t) => curr(None, dump(state) @ t)
        }
      /* State: Floats ending in a E */
      | Some(Partial2(f) as state) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Scientific(f ++ "E" $^ i)), t)
        | (('+' | '-') as s, t) => next(Some(Partial3(f, s)), t)
        | (_, t) => curr(None, dump(state) @ t)
        }
      /* State: Scientific floats ending in a + or - */
      | Some(Partial3(f, s) as state) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) =>
          next(Some(Scientific(f ++ "E" $^ s $^ i)), t)
        | (_, t) => curr(None, dump(state) @ t)
        }
      /* State: Valid floats in scientific notation */
      | Some(Scientific(f) as state) =>
        switch (head, tokens) {
        | ('0'..'9' as i, t) => next(Some(Scientific(f $^ i)), t)
        | (_, t) => curr(None, dump(state) @ t)
        }
      /* State: Identifiers */
      | Some(String(s) as state) =>
        switch (head, tokens) {
        | (('a'..'z' | 'A'..'Z') as a, t) => next(Some(String(s $^ a)), t)
        | (_, t) => curr(None, dump(state) @ t)
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
  tok(explode(input), state, []);
};
