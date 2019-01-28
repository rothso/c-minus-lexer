open Jest;
open Lexer;

/* Wrap the tokenize function and discard the output state */
let tokenize = (input: string) =>
  switch (tokenize(input)) {
  | (tokens, _) => tokens
  };

describe("Lexer", () => {
  open Expect;
  open! Expect.Operators;

  test("tokenizes Eggen's sample input", () => {
    let input = {|
      /**/          /*/* */   */
      /*/*/****This**********/*/    */
      /**************/
      /*************************
      i = 333;        ******************/

      iiii = 3@33;

      int g 4 cd (int u, int v)      {
      if(v == >= 0) return/*a comment*/ u;
      else ret_urn gcd(vxxxxxxvvvvv, u-u/v*v);
            /* u-u/v*v == u mod v*/
      !
      }

      return void while       void main()|};
    expect(tokenize(input))
    |> toEqual([
         Ident("iiii"),
         Assignment,
         Integer(3),
         Invalid("@"),
         Integer(33),
         Semicolon,
         Keyword(Int),
         Ident("g"),
         Integer(4),
         Ident("cd"),
         LParen,
         Keyword(Int),
         Ident("u"),
         Comma,
         Keyword(Int),
         Ident("v"),
         RParen,
         LBrace,
         Keyword(If),
         LParen,
         Ident("v"),
         Equal,
         GreaterThanEql,
         Integer(0),
         RParen,
         Keyword(Return),
         Ident("u"),
         Semicolon,
         Keyword(Else),
         Ident("ret"),
         Invalid("_"),
         Ident("urn"),
         Ident("gcd"),
         LParen,
         Ident("vxxxxxxvvvvv"),
         Comma,
         Ident("u"),
         Minus,
         Ident("u"),
         Divide,
         Ident("v"),
         Times,
         Ident("v"),
         RParen,
         Semicolon,
         Invalid("!"),
         RBrace,
         Keyword(Return),
         Keyword(Void),
         Keyword(While),
         Keyword(Void),
         Ident("main"),
         LParen,
         RParen,
       ]);
  });

  test("ignores whitespace", () =>
    expect(tokenize(" 1\t1\n\r ")) |> toEqual([Integer(1), Integer(1)])
  );

  test("ignores comments", () =>
    expect(tokenize("1/*comment*/1")) |> toEqual([Integer(1), Integer(1)])
  );

  test("ignores comments containing spaces", () =>
    expect(tokenize("/**/          /*/* */   */")) |> toEqual([])
  );

  test("ignores nested comments", () =>
    expect(tokenize("1/*co/*e*/nt*/1"))
    |> toEqual([Integer(1), Integer(1)])
  );

  test("ignores multiline comments", () => {
    let str = {|/**************/
                /*************************
                i = 333;        ******************/|};
    expect(tokenize(str)) |> toEqual([]);
  });

  test("ignores line comments", () =>
    expect(tokenize("1
    // ignore me
    2"))
    |> toEqual([Integer(1), Integer(2)])
  );

  test("reads integers surrounded by parentheses", () =>
    expect(tokenize("(123)")) |> toEqual([LParen, Integer(123), RParen])
  );

  test("reads floats at the end of the string", () =>
    expect(tokenize("123.12345")) |> toEqual([FloatingPoint(123.12345)])
  );

  test("doesn't read float with trailing dot", () =>
    expect(tokenize("123.")) |> toEqual([Integer(123), Invalid(".")])
  );

  test("doesn't read float with trailing E", () =>
    expect(tokenize("4.0E"))
    |> toEqual([FloatingPoint(4.0), Invalid("E")])
  );

  test("doesn't read float without leading number", () =>
    expect(tokenize(".5")) |> toEqual([Invalid("."), Integer(5)])
  );

  test("doesn't read .E as float", () =>
    expect(tokenize(".E")) |> toEqual([Invalid("."), Ident("E")])
  );

  test("doesn't read incomplete scientific notation float", () =>
    expect(tokenize("6.0E+"))
    |> toEqual([FloatingPoint(6.0), Invalid("E"), Invalid("+")])
  );

  test("reads integers in scientific notation as floats", () =>
    expect(tokenize("1E2")) |> toEqual([FloatingPoint(1E2)])
  );

  test("reads floats in scientific notation", () =>
    expect(tokenize("1.2E2")) |> toEqual([FloatingPoint(120.0)])
  );

  test("reads floats in signed scientific notation", () =>
    expect(tokenize("120.0E-2")) |> toEqual([FloatingPoint(1.2)])
  );

  test("skips dangling float decimal", () =>
    expect(tokenize("1.)")) |> toEqual([Integer(1), Invalid("."), RParen])
  );

  test("recognizes keywords", () =>
    expect(tokenize("int float void while if else return"))
    |> toEqual([
         Keyword(Int),
         Keyword(Float),
         Keyword(Void),
         Keyword(While),
         Keyword(If),
         Keyword(Else),
         Keyword(Return),
       ])
  );

  test("reads identifiers", () =>
    expect(tokenize("a bba")) |> toEqual([Ident("a"), Ident("bba")])
  );

  test("reads identifiers containing capital letters", () =>
    expect(tokenize("aBC AAA")) |> toEqual([Ident("aBC"), Ident("AAA")])
  );

  test("reads identifiers followed by numbers", () =>
    expect(tokenize("abc99")) |> toEqual([Ident("abc"), Integer(99)])
  );
});
