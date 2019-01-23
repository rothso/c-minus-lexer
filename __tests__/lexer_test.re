open Jest;
open Lexer;
describe("Lexer", () => {
  open Expect;
  open! Expect.Operators;

  test("lexes Eggen's sample input", () => {
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
       }|};
    expect(lexer(input))
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
       ]);
  });

  test("ignores comments", () =>
    expect(lexer("1/*comment*/1")) |> toEqual([Integer(1), Integer(1)])
  );

  test("ignores comments containing spaces", () =>
    expect(lexer("/**/          /*/* */   */")) |> toEqual([])
  );

  test("ignores nested comments", () =>
    expect(lexer("1/*co/*e*/nt*/1")) |> toEqual([Integer(1), Integer(1)])
  );

  test("ignores multiline comments", () => {
    let str = {|/**************/
                /*************************
                i = 333;        ******************/|};
    expect(lexer(str)) |> toEqual([]);
  });

  test("ignores line comments", () =>
    expect(lexer("1
    // ignore me
    2")) |> toEqual([Integer(1), Integer(2)])
  );

  test("reads integers at the end of the string", () =>
    expect(lexer("123")) |> toEqual([Integer(123)])
  );

  test("reads integers surrounded by parentheses", () =>
    expect(lexer("(123)")) |> toEqual([LParen, Integer(123), RParen])
  );

  test("reads floats at the end of the string", () =>
    expect(lexer("123.12345")) |> toEqual([FloatingPoint(123.12345)])
  );

  test("doesn't read improper floats", () =>
    expect(lexer("123.")) |> toEqual([Integer(123), Invalid(".")])
  );

  test("skips dangling float decimal", () =>
    expect(lexer("1.)")) |> toEqual([Integer(1), Invalid("."), RParen])
  );

  test("recognizes keywords", () =>
    expect(lexer("int float void while if else return"))
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
    expect(lexer("a bba")) |> toEqual([Ident("a"), Ident("bba")])
  );

  test("reads identifiers followed by numbers", () =>
    expect(lexer("abc99")) |> toEqual([Ident("abc"), Integer(99)])
  );
});
