open Jest;
open Lexer;
describe("Lexer", () => {
  open Expect;
  open! Expect.Operators;

  /* test("parses exampe 1", () => {
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
       expect(lexer(input)) |> toEqual([Integer(123)]);
     }); */

  test("ignores comments", () =>
    expect(lexer("1/*comment*/1")) |> toEqual([Integer(1), Integer(1)])
  );

  test("ignores comments with spaces", () =>
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

  test("reads integers at the end", () =>
    expect(lexer("123")) |> toEqual([Integer(123)])
  );

  test("reads integers surrounded by parentheses", () =>
    expect(lexer("(123)")) |> toEqual([LParen, Integer(123), RParen])
  );

  test("reads floats at the end", () =>
    expect(lexer("123.12345")) |> toEqual([FloatingPoint(123.12345)])
  );

  test("doesn't read improper float", () =>
    expect(lexer("123.")) |> toEqual([Integer(123), Invalid(".")])
  );

  test("skips improper float decimal", () =>
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

  test("recognizes identifiers", () =>
    expect(lexer("a bba")) |> toEqual([Ident("a"), Ident("bba")])
  );

  test("splits numbers from identifiers", () =>
    expect(lexer("abc99")) |> toEqual([Ident("abc"), Integer(99)])
  );
});
