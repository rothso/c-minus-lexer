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

  test("reads integers at the end", () =>
    expect(lexer("123")) |> toEqual([Integer(123)])
  );

  test("reads integers surrounded by parentheses", () =>
    expect(lexer("(123)")) |> toEqual([LParen, Integer(123), RParen])
  );

  test("reads floats at the end", () =>
    expect(lexer("123.12345")) |> toEqual([Float(123.12345)])
  );

  test("doesn't read improper float", () =>
    expect(lexer("123.")) |> toEqual([Integer(123), Invalid(".")])
  );

  test("skips improper float decimal", () =>
    expect(lexer("123.)"))
    |> toEqual([Integer(123), Invalid("."), RParen])
  );
});