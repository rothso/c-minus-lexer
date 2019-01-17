open Jest;
open Lexer;
describe("Lexer", () => {
  open Expect;
  open! Expect.Operators;

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
    expect(lexer("123.)")) |> toEqual([Integer(123), Invalid("."), RParen])
  );
});