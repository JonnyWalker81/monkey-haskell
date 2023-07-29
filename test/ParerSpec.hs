{-# LANGUAGE QuasiQuotes #-}

module ParerSpec where

import Ast
import Lexer
import Parser
import Test.Syd
import Text.RawString.QQ
import qualified Token as T

spec :: Spec
spec =
  describe "Parser" $
    do
      it "parse let statement" $
        case parse
          ( tokenize
              [r|
                             let x = 5;
                             let y = 10;
                             let foobar = 838383;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.Let {Ast.name = Identifier "x", Ast.value = Ast.Integer 5},
                           Ast.Let {Ast.name = Identifier "y", Ast.value = Ast.Integer 10},
                           Ast.Let {Ast.name = Identifier "foobar", Ast.value = Ast.Integer 838383}
                         ]

      it
        "parse return statement"
        $ case parse
          ( tokenize
              [r|
                             return 5;
                             return 10;
                             return 993322;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.Return (Ast.Integer 5),
                           Ast.Return (Ast.Integer 10),
                           Ast.Return (Ast.Integer 993322)
                         ]

      it "parse identifier expression" $
        case parse
          ( tokenize
              [r|
                             foobar;
                             |]
          ) of
          Ast.Program s -> s `shouldBe` [ExpressionStatement (Ident (Identifier "foobar"))]

      it "parse number literal" $
        case parse
          ( tokenize
              [r|
                             5;
                             |]
          ) of
          Ast.Program s -> s `shouldBe` [Ast.ExpressionStatement (Ast.Integer 5)]

      it "parse bang and minus" $
        case parse
          ( tokenize
              [r|
                             !5;
                             -15;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.ExpressionStatement
                             Ast.Prefix {Ast.operator = T.Bang, Ast.right = Ast.Integer 5},
                           Ast.ExpressionStatement
                             Ast.Prefix {Ast.operator = T.Minus, Ast.right = Ast.Integer 15}
                         ]

      it "parse addition: 5 + 5;" $
        case parse
          ( tokenize
              [r|
                5 + 5;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.ExpressionStatement Ast.InfixExpr {Ast.left = Ast.Integer 5, Ast.operator = T.Plus, Ast.right = Ast.Integer 5}
                         ]
      it "parse addition: 5 - 5;" $
        case parse
          ( tokenize
              [r|
                5 - 5;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.ExpressionStatement Ast.InfixExpr {Ast.left = Ast.Integer 5, Ast.operator = T.Minus, Ast.right = Ast.Integer 5}
                         ]

      it "infix operators" $
        case parse
          ( tokenize
              [r|
                5 + 5;
                5 - 5;
                5 * 5;
                5 / 5;
                5 > 5;
                5 < 5;
                5 == 5;
                5 != 5;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.ExpressionStatement Ast.InfixExpr {Ast.left = Ast.Integer 5, Ast.operator = T.Plus, Ast.right = Ast.Integer 5},
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.Minus,
                                 Ast.right = Ast.Integer 5
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.Asterisk,
                                 Ast.right = Ast.Integer 5
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.Slash,
                                 Ast.right = Ast.Integer 5
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.GreaterThan,
                                 Ast.right = Ast.Integer 5
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.LessThan,
                                 Ast.right = Ast.Integer 5
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.Equal,
                                 Ast.right = Ast.Integer 5
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left = Ast.Integer 5,
                                 Ast.operator = T.NotEqual,
                                 Ast.right = Ast.Integer 5
                               }
                         ]

      it "bool literals" $
        case parse
          ( tokenize
              [r|
                true;
                false;
                3 > 5 == false;
                3 < 5 == true;
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Ast.ExpressionStatement (Ast.Boolean True),
                           Ast.ExpressionStatement (Ast.Boolean False),
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left =
                                   Ast.InfixExpr
                                     { Ast.left = Ast.Integer 3,
                                       Ast.operator = T.GreaterThan,
                                       Ast.right = Ast.Integer 5
                                     },
                                 Ast.operator = T.Equal,
                                 Ast.right = Ast.Boolean False
                               },
                           Ast.ExpressionStatement
                             Ast.InfixExpr
                               { Ast.left =
                                   Ast.InfixExpr
                                     { Ast.left = Ast.Integer 3,
                                       Ast.operator = T.LessThan,
                                       Ast.right = Ast.Integer 5
                                     },
                                 Ast.operator = T.Equal,
                                 Ast.right = Ast.Boolean True
                               }
                         ]

      it "parse grouped expression" $
        case parse
          ( tokenize
              [r|
                1 + (2 + 3) + 4;
                (5 + 5) * 2;
                1 + 2 + 3 + 4;
                1 + 2 + (2 * 5) - (6 / 8);
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ ExpressionStatement
                             InfixExpr
                               { left =
                                   InfixExpr
                                     { left = Integer 1,
                                       operator = T.Plus,
                                       right =
                                         InfixExpr
                                           { left = Integer 2,
                                             operator = T.Plus,
                                             right = Integer 3
                                           }
                                     },
                                 operator = T.Plus,
                                 right = Integer 4
                               },
                           ExpressionStatement
                             InfixExpr
                               { left =
                                   InfixExpr
                                     { left = Integer 5,
                                       operator = T.Plus,
                                       right = Integer 5
                                     },
                                 operator = T.Asterisk,
                                 right = Integer 2
                               },
                           ExpressionStatement
                             InfixExpr
                               { left =
                                   InfixExpr
                                     { left =
                                         InfixExpr
                                           { left = Integer 1,
                                             operator = T.Plus,
                                             right = Integer 2
                                           },
                                       operator = T.Plus,
                                       right = Integer 3
                                     },
                                 operator = T.Plus,
                                 right = Integer 4
                               },
                           ExpressionStatement
                             InfixExpr
                               { left =
                                   InfixExpr
                                     { left =
                                         InfixExpr
                                           { left = Integer 1,
                                             operator = T.Plus,
                                             right = Integer 2
                                           },
                                       operator = T.Plus,
                                       right =
                                         InfixExpr
                                           { left = Integer 2,
                                             operator = T.Asterisk,
                                             right = Integer 5
                                           }
                                     },
                                 operator = T.Minus,
                                 right =
                                   InfixExpr
                                     { left = Integer 6,
                                       operator = T.Slash,
                                       right = Integer 8
                                     }
                               }
                         ]

      it "parse if expression" $
        case parse
          ( tokenize
              [r|
                if (x < y) { x } else { y };
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ ExpressionStatement
                             If
                               { condition =
                                   InfixExpr
                                     { left = Ident (Identifier "x"),
                                       operator = T.LessThan,
                                       right = Ident (Identifier "y")
                                     },
                                 consequence =
                                   Block [ExpressionStatement (Ident (Identifier "x"))],
                                 alternative = Just (Block [ExpressionStatement (Ident (Identifier "y"))])
                               }
                         ]

      it "parse function expression" $
        case parse
          ( tokenize
              [r|
                fn(x, y) { x + y; };
                fn() {};
                fn(x) {};
                fn(x, y, z) {};
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ ExpressionStatement
                             FunctionLiteral
                               { parameters = [Identifier "x", Identifier "y"],
                                 body =
                                   Block
                                     [ ExpressionStatement
                                         InfixExpr
                                           { left = Ident (Identifier "x"),
                                             operator = T.Plus,
                                             right = Ident (Identifier "y")
                                           }
                                     ]
                               },
                           ExpressionStatement
                             FunctionLiteral {parameters = [], body = Block []},
                           ExpressionStatement
                             FunctionLiteral
                               { parameters = [Identifier "x"],
                                 body = Block []
                               },
                           ExpressionStatement
                             FunctionLiteral
                               { parameters = [Identifier "x", Identifier "y", Identifier "z"],
                                 body = Block []
                               }
                         ]
      it "parse call expression" $
        case parse
          ( tokenize
              [r|
                add(1, 2 * 3, 4 + 5);
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ ExpressionStatement
                             Ast.Call
                               { func = Ident (Identifier "add"),
                                 arguments =
                                   [ Integer 1,
                                     InfixExpr
                                       { left = Integer 2,
                                         operator = T.Asterisk,
                                         right = Integer 3
                                       },
                                     InfixExpr
                                       { left = Integer 4,
                                         operator = T.Plus,
                                         right = Integer 5
                                       }
                                   ]
                               }
                         ]

      it "parse large" $
        case parse
          ( tokenize
              [r|
                let five = 5;
let ten = 10;
   let add = fn(x, y) {
     x + y;
};
                             |]
          ) of
          Ast.Program s ->
            s
              `shouldBe` [ Let {name = Identifier "five", value = Integer 5},
                           Let {name = Identifier "ten", value = Integer 10},
                           Let
                             { name = Identifier "add",
                               value =
                                 FunctionLiteral
                                   { parameters = [Identifier "x", Identifier "y"],
                                     body =
                                       Block
                                         [ ExpressionStatement
                                             InfixExpr
                                               { left = Ident (Identifier "x"),
                                                 operator = T.Plus,
                                                 right = Ident (Identifier "y")
                                               }
                                         ]
                                   }
                             }
                         ]
