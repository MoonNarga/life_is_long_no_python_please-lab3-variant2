{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MathExpression
  ( eval,
    tokenizer,
  )
  where

import qualified Data.Char
import Text.Regex.Posix ()

data Token
  = Num Int
  | TPlus
  | TSub
  | TMul
  | TDiv
  deriving (Show)

getPrecedence :: Token -> Int
getPrecedence TPlus = 1
getPrecedence TSub = 1
getPrecedence TMul = 2
getPrecedence TDiv = 2
-- getPrecedence (Num _) = 0

fixString :: String -> String
fixString "" = ""
fixString (ch: rest)
  | ch == ' ' = fixString rest
  | otherwise = ch: fixString rest

_tokenizer :: String -> String -> [Token] -> [Token] -- 尾递归写法
_tokenizer "" [] previous = previous -- 所有字符都解析完毕
_tokenizer "" buf previous = Num (read $ reverse buf) : previous -- 字符都解析完毕，但是缓冲区还有数据
_tokenizer (ch : expr) buf previous
  | Data.Char.isDigit ch = _tokenizer expr (ch : buf) previous -- 把数字放入缓冲区
  | otherwise = -- 当前字符不是数字
    case buf of -- 检查缓冲区是否为空
      [] ->
        -- 缓冲区为空
        case ch of -- 解析当前字符
          '+' -> _tokenizer expr [] (TPlus : previous)
          '-' -> _tokenizer expr [] (TSub : previous)
          '*' -> _tokenizer expr [] (TMul : previous)
          '/' -> _tokenizer expr [] (TDiv : previous)
          _ -> error "illegal operational char"
      _ -> _tokenizer (ch : expr) [] (Num (read $ reverse buf) : previous) -- 缓冲区不为空，读取缓冲区字符

tokenizer :: String -> [Token]
tokenizer expr = _tokenizer (fixString expr) [] []

-- 定义操作符运算
evalOp :: Token -> Int -> Int -> Int
evalOp TPlus a b = a + b
evalOp TSub a b = a - b
evalOp TMul a b = a * b
evalOp TDiv a b = a `div` b

-- 搞一个好看点的包装函数
eval :: [Token] -> Int
eval ((Num value):tokens) = _eval tokens [value] []

-- 真正运作的函数在这里
_eval :: [Token] -> [Int] -> [Token] -> Int
-- 操作数栈里面只有一个数了，就是我们要求的值了
_eval [] [value] _ = value
-- 操作符栈数值为空，进行SHIFT操作
_eval (op:(Num num):tokens) numStack [] =
    _eval tokens (num:numStack) [op]
-- 操作符栈不为空
_eval (op:(Num num):tokens) numStack (topOp:opStack) =
    if getPrecedence op > getPrecedence topOp then
        _eval tokens (num:numStack) (op:topOp:opStack) -- SHIFT
    else -- REDUCE
        case numStack of
            (num1:num2:stack) ->
                _eval (op:Num num:tokens) (evalOp topOp num1 num2:stack) opStack
-- 栈里还有一些残留的值，继续运算
_eval [] (num1:num2:numStack) (topOp:opStack) =
    _eval [] (evalOp topOp num1 num2:numStack) opStack