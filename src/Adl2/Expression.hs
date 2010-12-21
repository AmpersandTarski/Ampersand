{-# OPTIONS_GHC -Wall #-}
module Adl2.Expression where


  data Expression r -- ^The basic Expression
     = Fi [(Expression r)] -- ^Intersect of expressions
     | Fu [(Expression r)] -- ^Union of expressions
     | Cp (Expression r)   -- ^Complement of an expression
     | Fj [(Expression r)] -- ^Join of expressions
     | Fd [(Expression r)] -- ^Dagger of expressions
     | Mph r               -- ^The basic relation
