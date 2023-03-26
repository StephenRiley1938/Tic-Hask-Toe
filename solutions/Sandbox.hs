module Sandbox where
import Control.Exception (ArrayException(UndefinedElement))

myFifth :: [a] -> a
myFifth (a : (b : (c : (d : (e : _))))) = e
myFifth (a : _) = a



