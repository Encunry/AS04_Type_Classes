{-# LANGUAGE InstanceSigs #-}
module TypeClasses.Person where

data Person = MkPerson { nr:: Int, email :: String }

instance Show Person where
    show :: Person -> String
    show (MkPerson nr1 email1) = "nr: " ++ show nr1 ++ ", email: " ++ email1

instance Eq Person where
    (==) :: Person -> Person -> Bool
    (==) (MkPerson nr1 _) (MkPerson nr2 _) = nr1 == nr2

instance Ord Person where
    (<=) :: Person -> Person -> Bool
    (<=) (MkPerson nr1 _) (MkPerson nr2 _) = nr1 <= nr2