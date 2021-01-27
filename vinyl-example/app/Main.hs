{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, TypeOperators, DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Main where

import Database.Selda hiding ((:*:), from)
import qualified Database.Selda as S
import Database.Selda.SQLite
import Data.Vinyl
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits
import GHC.Generics


-- | We make this newtype in order to make a Generic instance for Rec that looks
-- exactly like the plain instance that would have been derived from the
-- equivalent classical Haskell record. It doesn't support empty records.
newtype GenRec rs = GenRec { unGenRec :: Rec ElField rs }

class GenericRec ( rs :: [(Symbol,*)] ) where
  type RecRep rs :: Type -> Type

  fromRec :: Rec ElField rs -> RecRep rs x
  toRec :: RecRep rs x -> Rec ElField rs

instance (TypeError ('Text "Empty records are not supported by GenRec")) => GenericRec '[] where
  type RecRep '[] = RecRep '[ "error" ::: () ]
  fromRec = error "unreachable"
  toRec = error "unreachable"

type FieldRep s a =
  S1 ('MetaSel ('Just s)
               'NoSourceUnpackedness
               'SourceStrict
               'DecidedStrict)
     (Rec0 a)

instance (GenericRec (fld2 ': rs), KnownSymbol s)
  => GenericRec ((s ::: a) ': (fld2 ': rs)) where
  type RecRep ((s ::: a) ': (fld2 ': rs)) =
    FieldRep s a :*: RecRep (fld2 ': rs)
  fromRec (Field x :& xs@(_ :& _)) = M1 (K1 x) :*: fromRec xs
  toRec (M1 (K1 x) :*: xs) = Field x :& toRec xs

instance (KnownSymbol s) => GenericRec ((s ::: a) ': '[]) where
  type RecRep ((s ::: a) ': '[]) = FieldRep s a
  fromRec (Field x :& RNil) = M1 (K1 x)
  toRec (M1 (K1 x)) = Field x :& RNil

instance (GenericRec rs) => Generic (GenRec rs) where
  type Rep (GenRec rs) =
    C1 ('MetaCons "GenRec" 'PrefixI 'True) (RecRep rs)
  from (GenRec r) = M1 (fromRec r)
  to (M1 x) = GenRec $ toRec x

instance (GenericRec rs, Typeable rs, GSqlRow (RecRep rs)) => SqlRow (GenRec rs)



data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

type Person = GenRec '["name" ::: Text, "age" ::: Int, "pet" ::: Maybe Pet] 

people :: Table Person
people = table "people" [#name :- primary]

main = withSQLite "people.sqlite" $ do
  tryCreateTable people
  insert_ people $ map GenRec $
    [ #name =: "Velvet"    :& #age =: 19 :& #pet =: Just Dog    :& RNil
    , #name =: "Kobayashi" :& #age =: 23 :& #pet =: Just Dragon :& RNil
    , #name =: "Miyu"      :& #age =: 10 :& #pet =: Nothing     :& RNil
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name S.:*: person ! #pet)
  liftIO $ print adultsAndTheirPets
