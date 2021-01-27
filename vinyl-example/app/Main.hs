{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances,
  UndecidableInstances, StandaloneDeriving, MultiParamTypeClasses #-}
module Main where

import Database.Selda hiding ((:*:), from)
import qualified Database.Selda as S
import Database.Selda.Debug (compile)
import Database.Selda.SQLite
import Data.Vinyl
import Data.Vinyl.ARec
import Data.Vinyl.TypeLevel
import Data.Kind (Type)
import Data.Typeable (Typeable, Proxy(..))
import GHC.TypeLits
import GHC.Generics


type FieldRep s a =
  S1 ('MetaSel ('Just s)
               'NoSourceUnpackedness
               'SourceStrict
               'DecidedStrict)
     (Rec0 a)

type family RecRep rs :: Type -> Type where
  RecRep '[] = TypeError ('Text "Empty records are not supported by GenericRec")
  RecRep ((s ::: a) ': (fld2 ': rs)) =
    FieldRep s a :*: RecRep (fld2 ': rs)
  RecRep ((s ::: a) ': '[]) = FieldRep s a

class GenericRec r ( rs :: [(Symbol,*)] ) where
  fromRec :: r ElField rs -> RecRep rs x
  toRec :: RecRep rs x -> r ElField rs

instance (TypeError ('Text "Empty records are not supported by GenericRec"))
  => GenericRec r '[] where
  fromRec = error "unreachable"
  toRec = error "unreachable"

instance (GenericRec Rec (fld2 ': rs), KnownSymbol s)
  => GenericRec Rec ((s ::: a) ': (fld2 ': rs)) where
  fromRec (Field x :& xs@(_ :& _)) = M1 (K1 x) :*: fromRec xs
  {-# INLINE fromRec #-}
  toRec (M1 (K1 x) :*: xs) = Field x :& toRec xs
  {-# INLINE toRec #-}

instance (KnownSymbol s) => GenericRec Rec ((s ::: a) ': '[]) where
  fromRec (Field x :& RNil) = M1 (K1 x)
  {-# INLINE fromRec #-}
  toRec (M1 (K1 x)) = Field x :& RNil
  {-# INLINE toRec #-}

instance ( GenericRec Rec rs, RecApplicative rs, RPureConstrained (IndexableField rs) rs
         , NatToInt (RLength rs) )
  => GenericRec ARec rs where
  fromRec = fromRec . fromARec
    -- Normally this should get fully inlined and fromARec should be optimized
    -- away. If perf problems happen we should check
  {-# INLINE fromRec #-}
  toRec = toARec . toRec
  {-# INLINE toRec #-}

-- | We make this newtype in order to make a Generic instance for Rec that looks
-- exactly like the plain instance that would have been derived from the
-- equivalent classical Haskell record. It doesn't support empty records.
newtype GenRec r (rs :: [(Symbol,*)]) = GenRec { unGenRec :: r ElField rs }
-- deriving instance (RMap rs, RecordToList rs, ReifyConstraint Show ElField rs) => Show (GenRec r rs)

instance (GenericRec r rs) => Generic (GenRec r rs) where
  type Rep (GenRec r rs) =
    C1 ('MetaCons "GenRec" 'PrefixI 'True) (RecRep rs)
  from (GenRec r) = M1 (fromRec r)
  {-# INLINE from #-}
  to (M1 x) = GenRec $ toRec x
  {-# INLINE to #-}

instance (GenericRec r rs, Typeable r, Typeable rs, GSqlRow (RecRep rs))
  => SqlRow (GenRec r rs)



data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

type Person = GenRec ARec '["name" ::: Text, "age" ::: Int, "pet" ::: Maybe Pet] 

people :: Table Person
people = table "people" [#name :- primary]

main = withSQLite "people.sqlite" $ do
  tryCreateTable people
  insert_ people $ map (GenRec . toARec) $
    [ #name =: "Velvet"    :& #age =: 19 :& #pet =: Just Dog    :& RNil
    , #name =: "Kobayashi" :& #age =: 23 :& #pet =: Just Dragon :& RNil
    , #name =: "Miyu"      :& #age =: 10 :& #pet =: Nothing     :& RNil
    ]

  let theQuery = do
        person <- select people
        restrict (person ! #age .>= 18)
        return (person ! #name S.:*: person ! #pet)
  liftIO $ print $ compile theQuery
  adultsAndTheirPets <- query theQuery
  liftIO $ print adultsAndTheirPets
