module Halogen.Form.Projective where

import Prologue

import Control.Bind (bindFlipped)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Identity (Identity(..))
import Data.Lens (preview, view)
import Data.List (List)
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LLNE
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Profunctor.Choice ((+++))
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong ((***))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Traversable, traverse)
import Halogen.Form.Types
  ( FieldState(..)
  , InitializeField(..)
  , _FromOutput
  , _fieldOutput
  )
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | Class for projecting a result from a structure. Laws:
-- |
-- |  ```
-- |  project blank == Nothing
-- |  ```
-- |
class Projective r a | a -> r where
  project :: a -> Maybe r
  blank :: a

instance Monoid input => Projective output (FieldState input output) where
  project = view _fieldOutput
  blank = FieldState mempty Nothing

instance Projective output (InitializeField input output) where
  project = preview _FromOutput
  blank = FromBlank

instance
  ( Projective r1 a
  , Projective r2 b
  ) =>
  Projective (Tuple r1 r2) (Tuple a b) where
  project = unwrap $ (Star project) *** (Star project)
  blank = Tuple blank blank

instance
  ( Projective r1 a
  , Projective r2 b
  ) =>
  Projective (Either r1 r2) (Either a b) where
  project = unwrap $ (Star project) +++ (Star project)
  blank = Left blank

instance Projective r a => Projective r (Identity a) where
  project = project <<< unwrap
  blank = Identity blank

instance Projective r a => Projective r (Maybe a) where
  project = bindFlipped project
  blank = Nothing

instance Projective r a => Projective (Array r) (Array a) where
  project = traversableProject
  blank = mempty

instance Projective r a => Projective (NonEmptyArray r) (NonEmptyArray a) where
  project = traversableProject
  blank = pure blank

instance Projective r a => Projective (List r) (List a) where
  project = traversableProject
  blank = mempty

instance Projective r a => Projective (NonEmptyList r) (NonEmptyList a) where
  project = traversableProject
  blank = pure blank

instance Projective r a => Projective (LL.List r) (LL.List a) where
  project = traversableProject
  blank = mempty

instance
  Projective r a =>
  Projective (LLNE.NonEmptyList r) (LLNE.NonEmptyList a) where
  project = traversableProject
  blank = pure blank

instance (Ord r, Ord a, Projective r a) => Projective (Set r) (Set a) where
  project =
    map (Set.fromFoldable :: Array r -> Set r)
      <<< traversableProject
      <<< Set.toUnfoldable
  blank = mempty

instance Projective r a => Projective (Map k r) (Map k a) where
  project = traversableProject
  blank = Map.empty

traversableProject
  :: forall t r a. Projective r a => Traversable t => t a -> Maybe (t r)
traversableProject = traverse project

class ProjectiveRecord (rl :: RowList Type) rip rib ro | rl -> rip rib ro where
  projectRecord :: Proxy rl -> { | rip } -> Maybe (Builder {} { | ro })
  blankRecord :: Proxy rl -> Builder {} { | rib }

instance
  ( IsSymbol label
  , Row.Lacks label ro'
  , Row.Lacks label rib'
  , Row.Cons label a rip' rip
  , Row.Cons label a rib' rib
  , Row.Cons label r ro' ro
  , ProjectiveRecord rl rip rib' ro'
  , Projective r a
  ) =>
  ProjectiveRecord (RL.Cons label a rl) rip rib ro where
  projectRecord _ ri =
    (compose <<< Builder.insert label)
      <$> project (Record.get label ri)
      <*> projectRecord (Proxy :: _ rl) ri
    where
    label = Proxy :: _ label
  blankRecord _ =
    Builder.insert (Proxy :: _ label) (blank :: a)
      <<< blankRecord (Proxy :: _ rl)

-- | Note: unlawful instance. project blank == Just {}
-- | Needed for record instances to resolve though.
instance ProjectiveRecord RL.Nil rip () () where
  projectRecord _ _ = pure identity
  blankRecord _ = identity

instance
  ( RowToList ri rl
  , ProjectiveRecord rl ri ri ro
  ) =>
  Projective { | ro } { | ri } where
  project ri = Builder.buildFromScratch <$> projectRecord (Proxy :: _ rl) ri
  blank = Builder.buildFromScratch $ blankRecord (Proxy :: _ rl)
