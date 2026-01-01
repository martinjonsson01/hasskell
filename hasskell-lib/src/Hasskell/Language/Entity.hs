module Hasskell.Language.Entity
  ( HasEntityId (..),
    HasKnownEntityId (..),
    HasReferencedEntities (..),
    HasDomain (..),
  )
where

import Data.Set
import Hasskell.HomeAssistant.API
import Hasskell.Language.CallStack

-- | Things that have an entity ID.
class HasEntityId a where
  idOf :: a -> EntityId

instance (HasEntityId a) => HasEntityId (Located a) where
  idOf (a :@ _) = idOf a

instance HasEntityId KnownEntityId where
  idOf = unwrapKnownEntityId

--------------------------------------------------------------------------------

-- | Things that have a known-verified entity ID.
class HasKnownEntityId a where
  knownIdOf :: a -> KnownEntityId

instance (HasKnownEntityId a) => HasKnownEntityId (Located a) where
  knownIdOf (a :@ _) = knownIdOf a

instance {-# OVERLAPPABLE #-} (HasKnownEntityId a) => HasEntityId a where
  idOf = unwrapKnownEntityId . knownIdOf

class HasReferencedEntities a where
  referencedEntitiesIn :: a -> Set EntityId

instance (HasReferencedEntities a) => HasReferencedEntities (Located a) where
  referencedEntitiesIn (a :@ _) = referencedEntitiesIn a

--------------------------------------------------------------------------------

-- | Things that is a part of a domain.
class HasDomain a where
  domainOf :: a -> HASSDomain

instance (HasDomain a) => HasDomain (Located a) where
  domainOf (a :@ _) = domainOf a
