{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMDocumentType@.
module ObjC.WebKit.DOMDocumentType
  ( DOMDocumentType
  , IsDOMDocumentType(..)
  , name
  , entities
  , notations
  , publicId
  , systemId
  , internalSubset
  , entitiesSelector
  , internalSubsetSelector
  , nameSelector
  , notationsSelector
  , publicIdSelector
  , systemIdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
name domDocumentType =
  sendMessage domDocumentType nameSelector

-- | @- entities@
entities :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id DOMNamedNodeMap)
entities domDocumentType =
  sendMessage domDocumentType entitiesSelector

-- | @- notations@
notations :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id DOMNamedNodeMap)
notations domDocumentType =
  sendMessage domDocumentType notationsSelector

-- | @- publicId@
publicId :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
publicId domDocumentType =
  sendMessage domDocumentType publicIdSelector

-- | @- systemId@
systemId :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
systemId domDocumentType =
  sendMessage domDocumentType systemIdSelector

-- | @- internalSubset@
internalSubset :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
internalSubset domDocumentType =
  sendMessage domDocumentType internalSubsetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @entities@
entitiesSelector :: Selector '[] (Id DOMNamedNodeMap)
entitiesSelector = mkSelector "entities"

-- | @Selector@ for @notations@
notationsSelector :: Selector '[] (Id DOMNamedNodeMap)
notationsSelector = mkSelector "notations"

-- | @Selector@ for @publicId@
publicIdSelector :: Selector '[] (Id NSString)
publicIdSelector = mkSelector "publicId"

-- | @Selector@ for @systemId@
systemIdSelector :: Selector '[] (Id NSString)
systemIdSelector = mkSelector "systemId"

-- | @Selector@ for @internalSubset@
internalSubsetSelector :: Selector '[] (Id NSString)
internalSubsetSelector = mkSelector "internalSubset"

