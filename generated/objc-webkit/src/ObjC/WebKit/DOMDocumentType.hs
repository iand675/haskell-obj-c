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
  , nameSelector
  , entitiesSelector
  , notationsSelector
  , publicIdSelector
  , systemIdSelector
  , internalSubsetSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
name domDocumentType  =
  sendMsg domDocumentType (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- entities@
entities :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id DOMNamedNodeMap)
entities domDocumentType  =
  sendMsg domDocumentType (mkSelector "entities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notations@
notations :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id DOMNamedNodeMap)
notations domDocumentType  =
  sendMsg domDocumentType (mkSelector "notations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- publicId@
publicId :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
publicId domDocumentType  =
  sendMsg domDocumentType (mkSelector "publicId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- systemId@
systemId :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
systemId domDocumentType  =
  sendMsg domDocumentType (mkSelector "systemId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- internalSubset@
internalSubset :: IsDOMDocumentType domDocumentType => domDocumentType -> IO (Id NSString)
internalSubset domDocumentType  =
  sendMsg domDocumentType (mkSelector "internalSubset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @entities@
entitiesSelector :: Selector
entitiesSelector = mkSelector "entities"

-- | @Selector@ for @notations@
notationsSelector :: Selector
notationsSelector = mkSelector "notations"

-- | @Selector@ for @publicId@
publicIdSelector :: Selector
publicIdSelector = mkSelector "publicId"

-- | @Selector@ for @systemId@
systemIdSelector :: Selector
systemIdSelector = mkSelector "systemId"

-- | @Selector@ for @internalSubset@
internalSubsetSelector :: Selector
internalSubsetSelector = mkSelector "internalSubset"

