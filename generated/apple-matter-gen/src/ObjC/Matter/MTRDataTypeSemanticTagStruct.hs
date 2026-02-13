{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeSemanticTagStruct@.
module ObjC.Matter.MTRDataTypeSemanticTagStruct
  ( MTRDataTypeSemanticTagStruct
  , IsMTRDataTypeSemanticTagStruct(..)
  , mfgCode
  , setMfgCode
  , namespaceID
  , setNamespaceID
  , tag
  , setTag
  , label
  , setLabel
  , labelSelector
  , mfgCodeSelector
  , namespaceIDSelector
  , setLabelSelector
  , setMfgCodeSelector
  , setNamespaceIDSelector
  , setTagSelector
  , tagSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mfgCode@
mfgCode :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSNumber)
mfgCode mtrDataTypeSemanticTagStruct =
  sendMessage mtrDataTypeSemanticTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSNumber value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setMfgCode mtrDataTypeSemanticTagStruct value =
  sendMessage mtrDataTypeSemanticTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- namespaceID@
namespaceID :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSNumber)
namespaceID mtrDataTypeSemanticTagStruct =
  sendMessage mtrDataTypeSemanticTagStruct namespaceIDSelector

-- | @- setNamespaceID:@
setNamespaceID :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSNumber value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setNamespaceID mtrDataTypeSemanticTagStruct value =
  sendMessage mtrDataTypeSemanticTagStruct setNamespaceIDSelector (toNSNumber value)

-- | @- tag@
tag :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSNumber)
tag mtrDataTypeSemanticTagStruct =
  sendMessage mtrDataTypeSemanticTagStruct tagSelector

-- | @- setTag:@
setTag :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSNumber value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setTag mtrDataTypeSemanticTagStruct value =
  sendMessage mtrDataTypeSemanticTagStruct setTagSelector (toNSNumber value)

-- | @- label@
label :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSString)
label mtrDataTypeSemanticTagStruct =
  sendMessage mtrDataTypeSemanticTagStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSString value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setLabel mtrDataTypeSemanticTagStruct value =
  sendMessage mtrDataTypeSemanticTagStruct setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector '[] (Id NSNumber)
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector '[Id NSNumber] ()
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @namespaceID@
namespaceIDSelector :: Selector '[] (Id NSNumber)
namespaceIDSelector = mkSelector "namespaceID"

-- | @Selector@ for @setNamespaceID:@
setNamespaceIDSelector :: Selector '[Id NSNumber] ()
setNamespaceIDSelector = mkSelector "setNamespaceID:"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] (Id NSNumber)
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[Id NSNumber] ()
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

