{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to set attributes on an item.
--
-- Methods that take attributes use this type to receive attribute values and to indicate which attributes they support. The various members of the parent type, ``FSItemAttributes``, contain the values of the attributes to set.
--
-- Modify the ``consumedAttributes`` property to indicate which attributes your file system successfully used. FSKit calls the ``wasAttributeConsumed(_:)`` method to determine whether the file system successfully used a given attribute. Only set the attributes that your file system supports.
--
-- Generated bindings for @FSItemSetAttributesRequest@.
module ObjC.FSKit.FSItemSetAttributesRequest
  ( FSItemSetAttributesRequest
  , IsFSItemSetAttributesRequest(..)
  , wasAttributeConsumed
  , consumedAttributes
  , setConsumedAttributes
  , consumedAttributesSelector
  , setConsumedAttributesSelector
  , wasAttributeConsumedSelector

  -- * Enum types
  , FSItemAttribute(FSItemAttribute)
  , pattern FSItemAttributeType
  , pattern FSItemAttributeMode
  , pattern FSItemAttributeLinkCount
  , pattern FSItemAttributeUID
  , pattern FSItemAttributeGID
  , pattern FSItemAttributeFlags
  , pattern FSItemAttributeSize
  , pattern FSItemAttributeAllocSize
  , pattern FSItemAttributeFileID
  , pattern FSItemAttributeParentID
  , pattern FSItemAttributeAccessTime
  , pattern FSItemAttributeModifyTime
  , pattern FSItemAttributeChangeTime
  , pattern FSItemAttributeBirthTime
  , pattern FSItemAttributeBackupTime
  , pattern FSItemAttributeAddedTime
  , pattern FSItemAttributeSupportsLimitedXAttrs
  , pattern FSItemAttributeInhibitKernelOffloadedIO

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A method that indicates whether the file system used the given attribute.
--
-- - Parameter attribute: The ``FSItemAttribute`` to check.
--
-- ObjC selector: @- wasAttributeConsumed:@
wasAttributeConsumed :: IsFSItemSetAttributesRequest fsItemSetAttributesRequest => fsItemSetAttributesRequest -> FSItemAttribute -> IO Bool
wasAttributeConsumed fsItemSetAttributesRequest attribute =
  sendMessage fsItemSetAttributesRequest wasAttributeConsumedSelector attribute

-- | The attributes successfully used by the file system.
--
-- This property is a bit field in Objective-C and an <doc://com.apple.documentation/documentation/Swift/OptionSet> in Swift.
--
-- ObjC selector: @- consumedAttributes@
consumedAttributes :: IsFSItemSetAttributesRequest fsItemSetAttributesRequest => fsItemSetAttributesRequest -> IO FSItemAttribute
consumedAttributes fsItemSetAttributesRequest =
  sendMessage fsItemSetAttributesRequest consumedAttributesSelector

-- | The attributes successfully used by the file system.
--
-- This property is a bit field in Objective-C and an <doc://com.apple.documentation/documentation/Swift/OptionSet> in Swift.
--
-- ObjC selector: @- setConsumedAttributes:@
setConsumedAttributes :: IsFSItemSetAttributesRequest fsItemSetAttributesRequest => fsItemSetAttributesRequest -> FSItemAttribute -> IO ()
setConsumedAttributes fsItemSetAttributesRequest value =
  sendMessage fsItemSetAttributesRequest setConsumedAttributesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wasAttributeConsumed:@
wasAttributeConsumedSelector :: Selector '[FSItemAttribute] Bool
wasAttributeConsumedSelector = mkSelector "wasAttributeConsumed:"

-- | @Selector@ for @consumedAttributes@
consumedAttributesSelector :: Selector '[] FSItemAttribute
consumedAttributesSelector = mkSelector "consumedAttributes"

-- | @Selector@ for @setConsumedAttributes:@
setConsumedAttributesSelector :: Selector '[FSItemAttribute] ()
setConsumedAttributesSelector = mkSelector "setConsumedAttributes:"

