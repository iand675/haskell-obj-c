{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetMessageAttributeIntent@.
module ObjC.Intents.INSetMessageAttributeIntent
  ( INSetMessageAttributeIntent
  , IsINSetMessageAttributeIntent(..)
  , initWithIdentifiers_attribute
  , identifiers
  , attribute
  , attributeSelector
  , identifiersSelector
  , initWithIdentifiers_attributeSelector

  -- * Enum types
  , INMessageAttribute(INMessageAttribute)
  , pattern INMessageAttributeUnknown
  , pattern INMessageAttributeRead
  , pattern INMessageAttributeUnread
  , pattern INMessageAttributeFlagged
  , pattern INMessageAttributeUnflagged
  , pattern INMessageAttributePlayed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifiers:attribute:@
initWithIdentifiers_attribute :: (IsINSetMessageAttributeIntent inSetMessageAttributeIntent, IsNSArray identifiers) => inSetMessageAttributeIntent -> identifiers -> INMessageAttribute -> IO (Id INSetMessageAttributeIntent)
initWithIdentifiers_attribute inSetMessageAttributeIntent identifiers attribute =
  sendOwnedMessage inSetMessageAttributeIntent initWithIdentifiers_attributeSelector (toNSArray identifiers) attribute

-- | @- identifiers@
identifiers :: IsINSetMessageAttributeIntent inSetMessageAttributeIntent => inSetMessageAttributeIntent -> IO (Id NSArray)
identifiers inSetMessageAttributeIntent =
  sendMessage inSetMessageAttributeIntent identifiersSelector

-- | @- attribute@
attribute :: IsINSetMessageAttributeIntent inSetMessageAttributeIntent => inSetMessageAttributeIntent -> IO INMessageAttribute
attribute inSetMessageAttributeIntent =
  sendMessage inSetMessageAttributeIntent attributeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifiers:attribute:@
initWithIdentifiers_attributeSelector :: Selector '[Id NSArray, INMessageAttribute] (Id INSetMessageAttributeIntent)
initWithIdentifiers_attributeSelector = mkSelector "initWithIdentifiers:attribute:"

-- | @Selector@ for @identifiers@
identifiersSelector :: Selector '[] (Id NSArray)
identifiersSelector = mkSelector "identifiers"

-- | @Selector@ for @attribute@
attributeSelector :: Selector '[] INMessageAttribute
attributeSelector = mkSelector "attribute"

