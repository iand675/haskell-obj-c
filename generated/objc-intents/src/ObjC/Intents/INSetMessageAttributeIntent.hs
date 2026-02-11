{-# LANGUAGE PatternSynonyms #-}
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
  , initWithIdentifiers_attributeSelector
  , identifiersSelector
  , attributeSelector

  -- * Enum types
  , INMessageAttribute(INMessageAttribute)
  , pattern INMessageAttributeUnknown
  , pattern INMessageAttributeRead
  , pattern INMessageAttributeUnread
  , pattern INMessageAttributeFlagged
  , pattern INMessageAttributeUnflagged
  , pattern INMessageAttributePlayed

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifiers:attribute:@
initWithIdentifiers_attribute :: (IsINSetMessageAttributeIntent inSetMessageAttributeIntent, IsNSArray identifiers) => inSetMessageAttributeIntent -> identifiers -> INMessageAttribute -> IO (Id INSetMessageAttributeIntent)
initWithIdentifiers_attribute inSetMessageAttributeIntent  identifiers attribute =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg inSetMessageAttributeIntent (mkSelector "initWithIdentifiers:attribute:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ()), argCLong (coerce attribute)] >>= ownedObject . castPtr

-- | @- identifiers@
identifiers :: IsINSetMessageAttributeIntent inSetMessageAttributeIntent => inSetMessageAttributeIntent -> IO (Id NSArray)
identifiers inSetMessageAttributeIntent  =
  sendMsg inSetMessageAttributeIntent (mkSelector "identifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attribute@
attribute :: IsINSetMessageAttributeIntent inSetMessageAttributeIntent => inSetMessageAttributeIntent -> IO INMessageAttribute
attribute inSetMessageAttributeIntent  =
  fmap (coerce :: CLong -> INMessageAttribute) $ sendMsg inSetMessageAttributeIntent (mkSelector "attribute") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifiers:attribute:@
initWithIdentifiers_attributeSelector :: Selector
initWithIdentifiers_attributeSelector = mkSelector "initWithIdentifiers:attribute:"

-- | @Selector@ for @identifiers@
identifiersSelector :: Selector
identifiersSelector = mkSelector "identifiers"

-- | @Selector@ for @attribute@
attributeSelector :: Selector
attributeSelector = mkSelector "attribute"

