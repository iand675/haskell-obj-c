{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that describes a sticker someone sends in a message.
--
-- Generated bindings for @INSticker@.
module ObjC.Intents.INSticker
  ( INSticker
  , IsINSticker(..)
  , initWithType_emoji
  , type_
  , emoji
  , emojiSelector
  , initWithType_emojiSelector
  , typeSelector

  -- * Enum types
  , INStickerType(INStickerType)
  , pattern INStickerTypeUnknown
  , pattern INStickerTypeEmoji
  , pattern INStickerTypeGeneric

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

-- | Creates an object that represents a sticker a person sends in a message.
--
-- - Parameters:   - type: The type of the sticker.   - emoji: The single emoji character that the sticker represents.
--
-- ObjC selector: @- initWithType:emoji:@
initWithType_emoji :: (IsINSticker inSticker, IsNSString emoji) => inSticker -> INStickerType -> emoji -> IO (Id INSticker)
initWithType_emoji inSticker type_ emoji =
  sendOwnedMessage inSticker initWithType_emojiSelector type_ (toNSString emoji)

-- | @- type@
type_ :: IsINSticker inSticker => inSticker -> IO INStickerType
type_ inSticker =
  sendMessage inSticker typeSelector

-- | @- emoji@
emoji :: IsINSticker inSticker => inSticker -> IO (Id NSString)
emoji inSticker =
  sendMessage inSticker emojiSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:emoji:@
initWithType_emojiSelector :: Selector '[INStickerType, Id NSString] (Id INSticker)
initWithType_emojiSelector = mkSelector "initWithType:emoji:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] INStickerType
typeSelector = mkSelector "type"

-- | @Selector@ for @emoji@
emojiSelector :: Selector '[] (Id NSString)
emojiSelector = mkSelector "emoji"

