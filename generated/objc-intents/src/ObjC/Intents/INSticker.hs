{-# LANGUAGE PatternSynonyms #-}
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
  , initWithType_emojiSelector
  , typeSelector
  , emojiSelector

  -- * Enum types
  , INStickerType(INStickerType)
  , pattern INStickerTypeUnknown
  , pattern INStickerTypeEmoji
  , pattern INStickerTypeGeneric

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

-- | Creates an object that represents a sticker a person sends in a message.
--
-- - Parameters:   - type: The type of the sticker.   - emoji: The single emoji character that the sticker represents.
--
-- ObjC selector: @- initWithType:emoji:@
initWithType_emoji :: (IsINSticker inSticker, IsNSString emoji) => inSticker -> INStickerType -> emoji -> IO (Id INSticker)
initWithType_emoji inSticker  type_ emoji =
withObjCPtr emoji $ \raw_emoji ->
    sendMsg inSticker (mkSelector "initWithType:emoji:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_emoji :: Ptr ())] >>= ownedObject . castPtr

-- | @- type@
type_ :: IsINSticker inSticker => inSticker -> IO INStickerType
type_ inSticker  =
  fmap (coerce :: CLong -> INStickerType) $ sendMsg inSticker (mkSelector "type") retCLong []

-- | @- emoji@
emoji :: IsINSticker inSticker => inSticker -> IO (Id NSString)
emoji inSticker  =
  sendMsg inSticker (mkSelector "emoji") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:emoji:@
initWithType_emojiSelector :: Selector
initWithType_emojiSelector = mkSelector "initWithType:emoji:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @emoji@
emojiSelector :: Selector
emojiSelector = mkSelector "emoji"

