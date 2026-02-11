{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMessageReaction@.
module ObjC.Intents.INMessageReaction
  ( INMessageReaction
  , IsINMessageReaction(..)
  , initWithReactionType_reactionDescription_emoji
  , reactionType
  , reactionDescription
  , emoji
  , initWithReactionType_reactionDescription_emojiSelector
  , reactionTypeSelector
  , reactionDescriptionSelector
  , emojiSelector

  -- * Enum types
  , INMessageReactionType(INMessageReactionType)
  , pattern INMessageReactionTypeUnknown
  , pattern INMessageReactionTypeEmoji
  , pattern INMessageReactionTypeGeneric

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

-- | @- initWithReactionType:reactionDescription:emoji:@
initWithReactionType_reactionDescription_emoji :: (IsINMessageReaction inMessageReaction, IsNSString reactionDescription, IsNSString emoji) => inMessageReaction -> INMessageReactionType -> reactionDescription -> emoji -> IO (Id INMessageReaction)
initWithReactionType_reactionDescription_emoji inMessageReaction  reactionType reactionDescription emoji =
withObjCPtr reactionDescription $ \raw_reactionDescription ->
  withObjCPtr emoji $ \raw_emoji ->
      sendMsg inMessageReaction (mkSelector "initWithReactionType:reactionDescription:emoji:") (retPtr retVoid) [argCLong (coerce reactionType), argPtr (castPtr raw_reactionDescription :: Ptr ()), argPtr (castPtr raw_emoji :: Ptr ())] >>= ownedObject . castPtr

-- | @- reactionType@
reactionType :: IsINMessageReaction inMessageReaction => inMessageReaction -> IO INMessageReactionType
reactionType inMessageReaction  =
  fmap (coerce :: CLong -> INMessageReactionType) $ sendMsg inMessageReaction (mkSelector "reactionType") retCLong []

-- | @- reactionDescription@
reactionDescription :: IsINMessageReaction inMessageReaction => inMessageReaction -> IO (Id NSString)
reactionDescription inMessageReaction  =
  sendMsg inMessageReaction (mkSelector "reactionDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- emoji@
emoji :: IsINMessageReaction inMessageReaction => inMessageReaction -> IO (Id NSString)
emoji inMessageReaction  =
  sendMsg inMessageReaction (mkSelector "emoji") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReactionType:reactionDescription:emoji:@
initWithReactionType_reactionDescription_emojiSelector :: Selector
initWithReactionType_reactionDescription_emojiSelector = mkSelector "initWithReactionType:reactionDescription:emoji:"

-- | @Selector@ for @reactionType@
reactionTypeSelector :: Selector
reactionTypeSelector = mkSelector "reactionType"

-- | @Selector@ for @reactionDescription@
reactionDescriptionSelector :: Selector
reactionDescriptionSelector = mkSelector "reactionDescription"

-- | @Selector@ for @emoji@
emojiSelector :: Selector
emojiSelector = mkSelector "emoji"

