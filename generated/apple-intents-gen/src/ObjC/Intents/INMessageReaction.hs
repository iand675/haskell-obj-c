{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , emojiSelector
  , initWithReactionType_reactionDescription_emojiSelector
  , reactionDescriptionSelector
  , reactionTypeSelector

  -- * Enum types
  , INMessageReactionType(INMessageReactionType)
  , pattern INMessageReactionTypeUnknown
  , pattern INMessageReactionTypeEmoji
  , pattern INMessageReactionTypeGeneric

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

-- | @- initWithReactionType:reactionDescription:emoji:@
initWithReactionType_reactionDescription_emoji :: (IsINMessageReaction inMessageReaction, IsNSString reactionDescription, IsNSString emoji) => inMessageReaction -> INMessageReactionType -> reactionDescription -> emoji -> IO (Id INMessageReaction)
initWithReactionType_reactionDescription_emoji inMessageReaction reactionType reactionDescription emoji =
  sendOwnedMessage inMessageReaction initWithReactionType_reactionDescription_emojiSelector reactionType (toNSString reactionDescription) (toNSString emoji)

-- | @- reactionType@
reactionType :: IsINMessageReaction inMessageReaction => inMessageReaction -> IO INMessageReactionType
reactionType inMessageReaction =
  sendMessage inMessageReaction reactionTypeSelector

-- | @- reactionDescription@
reactionDescription :: IsINMessageReaction inMessageReaction => inMessageReaction -> IO (Id NSString)
reactionDescription inMessageReaction =
  sendMessage inMessageReaction reactionDescriptionSelector

-- | @- emoji@
emoji :: IsINMessageReaction inMessageReaction => inMessageReaction -> IO (Id NSString)
emoji inMessageReaction =
  sendMessage inMessageReaction emojiSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReactionType:reactionDescription:emoji:@
initWithReactionType_reactionDescription_emojiSelector :: Selector '[INMessageReactionType, Id NSString, Id NSString] (Id INMessageReaction)
initWithReactionType_reactionDescription_emojiSelector = mkSelector "initWithReactionType:reactionDescription:emoji:"

-- | @Selector@ for @reactionType@
reactionTypeSelector :: Selector '[] INMessageReactionType
reactionTypeSelector = mkSelector "reactionType"

-- | @Selector@ for @reactionDescription@
reactionDescriptionSelector :: Selector '[] (Id NSString)
reactionDescriptionSelector = mkSelector "reactionDescription"

-- | @Selector@ for @emoji@
emojiSelector :: Selector '[] (Id NSString)
emojiSelector = mkSelector "emoji"

