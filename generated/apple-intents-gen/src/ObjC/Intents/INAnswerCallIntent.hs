{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAnswerCallIntent@.
module ObjC.Intents.INAnswerCallIntent
  ( INAnswerCallIntent
  , IsINAnswerCallIntent(..)
  , initWithAudioRoute_callIdentifier
  , audioRoute
  , callIdentifier
  , audioRouteSelector
  , callIdentifierSelector
  , initWithAudioRoute_callIdentifierSelector

  -- * Enum types
  , INCallAudioRoute(INCallAudioRoute)
  , pattern INCallAudioRouteUnknown
  , pattern INCallAudioRouteSpeakerphoneAudioRoute
  , pattern INCallAudioRouteBluetoothAudioRoute

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

-- | @- initWithAudioRoute:callIdentifier:@
initWithAudioRoute_callIdentifier :: (IsINAnswerCallIntent inAnswerCallIntent, IsNSString callIdentifier) => inAnswerCallIntent -> INCallAudioRoute -> callIdentifier -> IO (Id INAnswerCallIntent)
initWithAudioRoute_callIdentifier inAnswerCallIntent audioRoute callIdentifier =
  sendOwnedMessage inAnswerCallIntent initWithAudioRoute_callIdentifierSelector audioRoute (toNSString callIdentifier)

-- | @- audioRoute@
audioRoute :: IsINAnswerCallIntent inAnswerCallIntent => inAnswerCallIntent -> IO INCallAudioRoute
audioRoute inAnswerCallIntent =
  sendMessage inAnswerCallIntent audioRouteSelector

-- | @- callIdentifier@
callIdentifier :: IsINAnswerCallIntent inAnswerCallIntent => inAnswerCallIntent -> IO (Id NSString)
callIdentifier inAnswerCallIntent =
  sendMessage inAnswerCallIntent callIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioRoute:callIdentifier:@
initWithAudioRoute_callIdentifierSelector :: Selector '[INCallAudioRoute, Id NSString] (Id INAnswerCallIntent)
initWithAudioRoute_callIdentifierSelector = mkSelector "initWithAudioRoute:callIdentifier:"

-- | @Selector@ for @audioRoute@
audioRouteSelector :: Selector '[] INCallAudioRoute
audioRouteSelector = mkSelector "audioRoute"

-- | @Selector@ for @callIdentifier@
callIdentifierSelector :: Selector '[] (Id NSString)
callIdentifierSelector = mkSelector "callIdentifier"

