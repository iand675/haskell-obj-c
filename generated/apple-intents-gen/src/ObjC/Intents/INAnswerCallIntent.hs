{-# LANGUAGE PatternSynonyms #-}
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
  , initWithAudioRoute_callIdentifierSelector
  , audioRouteSelector
  , callIdentifierSelector

  -- * Enum types
  , INCallAudioRoute(INCallAudioRoute)
  , pattern INCallAudioRouteUnknown
  , pattern INCallAudioRouteSpeakerphoneAudioRoute
  , pattern INCallAudioRouteBluetoothAudioRoute

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

-- | @- initWithAudioRoute:callIdentifier:@
initWithAudioRoute_callIdentifier :: (IsINAnswerCallIntent inAnswerCallIntent, IsNSString callIdentifier) => inAnswerCallIntent -> INCallAudioRoute -> callIdentifier -> IO (Id INAnswerCallIntent)
initWithAudioRoute_callIdentifier inAnswerCallIntent  audioRoute callIdentifier =
  withObjCPtr callIdentifier $ \raw_callIdentifier ->
      sendMsg inAnswerCallIntent (mkSelector "initWithAudioRoute:callIdentifier:") (retPtr retVoid) [argCLong (coerce audioRoute), argPtr (castPtr raw_callIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- audioRoute@
audioRoute :: IsINAnswerCallIntent inAnswerCallIntent => inAnswerCallIntent -> IO INCallAudioRoute
audioRoute inAnswerCallIntent  =
    fmap (coerce :: CLong -> INCallAudioRoute) $ sendMsg inAnswerCallIntent (mkSelector "audioRoute") retCLong []

-- | @- callIdentifier@
callIdentifier :: IsINAnswerCallIntent inAnswerCallIntent => inAnswerCallIntent -> IO (Id NSString)
callIdentifier inAnswerCallIntent  =
    sendMsg inAnswerCallIntent (mkSelector "callIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioRoute:callIdentifier:@
initWithAudioRoute_callIdentifierSelector :: Selector
initWithAudioRoute_callIdentifierSelector = mkSelector "initWithAudioRoute:callIdentifier:"

-- | @Selector@ for @audioRoute@
audioRouteSelector :: Selector
audioRouteSelector = mkSelector "audioRoute"

-- | @Selector@ for @callIdentifier@
callIdentifierSelector :: Selector
callIdentifierSelector = mkSelector "callIdentifier"

