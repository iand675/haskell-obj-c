{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeskViewApplicationLaunchConfiguration
--
-- An object for configuring how the Desk View application is presented.
--
-- Developers may use this interface to customize the presentation of the Desk View application upon launch.
--
-- Generated bindings for @AVCaptureDeskViewApplicationLaunchConfiguration@.
module ObjC.AVFoundation.AVCaptureDeskViewApplicationLaunchConfiguration
  ( AVCaptureDeskViewApplicationLaunchConfiguration
  , IsAVCaptureDeskViewApplicationLaunchConfiguration(..)
  , requiresSetUpModeCompletion
  , setRequiresSetUpModeCompletion
  , requiresSetUpModeCompletionSelector
  , setRequiresSetUpModeCompletionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | requiresSetUpModeCompletion
--
-- Specifies whether presentWithLaunchConfiguration:completionHandler:'s completionHandler fires immediately after the application is launched, or only after the user has completed set up.
--
-- The Desk View application launches in set up mode, showing the full field of view of an Ultra Wide camera with a superimposed trapezoid indicating the desk region crop that will be shown once set up is complete. By default, your presentWithLaunchConfiguration:completionHandler:'s completionHandler fires after the Desk View application is launched and visible to the user. By setting this property to YES, your completionHandler only fires when the user has completed set up mode and transitioned to the cropped Desk View mode.
--
-- ObjC selector: @- requiresSetUpModeCompletion@
requiresSetUpModeCompletion :: IsAVCaptureDeskViewApplicationLaunchConfiguration avCaptureDeskViewApplicationLaunchConfiguration => avCaptureDeskViewApplicationLaunchConfiguration -> IO Bool
requiresSetUpModeCompletion avCaptureDeskViewApplicationLaunchConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDeskViewApplicationLaunchConfiguration (mkSelector "requiresSetUpModeCompletion") retCULong []

-- | requiresSetUpModeCompletion
--
-- Specifies whether presentWithLaunchConfiguration:completionHandler:'s completionHandler fires immediately after the application is launched, or only after the user has completed set up.
--
-- The Desk View application launches in set up mode, showing the full field of view of an Ultra Wide camera with a superimposed trapezoid indicating the desk region crop that will be shown once set up is complete. By default, your presentWithLaunchConfiguration:completionHandler:'s completionHandler fires after the Desk View application is launched and visible to the user. By setting this property to YES, your completionHandler only fires when the user has completed set up mode and transitioned to the cropped Desk View mode.
--
-- ObjC selector: @- setRequiresSetUpModeCompletion:@
setRequiresSetUpModeCompletion :: IsAVCaptureDeskViewApplicationLaunchConfiguration avCaptureDeskViewApplicationLaunchConfiguration => avCaptureDeskViewApplicationLaunchConfiguration -> Bool -> IO ()
setRequiresSetUpModeCompletion avCaptureDeskViewApplicationLaunchConfiguration  value =
  sendMsg avCaptureDeskViewApplicationLaunchConfiguration (mkSelector "setRequiresSetUpModeCompletion:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requiresSetUpModeCompletion@
requiresSetUpModeCompletionSelector :: Selector
requiresSetUpModeCompletionSelector = mkSelector "requiresSetUpModeCompletion"

-- | @Selector@ for @setRequiresSetUpModeCompletion:@
setRequiresSetUpModeCompletionSelector :: Selector
setRequiresSetUpModeCompletionSelector = mkSelector "setRequiresSetUpModeCompletion:"

