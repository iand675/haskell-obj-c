{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object representing capabilities that can be granted to a helper extension process.
--
-- Generated bindings for @BEProcessCapability@.
module ObjC.BrowserEngineKit.BEProcessCapability
  ( BEProcessCapability
  , IsBEProcessCapability(..)
  , mediaPlaybackAndCaptureWithEnvironment
  , background
  , foreground
  , suspended
  , requestWithError
  , backgroundSelector
  , foregroundSelector
  , mediaPlaybackAndCaptureWithEnvironmentSelector
  , requestWithErrorSelector
  , suspendedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The helper extension process may access AV hardware required for media capture and playback.
--
-- ObjC selector: @+ mediaPlaybackAndCaptureWithEnvironment:@
mediaPlaybackAndCaptureWithEnvironment :: IsBEMediaEnvironment environment => environment -> IO (Id BEProcessCapability)
mediaPlaybackAndCaptureWithEnvironment environment =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMessage cls' mediaPlaybackAndCaptureWithEnvironmentSelector (toBEMediaEnvironment environment)

-- | The helper extension process may run in the background to finish work.
--
-- ObjC selector: @+ background@
background :: IO (Id BEProcessCapability)
background  =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMessage cls' backgroundSelector

-- | The helper extension process may run at foreground priority to work on behalf of the host process while it is foreground.
--
-- ObjC selector: @+ foreground@
foreground :: IO (Id BEProcessCapability)
foreground  =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMessage cls' foregroundSelector

-- | The helper extension process may remain resident in a suspended state (it will not be granted CPU time).
--
-- ObjC selector: @+ suspended@
suspended :: IO (Id BEProcessCapability)
suspended  =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMessage cls' suspendedSelector

-- | Requests the capability to be granted to the current process.
--
-- Returns the granted capability or nil and an error if it can not be granted
--
-- ObjC selector: @- requestWithError:@
requestWithError :: (IsBEProcessCapability beProcessCapability, IsNSError error_) => beProcessCapability -> error_ -> IO RawId
requestWithError beProcessCapability error_ =
  sendMessage beProcessCapability requestWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaPlaybackAndCaptureWithEnvironment:@
mediaPlaybackAndCaptureWithEnvironmentSelector :: Selector '[Id BEMediaEnvironment] (Id BEProcessCapability)
mediaPlaybackAndCaptureWithEnvironmentSelector = mkSelector "mediaPlaybackAndCaptureWithEnvironment:"

-- | @Selector@ for @background@
backgroundSelector :: Selector '[] (Id BEProcessCapability)
backgroundSelector = mkSelector "background"

-- | @Selector@ for @foreground@
foregroundSelector :: Selector '[] (Id BEProcessCapability)
foregroundSelector = mkSelector "foreground"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] (Id BEProcessCapability)
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @requestWithError:@
requestWithErrorSelector :: Selector '[Id NSError] RawId
requestWithErrorSelector = mkSelector "requestWithError:"

