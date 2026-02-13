{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a temporary break in participation.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- - NOTE: See AVPlaybackCoordinator's beginSuspensionForReason: method for details on use.
--
-- Generated bindings for @AVCoordinatedPlaybackSuspension@.
module ObjC.AVFoundation.AVCoordinatedPlaybackSuspension
  ( AVCoordinatedPlaybackSuspension
  , IsAVCoordinatedPlaybackSuspension(..)
  , init_
  , new
  , end
  , reason
  , beginDate
  , beginDateSelector
  , endSelector
  , initSelector
  , newSelector
  , reasonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO (Id AVCoordinatedPlaybackSuspension)
init_ avCoordinatedPlaybackSuspension =
  sendOwnedMessage avCoordinatedPlaybackSuspension initSelector

-- | @+ new@
new :: IO (Id AVCoordinatedPlaybackSuspension)
new  =
  do
    cls' <- getRequiredClass "AVCoordinatedPlaybackSuspension"
    sendOwnedClassMessage cls' newSelector

-- | Ends the suspension.
--
-- If this is the last suspension, the coordinator will adjust timing of its playback object to match the group. Also see endProposingNewTime: for a way to end a suspension and simultaneously proposing a new time to the group.
--
-- ObjC selector: @- end@
end :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO ()
end avCoordinatedPlaybackSuspension =
  sendMessage avCoordinatedPlaybackSuspension endSelector

-- | The reason for the suspension. This will be communicated to other participants while coordination is suspended.
--
-- ObjC selector: @- reason@
reason :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO (Id NSString)
reason avCoordinatedPlaybackSuspension =
  sendMessage avCoordinatedPlaybackSuspension reasonSelector

-- | The begin time of the suspension.
--
-- ObjC selector: @- beginDate@
beginDate :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO (Id NSDate)
beginDate avCoordinatedPlaybackSuspension =
  sendMessage avCoordinatedPlaybackSuspension beginDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCoordinatedPlaybackSuspension)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCoordinatedPlaybackSuspension)
newSelector = mkSelector "new"

-- | @Selector@ for @end@
endSelector :: Selector '[] ()
endSelector = mkSelector "end"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] (Id NSString)
reasonSelector = mkSelector "reason"

-- | @Selector@ for @beginDate@
beginDateSelector :: Selector '[] (Id NSDate)
beginDateSelector = mkSelector "beginDate"

