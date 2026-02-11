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
  , initSelector
  , newSelector
  , endSelector
  , reasonSelector
  , beginDateSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO (Id AVCoordinatedPlaybackSuspension)
init_ avCoordinatedPlaybackSuspension  =
  sendMsg avCoordinatedPlaybackSuspension (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCoordinatedPlaybackSuspension)
new  =
  do
    cls' <- getRequiredClass "AVCoordinatedPlaybackSuspension"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Ends the suspension.
--
-- If this is the last suspension, the coordinator will adjust timing of its playback object to match the group. Also see endProposingNewTime: for a way to end a suspension and simultaneously proposing a new time to the group.
--
-- ObjC selector: @- end@
end :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO ()
end avCoordinatedPlaybackSuspension  =
  sendMsg avCoordinatedPlaybackSuspension (mkSelector "end") retVoid []

-- | The reason for the suspension. This will be communicated to other participants while coordination is suspended.
--
-- ObjC selector: @- reason@
reason :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO (Id NSString)
reason avCoordinatedPlaybackSuspension  =
  sendMsg avCoordinatedPlaybackSuspension (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The begin time of the suspension.
--
-- ObjC selector: @- beginDate@
beginDate :: IsAVCoordinatedPlaybackSuspension avCoordinatedPlaybackSuspension => avCoordinatedPlaybackSuspension -> IO (Id NSDate)
beginDate avCoordinatedPlaybackSuspension  =
  sendMsg avCoordinatedPlaybackSuspension (mkSelector "beginDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @end@
endSelector :: Selector
endSelector = mkSelector "end"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @beginDate@
beginDateSelector :: Selector
beginDateSelector = mkSelector "beginDate"

