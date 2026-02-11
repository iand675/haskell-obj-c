{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPFeedbackCommand@.
module ObjC.MediaPlayer.MPFeedbackCommand
  ( MPFeedbackCommand
  , IsMPFeedbackCommand(..)
  , active
  , setActive
  , localizedTitle
  , setLocalizedTitle
  , localizedShortTitle
  , setLocalizedShortTitle
  , activeSelector
  , setActiveSelector
  , localizedTitleSelector
  , setLocalizedTitleSelector
  , localizedShortTitleSelector
  , setLocalizedShortTitleSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the feedback command is in an "active" state. An example of when a feedback command would be active is if the user already "liked" a particular content item.
--
-- ObjC selector: @- active@
active :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> IO Bool
active mpFeedbackCommand  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpFeedbackCommand (mkSelector "active") retCULong []

-- | Whether the feedback command is in an "active" state. An example of when a feedback command would be active is if the user already "liked" a particular content item.
--
-- ObjC selector: @- setActive:@
setActive :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> Bool -> IO ()
setActive mpFeedbackCommand  value =
  sendMsg mpFeedbackCommand (mkSelector "setActive:") retVoid [argCULong (if value then 1 else 0)]

-- | A localized string briefly describing the context of the command.
--
-- ObjC selector: @- localizedTitle@
localizedTitle :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> IO (Id NSString)
localizedTitle mpFeedbackCommand  =
  sendMsg mpFeedbackCommand (mkSelector "localizedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A localized string briefly describing the context of the command.
--
-- ObjC selector: @- setLocalizedTitle:@
setLocalizedTitle :: (IsMPFeedbackCommand mpFeedbackCommand, IsNSString value) => mpFeedbackCommand -> value -> IO ()
setLocalizedTitle mpFeedbackCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpFeedbackCommand (mkSelector "setLocalizedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An optional shorter version of the localized title for this feedback command. MediaPlayer uses this property to display this command's title on remote control interfaces with little screen space.
--
-- ObjC selector: @- localizedShortTitle@
localizedShortTitle :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> IO (Id NSString)
localizedShortTitle mpFeedbackCommand  =
  sendMsg mpFeedbackCommand (mkSelector "localizedShortTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional shorter version of the localized title for this feedback command. MediaPlayer uses this property to display this command's title on remote control interfaces with little screen space.
--
-- ObjC selector: @- setLocalizedShortTitle:@
setLocalizedShortTitle :: (IsMPFeedbackCommand mpFeedbackCommand, IsNSString value) => mpFeedbackCommand -> value -> IO ()
setLocalizedShortTitle mpFeedbackCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpFeedbackCommand (mkSelector "setLocalizedShortTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @setLocalizedTitle:@
setLocalizedTitleSelector :: Selector
setLocalizedTitleSelector = mkSelector "setLocalizedTitle:"

-- | @Selector@ for @localizedShortTitle@
localizedShortTitleSelector :: Selector
localizedShortTitleSelector = mkSelector "localizedShortTitle"

-- | @Selector@ for @setLocalizedShortTitle:@
setLocalizedShortTitleSelector :: Selector
setLocalizedShortTitleSelector = mkSelector "setLocalizedShortTitle:"

