{-# LANGUAGE DataKinds #-}
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
  , localizedShortTitleSelector
  , localizedTitleSelector
  , setActiveSelector
  , setLocalizedShortTitleSelector
  , setLocalizedTitleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the feedback command is in an "active" state. An example of when a feedback command would be active is if the user already "liked" a particular content item.
--
-- ObjC selector: @- active@
active :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> IO Bool
active mpFeedbackCommand =
  sendMessage mpFeedbackCommand activeSelector

-- | Whether the feedback command is in an "active" state. An example of when a feedback command would be active is if the user already "liked" a particular content item.
--
-- ObjC selector: @- setActive:@
setActive :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> Bool -> IO ()
setActive mpFeedbackCommand value =
  sendMessage mpFeedbackCommand setActiveSelector value

-- | A localized string briefly describing the context of the command.
--
-- ObjC selector: @- localizedTitle@
localizedTitle :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> IO (Id NSString)
localizedTitle mpFeedbackCommand =
  sendMessage mpFeedbackCommand localizedTitleSelector

-- | A localized string briefly describing the context of the command.
--
-- ObjC selector: @- setLocalizedTitle:@
setLocalizedTitle :: (IsMPFeedbackCommand mpFeedbackCommand, IsNSString value) => mpFeedbackCommand -> value -> IO ()
setLocalizedTitle mpFeedbackCommand value =
  sendMessage mpFeedbackCommand setLocalizedTitleSelector (toNSString value)

-- | An optional shorter version of the localized title for this feedback command. MediaPlayer uses this property to display this command's title on remote control interfaces with little screen space.
--
-- ObjC selector: @- localizedShortTitle@
localizedShortTitle :: IsMPFeedbackCommand mpFeedbackCommand => mpFeedbackCommand -> IO (Id NSString)
localizedShortTitle mpFeedbackCommand =
  sendMessage mpFeedbackCommand localizedShortTitleSelector

-- | An optional shorter version of the localized title for this feedback command. MediaPlayer uses this property to display this command's title on remote control interfaces with little screen space.
--
-- ObjC selector: @- setLocalizedShortTitle:@
setLocalizedShortTitle :: (IsMPFeedbackCommand mpFeedbackCommand, IsNSString value) => mpFeedbackCommand -> value -> IO ()
setLocalizedShortTitle mpFeedbackCommand value =
  sendMessage mpFeedbackCommand setLocalizedShortTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Bool] ()
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector '[] (Id NSString)
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @setLocalizedTitle:@
setLocalizedTitleSelector :: Selector '[Id NSString] ()
setLocalizedTitleSelector = mkSelector "setLocalizedTitle:"

-- | @Selector@ for @localizedShortTitle@
localizedShortTitleSelector :: Selector '[] (Id NSString)
localizedShortTitleSelector = mkSelector "localizedShortTitle"

-- | @Selector@ for @setLocalizedShortTitle:@
setLocalizedShortTitleSelector :: Selector '[Id NSString] ()
setLocalizedShortTitleSelector = mkSelector "setLocalizedShortTitle:"

