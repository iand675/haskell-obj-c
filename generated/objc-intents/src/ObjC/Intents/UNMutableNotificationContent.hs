{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNMutableNotificationContent@.
module ObjC.Intents.UNMutableNotificationContent
  ( UNMutableNotificationContent
  , IsUNMutableNotificationContent(..)
  , badge
  , setBadge
  , launchImageName
  , setLaunchImageName
  , summaryArgument
  , setSummaryArgument
  , summaryArgumentCount
  , setSummaryArgumentCount
  , interruptionLevel
  , setInterruptionLevel
  , relevanceScore
  , setRelevanceScore
  , filterCriteria
  , setFilterCriteria
  , badgeSelector
  , setBadgeSelector
  , launchImageNameSelector
  , setLaunchImageNameSelector
  , summaryArgumentSelector
  , setSummaryArgumentSelector
  , summaryArgumentCountSelector
  , setSummaryArgumentCountSelector
  , interruptionLevelSelector
  , setInterruptionLevelSelector
  , relevanceScoreSelector
  , setRelevanceScoreSelector
  , filterCriteriaSelector
  , setFilterCriteriaSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- badge@
badge :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSNumber)
badge unMutableNotificationContent  =
  sendMsg unMutableNotificationContent (mkSelector "badge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBadge:@
setBadge :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSNumber value) => unMutableNotificationContent -> value -> IO ()
setBadge unMutableNotificationContent  value =
withObjCPtr value $ \raw_value ->
    sendMsg unMutableNotificationContent (mkSelector "setBadge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- launchImageName@
launchImageName :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
launchImageName unMutableNotificationContent  =
  sendMsg unMutableNotificationContent (mkSelector "launchImageName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLaunchImageName:@
setLaunchImageName :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setLaunchImageName unMutableNotificationContent  value =
withObjCPtr value $ \raw_value ->
    sendMsg unMutableNotificationContent (mkSelector "setLaunchImageName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- summaryArgument@
summaryArgument :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
summaryArgument unMutableNotificationContent  =
  sendMsg unMutableNotificationContent (mkSelector "summaryArgument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- setSummaryArgument:@
setSummaryArgument :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setSummaryArgument unMutableNotificationContent  value =
withObjCPtr value $ \raw_value ->
    sendMsg unMutableNotificationContent (mkSelector "setSummaryArgument:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- summaryArgumentCount@
summaryArgumentCount :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO CULong
summaryArgumentCount unMutableNotificationContent  =
  sendMsg unMutableNotificationContent (mkSelector "summaryArgumentCount") retCULong []

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- setSummaryArgumentCount:@
setSummaryArgumentCount :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> CULong -> IO ()
setSummaryArgumentCount unMutableNotificationContent  value =
  sendMsg unMutableNotificationContent (mkSelector "setSummaryArgumentCount:") retVoid [argCULong (fromIntegral value)]

-- | @- interruptionLevel@
interruptionLevel :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO UNNotificationInterruptionLevel
interruptionLevel unMutableNotificationContent  =
  fmap (coerce :: CULong -> UNNotificationInterruptionLevel) $ sendMsg unMutableNotificationContent (mkSelector "interruptionLevel") retCULong []

-- | @- setInterruptionLevel:@
setInterruptionLevel :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> UNNotificationInterruptionLevel -> IO ()
setInterruptionLevel unMutableNotificationContent  value =
  sendMsg unMutableNotificationContent (mkSelector "setInterruptionLevel:") retVoid [argCULong (coerce value)]

-- | @- relevanceScore@
relevanceScore :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO CDouble
relevanceScore unMutableNotificationContent  =
  sendMsg unMutableNotificationContent (mkSelector "relevanceScore") retCDouble []

-- | @- setRelevanceScore:@
setRelevanceScore :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> CDouble -> IO ()
setRelevanceScore unMutableNotificationContent  value =
  sendMsg unMutableNotificationContent (mkSelector "setRelevanceScore:") retVoid [argCDouble (fromIntegral value)]

-- | @- filterCriteria@
filterCriteria :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
filterCriteria unMutableNotificationContent  =
  sendMsg unMutableNotificationContent (mkSelector "filterCriteria") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilterCriteria:@
setFilterCriteria :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setFilterCriteria unMutableNotificationContent  value =
withObjCPtr value $ \raw_value ->
    sendMsg unMutableNotificationContent (mkSelector "setFilterCriteria:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @badge@
badgeSelector :: Selector
badgeSelector = mkSelector "badge"

-- | @Selector@ for @setBadge:@
setBadgeSelector :: Selector
setBadgeSelector = mkSelector "setBadge:"

-- | @Selector@ for @launchImageName@
launchImageNameSelector :: Selector
launchImageNameSelector = mkSelector "launchImageName"

-- | @Selector@ for @setLaunchImageName:@
setLaunchImageNameSelector :: Selector
setLaunchImageNameSelector = mkSelector "setLaunchImageName:"

-- | @Selector@ for @summaryArgument@
summaryArgumentSelector :: Selector
summaryArgumentSelector = mkSelector "summaryArgument"

-- | @Selector@ for @setSummaryArgument:@
setSummaryArgumentSelector :: Selector
setSummaryArgumentSelector = mkSelector "setSummaryArgument:"

-- | @Selector@ for @summaryArgumentCount@
summaryArgumentCountSelector :: Selector
summaryArgumentCountSelector = mkSelector "summaryArgumentCount"

-- | @Selector@ for @setSummaryArgumentCount:@
setSummaryArgumentCountSelector :: Selector
setSummaryArgumentCountSelector = mkSelector "setSummaryArgumentCount:"

-- | @Selector@ for @interruptionLevel@
interruptionLevelSelector :: Selector
interruptionLevelSelector = mkSelector "interruptionLevel"

-- | @Selector@ for @setInterruptionLevel:@
setInterruptionLevelSelector :: Selector
setInterruptionLevelSelector = mkSelector "setInterruptionLevel:"

-- | @Selector@ for @relevanceScore@
relevanceScoreSelector :: Selector
relevanceScoreSelector = mkSelector "relevanceScore"

-- | @Selector@ for @setRelevanceScore:@
setRelevanceScoreSelector :: Selector
setRelevanceScoreSelector = mkSelector "setRelevanceScore:"

-- | @Selector@ for @filterCriteria@
filterCriteriaSelector :: Selector
filterCriteriaSelector = mkSelector "filterCriteria"

-- | @Selector@ for @setFilterCriteria:@
setFilterCriteriaSelector :: Selector
setFilterCriteriaSelector = mkSelector "setFilterCriteria:"

