{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKAchievementDescription is a full description of the achievement as defined before app submission in App Store Connect.
--
-- Generated bindings for @GKAchievementDescription@.
module ObjC.GameKit.GKAchievementDescription
  ( GKAchievementDescription
  , IsGKAchievementDescription(..)
  , loadImageWithCompletionHandler
  , incompleteAchievementImage
  , placeholderCompletedAchievementImage
  , identifier
  , groupIdentifier
  , title
  , achievedDescription
  , unachievedDescription
  , maximumPoints
  , hidden
  , replayable
  , releaseState
  , activityIdentifier
  , activityProperties
  , loadImageWithCompletionHandlerSelector
  , incompleteAchievementImageSelector
  , placeholderCompletedAchievementImageSelector
  , identifierSelector
  , groupIdentifierSelector
  , titleSelector
  , achievedDescriptionSelector
  , unachievedDescriptionSelector
  , maximumPointsSelector
  , hiddenSelector
  , replayableSelector
  , releaseStateSelector
  , activityIdentifierSelector
  , activityPropertiesSelector

  -- * Enum types
  , GKReleaseState(GKReleaseState)
  , pattern GKReleaseStateUnknown
  , pattern GKReleaseStateReleased
  , pattern GKReleaseStatePrereleased

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

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> Ptr () -> IO ()
loadImageWithCompletionHandler gkAchievementDescription  completionHandler =
  sendMsg gkAchievementDescription (mkSelector "loadImageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ incompleteAchievementImage@
incompleteAchievementImage :: IO (Id NSImage)
incompleteAchievementImage  =
  do
    cls' <- getRequiredClass "GKAchievementDescription"
    sendClassMsg cls' (mkSelector "incompleteAchievementImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ placeholderCompletedAchievementImage@
placeholderCompletedAchievementImage :: IO (Id NSImage)
placeholderCompletedAchievementImage  =
  do
    cls' <- getRequiredClass "GKAchievementDescription"
    sendClassMsg cls' (mkSelector "placeholderCompletedAchievementImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSString)
identifier gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The group identifier for the achievement, if one exists.
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSString)
groupIdentifier gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title of the achievement.
--
-- ObjC selector: @- title@
title :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSString)
title gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The description for an unachieved achievement.
--
-- ObjC selector: @- achievedDescription@
achievedDescription :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSString)
achievedDescription gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "achievedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The description for an achieved achievement.
--
-- ObjC selector: @- unachievedDescription@
unachievedDescription :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSString)
unachievedDescription gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "unachievedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Maximum points available for completing this achievement.
--
-- ObjC selector: @- maximumPoints@
maximumPoints :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO CLong
maximumPoints gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "maximumPoints") retCLong []

-- | Whether or not the achievement should be listed or displayed if not yet unhidden by the game.
--
-- ObjC selector: @- hidden@
hidden :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO Bool
hidden gkAchievementDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAchievementDescription (mkSelector "hidden") retCULong []

-- | Whether or not the achievement will be reported by the game when the user earns it again. This allows the achievement to be used for challenges when the recipient has previously earned it.
--
-- ObjC selector: @- replayable@
replayable :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO Bool
replayable gkAchievementDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAchievementDescription (mkSelector "replayable") retCULong []

-- | The release state of the achievement in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO GKReleaseState
releaseState gkAchievementDescription  =
  fmap (coerce :: CULong -> GKReleaseState) $ sendMsg gkAchievementDescription (mkSelector "releaseState") retCULong []

-- | The identifier of the game activity associated with this achievement, as configured by the developer in App Store Connect.
--
-- ObjC selector: @- activityIdentifier@
activityIdentifier :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSString)
activityIdentifier gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "activityIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The properties when associating this achievement with a game activity, as configured by the developer in App Store Connect.
--
-- ObjC selector: @- activityProperties@
activityProperties :: IsGKAchievementDescription gkAchievementDescription => gkAchievementDescription -> IO (Id NSDictionary)
activityProperties gkAchievementDescription  =
  sendMsg gkAchievementDescription (mkSelector "activityProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @incompleteAchievementImage@
incompleteAchievementImageSelector :: Selector
incompleteAchievementImageSelector = mkSelector "incompleteAchievementImage"

-- | @Selector@ for @placeholderCompletedAchievementImage@
placeholderCompletedAchievementImageSelector :: Selector
placeholderCompletedAchievementImageSelector = mkSelector "placeholderCompletedAchievementImage"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @achievedDescription@
achievedDescriptionSelector :: Selector
achievedDescriptionSelector = mkSelector "achievedDescription"

-- | @Selector@ for @unachievedDescription@
unachievedDescriptionSelector :: Selector
unachievedDescriptionSelector = mkSelector "unachievedDescription"

-- | @Selector@ for @maximumPoints@
maximumPointsSelector :: Selector
maximumPointsSelector = mkSelector "maximumPoints"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @replayable@
replayableSelector :: Selector
replayableSelector = mkSelector "replayable"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector
releaseStateSelector = mkSelector "releaseState"

-- | @Selector@ for @activityIdentifier@
activityIdentifierSelector :: Selector
activityIdentifierSelector = mkSelector "activityIdentifier"

-- | @Selector@ for @activityProperties@
activityPropertiesSelector :: Selector
activityPropertiesSelector = mkSelector "activityProperties"

