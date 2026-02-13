{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPageController@.
module ObjC.AppKit.NSPageController
  ( NSPageController
  , IsNSPageController(..)
  , navigateForwardToObject
  , completeTransition
  , navigateBack
  , navigateForward
  , takeSelectedIndexFrom
  , delegate
  , setDelegate
  , selectedViewController
  , transitionStyle
  , setTransitionStyle
  , arrangedObjects
  , setArrangedObjects
  , selectedIndex
  , setSelectedIndex
  , arrangedObjectsSelector
  , completeTransitionSelector
  , delegateSelector
  , navigateBackSelector
  , navigateForwardSelector
  , navigateForwardToObjectSelector
  , selectedIndexSelector
  , selectedViewControllerSelector
  , setArrangedObjectsSelector
  , setDelegateSelector
  , setSelectedIndexSelector
  , setTransitionStyleSelector
  , takeSelectedIndexFromSelector
  , transitionStyleSelector

  -- * Enum types
  , NSPageControllerTransitionStyle(NSPageControllerTransitionStyle)
  , pattern NSPageControllerTransitionStyleStackHistory
  , pattern NSPageControllerTransitionStyleStackBook
  , pattern NSPageControllerTransitionStyleHorizontalStrip

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- navigateForwardToObject:@
navigateForwardToObject :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
navigateForwardToObject nsPageController object =
  sendMessage nsPageController navigateForwardToObjectSelector object

-- | @- completeTransition@
completeTransition :: IsNSPageController nsPageController => nsPageController -> IO ()
completeTransition nsPageController =
  sendMessage nsPageController completeTransitionSelector

-- | @- navigateBack:@
navigateBack :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
navigateBack nsPageController sender =
  sendMessage nsPageController navigateBackSelector sender

-- | @- navigateForward:@
navigateForward :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
navigateForward nsPageController sender =
  sendMessage nsPageController navigateForwardSelector sender

-- | @- takeSelectedIndexFrom:@
takeSelectedIndexFrom :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
takeSelectedIndexFrom nsPageController sender =
  sendMessage nsPageController takeSelectedIndexFromSelector sender

-- | @- delegate@
delegate :: IsNSPageController nsPageController => nsPageController -> IO RawId
delegate nsPageController =
  sendMessage nsPageController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
setDelegate nsPageController value =
  sendMessage nsPageController setDelegateSelector value

-- | @- selectedViewController@
selectedViewController :: IsNSPageController nsPageController => nsPageController -> IO (Id NSViewController)
selectedViewController nsPageController =
  sendMessage nsPageController selectedViewControllerSelector

-- | @- transitionStyle@
transitionStyle :: IsNSPageController nsPageController => nsPageController -> IO NSPageControllerTransitionStyle
transitionStyle nsPageController =
  sendMessage nsPageController transitionStyleSelector

-- | @- setTransitionStyle:@
setTransitionStyle :: IsNSPageController nsPageController => nsPageController -> NSPageControllerTransitionStyle -> IO ()
setTransitionStyle nsPageController value =
  sendMessage nsPageController setTransitionStyleSelector value

-- | @- arrangedObjects@
arrangedObjects :: IsNSPageController nsPageController => nsPageController -> IO (Id NSArray)
arrangedObjects nsPageController =
  sendMessage nsPageController arrangedObjectsSelector

-- | @- setArrangedObjects:@
setArrangedObjects :: (IsNSPageController nsPageController, IsNSArray value) => nsPageController -> value -> IO ()
setArrangedObjects nsPageController value =
  sendMessage nsPageController setArrangedObjectsSelector (toNSArray value)

-- | @- selectedIndex@
selectedIndex :: IsNSPageController nsPageController => nsPageController -> IO CLong
selectedIndex nsPageController =
  sendMessage nsPageController selectedIndexSelector

-- | @- setSelectedIndex:@
setSelectedIndex :: IsNSPageController nsPageController => nsPageController -> CLong -> IO ()
setSelectedIndex nsPageController value =
  sendMessage nsPageController setSelectedIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @navigateForwardToObject:@
navigateForwardToObjectSelector :: Selector '[RawId] ()
navigateForwardToObjectSelector = mkSelector "navigateForwardToObject:"

-- | @Selector@ for @completeTransition@
completeTransitionSelector :: Selector '[] ()
completeTransitionSelector = mkSelector "completeTransition"

-- | @Selector@ for @navigateBack:@
navigateBackSelector :: Selector '[RawId] ()
navigateBackSelector = mkSelector "navigateBack:"

-- | @Selector@ for @navigateForward:@
navigateForwardSelector :: Selector '[RawId] ()
navigateForwardSelector = mkSelector "navigateForward:"

-- | @Selector@ for @takeSelectedIndexFrom:@
takeSelectedIndexFromSelector :: Selector '[RawId] ()
takeSelectedIndexFromSelector = mkSelector "takeSelectedIndexFrom:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @selectedViewController@
selectedViewControllerSelector :: Selector '[] (Id NSViewController)
selectedViewControllerSelector = mkSelector "selectedViewController"

-- | @Selector@ for @transitionStyle@
transitionStyleSelector :: Selector '[] NSPageControllerTransitionStyle
transitionStyleSelector = mkSelector "transitionStyle"

-- | @Selector@ for @setTransitionStyle:@
setTransitionStyleSelector :: Selector '[NSPageControllerTransitionStyle] ()
setTransitionStyleSelector = mkSelector "setTransitionStyle:"

-- | @Selector@ for @arrangedObjects@
arrangedObjectsSelector :: Selector '[] (Id NSArray)
arrangedObjectsSelector = mkSelector "arrangedObjects"

-- | @Selector@ for @setArrangedObjects:@
setArrangedObjectsSelector :: Selector '[Id NSArray] ()
setArrangedObjectsSelector = mkSelector "setArrangedObjects:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CLong
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CLong] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

