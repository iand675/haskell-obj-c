{-# LANGUAGE PatternSynonyms #-}
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
  , navigateForwardToObjectSelector
  , completeTransitionSelector
  , navigateBackSelector
  , navigateForwardSelector
  , takeSelectedIndexFromSelector
  , delegateSelector
  , setDelegateSelector
  , selectedViewControllerSelector
  , transitionStyleSelector
  , setTransitionStyleSelector
  , arrangedObjectsSelector
  , setArrangedObjectsSelector
  , selectedIndexSelector
  , setSelectedIndexSelector

  -- * Enum types
  , NSPageControllerTransitionStyle(NSPageControllerTransitionStyle)
  , pattern NSPageControllerTransitionStyleStackHistory
  , pattern NSPageControllerTransitionStyleStackBook
  , pattern NSPageControllerTransitionStyleHorizontalStrip

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- navigateForwardToObject:@
navigateForwardToObject :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
navigateForwardToObject nsPageController  object =
    sendMsg nsPageController (mkSelector "navigateForwardToObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- completeTransition@
completeTransition :: IsNSPageController nsPageController => nsPageController -> IO ()
completeTransition nsPageController  =
    sendMsg nsPageController (mkSelector "completeTransition") retVoid []

-- | @- navigateBack:@
navigateBack :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
navigateBack nsPageController  sender =
    sendMsg nsPageController (mkSelector "navigateBack:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- navigateForward:@
navigateForward :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
navigateForward nsPageController  sender =
    sendMsg nsPageController (mkSelector "navigateForward:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- takeSelectedIndexFrom:@
takeSelectedIndexFrom :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
takeSelectedIndexFrom nsPageController  sender =
    sendMsg nsPageController (mkSelector "takeSelectedIndexFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- delegate@
delegate :: IsNSPageController nsPageController => nsPageController -> IO RawId
delegate nsPageController  =
    fmap (RawId . castPtr) $ sendMsg nsPageController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSPageController nsPageController => nsPageController -> RawId -> IO ()
setDelegate nsPageController  value =
    sendMsg nsPageController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- selectedViewController@
selectedViewController :: IsNSPageController nsPageController => nsPageController -> IO (Id NSViewController)
selectedViewController nsPageController  =
    sendMsg nsPageController (mkSelector "selectedViewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transitionStyle@
transitionStyle :: IsNSPageController nsPageController => nsPageController -> IO NSPageControllerTransitionStyle
transitionStyle nsPageController  =
    fmap (coerce :: CLong -> NSPageControllerTransitionStyle) $ sendMsg nsPageController (mkSelector "transitionStyle") retCLong []

-- | @- setTransitionStyle:@
setTransitionStyle :: IsNSPageController nsPageController => nsPageController -> NSPageControllerTransitionStyle -> IO ()
setTransitionStyle nsPageController  value =
    sendMsg nsPageController (mkSelector "setTransitionStyle:") retVoid [argCLong (coerce value)]

-- | @- arrangedObjects@
arrangedObjects :: IsNSPageController nsPageController => nsPageController -> IO (Id NSArray)
arrangedObjects nsPageController  =
    sendMsg nsPageController (mkSelector "arrangedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArrangedObjects:@
setArrangedObjects :: (IsNSPageController nsPageController, IsNSArray value) => nsPageController -> value -> IO ()
setArrangedObjects nsPageController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPageController (mkSelector "setArrangedObjects:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectedIndex@
selectedIndex :: IsNSPageController nsPageController => nsPageController -> IO CLong
selectedIndex nsPageController  =
    sendMsg nsPageController (mkSelector "selectedIndex") retCLong []

-- | @- setSelectedIndex:@
setSelectedIndex :: IsNSPageController nsPageController => nsPageController -> CLong -> IO ()
setSelectedIndex nsPageController  value =
    sendMsg nsPageController (mkSelector "setSelectedIndex:") retVoid [argCLong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @navigateForwardToObject:@
navigateForwardToObjectSelector :: Selector
navigateForwardToObjectSelector = mkSelector "navigateForwardToObject:"

-- | @Selector@ for @completeTransition@
completeTransitionSelector :: Selector
completeTransitionSelector = mkSelector "completeTransition"

-- | @Selector@ for @navigateBack:@
navigateBackSelector :: Selector
navigateBackSelector = mkSelector "navigateBack:"

-- | @Selector@ for @navigateForward:@
navigateForwardSelector :: Selector
navigateForwardSelector = mkSelector "navigateForward:"

-- | @Selector@ for @takeSelectedIndexFrom:@
takeSelectedIndexFromSelector :: Selector
takeSelectedIndexFromSelector = mkSelector "takeSelectedIndexFrom:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @selectedViewController@
selectedViewControllerSelector :: Selector
selectedViewControllerSelector = mkSelector "selectedViewController"

-- | @Selector@ for @transitionStyle@
transitionStyleSelector :: Selector
transitionStyleSelector = mkSelector "transitionStyle"

-- | @Selector@ for @setTransitionStyle:@
setTransitionStyleSelector :: Selector
setTransitionStyleSelector = mkSelector "setTransitionStyle:"

-- | @Selector@ for @arrangedObjects@
arrangedObjectsSelector :: Selector
arrangedObjectsSelector = mkSelector "arrangedObjects"

-- | @Selector@ for @setArrangedObjects:@
setArrangedObjectsSelector :: Selector
setArrangedObjectsSelector = mkSelector "setArrangedObjects:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

