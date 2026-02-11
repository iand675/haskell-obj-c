{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTouchBarItem@.
module ObjC.AppKit.NSTouchBarItem
  ( NSTouchBarItem
  , IsNSTouchBarItem(..)
  , initWithIdentifier
  , initWithCoder
  , init_
  , identifier
  , visibilityPriority
  , setVisibilityPriority
  , view
  , viewController
  , customizationLabel
  , visible
  , initWithIdentifierSelector
  , initWithCoderSelector
  , initSelector
  , identifierSelector
  , visibilityPrioritySelector
  , setVisibilityPrioritySelector
  , viewSelector
  , viewControllerSelector
  , customizationLabelSelector
  , visibleSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSTouchBarItem nsTouchBarItem, IsNSString identifier) => nsTouchBarItem -> identifier -> IO (Id NSTouchBarItem)
initWithIdentifier nsTouchBarItem  identifier =
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg nsTouchBarItem (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTouchBarItem nsTouchBarItem, IsNSCoder coder) => nsTouchBarItem -> coder -> IO (Id NSTouchBarItem)
initWithCoder nsTouchBarItem  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsTouchBarItem (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSTouchBarItem)
init_ nsTouchBarItem  =
    sendMsg nsTouchBarItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSString)
identifier nsTouchBarItem  =
    sendMsg nsTouchBarItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- visibilityPriority@
visibilityPriority :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO CFloat
visibilityPriority nsTouchBarItem  =
    sendMsg nsTouchBarItem (mkSelector "visibilityPriority") retCFloat []

-- | @- setVisibilityPriority:@
setVisibilityPriority :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> CFloat -> IO ()
setVisibilityPriority nsTouchBarItem  value =
    sendMsg nsTouchBarItem (mkSelector "setVisibilityPriority:") retVoid [argCFloat value]

-- | @- view@
view :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSView)
view nsTouchBarItem  =
    sendMsg nsTouchBarItem (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- viewController@
viewController :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSViewController)
viewController nsTouchBarItem  =
    sendMsg nsTouchBarItem (mkSelector "viewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- customizationLabel@
customizationLabel :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSString)
customizationLabel nsTouchBarItem  =
    sendMsg nsTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- visible@
visible :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO Bool
visible nsTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTouchBarItem (mkSelector "visible") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @visibilityPriority@
visibilityPrioritySelector :: Selector
visibilityPrioritySelector = mkSelector "visibilityPriority"

-- | @Selector@ for @setVisibilityPriority:@
setVisibilityPrioritySelector :: Selector
setVisibilityPrioritySelector = mkSelector "setVisibilityPriority:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

