{-# LANGUAGE DataKinds #-}
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
  , customizationLabelSelector
  , identifierSelector
  , initSelector
  , initWithCoderSelector
  , initWithIdentifierSelector
  , setVisibilityPrioritySelector
  , viewControllerSelector
  , viewSelector
  , visibilityPrioritySelector
  , visibleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSTouchBarItem nsTouchBarItem, IsNSString identifier) => nsTouchBarItem -> identifier -> IO (Id NSTouchBarItem)
initWithIdentifier nsTouchBarItem identifier =
  sendOwnedMessage nsTouchBarItem initWithIdentifierSelector (toNSString identifier)

-- | @- initWithCoder:@
initWithCoder :: (IsNSTouchBarItem nsTouchBarItem, IsNSCoder coder) => nsTouchBarItem -> coder -> IO (Id NSTouchBarItem)
initWithCoder nsTouchBarItem coder =
  sendOwnedMessage nsTouchBarItem initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSTouchBarItem)
init_ nsTouchBarItem =
  sendOwnedMessage nsTouchBarItem initSelector

-- | @- identifier@
identifier :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSString)
identifier nsTouchBarItem =
  sendMessage nsTouchBarItem identifierSelector

-- | @- visibilityPriority@
visibilityPriority :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO CFloat
visibilityPriority nsTouchBarItem =
  sendMessage nsTouchBarItem visibilityPrioritySelector

-- | @- setVisibilityPriority:@
setVisibilityPriority :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> CFloat -> IO ()
setVisibilityPriority nsTouchBarItem value =
  sendMessage nsTouchBarItem setVisibilityPrioritySelector value

-- | @- view@
view :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSView)
view nsTouchBarItem =
  sendMessage nsTouchBarItem viewSelector

-- | @- viewController@
viewController :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSViewController)
viewController nsTouchBarItem =
  sendMessage nsTouchBarItem viewControllerSelector

-- | @- customizationLabel@
customizationLabel :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO (Id NSString)
customizationLabel nsTouchBarItem =
  sendMessage nsTouchBarItem customizationLabelSelector

-- | @- visible@
visible :: IsNSTouchBarItem nsTouchBarItem => nsTouchBarItem -> IO Bool
visible nsTouchBarItem =
  sendMessage nsTouchBarItem visibleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id NSTouchBarItem)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTouchBarItem)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTouchBarItem)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @visibilityPriority@
visibilityPrioritySelector :: Selector '[] CFloat
visibilityPrioritySelector = mkSelector "visibilityPriority"

-- | @Selector@ for @setVisibilityPriority:@
setVisibilityPrioritySelector :: Selector '[CFloat] ()
setVisibilityPrioritySelector = mkSelector "setVisibilityPriority:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector '[] (Id NSViewController)
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

