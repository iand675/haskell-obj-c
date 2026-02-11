{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFontPanel@.
module ObjC.AppKit.NSFontPanel
  ( NSFontPanel
  , IsNSFontPanel(..)
  , setPanelFont_isMultiple
  , panelConvertFont
  , reloadDefaultFontFamilies
  , sharedFontPanel
  , sharedFontPanelExists
  , accessoryView
  , setAccessoryView
  , worksWhenModal
  , setWorksWhenModal
  , enabled
  , setEnabled
  , setPanelFont_isMultipleSelector
  , panelConvertFontSelector
  , reloadDefaultFontFamiliesSelector
  , sharedFontPanelSelector
  , sharedFontPanelExistsSelector
  , accessoryViewSelector
  , setAccessoryViewSelector
  , worksWhenModalSelector
  , setWorksWhenModalSelector
  , enabledSelector
  , setEnabledSelector


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

-- | @- setPanelFont:isMultiple:@
setPanelFont_isMultiple :: (IsNSFontPanel nsFontPanel, IsNSFont fontObj) => nsFontPanel -> fontObj -> Bool -> IO ()
setPanelFont_isMultiple nsFontPanel  fontObj flag =
withObjCPtr fontObj $ \raw_fontObj ->
    sendMsg nsFontPanel (mkSelector "setPanelFont:isMultiple:") retVoid [argPtr (castPtr raw_fontObj :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- panelConvertFont:@
panelConvertFont :: (IsNSFontPanel nsFontPanel, IsNSFont fontObj) => nsFontPanel -> fontObj -> IO (Id NSFont)
panelConvertFont nsFontPanel  fontObj =
withObjCPtr fontObj $ \raw_fontObj ->
    sendMsg nsFontPanel (mkSelector "panelConvertFont:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ())] >>= retainedObject . castPtr

-- | @- reloadDefaultFontFamilies@
reloadDefaultFontFamilies :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO ()
reloadDefaultFontFamilies nsFontPanel  =
  sendMsg nsFontPanel (mkSelector "reloadDefaultFontFamilies") retVoid []

-- | @+ sharedFontPanel@
sharedFontPanel :: IO (Id NSFontPanel)
sharedFontPanel  =
  do
    cls' <- getRequiredClass "NSFontPanel"
    sendClassMsg cls' (mkSelector "sharedFontPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sharedFontPanelExists@
sharedFontPanelExists :: IO Bool
sharedFontPanelExists  =
  do
    cls' <- getRequiredClass "NSFontPanel"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "sharedFontPanelExists") retCULong []

-- | @- accessoryView@
accessoryView :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO (Id NSView)
accessoryView nsFontPanel  =
  sendMsg nsFontPanel (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSFontPanel nsFontPanel, IsNSView value) => nsFontPanel -> value -> IO ()
setAccessoryView nsFontPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFontPanel (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- worksWhenModal@
worksWhenModal :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO Bool
worksWhenModal nsFontPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontPanel (mkSelector "worksWhenModal") retCULong []

-- | @- setWorksWhenModal:@
setWorksWhenModal :: IsNSFontPanel nsFontPanel => nsFontPanel -> Bool -> IO ()
setWorksWhenModal nsFontPanel  value =
  sendMsg nsFontPanel (mkSelector "setWorksWhenModal:") retVoid [argCULong (if value then 1 else 0)]

-- | @- enabled@
enabled :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO Bool
enabled nsFontPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontPanel (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSFontPanel nsFontPanel => nsFontPanel -> Bool -> IO ()
setEnabled nsFontPanel  value =
  sendMsg nsFontPanel (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPanelFont:isMultiple:@
setPanelFont_isMultipleSelector :: Selector
setPanelFont_isMultipleSelector = mkSelector "setPanelFont:isMultiple:"

-- | @Selector@ for @panelConvertFont:@
panelConvertFontSelector :: Selector
panelConvertFontSelector = mkSelector "panelConvertFont:"

-- | @Selector@ for @reloadDefaultFontFamilies@
reloadDefaultFontFamiliesSelector :: Selector
reloadDefaultFontFamiliesSelector = mkSelector "reloadDefaultFontFamilies"

-- | @Selector@ for @sharedFontPanel@
sharedFontPanelSelector :: Selector
sharedFontPanelSelector = mkSelector "sharedFontPanel"

-- | @Selector@ for @sharedFontPanelExists@
sharedFontPanelExistsSelector :: Selector
sharedFontPanelExistsSelector = mkSelector "sharedFontPanelExists"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @worksWhenModal@
worksWhenModalSelector :: Selector
worksWhenModalSelector = mkSelector "worksWhenModal"

-- | @Selector@ for @setWorksWhenModal:@
setWorksWhenModalSelector :: Selector
setWorksWhenModalSelector = mkSelector "setWorksWhenModal:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

