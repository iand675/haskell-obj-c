{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAppearance@.
module ObjC.AppKit.NSAppearance
  ( NSAppearance
  , IsNSAppearance(..)
  , performAsCurrentDrawingAppearance
  , appearanceNamed
  , initWithAppearanceNamed_bundle
  , initWithCoder
  , bestMatchFromAppearancesWithNames
  , name
  , currentAppearance
  , setCurrentAppearance
  , currentDrawingAppearance
  , allowsVibrancy
  , performAsCurrentDrawingAppearanceSelector
  , appearanceNamedSelector
  , initWithAppearanceNamed_bundleSelector
  , initWithCoderSelector
  , bestMatchFromAppearancesWithNamesSelector
  , nameSelector
  , currentAppearanceSelector
  , setCurrentAppearanceSelector
  , currentDrawingAppearanceSelector
  , allowsVibrancySelector


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

-- | @- performAsCurrentDrawingAppearance:@
performAsCurrentDrawingAppearance :: IsNSAppearance nsAppearance => nsAppearance -> Ptr () -> IO ()
performAsCurrentDrawingAppearance nsAppearance  block =
    sendMsg nsAppearance (mkSelector "performAsCurrentDrawingAppearance:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @+ appearanceNamed:@
appearanceNamed :: IsNSString name => name -> IO (Id NSAppearance)
appearanceNamed name =
  do
    cls' <- getRequiredClass "NSAppearance"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "appearanceNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithAppearanceNamed:bundle:@
initWithAppearanceNamed_bundle :: (IsNSAppearance nsAppearance, IsNSString name, IsNSBundle bundle) => nsAppearance -> name -> bundle -> IO (Id NSAppearance)
initWithAppearanceNamed_bundle nsAppearance  name bundle =
  withObjCPtr name $ \raw_name ->
    withObjCPtr bundle $ \raw_bundle ->
        sendMsg nsAppearance (mkSelector "initWithAppearanceNamed:bundle:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSAppearance nsAppearance, IsNSCoder coder) => nsAppearance -> coder -> IO (Id NSAppearance)
initWithCoder nsAppearance  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsAppearance (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- bestMatchFromAppearancesWithNames:@
bestMatchFromAppearancesWithNames :: (IsNSAppearance nsAppearance, IsNSArray appearances) => nsAppearance -> appearances -> IO (Id NSString)
bestMatchFromAppearancesWithNames nsAppearance  appearances =
  withObjCPtr appearances $ \raw_appearances ->
      sendMsg nsAppearance (mkSelector "bestMatchFromAppearancesWithNames:") (retPtr retVoid) [argPtr (castPtr raw_appearances :: Ptr ())] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSAppearance nsAppearance => nsAppearance -> IO (Id NSString)
name nsAppearance  =
    sendMsg nsAppearance (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ currentAppearance@
currentAppearance :: IO (Id NSAppearance)
currentAppearance  =
  do
    cls' <- getRequiredClass "NSAppearance"
    sendClassMsg cls' (mkSelector "currentAppearance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setCurrentAppearance:@
setCurrentAppearance :: IsNSAppearance value => value -> IO ()
setCurrentAppearance value =
  do
    cls' <- getRequiredClass "NSAppearance"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setCurrentAppearance:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ currentDrawingAppearance@
currentDrawingAppearance :: IO (Id NSAppearance)
currentDrawingAppearance  =
  do
    cls' <- getRequiredClass "NSAppearance"
    sendClassMsg cls' (mkSelector "currentDrawingAppearance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allowsVibrancy@
allowsVibrancy :: IsNSAppearance nsAppearance => nsAppearance -> IO Bool
allowsVibrancy nsAppearance  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAppearance (mkSelector "allowsVibrancy") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performAsCurrentDrawingAppearance:@
performAsCurrentDrawingAppearanceSelector :: Selector
performAsCurrentDrawingAppearanceSelector = mkSelector "performAsCurrentDrawingAppearance:"

-- | @Selector@ for @appearanceNamed:@
appearanceNamedSelector :: Selector
appearanceNamedSelector = mkSelector "appearanceNamed:"

-- | @Selector@ for @initWithAppearanceNamed:bundle:@
initWithAppearanceNamed_bundleSelector :: Selector
initWithAppearanceNamed_bundleSelector = mkSelector "initWithAppearanceNamed:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @bestMatchFromAppearancesWithNames:@
bestMatchFromAppearancesWithNamesSelector :: Selector
bestMatchFromAppearancesWithNamesSelector = mkSelector "bestMatchFromAppearancesWithNames:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @currentAppearance@
currentAppearanceSelector :: Selector
currentAppearanceSelector = mkSelector "currentAppearance"

-- | @Selector@ for @setCurrentAppearance:@
setCurrentAppearanceSelector :: Selector
setCurrentAppearanceSelector = mkSelector "setCurrentAppearance:"

-- | @Selector@ for @currentDrawingAppearance@
currentDrawingAppearanceSelector :: Selector
currentDrawingAppearanceSelector = mkSelector "currentDrawingAppearance"

-- | @Selector@ for @allowsVibrancy@
allowsVibrancySelector :: Selector
allowsVibrancySelector = mkSelector "allowsVibrancy"

