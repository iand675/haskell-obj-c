{-# LANGUAGE DataKinds #-}
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
  , allowsVibrancySelector
  , appearanceNamedSelector
  , bestMatchFromAppearancesWithNamesSelector
  , currentAppearanceSelector
  , currentDrawingAppearanceSelector
  , initWithAppearanceNamed_bundleSelector
  , initWithCoderSelector
  , nameSelector
  , performAsCurrentDrawingAppearanceSelector
  , setCurrentAppearanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- performAsCurrentDrawingAppearance:@
performAsCurrentDrawingAppearance :: IsNSAppearance nsAppearance => nsAppearance -> Ptr () -> IO ()
performAsCurrentDrawingAppearance nsAppearance block =
  sendMessage nsAppearance performAsCurrentDrawingAppearanceSelector block

-- | @+ appearanceNamed:@
appearanceNamed :: IsNSString name => name -> IO (Id NSAppearance)
appearanceNamed name =
  do
    cls' <- getRequiredClass "NSAppearance"
    sendClassMessage cls' appearanceNamedSelector (toNSString name)

-- | @- initWithAppearanceNamed:bundle:@
initWithAppearanceNamed_bundle :: (IsNSAppearance nsAppearance, IsNSString name, IsNSBundle bundle) => nsAppearance -> name -> bundle -> IO (Id NSAppearance)
initWithAppearanceNamed_bundle nsAppearance name bundle =
  sendOwnedMessage nsAppearance initWithAppearanceNamed_bundleSelector (toNSString name) (toNSBundle bundle)

-- | @- initWithCoder:@
initWithCoder :: (IsNSAppearance nsAppearance, IsNSCoder coder) => nsAppearance -> coder -> IO (Id NSAppearance)
initWithCoder nsAppearance coder =
  sendOwnedMessage nsAppearance initWithCoderSelector (toNSCoder coder)

-- | @- bestMatchFromAppearancesWithNames:@
bestMatchFromAppearancesWithNames :: (IsNSAppearance nsAppearance, IsNSArray appearances) => nsAppearance -> appearances -> IO (Id NSString)
bestMatchFromAppearancesWithNames nsAppearance appearances =
  sendMessage nsAppearance bestMatchFromAppearancesWithNamesSelector (toNSArray appearances)

-- | @- name@
name :: IsNSAppearance nsAppearance => nsAppearance -> IO (Id NSString)
name nsAppearance =
  sendMessage nsAppearance nameSelector

-- | @+ currentAppearance@
currentAppearance :: IO (Id NSAppearance)
currentAppearance  =
  do
    cls' <- getRequiredClass "NSAppearance"
    sendClassMessage cls' currentAppearanceSelector

-- | @+ setCurrentAppearance:@
setCurrentAppearance :: IsNSAppearance value => value -> IO ()
setCurrentAppearance value =
  do
    cls' <- getRequiredClass "NSAppearance"
    sendClassMessage cls' setCurrentAppearanceSelector (toNSAppearance value)

-- | @+ currentDrawingAppearance@
currentDrawingAppearance :: IO (Id NSAppearance)
currentDrawingAppearance  =
  do
    cls' <- getRequiredClass "NSAppearance"
    sendClassMessage cls' currentDrawingAppearanceSelector

-- | @- allowsVibrancy@
allowsVibrancy :: IsNSAppearance nsAppearance => nsAppearance -> IO Bool
allowsVibrancy nsAppearance =
  sendMessage nsAppearance allowsVibrancySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performAsCurrentDrawingAppearance:@
performAsCurrentDrawingAppearanceSelector :: Selector '[Ptr ()] ()
performAsCurrentDrawingAppearanceSelector = mkSelector "performAsCurrentDrawingAppearance:"

-- | @Selector@ for @appearanceNamed:@
appearanceNamedSelector :: Selector '[Id NSString] (Id NSAppearance)
appearanceNamedSelector = mkSelector "appearanceNamed:"

-- | @Selector@ for @initWithAppearanceNamed:bundle:@
initWithAppearanceNamed_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id NSAppearance)
initWithAppearanceNamed_bundleSelector = mkSelector "initWithAppearanceNamed:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSAppearance)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @bestMatchFromAppearancesWithNames:@
bestMatchFromAppearancesWithNamesSelector :: Selector '[Id NSArray] (Id NSString)
bestMatchFromAppearancesWithNamesSelector = mkSelector "bestMatchFromAppearancesWithNames:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @currentAppearance@
currentAppearanceSelector :: Selector '[] (Id NSAppearance)
currentAppearanceSelector = mkSelector "currentAppearance"

-- | @Selector@ for @setCurrentAppearance:@
setCurrentAppearanceSelector :: Selector '[Id NSAppearance] ()
setCurrentAppearanceSelector = mkSelector "setCurrentAppearance:"

-- | @Selector@ for @currentDrawingAppearance@
currentDrawingAppearanceSelector :: Selector '[] (Id NSAppearance)
currentDrawingAppearanceSelector = mkSelector "currentDrawingAppearance"

-- | @Selector@ for @allowsVibrancy@
allowsVibrancySelector :: Selector '[] Bool
allowsVibrancySelector = mkSelector "allowsVibrancy"

