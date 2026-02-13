{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMBundleAction@.
module ObjC.Automator.AMBundleAction
  ( AMBundleAction
  , IsAMBundleAction(..)
  , awakeFromBundle
  , hasView
  , view
  , bundle
  , parameters
  , setParameters
  , awakeFromBundleSelector
  , bundleSelector
  , hasViewSelector
  , parametersSelector
  , setParametersSelector
  , viewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Automator.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- awakeFromBundle@
awakeFromBundle :: IsAMBundleAction amBundleAction => amBundleAction -> IO ()
awakeFromBundle amBundleAction =
  sendMessage amBundleAction awakeFromBundleSelector

-- | @- hasView@
hasView :: IsAMBundleAction amBundleAction => amBundleAction -> IO Bool
hasView amBundleAction =
  sendMessage amBundleAction hasViewSelector

-- | @- view@
view :: IsAMBundleAction amBundleAction => amBundleAction -> IO (Id NSView)
view amBundleAction =
  sendMessage amBundleAction viewSelector

-- | @- bundle@
bundle :: IsAMBundleAction amBundleAction => amBundleAction -> IO (Id NSBundle)
bundle amBundleAction =
  sendMessage amBundleAction bundleSelector

-- | @- parameters@
parameters :: IsAMBundleAction amBundleAction => amBundleAction -> IO (Id NSMutableDictionary)
parameters amBundleAction =
  sendMessage amBundleAction parametersSelector

-- | @- setParameters:@
setParameters :: (IsAMBundleAction amBundleAction, IsNSMutableDictionary value) => amBundleAction -> value -> IO ()
setParameters amBundleAction value =
  sendMessage amBundleAction setParametersSelector (toNSMutableDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @awakeFromBundle@
awakeFromBundleSelector :: Selector '[] ()
awakeFromBundleSelector = mkSelector "awakeFromBundle"

-- | @Selector@ for @hasView@
hasViewSelector :: Selector '[] Bool
hasViewSelector = mkSelector "hasView"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @bundle@
bundleSelector :: Selector '[] (Id NSBundle)
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @parameters@
parametersSelector :: Selector '[] (Id NSMutableDictionary)
parametersSelector = mkSelector "parameters"

-- | @Selector@ for @setParameters:@
setParametersSelector :: Selector '[Id NSMutableDictionary] ()
setParametersSelector = mkSelector "setParameters:"

